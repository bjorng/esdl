/*
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id$
 */

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "woggle_win.h"
#include "woggle_driver.h"
#include "woggle_compat_if.h"
#else
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>

#include "woggle_driver.h"

#define TEMP_BINARY_SIZE 512

static ErlDrvData wog_driver_start(ErlDrvPort port, char *buff);
static void wog_driver_stop(ErlDrvData handle);
static void wog_driver_finish(void);
static int wog_driver_control(ErlDrvData handle, unsigned int command, 
			      char* buf, int count, char** res, int res_size);
static void standard_outputv(ErlDrvData drv_data, ErlIOVec *ev);

/*
** The driver struct
*/
static ErlDrvEntry wog_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    wog_driver_start,      /* L_PTR start, called when port is opened */
    wog_driver_stop,       /* F_PTR stop, called when port is closed */
    NULL,	           /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "woggle_driver",          /* char *driver_name, the argument to open_port */
    wog_driver_finish,     /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    wog_driver_control,    /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, reserved */
    standard_outputv,	   /* F_PTR outputv, reserved */
};

DRIVER_INIT(wog_driver)
{
   return &wog_driver_entry;
}

static ErlDrvData wog_driver_start(ErlDrvPort port, char *buff)
{      
   sdl_data *data;   

   data = malloc(sizeof(sdl_data));
   
   if (data == NULL) {
      fprintf(stderr, " Couldn't alloc mem\r\n");
      return(ERL_DRV_ERROR_GENERAL);  /* ENOMEM */      
   } else {
      set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
      data->driver_data = port;
      data->op    = 0;
      data->len   = 0;
      data->buff  = NULL; 

      data->temp_bin = driver_alloc_binary(TEMP_BINARY_SIZE);

      data->next_bin = 0;
#ifdef WIN32
      /* wgl stuff */
      data->qh = NULL;
      memset(&(data->wd),0,sizeof(WogWindowData));
#endif
      data->save_x = data->save_y = -1;
      data->saved_event = NULL;
      init_fps(data);
      data->extensions_loaded = 0;
   }
   return (ErlDrvData) data;
}

static void
wog_driver_stop(ErlDrvData handle) 
{  
    sdl_data *sd = ((sdl_data *)handle);

    if (sd->saved_event != NULL) {
	wog_free_event_message(sd->saved_event);
    }
    wog_close_window(&(sd->wd));
#ifdef HARDDEBUG
    FreeConsole();
#endif
    free(sd->fun_tab);
    free(sd->str_tab);
    
    free(handle);
}

static void
wog_driver_finish(void) 
{
}

static int
wog_driver_control(ErlDrvData handle, unsigned op,
		   char* buf, int count, char** res, int res_size)
{
  sdl_data* sd = (sdl_data *) handle;
  sdl_fun func;

  sd->buff = NULL;
  sd->len = 0;
  sd->op = op;
  func = sd->fun_tab[op];
  func(sd, count, buf);
  (*res) = sd->buff;
  return sd->len;
}

static int
wog_driver_debug_control(ErlDrvData handle, unsigned op,
			 char* buf, int count, char** res, int res_size)
{
  sdl_data* sd = (sdl_data *) handle;
  sdl_fun func;
  int len;

  sd->buff = NULL;
  sd->len = 0;
  sd->op = op;
  fprintf(stderr, "Command:%d:%s: ", op, sd->str_tab[op]);
  func = sd->fun_tab[op];

  func(sd, count, buf);
  if ((len = sd->len) >= 0) {
    fprintf(stderr, "ok\r\n");
    (*res) = sd->buff;
    return len;
  } else {
    fprintf(stderr, "error\r\n");
    *res = 0;
    return -1;
  }
}

void sdl_send(sdl_data *sd, int len)
{
  if (sd->buff == NULL) {
    fprintf(stderr, "EWOG INTERNAL ERROR: sdl_send in %s sent NULL buffer: %d\r\n",
	    sd->str_tab[sd->op], len);
    abort();
  }
  if (len > sd->len) {
    fprintf(stderr, "EWOG INTERNAL ERROR: sdl_send in %s allocated %d sent %d\r\n",
	    sd->str_tab[sd->op], sd->len, len);
    abort();
  }

  /* Workaround that driver_control doesn't check length */
  ((ErlDrvBinary *) sd->buff)->orig_size = len;
  sd->len = len;
}

char* sdl_getbuff(sdl_data *sd, int size)
{  
  ErlDrvBinary* bin;
  sd->len = size;  
  bin = driver_alloc_binary(size); 
  sd->buff = bin;
  /* And return the pointer to the bytes */
  return bin->orig_bytes;
}

#ifdef DANGEROUS
char* sdl_get_temp_buff(sdl_data* sd, int size)
{
  if (size > TEMP_BINARY_SIZE) {
    return sdl_getbuff(sd, size);
  } else {
    ErlDrvBinary* bin = (ErlDrvBinary *) sd->temp_bin;
    bin->refc++;
    sd->buff = bin;
    sd->len = size;
    return bin->orig_bytes;
  }
}
#else
char* sdl_get_temp_buff(sdl_data* sd, int size)
{
    return sdl_getbuff(sd, size);
}
#endif

void
sdl_util_debug(sdl_data *sd, int len, char* bp)
{
  if (*bp) {
    wog_driver_entry.control = wog_driver_debug_control;
  } else {
    wog_driver_entry.control = wog_driver_control;
  }
}

static void
standard_outputv(ErlDrvData drv_data, ErlIOVec* ev)
{
  sdl_data* sd = (sdl_data *) drv_data;
  ErlDrvBinary* bin;

  if (ev->vsize == 2) {
    int i = sd->next_bin;

    sd->bin[i].base = ev->iov[1].iov_base;
    sd->bin[i].size = ev->iov[1].iov_len;
    bin = ev->binv[1];
    bin->refc++;		/* Otherwise it could get deallocated */
    sd->bin[i].bin = bin;
    sd->next_bin++;
  }
}

void
sdl_free_binaries(sdl_data* sd)
{
  int i;
  
  for (i = sd->next_bin - 1; i >= 0; i--) {
    driver_free_binary(sd->bin[i].bin);
  }
  sd->next_bin = 0;
}
