/*
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id$
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
#include <io.h>
#else
#include <unistd.h>
#endif

#ifdef _OSX_COCOA
//#include "CPS.h"
//#include "SDLMain.h"
#include <Cocoa/Cocoa.h>
#include <objc/objc-runtime.h>
#endif

#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>

#include "esdl.h"

#define TEMP_BINARY_SIZE 512

#if (ERL_DRV_EXTENDED_MAJOR_VERSION < 2)
/* R14B or earlier types */
#define ErlDrvSizeT  int
#define ErlDrvSSizeT int
#endif

#ifdef  _OSX_COCOA
#  define AUTO_RELEASE_POOL @autoreleasepool
#else
#  define AUTO_RELEASE_POOL
#endif

static ErlDrvData sdl_driver_start(ErlDrvPort port, char *buff);
static void sdl_driver_stop(ErlDrvData handle);
static void sdl_driver_finish(void);
static ErlDrvSSizeT sdl_driver_control(ErlDrvData handle, unsigned int command,
				       char* buf, ErlDrvSizeT, char** res, ErlDrvSizeT);
static ErlDrvSSizeT sdl_driver_debug_control(ErlDrvData handle, unsigned int command,
					     char* buf, ErlDrvSizeT count, char** res,
					     ErlDrvSizeT res_size);

static void standard_outputv(ErlDrvData drv_data, ErlIOVec *ev);

/*
** The driver struct
*/
static ErlDrvEntry sdl_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    sdl_driver_start,      /* L_PTR start, called when port is opened */
    sdl_driver_stop,       /* F_PTR stop, called when port is closed */
    NULL,	           /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "sdl_driver",          /* char *driver_name, the argument to open_port */
    sdl_driver_finish,     /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    sdl_driver_control,    /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, reserved */
    standard_outputv,	   /* F_PTR outputv, reserved */
    NULL,                  /* async */ 
    NULL,                  /* flush */
    NULL,                  /* call */
    NULL,                  /* Event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING, /* Port lock */ 
    NULL,                  /* Reserved Handle */
    NULL,                  /* Process Exited */
};

DRIVER_INIT(sdl_driver)
{
   return &sdl_driver_entry;
}

static ErlDrvData sdl_driver_start(ErlDrvPort port, char *buff)
{      
   sdl_data *data;   
   ErlDrvSysInfo info;

#ifdef _WIN32
   if ( SDL_RegisterApp("Erlang SDL", CS_BYTEALIGNCLIENT, 
			GetModuleHandle(NULL)) < 0 ) {
      fprintf(stderr, "WinMain() error %s \n", SDL_GetError());
      return(ERL_DRV_ERROR_GENERAL);
   }
#endif /* _WIN32 */
   
   data = malloc(sizeof(sdl_data));
   
   if (data == NULL) {
      fprintf(stderr, " Couldn't alloc mem\r\n");
      return(ERL_DRV_ERROR_GENERAL);  /* ENOMEM */      
   }
   set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
   data->driver_data = port;
   //      data->fns   = init_fps();   
   
   data->op    = 0;
   data->len   = 0;
   data->buff  = NULL; 
   
   data->temp_bin = driver_alloc_binary(TEMP_BINARY_SIZE);   
   data->next_bin = 0;
   
   driver_system_info(&info, sizeof(ErlDrvSysInfo));
#ifdef _OSX_COCOA
   data->use_smp = info.smp_support;
#else
   data->use_smp = info.smp_support && info.scheduler_threads > 1;
#endif
   // fprintf(stderr, "Use smp %d\r\n", data->use_smp);
   if(data->use_smp) {
      start_opengl_thread(data);
   } else { // The following code needs to called from main thread
      esdl_init_native_gui();
   }
   init_fps(data);
   
   return (ErlDrvData) data;
}

#ifdef _OSX_COCOA
extern OSErr  CPSSetProcessName (ProcessSerialNumber *psn, char *processname);
#endif

void esdl_init_native_gui() 
{
#ifdef _OSX_COCOA
   ProcessSerialNumber psn;
   NSAutoreleasePool *pool;

   // [[NSProcessInfo processInfo] setProcessName:@"Erlang"]; 
   // Enable GUI 
   GetCurrentProcess(&psn);
   CPSSetProcessName(&psn, "Erlang");  // Undocumented function (but above doesn't work)
   TransformProcessType(&psn, kProcessTransformToForegroundApplication);
   SetFrontProcess(&psn);

   // Enable Cocoa calls from Carbon app
   NSApplicationLoad();

   // Setup and enable gui
   pool = [[NSAutoreleasePool alloc] init];

   NSApplication *app = [NSApplication sharedApplication];
   // Load and set icon
   
   //NSMutableString *file = [[NSMutableString alloc] init];
   //[file appendFormat:@"%s/%s", erl_wx_privdir, "erlang-logo64.png"];
   //[file appendFormat:@"%s/%s", "/usr/local/lib/erlang/lib/wx-0.98.8/priv", "erlang-logo64.png"];
   //NSImage *icon = [[NSImage alloc] initWithContentsOfFile: file];
   //[app setApplicationIconImage: icon];
   [app activateIgnoringOtherApps: YES];

#endif /* _OSX_COCOA */
}

static void
sdl_driver_stop(ErlDrvData handle) 
{  
  sdl_data *sd = ((sdl_data *)handle);

  if(sd->use_smp)
     stop_opengl_thread();
  else 
     SDL_Quit();
  free(sd->fun_tab);
  free(sd->str_tab);

#ifdef _WIN32
  UnregisterClass("Erlang SDL", GetModuleHandle(NULL));
#endif /* _WIN32 */

  free(handle);
}

static void
sdl_driver_finish(void) 
{
}

static ErlDrvSSizeT
sdl_driver_control(ErlDrvData handle, unsigned op,
		   char* buf, ErlDrvSizeT count, char** res, ErlDrvSizeT res_size)
{
  sdl_data* sd = (sdl_data *) handle;
  sdl_fun func;

  sd->buff = NULL;
  sd->len = 0;
  sd->op = op;

  if(op < OPENGL_START) {
     // fprintf(stderr, "Command:%d:%s: ", op, sd->str_tab[op]);fflush(stderr);
     func = sd->fun_tab[op];
     AUTO_RELEASE_POOL {
	 func(sd, (int) count, buf);
     }
  } else {
      // fprintf(stderr, "Command:%d:gl_??\r\n", op); fflush(stderr);
      gl_dispatch(sd, op, (int) count, buf);
     sdl_free_binaries(sd);
  }
  // fprintf(stderr, "%s:%d: Eed %d\r\n", __FILE__,__LINE__,op); fflush(stderr);
  (*res) = sd->buff;
  return (ErlDrvSizeT) sd->len;
}

static ErlDrvSSizeT
sdl_driver_debug_control(ErlDrvData handle, unsigned op,
			 char* buf, ErlDrvSizeT count, char** res, ErlDrvSizeT res_size)
{
  sdl_data* sd = (sdl_data *) handle;
  sdl_fun func;
  int len;

  sd->buff = NULL;
  sd->len = 0;
  sd->op = op;
  if(op < OPENGL_START) {
     fprintf(stderr, "Command:%d:%s: ", op, sd->str_tab[op]);fflush(stderr);
     func = sd->fun_tab[op];
     func(sd, (int) count, buf);
     if ((len = sd->len) >= 0) {
	fprintf(stderr, "ok %d %p\r\n", len, sd->buff);fflush(stderr);
	(*res) = sd->buff;
	return (ErlDrvSizeT) len;
     } else {
	fprintf(stderr, "error\r\n");fflush(stderr);
	*res = 0;
	return -1;
     }     
  } else {
      fprintf(stderr, "Command:%d ", op);fflush(stderr);
      gl_dispatch(sd, op, count, buf);
      sdl_free_binaries(sd);
      fprintf(stderr, "\r\n");fflush(stderr);
      return 0;
  }
}

void sdl_send(sdl_data *sd, int len)
{
  if (sd->buff == NULL) {
    fprintf(stderr, "ESDL INTERNAL ERROR: sdl_send in %s sent NULL buffer: %d\r\n",
	    sd->str_tab[sd->op], len);
    abort();
  }
  if (len > sd->len) {
    fprintf(stderr, "ESDL INTERNAL ERROR: sdl_send in %s allocated %d sent %d\r\n",
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

char* sdl_get_temp_buff(sdl_data* sd, int size)
{
  if (size > TEMP_BINARY_SIZE) {
    return sdl_getbuff(sd, size);
  } else {
    ErlDrvBinary* bin = (ErlDrvBinary *) sd->temp_bin;
    driver_binary_inc_refc(bin);
    sd->buff = bin;
    sd->len = size;
    return bin->orig_bytes;
  }
}

void
sdl_util_debug(sdl_data *sd, int len, char* bp)
{
  if (*bp) {
    sdl_driver_entry.control = sdl_driver_debug_control;
  } else {
    sdl_driver_entry.control = sdl_driver_control;
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
    driver_binary_inc_refc(bin); /* Otherwise it could get deallocated */
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
