/*
 *  Copyright (c) 2010 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id$
 */

#include <stdio.h>
#include <string.h>
#ifndef _WIN32
#include <dlfcn.h>
#endif
#include "esdl.h"

int esdl_gl_initiated = 0;
ErlDrvTid esdl_thread;
ErlDrvMutex * esdl_batch_locker_m;
ErlDrvCond  * esdl_batch_locker_c;
SDL_Surface * esdl_init_screen;

void * esdl_gl_main_loop(void * );

typedef struct {
   ErlDrvTermData caller;
   int op;
   char *buff;
   int no_bins;
   char *base[3];
   ErlDrvBinary *bin[3];
} esdl_q_t;

#define MAX_Q 1024
int esdl_q_first, esdl_q_n;
esdl_q_t esdl_q[MAX_Q];

/* See wx/c_src/egl_impl.h */
typedef void (*ESDL_GL_DISPATCH) (int, char *, ErlDrvPort, ErlDrvTermData, char **);
ESDL_GL_DISPATCH esdl_gl_dispatch;

typedef int (*ESDL_GL_INIT) ();

/*** WIN32 ***/ 
#ifdef _WIN32
#define RTLD_LAZY 0
typedef HMODULE DL_LIB_P;

void * dlsym(HMODULE Lib, const char *func) 
{
    void * funcp;
    funcp = (void *) GetProcAddress(Lib, func);
    return funcp;
}

HMODULE dlopen(const char *DLL, int unused) 
{
    return LoadLibrary(DLL);
}

#else 
typedef void * DL_LIB_P;
#endif 

/** Initialization code **/ 
void es_init_opengl(sdl_data *sd, int len, char *bp) 
{
   DL_LIB_P LIBhandle;
   ESDL_GL_INIT init_opengl;

   char *start;
   
   start = sdl_get_temp_buff(sd, 2);
   
   if(esdl_gl_initiated == 0) {
      if((LIBhandle = dlopen(bp, RTLD_LAZY))) {
	 init_opengl = (ESDL_GL_INIT) dlsym(LIBhandle, "egl_init_opengl");
	 esdl_gl_dispatch = (ESDL_GL_DISPATCH) dlsym(LIBhandle, "egl_dispatch");
	 if(init_opengl && esdl_gl_dispatch) {
	    init_opengl();
	    start[0] = 1;
	    esdl_gl_initiated = 1;
	 } else {
	    fprintf(stderr, "In lib %s:\r\n", bp);
	    if(!init_opengl) 
	       fprintf(stderr, " function not found egl_init_opengl\r\n");
	    if(!esdl_gl_dispatch) 
	       fprintf(stderr, " function not found egl_dispatch\r\n");
	    fflush(stderr);
	    start[0] = 0;
	 }
      } else {
	 fprintf(stderr, "Failed locating lib %s:\r\n", bp);
	 fflush(stderr);
	 start[0] = 0;
      }
   } else {
      start[0] = 2;
   }
   sdl_send(sd, 1);
}

void gl_dispatch(sdl_data *sd, int op, int len, char *bp) 
{
   int i;
   if(!sd->use_smp) {  /* Not SMP invoke directly */
      char * bs[3];
      for(i=0; i<3; i++) {
	 bs[i] = sd->bin[i].base;
      }
      esdl_gl_dispatch(op, bp, sd->driver_data, driver_caller(sd->driver_data), bs);
   } else { 
      //printf("Dispatch cmd %d \r\n", op);
      /* SMP * copy everything and queue request to thread */
      int pos;
      erl_drv_mutex_lock(esdl_batch_locker_m);
      
      while(esdl_q_n == MAX_Q)  /* queue is full wait */
	 erl_drv_cond_wait(esdl_batch_locker_c, esdl_batch_locker_m);
      
      pos = (esdl_q_first + esdl_q_n) % MAX_Q;
      esdl_q[pos].op = op;
      esdl_q[pos].buff = driver_alloc(len);
      memcpy(esdl_q[pos].buff, bp, len);
      esdl_q[pos].caller = driver_caller(sd->driver_data);
      for(i=0; i< sd->next_bin; i++) {
	 esdl_q[pos].base[i] = sd->bin[i].base;
	 esdl_q[pos].bin[i]  = sd->bin[i].bin;
	 driver_binary_inc_refc(sd->bin[i].bin);
      }
      esdl_q[pos].no_bins = sd->next_bin;
      esdl_q_n++;
      erl_drv_cond_signal(esdl_batch_locker_c);
      erl_drv_mutex_unlock(esdl_batch_locker_m);
   }
}


void start_opengl_thread(sdl_data *sd) 
{
   esdl_batch_locker_m = erl_drv_mutex_create((char *)"esdl_batch_locker_m");
   esdl_batch_locker_c = erl_drv_cond_create((char *)"esdl_batch_locker_c");
   esdl_q_first = 0;
   esdl_q_n = 0;
   esdl_init_screen = (void *) -1;
   erl_drv_thread_create("ESDL OpenGL dispatcher", &esdl_thread,
			 esdl_gl_main_loop, (void *) sd, NULL);
}

void stop_opengl_thread() 
{   
   erl_drv_mutex_lock(esdl_batch_locker_m);
   esdl_q_n = -(esdl_q_n+1);
   erl_drv_cond_signal(esdl_batch_locker_c);
   erl_drv_mutex_unlock(esdl_batch_locker_m);   
   
   erl_drv_thread_join(esdl_thread, NULL);
   erl_drv_mutex_destroy(esdl_batch_locker_m);
   erl_drv_cond_destroy(esdl_batch_locker_c);
}

void * esdl_gl_main_loop(void *sd) {
   char * bs[3];
   int i,j,pos;
   ErlDrvPort port = ((sdl_data *)sd)->driver_data;
   erl_drv_mutex_lock(esdl_batch_locker_m);
   while(1) {
      if(esdl_q_n > 0) {
	 for(i=0; i<3; i++) {
	    bs[i] = esdl_q[esdl_q_first].base[i];
	 }
	 if(esdl_q[esdl_q_first].op >= OPENGL_START) {
	    esdl_gl_dispatch(esdl_q[esdl_q_first].op, esdl_q[esdl_q_first].buff,
			     port, esdl_q[esdl_q_first].caller, bs);
	 } else {
	    if(esdl_q[esdl_q_first].op == SDL_GL_SwapBuffersFunc) {	       
	       SDL_GL_SwapBuffers();
	    }
	    /* OpenGL must be initilized in the thread */
	    if(esdl_q[esdl_q_first].op == SDL_SetVideoModeFunc) {
	       esdl_init_screen = es_setVideoMode2(esdl_q[esdl_q_first].buff);
	    }
	 }
	 for(i=0; i < esdl_q[esdl_q_first].no_bins; i++)
	    driver_binary_dec_refc(esdl_q[esdl_q_first].bin[i]);
	 driver_free(esdl_q[esdl_q_first].buff);
	 esdl_q_first++;
	 esdl_q_first %= MAX_Q;
	 esdl_q_n--;
      } else {
	 erl_drv_cond_signal(esdl_batch_locker_c);
	 while(esdl_q_n == 0) {
	    erl_drv_cond_wait(esdl_batch_locker_c, esdl_batch_locker_m);
	 }
	 if(esdl_q_n < 0) { /* Time to quit */
	    esdl_q_n = -esdl_q_n-1;
	    break;
	 }
      }
   }
   /* Free all unused memory */
   for(i=0; i<esdl_q_n; i++) {
      pos = (esdl_q_first + i) % MAX_Q;
      driver_free(esdl_q[pos].buff);
      for(j=0; j < esdl_q[pos].no_bins; j++)
	 driver_binary_dec_refc(esdl_q[pos].bin[j]);      
   }
   erl_drv_mutex_unlock(esdl_batch_locker_m);
   erl_drv_thread_exit(NULL);
   return NULL;
}

SDL_Surface * gl_sync_set_video_mode() {
   SDL_Surface * screen;
   erl_drv_mutex_lock(esdl_batch_locker_m);
   while(esdl_init_screen == (void *) -1)
      erl_drv_cond_wait(esdl_batch_locker_c, esdl_batch_locker_m);
   screen = esdl_init_screen;
   erl_drv_mutex_unlock(esdl_batch_locker_m);
   esdl_init_screen = (void *) -1;
   return screen;
}
