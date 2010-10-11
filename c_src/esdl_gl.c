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

/* See wx/c_src/egl_impl.h */
typedef void (*ESDL_GL_DISPATCH) (int, char *, ErlDrvPort, ErlDrvTermData, char **);
ESDL_GL_DISPATCH esdl_gl_dispatch;

typedef int (*ESDL_GL_INIT) ();

#ifdef _WIN32
#define RTLD_LAZY 0
typedef HMODULE DL_LIB_P;

void * dlsym(HMODULE Lib, const char *func) {
    void * funcp;
    funcp = (void *) GetProcAddress(Lib, func);
    return funcp;
}

HMODULE dlopen(const char *DLL, int unused) {
    return LoadLibrary(DLL);
}

#else 
typedef void * DL_LIB_P;
#endif 

void es_init_opengl(sdl_data *sd, int len, char *bp) {
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

void gl_dispatch(sdl_data *sd, int op, char *bp) {
   char * bs[3];
   int i;
   for(i=0; i<3; i++) {
      bs[i] = sd->bin[i].base;
   }
   esdl_gl_dispatch(op, bp, sd->driver_data, driver_caller(sd->driver_data), bs);
}
