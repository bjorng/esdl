/*
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 * 
 *     $Id$
 */

#ifdef __cplusplus
    extern "C" {
#endif 

#define getPointer(bp)       ((void *) get32be((bp)))
#define putPointer(bp, ptr)  put32be((bp), ((int) (ptr)))

#define SDL_UTIL_copySdlImage2GLArrayFunc (SDL_UTIL_H+1)
void copySdlImage2GLArray(sdl_data *, int, char *);
#define SDL_UTIL_DebugFunc (SDL_UTIL_H+2)
void sdl_util_debug(sdl_data *sd, int len, char * buff);

#define mygl_allocFunc        (SDL_UTIL_H+3)
void mygl_alloc(sdl_data *, int, char *);
#define mygl_writeFunc        (SDL_UTIL_H+4)
void mygl_write(sdl_data *, int, char *);

#ifdef __cplusplus
    }
#endif 
