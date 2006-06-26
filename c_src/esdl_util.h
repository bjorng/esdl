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

#include <string.h>

#define getPointer(bp)       ((void *) get32be((bp)))
#define putPointer(bp, ptr)  put32be((bp), ((int) (ptr)))


/* We always use 8 bytes to store pointers */

#define POPGLPTR(dstp, srcp) \
  do { memcpy(&dstp,srcp,sizeof(void *)); srcp += 8; } while (0)
#define PUSHGLPTR(srcp,dstp) \
  do { memset(dstp,0,8);memcpy(dstp,&srcp,sizeof(void *)); dstp += 8; } while (0)

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
