/* 
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id$
 */
/* Define function's */
#ifdef __cplusplus
    extern "C" {
#endif 

#ifndef SDL_H
#include <erl_driver.h>

#ifdef WIN32
#include <windows.h>  /* needed by Windows' gl.h etc */
#include <SDL.h>
#else
#include <SDL/SDL.h>
#endif


#ifdef _OSX_COCOA
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
       
/* Some new GL types (eleminates the need for glext.h) */
#ifndef GL_VERSION_1_5
#include <stddef.h>
/* GL types for handling large vertex buffer objects */
typedef ptrdiff_t GLintptr;
typedef ptrdiff_t GLsizeiptr;
#endif
#ifndef GL_ARB_shader_objects
/* GL types for handling shader object handles and characters */
typedef char GLcharARB;		     /* native character */
typedef unsigned int GLhandleARB;    /* shader object handle */
#endif


#include "esdl_conv.h"

#define MAXBUFF 8000000  /* Covers 1600x1200x4 (32bits) */

#define error() {fprintf(stderr, "Error in %s:%d \n\r", \
                                 __FILE__, __LINE__); \
                return;}

typedef struct sdl_data_def *sdl_data_ptr;
typedef void (*sdl_fun)(sdl_data_ptr, int, char*);

typedef sdl_fun (*sdl_load_fun)(void);

typedef struct {
  char* base;
  size_t size;
  ErlDrvBinary* bin;
} EsdlBinRef;

typedef struct sdl_data_def {
  void* driver_data;		/* Port or Driver specific data */
  sdl_fun* fun_tab;		/* Pointers to functions */
  char** str_tab;		/* Pointers to function names */

  int op;			/* Current (or last) function */
  int len;			/* Length of message buffer */
  void* buff;			/* Pointer to message buffer */

  void* temp_bin;		/* Temporary binary */
  EsdlBinRef bin[3];		/* Argument binaries */
  int next_bin;			/* Next binary */
#ifdef _OSX_COCOA
  void* release_pool;
  void* app;
#endif
} sdl_data;

void sdl_send(sdl_data *, int);
char* sdl_getbuff(sdl_data*, int);
char* sdl_get_temp_buff(sdl_data*, int);
void sdl_free_binaries(sdl_data*);

void init_fps(sdl_data*);
void init_glexts(sdl_data*);

/*   These must exactly match those in src/esdl.hrl */
#define SDL_H                20
#define VIDEO_H              30
#define EVENTS_H            100
#define MOUSE_H             110
#define KEYBOARD_H          120
#define ACTIVE_H            130
#define JOYSTICK_H          133
#define AUDIO_H             150
#define SDL_UTIL_H          180
#define OPENGL_H            200
#define OPENGLU_H           600
#define OPENGL_EXTS_H       700  /* Must be last */
#define MAX_FUNCTIONS_H    1023  /* Current Max.. Increase if needed */

#define SDL_InitFunc (SDL_H + 1)
#define SDL_QuitFunc (SDL_InitFunc + 1)
#define SDL_GetErrorFunc (SDL_QuitFunc + 1)

#include "esdl_video.h"
#include "esdl_events.h"
#include "esdl_audio.h"
#include "esdl_util.h"

void es_init(sdl_data *sd, int len, char * buff);
void es_quit(sdl_data *sd, int len, char * buff);
void es_getError(sdl_data *sd, int len, char *buff);

#endif

#ifdef __cplusplus
    }
#endif 
