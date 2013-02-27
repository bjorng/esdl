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
#include "SDL.h"
#include "SDL_byteorder.h"
#include "SDL_events.h"
#include "SDL_syswm.h"
#else
#include "SDL/SDL.h"
#include "SDL/SDL_byteorder.h"
#include "SDL/SDL_events.h"
#include "SDL/SDL_syswm.h"
#endif

#include "esdl_conv.h"

#define MAXBUFF 8000000  /* Covers 1600x1200x4 (32bits) */

#define error() {fprintf(stderr, "Error in %s:%d \n\r", __FILE__, __LINE__); return;}

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
  int use_smp;                  /* Use a thread for opengl commands */
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
void gl_dispatch(sdl_data *, int, ErlDrvSizeT, char *);
void es_init_opengl(sdl_data *, int, char *);
void start_opengl_thread(sdl_data *);
void stop_opengl_thread();
void * esdl_gl_sync();

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
#define TTF_H               200
#define IMG_H               300
#define MAX_FUNCTIONS_H     400  /* Current Max.. Increase if needed */

#define OPENGL_START        5000 /* see wx/c_src/wxe_driver.h */

#define SDL_InitFunc (SDL_H + 1)
#define SDL_QuitFunc (SDL_InitFunc + 1)
#define SDL_GetErrorFunc (SDL_QuitFunc + 1)
#define ESDL_OpenglInitFunc (SDL_GetErrorFunc + 1)

#include "esdl_video.h"
#include "esdl_events.h"
#include "esdl_audio.h"
#include "esdl_util.h"
#include "esdl_ttf.h"
#include "esdl_img.h"

void es_init(sdl_data *sd, int len, char * buff);
void es_quit(sdl_data *sd, int len, char * buff);
void es_getError(sdl_data *sd, int len, char *buff);

void esdl_init_native_gui();

#endif

#ifdef __cplusplus
    }
#endif 
