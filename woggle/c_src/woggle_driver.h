/* 
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id$
 */
#ifndef _WOGGLE_DRIVER_H
#define _WOGGLE_DRIVER_H
#include <erl_driver.h>

#ifndef FLAVOUR_WOGGLE
#define FLAVOUR_WOGGLE 1
#endif
#define SDL_BIGENDIAN 0
#define SDL_LITTLEENDIAN 1
#define SDL_BYTEORDER SDL_LITTLEENDIAN

#define WOGGLE_VERSION_MAJOR 0
#define WOGGLE_VERSION_MINOR 1
#define WOGGLE_VERSION_PATCH 0

typedef unsigned char Uint8;
typedef unsigned int Uint32;

#ifdef WIN32
#include <windows.h>  
#include <gl/gl.h>
#include "woggle_win.h"
#endif

/*
 * It seems I need to  steal more and more from esdl.h...
 */

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

#include <esdl_conv.h>

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
    int extensions_loaded;

    HANDLE qh;

    WogWindowData wd;
    WogEventMessage *saved_event; /* Used by compatibility interface */
    int save_x,save_y;            /* Used by compat IF */
    int op;			/* Current (or last) function */
    int len;			/* Length of message buffer */
    void* buff;			/* Pointer to message buffer */
    
    void* temp_bin;		/* Temporary binary */
    EsdlBinRef bin[3];		/* Argument binaries */
    int next_bin;		/* Next binary */
} sdl_data;

void sdl_send(sdl_data *, int);
char* sdl_getbuff(sdl_data*, int);
char* sdl_get_temp_buff(sdl_data*, int);
void sdl_free_binaries(sdl_data*);

void init_fps(sdl_data*);
void init_glexts(sdl_data*);

/*   These must exactly match those in src/esdl.hrl */
#if 0
#define SDL_H                20
#endif
#define VIDEO_H              30
#define EVENTS_H            100

/* This is how they are in SDL */
#if 0 
#define MOUSE_H             110
#define KEYBOARD_H          120
#define ACTIVE_H            130
#define JOYSTICK_H          133
#define AUDIO_H             150
#endif

/* Using same as JOYSTICK_H, will never implement Joy and audio */
#define WOGGLE_IF_H         133

#define SDL_UTIL_H          180
#define OPENGL_H            200
#define OPENGLU_H           600
#define OPENGL_EXTS_H       700  /* Must be last */
#define MAX_FUNCTIONS_H    1023  /* Current Max.. Increase if needed */

#define SDL_InitFunc (SDL_H + 1)
#define SDL_QuitFunc (SDL_InitFunc + 1)
#define SDL_GetErrorFunc (SDL_QuitFunc + 1)

#if 0
#include "esdl_video.h"
#include "esdl_events.h"
#include "esdl_audio.h"
#endif
#include <esdl_util.h>

void es_init(sdl_data *sd, int len, char * buff);
void es_quit(sdl_data *sd, int len, char * buff);
void es_getError(sdl_data *sd, int len, char *buff);

#endif

