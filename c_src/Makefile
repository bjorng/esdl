#  Copyright (c) 2001 Dan Gudmundsson
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
#     $Id$
#

OS_FLAG = unix

ifeq ($(OS_FLAG), mingw)
include ../win32_conf/mingw_vars.mk
else

  OGLDIR  = /usr

# Uncomment these if your opengl header is located in another directory than
# the usual $(OGLDIR)/include/GL/gl.h
#GL_INCLUDE_DIR      = OpenGL
#USE_GL_INCLUDE_DIR  = -DFULL_PATH_TO_GL_INCLUDE_DIR

  GL_LIBS		= -L$(OGLDIR)/lib -lGL -lGLU 
  GL_INCS		= -I$(OGLDIR)/include/$(GL_INCLUDE_DIR)

  SDL_LIBS		= $(shell sdl-config --libs)
  SDL_INCS		= $(shell sdl-config --cflags)

ifeq ($(ENABLE_SDL_TTF), yes)
  SDL_TTF_LIBS          = -lSDL_ttf
  SDL_TTF_INCS          =                     # specify if different from SDL
  SDL_TTF_HELPER        = esdl_ttf
  USE_SDL_TTF           = -D_USE_SDL_TTF
endif

ifeq ($(ENABLE_SDL_IMAGE), yes)
  SDL_IMAGE_LIBS        = -lSDL_image
  SDL_IMAGE_INCS        =                     # specify if different from SDL
  SDL_IMAGE_HELPER      = esdl_img
  USE_SDL_IMAGE         = -D_USE_SDL_IMAGE
endif

  LIBS			= $(GL_LIBS) $(SDL_LIBS) $(SDL_TTF_LIBS) \
                          $(SDL_IMAGE_LIBS) -lm 
  INCS                  = $(GL_INCS) $(SDL_INCS)  $(SDL_TTF_INCS) \
                          $(SDL_IMAGE_INCS) $(ERL_INCS)

  CFLAGS		= -g -O2 -funroll-loops -Wall -ffast-math -fpic \
			-fomit-frame-pointer $(USE_GL_INCLUDE_DIR) -DSHM \
                        $(USE_SDL_TTF) $(USE_SDL_IMAGE) \
			-DPTHREADS $(INCS)

  CLINKFLAGS 		=  -fpic -shared  

  SOEXT = so

## The driver part needs erlang includes
  ERL_DIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),halt().' | erl | grep '1>' | sed 's,^[0-9]*> *,,g')
  ERL_INCS = -I$(ERL_DIR)/usr/include

  CC = gcc

endif

TARGETDIR = ../priv

GCC_O	= $(CC) -c $(CFLAGS) $<

# Files

Helpers		= esdl_wrapper esdl_gen esdl_spec esdl_util \
			esdl_video esdl_events esdl_audio \
			esdl_gl esdl_glu esdl_glext \
			$(SDL_TTF_HELPER) $(SDL_IMAGE_HELPER)

HelpersObjects	= $(Helpers:%=%.o)

# Targets

target:  $(TARGETDIR)/sdl_driver.$(SOEXT)

clean:  
	rm -f $(HelpersObjects) 
	rm -f ../priv/*.$(SOEXT) ../priv/sdlwrapper *.o
	rm -f *~ erl_crash.dump

%.o: %.c
	$(GCC_O)


$(TARGETDIR)/sdl_driver.$(SOEXT): esdl_driver.c $(HelpersObjects)
	$(CC) $(CFLAGS) $(CLINKFLAGS) $(ERL_INCS) $< $(HelpersObjects) \
		$(LIBS) -o $@	



