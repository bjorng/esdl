%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%----------------------------------------------------------------------
%%% File    : esdl.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created :  6 Oct 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-ifndef(SDL_HRL).  %% These must exactly match those in c_src/esdl.h

-define(SDL_HRL,                    20).
-define(SDL_VIDEO_HRL,              30).
-define(SDL_EVENTS_HRL,            100).
-define(SDL_MOUSE_HRL,             110).
-define(SDL_KEYBOARD_HRL,          120).
-define(SDL_ACTIVE_HRL,            130).
-define(SDL_JOYSTICK_HRL,          133).
-define(SDL_AUDIO_HRL,             150).
-define(SDL_UTIL_HRL,              180).
-define(SDL_TTF_HRL,               200).
-define(SDL_IMG_HRL,               300).
-define(SDL_MAX_FUNCTIONS_HRL,     400).   %/* Current Max.. Increase if needed */

-define(SDL_Init, ?SDL_HRL + 1).
-define(SDL_Quit, ?SDL_Init + 1).
-define(SDL_GetError, ?SDL_Quit +1).
-define(ESDL_Init_Opengl, ?SDL_GetError +1).

-endif.
