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
-define(SDL_OPENGL_HRL,            200).
-define(SDL_OPENGLU_HRL,           600).
-define(SDL_OPENGL_EXTS_HRL,       700).   %/* Must be last */
-define(SDL_MAX_FUNCTIONS_HRL,    1023).   %/* Current Max.. Increase if needed */
-endif.
