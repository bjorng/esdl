%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_joystick.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created : 19 Apr 2001 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-define( SDL_HAT_CENTERED,	16#00).
-define( SDL_HAT_UP,		16#01).
-define( SDL_HAT_RIGHT,		16#02).
-define( SDL_HAT_DOWN,		16#04).
-define( SDL_HAT_LEFT,		16#08).
-define( SDL_HAT_RIGHTUP,	(?SDL_HAT_RIGHT bor ?SDL_HAT_UP)).
-define( SDL_HAT_RIGHTDOWN,	(?SDL_HAT_RIGHT bor ?SDL_HAT_DOWN)).
-define( SDL_HAT_LEFTUP,	(?SDL_HAT_LEFT  bor ?SDL_HAT_UP)).
-define( SDL_HAT_LEFTDOWN,	(?SDL_HAT_LEFT  bor ?SDL_HAT_DOWN)).

