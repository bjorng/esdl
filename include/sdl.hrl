%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%  Defines for SDL 
%%%  see documentation for SDL and SDL/sdl.h
%%%
%%%  By Dan Gudmundsson
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sdl.h

-define(SDL_INIT_TIMER,       16#00000001).
-define(SDL_INIT_AUDIO,       16#00000010).
-define(SDL_INIT_VIDEO,       16#00000020).
-define(SDL_INIT_CDROM,       16#00000100).
-define(SDL_INIT_JOYSTICK,    16#00000200).
-define(SDL_INIT_NOPARACHUTE, 16#00100000).
-define(SDL_INIT_EVENTTHREAD, 16#01000000).  %% Don't work on windows and in the driver
-define(SDL_INIT_EVERYTHING,  16#0000FFFF).

-define(SDL_INIT_NOERLDRIVER, 16#02000000).  %% Don't use the linked in driver
-define(SDL_INIT_ERLDRIVER,   16#04000000).  %% Use the linked driver

-define(printError(), 
	fun([]) -> ignore;
	   (Estr) ->
		io:format("SDL Error in ~p ~p: " ++ Estr ++"~n", 
			  [?MODULE, ?LINE]) 
	end (sdl:getError())).
