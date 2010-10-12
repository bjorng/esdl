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

-define(GL_BYTE_SIZE, 8).
-define(GL_UNSIGNED_BYTE_SIZE, 8).
-define(GL_SHORT_SIZE, 16).
-define(GL_UNSIGNED_SHORT_SIZE, 16).
-define(GL_INT_SIZE, 32).
-define(GL_UNSIGNED_INT_SIZE, 32).
-define(GL_FLOAT_SIZE, 32).
-define(GL_DOUBLE_SIZE, 64).

-define(gl_type_size(TYPE), 
	case (TYPE) of 
	    16#1400 ->           ?GL_BYTE_SIZE;
	    16#1401 ->  ?GL_UNSIGNED_BYTE_SIZE;
	    16#1402 ->          ?GL_SHORT_SIZE;
	    16#1403 -> ?GL_UNSIGNED_SHORT_SIZE;
	    16#1404 ->            ?GL_INT_SIZE;
	    16#1405 ->   ?GL_UNSIGNED_INT_SIZE;
	    16#1406 ->          ?GL_FLOAT_SIZE;
	    16#140A ->         ?GL_DOUBLE_SIZE
	end).

-define(SDL_BYTE, 16#1400).
-define(SDL_UNSIGNED_BYTE, 16#1401).
-define(SDL_SHORT, 16#1402).
-define(SDL_UNSIGNED_SHORT, 16#1403).
-define(SDL_INT, 16#1404).
-define(SDL_UNSIGNED_INT, 16#1405).
-define(SDL_FLOAT, 16#1406).
-define(SDL_2_BYTES, 16#1407).
-define(SDL_3_BYTES, 16#1408).
-define(SDL_4_BYTES, 16#1409).
-define(SDL_DOUBLE, 16#140A).
-define(SDL_DOUBLE_EXT, 16#140A).
