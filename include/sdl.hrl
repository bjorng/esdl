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
	    ?GL_BYTE ->           ?GL_BYTE_SIZE;
	    ?GL_UNSIGNED_BYTE->   ?GL_UNSIGNED_BYTE_SIZE;
	    ?GL_SHORT ->          ?GL_SHORT_SIZE;
	    ?GL_UNSIGNED_SHORT -> ?GL_UNSIGNED_SHORT_SIZE;
	    ?GL_INT ->            ?GL_INT_SIZE;
	    ?GL_UNSIGNED_INT ->   ?GL_UNSIGNED_INT_SIZE;
	    ?GL_FLOAT ->          ?GL_FLOAT_SIZE;
	    ?GL_DOUBLE ->         ?GL_DOUBLE_SIZE
	end).

-ifndef(GL_BYTE).
-define(GL_BYTE, 16#1400).
-define(GL_UNSIGNED_BYTE, 16#1401).
-define(GL_SHORT, 16#1402).
-define(GL_UNSIGNED_SHORT, 16#1403).
-define(GL_INT, 16#1404).
-define(GL_UNSIGNED_INT, 16#1405).
-define(GL_FLOAT, 16#1406).
-define(GL_2_BYTES, 16#1407).
-define(GL_3_BYTES, 16#1408).
-define(GL_4_BYTES, 16#1409).
-define(GL_DOUBLE, 16#140A).
-define(GL_DOUBLE_EXT, 16#140A).
-endif.
