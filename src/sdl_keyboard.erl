%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_keyboard.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created : 12 Jul 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(sdl_keyboard).

-include("esdl.hrl").

%%-compile(export_all).
-export([enableKeyRepeat/2,
	 enableUNICODE/1,
	 getKeyName/1,
	 getKeyState/0, 
	 getModState/0, 
	 setModState/1]).

-define(SDL_EnableUNICODE, ?SDL_KEYBOARD_HRL+1).
-define(SDL_EnableKeyRepeat, ?SDL_EnableUNICODE+1).
-define(SDL_GetKeyName, ?SDL_EnableKeyRepeat+1).
-define(SDL_GetKeyState, ?SDL_GetKeyName+1).
-define(SDL_GetModState, ?SDL_GetKeyState+1).
-define(SDL_SetModState, ?SDL_GetModState+1).

-import(sdl, [call/2,cast/2]).

%%%%%%%%%%%%%%%% KEYBOARD FUNCTIONS %%%%%%%%%%%%%%%%%%%
%% Func:  getKeyState
%% Args:  none
%% Returns:  A tuple continaing the state of each key(i.e. state of each Key) 
%% C-API func:  Uint8 * SDL_GetKeyState(int *numkeys);
getKeyState() ->
    Res = call(?SDL_GetKeyState, []),
    list_to_tuple(binary_to_list(Res)).

%% Func:  SDL_EnableUNICODE
%% Args:  true | false
%% Returns:  Previous Setting (true | False)
%% C-API func: int SDL_EnableUNICODE(int enable);
%% Desc:
enableUNICODE(Bool) ->
    B = if Bool == true -> 1; true -> 0 end,
    <<Res:8>> = call(?SDL_EnableUNICODE, [B]),
    Res.

%% Func:  SDL_EnableKeyRepeat
%% Args:  Delay Interval
%% Returns:  true | false (if failure)
%% C-API func: int SDL_EnableKeyRepeat(int delay, int interval);
%% Desc:  
enableKeyRepeat(Delay, Interval) ->
    <<Res:8>> = call(?SDL_EnableKeyRepeat, <<Delay:16,Interval:16>>),
    Res =:= 0.

%% Func:  SDL_GetKeyName
%% Args:  SDLKey 
%% Returns:  A string 
%% C-API func:  char * SDL_GetKeyName(SDLKey key);
%% Desc:
getKeyName(Key) ->
    binary_to_list(call(?SDL_GetKeyName, <<Key:16>>)).

%% Func:  getModState
%% Args:  none
%% Returns:  KModState (see KMOD_* in sdl_keyboard.hrl)
%% C-API func:  SDLMod SDL_GetModState(void);
getModState() ->
    case call(?SDL_GetModState, []) of
	[] -> 0;
	<<Res:16>> -> Res
    end.

%% Func: setModState 
%% Args: KModState (see KMOD_* in sdl_keyboard.hrl)
%% Returns: ok 
%% C-API func: void SDL_SetModState(SDLMod modstate);
setModState(ModState) ->
    cast(?SDL_SetModState, <<ModState:16>>).


