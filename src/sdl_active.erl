%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_active.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created : 12 Jul 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(sdl_active).

-include("sdl_events.hrl").
-include("esdl.hrl").

-export([getAppState/0, 
	 quitRequested/0]).

-define(SDL_GetAppState, ?SDL_ACTIVE_HRL+1).

-import(sdl, [call/2]).

%% Func:  quitRequested
%% Args:  none
%% Returns:  true | false
%% C-API func: SDL_QuitRequested()
quitRequested() ->
    sdl_events:pumpEvents(),
    case sdl_events:peepEvents(0, ?SDL_PEEKEVENT, ?SDL_QUITMASK) of
	[] -> false;
	_  -> true
    end.

%% Func:  getAppState
%% Args:  none
%% Returns:  State (bitmask)
%% C-API func: Uint8 SDL_GetAppState(void);
getAppState() ->
    <<Res:8>> = call(?SDL_GetAppState, []),
    Res.
