%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_mouse.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : Mouse related functions
%%% Created : 12 Jul 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(sdl_mouse).

-include("esdl.hrl").
-include("sdl_mouse.hrl").
-include("sdl_util.hrl").

-export([createCursor/6,
	 freeCursor/1,
	 getCursor/0,
	 getMouseState/0, 
	 getRelativeMouseState/0,
	 setCursor/1,
	 showCursor/1,
	 warpMouse/2
	]).

-import(sdl, [call/2,cast/2]).

-define(SDL_GetMouseState, ?SDL_MOUSE_HRL+1).
-define(SDL_GetRelativeMouseState, ?SDL_GetMouseState+1).
-define(SDL_WarpMouse, ?SDL_GetRelativeMouseState +1).
-define(SDL_CreateCursor, ?SDL_WarpMouse +1).
-define(SDL_SetCursor,  ?SDL_CreateCursor+1).
-define(SDL_GetCursor,  ?SDL_SetCursor+1).
-define(SDL_FreeCursor, ?SDL_GetCursor+1).
-define(SDL_ShowCursor, ?SDL_FreeCursor+1).

%%%%%%%%%%%%%%%%%%%%% MOUSE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%
%% Func:  getMouseState
%% Args:  none
%% Returns:  {MouseState, X, Y}
%% C-API func:  Uint8 SDL_GetMouseState(int *x, int *y);
%% Desc:  
getMouseState() ->
    <<State:8,X:16,Y:16>> = call(?SDL_GetMouseState, []),
    {State,X,Y}.

%% Func:  getRelativeMouseState
%% Args:  none
%% Returns:  {MouseState, X, Y}
%% C-API func: Uint8 SDL_GetRelativeMouseState(int *x, int *y);
%% Desc:  
getRelativeMouseState() ->
    <<State:8,X:16,Y:16>> = call(?SDL_GetRelativeMouseState, []),
    {State,X,Y}.

%% Func:  warpMouse
%% Args:  X, Y
%% Returns:  ok 
%% C-API func: void SDL_WarpMouse(Uint16 x, Uint16 y);
%% Desc:  
warpMouse(X, Y) ->
    cast(?SDL_WarpMouse, <<X:16/native,Y:16/native>>).

%% Func:  createCursor
%% Args:  Data (Binary), Mask(Binary), W, H, HotX, HotY
%% Returns:  CursorRef
%% C-API func:  SDL_Cursor *SDL_CreateCursor
%%		(Uint8 *data, Uint8 *mask, int w, int h, int hot_x, int hot_y);
%% Desc:  Data & Mask must be less than 32*32 bytes.
createCursor(Data, Mask, W, H, HotX, HotY) when is_binary(Data), is_binary(Mask) ->
    case call(?SDL_CreateCursor,
	      <<W:16/native,H:16/native,HotX:16/native,HotY:16/native,
	       (size(Data)):16/native,Data/binary,Mask/binary>>) of
	<<0:64>> ->
	    exit({createCursor, returned_null});
	<<Ptr:?_PTR>> ->
	    {cursorp,Ptr}
    end.

%% Func:  setCursor
%% Args:  CursorRef
%% Returns:  ok
%% C-API func:  void SDL_SetCursor(SDL_Cursor *cursor);
setCursor({cursorp,Ref}) -> 
    cast(?SDL_SetCursor, <<Ref:?_PTR>>).

%% Func:  getCursor
%% Args:  none
%% Returns:  A cursorRef
%% C-API func:  void SDL_SetCursor(SDL_Cursor *cursor);
getCursor() ->  
    case call(?SDL_GetCursor, []) of
	<<0:64>> ->
	    exit({getCursor, returned_null});
	<<Ptr:?_PTR>> ->
	    {cursorp, Ptr}
    end.

%% Func:  freeCursor
%% Args:  CursorRef
%% Returns:  ok
%% C-API func:  void SDL_FreeCursor(SDL_Cursor *cursor);
freeCursor({cursorp,Ref}) -> 
    cast(?SDL_FreeCursor, <<Ref:?_PTR>>).

%% Func:  showCursor
%% Args:  true | false
%% Returns:  true | false (if cursor was displayed before the call)
%% C-API func:  int SDL_ShowCursor(int toggle);
showCursor(Bool) -> 
    B = case Bool of
	    false -> 0;
	    true -> 1
	end,
    call(?SDL_ShowCursor, [B]),
    receive 
	{'_esdl_result_', Res} ->
	    Res =:= 1
    end.
