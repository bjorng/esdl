%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl.erl
%%% Author  : Dan Gudmundsson <dgud@users.sf.net>
%%% Purpose : To give access to the Simple Direct Media Layer
%%%           by Sam Lantinga, see www.libsdl.org.
%%% Created : 23 Jun 2000 by Dan Gudmundsson <dgud@users.sf.net>
%%%----------------------------------------------------------------------

-module(sdl).

-include("sdl.hrl").
-include("esdl.hrl").
-include("sdl_util.hrl").

-export([init/1, quit/0, getError/0]).
-export([cast/2, call/2]).
-export([send_bin/1, send_bin/3]).

%% SDL functions
%%% Functions  

%% Func:  init
%% Args:  BitMask
%% Returns:  WrapperRef
%% C-API func: int SDL_Init(Uint32 flags);
%% Desc:  Initializes the SDL (including the erlang specific parts)
init(Flags) when is_integer(Flags) ->
    Path = case code:priv_dir(esdl) of
	       P when is_list(P) -> 
		   P;
	       {error, _} ->  %% in case you use erl -pa ../ebin priv_dir don't work :-(		   
		   case code:is_loaded(?MODULE) of
		       {file, SDLPath} -> 
			   strip(SDLPath, "ebin/" ++ atom_to_list(?MODULE) ++ ".beam") ++ "priv/";
		       _ ->  %% For debugging 
			   atom_to_list(c:pwd()) ++ "../priv/"
		   end
	   end,
    case os:type() of
	{win32, _} ->
	    OsPath = Path ++ ";" ++ os:getenv("PATH"),
	    os:putenv("PATH", OsPath);
	_ ->
	    ignore
    end,
    case catch erl_ddll:load_driver(Path, "sdl_driver") of
	ok -> 
	    ok;
	{error, R} ->
	    io:format("Driver failed: ~s ~n", [erl_ddll:format_error(R)]);
	Other ->
	    io:format("Driver crashed: ~p ~n", [Other])
    end,
    Port = open_port({spawn, "sdl_driver"}, [binary]),
    register(esdl_port, Port), 
    cast(?SDL_Init, <<Flags:32/native>>),
    Port.

%% Func:  quit
%% Args:  none
%% Returns:  Quits the wrapper
%% C-API func: void SDL_Quit(void);
quit() ->    
%%    cast(?SDL_Quit, []),
    erlang:port_close(esdl_port),
    erl_ddll:unload_driver("sdl_driver").

%% Func:  getError
%% Args:  none
%% Returns:  DescString
%% C-API func: char * SDL_GetError(void);
getError() ->
    Bin = call(?SDL_GetError, []),
    binary_to_list(Bin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cast(Op, Arg) ->
    erlang:port_control(esdl_port, Op, Arg),
    ok.

call(Op, Arg) ->
    erlang:port_control(esdl_port, Op, Arg).

send_bin(Bin) when is_binary(Bin) ->
    erlang:port_command(esdl_port, Bin).

send_bin(#sdlmem{bin=Bin}, _, _) -> send_bin(Bin);
send_bin(Bin, _, _) when is_binary(Bin) -> send_bin(Bin);
send_bin(Term, Mod, Line) -> erlang:error({Mod,Line,unsupported_type,Term}).

%%%%%%%%%%%%%%%%% NON SDL FUNCTIONS %%%%%%%%%%%%%%%%

strip(Src, Src) ->
    [];
strip([H|R], Src) ->
    [H| strip(R, Src)].
