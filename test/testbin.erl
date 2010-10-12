%%  Copyright (c) 2001 Dan Gudmundsson
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : testbin.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created : 12 Sep 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(testbin).
-author('dgud@erix.ericsson.se').

-compile(export_all).
%%-export([Function/Arity, ...]).
-include("sdl.hrl").

init() ->
    go().

go() ->
    Wrapper = sdl:init(?SDL_INIT_VIDEO),
    io:format("Wrapper ~p~n", [Wrapper]),    
    F32 = sdl_util:malloc(7, ?SDL_FLOAT),
    F64 = sdl_util:malloc(7, ?SDL_DOUBLE),
    Args = [0.0, 1.0, 1000000.0, 0.000001, -1.0, -1000000.0, -0.000001],
    io:format("E Writing ~f ~f ~f ~f ~f ~f ~f~n", Args),
    sdl_util:write(F32, Args),
    sdl_util:write(F64, Args),
    io:format("Reading~n"),
    
    List32 = sdl_util:read(F32, length(Args)),
    io:format("E Read32 ~f ~f ~f ~f ~f ~f ~f~n", List32),
    List64 = sdl_util:read(F64, length(Args)),
    io:format("E Read64 ~f ~f ~f ~f ~f ~f ~f~n", List64),
    ok.
