%%  Copyright (c) 2001 Dan Gudmundsson
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : testaudio.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : Test the audio functionality
%%% Created : 21 Sep 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(testaudio).
-author('dgud@erix.ericsson.se').

-compile(export_all).
%%-export([Function/Arity, ...]).

-include("sdl.hrl").

go() ->
    sdl:init(?SDL_INIT_AUDIO),
    
    {ASpec,Sample} = sdl_audio:loadWAV("Beep.wav"),
    Obtained = sdl_audio:openAudio(ASpec, true),
    io:format("Driver: ~s\n", [sdl_audio:audioDrivername()]),
    io:format("Obtained: ~p\n", [Obtained]),
    sdl_audio:play_audio(Sample, 3),
    sdl_audio:pauseAudio(false),
    timer:sleep(2500),
    sdl_audio:pauseAudio(true),
    sdl_audio:freeWAV(Sample),
    timer:sleep(500),
    sdl_audio:closeAudio(),    
    sdl:getError().    
