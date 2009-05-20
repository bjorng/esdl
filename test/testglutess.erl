%%  Copyright (c) 2001 Dan Gudmundsson
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : testgl.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created : 11 Sep 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(testglutess).
-author('dgud@erix.ericsson.se').
-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").
-include("glu.hrl").

-export([go/0, go/1, drawBox/2]).
-compile(export_all).
init() ->
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER bor ?SDL_INIT_NOPARACHUTE).

go() ->
    go([]).
go(Config) ->
    %% Init 
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER bor ?SDL_INIT_NOPARACHUTE),
    sdl_util:debug(1),
    sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),
    SR = sdl_video:setVideoMode(640, 480, 16, ?SDL_OPENGL),
    sdl_events:eventState(?SDL_ALLEVENTS ,?SDL_IGNORE),
    sdl_events:eventState(?SDL_KEYDOWN ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_QUIT ,?SDL_ENABLE),
    initWin(),
    test_triangulate(),
    
    Cube = [{ 1,   1,      -0.5}, 
	    { 0.5, 0,      -0.5},
	    { 1,   0,      -0.5},
	    { 1,  -1,      -0.5},
	    { 0,   0,      -0.5},
	    { -1,  0,      0},
	    { -1,  1,      0},
	    {-0.5, 0.5,   0},
	    { 0,   -0.5,     0}],
    
    {Time, N} = timer:tc(?MODULE, drawBox, [Cube, 0]),
    erlang:display(N),
    Secs = Time / 1000000,
    sdl:quit(),       
    erlang:display(round(N/Secs)),
    N.

initWin() ->
    gl:viewport(0,0,640,480),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:ortho( -2.0, 2.0, -2.0, 2.0, -20.0, 20.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),    
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    gl:clearColor(0.0,0.0,0.0,1.0).

drawBox(Cube, N) ->
    %%timer:sleep(30),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:color3f(1,1,1),

    Tess = glu:newTess(),
    glu:tessCallback(Tess, ?GLU_TESS_BEGIN,   ?ESDL_TESSCB_GLBEGIN),
    glu:tessCallback(Tess, ?GLU_TESS_END,     ?ESDL_TESSCB_GLEND),
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX,  ?ESDL_TESSCB_GLVERTEX),
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX_DATA,  ?ESDL_TESSCB_VERTEX_DATA),
    glu:tessCallback(Tess, ?GLU_TESS_ERROR,   ?ESDL_TESSCB_ERROR_PRINT),
    glu:tessCallback(Tess, ?GLU_TESS_COMBINE, ?ESDL_TESSCB_COMBINE),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    glu:tessNormal(Tess, 0, 0, -1),
    lists:foreach(fun(V) -> glu:tessVertex(Tess, V, [{color, myabs(V)}]) end, Cube),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess),
    gl:disable(?GL_DEPTH_TEST),
    gl:color3f(1,0,0),
    %% timer:sleep(200),
    case {gl:getError(), sdl:getError()} of
	{0, ""} ->
	    ok;
	{GL, SDL} ->
	    io:format("Errors Reported ~p ~s~n", [GL, SDL])
    end,
    gl:swapBuffers(),
    case check_event() of
	ok ->
	    drawBox(Cube, N+1);
	quit ->
	    N
    end.

myabs({R,G,B}) -> {abs(R),abs(G), abs(B)}.
         
check_event() ->
    case sdl_events:pollEvent() of 
	#quit{} -> 
	    quit;
	no_event -> 
	    ok;
	Quit when is_record(Quit, keyboard) -> 
	    if 
		Quit#keyboard.sym == ?SDLK_ESCAPE ->
		    quit;
		Quit#keyboard.sym == ?SDLK_q ->
		    quit;
		true -> 
		    io:format("Got event ~p~n", [Quit]),
		    ok
	    end;		    
	{0, []} -> 
	    io:format(".", []),
	    ok;
	{NE, Evs} ->
	    io:format("Got ~p events: ~p~n", [NE, Evs]),
	    ok;
	Event -> 
	    io:format("Got event ~p~n", [Event]),
	    ok
    end.
    

test_triangulate() ->
    %% Used to crash.
    N = {0.00000e+0,-1.00000,0.00000e+0},
    Ps = [{-3.92889e-2,1.00000,0.00000e+0},
	  {0.923880,1.00000,-0.382683},
	  {-2.77814e-2,1.00000,2.77814e-2},
	  {0.382683,1.00000,-0.923880},
	  {7.21725e-18,1.00000,3.92889e-2},
	  {-0.382683,1.00000,-0.923880},
	  {2.77814e-2,1.00000,2.77814e-2},
	  {-0.923880,1.00000,-0.382683},
	  {3.92889e-2,1.00000,-4.81150e-18},
	  {-0.923880,1.00000,0.382683},
	  {2.77814e-2,1.00000,-2.77814e-2},
	  {-0.382683,1.00000,0.923880},
	  {-2.40575e-18,1.00000,-3.92889e-2},
	  {0.382683,1.00000,0.923880},
	  {-2.77814e-2,1.00000,-2.77814e-2},
	  {0.923880,1.00000,0.382683}],
    io:format("~p\n", [glu:triangulate(N, Ps)]),
    ok.

