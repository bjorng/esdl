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

-module(testgl).
-author('dgud@erix.ericsson.se').
-include_lib("wx/include/gl.hrl").
-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").

-export([go/0, go/1]).

go() ->
    go([]).
go(Config) ->
    %% Init 
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER bor
	     ?SDL_INIT_NOPARACHUTE),
    sdl_util:debug(2),
    Flags = 
	case lists:member(fullscreen, Config) of 
	    true ->
		?SDL_OPENGL  bor ?SDL_FULLSCREEN;
	    _ -> 
		?SDL_OPENGL  bor ?SDL_RESIZABLE
	end,
    sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),

    AvailableWindowedSzs = sdl_video:listModes(null, Flags bor ?SDL_FULLSCREEN),    
    DriverName = sdl_video:videoDriverName(),
    
    io:format("Driver ~p ~n", [DriverName]),
    io:format("Available WindowSizes ~p ~n", [AvailableWindowedSzs]),

    case AvailableWindowedSzs of
	[{_, 0,0,W,H}|_] ->
	    Res = [Test || Test <- [32,24,16,15],
			   true == sdl_video:videoModeOK(W,H,Test,Flags)],
	    io:format("A guess at max video res is ~px~p:~p ~n", [W,H, hd(Res)]);
	_ ->
	    io:format("Can't guess max resolution~n", [])
    end,

    SR = sdl_video:setVideoMode(640, 480, 16, Flags),
    Rs= sdl_video:gl_getAttribute(?SDL_GL_RED_SIZE),
    Gs= sdl_video:gl_getAttribute(?SDL_GL_GREEN_SIZE),
    Bs= sdl_video:gl_getAttribute(?SDL_GL_BLUE_SIZE),
    Ds= sdl_video:gl_getAttribute(?SDL_GL_DEPTH_SIZE),
    Db= (1 == sdl_video:gl_getAttribute(?SDL_GL_DOUBLEBUFFER)),
    io:format("OpenGL attributes ~n"),
    io:format("Sizes in bits Red ~p Green ~p Blue ~p Depth ~p Doublebuffered ~p~n",
	      [Rs, Gs, Bs, Ds, Db]),   
    io:format("Vendor:     ~s~n",  [gl:getString(?GL_VENDOR)]),
    io:format("Renderer:   ~s~n",  [gl:getString(?GL_RENDERER)]),
    io:format("Version:    ~s~n",  [gl:getString(?GL_VERSION)]),
    io:format("GL AUX BUFFERS ~p~n",  [hd(gl:getIntegerv(?GL_AUX_BUFFERS))]),
    io:format("SDL Version ~p~n",  [sdl_video:wm_getInfo()]),

    io:format("Extensions: ~s~n",  [gl:getString(?GL_EXTENSIONS)]),    
    io:format("Maximized: ~p~n",   [sdl_video:wm_isMaximized()]), 

    io:format("~p~n~n", [catch gl:getConvolutionParameterfv(16#8011, 16#801A)]),

    sdl_events:eventState(?SDL_ALLEVENTS ,?SDL_IGNORE),
    sdl_events:eventState(?SDL_KEYDOWN ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_QUIT ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_VIDEORESIZE, ?SDL_ENABLE),
    ?printError(),

    initWin(),
    sdl_util:debug(00),
    Cube = {{ 0.5,  0.5, -0.5}, 
	    { 0.5, -0.5, -0.5},
	    {-0.5, -0.5, -0.5},
	    {-0.5,  0.5, -0.5},
	    {-0.5,  0.5,  0.5},
	    { 0.5,  0.5,  0.5},
	    { 0.5, -0.5,  0.5},
	    {-0.5, -0.5,  0.5}},
    Colors = {{ 1.0,  1.0,  0.0}, 
	      { 1.0,  0.0,  0.0},
	      { 0.0,  0.0,  0.0},
	      { 0.0,  1.0,  0.0},
	      { 0.0,  1.0,  1.0},
	      { 1.0,  1.0,  1.0},
	      { 1.0,  0.0,  1.0},
	      { 0.0,  0.0,  1.0}},
    
    drawBox(Cube, Colors),    
    sdl:quit(),       
    ok.

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

drawBox(Cube, Colors) ->
    %%timer:sleep(30),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:'begin'(?GL_QUADS),

    gl:color3fv(element(1, Colors)),
    gl:vertex3fv(element(1, Cube)),
    gl:color3fv(element(2, Colors)),
    gl:vertex3fv(element(2, Cube)),
    gl:color3fv(element(3, Colors)),
    gl:vertex3fv(element(3, Cube)),
    gl:color3fv(element(4, Colors)),
    gl:vertex3fv(element(4, Cube)),
    
    gl:color3fv(element(4, Colors)),
    gl:vertex3fv(element(4, Cube)),
    gl:color3fv(element(5, Colors)),
    gl:vertex3fv(element(5, Cube)),
    gl:color3fv(element(8, Colors)),
    gl:vertex3fv(element(8, Cube)),
    gl:color3fv(element(3, Colors)),
    gl:vertex3fv(element(3, Cube)),

    gl:color3fv(element(1, Colors)),
    gl:vertex3fv(element(1, Cube)),
    gl:color3fv(element(6, Colors)),
    gl:vertex3fv(element(6, Cube)),
    gl:color3fv(element(7, Colors)),
    gl:vertex3fv(element(7, Cube)),
    gl:color3fv(element(2, Colors)),
    gl:vertex3fv(element(2, Cube)),

    gl:color3fv(element(6, Colors)),
    gl:vertex3fv(element(6, Cube)),
    gl:color3fv(element(5, Colors)),
    gl:vertex3fv(element(5, Cube)),
    gl:color3fv(element(8, Colors)),
    gl:vertex3fv(element(8, Cube)),
    gl:color3fv(element(7, Colors)),
    gl:vertex3fv(element(7, Cube)),

    gl:color3fv(element(6, Colors)),
    gl:vertex3fv(element(6, Cube)),
    gl:color3fv(element(1, Colors)),
    gl:vertex3fv(element(1, Cube)),
    gl:color3fv(element(4, Colors)),
    gl:vertex3fv(element(4, Cube)),
    gl:color3fv(element(5, Colors)),
    gl:vertex3fv(element(5, Cube)),
    
    gl:color3fv(element(7, Colors)),
    gl:vertex3fv(element(7, Cube)),
    gl:color3fv(element(2, Colors)),
    gl:vertex3fv(element(2, Cube)),
    gl:color3fv(element(3, Colors)),
    gl:vertex3fv(element(3, Cube)),
    gl:color3fv(element(8, Colors)),
    gl:vertex3fv(element(8, Cube)),

    gl:'end'(),
    
    gl:matrixMode(?GL_MODELVIEW),
    gl:rotatef(5.0, 1.0, 1.0, 1.0),
    case {gl:getError(), sdl:getError()} of
	{0, ""} ->
	    ok;
	{GL, ""} ->	    
	    io:format("Errors Reported ~p => ~n", [GL]), 
	    io:format("~s~n", [glu:errorString(GL)]);
	{GL, SDL} ->
	    io:format("Errors Reported ~p ~s~n", [GL, SDL])
    end,
    sdl_video:gl_swapBuffers(),
    case check_event() of
	ok ->
	    timer:sleep(10),
	    drawBox(Cube, Colors);
	quit ->
	    ok
    end.
    
check_event() ->
    case sdl_events:pollEvent() of 
	#quit{} -> 
	    quit;
	#resize{} ->
	    io:format("Maximized: ~p~n",   [sdl_video:wm_isMaximized()]), 
	    ok;
	no_event -> 
	    ok;
	#keyboard{sym=$f} ->
	    Surface = sdl_video:getVideoSurface(),
	    io:format("~p\n", [sdl_video:wm_toggleFullScreen(Surface)]),
	    ok;
	#keyboard{sym=?SDLK_q} ->
	    quit;
	#keyboard{sym=?SDLK_ESCAPE} ->
	    quit;
	Event -> 
	    io:format("Got event ~p~n", [Event]),
	    ok
    end.
    
