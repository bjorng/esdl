%%  Copyright (c) 2007 Klas Johansson
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : test_glimg.erl
%%% Author  : Klas Johansson <klajo at users.sourceforge.net>
%%% Purpose : Demonstrate the use of SDL_image as OpenGL textures.
%%%           This is based on testgl.erl (actually test_glfont.erl).
%%% Created : 19 Feb 2007 by Klas Johansson <klajo at users.sourceforge.net>
%%%----------------------------------------------------------------------

-module(test_glimg).
-author('klajo at users.sourceforge.net').
-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").

-export([go/0, go/1]).

-define(def_img_file, "erlang-drop-shadow.png").

go() ->
    go([]).
go(Config) ->
    %% Init 
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER bor
	     ?SDL_INIT_NOPARACHUTE),
    sdl_util:debug(1),
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

    SR = sdl_video:setVideoMode(800, 600, 0, Flags),
    S = sdl_video:getSurface(SR),
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
    io:format("GL AUX BUFFERS ~p~n",  [gl:getIntegerv(?GL_AUX_BUFFERS)]),
    io:format("SDL Version ~p~n",  [sdl_video:wm_getInfo()]),

    io:format("Extensions: ~s~n",  [gl:getString(?GL_EXTENSIONS)]),    
    io:format("Maximized: ~p~n",   [sdl_video:wm_isMaximized()]), 

    io:format("~p", [catch gl:getConvolutionParameterfv(16#8011, 16#801A)]),

    sdl_events:eventState(?SDL_ALLEVENTS ,?SDL_IGNORE),
    sdl_events:eventState(?SDL_KEYDOWN ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_QUIT ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_VIDEORESIZE, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_MOUSEMOTION, ?SDL_ENABLE),
    ?printError(),

    %% Open image and draw it onto an SDL surface
    ImgRef = sdl_img:load(?def_img_file),
    Img = sdl_video:getSurface(ImgRef),

    %% Move this onto an OpenGL texture
    {Texture, MinX, MinY, MaxX, MaxY} = load_texture(Img),
    sdl_video:freeSurface(ImgRef),

    init_win(S),
    sdl_util:debug(0),
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
    
    draw(Cube, Colors, Texture, 
	    MinX, MinY, MaxX, MaxY, 
	    ((S#sdl_surface.w - Img#sdl_surface.w) div 2), 
	    ((S#sdl_surface.h - Img#sdl_surface.h) div 2), 
	    Img#sdl_surface.w, 
	    Img#sdl_surface.h),  

    sdl:quit(),       
    ok.

init_win(#sdl_surface{w = W, h = H}) ->
    Ratio = W/H,
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:ortho( -2.0*Ratio, 2.0*Ratio, -2.0, 2.0, -20.0, 20.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),    
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    gl:shadeModel(?GL_SMOOTH),
    gl:clearColor(1.0,1.0,1.0,1.0).

draw(Cube, Colors, Texture, MinX, MinY, MaxX, MaxY, X, Y, W, H) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:glBegin(?GL_QUADS),

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

    gl:glEnd(),
    
    gl:matrixMode(?GL_MODELVIEW),
    gl:rotatef(0.1, 1.0, 1.0, 1.0),
    case {gl:getError(), sdl:getError()} of
	{0, ""} ->
	    ok;
	{GL, ""} ->	    
	    io:format("Errors Reported ~p => ~n", [GL]), 
	    io:format("~s~n", [glu:errorString(GL)]);
	{GL, SDL} ->
	    io:format("Errors Reported ~p ~s~n", [GL, SDL])
    end,

    enter_2d_mode(),
    gl:bindTexture(?GL_TEXTURE_2D, Texture),
    gl:'begin'(?GL_TRIANGLE_STRIP),
    gl:texCoord2f(MinX, MinY), gl:vertex2i(X,   Y  ),
    gl:texCoord2f(MaxX, MinY), gl:vertex2i(X+W, Y  ),
    gl:texCoord2f(MinX, MaxY), gl:vertex2i(X,   Y+H),
    gl:texCoord2f(MaxX, MaxY), gl:vertex2i(X+W, Y+H),
    gl:'end'(),
    leave_2d_mode(),

    gl:swapBuffers(),
    case check_event() of
	ok ->
	    draw(Cube, Colors, Texture, MinX, MinY, MaxX, MaxY, X, Y, W, H);
	{mouse_moved, MX, MY} ->
	    X2 = MX - W div 2,
	    Y2 = MY - H div 2,
	    draw(Cube, Colors, Texture, MinX, MinY, MaxX, MaxY, X2, Y2, W, H);
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
	#mousemotion{x=MX, y=MY} ->
	    {mouse_moved, MX, MY};
	Event -> 
	    io:format("Got event ~p~n", [Event]),
	    ok
    end.
    

load_texture(Surface) ->
    #sdl_surface{w = SW,
		 h = SH,
		 flags = SFlags} = Surface,
    #sdl_pixelformat{alpha = SAlpha} = sdl_video:getPixelFormat(Surface),
    
    %% Use the surface width and height expanded to powers of 2
    W = get_power_of_two_roof(Surface#sdl_surface.w),
    H = get_power_of_two_roof(Surface#sdl_surface.h),
    MinX = 0,
    MinY = 0,
    MaxX = SW / W,
    MaxY = SH / H,

    Image = sdl_video:createRGBsurface(?SDL_SWSURFACE,
				       W,
				       H,
				       _Depth=32,
  				       16#FF000000, % createRGBsurface expects
  				       16#00FF0000, % big endian mask values 
  				       16#0000FF00, 
  				       16#000000FF),

    %% Save the alpha blending attributes
    SavedFlags = SFlags band (?SDL_SRCALPHA bor ?SDL_RLEACCELOK),
    if (SavedFlags band ?SDL_SRCALPHA) == ?SDL_SRCALPHA ->
	    sdl_video:setAlpha(Surface, 0, 0);
       true ->
	    ok
    end,

    %% Copy the surface into the GL texture image
    Area = #sdl_rect{x = 0, y = 0, w = SW, h = SH},
    sdl_video:blitSurface(Surface, Area, Image, Area),
    Pixels = sdl_video:getPixels(Image, #sdl_rect{x = 0, y = 0, w = W, h = H}),

    %% Restore the alpha blending attributes 
    if (SavedFlags band ?SDL_SRCALPHA) == ?SDL_SRCALPHA ->
	    sdl_video:setAlpha(Surface, SavedFlags, SAlpha);
       true ->
	    ok
    end,

    %% Create an OpenGL texture for the image
    [Texture] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, Texture),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),
    gl:texImage2D(?GL_TEXTURE_2D,
		  0,
		  ?GL_RGBA,
		  W,
		  H,
		  0,
		  ?GL_RGBA,
		  ?GL_UNSIGNED_BYTE,
		  Pixels),
    sdl_video:freeSurface(Image),
    {Texture, MinX, MinY, MaxX, MaxY}.
   

get_power_of_two_roof(X) ->
    get_power_of_two_roof_2(1, X).

get_power_of_two_roof_2(N, X) when N >= X -> N;
get_power_of_two_roof_2(N, X)             -> get_power_of_two_roof_2(N*2, X).
    
    

enter_2d_mode() ->
    ScreenPtr = sdl_video:getVideoSurface(),
    Screen = sdl_video:getSurface(ScreenPtr),
    W = Screen#sdl_surface.w,
    H = Screen#sdl_surface.h,

    %% Note, there may be other things you need to change,
    %% depending on how you have your OpenGL state set up.
    gl:pushAttrib(?GL_ENABLE_BIT),
    gl:disable(?GL_DEPTH_TEST),
    gl:disable(?GL_CULL_FACE),
    gl:enable(?GL_TEXTURE_2D),

    %% This allows alpha blending of 2D textures with the scene
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    
    gl:viewport(0, 0, W, H),
    
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),

    %% SDL coordinates will be upside-down in the OpenGL world.  We'll
    %% therefore flip the bottom and top coordinates in the orthogonal
    %% projection to correct this.  
    %% Note: We could flip the texture/image itself, but this will
    %% also work for mouse coordinates.
    gl:ortho(0.0, W, H, 0.0, 0.0, 1.0),

    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),

    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE).

leave_2d_mode() ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:popAttrib().
