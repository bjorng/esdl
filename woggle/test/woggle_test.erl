-module(woggle_test).

-compile(export_all).
-include("sdl_video.hrl").
-include("sdl_events.hrl").
-include("gl.hrl").

go() ->
    code:add_path("../../ebin"),
    woggle:init(),
    io:format("Driver ~p ~n", [sdl_video:videoDriverName()]),
    io:format("~p~n",[woggle:listModes()]),
    io:format("~p~n",[sdl_video:listModes(null,?SDL_FULLSCREEN)]),

    %% Misconseption from testgl.erl, but compatibility is useful anyway...
     AvailableWindowedSzs = sdl_video:listModes(null, ?SDL_FULLSCREEN),
     io:format("Available WindowSizes ~p ~n", [AvailableWindowedSzs]),
     case AvailableWindowedSzs of
 	[{_, 0,0,W,H}|_] ->
 	    Res = [Test || Test <- [32,24,16,15],
 			   true == sdl_video:videoModeOK(W,H,Test,0)],
 	    io:format("A guess at max video res is ~px~p:~p ~n", [W,H, hd(Res)]);
 	_ ->
 	    io:format("Can't guess max resolution~n", [])
     end,

    sdl_video:setVideoMode(640,480,16,?SDL_OPENGL bor ?SDL_RESIZABLE),
    sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1), %Ignored
    Rs= sdl_video:gl_getAttribute(?SDL_GL_RED_SIZE),
    Gs= sdl_video:gl_getAttribute(?SDL_GL_GREEN_SIZE),
    Bs= sdl_video:gl_getAttribute(?SDL_GL_BLUE_SIZE),
    Ds= sdl_video:gl_getAttribute(?SDL_GL_DEPTH_SIZE),
    Dz= sdl_video:gl_getAttribute(?SDL_GL_BUFFER_SIZE),
    Db= (1 == sdl_video:gl_getAttribute(?SDL_GL_DOUBLEBUFFER)),
    io:format("OpenGL attributes ~n"),
    io:format("Sizes in bits Red ~p Green ~p Blue ~p Depth ~p Buffer ~p Doublebuffered ~p~n",
	      [Rs, Gs, Bs, Ds, Dz, Db]),   
    io:format("Vendor:     ~s~n",  [gl:getString(?GL_VENDOR)]),
    io:format("Renderer:   ~s~n",  [gl:getString(?GL_RENDERER)]),
    io:format("Version:    ~s~n",  [gl:getString(?GL_VERSION)]),
    io:format("GL AUX BUFFERS ~p~n",  [gl:getIntegerv(?GL_AUX_BUFFERS)]),
    io:format("SDL Version ~p~n",  [sdl_video:wm_getInfo()]),

    io:format("Extensions: ~s~n",  [gl:getString(?GL_EXTENSIONS)]),    
    io:format("Maximized: ~p~n",   [sdl_video:wm_isMaximized()]), 

    io:format("~p", [catch gl:getConvolutionParameterfv(16#8011, 16#801A)]),
    sdl_events:eventState(?SDL_ALLEVENTS ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_KEYDOWN ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_QUIT ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_VIDEORESIZE, ?SDL_ENABLE),

    dogl_start(),
    dogl(10000).


dogl_start() ->
    gl:viewport(0,0,640,480),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:ortho( -2.0, 2.0, -2.0, 2.0, -20.0, 20.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),    
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    gl:clearColor(0.0,0.0,0.0,1.0),
    io:format("Vendor:     ~s~n",  [gl:getString(?GL_VENDOR)]),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT).
    
dogl(0) ->
    ok;

dogl(N) ->
    gl:clearColor(0.0,0.0,0.0,1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
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
    gl:rotatef(5.0, 1.0, 1.0, 1.0),
    woggle:swapBuffers(),
    %gl:swapBuffers(),
    poll_events(),
    dogl(N-1).
    
poll_events() ->
     case sdl_events:pollEvent() of 
	 no_event ->
	     ok;
	 Event ->
	     io:format("Got event ~p~n", [Event]),
	     poll_events()
     end.




