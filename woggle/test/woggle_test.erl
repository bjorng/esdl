-module(woggle_test).

-compile(export_all).
-include("sdl_video.hrl").
-include("gl.hrl").

go() ->
    code:add_path("../../ebin"),
    woggle:init(),
    io:format("~p~n",[woggle:listModes()]),
    io:format("~p~n",[sdl_video:listModes(null,?SDL_FULLSCREEN)]),
    sdl_video:setVideoMode(640,480,16,?SDL_OPENGL bor ?SDL_RESIZABLE),
    dogl_start(),
    dogl(1000).


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
    dogl(N-1).
    





