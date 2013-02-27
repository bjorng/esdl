%%  Copyright (c) 2001 Dan Gudmundsson
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : erldemo.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : Show that erlang can do opengl.
%%% Created : 11 Sep 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(erldemo).
-author('dgud@erix.ericsson.se').

-include_lib("wx/include/gl.hrl").
-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_audio.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").

-export([go/0, go/1]).
-compile(export_all).

-define(W, 640).
-define(H, 480).

go() ->
    go([window]).

go(Mode) ->
    %% Init 
    Wrapper = sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_AUDIO bor 
			   ?SDL_INIT_ERLDRIVER bor ?SDL_INIT_NOPARACHUTE),

    Audio = audio_server(not lists:member(no_audio,Mode)),
    
    sdl_events:eventState(?SDL_ALLEVENTS ,?SDL_IGNORE),
    sdl_events:eventState(?SDL_KEYDOWN ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_QUIT ,?SDL_ENABLE),

    Flags =
	case lists:member(fullscreen, Mode) of
	    true ->
		?SDL_OPENGL  bor ?SDL_FULLSCREEN;
	    _ -> 
		?SDL_OPENGL
	end,
    sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),
    R = sdl_video:setVideoMode(?W, ?H, 16, Flags),

    %% Get Ready for events
    
    initWin(),
    Texture = loadTexture("Powered.bmp", false),
    Bump = loadTexture("PoweredBump.bmp", true),
    
    Verts = {{ 0.5,  0.5, -0.5},  %1
	     { 0.5, -0.5, -0.5},  %2
	     {-0.5, -0.5, -0.5},   
	     {-0.5,  0.5, -0.5},  %4
	     {-0.5,  0.5,  0.5},
	     { 0.5,  0.5,  0.5},  %6
	     { 0.5, -0.5,  0.5}, 
	     {-0.5, -0.5,  0.5}}, %8    

    %%         Faces    Normal     U-axis   V-axis 
    Faces = [{{1,2,3,4},{0,0,-1},{-1,0,0}, {0,1,0}},  % 
	     {{3,8,5,4},{-1,0,0},{0,0,1},  {0,1,0}},  %
	     {{1,6,7,2},{1,0,0}, {0,0,-1}, {0,1,0}},  %
	     {{6,5,8,7},{0,0,1}, {1,0,0},  {0,1,0}},  %
	     {{6,1,4,5},{0,1,0}, {-1,0,0}, {0,0,1}},  %
	     {{7,8,3,2},{0,-1,0},{1,0,0},  {0,0,1}}],
    Data = {Faces, Verts},       

    BumpPossible = is_bump_possible(),
    put(bumpPossible, BumpPossible),
    io:format("Press 'c' to toggle texture.~n"
	      "Press 'b' to toggle bumpmap.~n"
	      "Press 'q' or 'Esc' to quit.~n"),

    {Time, Frames} = timer:tc(?MODULE, drawBox, 
			      [Audio, Data, 0, 5.0, 
			       {0.0, 0.0, 0.0}, 
			       {0.04, 0.05, 0.06},
			       {0.2, unknown},
			       {Texture,Bump},
			       true, BumpPossible
			      ]),
    Secs = Time / 1000000,
    io:format("FPS: ~p (Frames ~p)~n", [catch Frames/Secs, Frames]),
    sdl:quit(),           
    ok.

is_bump_possible() ->
    Ext = gl:getString(?GL_EXTENSIONS),
    Exts = string:tokens(Ext, " "),
    GLVerHasDOT3 = 
	case gl:getString(?GL_VERSION) of
	    "1.3" ++ _ -> true;
	    "1.4" ++ _ -> true;
	    _ -> false
	end,
    HasExtensions = lists:member("GL_ARB_texture_env_dot3", Exts) and 
	lists:member("GL_ARB_multitexture", Exts),
    HasExtensions or GLVerHasDOT3.

initWin() ->
    gl:viewport(0,0,?W,?H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:perspective(30.0, ?W/?H, 0.1, 30.0), 
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),    
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_CULL_FACE),
    
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_DIFFUSE, {0.7,0.7,0.7,1.0}),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_AMBIENT, {0.6,0.6,0.6,1.0}),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_SPECULAR,{0.7,0.7,0.7,1.0}),
    gl:materialf(?GL_FRONT_AND_BACK, ?GL_SHININESS, 80.2),
    
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0.3,0.3,0.3,1.0}),
    gl:enable(?GL_LIGHT0),
    gl:lightfv(?GL_LIGHT0, ?GL_DIFFUSE, {0.7,0.7,0.7,1}),
    gl:lightfv(?GL_LIGHT0, ?GL_SPECULAR, {0.7,0.7,0.7,1}),

    gl:enable(?GL_TEXTURE_2D),
    gl:shadeModel(?GL_SMOOTH),
    gl:clearColor(0.5,0.5,0.9,1.0).

draw_quads({Faces, Verts},Mode) ->
    gl:'begin'(?GL_QUADS),
    draw_quads2(Faces, Verts,Mode).

draw_quads2([{{V1,V2,V3,V4},N,Ut,Vt}|R],Cube, color) ->
    gl:normal3fv(N),
    gl:texCoord2f(0.0, 1.0), 
    gl:vertex3fv(element(V1, Cube)),
    gl:texCoord2f(0.0, 0.0), 
    gl:vertex3fv(element(V2, Cube)),
    gl:texCoord2f(1.0, 0.0), 
    gl:vertex3fv(element(V3, Cube)),
    gl:texCoord2f(1.0, 1.0), 
    gl:vertex3fv(element(V4, Cube)),
    draw_quads2(R,Cube,color);
draw_quads2([{{V1,V2,V3,V4},N,Ut,Vt}|R],Cube, Light) ->
    X = dot(Ut,Light),
    Y = dot(Vt,Light),
    Z = dot(N,Light),
    gl:color3fv(norm_rgb(X,Y,Z)),
    gl:normal3fv(N),
    gl:multiTexCoord2f(?GL_TEXTURE0,0.0, 1.0), 
    gl:multiTexCoord2f(?GL_TEXTURE1,0.0, 1.0), 
    gl:vertex3fv(element(V1, Cube)),
    gl:multiTexCoord2f(?GL_TEXTURE0,0.0, 0.0), 
    gl:multiTexCoord2f(?GL_TEXTURE1,0.0, 0.0), 
    gl:vertex3fv(element(V2, Cube)),
    gl:multiTexCoord2f(?GL_TEXTURE0,1.0, 0.0), 
    gl:multiTexCoord2f(?GL_TEXTURE1,1.0, 0.0), 
    gl:vertex3fv(element(V3, Cube)),
    gl:multiTexCoord2f(?GL_TEXTURE0,1.0, 1.0), 
    gl:multiTexCoord2f(?GL_TEXTURE1,1.0, 1.0), 
    gl:vertex3fv(element(V4, Cube)),
    draw_quads2(R,Cube,Light);
draw_quads2([],_,_) -> 
    gl:'end'(),
    ok.

drawBox(Audio, Cube, N, R, Pos, Vel, Speed = {S, T0}, 
	{Txt,BumpTxt}, Color, BumpMap) -> 
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    {NewPos = {X, Y, Z}, NewVel} = calc_pos(Audio, Pos, Vel, S),
    gl:loadIdentity(),

    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {0,10,10,1}),
    gl:lightfv(?GL_LIGHT0, ?GL_SPOT_DIRECTION, {0,0,-1}),
    gl:lightf(?GL_LIGHT0, ?GL_SPOT_CUTOFF, 45),

    gl:translatef(X, Y, Z-3.0),
    RotV = {0.5,0.5,1.0},
    gl:rotatef(R, element(1,RotV),element(2,RotV),element(3,RotV)),

    case BumpMap of
	true -> 
	    Rot = rotate(-R, RotV),
	    LightInObjectSpace = mul_point(Rot, {0.0,10.0,10.0}),

	    %% Setup first texture stage to DOT the color (which contains the
	    %% light vector in tangent space) with the pre-filtered height map.

	    gl:disable(?GL_BLEND),	    
	    gl:activeTexture(?GL_TEXTURE0),
	    gl:enable(?GL_TEXTURE_2D),	    
	    gl:bindTexture(?GL_TEXTURE_2D,BumpTxt),
	    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_COMBINE),
	    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_COMBINE_RGB, ?GL_DOT3_RGB),
	    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_SOURCE0_RGB, ?GL_PRIMARY_COLOR),
	    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_SOURCE1_RGB, ?GL_TEXTURE),

	    %% Setup the second texture stage to modulate the previous dot product
	    %% and the the base texture.

	    gl:activeTexture(?GL_TEXTURE1),
	    case Color of
		true ->
		    gl:enable(?GL_TEXTURE_2D);
		false ->
		    gl:disable(?GL_TEXTURE_2D)
	    end,
	    gl:bindTexture(?GL_TEXTURE_2D,Txt),
	    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_COMBINE),
	    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_COMBINE_RGB, ?GL_MODULATE),
	    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_SOURCE0_RGB, ?GL_TEXTURE),
	    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_SOURCE1_RGB, ?GL_PREVIOUS),
	    draw_quads(Cube,LightInObjectSpace),

	    %% Disable texturing for TEXTURE1
	    gl:disable(?GL_TEXTURE_2D),
	    %% and for texture0
	    gl:activeTexture(?GL_TEXTURE0),	    
	    gl:disable(?GL_TEXTURE_2D);
	false ->
	    gl:enable(?GL_LIGHTING),
	    case Color of
		true ->
		    gl:enable(?GL_TEXTURE_2D),
		    gl:bindTexture(?GL_TEXTURE_2D,Txt),
		    gl:color3f(1,1,1),
		    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),
		    draw_quads(Cube,color);
		false ->
		    gl:disable(?GL_TEXTURE_2D),
		    draw_quads(Cube,color)
	    end
    end,
    
%     if Color == true ->
% 	    if BumpMap == true ->
% 		    gl:enable(?GL_BLEND),
% 		    gl:blendFunc(?GL_DST_COLOR,?GL_ZERO);
% 	       true -> ignore
% 	    end,
% 	    gl:enable(?GL_TEXTURE_2D),
% 	    gl:bindTexture(?GL_TEXTURE_2D,Txt),
% 	    gl:color3f(1,1,1),
% 	    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),
% 	    draw_quads(Cube,color);
%        Color == false, BumpMap == false  ->
% 	    gl:disable(?GL_TEXTURE_2D),
% 	    draw_quads(Cube,color);
%        Color == false ->
% 	    ok
%     end,
    gl:disable(?GL_BLEND),
    gl:disable(?GL_LIGHTING),
        
    check_error("DrawCube"),
    T1 = sdl_video:gl_swapBuffers(),
    case check_event(Color,BumpMap) of
	{NewCol, NewBump} ->
	    NewS = calc_speed(Speed, T1),
	    timer:sleep(10),
	    drawBox(Audio, Cube, N + 1, R + (3 * S),NewPos, NewVel, 
		    NewS,{Txt,BumpTxt},NewCol,NewBump);
	quit ->
	    N + 1
    end.

calc_pos(Audio, {X, Y, Z}, {Vx, Vy, Vz}, S) ->
    NX = X + S*Vx,
    NY = Y + S*Vy,
    NZ = Z + S*Vz,
    Nvx = if NX > 1.0; NX < -1.0  ->  play(Audio, {nx, NX}), Vx * -1; 
	     true -> Vx end,    
    Nvy = if NY > 1.0; NY < -1.0  ->  play(Audio, {ny, NY}), Vy * -1; 
	     true -> Vy end,
    Nvz = if NZ > 0.0; NZ < -10.0 ->  play(Audio, {nz, NZ}), Vz * -1; 
	     true -> Vz end,
    {{NX, NY, NZ}, {Nvx, Nvy, Nvz}}.

calc_speed({S, unknown}, T1) ->
    {S, T1};
calc_speed({S, T0}, T1) ->
    SPF  = 0.2 + ((T1 - T0) / 1000), %% Last frame took 
    Diff = (SPF - S) / 100,
    NS   = S + Diff,
    {NS, T1}. 

loadTexture(File, Bump) ->
    Ref = 
	case sdl_video:loadBMP(File) of
	    null -> 
		io:format("Couldn't load BMP file: ~p ~n", [File]),
		exit(load_bmp);
	    SR ->
		SR
	end,
    Image  = sdl_video:getSurface(Ref),
    Format = sdl_video:getPixelFormat(Image),  
    Size = Image#sdl_surface.w * Image#sdl_surface.h * 3,
    
    Mem0 = sdl_util:malloc(Size, ?GL_UNSIGNED_BYTE),
    sdl_util:copySdlImage2GLArray(Image, Mem0, 3),  %% Convert 2 GL format
    
    Mem = case Bump of
	      true -> 
		  Map = bumpmap(Mem0, Image#sdl_surface.w, Image#sdl_surface.h),
		  io:format("Bumped~n"),
		  Map;
	      false ->
		  Mem0
	  end,

    [Ture] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, Ture),
    
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP),
    gl:texImage2D(?GL_TEXTURE_2D, 0, 3, 
		  Image#sdl_surface.w, 
		  Image#sdl_surface.h, 0, 
		  ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    sdl_util:free(Mem0),
    sdl_video:freeSurface(Image),
    Ture.

check_error(Str) ->
    case {gl:getError(), sdl:getError()} of
	{0, ""} ->
	    ok;
	{GL, SDL} ->
	    io:format("In ~s Errors Reported GL ~p SDL ~s~n", [Str, GL, SDL])
    end.

check_event(ColMap, BumpMap) ->
    case sdl_events:pollEvent() of 
	#quit{} -> 
	    quit;
	no_event -> 
	    {ColMap,BumpMap};
	Quit when is_record(Quit, keyboard) -> 
	    if 
		Quit#keyboard.sym == ?SDLK_ESCAPE ->
		    quit;
		Quit#keyboard.sym == ?SDLK_q ->
		    quit;
		Quit#keyboard.sym == ?SDLK_b ->
		    {ColMap,get(bumpPossible) and not BumpMap};
		Quit#keyboard.sym == ?SDLK_c ->
		    {not ColMap,BumpMap};
		true -> 
		    io:format("Got event ~p~n", [Quit]),
		    {ColMap,BumpMap}
	    end;		    
	Event -> 
	    io:format("Got event ~p~n", [Event]),
	    {ColMap,BumpMap}
    end.

play(no_audio, Reason) -> ok;
play(Sample, Reason) ->
%%    io:format("eplay ~p ~n", [Reason]),
    sdl_audio:play_audio(Sample, 1).


audio_server(false) -> ok;
audio_server(_) ->
    {ASpec, Sample} = sdl_audio:loadWAV("Beep.wav"),
    MySpec = ASpec#audiospec{samples = 1024}, %, format=?AUDIO_U8},
    case catch sdl_audio:openAudio(MySpec, true) of
	{'EXIT', _} ->
	    io:format("Failed to open Audio, ignoring sound ~n", []),
	    sdl_audio:freeWAV(Sample),
	    no_audio;
	ObtainedFormat ->
%	    io:format("Audio Spec's:~n~p~n~p~n~p~n", 
%		      [ASpec, MySpec, ObtainedFormat]),
	    ConvS = sdl_audio:convertAudio(ASpec, ObtainedFormat, Sample),
	    sdl_audio:freeWAV(Sample),  %% Clean old sample
	    sdl_audio:pauseAudio(false),
	    ConvS
    end.

bumpmap(Mem,PW,PH) ->
    W = PW*3,
    <<Row1:W/binary,Row2:W/binary,Rest/binary>> = 
	sdl_util:readBin(Mem,PW*PH*3),
    bumpmap(Row1,Row2,Rest,W,Row1,[]).

bumpmap(R1,<<>>,<<>>,W,First,Bump) ->
    <<F:8,_/binary>> = R1,
    Row = bumpmapRow(R1,First,F,[]),
    list_to_binary(lists:reverse([Row|Bump]));

bumpmap(R1,R2,<<>>,W,First,Bump) ->
    <<F:8,_/binary>> = R1,
    Row1 = bumpmapRow(R1,R2,F,[]),
    Row2 = bumpmapRow(R2,First,F,[]),
    list_to_binary(lists:reverse([Row2,Row1|Bump]));
bumpmap(R1,R2, Rest0,W,First,Bump) ->
    <<F:8,_/binary>> = R1,
    Row = bumpmapRow(R1,R2,F,[]),
    <<R3:W/binary,Rest/binary>> = Rest0,
    bumpmap(R2,R3,Rest,W,First,[Row|Bump]).

bumpmapRow(<<>>,<<>>,_,Br) ->
    list_to_binary(lists:reverse(Br));
bumpmapRow(R1,R2,F,BR) ->
    <<C0:8,_:16,Row1/binary>> = R1,  %% Should be greyscale 
    <<Cy0:8, _:16,Row2/binary>> = R2,  %% so Green Blue channels
    ToFloat = 1.0/255.0,
    C = C0*ToFloat,
    Cx = case Row1 of
	     <<Cx0:8, _/binary>> ->
		 Cx0*ToFloat;
	     <<>> ->
		 F*ToFloat
	 end,
    Cy = Cy0*ToFloat,
    Scale = 4.0,
    DCX = Scale * (C-Cx),
    DCY = Scale * (C-Cy),
    %% Normalize
    Sqlen = DCX*DCX+DCY*DCY+1,
    Recip = 1.0/math:sqrt(Sqlen),
    Nx = DCY*Recip,
    Ny = -DCX*Recip,
    Nz = Recip,
    %% Pack in RGB
    RGB = [round(128.0+127.0*Nx),
	   round(128.0+127.0*Ny),
	   round(128.0+127.0*Nz)],
    bumpmapRow(Row1,Row2,F,[RGB|BR]).

dot({V10,V11,V12}, {V20,V21,V22}) ->
    V10*V20 + V11*V21 + V12*V22.

norm(V1, V2, V3) when is_float(V1), is_float(V2), is_float(V3) ->
    D = math:sqrt(V1*V1+V2*V2+V3*V3),
    case catch {V1/D,V2/D,V3/D} of
	{'EXIT',_} -> {0.0,0.0,0.0};
	R -> R
    end.

norm_rgb(V1, V2, V3) when is_float(V1), is_float(V2), is_float(V3) ->
    D = math:sqrt(V1*V1+V2*V2+V3*V3),
    case catch {0.5+V1/D*0.5,0.5+V2/D*0.5,0.5+V3/D*0.5} of
	{'EXIT',_} -> {0.0,0.0,0.0};
	R -> R
    end.

rotate(A0, {X,Y,Z}) when is_float(X), is_float(Y), is_float(Z) ->
    A = A0*3.14159/180,
    CosA = math:cos(A),
    SinA = math:sin(A),
    XSinA = X*SinA,
    YSinA = Y*SinA,
    ZSinA = Z*SinA,
    {C2,C3, C4,C6, C7,C8} =
	{-ZSinA,YSinA,
	 ZSinA,-XSinA,
	 -YSinA,XSinA},
    {U1,U2,U3,U4,U5,U6,U7,U8,U9} =
	{X*X,X*Y,X*Z,
	 Y*X,Y*Y,Y*Z,
	 Z*X,Z*Y,Z*Z},
    S = CosA,
    NegS = -S,
    {U1+S*(1.0-U1), U4+NegS*U4+C4, U7+NegS*U7+C7,
     U2+NegS*U2+C2, U5+S*(1.0-U5), U8+NegS*U8+C8,
     U3+NegS*U3+C3, U6+NegS*U6+C6, U9+S*(1.0-U9),
     0.0,0.0,0.0}.

mul_point({A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}, {X,Y,Z})
  when is_float(A),is_float(B),is_float(C),is_float(D),
       is_float(E),is_float(F),is_float(G),is_float(H),
       is_float(I),is_float(Tx),is_float(Ty),is_float(Tz), 
       is_float(X), is_float(Y), is_float(Z) ->
    share(X*A + Y*D + Z*G + Tx,
	  X*B + Y*E + Z*H + Ty,
	  X*C + Y*F + Z*I + Tz).

share(X, X, X) -> {X,X,X};
share(X, X, Z) -> {X,X,Z};
share(X, Y, Y) -> {X,Y,Y};
share(X, Y, X) -> {X,Y,X};
share(X, Y, Z) -> {X,Y,Z}.
