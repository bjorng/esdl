%%  Copyright (c) 2001 Dan Gudmundsson
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%----------------------------------------------------------------------
%%% File    : testjoy.erl
%%% Author  :  <dgud@erix.ericsson.se>
%%% Purpose : Test joystick handling 
%%% Created : 20 Apr 2001 by  <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(testjoy).
-author('dgud@erix.ericsson.se').

-compile(export_all).
%%-export([Function/Arity, ...]).
-include("sdl.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("sdl_events.hrl").
-include("gl.hrl").
-include("sdl_joystick.hrl").

-define(CUBE,  {{ 10.0,  10.0, -10.0},   %1
		{ 10.0, -10.0, -10.0},   %2
		{-10.0, -10.0, -10.0},   
		{-10.0,  10.0, -10.0},   %4
		{-10.0,  10.0,  10.0},
		{ 10.0,  10.0,  10.0},   %6
		{ 10.0, -10.0,  10.0}, 
		{-10.0, -10.0,  10.0}}). %8           	

go() ->    go([]).
go(Flags) ->
    init(Flags), 
    Nr = sdl_joystick:numJoysticks(),
    io:format("Found ~p joysticks ~n", [Nr]),
    if 
	Nr > 0 -> ok;
	true -> exit(no_josticks_found)
    end,    
    PrintName = fun(Index) -> 
			io:format("Joystick ~p: ~s~n", 
				  [Index, sdl_joystick:name(Index)])
		end,
    for(PrintName, Nr - 1),
    JoyI = case lists:keysearch(joystick, 1, Flags) of 
	      {value, {joystick, N}} when N < Nr,
					  N >= 0 ->
		  N;
	      _ ->
		  0
	  end,
    Joy = sdl_joystick:open(JoyI),
%%    sdl_util:debug(19),
    io:format("Testing joystick ~p: ~n", [JoyI]),
    io:format("Number of axis ~p: ~n", [sdl_joystick:numAxes(Joy)]),
    io:format("Number of balls ~p: ~n", [sdl_joystick:numBalls(Joy)]),
    io:format("Number of hats ~p: ~n", [sdl_joystick:numHats(Joy)]),
    io:format("Number of buttons ~p: ~n", [sdl_joystick:numButtons(Joy)]),
    init_video(Flags),
    Res = (catch test(Joy, {0.0, 0.0, 0.0}, [])),
    io:format("~p ~n", [Res]),		
    sdl_joystick:close(Joy),
    sdl:quit().

test(Joy, Pos, Buttons) ->
    {_, Evs} = sdl_events:peepEvents(sune, 10, ?SDL_GETEVENT, 16#FFFF),
    {Npos = {NX, NY, NZ}, Nbuttons} = check_joy(Evs, Pos, Buttons), 
%%    io:format("Pressed buttons ~p ~n", [Nbuttons]),
    gl:loadIdentity(),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    drawButtons(Nbuttons),
    gl:loadIdentity(),
    gl:translatef(-5.0, -5.0, -50.0),
    draw_cube(),
    gl:color3f(0.0, 0.0, 1.0),
    gl:translatef(NX, NY, NZ),
    gl:'begin'(?GL_LINES),  % Draw Cross 
    gl:vertex3f(-1.0, 0.0, 0.0),
    gl:vertex3f(1.0, 0.0, 0.0),
    gl:vertex3f(0.0, -1.0, 0.0),
    gl:vertex3f(0.0, 1.0, 0.0),
    gl:'end'(),
    gl:swapBuffers(),
    case {gl:getError(), sdl:getError()} of
	{0, ""} ->
	    ok;
	{GL, SDL} ->
	    io:format("Errors Reported ~p ~s~n", [GL, SDL])
    end,
    timer:sleep(5),
    test(Joy, Npos, Nbuttons).

drawButtons([]) ->
    ok;
drawButtons([H|R]) -> 
    Size = 1.0,
    X1 = Size * H,
    X2 = X1 + Size,
    Y1 = 5.0, Y2 = Y1*2,
    gl:'begin'(?GL_QUADS),	     
    gl:vertex3f(X1 -10, Y1, -20.0),
    gl:vertex3f(X1 -10, Y2, -20.0),
    gl:vertex3f(X2 -10, Y2, -20.0),	
    gl:vertex3f(X2 -10, Y1, -20.0),	
    gl:'end'(), 
    drawButtons(R).

check_joy([], Pos, B) ->
   {Pos, B};
check_joy([Ev|R], Pos = {X, Y, Z}, B) ->
    case Ev of
	JA when record(JA, joyaxis) ->
%%	    io:format("Got ~p ~n", [JA]),
	    NewA = 10 / 32767 * JA#joyaxis.value,
  	    case JA#joyaxis.axis of
		0 -> check_joy(R, {NewA, Y, Z},B);
		1 -> check_joy(R, {X, -NewA, Z},B);
		3 -> check_joy(R, {X, Y, -NewA},B);
	        _ -> check_joy(R, Pos, B) %% Ignore
	    end;
	JB when record(JB, joybutton) ->
            Butt = JB#joybutton.button,
%%	    io:format("Button ~w changed ~n", [Butt]),
	    case JB#joybutton.state of
	        ?SDL_PRESSED ->
	             check_joy(R, Pos, [Butt|B]);
	        ?SDL_RELEASED ->
                     check_joy(R, Pos, lists:delete(Butt, B))
	    end; 
	#quit{} -> 
	    throw(quit);
	no_event -> 
	    check_joy(R, Pos, B);
	Quit when record(Quit, keyboard) -> 
	    if
		Quit#keyboard.sym == ?SDLK_ESCAPE ->
		    throw(quit);
		Quit#keyboard.sym == ?SDLK_q ->
		    throw(quit);
		true -> 
		    io:format("Got event ~p~n", [Quit]),
		    check_joy(R, Pos, B)
	    end;	    	    

	Event ->
	    io:format("Got ~p ~n", [Event]),
	    check_joy(R, Pos, B)
    end.

draw_cube() ->
    gl:'begin'(?GL_LINES),
    gl:color3f(1.0, 0.0, 0.0),
    gl:vertex3fv(element(1, ?CUBE)),
    gl:vertex3fv(element(2, ?CUBE)),
    gl:vertex3fv(element(2, ?CUBE)),
    gl:vertex3fv(element(3, ?CUBE)),
    gl:vertex3fv(element(3, ?CUBE)),
    gl:vertex3fv(element(4, ?CUBE)),
    
    gl:vertex3fv(element(4, ?CUBE)),
    gl:vertex3fv(element(5, ?CUBE)),
    gl:vertex3fv(element(5, ?CUBE)),
    gl:vertex3fv(element(8, ?CUBE)),
    gl:vertex3fv(element(8, ?CUBE)),
    gl:vertex3fv(element(3, ?CUBE)),
    
    gl:vertex3fv(element(1, ?CUBE)),
    gl:vertex3fv(element(6, ?CUBE)),
    gl:vertex3fv(element(6, ?CUBE)),
    gl:vertex3fv(element(7, ?CUBE)),
    gl:vertex3fv(element(7, ?CUBE)),
    gl:vertex3fv(element(2, ?CUBE)),
    
    gl:vertex3fv(element(6, ?CUBE)),
    gl:vertex3fv(element(5, ?CUBE)),
    gl:vertex3fv(element(5, ?CUBE)),
    gl:vertex3fv(element(8, ?CUBE)),
    gl:vertex3fv(element(8, ?CUBE)),
    gl:vertex3fv(element(7, ?CUBE)),
    
    gl:vertex3fv(element(6, ?CUBE)),
    gl:vertex3fv(element(1, ?CUBE)),
    gl:vertex3fv(element(1, ?CUBE)),
    gl:vertex3fv(element(4, ?CUBE)),
    gl:vertex3fv(element(4, ?CUBE)),
    gl:vertex3fv(element(5, ?CUBE)),
    
    gl:vertex3fv(element(7, ?CUBE)),
    gl:vertex3fv(element(2, ?CUBE)),
    gl:vertex3fv(element(3, ?CUBE)),
    gl:vertex3fv(element(8, ?CUBE)),

    gl:'end'().


for(Fun, N) when N < 0 ->
    ok;
for(Fun, N) ->
    Fun(N),
    for(Fun, N-1).

init(Config) ->
    Wrapper = 
	sdl:init(?SDL_INIT_JOYSTICK bor 
		 ?SDL_INIT_VIDEO bor 
		 ?SDL_INIT_ERLDRIVER),
    ok.

init_video(Config) ->
    Flags = 
	case lists:member(fullscreen, Config) of 
	    true ->
		?SDL_OPENGL  bor ?SDL_FULLSCREEN;
	    _ -> 
		?SDL_OPENGL
	end,
    sdl_video:setVideoMode(640, 480, 16, Flags),
    gl:viewport(0,0,640,480),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:perspective(40.0, 640/480, 10.0, 80.0), 
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),    
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    gl:clearColor(0.9,0.9,0.9,0.0).

