%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_events.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : Provides the sdl_events API
%%% Created : 7 Jul 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(sdl_events).
-include("esdl.hrl").
-include("sdl_events.hrl").

-export([eventState/2, 
	 peepEvents/0,
	 peepEvents/2,
	 peepEvents/3,
	 pollEvent/0, 
	 pumpEvents/0, 
	 waitEvent/0]).

-export([decode_events/1]).

-import(sdl, [call/2,cast/2]).

%%% Available Functions 
-define(SDL_PumpEvents, ?SDL_EVENTS_HRL + 1).
-define(SDL_PeepEvents, ?SDL_PumpEvents +1).
-define(SDL_PollEvent,  ?SDL_PeepEvents +1).
-define(SDL_WaitEvent,  ?SDL_PollEvent +1).
-define(SDL_EventState, ?SDL_WaitEvent +1).

%%%%%%%%%%%%%%% GENERAL EVENT FUNCTIONS %%%%%%%%%%%%%%%

%% Func:  pumpEvents
%% Args:  none
%% Returns:  ok
%% C-API func: void SDL_PumpEvents(void);
pumpEvents() ->
    cast(?SDL_PumpEvents, []).

%% Func:  peepEvents/0
%% Args:  None
%% Returns:  {NumOfEvents, [Events]}
%% C-API func: int SDL_PeepEvents(SDL_Event *events, int numevents,
%%				SDL_eventaction action, Uint32 mask)
%% Desc:  Get up to to 16 events of all types.
peepEvents() ->
    peepEvents(16, ?SDL_GETEVENT, ?SDL_ALLEVENTS).


%% Func:  peepEvents/2
%% Args:  NumEvents (might be 0) 
%%       Mask (32 bits event mask)
%% Returns:  {NumOfEvents, [Events]}
%% C-API func: int SDL_PeepEvents(SDL_Event *events, int numevents,
%%				SDL_eventaction action, Uint32 mask)
%% Desc:  Exits if error (NumEvents < 256) 
peepEvents(NumEvents, Mask) when NumEvents < 256 ->
    peepEvents(NumEvents, ?SDL_GETEVENT, Mask).

%% Func:  peepEvents/2
%% Args:  NumEvents (might be 0) 
%%	  ?SDL_GETEVENT
%%       Mask (32 bits event mask)
%% Returns:  {NumOfEvents, [Events]}
%% C-API func: int SDL_PeepEvents(SDL_Event *events, int numevents,
%%				SDL_eventaction action, Uint32 mask)
%% Desc:  Exits if error (NumEvents < 256) 
peepEvents(NumEvents, ?SDL_GETEVENT, Mask) when NumEvents < 256 ->
    call(?SDL_PeepEvents, <<Mask:32,NumEvents:8>>),
    receive 
	{'_esdl_result_', <<>>} -> 
	    [];
	{'_esdl_result_', Events} -> 
	    decode_events(Events, [])
    end.

%% Func:  pollEvent
%% Args:  none
%% Returns:  no_event | Event (one of the event records)
%% C-API func: int SDL_PollEvent(SDL_Event *event);
%% Desc:
pollEvent() ->
    call(?SDL_PollEvent, []),
    receive 
	{'_esdl_result_', <<>>} ->
	    no_event;
	{'_esdl_result_', Events} -> 
	    hd(decode_events(Events, []))
    end.

%% Func:  waitEvent
%% Args:  none
%% Returns:  Event (one of the event records)
%% C-API func: int SDL_WaitEvent(SDL_Event *event);
%% Desc:
waitEvent() ->
    call(?SDL_WaitEvent, []),
    receive 
	{'_esdl_result_', <<>>} -> 
	    [];
	{'_esdl_result_', Events} -> 
	    hd(decode_events(Events, []))
    end.

%% Func:  eventState
%% Args:  EventType (see sdl_events.hrl), State (SDL_QUERY | SDL_IGNORE |SDL_ENABLE)
%% Returns:  State (?SDL_ENABLE | ?SDL_IGNORE)
%% C-API func: Uint8 SDL_EventState(Uint8 type, int state);
%% Desc:  
eventState(Type, State) ->
    <<Res:8>> = call(?SDL_EventState, <<Type:8,State:8>>),
    Res.

%%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_events(Bin) ->
    decode_events(Bin, []).

decode_events(<<>>, Acc) ->
    lists:reverse(Acc);
decode_events(<<?SDL_NOEVENT:8, Rest/binary>>, Acc) ->
    decode_events(Rest, [no_event | Acc]);
decode_events(<<?SDL_ACTIVEEVENT:8, Gain:8, State:8, Rest/binary>>, Acc) ->
    Event = #active{gain = Gain, state = State},
    decode_events(Rest, [Event | Acc]);
decode_events(<<?SDL_KEYDOWN:8, Which:8, State:8, 
	      Scancode:8, Sym:16, Mod:16, Unicode:16, Rest/binary>>, Acc) ->
    Event = #keyboard{which=Which,state=State, 
		      scancode=Scancode,sym=Sym,mod=Mod,unicode=Unicode},
    decode_events(Rest, [Event | Acc]);
decode_events(<<?SDL_MOUSEMOTION:8, Which:8, State:8, Mod:16,
	      X:16, Y:16, Xrel:16/signed, Yrel:16/signed,Rest/binary>>, Acc) ->
    Event = #mousemotion{which = Which, state = State, 
			 mod = Mod, x = X, y = Y, xrel = Xrel, yrel = Yrel},
    decode_events(Rest, [Event | Acc]);
decode_events(<<?SDL_MOUSEBUTTONDOWN:8, 
	       Which:8, Button:8, State:8, Mod:16,
	       X:16, Y:16,Rest/binary>>, Acc) ->
    Event = #mousebutton{which = Which, button = Button, state = State,
			 mod = Mod, x = X, y = Y},
    decode_events(Rest, [Event | Acc]);
decode_events(<<?SDL_JOYAXISMOTION, Which:8, Axis:8, Value:16/signed,Rest/binary>>, Acc) ->
    Event = #joyaxis{which = Which, axis = Axis, value = Value},
    decode_events(Rest, [Event | Acc]);
decode_events(<<?SDL_JOYBALLMOTION, Which:8, Ball:8, 
	      Xrel:16/signed, Yrel:16/signed,Rest/binary>>, Acc) ->
    Event = #joyball{which = Which, ball = Ball, xrel =Xrel, yrel=Yrel},
    decode_events(Rest, [Event | Acc]);
decode_events(<<?SDL_JOYHATMOTION,Which:8,Hat:8,Value:16,Rest/binary>>,Acc) ->
    Event = #joyhat{which = Which, hat = Hat, value = Value},
    decode_events(Rest, [Event | Acc]);
decode_events(<<?SDL_JOYBUTTONDOWN, Which:8, Button:8, State:8,Rest/binary>>, Acc) ->
    Event = #joybutton{which = Which, button = Button, state = State},
    decode_events(Rest, [Event | Acc]);
decode_events(<<?SDL_JOYBUTTONUP, Which:8, Button:8, State:8,Rest/binary>>, Acc) ->
    Event = #joybutton{which = Which, button = Button, state = State},
    decode_events(Rest, [Event | Acc]);
decode_events(<<?SDL_QUIT,Rest/binary>>, Acc) ->
    Event = #quit{},
    decode_events(Rest, [Event | Acc]);
decode_events(<<?SDL_VIDEORESIZE, W:16, H:16,Rest/binary>>, Acc) ->
    Event = #resize{w = W, h = H},
    decode_events(Rest, [Event | Acc]);
decode_events(<<?SDL_VIDEOEXPOSE, Rest/binary>>, Acc) ->
    Event = #expose{},
    decode_events(Rest, [Event | Acc]).
