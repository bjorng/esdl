%%  Copyright (c) 2001 Dan Gudmundsson
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : testsprite.erl
%%% Author  : Dan Gudmundsson <dgud@dan.du.uab.ericsson.se>
%%% Purpose : testing, this code should correspond to the testsprite.c 
%%%           in sdl library
%%% Created : 26 Jun 2000 by Dan Gudmundsson <dgud@dan.du.uab.ericsson.se>
%%%----------------------------------------------------------------------

-module(testsprite).
-author('dgud@dan.du.uab.ericsson.se').

-compile(export_all).
-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").

-define(NUM_SPRITES, 100).
-define(MAX_SPEED,   1).

go() ->
    go([]).
go(Config) ->
    Server = sdl:init(?SDL_INIT_VIDEO),
    sdl_util:debug(1),

    Flags = 
	case lists:member(fullscreen, Config) of 
	    true ->
		?SDL_ANYFORMAT bor ?SDL_FULLSCREEN bor ?SDL_RESIZABLE;
	    _ -> 
		?SDL_ANYFORMAT bor ?SDL_RESIZABLE
	end,
    ScreenRef = sdl_video:setVideoMode(640, 480, 8, Flags),
    io:format("Video Driver Name: ~s\n", [sdl_video:videoDriverName()]),
    Screen = sdl_video:getSurface(ScreenRef),
    additional_tests(ScreenRef), 
    SpriteRef = load_sprite("icon.bmp"),
    Sprite    = sdl_video:getSurface(SpriteRef),    
    {R1, R2, R3} = erlang:now(),
    random:seed(R1,R2,R3),
    Rects = create_rects(?NUM_SPRITES, 
			 Sprite#sdl_surface.w, 
			 Sprite#sdl_surface.h, 
			 Screen#sdl_surface.w, 
			 Screen#sdl_surface.h),    
    Vels  = [velocity(0,0) || _ <- Rects],

    Background = sdl_video:mapRGB(Screen, 0, 0, 0),
    print_info(Screen, Sprite),    
    
    testblit(Screen, Sprite, Background),
    timer:sleep(500),

    {S1, S2, S3} = erlang:now(),
    NumUpdates = 400,
    sdl_util:debug(0),
    
    moveSprites(NumUpdates, Screen, Sprite, Background, Rects, Vels),
    
    {E1, E2, E3}  = erlang:now(),
    
    S = 100000 * S1 + S2 + S3 / 100000,
    E = 100000 * E1 + E2 + E3 / 100000,
    io:format("FPS: ~p\n", [NumUpdates / (E - S)]),
    sdl:quit(),
    Background.

moveSprites(0, _, _, _,_,_) ->
    ok;
moveSprites(N, Screen, Sprite, Background, Rects, Vels) ->
    true = sdl_video:fillRect(Screen, null, Background),
    {Nr, Nv, Updates} = moveSprite(Rects, [], Vels, [], Screen, Sprite, []),
    DB = (Screen#sdl_surface.flags band ?SDL_DOUBLEBUF),
    if
	DB == ?SDL_DOUBLEBUF ->
	    sdl_video:flip(Screen);
	true ->
	    sdl_video:updateRects(Screen, Updates)
    end,
    case sdl_events:pollEvent() of 
	#quit{} -> 
	    exit(normal);
	no_event -> 
	    ignore;
	#mousemotion{} ->
	    ignore;
	Event -> 
	    io:format("Got event ~p~n", [Event])
    end,
    moveSprites(N-1, Screen, Sprite, Background, lists:reverse(Nr), Nv).

moveSprite([], RA, [], VA, _, _, Updates) -> {RA, VA, Updates};
moveSprite([R |RTail], RAcc, [{XS,YS}|VTail], VAcc, Screen, Sprite, UAcc) ->
    TX = R#sdl_rect.x + XS,
    TY = R#sdl_rect.y + YS,
    {Nx, Nxs} =
	if 
	    TX < 0; TX >= Screen#sdl_surface.w ->
		Xvel = -XS,
		{TX + Xvel, Xvel};
	    true ->
		{TX, XS}
	end,
    {Ny, Nys} =
	if 
	    TY < 0; TY >= Screen#sdl_surface.h ->
		Yvel = -YS,
		{TY + Yvel, Yvel};
	    true ->
		{TY, YS}
	end,
    NR = R#sdl_rect{x = Nx, y = Ny},
    {_, CR} = sdl_video:blitSurface(Sprite, null, Screen, NR), 
    moveSprite(RTail, [NR|RAcc], VTail, [{Nxs, Nys}|VAcc],
	       Screen, Sprite, [CR | UAcc]).

testblit(Screen, Sprite, BG) ->
    Rect = #sdl_rect{x = 0, y = 0, 
		     w = Sprite#sdl_surface.w, 
		     h = Sprite#sdl_surface.h},

    true = sdl_video:fillRect(Screen, null, BG),
    
    CR = 
	case sdl_video:blitSurface(Sprite, null, Screen, Rect) of
	    {null, ClippedRect} -> io:format("Blit successfull~n"),
				   ClippedRect;
	    _  -> io:format("Blit failed~n"),
		  ?printError(),
		  exit({error, bad_blit})
	end,
    sdl_video:updateRects(Screen, [CR]),
    %% It migth have changed
    Sprite1 = sdl_video:getSurface(Sprite),
    SpriteFlags = Sprite1#sdl_surface.flags,    
    case ( SpriteFlags band ?SDL_HWACCEL) > 0 of 
	true ->
	    io:format("Sprite blit uses hardware acc~n");
	false ->
	    io:format("Sprite blit don't use HW acceleration~n")
    end,
    case ( SpriteFlags band ?SDL_RLEACCEL) > 0 of 
	true ->
	    io:format("Sprite blit uses RLE acc~n");
	false ->
	    io:format("Sprite blit don't use RLE acceleration~n")
    end,
    Sprite1.

load_sprite(File) ->
    SpriteRef = 
	case sdl_video:loadBMP(File) of
	    null -> 
		io:format("Couldn't load BMP file: ~p ~n", [File]),
		exit(load_bmp);
	    SR ->
		SR
	end,

    Sprite  = sdl_video:getSurface(SpriteRef),
    Pf      = sdl_video:getPixelFormat(SpriteRef),
    case Pf#sdl_pixelformat.palette of
	null ->
	    io:format("No Palette found ~n"),
	    ignore;
	Palette ->
	    <<Pixel>> = 
		sdl_video:getPixels(Sprite, 
				    #sdl_rect{x = 0, y = 0, 
					      w = 1, h = 1}),
	    io:format("Palette found set key ~p to transparent ~n", 
		      [Pixel]),
	    true = sdl_video:setColorKey(Sprite, 
					 ?SDL_SRCCOLORKEY bor ?SDL_RLEACCEL,
					 Pixel)
    end,
    case sdl_video:displayFormat(Sprite) of
	null ->
	    sdl_video:freeSurface(Sprite),
	    io:format("Failed to convert background~n"),
	    exit({error, convert_failed});
	Converted ->
	    sdl_video:freeSurface(Sprite),
	    Converted
    end.

create_rects(0, SpriteW, SpriteH, WinW, WinH) ->
    [];

create_rects(N, SpriteW, SpriteH, WinW, WinH) ->
    R = #sdl_rect{x = random:uniform(WinW),
		  y = random:uniform(WinH),
		  w = SpriteW, h = SpriteH},
    [R | create_rects(N-1, SpriteW, SpriteH, WinW, WinH)].

print_info(Screen, Sprite) ->
    ScreenFlags = Screen#sdl_surface.flags,    
    SpriteFlags = Sprite#sdl_surface.flags,    
    case ( ScreenFlags band ?SDL_HWSURFACE) > 0 of 
	true ->
	    io:format("Screen is in video memory~n");
	false ->
	    io:format("Screen is in system memory~n")
    end,
    case ( ScreenFlags band ?SDL_DOUBLEBUF) > 0 of 
	true ->
	    io:format("Screen is doubled buffered~n");
	_  ->
	    ignore
    end,
    case (SpriteFlags band ?SDL_HWSURFACE) > 0 of 
	true ->
	    io:format("Sprite is in video memory~n");
	false ->
	    io:format("Sprite is in system memory~n")
    end.

velocity(0, 0) ->
    XV = random:uniform(?MAX_SPEED *2 + 1) - (?MAX_SPEED + 1),
    YV = random:uniform(?MAX_SPEED *2) -  (?MAX_SPEED + 1),
    velocity(XV, YV);
velocity(X, Y) ->
    {X, Y}.

additional_tests(Screen) ->
    PFormat = sdl_video:getPixelFormat(Screen),    
    Palette = sdl_video:getPalette(Screen),
    io:format("Got a palette with ~p colors~n", [length(Palette)]).

