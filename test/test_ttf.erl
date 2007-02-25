%%  Copyright (c) 2007 Klas Johansson
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : test_ttf.erl
%%% Author  : Klas Johansson <klajo at users.sourceforge.net>
%%% Purpose : Testing; this code test various functions of the sdl_ttf API.
%%% Created : 29 Jan 2007 by Klas Johansson <klajo at users.sourceforge.net>
%%%----------------------------------------------------------------------

-module(test_ttf).
-author('klajo at users.sourceforge.net').

-compile(export_all).
-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("sdl_ttf.hrl").

go(FontFile, FontSize) ->
    go(FontFile, FontSize, []).
go(FontFile, FontSize, Config) ->
    FontStyle  = ?SDL_TTF_STYLE_NORMAL,
    FgColor    = #sdl_color{r=0,g=153,b=255},
    BgColor    = #sdl_color{r=0,g=0,b=0},

    TextStr    = "The quick brown fox jumped over the lazy dog",
    Utf8Str    = "The quick brown fox jumped over the lazy dog",
    UnicodeStr = 
	"If you have a compatible font you'll see a Unicode fi-ligature here: "
	++ [16#fb01],

    %% initialization
    sdl:init(?SDL_INIT_VIDEO),
    sdl_ttf:init(),
    sdl_util:debug(1),
    listen_to_keypress_events(),

    1 = sdl_ttf:wasInit(),

    Flags = 
	case lists:member(fullscreen, Config) of 
	    true ->
		?SDL_ANYFORMAT bor ?SDL_FULLSCREEN bor ?SDL_RESIZABLE;
	    _ -> 
		?SDL_ANYFORMAT bor ?SDL_RESIZABLE
	end,
    ScreenRef = sdl_video:setVideoMode(640, 480, 8, Flags),
    Screen = sdl_video:getSurface(ScreenRef),

    %% non-existing font
    {'EXIT', _} = (catch sdl_ttf:openFont("/tmp/no-such-font.ttf", 18)),
    [_|_] = sdl_ttf:getError(),

    %% the user-specified font
    F = case (catch sdl_ttf:openFont(FontFile, FontSize)) of
	    {'EXIT', {openFont, returned_null_pointer}} ->
		exit({error, {failed_to_open_font, sdl_ttf:getError()}});
	    F0 ->
		F0
	end,

    %% run the getters (and a couple of setters)
    io:format("Vsn=~p~n",                 [sdl_ttf:linkedVersion()]),
    sdl_ttf:byteSwappedUNICODE(0),
    sdl_ttf:setFontStyle(F, FontStyle),
    io:format("getFontStyle=0x~.16b~n",   [sdl_ttf:getFontStyle(F)]),
    io:format("fontHeight=~p~n",          [sdl_ttf:fontHeight(F)]),
    io:format("fontAscent=~p~n",          [sdl_ttf:fontAscent(F)]),
    io:format("fontDescent=~p~n",         [sdl_ttf:fontDescent(F)]),
    io:format("fontLineSkip=~p~n",        [sdl_ttf:fontLineSkip(F)]),
    io:format("fontFaces=~p~n",           [sdl_ttf:fontFaces(F)]),
    io:format("fontFaceIsFixedWidth=~p~n",[sdl_ttf:fontFaceIsFixedWidth(F)]),
    io:format("fontFaceFamilyName=~p~n",  [sdl_ttf:fontFaceFamilyName(F)]),
    io:format("fontFaceStyleName=~p~n",   [sdl_ttf:fontFaceStyleName(F)]),
    io:format("glyphMetrics=~p~n",        [sdl_ttf:glyphMetrics(F, $A)]),
    io:format("sizeText=~p~n",            [sdl_ttf:sizeText(F, "abcd")]),
    io:format("sizeUTF8=~p~n",            [sdl_ttf:sizeUTF8(F, "abcd")]),
    io:format("sizeUNICODE=~p~n",         [sdl_ttf:sizeUNICODE(F, "abcd")]),

    %% draw some text
    print_at(Screen, F, TextStr,    FgColor, BgColor, latin1,  solid,  10, 10),
    print_at(Screen, F, Utf8Str,    FgColor, BgColor, utf8,    shaded, 20, 40),
    print_at(Screen, F, UnicodeStr, FgColor, BgColor, unicode, shaded, 30, 70),

    %% quit at keypress
    await_keypress(),
    sdl_ttf:closeFont(F),
    sdl:quit().


print_at(Screen, Font, TextStr, FgColor, BgColor, TextEnc, RenderMode, X, Y)-> 
    TextRef = mk_text_surface(Font,TextStr,FgColor,BgColor,TextEnc,RenderMode),
    Text = sdl_video:getSurface(TextRef),    
    blit_text(Screen, Text, BgColor, X, Y),
    %% save an image containing the text, just for fun
    sdl_video:saveBMP(Text, "/tmp/sdl-ttf-test.bmp"),
    %% set the text free :-)
    sdl_video:freeSurface(Text).


mk_text_surface(Font, TextStr, FgColor, BgColor, TextEnc, RenderMode) ->
    TextRef = render_text(Font,TextStr,FgColor,BgColor,TextEnc,RenderMode),
    Text = sdl_video:getSurface(TextRef),
    PFmt = sdl_video:getPixelFormat(TextRef),
    case PFmt#sdl_pixelformat.palette of
	null ->
	    io:format("No Palette found ~n"),
	    ignore;
	_Palette ->
 	    <<Pixel>> = sdl_video:getPixels(Text, #sdl_rect{x = 0, y = 0, 
							    w = 1, h = 1}),
 	    io:format("Palette found set key ~p to transparent ~n", 
 		      [Pixel]),
	    true = sdl_video:setColorKey(Text, 
					 ?SDL_SRCCOLORKEY bor ?SDL_RLEACCEL,
					 Pixel)
    end,
    case sdl_video:displayFormat(Text) of
	null ->
	    sdl_video:freeSurface(Text),
	    io:format("Failed to convert background~n"),
	    exit({error, convert_failed});
	ConvS ->
	    sdl_video:freeSurface(Text),
	    ConvS
    end.


render_text(Font, Text, FgColor, _BgColor, latin1, solid) ->
    sdl_ttf:renderTextSolid(Font, Text, FgColor);
render_text(Font, Text, FgColor, BgColor, latin1, shaded) ->
    sdl_ttf:renderTextShaded(Font, Text, FgColor, BgColor);
render_text(Font, Text, FgColor, _BgColor, latin1, blended) ->
    sdl_ttf:renderTextBlended(Font, Text, FgColor);
render_text(Font, Text, FgColor, _BgColor, utf8, solid) ->
    sdl_ttf:renderUTF8Solid(Font, Text, FgColor);
render_text(Font, Text, FgColor, BgColor, utf8, shaded) ->
    sdl_ttf:renderUTF8Shaded(Font, Text, FgColor, BgColor);
render_text(Font, Text, FgColor, _BgColor, utf8, blended) ->
    sdl_ttf:renderUTF8Blended(Font, Text, FgColor);
render_text(Font, Text, FgColor, _BgColor, unicode, solid) ->
    sdl_ttf:renderUNICODESolid(Font, Text, FgColor);
render_text(Font, Text, FgColor, BgColor, unicode, shaded) ->
    sdl_ttf:renderUNICODEShaded(Font, Text, FgColor, BgColor);
render_text(Font, Text, FgColor, _BgColor, unicode, blended) ->
    sdl_ttf:renderUNICODEBlended(Font, Text, FgColor).


blit_text(Screen, Text, BgColor, X, Y) ->
    Background = sdl_video:mapRGB(Screen, 
				  BgColor#sdl_color.r, 
				  BgColor#sdl_color.g, 
				  BgColor#sdl_color.b),
    Rect = #sdl_rect{x = X, 
		     y = Y, 
		     w = Text#sdl_surface.w, 
		     h = Text#sdl_surface.h},
    true = sdl_video:fillRect(Screen, null, Background),
    {null, ClippedRect} = sdl_video:blitSurface(Text, null, Screen, Rect),
    sdl_video:updateRects(Screen, [ClippedRect]),
    sdl_video:getSurface(Text).


listen_to_keypress_events() ->
    sdl_events:eventState(?SDL_ALLEVENTS ,?SDL_IGNORE),
    sdl_events:eventState(?SDL_KEYDOWN ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_QUIT ,?SDL_ENABLE),
    ?printError().

await_keypress() ->
    sdl_events:waitEvent().
