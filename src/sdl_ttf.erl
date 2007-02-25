%%  Copyright (c) 2007 Klas Johansson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_ttf.erl
%%% Author  : Klas Johansson <klajo at users.sourceforge.net>
%%% Purpose : Provide access to SDL's TTF functions (tested with SDL_ttf 2.0.7)
%%% Created : 29 Jan 2007 by Klas Johansson <klajo at users.sourceforge.net>
%%%----------------------------------------------------------------------

-module(sdl_ttf).

-include("sdl_ttf.hrl").
-include("sdl_ttf_funcs.hrl").
-include("gl.hrl").
-include("sdl_util.hrl").
-include("sdl_video.hrl").

%%% ERL_SDL functions
-export([]).
%%% SDL_Functions
-export([linkedVersion/0]).
-export([byteSwappedUNICODE/1]).
-export([init/0]).

-export([openFont/2]).
-export([openFontIndex/3]).
-export([openFontRW/0]).
-export([openFontIndexRW/0]).

-export([getFontStyle/1]).
-export([setFontStyle/2]).

-export([fontHeight/1]).
-export([fontAscent/1]).
-export([fontDescent/1]).
-export([fontLineSkip/1]).
-export([fontFaces/1]).
-export([fontFaceIsFixedWidth/1]).
-export([fontFaceFamilyName/1]).
-export([fontFaceStyleName/1]).

-export([glyphMetrics/2]).
-export([sizeText/2]).
-export([sizeUTF8/2]).
-export([sizeUNICODE/2]).

-export([renderTextSolid/3]).
-export([renderUTF8Solid/3]).
-export([renderUNICODESolid/3]).
-export([renderGlyphSolid/3]).

-export([renderTextShaded/4]).
-export([renderUTF8Shaded/4]).
-export([renderUNICODEShaded/4]).
-export([renderGlyphShaded/4]).

-export([renderTextBlended/3]).
-export([renderUTF8Blended/3]).
-export([renderUNICODEBlended/3]).
-export([renderGlyphBlended/3]).

-export([closeFont/1]).
-export([quit/0]).
-export([wasInit/0]).

-export([setError/1]).
-export([getError/0]).

%% Imports
-import(sdl, [call/2, cast/2]).

%% Defines 
-define(INT,    16/signed).
-define(LONG,   32/signed).
-define(UINT8,   8/unsigned).
-define(UINT16, 16/unsigned).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ERL_SDL API

%% Func: linkedVersion
%% Args: -
%% Returns: {Major, Minor, Patch}
%% C-API func: const SDL_version * TTF_Linked_Version(void);
%% Desc: This function gets the version of the SDL_ttf library.
linkedVersion() ->
    <<Major:?UINT8, Minor:?UINT8, Patch:?UINT8>> = 
	call(?SDL_TTF_LinkedVersion, []),
    {Major, Minor, Patch}.

%% Func: byteSwappedUNICODE
%% Args: Int
%% Returns: void
%% C-API func: void TTF_ByteSwappedUNICODE(int swapped);
%% Desc: This function tells the library whether UNICODE text is generally
%%       byteswapped.  A UNICODE BOM character in a string will override
%%       this setting for the remainder of that string.
byteSwappedUNICODE(Int) ->
    cast(?SDL_TTF_ByteSwappedUNICODE, [<<Int:?INT>>]).

%% Func: init
%% Args: -
%% Returns: Result
%% C-API func: int TTF_Init(void);
%% Desc: Initialize the TTF engine.  Returns 0 on success, -1 on any error.
init() ->
    <<Res:?INT>> = call(?SDL_TTF_Init, []),
    Res.

%% Func: openFont
%% Args: File, PointSize
%% Returns: TTF_Font Ref
%% C-API func: TTF_Font * TTF_OpenFont(const char *file, int ptsize);
%% Desc: Open a font file and create a font of the specified point size.
openFont(File, PointSize) ->
    <<FontP:?_PTR>> = call(?SDL_TTF_OpenFont, [[File,0], <<PointSize:?INT>>]),
    case FontP of 
	0 -> exit({openFont, returned_null_pointer});
	_ -> #ttf_font{ptr = FontP}
    end.

%% Func: openFontIndex
%% Args: File, PointSize, Index
%% Returns: TTF_Font Ref
%% C-API func: TTF_Font * TTF_OpenFontIndex(const char *file, 
%%                                          int ptsize, 
%%                                          long index);
%% Desc: Open a font file and create a font of the specified point size.
%%       Some .fon fonts will have several sizes embedded in the file, so the
%%       point size becomes the index of choosing which size.  If the value
%%       is too high, the last indexed size will be the default.
openFontIndex(File, PointSize, Index) ->
    Args = [[File,0], <<PointSize:?INT>>, <<Index:?LONG>>],
    <<FontP:?_PTR>> = call(?SDL_TTF_OpenFontIndex, Args),
    case FontP of 
	0 -> exit({openFont, returned_null_pointer});
	_ -> #ttf_font{ptr = FontP}
    end.

%% Func: openFontRW
%% Args: N/A
%% Returns: N/A
%% C-API func: TTF_Font * TTF_OpenFontRW(SDL_RWops *src, 
%%                                       int freesrc, 
%%                                       int ptsize);
%% Desc: Not implemented.
openFontRW() ->
    exit({not_implemented, openFontRW}).

%% Func: openFontIndexRW
%% Args: N/A
%% Returns: N/A
%% C-API func: TTF_Font * TTF_OpenFontIndexRW(SDL_RWops *src, 
%%                                            int freesrc, 
%%                                            int ptsize, 
%%                                            long index);
%% Desc: Not implemented.
openFontIndexRW() ->
    exit({not_implemented, openFontIndexRW}).

%% Func: getFontStyle
%% Args: TTF_Font Ref
%% Returns: FontStyle
%% C-API func: int TTF_GetFontStyle(TTF_Font *font);
%% Desc: Retrieve the font style.
%%       This font style is implemented by modifying the font glyphs, and
%%       doesn't reflect any inherent properties of the truetype font file.
getFontStyle(Font) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    <<FontStyle:16>> = call(?SDL_TTF_GetFontStyle, [<<FontP:?_PTR>>]),
    FontStyle.

%% Func: setFontStyle
%% Args: TTF_Font Ref, FontStyle
%% Returns:
%% C-API func: void TTF_SetFontStyle(TTF_Font *font, int style);
%% Desc: Set the font style.
%%       This font style is implemented by modifying the font glyphs, and
%%       doesn't reflect any inherent properties of the truetype font file.
setFontStyle(Font, FontStyle) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    cast(?SDL_TTF_SetFontStyle, [<<FontP:?_PTR>>, <<FontStyle:?INT>>]).

%% Func: fontHeight
%% Args: TTF_Font Ref
%% Returns: FontHeight
%% C-API func: int TTF_FontHeight(TTF_Font *font);
%% Desc: Get the total height of the font - usually equal to point size
fontHeight(Font) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    <<FontHeight:?INT>> = call(?SDL_TTF_FontHeight, [<<FontP:?_PTR>>]),
    FontHeight.

%% Func: fontAscent
%% Args: TTF_Font Ref
%% Returns: FontAscent
%% C-API func: int TTF_FontAscent(TTF_Font *font);
%% Desc: Get the offset from the baseline to the top of the font
%%       This is a positive value, relative to the baseline.
fontAscent(Font) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    <<FontAscent:?INT>> = call(?SDL_TTF_FontAscent, [<<FontP:?_PTR>>]),
    FontAscent.

%% Func: fontDescent
%% Args: TTF_Font Ref
%% Returns: FontDescent
%% C-API func: int TTF_FontDescent(TTF_Font *font);
%% Desc: Get the offset from the baseline to the bottom of the font
%%       This is a negative value, relative to the baseline.
fontDescent(Font) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    <<FontDescent:?INT>> = call(?SDL_TTF_FontDescent, [<<FontP:?_PTR>>]),
    FontDescent.

%% Func: fontLineSkip
%% Args: TTF_Font Ref
%% Returns: FontLineSkip
%% C-API func: int TTF_FontLineSkip(TTF_Font *font);
%% Desc: Get the recommended spacing between lines of text for this font
fontLineSkip(Font) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    <<FontLineSkip:?INT>> = call(?SDL_TTF_FontLineSkip, [<<FontP:?_PTR>>]),
    FontLineSkip.

%% Func: fontFaces
%% Args: TTF_Font Ref
%% Returns: FontFaces
%% C-API func: long TTF_FontFaces(TTF_Font *font);
%% Desc: Get the number of faces of the font
fontFaces(Font) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    <<FontFaces:?LONG>> = call(?SDL_TTF_FontFaces, [<<FontP:?_PTR>>]),
    FontFaces.

%% Func: fontFaceIsFixedWidth
%% Args: TTF_Font Ref
%% Returns: IsFixedWidth
%% C-API func: int TTF_FontFaceIsFixedWidth(TTF_Font *font);
%% Desc: Test if the current font face of the loaded font is a fixed
%%       width font. Fixed width fonts are monospace, meaning every 
%%       character that exists in the font is the same width, thus you
%%       can assume that a rendered string's width is going to be
%%       the result of a simple calculation:
%%       glyph_width * string_length 
fontFaceIsFixedWidth(Font) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    <<IsFixedW:?INT>> = call(?SDL_TTF_FontFaceIsFixedWidth, [<<FontP:?_PTR>>]),
    IsFixedW.

%% Func: fontFaceFamilyName
%% Args: TTF_Font Ref
%% Returns: FaceFamilyName
%% C-API func: char * TTF_FontFaceFamilyName(TTF_Font *font);
%% Desc: Get the current font face family name from the loaded
%%       font. This function may return a NULL pointer, in which case
%%       the information is not available.
fontFaceFamilyName(Font) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    binary_to_list(call(?SDL_TTF_FontFaceFamilyName, [<<FontP:?_PTR>>])).

%% Func: fontFaceStyleName
%% Args: TTF_Font Ref
%% Returns: FaceStyleName
%% C-API func: char * TTF_FontFaceStyleName(TTF_Font *font);
%% Desc: Get the current font face style name from the loaded
%%       font. This function may return a NULL pointer, in which case
%%       the information is not available.
fontFaceStyleName(Font) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    binary_to_list(call(?SDL_TTF_FontFaceStyleName, [<<FontP:?_PTR>>])).

%% Func: glyphMetrics
%% Args: TTF_Font Ref, Ch
%% Returns: {MinX, MinY, MaxX, MaxY, Advance}
%% C-API func: int TTF_GlyphMetrics(TTF_Font *font, Uint16 ch,
%%                                  int *minx, int *maxx,
%%                                  int *miny, int *maxy, 
%%                                  int *advance);
%% Desc: Get the metrics (dimensions) of a glyph.
glyphMetrics(Font, Ch) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    <<Res:?INT, MinX:?INT, MaxX:?INT, MinY:?INT, MaxY:?INT, Adv:?INT>> =
	call(?SDL_TTF_GlyphMetrics, [<<FontP:?_PTR>>, <<Ch:?INT>>]),
    case Res of
	0  -> {MinX, MaxX, MinY, MaxY, Adv};
	-1 -> exit({error, {failed_to_get_glyph_metrics, Ch}})
    end.

%% Func: sizeText
%% Args: TTF_Font Ref, Text
%% Returns: {Width, Height}
%% C-API func: int TTF_SizeText(TTF_Font *font, const char *text, 
%%                              int *w, int *h);
%% Desc: Get the dimensions of a rendered string of text.
sizeText(Font, Text) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    <<Res:?INT, W:?INT, H:?INT>> = 
	call(?SDL_TTF_SizeText, [<<FontP:?_PTR>>, [Text,0]]),
    case Res of
	0  -> {W, H};
	-1 -> exit({error, {failed_to_get_text_size, Text}})
    end.

%% Func: sizeUTF8
%% Args: TTF_Font Ref, Text
%% Returns: {Width, Height}
%% C-API func: int TTF_SizeUTF8(TTF_Font *font, const char *text, 
%%                              int *w, int *h);
%% Desc: Get the dimensions of a rendered string of text.
sizeUTF8(Font, Text) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    <<Res:?INT, W:?INT, H:?INT>> = 
	call(?SDL_TTF_SizeUTF8, [<<FontP:?_PTR>>, [Text,0]]),
    case Res of
	0  -> {W, H};
	-1 -> exit({error, {failed_to_get_utf8_text_size, Text}})
    end.

%% Func: sizeUNICODE
%% Args: TTF_Font Ref, Text
%% Returns: {Width, Height}
%% C-API func: int TTF_SizeUNICODE(TTF_Font *font, const Uint16 *text, 
%%                                 int *w, int *h);
%% Desc: Get the dimensions of a rendered string of text.
sizeUNICODE(Font, Text) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    Args = [<<FontP:?_PTR>>, mk_unicode_text(Text)],
    <<Res:?INT, W:?INT, H:?INT>> = call(?SDL_TTF_SizeUNICODE, Args),
    case Res of
	0  -> {W, H};
	-1 -> exit({error, {failed_to_get_unicode_text_size, Text}})
    end.

%% Func: renderTextSolid
%% Args: TTF_Font Ref, Text, FgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderText_Solid(TTF_Font *font,
%%                                                const char *text, 
%%                                                SDL_Color fg);
%% Desc: Create an 8-bit palettized surface and render the given text
%%       at the fast quality with the given font and color.
%%     
%%       The palette has 0 as the colorkey, giving it a transparent
%%       background, with 1 as the text color.
renderTextSolid(Font, Text, FgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderTextSolid, 
		   mk_render_args(FontP, mk_latin1_text(Text), FgColor)).

%% Func: renderUTF8Solid
%% Args: TTF_Font Ref, Text, FgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderUTF8_Solid(TTF_Font *font,
%%                                                const char *text, 
%%                                                SDL_Color fg);
%% Desc: Create an 8-bit palettized surface and render the given text
%%       at the fast quality with the given font and color.
%%     
%%       The palette has 0 as the colorkey, giving it a transparent
%%       background, with 1 as the text color.
renderUTF8Solid(Font, Text, FgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderUTF8Solid, 
                   mk_render_args(FontP, mk_utf8_text(Text), FgColor)).

%% Func: renderUNICODESolid
%% Args: TTF_Font Ref, Text, FgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderUNICODE_Solid(TTF_Font *font,
%%                                                   const Uint16 *text,
%%                                                   SDL_Color fg);
%% Desc: Create an 8-bit palettized surface and render the given text
%%       at the fast quality with the given font and color.
%%     
%%       The palette has 0 as the colorkey, giving it a transparent
%%       background, with 1 as the text color.
renderUNICODESolid(Font, Text, FgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderUNICODESolid, 
		   mk_render_args(FontP, mk_unicode_text(Text), FgColor)).

%% Func: renderGlyphSolid
%% Args: TTF_Font Ref, Glyph, FgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderGlyph_Solid(TTF_Font *font,
%%                                                 Uint16 ch, 
%%                                                 SDL_Color fg);
%% Desc: Create an 8-bit palettized surface and render the given glyph
%%       at fast quality with the given font and color.  The 0 pixel
%%       is the colorkey, giving a transparent background, and the 1
%%       pixel is set to the text color.  The glyph is rendered
%%       without any padding or centering in the X direction, and
%%       aligned normally in the Y direction.
renderGlyphSolid(Font, Glyph, FgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderGlyphSolid, 
		   mk_render_args(FontP, mk_uint16_glyph(Glyph), FgColor)).

%% Func: renderTextShaded
%% Args: TTF_Font Ref, Text, FgColor, BgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderText_Shaded(TTF_Font *font,
%%                                                 const char *text, 
%%                                                 SDL_Color fg, 
%%                                                 SDL_Color bg);
%% Desc: Create an 8-bit palettized surface and render the given text
%%       at high quality with the given font and colors.
%%
%%       The 0 pixel is background, while other pixels have varying
%%       degrees of the foreground color.
renderTextShaded(Font, Text, FgColor, BgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderTextShaded, 
		   mk_render_args(FontP,mk_latin1_text(Text),FgColor,BgColor)).

%% Func: renderUTF8Shaded
%% Args: TTF_Font Ref, Text, FgColor, BgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderUTF8_Shaded(TTF_Font *font,
%%                                                 const char *text, 
%%                                                 SDL_Color fg, 
%%                                                 SDL_Color bg);
%% Desc: Create an 8-bit palettized surface and render the given text
%%       at high quality with the given font and colors.
%%
%%       The 0 pixel is background, while other pixels have varying
%%       degrees of the foreground color.
renderUTF8Shaded(Font, Text, FgColor, BgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderUTF8Shaded, 
		   mk_render_args(FontP,mk_utf8_text(Text),FgColor,BgColor)).

%% Func: renderUNICODEShaded
%% Args: TTF_Font Ref, Text, FgColor, BgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderUNICODE_Shaded(TTF_Font *font,
%%                                                    const Uint16 *text,
%%                                                    SDL_Color fg, 
%%                                                    SDL_Color bg);
%% Desc: Create an 8-bit palettized surface and render the given text
%%       at high quality with the given font and colors.
%%
%%       The 0 pixel is background, while other pixels have varying
%%       degrees of the foreground color.
renderUNICODEShaded(Font, Text, FgColor, BgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderUNICODEShaded, 
		   mk_render_args(FontP,mk_unicode_text(Text),FgColor,BgColor)).

%% Func: renderGlyphShaded
%% Args: TTF_Font Ref, Glyph, FgColor, BgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderGlyph_Shaded(TTF_Font *font,
%%                                                  Uint16 ch, 
%%                                                  SDL_Color fg, 
%%                                                  SDL_Color bg);
%% Desc: Create an 8-bit palettized surface and render the given glyph
%%       at high quality with the given font and colors.  The 0 pixel
%%       is background, while other pixels have varying degrees of the
%%       foreground color. The glyph is rendered without any padding
%%       or centering in the X direction, and aligned normally in the
%%       Y direction.
renderGlyphShaded(Font, Glyph, FgColor, BgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderGlyphShaded, 
		   mk_render_args(FontP, mk_uint16_glyph(Glyph),
				  FgColor, BgColor)).

%% Func: renderTextBlended
%% Args: TTF_Font Ref, Text, FgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderText_Blended(TTF_Font *font,
%%                                                  const char *text, 
%%                                                  SDL_Color fg);
%% Desc: Create a 32-bit ARGB surface and render the given text at
%%       high quality, using alpha blending to dither the font with
%%       the given color.
renderTextBlended(Font, Text, FgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderTextBlended, 
		   mk_render_args(FontP, mk_latin1_text(Text), FgColor)).

%% Func: renderUTF8Blended
%% Args: TTF_Font Ref, Text, FgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderUTF8_Blended(TTF_Font *font,
%%                                                  const char *text, 
%%                                                  SDL_Color fg);
%% Desc: Create a 32-bit ARGB surface and render the given text at
%%       high quality, using alpha blending to dither the font with
%%       the given color.
renderUTF8Blended(Font, Text, FgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderUTF8Blended, 
		   mk_render_args(FontP, mk_utf8_text(Text), FgColor)).

%% Func: renderUNICODEBlended
%% Args: TTF_Font Ref, Text, FgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderUNICODE_Blended(TTF_Font *font,
%%                                                     const Uint16 *text,
%%                                                     SDL_Color fg);
%% Desc: Create a 32-bit ARGB surface and render the given text at
%%       high quality, using alpha blending to dither the font with
%%       the given color.
renderUNICODEBlended(Font, Text, FgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderUNICODEBlended, 
		   mk_render_args(FontP, mk_unicode_text(Text), FgColor)).

%% Func: renderGlyphBlended
%% Args: TTF_Font Ref, Glyph, FgColor
%% Returns: SDL_Surface Ref
%% C-API func: SDL_Surface * TTF_RenderGlyph_Blended(TTF_Font *font,
%%                                                   Uint16 ch, 
%%                                                   SDL_Color fg);
%% Desc: Create a 32-bit ARGB surface and render the given glyph at
%%       high quality, using alpha blending to dither the font with
%%       the given color. The glyph is rendered without any padding or
%%       centering in the X direction, and aligned normally in the
%%       Y direction.
renderGlyphBlended(Font, Glyph, FgColor) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    mk_render_call(?SDL_TTF_RenderGlyphBlended, 
		   mk_render_args(FontP, mk_uint16_glyph(Glyph), FgColor)).

%% Func: closeFont
%% Args: TTF_Font Ref
%% Returns: void
%% C-API func: void TTF_CloseFont(TTF_Font *font);
%% Desc: Close an opened font file.
closeFont(Font) when record(Font, ttf_font) ->
    FontP = Font#ttf_font.ptr,
    call(?SDL_TTF_CloseFont, [<<FontP:?_PTR>>]).

%% Func: quit
%% Args: -
%% Returns: void
%% C-API func: void TTF_Quit(void);
%% Desc: De-initialize the TTF engine.
quit() ->
    call(?SDL_TTF_Quit, []).

%% Func: wasInit
%% Args: -
%% Returns: Int
%% C-API func: int TTF_WasInit(void);
%% Desc: Check if the TTF engine is initialized.
wasInit() ->
    <<WasInit:?INT>> = call(?SDL_TTF_WasInit, []),
    WasInit.

%% Func: setError
%% Args: N/A
%% Returns: N/A
%% C-API func: void TTF_SetError(const char *fmt, ...)
%% Desc: Not implemented.
setError(_Error) ->
    exit({not_implemented, setError}).

%% Func: getError
%% Args: -
%% Returns: Error
%% C-API func: char * TTF_GetError()
%% Desc: Returns a (string) containing a human readable version or the
%%       reason for the last error that occured.
getError() ->
    Bin = call(?SDL_TTF_GetError, []),
    binary_to_list(Bin).

%%%%%%%%%%% Internal functions %%%%%%%%%%%    

%% Note: The order in which the arguments arrive and are passed to
%%       the driver differ.  We're avoiding a strlen (and the
%%       like) in the driver by having the text as the last
%%       parameter.
mk_render_args(FontP, Text, FgColor) ->
    [<<FontP:?_PTR>>, mk_color(FgColor), Text].
mk_render_args(FontP, Text, FgColor, BgColor) ->
    [<<FontP:?_PTR>>, mk_color(FgColor), mk_color(BgColor), Text].

mk_render_call(RenderFunc, Args) ->
    <<SurfacePtr:?_PTR>> = call(RenderFunc, Args),
    case SurfacePtr of
	0 -> exit({render_function_returned_null_pointer, RenderFunc});
	_ -> {surfacep, SurfacePtr}
    end.

mk_latin1_text(Text)  -> [Text,0].
mk_utf8_text(Text)    -> [Text,0].
mk_unicode_text(Text) -> lists:map(fun(C) -> <<C:?UINT16-native>> end, 
				   Text++"\0").

mk_uint16_glyph(C)    -> <<C:?UINT16>>.
     
mk_color(C) ->
    <<(C#sdl_color.r):?UINT8, (C#sdl_color.g):?UINT8,(C#sdl_color.b):?UINT8>>. 
