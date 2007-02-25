%%  Copyright (c) 2007 Klas Johansson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_ttf_funcs.hrl
%%% Author  : Klas Johansson <klajo at users.sourceforge.net>
%%% Purpose : 
%%% Created : 29 Jan 2007 by Klas Johansson <klajo at users.sourceforge.net>
%%%----------------------------------------------------------------------

%%% Functions  
-include("esdl.hrl").

-define(SDL_TTF_LinkedVersion,        ?SDL_TTF_HRL + 1).
-define(SDL_TTF_ByteSwappedUNICODE,   ?SDL_TTF_LinkedVersion + 1).
-define(SDL_TTF_Init,                 ?SDL_TTF_ByteSwappedUNICODE + 1).
-define(SDL_TTF_OpenFont,             ?SDL_TTF_Init + 1).
-define(SDL_TTF_OpenFontIndex,        ?SDL_TTF_OpenFont + 1).
-define(SDL_TTF_OpenFontRW,           ?SDL_TTF_OpenFontIndex + 1).
-define(SDL_TTF_OpenFontIndexRW,      ?SDL_TTF_OpenFontRW + 1).
-define(SDL_TTF_GetFontStyle,         ?SDL_TTF_OpenFontIndexRW + 1).
-define(SDL_TTF_SetFontStyle,         ?SDL_TTF_GetFontStyle + 1).
-define(SDL_TTF_FontHeight,           ?SDL_TTF_SetFontStyle + 1).
-define(SDL_TTF_FontAscent,           ?SDL_TTF_FontHeight + 1).
-define(SDL_TTF_FontDescent,          ?SDL_TTF_FontAscent + 1).
-define(SDL_TTF_FontLineSkip,         ?SDL_TTF_FontDescent + 1).
-define(SDL_TTF_FontFaces,            ?SDL_TTF_FontLineSkip + 1).
-define(SDL_TTF_FontFaceIsFixedWidth, ?SDL_TTF_FontFaces + 1).
-define(SDL_TTF_FontFaceFamilyName,   ?SDL_TTF_FontFaceIsFixedWidth + 1).
-define(SDL_TTF_FontFaceStyleName,    ?SDL_TTF_FontFaceFamilyName + 1).
-define(SDL_TTF_GlyphMetrics,         ?SDL_TTF_FontFaceStyleName + 1).
-define(SDL_TTF_SizeText,             ?SDL_TTF_GlyphMetrics + 1).
-define(SDL_TTF_SizeUTF8,             ?SDL_TTF_SizeText + 1).
-define(SDL_TTF_SizeUNICODE,          ?SDL_TTF_SizeUTF8 + 1).
-define(SDL_TTF_RenderTextSolid,      ?SDL_TTF_SizeUNICODE + 1).
-define(SDL_TTF_RenderUTF8Solid,      ?SDL_TTF_RenderTextSolid + 1).
-define(SDL_TTF_RenderUNICODESolid,   ?SDL_TTF_RenderUTF8Solid + 1).
-define(SDL_TTF_RenderGlyphSolid,     ?SDL_TTF_RenderUNICODESolid + 1).
-define(SDL_TTF_RenderTextShaded,     ?SDL_TTF_RenderGlyphSolid + 1).
-define(SDL_TTF_RenderUTF8Shaded,     ?SDL_TTF_RenderTextShaded + 1).
-define(SDL_TTF_RenderUNICODEShaded,  ?SDL_TTF_RenderUTF8Shaded + 1).
-define(SDL_TTF_RenderGlyphShaded,    ?SDL_TTF_RenderUNICODEShaded + 1).
-define(SDL_TTF_RenderTextBlended,    ?SDL_TTF_RenderGlyphShaded + 1).
-define(SDL_TTF_RenderUTF8Blended,    ?SDL_TTF_RenderTextBlended + 1).
-define(SDL_TTF_RenderUNICODEBlended, ?SDL_TTF_RenderUTF8Blended + 1).
-define(SDL_TTF_RenderGlyphBlended,   ?SDL_TTF_RenderUNICODEBlended + 1).
-define(SDL_TTF_CloseFont,            ?SDL_TTF_RenderGlyphBlended + 1).
-define(SDL_TTF_Quit,                 ?SDL_TTF_CloseFont + 1).
-define(SDL_TTF_WasInit,              ?SDL_TTF_Quit + 1).
-define(SDL_TTF_SetError,             ?SDL_TTF_WasInit + 1).
-define(SDL_TTF_GetError,             ?SDL_TTF_SetError + 1).
