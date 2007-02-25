/*  
 *  Copyright (c) 2007 Klas Johansson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 * 
 * 
 *     $Id$
 */
/* Defines the text/ttf functions */ 

#ifdef _USE_SDL_TTF

#ifdef __cplusplus
    extern "C" {
#endif 

#define SDL_TTF_Linked_VersionFunc         TTF_H +1
void es_ttf_linkedVersion(sdl_data *, int, char *);
#define SDL_TTF_ByteSwappedUNICODEFunc     SDL_TTF_Linked_VersionFunc +1
void es_ttf_byteSwappedUNICODE(sdl_data *, int, char *);
#define SDL_TTF_InitFunc                   SDL_TTF_ByteSwappedUNICODEFunc +1
void es_ttf_init(sdl_data *, int, char *);
#define SDL_TTF_OpenFontFunc               SDL_TTF_InitFunc +1
void es_ttf_openFont(sdl_data *, int, char *);
#define SDL_TTF_OpenFontIndexFunc          SDL_TTF_OpenFontFunc +1
void es_ttf_openFontIndex(sdl_data *, int, char *);
#define SDL_TTF_OpenFontRWFunc             SDL_TTF_OpenFontIndexFunc +1
void es_ttf_openFontRW(sdl_data *, int, char *);
#define SDL_TTF_OpenFontIndexRWFunc        SDL_TTF_OpenFontRWFunc +1
void es_ttf_openFontIndexRW(sdl_data *, int, char *);
#define SDL_TTF_GetFontStyleFunc           SDL_TTF_OpenFontIndexRWFunc +1
void es_ttf_getFontStyle(sdl_data *, int, char *);
#define SDL_TTF_SetFontStyleFunc           SDL_TTF_GetFontStyleFunc +1
void es_ttf_setFontStyle(sdl_data *, int, char *);
#define SDL_TTF_FontHeightFunc             SDL_TTF_SetFontStyleFunc +1
void es_ttf_fontHeight(sdl_data *, int, char *);
#define SDL_TTF_FontAscentFunc             SDL_TTF_FontHeightFunc +1
void es_ttf_fontAscent(sdl_data *, int, char *);
#define SDL_TTF_FontDescentFunc            SDL_TTF_FontAscentFunc +1
void es_ttf_fontDescent(sdl_data *, int, char *);
#define SDL_TTF_FontLineSkipFunc           SDL_TTF_FontDescentFunc +1
void es_ttf_fontLineSkip(sdl_data *, int, char *);
#define SDL_TTF_FontFacesFunc              SDL_TTF_FontLineSkipFunc +1
void es_ttf_fontFaces(sdl_data *, int, char *);
#define SDL_TTF_FontFaceIsFixedWidthFunc   SDL_TTF_FontFacesFunc +1
void es_ttf_fontFaceIsFixedWidth(sdl_data *, int, char *);
#define SDL_TTF_FontFaceFamilyNameFunc     SDL_TTF_FontFaceIsFixedWidthFunc +1
void es_ttf_fontFaceFamilyName(sdl_data *, int, char *);
#define SDL_TTF_FontFaceStyleNameFunc      SDL_TTF_FontFaceFamilyNameFunc +1
void es_ttf_fontFaceStyleName(sdl_data *, int, char *);
#define SDL_TTF_GlyphMetricsFunc           SDL_TTF_FontFaceStyleNameFunc +1
void es_ttf_glyphMetrics(sdl_data *, int, char *);
#define SDL_TTF_SizeTextFunc               SDL_TTF_GlyphMetricsFunc +1
void es_ttf_sizeText(sdl_data *, int, char *);
#define SDL_TTF_SizeUTF8Func               SDL_TTF_SizeTextFunc +1
void es_ttf_sizeUTF8(sdl_data *, int, char *);
#define SDL_TTF_SizeUNICODEFunc            SDL_TTF_SizeUTF8Func +1
void es_ttf_sizeUNICODE(sdl_data *, int, char *);
#define SDL_TTF_RenderText_SolidFunc       SDL_TTF_SizeUNICODEFunc +1
void es_ttf_renderTextSolid(sdl_data *, int, char *);
#define SDL_TTF_RenderUTF8_SolidFunc       SDL_TTF_RenderText_SolidFunc +1
void es_ttf_renderUTF8Solid(sdl_data *, int, char *);
#define SDL_TTF_RenderUNICODE_SolidFunc    SDL_TTF_RenderUTF8_SolidFunc +1
void es_ttf_renderUNICODESolid(sdl_data *, int, char *);
#define SDL_TTF_RenderGlyph_SolidFunc      SDL_TTF_RenderUNICODE_SolidFunc +1
void es_ttf_renderGlyphSolid(sdl_data *, int, char *);
#define SDL_TTF_RenderText_ShadedFunc      SDL_TTF_RenderGlyph_SolidFunc +1
void es_ttf_renderTextShaded(sdl_data *, int, char *);
#define SDL_TTF_RenderUTF8_ShadedFunc      SDL_TTF_RenderText_ShadedFunc +1
void es_ttf_renderUTF8Shaded(sdl_data *, int, char *);
#define SDL_TTF_RenderUNICODE_ShadedFunc   SDL_TTF_RenderUTF8_ShadedFunc +1
void es_ttf_renderUNICODEShaded(sdl_data *, int, char *);
#define SDL_TTF_RenderGlyph_ShadedFunc     SDL_TTF_RenderUNICODE_ShadedFunc +1
void es_ttf_renderGlyphShaded(sdl_data *, int, char *);
#define SDL_TTF_RenderText_BlendedFunc     SDL_TTF_RenderGlyph_ShadedFunc +1
void es_ttf_renderTextBlended(sdl_data *, int, char *);
#define SDL_TTF_RenderUTF8_BlendedFunc     SDL_TTF_RenderText_BlendedFunc +1
void es_ttf_renderUTF8Blended(sdl_data *, int, char *);
#define SDL_TTF_RenderUNICODE_BlendedFunc  SDL_TTF_RenderUTF8_BlendedFunc +1
void es_ttf_renderUNICODEBlended(sdl_data *, int, char *);
#define SDL_TTF_RenderGlyph_BlendedFunc    SDL_TTF_RenderUNICODE_BlendedFunc +1
void es_ttf_renderGlyphBlended(sdl_data *, int, char *);
#define SDL_TTF_CloseFontFunc              SDL_TTF_RenderGlyph_BlendedFunc +1
void es_ttf_closeFont(sdl_data *, int, char *);
#define SDL_TTF_QuitFunc                   SDL_TTF_CloseFontFunc +1
void es_ttf_quit(sdl_data *, int, char *);
#define SDL_TTF_WasInitFunc                SDL_TTF_QuitFunc +1
void es_ttf_wasInit(sdl_data *, int, char *);
#define SDL_TTF_SetErrorFunc               SDL_TTF_WasInitFunc +1
void es_ttf_setError(sdl_data *, int, char *);
#define SDL_TTF_GetErrorFunc               SDL_TTF_SetErrorFunc +1
void es_ttf_getError(sdl_data *, int, char *);

#ifdef __cplusplus
    }
#endif 

#endif /* _USE_SDL_TTF */
