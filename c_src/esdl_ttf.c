/*  
 *  Copyright (c) 2007 Klas Johanssson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 * 
 *     $Id$
 */
/* 
   Map erl esdl_ttf calls to C sdl ttf calls      
*/

#include "esdl.h"
#include <string.h>
#include <stdlib.h>
#include <SDL_ttf.h>

void es_ttf_linkedVersion(sdl_data *sd, int len, char *buff)
{
   char *bp, *start;
   int sendlen;
   const SDL_version *version;

   version = TTF_Linked_Version();

   bp = start = sdl_get_temp_buff(sd, 3);
   put8(bp, version->major);
   put8(bp, version->minor);
   put8(bp, version->patch);
   sendlen = bp - start;
   sdl_send(sd, sendlen);
}

void es_ttf_byteSwappedUNICODE(sdl_data *sd, int len, char *buff)
{
    char *bp;
    int byteswap;
    
    bp = buff;   
    byteswap = get16be(bp);
    TTF_ByteSwappedUNICODE(byteswap);
}

void es_ttf_init(sdl_data *sd, int len, char *buff) 
{
    char *bp, *start;
    int sendlen;
    int result;

    result = TTF_Init();
    
    bp = start = sdl_get_temp_buff(sd, 2);
    put16be(bp, result);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_openFont(sdl_data *sd, int len, char *buff)
{
    char *file, *bp, *start;
    int pointsize;
    int sendlen;
    TTF_Font *font;

    // read filename and pointsize
    file = buff;
    bp = file + strlen(file) + 1;
    pointsize = get16be(bp);
    
    // load font
    font = TTF_OpenFont(file, pointsize);
    
    // return a font pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(font, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_openFontIndex(sdl_data *sd, int len, char *buff)
{
    char *file, *bp, *start;
    int pointsize;
    long index;
    int sendlen;
    TTF_Font *font;

    // read filename and pointsize
    file = buff;
    bp = file + strlen(file) + 1;
    pointsize = get16be(bp);
    index = get32be(bp);
    
    // load font
    font = TTF_OpenFontIndex(file, pointsize, index);
    
    // return a font pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(font, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_openFontRW(sdl_data *sd, int len, char *buff)
{
    // not implemented
}

void es_ttf_openFontIndexRW(sdl_data *sd, int len, char *buff)
{
    // not implemented
}

void es_ttf_getFontStyle(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    int sendlen;
    TTF_Font *font;
    int fontstyle;

    bp = buff;
    POPGLPTR(font, bp);
    fontstyle = TTF_GetFontStyle(font);
    
    start = bp = sdl_get_temp_buff(sd, 2);
    put16be(bp, fontstyle);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_setFontStyle(sdl_data *sd, int len, char *buff)
{
    char *bp;
    TTF_Font *font;
    int fontstyle;

    bp = buff;
    POPGLPTR(font, bp);
    fontstyle = get16be(bp);
    
    TTF_SetFontStyle(font, fontstyle);
}

void es_ttf_fontHeight(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    int sendlen;
    TTF_Font *font;
    int fontheight;

    bp = buff;
    POPGLPTR(font, bp);
    fontheight = TTF_FontHeight(font);
    
    start = bp = sdl_get_temp_buff(sd, 2);
    put16be(bp, fontheight);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_fontAscent(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    int sendlen;
    TTF_Font *font;
    int fontascent;

    bp = buff;
    POPGLPTR(font, bp);
    fontascent = TTF_FontAscent(font);
    
    start = bp = sdl_get_temp_buff(sd, 2);
    put16be(bp, fontascent);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_fontDescent(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    int sendlen;
    TTF_Font *font;
    int fontdescent;

    bp = buff;
    POPGLPTR(font, bp);
    fontdescent = TTF_FontDescent(font);

    start = bp = sdl_get_temp_buff(sd, 2);
    put16be(bp, fontdescent);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_fontLineSkip(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    int sendlen;
    TTF_Font *font;
    int fontlineskip;

    bp = buff;
    POPGLPTR(font, bp);
    fontlineskip = TTF_FontLineSkip(font);
    
    start = bp = sdl_get_temp_buff(sd, 2);
    put16be(bp, fontlineskip);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_fontFaces(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    int sendlen;
    TTF_Font *font;
    long numfaces;

    bp = buff;
    POPGLPTR(font, bp);
    numfaces = TTF_FontFaces(font);
    
    start = bp = sdl_get_temp_buff(sd, 4);
    put32be(bp, numfaces);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_fontFaceIsFixedWidth(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    int sendlen;
    TTF_Font *font;
    int isfixed;

    bp = buff;
    POPGLPTR(font, bp);
    isfixed = TTF_FontFaceIsFixedWidth(font);
    
    start = bp = sdl_get_temp_buff(sd, 2);
    put16be(bp, isfixed);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_fontFaceFamilyName(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    int sendlen = 0;
    TTF_Font *font;
    char *familyname;
  
    bp = buff;
    POPGLPTR(font, bp);
    familyname = TTF_FontFaceFamilyName(font);
    if (familyname) {
	sendlen = strlen(familyname);
	bp = start = sdl_getbuff(sd, sendlen);
	while(*familyname != '\0') {
	    put8(bp, *familyname++);
	}
    }
    sdl_send(sd, sendlen);
}

void es_ttf_fontFaceStyleName(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    int sendlen = 0;
    TTF_Font *font;
    char *stylename;
  
    bp = buff;
    POPGLPTR(font, bp);
    stylename = TTF_FontFaceStyleName(font);
    if (stylename) {
	sendlen = strlen(stylename);
	bp = start = sdl_getbuff(sd, sendlen);
	while(*stylename != '\0') {
	    put8(bp, *stylename++);
	}
    }
    sdl_send(sd, sendlen);
}

void es_ttf_glyphMetrics(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    int sendlen;
    TTF_Font *font;
    Uint16 ch;
    int minx, maxx, miny, maxy, advance;
    int res;

    bp = buff;
    POPGLPTR(font, bp);
    ch = get16be(bp);
    res = TTF_GlyphMetrics(font, ch, &minx, &maxx, &miny, &maxy, &advance);
    bp = start = sdl_get_temp_buff(sd, 12);
    put16be(bp, res);
    put16be(bp, minx);
    put16be(bp, maxx);
    put16be(bp, miny);
    put16be(bp, maxy);
    put16be(bp, advance);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_sizeText(sdl_data *sd, int len, char *buff)
{
    char *text, *bp, *start;
    int sendlen;
    TTF_Font *font;
    int res, w, h;
    bp = buff;
    
    POPGLPTR(font, bp);
    text = bp;
    res = TTF_SizeText(font, text, &w, &h);
    bp = start = sdl_get_temp_buff(sd, 6);
    put16be(bp, res);
    put16be(bp, w);
    put16be(bp, h);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_sizeUTF8(sdl_data *sd, int len, char *buff)
{
    char *text, *bp, *start;
    int sendlen;
    TTF_Font *font;
    int res, w, h;
    bp = buff;
    
    POPGLPTR(font, bp);
    text = bp;
    res = TTF_SizeUTF8(font, text, &w, &h);
    bp = start = sdl_get_temp_buff(sd, 6);
    put16be(bp, res);
    put16be(bp, w);
    put16be(bp, h);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_sizeUNICODE(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    Uint16 *text;
    int sendlen;
    TTF_Font *font;
    int res, w, h;
    bp = buff;
    
    POPGLPTR(font, bp);
    text = (Uint16 *) bp;
    res = TTF_SizeUNICODE(font, text, &w, &h);
    bp = start = sdl_get_temp_buff(sd, 6);
    put16be(bp, res);
    put16be(bp, w);
    put16be(bp, h);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderTextSolid(sdl_data *sd, int len, char *buff) 
{
    char *text, *bp, *start;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor;
    bp = buff;
    
    // read input
    //     note: The order in which the arguments arrive and are passed to
    //           the TTF function differ.  We're avoiding a strlen (and the
    //           like) by having the text as the last parameter.
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    text = bp;

    // render text
    surface = TTF_RenderText_Solid(font, text, fgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderUTF8Solid(sdl_data *sd, int len, char *buff)
{
    char *text, *bp, *start;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor;
    bp = buff;
    
    // read input
    //     note: note: see note on parameter ordering in es_ttf_renderTextSolid
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    text = bp;

    // render text
    surface = TTF_RenderUTF8_Solid(font, text, fgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderUNICODESolid(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    Uint16 *text;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor;
    bp = buff;
    
    // read input
    //     note: note: see note on parameter ordering in es_ttf_renderTextSolid
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    text = (Uint16 *) bp;

    // render text
    surface = TTF_RenderUNICODE_Solid(font, text, fgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderGlyphSolid(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    Uint16 ch;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor;
    bp = buff;

    // read input
    //     note: note: see note on parameter ordering in es_ttf_renderTextSolid
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    ch = get16be(bp);

    // render glyph
    surface = TTF_RenderGlyph_Solid(font, ch, fgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderTextShaded(sdl_data *sd, int len, char *buff) 
{
    char *text, *bp, *start;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor, bgcolor;
    bp = buff;
    
    // read input
    //     note: note: see note on parameter ordering in es_ttf_renderTextSolid
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    bgcolor.r = get8(bp);
    bgcolor.g = get8(bp);
    bgcolor.b = get8(bp);
    text = bp;

    // render text
    surface = TTF_RenderText_Shaded(font, text, fgcolor, bgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderUTF8Shaded(sdl_data *sd, int len, char *buff)
{
    char *text, *bp, *start;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor, bgcolor;
    bp = buff;
    
    // read input
    //     note: note: see note on parameter ordering in es_ttf_renderTextSolid
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    bgcolor.r = get8(bp);
    bgcolor.g = get8(bp);
    bgcolor.b = get8(bp);
    text = bp;

    // render text
    surface = TTF_RenderUTF8_Shaded(font, text, fgcolor, bgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderUNICODEShaded(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    Uint16 *text;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor, bgcolor;
    bp = buff;
    
    // read input
    //     note: note: see note on parameter ordering in es_ttf_renderTextSolid
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    bgcolor.r = get8(bp);
    bgcolor.g = get8(bp);
    bgcolor.b = get8(bp);
    text = (Uint16 *) bp;

    // render text
    surface = TTF_RenderUNICODE_Shaded(font, text, fgcolor, bgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderGlyphShaded(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    Uint16 ch;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor, bgcolor;
    bp = buff;

    // read input
    //     note: note: see note on parameter ordering in es_ttf_renderTextSolid
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    bgcolor.r = get8(bp);
    bgcolor.g = get8(bp);
    bgcolor.b = get8(bp);
    ch = get16be(bp);

    // render glyph
    surface = TTF_RenderGlyph_Shaded(font, ch, fgcolor, bgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderTextBlended(sdl_data *sd, int len, char *buff) 
{
    char *text, *bp, *start;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor;
    bp = buff;
    
    // read input
    //     note: note: see note on parameter ordering in es_ttf_renderTextSolid
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    text = bp;

    // render text
    surface = TTF_RenderText_Blended(font, text, fgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderUTF8Blended(sdl_data *sd, int len, char *buff)
{
    char *text, *bp, *start;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor;
    bp = buff;
    
    // read input
    //     note: note: see note on parameter ordering in es_ttf_renderTextSolid
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    text = bp;

    // render text
    surface = TTF_RenderText_Blended(font, text, fgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderUNICODEBlended(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    Uint16 *text;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor;
    bp = buff;
    
    // read input
    //     note: note: see note on parameter ordering in es_ttf_renderTextSolid
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    text = (Uint16 *) bp;

    // render text
    surface = TTF_RenderUNICODE_Blended(font, text, fgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_renderGlyphBlended(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    Uint16 ch;
    int sendlen;
    TTF_Font *font;
    SDL_Surface *surface;
    SDL_Color fgcolor;
    bp = buff;

    // read input
    //     note: note: see note on parameter ordering in es_ttf_renderTextSolid
    POPGLPTR(font, bp);
    fgcolor.r = get8(bp);
    fgcolor.g = get8(bp);
    fgcolor.b = get8(bp);
    ch = get16be(bp);

    // render glyph
    surface = TTF_RenderGlyph_Blended(font, ch, fgcolor);
    
    // return a surface pointer
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_closeFont(sdl_data *sd, int len, char *buff)
{
    char *bp;
    TTF_Font *font;

    bp = buff;
    POPGLPTR(font, bp);
    TTF_CloseFont(font);
}

void es_ttf_quit(sdl_data *sd, int len, char *buff)
{
    TTF_Quit();
}

void es_ttf_wasInit(sdl_data *sd, int len, char *buff)
{
    char *bp, *start;
    int res;
    int sendlen;

    res = TTF_WasInit();
    bp = start = sdl_get_temp_buff(sd, 2);
    put16be(bp, res);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_ttf_setError(sdl_data *sd, int len, char *buff)
{
    // not implemented
}

void es_ttf_getError(sdl_data *sd, int len, char *buff)
{
    char *err, *bp, *start;  
    int length;
    err = TTF_GetError();
    length = strlen(err);
    bp = start = sdl_getbuff(sd, length);
    while(*err != '\0') {
	put8(bp, *err++);
    }
    sdl_send(sd, bp - start);
}
