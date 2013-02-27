/*
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * 
 *     $Id$
 */
/* The special functions */

#include "esdl.h"

void es_getSurface(sdl_data *sd, int len, char * buff)
{
    char *bp, *start;
    int sendlen;
    SDL_Surface *screen;
    
    bp = buff;
    POPGLPTR(screen, bp);
    if(screen == NULL) {
	error();
    }
    bp = start = sdl_getbuff(sd, 4*4+2+8*2);
    put32be(bp, screen->flags);
    PUSHGLPTR(screen->format, bp);
    put32be(bp, screen->w);
    put32be(bp, screen->h);
    put16be(bp, screen->pitch);
    PUSHGLPTR(screen->pixels, bp);
    put32be(bp, screen->offset);
/*     put32be(bp, screen->clip_minx); */
/*     put32be(bp, screen->clip_maxx); */
/*     put32be(bp, screen->clip_miny); */
/*     put32be(bp, screen->clip_maxy); */
    
    sendlen = (int) (bp - start);
    sdl_send(sd, sendlen);
}

void es_getPixelFormat(sdl_data *sd, int len, char * buff) 
{
    char *bp, *start;
    int sendlen;
    SDL_Surface * sptr;
    SDL_PixelFormat *format;
    
    bp = buff;

    POPGLPTR(sptr, bp);
    if(sptr == NULL) 
       error();
    format = sptr->format;
    if(format == NULL) 
       error();
    
    bp = start = sdl_get_temp_buff(sd, 8+5*4+11);
    PUSHGLPTR(format->palette, bp);
    put8(bp, format->BitsPerPixel);
    put8(bp, format->BytesPerPixel);
    put8(bp, format->Rloss);
    put8(bp, format->Gloss);
    put8(bp, format->Bloss);
    put8(bp, format->Aloss);
    put8(bp, format->Rshift);
    put8(bp, format->Gshift);
    put8(bp, format->Bshift);
    put8(bp, format->Ashift);
    put32be(bp, format->Rmask);
    put32be(bp, format->Gmask);
    put32be(bp, format->Bmask);
    put32be(bp, format->Amask);
    put32be(bp, format->colorkey);
    put8(bp, format->alpha);
    
    sendlen = (int) (bp - start);
    sdl_send(sd, sendlen);
}

void es_getPalette(sdl_data *sd, int len, char * buff)
{
    char *bp, *start;
    int sendlen;
    SDL_Palette *palette;
    SDL_Surface *sptr;
    int i;

    bp = buff;
    POPGLPTR(sptr, bp);
    palette = sptr->format->palette;    
    
    if(palette == NULL) {
       bp = start = sdl_getbuff(sd, 2);
       put16be(bp, 0);
       sendlen = (int) (bp - start);
       sdl_send(sd, sendlen);
       return;
    }
    
    bp = start = sdl_getbuff(sd, 2 + palette->ncolors * 3);
    put16be(bp, palette->ncolors);
    
    for(i = 0; i < palette->ncolors; i++)
    {
	put8(bp, palette->colors[i].r);	
	put8(bp, palette->colors[i].g);	
	put8(bp, palette->colors[i].b);	
    }

    sendlen = (int) (bp - start);
    sdl_send(sd, sendlen);
}

void es_getPixels(sdl_data *sd, int len, char * buff)
{
   char *bp, *start;
   SDL_Surface *sptr;
   int sendlen, x, y, w, h, xi, yi;
   Uint8 *row;

   bp = buff;
   POPGLPTR(sptr, bp);
   
   if(sptr == NULL) 
      error();
   x = get16be(bp);
   y = get16be(bp);
   w = get16be(bp);
   h = get16be(bp);
  
   if(sptr->pixels == NULL)
      error();
   /* see /usr/local/src/SDL-1.1.3/src/video/SDL_surface.c FillRect */

   start = bp  = sdl_getbuff(sd, w*h*sptr->format->BytesPerPixel);
   row = (Uint8 *) sptr->pixels + y * sptr->pitch +
      x * sptr->format->BytesPerPixel;
   
   switch(sptr->format->BytesPerPixel) {
   case 1: {
      Uint8 *pixels;
      for(yi = h; yi; --yi){
	 pixels = (Uint8 *) row;
	 for(xi = w; xi; --xi){
	    put8(bp, *pixels);
	    pixels += 1;
	 }
	 row += sptr->pitch;
      }	   
      break;
   }
   case 2:
      {
	 Uint16 *pixels;
	 for(yi = h; yi; --yi){
	    pixels = (Uint16 *) row;
	    for(xi = w; xi; --xi){
	       put16be(bp, *pixels);
	       pixels++;
	    }
	    row += sptr->pitch;
	 }    
      }
      break;
   case 3:
      {
	 Uint8 *pixels;
	 for(yi = h; yi; --yi){
	    pixels = (Uint8 *) row;
	    for(xi = w; xi; --xi){
	       put8(bp, *pixels);
	       put8(bp, *(pixels+1));
	       put8(bp, *(pixels+2));
	       pixels += 3;
	    }
	    row += sptr->pitch;
	 }    
      }
      break;
   case 4:
      {
	 Uint32 *pixels;
	 for(yi = h; yi; --yi){
	    pixels = (Uint32 *) row;
	    for(xi = w; xi; --xi){
	       put32be(bp, *pixels);
	       pixels++;
	    }
	    row += sptr->pitch;
	 }
      }
      break;
   }
   
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}
