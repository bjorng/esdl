/*
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id$
 */
/* 
 * Some useful extra functions   
 */

#ifdef _WIN32
#include <windows.h>
#endif

#include <string.h>
#include <stdlib.h>  /* malloc */
#ifdef FLAVOUR_WOGGLE
#include <woggle_driver.h>
#else
#include "esdl.h"
#endif

void mygl_alloc(sdl_data *sd, int len, char *bp)
{
   char *start;
   unsigned size;

   size = * (unsigned *) bp;
   bp = start = sdl_getbuff(sd, size);
   sdl_send(sd, size);
}

void mygl_write(sdl_data *sd, int len, char *bp)
{
  if (sd->next_bin == 1) {
    memcpy(sd->bin[0].base, bp, len);
  } else if (sd->next_bin == 2) {
    memcpy(sd->bin[0].base, sd->bin[1].base, sd->bin[1].size);
  }
  sdl_free_binaries(sd);
}
#ifndef FLAVOUR_WOGGLE
void copySdlImage2GLArray(sdl_data *sd, int len, char * buff)
{
  Uint8 *rowhi, *rowlo, type;
  SDL_Surface *image;
  unsigned char * mem;
  char *bp, *start;
  int i, j = 0, k;
  Uint8  rs,bs,gs,as;

  bp = buff;
  POPGLPTR(image, bp);
  type = *bp;
  if (sd->next_bin == 1) {
    mem = (unsigned char *) sd->bin[0].base;
 
#if SDL_BYTEORDER == SDL_BIG_ENDIAN
    rs = (2 - image->format->Rshift/8);
    gs = (2 - image->format->Gshift/8);
    bs = (2 - image->format->Bshift/8);
    as = (2 - image->format->Ashift/8);     
#else
    rs = image->format->Rshift/8;
    gs = image->format->Gshift/8;
    bs = image->format->Bshift/8;
    as = image->format->Ashift/8;     
#endif   
    /* GL surfaces are upsidedown (according to SDL examples)?? */
    k = 0;
    rowhi = (Uint8 *)image->pixels;
    rowlo = rowhi + (image->h * image->pitch) - image->pitch;
   
    for(i=0; i<image->h; ++i ) {
      for(j=0; j<image->w; ++j ) {
	switch(image->format->BytesPerPixel)
	  {
	  case 1:
	    mem[k++] = image->format->palette->colors[rowlo[j]].r;
	    mem[k++] = image->format->palette->colors[rowlo[j]].g;
	    mem[k++] = image->format->palette->colors[rowlo[j]].b;
	    if(type == 4)
	      mem[k++] = 0;	       
	    break;   
	  case 3:
	    mem[k++] = rowlo[j*3 + rs];
	    mem[k++] = rowlo[j*3 + gs];
	    mem[k++] = rowlo[j*3 + bs];
	    if(type == 4) 
	      mem[k++] = 0; 
	    break;
	  case 4:
	    mem[k++] = rowlo[j*4 + rs];
	    mem[k++] = rowlo[j*4 + gs];
	    mem[k++] = rowlo[j*4 + bs];
	    if(type == 4)
	      mem[k++] = rowlo[j*4 + as];
	    break;
	  }
      }
      rowlo -= image->pitch;
    }
    /* fprintf(stderr, "i %d, j %d k%d\n\r", i, j, k); */
    start = sdl_get_temp_buff(sd, 2);
    start[0] = 1;
    sdl_send(sd, 1);
    sdl_free_binaries(sd);
  }
}
#endif
