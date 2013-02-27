/*
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id$
 */

/* 
 * General SDL functions.
 */

#include "esdl.h"
#include <string.h>

void es_init(sdl_data *sd, int len, char *bp) 
{
   Uint32 mode;
   
   mode = * (Uint32 *) bp;
   if (SDL_Init(mode) < 0)  {
     char* e = SDL_GetError();
     fprintf(stderr, "Couldn't initialize SDL: %s\n\r", e);
   }
}

void es_quit(sdl_data *sd, int len, char * buff) 
{
    SDL_Quit();
}

void es_getError(sdl_data *sd, int len, char *buff)
{
   char * err, *bp, *start;  
   int length;
   err = SDL_GetError();
   length = (int) strlen(err);
   bp = start = sdl_getbuff(sd, length);
   while(*err != '\0') {
      put8(bp, *err++);
   }
   sdl_send(sd, (int) (bp - start));
}
