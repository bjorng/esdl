/*  
 *  Copyright (c) 2007 Klas Johanssson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 * 
 *     $Id$
 */
/* 
   Map erl esdl_img calls to C SDL_image calls      
*/

#include "esdl.h"
#include <string.h>
#include <stdlib.h>
#include <SDL_image.h>

void es_img_linkedVersion(sdl_data *sd, int len, char *buff)
{
   char *bp, *start;
   int sendlen;
   const SDL_version *version;

   version = IMG_Linked_Version();

   bp = start = sdl_get_temp_buff(sd, 3);
   put8(bp, version->major);
   put8(bp, version->minor);
   put8(bp, version->patch);
   sendlen = bp - start;
   sdl_send(sd, sendlen);
}

void es_img_loadTypedRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_load(sdl_data *sd, int len, char *buff)
{
    char *file, *bp, *start;
    int sendlen;
    SDL_Surface *surface;

    file = buff;
    surface = IMG_Load(file);
    bp = start = sdl_get_temp_buff(sd, 8);
    PUSHGLPTR(surface, bp);
    sendlen = bp - start;
    sdl_send(sd, sendlen);
}

void es_img_loadRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_invertAlpha(sdl_data *sd, int len, char *buff)
{
}

void es_img_isBMP(sdl_data *sd, int len, char *buff)
{
}

void es_img_isPNM(sdl_data *sd, int len, char *buff)
{
}

void es_img_isXPM(sdl_data *sd, int len, char *buff)
{
}

void es_img_isXCF(sdl_data *sd, int len, char *buff)
{
}

void es_img_isPCX(sdl_data *sd, int len, char *buff)
{
}

void es_img_isGIF(sdl_data *sd, int len, char *buff)
{
}

void es_img_isJPG(sdl_data *sd, int len, char *buff)
{
}

void es_img_isTIF(sdl_data *sd, int len, char *buff)
{
}

void es_img_isPNG(sdl_data *sd, int len, char *buff)
{
}

void es_img_isLBM(sdl_data *sd, int len, char *buff)
{
}

void es_img_loadBMPRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_loadPNMRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_loadXPMRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_loadXCFRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_loadPCXRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_loadGIFRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_loadJPGRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_loadTIFRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_loadPNGRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_loadTGARW(sdl_data *sd, int len, char *buff)
{
}

void es_img_loadLBMRW(sdl_data *sd, int len, char *buff)
{
}

void es_img_readXPMFromArray(sdl_data *sd, int len, char *buff)
{
}

void es_img_setError(sdl_data *sd, int len, char *buff)
{
    // not implemented
}

void es_img_getError(sdl_data *sd, int len, char *buff)
{
    char *err, *bp, *start;  
    int length;
    err = IMG_GetError();
    length = strlen(err);
    bp = start = sdl_getbuff(sd, length);
    while(*err != '\0') {
	put8(bp, *err++);
    }
    sdl_send(sd, bp - start);
}
