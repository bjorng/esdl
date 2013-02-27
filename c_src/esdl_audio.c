/*  
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 * 
 *     $Id$
 */
/* 
   Map erl esdl_events calls to C sdl calls      
*/

#include "esdl.h"
#include <string.h>
#include <stdlib.h>


struct {
   Uint8   *sound;		/* Pointer to wave data */
   Uint32  soundlen;		/* Length of wave data */
   int     repeat;              /* Play sample 'repeat' times */
   int     soundpos;		/* Current play position */
    
   Uint8 silence;
} wave;

void play_audio(sdl_data *sd, int len, char *buff) 
{
   char *bp, *start;
   int sendlen;
   void * sbuff;
   bp = buff;
   
   SDL_LockAudio();   
   
   POPGLPTR(sbuff, bp);
   wave.sound    = sbuff;
   wave.soundlen = get32be(bp);
   wave.repeat   = get32be(bp);
   wave.soundpos = 0;
   
   SDL_UnlockAudio();
   bp = start = sdl_getbuff(sd, 0);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void myaudiomixer(void *mydata, Uint8 *stream, int len)
{   
   Uint8 *waveptr;
   int    waveleft;
   
   if(wave.sound != NULL && wave.repeat != 0) 
   {
      /* Set up the pointers */
      waveptr  = wave.sound + wave.soundpos;
      waveleft = wave.soundlen - wave.soundpos;
      /* fprintf(stderr, "ED1 0x%X %d 0x%X %d repeat %d\n\r", 
	 stream, len, waveptr, waveleft, wave.repeat); */
	 /* Go! */
      while( waveleft < len ) {
	 memcpy(stream, waveptr, waveleft);
	 /*	 SDL_MixAudio(stream, waveptr, waveleft, SDL_MIX_MAXVOLUME); */
	 stream += waveleft;
	 len -= waveleft;
	 waveptr = wave.sound;
	 waveleft = wave.soundlen;
	 wave.soundpos = 0;
	 if((wave.repeat -= 1) == 0) {
	    memset(stream, wave.silence, len);
	    break;
	 }
      }
      if(wave.repeat != 0) {
	 /*	 fprintf(stderr, "ED2 0x%X %d 0x%X %d repeat %d\n\r", 
		 stream, len, waveptr, waveleft, wave.repeat); */
	 memcpy(stream, waveptr, len);
	 /*	 SDL_MixAudio(stream, waveptr, len, SDL_MIX_MAXVOLUME); */
	 wave.soundpos += len;    
      }
   }
   else { 
       memset(stream, wave.silence, len);
   }
}

/* API */
void es_audioDriverName(sdl_data *sd, int len, char *bp)
{
  int sendlen = 0;
    
  bp = sdl_get_temp_buff(sd, 256);
  if (SDL_AudioDriverName(bp, 256) != NULL) {
      sendlen = (int) strlen(bp);
  }
  sdl_send(sd, sendlen);
}

void es_openAudio(sdl_data *sd, int len, char *buff)
{
   int sendlen;
   char *bp, *start;
   int ff;
   SDL_AudioSpec desired, obtained, *obptr;
   bp = buff;
   ff = get8(bp);
   desired.freq     = get32be(bp);
   desired.format   = get16be(bp);
   desired.channels = get8(bp);
   desired.samples  = get16be(bp);
   desired.padding  = get16be(bp);
   desired.callback = myaudiomixer;

   /* Init the global data structures */
   wave.sound = NULL;
   wave.soundpos = 0;
   wave.soundlen = 0;

   if(ff == 1)  /* Force the requested format */
      obptr = NULL;
   else 
      obptr = &obtained;
   
   bp = start = sdl_getbuff(sd, 16);
   if( SDL_OpenAudio(&desired, obptr) < 0 ) {
      fprintf(stderr, "Couldn't open audio: %s\n", SDL_GetError());		
   } else {
      if(ff == 1) 
	  obptr = &desired;
       put32be(bp, obptr->freq);
       put16be(bp, obptr->format);
       put8(bp, obptr->channels);
       put8(bp, obptr->silence);
       put16be(bp, obptr->samples);
       put16be(bp, obptr->padding);
       put32be(bp, obptr->size);
       wave.silence = obptr->silence;
   } 
  
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_getAudioStatus(sdl_data *sd, int len, char *buff)
{
   int sendlen;
   char *bp, *start;
   SDL_audiostatus s;
   
   s = SDL_GetAudioStatus();
   start = bp = sdl_getbuff(sd, 1);
   put8(bp, s);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_pauseAudio(sdl_data *sd, int len, char *buff)
{
    char *bp;
    int pause;
    bp = buff;
    pause = get8(bp);
    SDL_PauseAudio(pause);
}

void es_loadWAV(sdl_data *sd, int len, char *bp)
{
    int sendlen;
    char *name, *start;
    SDL_AudioSpec obtained;
    Uint8 * ptr;
    Uint32 blen;

    name = bp;
    bp = start = sdl_get_temp_buff(sd, 28);
    if(NULL != SDL_LoadWAV(name, &obtained, &ptr, &blen)) {
       put32be(bp, obtained.freq);
       put16be(bp, obtained.format);
       put8(bp, obtained.channels);
       put8(bp, obtained.silence);
       put16be(bp, obtained.samples);
       put16be(bp, obtained.padding);
       put32be(bp, obtained.size);
       PUSHGLPTR(ptr, bp);
       put32be(bp, blen);
    }
    sendlen = (int) (bp - start);
    sdl_send(sd, sendlen);
}

void es_loadWAVRW(sdl_data *sd, int len, char *buff)
{
    error();
}

void es_freeWAV(sdl_data *sd, int len, char *buff)
{
   char *bp;
   void *ptr;
   bp = buff;
   POPGLPTR(ptr, bp);
   SDL_FreeWAV(ptr);
}

void es_buildAudioCVT(sdl_data *sd, int len, char *buff)
{
   error();
}

void es_convertAudio(sdl_data *sd, int len, char *buff)
{
   char *bp, *start;
   void *mptr;
   Uint16 oformat, nformat;
   Uint8  ochannels, nchannels;
   int    ofreq, nfreq, osize, nsize;
   SDL_AudioCVT  wav_cvt;
   int sendlen;

   bp = buff;
   oformat = get16be(bp);
   ochannels = get8(bp);
   ofreq = get32be(bp);
   nformat = get16be(bp);
   nchannels = get8(bp);
   nfreq = get32be(bp);
   POPGLPTR(mptr, bp);
   osize = get32be(bp);

   bp = start = sdl_getbuff(sd, 12);
   
   /* Build AudioCVT */
   if(SDL_BuildAudioCVT(&wav_cvt,oformat, ochannels, ofreq,
			nformat, nchannels, nfreq) >= 0) {      
      /* Setup for conversion */
      nsize = osize*wav_cvt.len_mult;      
      wav_cvt.buf=(Uint8 *)malloc(nsize);
      if(wav_cvt.buf != NULL) {
	 wav_cvt.len=osize;
	 memcpy(wav_cvt.buf, mptr, osize);
	 if (SDL_ConvertAudio(&wav_cvt) >= 0) {
	   PUSHGLPTR(wav_cvt.buf, bp);
	   put32be(bp, nsize);	 	
	 }
      }
   }   
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}
void es_mixAudio(sdl_data *sd, int len, char *buff)
{
   error();
}

void es_lockAudio(sdl_data *sd, int len, char *buff)
{
   error();
}

void es_unlockAudio(sdl_data *sd, int len, char *buff)
{
   error();
}

void es_closeAudio(sdl_data *sd, int len, char *buff)
{
    SDL_CloseAudio(); 
}
