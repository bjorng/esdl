/*  
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 * 
 * 
 *     $Id$
 */
/* Defines the audio functions */ 
/* Used for switching          */

#ifdef __cplusplus
    extern "C" {
#endif 

#define SDL_AudioDriverNameFunc AUDIO_H +1
void es_audioDriverName(sdl_data *, int, char *);
#define SDL_OpenAudioFunc       SDL_AudioDriverNameFunc +1
void es_openAudio(sdl_data *, int, char *);
#define SDL_GetAudioStatusFunc  SDL_OpenAudioFunc +1
void es_getAudioStatus(sdl_data *, int, char *);
#define SDL_PauseAudioFunc      SDL_GetAudioStatusFunc +1
void es_pauseAudio(sdl_data *, int, char *);
#define SDL_LoadWAVFunc         SDL_PauseAudioFunc +1
void es_loadWAV(sdl_data *, int, char *);
#define SDL_LoadWAV_RWFunc      SDL_LoadWAVFunc +1
void es_loadWAV(sdl_data *, int, char *);
#define SDL_FreeWAVFunc         SDL_LoadWAV_RWFunc +1
void es_freeWAV(sdl_data *, int, char *);
#define SDL_BuildAudioCVTFunc   SDL_FreeWAVFunc +1
void es_buildAudioCVT(sdl_data *, int, char *);
#define SDL_ConvertAudioFunc    SDL_BuildAudioCVTFunc +1
void es_convertAudio(sdl_data *, int, char *);
#define SDL_MixAudioFunc        SDL_ConvertAudioFunc +1
void es_mixAudio(sdl_data *, int, char *);
#define SDL_LockAudioFunc       SDL_MixAudioFunc +1
void es_lockAudio(sdl_data *, int, char *);
#define SDL_UnlockAudioFunc     SDL_LockAudioFunc +1
void es_unlockAudio(sdl_data *, int, char *);
#define SDL_CloseAudioFunc      SDL_UnlockAudioFunc +1
void es_closeAudio(sdl_data *, int, char *);
#define play_audioFunc          SDL_CloseAudioFunc +1
void play_audio(sdl_data *, int, char *);


#ifdef __cplusplus
    }
#endif 
