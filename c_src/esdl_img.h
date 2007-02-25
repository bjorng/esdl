/*  
 *  Copyright (c) 2007 Klas Johansson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 * 
 * 
 *     $Id$
 */
/* Defines the image functions */ 

#ifdef _USE_SDL_IMAGE

#ifdef __cplusplus
    extern "C" {
#endif 

#define SDL_IMG_LinkedVersionFunc          IMG_H +1
void es_img_linkedVersion(sdl_data *, int, char *);
#define SDL_IMG_LoadTypedRWFunc            SDL_IMG_LinkedVersionFunc +1
void es_img_loadTypedRW(sdl_data *, int, char *);
#define SDL_IMG_LoadFunc                   SDL_IMG_LoadTypedRWFunc +1
void es_img_load(sdl_data *, int, char *);
#define SDL_IMG_LoadRWFunc                 SDL_IMG_LoadFunc +1
void es_img_loadRW(sdl_data *, int, char *);
#define SDL_IMG_InvertAlphaFunc            SDL_IMG_LoadRWFunc +1
void es_img_invertAlpha(sdl_data *, int, char *);
#define SDL_IMG_isBMPFunc                  SDL_IMG_InvertAlphaFunc +1
void es_img_isBMP(sdl_data *, int, char *);
#define SDL_IMG_isPNMFunc                  SDL_IMG_isBMPFunc +1
void es_img_isPNM(sdl_data *, int, char *);
#define SDL_IMG_isXPMFunc                  SDL_IMG_isPNMFunc +1
void es_img_isXPM(sdl_data *, int, char *);
#define SDL_IMG_isXCFFunc                  SDL_IMG_isXPMFunc +1
void es_img_isXCF(sdl_data *, int, char *);
#define SDL_IMG_isPCXFunc                  SDL_IMG_isXCFFunc +1
void es_img_isPCX(sdl_data *, int, char *);
#define SDL_IMG_isGIFFunc                  SDL_IMG_isPCXFunc +1
void es_img_isGIF(sdl_data *, int, char *);
#define SDL_IMG_isJPGFunc                  SDL_IMG_isGIFFunc +1
void es_img_isJPG(sdl_data *, int, char *);
#define SDL_IMG_isTIFFunc                  SDL_IMG_isJPGFunc +1
void es_img_isTIF(sdl_data *, int, char *);
#define SDL_IMG_isPNGFunc                  SDL_IMG_isTIFFunc +1
void es_img_isPNG(sdl_data *, int, char *);
#define SDL_IMG_isLBMFunc                  SDL_IMG_isPNGFunc +1
void es_img_isLBM(sdl_data *, int, char *);
#define SDL_IMG_LoadBMPRWFunc              SDL_IMG_isLBMFunc +1
void es_img_loadBMPRW(sdl_data *, int, char *);
#define SDL_IMG_LoadPNMRWFunc              SDL_IMG_LoadBMPRWFunc +1
void es_img_loadPNMRW(sdl_data *, int, char *);
#define SDL_IMG_LoadXPMRWFunc              SDL_IMG_LoadPNMRWFunc +1
void es_img_loadXPMRW(sdl_data *, int, char *);
#define SDL_IMG_LoadXCFRWFunc              SDL_IMG_LoadXPMRWFunc +1
void es_img_loadXCFRW(sdl_data *, int, char *);
#define SDL_IMG_LoadPCXRWFunc              SDL_IMG_LoadXCFRWFunc +1
void es_img_loadPCXRW(sdl_data *, int, char *);
#define SDL_IMG_LoadGIFRWFunc              SDL_IMG_LoadPCXRWFunc +1
void es_img_loadGIFRW(sdl_data *, int, char *);
#define SDL_IMG_LoadJPGRWFunc              SDL_IMG_LoadGIFRWFunc +1
void es_img_loadJPGRW(sdl_data *, int, char *);
#define SDL_IMG_LoadTIFRWFunc              SDL_IMG_LoadJPGRWFunc +1
void es_img_loadTIFRW(sdl_data *, int, char *);
#define SDL_IMG_LoadPNGRWFunc              SDL_IMG_LoadTIFRWFunc +1
void es_img_loadPNGRW(sdl_data *, int, char *);
#define SDL_IMG_LoadTGARWFunc              SDL_IMG_LoadPNGRWFunc +1
void es_img_loadTGARW(sdl_data *, int, char *);
#define SDL_IMG_LoadLBMRWFunc              SDL_IMG_LoadTGARWFunc +1
void es_img_loadLBMRW(sdl_data *, int, char *);
#define SDL_IMG_ReadXPMFromArrayFunc       SDL_IMG_LoadLBMRWFunc +1
void es_img_readXPMFromArray(sdl_data *, int, char *);
#define SDL_IMG_SetErrorFunc               SDL_IMG_ReadXPMFromArrayFunc +1
void es_img_setError(sdl_data *, int, char *);
#define SDL_IMG_GetErrorFunc               SDL_IMG_SetErrorFunc +1
void es_img_getError(sdl_data *, int, char *);

#ifdef __cplusplus
    }
#endif 

#endif /* _USE_SDL_IMAGE */
