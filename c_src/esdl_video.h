/*
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 * 
 *     $Id$
 */
/* The video functions */

#ifdef __cplusplus
    extern "C" {
#endif 

#define SDL_VideoDriverNameFunc (VIDEO_H + 1)
void es_videoDriverName(sdl_data *, int, char *);
#define SDL_GetVideoSurfaceFunc (SDL_VideoDriverNameFunc + 1)
void es_getVideoSurface(sdl_data *, int len, char *buff);
#define SDL_GetVideoInfoFunc    (SDL_GetVideoSurfaceFunc + 1)
void es_getVideoInfo(sdl_data *, int len, char*buff);
#define SDL_VideoModeOKFunc     (SDL_GetVideoInfoFunc + 1)
void es_videoModeOK(sdl_data *, int len, char*buff);
void es_videoModeOK2(ErlDrvPort, ErlDrvTermData, char *);
#define SDL_ListModesFunc       (SDL_VideoModeOKFunc + 1)
void es_listModes(sdl_data *, int len, char *buff);
#define SDL_SetVideoModeFunc    (SDL_ListModesFunc + 1)
void es_setVideoMode(sdl_data *, int len, char * buff);
void es_setVideoMode2(ErlDrvPort, ErlDrvTermData, char * buff);
#define SDL_UpdateRectFunc      (SDL_SetVideoModeFunc + 1)
/* Done from erlang using UpdateRectsFunc */
#define SDL_UpdateRectsFunc     (SDL_UpdateRectFunc + 1)
void es_updateRects(sdl_data *, int len, char * buff);
#define SDL_FlipFunc            (SDL_UpdateRectsFunc + 1)
void es_flip(sdl_data *, int len, char *buff);
#define SDL_SetColorsFunc       (SDL_FlipFunc + 1)
void es_setColors(sdl_data *, int, char *);
#define SDL_MapRGBFunc          (SDL_SetColorsFunc + 1)
void es_mapRGB(sdl_data *, int len, char *buff);
#define SDL_GetRGBFunc          (SDL_MapRGBFunc + 1)
void es_getRGB(sdl_data *, int len, char *buff);
#define SDL_CreateRGBSurfaceFunc (SDL_GetRGBFunc + 1)
void es_createRGBSurface(sdl_data *, int, char *);
#define SDL_CreateRGBSurfaceFromFunc (SDL_CreateRGBSurfaceFunc + 1)
void es_createRGBSurfaceFrom(sdl_data *, int, char *);
#define SDL_FreeSurfaceFunc     (SDL_CreateRGBSurfaceFromFunc + 1)
void es_freeSurface(sdl_data *, int len, char * buff);

#define SDL_MUSTLOCKFunc       (0)   /*  Needed ? */
#define SDL_LockSurfaceFunc    (SDL_FreeSurfaceFunc + 1)
void es_lockSurface(sdl_data *, int len, char * buff);
#define SDL_UnlockSurfaceFunc  (SDL_LockSurfaceFunc + 1)
void es_unlockSurface(sdl_data *, int len, char * buff);

#define SDL_LoadBMP_RWFunc      (SDL_UnlockSurfaceFunc + 1)
#define SDL_LoadBMPFunc         (SDL_LoadBMP_RWFunc + 1)
void es_loadBMP(sdl_data *, int len, char * buff);

#define SDL_SaveBMP_RWFunc      (SDL_LoadBMPFunc + 1)
#define SDL_SaveBMPFunc         (SDL_SaveBMP_RWFunc + 1)
void es_saveBMP(sdl_data *, int len, char * buff);

#define SDL_SetColorKeyFunc     (SDL_SaveBMPFunc + 1)
void es_setColorKey(sdl_data *, int len, char * buff);

#define SDL_SetAlphaFunc           (SDL_SetColorKeyFunc + 1)
void es_setAlpha(sdl_data *, int len, char *buff);
#define SDL_SetClippingFunc     (SDL_SetAlphaFunc + 1)
       /* void es_setClipping(sdl_data *, int len, char *buff); removed */

#define SDL_ConvertSurfaceFunc  (SDL_SetClippingFunc + 1 ) /* SDL_internal */
#define SDL_BlitSurfaceFunc     (SDL_ConvertSurfaceFunc + 1)
void es_blitSurface(sdl_data *, int len, char * buff);
#define SDL_UpperBlitFunc       (SDL_BlitSurfaceFunc + 1)
/* blitSurface is just a macro that does upperblit */
#define SDL_LowerBlitFunc       (SDL_UpperBlitFunc + 1 )   /* SDL_semi private */
#define SDL_FillRectFunc        (SDL_LowerBlitFunc + 1)
void es_fillRect(sdl_data *, int len, char * buff);

#define SDL_DisplayFormatFunc   (SDL_FillRectFunc + 1)
void es_displayFormat(sdl_data *, int len, char * buff);

#define SDL_WM_SetCaptionFunc   (SDL_DisplayFormatFunc +1)
void es_wm_setCaption(sdl_data *, int len, char *buff);
void es_wm_setCaption2(char *buff);
#define SDL_WM_GetCaptionFunc   (SDL_WM_SetCaptionFunc +1)
void es_wm_getCaption(sdl_data *, int len, char *buff);
#define SDL_WM_SetIconFunc      (SDL_WM_GetCaptionFunc +1)
void es_wm_setIcon(sdl_data *, int len, char *buff);
#define SDL_WM_IconifyWindowFunc (SDL_WM_SetIconFunc +1)
void es_wm_iconifyWindow(sdl_data *, int len, char *buff);
#define SDL_WM_ToggleFullScreenFunc (SDL_WM_IconifyWindowFunc +1)
void es_wm_toggleFullScreen(sdl_data *, int len, char *buff);
void es_wm_toggleFullScreen2(ErlDrvPort, ErlDrvTermData, char *);
#define SDL_WM_GrabInputFunc    (SDL_WM_ToggleFullScreenFunc +1)
void es_wm_grabInput(sdl_data *, int len, char *buff);
#define SDL_WM_GetInfoFunc    (SDL_WM_GrabInputFunc +1)
void es_wm_getInfo(sdl_data *, int len, char *buff);
void es_wm_getInfo2(ErlDrvPort, ErlDrvTermData, char *);

#define SDL_GL_SetAttributeFunc   (SDL_WM_GetInfoFunc + 1)
void es_gl_setAttribute(sdl_data *, int, char *);
void es_gl_setAttribute2(ErlDrvPort, ErlDrvTermData, char *);
#define SDL_GL_GetAttributeFunc   (SDL_GL_SetAttributeFunc + 1)
void es_gl_getAttribute(sdl_data *, int, char *);
void es_gl_getAttribute2(ErlDrvPort, ErlDrvTermData, char *);
#define SDL_GL_SwapBuffersFunc    (SDL_GL_GetAttributeFunc + 1)
void es_gl_swapBuffers(sdl_data *, int, char *);

/* Erl sdl special functions */
#define ESDL_getSurfaceFunc         (SDL_GL_SwapBuffersFunc +1)
void es_getSurface(sdl_data *, int len, char * buff);
#define ESDL_getPaletteFunc         (ESDL_getSurfaceFunc + 1)
void es_getPalette(sdl_data *, int len, char * buff);
#define ESDL_getPixelFormatFunc     (ESDL_getPaletteFunc +1)
void es_getPixelFormat(sdl_data *, int len, char * buff);
#define ESDL_getPixelsFunc          (ESDL_getPixelFormatFunc +1)
void es_getPixels(sdl_data *, int len, char * buff);
#define SDL_WM_IsMaximizedFunc          (ESDL_getPixelsFunc +1)
void es_wm_isMaximized(sdl_data *, int len, char * buff);
/* SDL additions since SDL 1.1 */
#define SDL_SetGammaFunc         (SDL_WM_IsMaximizedFunc +1)
void es_setGamma(sdl_data *, int len, char * buff);
#define SDL_SetGammaRampFunc         (SDL_SetGammaFunc +1)
void es_setGammaRamp(sdl_data *, int len, char * buff);
#define SDL_GetGammaRampFunc         (SDL_SetGammaRampFunc +1)
void es_getGammaRamp(sdl_data *, int len, char * buff);

#define SDL_MapRGBAFunc          (SDL_GetGammaRampFunc + 1)
void es_mapRGBA(sdl_data *, int len, char *buff);
#define SDL_GetRGBAFunc          (SDL_MapRGBAFunc + 1)
void es_getRGBA(sdl_data *, int len, char *buff);
#define SDL_GetClipRectFunc          (SDL_GetRGBAFunc + 1)
void es_getClipRect(sdl_data *, int len, char *buff);
#define SDL_SetClipRectFunc          (SDL_GetClipRectFunc + 1)
void es_setClipRect(sdl_data *, int len, char *buff);
#define SDL_DisplayFormatAlphaFunc   (SDL_SetClipRectFunc + 1)
void es_displayFormatAlpha(sdl_data *, int len, char * buff);

#define SDL_WM_MaximizeFunc   (SDL_DisplayFormatAlphaFunc + 1)
void es_wm_maximize(sdl_data *, int len, char * buff);
void es_wm_maximize2(ErlDrvPort, ErlDrvTermData, char *);
#define SDL_WM_MacFileDialog   (SDL_WM_MaximizeFunc + 1)
void es_wm_mac_file_dialog(sdl_data *, int len, char * buff);
void es_wm_mac_file_dialog2(ErlDrvPort, ErlDrvTermData, char *);

#ifdef __cplusplus
    }
#endif 


