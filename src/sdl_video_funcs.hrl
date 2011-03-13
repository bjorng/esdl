%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_video_funcs.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created : 30 Aug 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

%%% Functions  
-include("esdl.hrl").

-define(SDL_VideoDriverName, ?SDL_VIDEO_HRL + 1).
-define(SDL_GetVideoSurface, ?SDL_VideoDriverName + 1).
-define(SDL_GetVideoInfo,    ?SDL_GetVideoSurface + 1).
-define(SDL_VideoModeOK,     ?SDL_GetVideoInfo + 1).
-define(SDL_ListModes,       ?SDL_VideoModeOK + 1).
-define(SDL_SetVideoMode,    ?SDL_ListModes + 1).
-define(SDL_UpdateRect,      ?SDL_SetVideoMode + 1).
-define(SDL_UpdateRects,     ?SDL_UpdateRect + 1).
-define(SDL_Flip,            ?SDL_UpdateRects + 1).
-define(SDL_SetColors,       ?SDL_Flip + 1).
-define(SDL_MapRGB,          ?SDL_SetColors + 1).
-define(SDL_GetRGB,          ?SDL_MapRGB + 1).
-define(SDL_CreateRGBSurface,?SDL_GetRGB + 1).
-define(SDL_CreateRGBSurfaceFrom, ?SDL_CreateRGBSurface + 1).
-define(SDL_FreeSurface,     ?SDL_CreateRGBSurfaceFrom + 1).

-define(SDL_MUSTLOCK,        0).    %% Needed ? Implemented as an erlang func!!
-define(SDL_LockSurface,     ?SDL_FreeSurface + 1 ).
-define(SDL_UnlockSurface,   ?SDL_LockSurface  + 1).

-define(SDL_LoadBMP_RW,      ?SDL_UnlockSurface + 1).
-define(SDL_LoadBMP,         ?SDL_LoadBMP_RW + 1).

-define(SDL_SaveBMP_RW,      ?SDL_LoadBMP + 1).
-define(SDL_SaveBMP,         ?SDL_SaveBMP_RW + 1).

-define(SDL_SetColorKey,     ?SDL_SaveBMP + 1).
-define(SDL_SetAlpha,        ?SDL_SetColorKey + 1).
-define(SDL_SetClipping,     ?SDL_SetAlpha + 1).

-define(SDL_ConvertSurface,  ?SDL_SetClipping + 1 ). % SDL_internal
-define(SDL_BlitSurface,     ?SDL_ConvertSurface + 1).
-define(SDL_UpperBlit,       ?SDL_BlitSurface + 1).
-define(SDL_LowerBlit,       ?SDL_UpperBlit + 1 ).% SDL_semi private
-define(SDL_FillRect,        ?SDL_LowerBlit + 1).

-define(SDL_DisplayFormat,   ?SDL_FillRect + 1).

%% video overlays ... not supported rigth now

%% Windowing stuff
-define(SDL_WM_SetCaption,   ?SDL_DisplayFormat +1).
-define(SDL_WM_GetCaption,   ?SDL_WM_SetCaption +1).
-define(SDL_WM_SetIcon,      ?SDL_WM_GetCaption +1).
-define(SDL_WM_IconifyWindow,?SDL_WM_SetIcon +1).
-define(SDL_WM_ToggleFullScreen, ?SDL_WM_IconifyWindow +1).

-define(SDL_WM_GrabInput, ?SDL_WM_ToggleFullScreen+1).
-define(SDL_WM_GetInfo,  ?SDL_WM_GrabInput+1).

-define(SDL_GL_SetAttribute, ?SDL_WM_GetInfo+1).
-define(SDL_GL_GetAttribute, ?SDL_GL_SetAttribute +1).
-define(SDL_GL_SwapBuffers, ?SDL_GL_GetAttribute +1).

%% Erl sdl special functions 
-define(ESDL_getSurface,         ?SDL_GL_SwapBuffers + 1).
-define(ESDL_getPalette,         ?ESDL_getSurface + 1).
-define(ESDL_getPixelFormat,     ?ESDL_getPalette +1).
-define(ESDL_getPixels,          ?ESDL_getPixelFormat +1).
-define(SDL_WM_IsMaximized,      ?ESDL_getPixels +1).

%% Sdl additions 
-define(SDL_SetGamma, ?SDL_WM_IsMaximized +1).
-define(SDL_SetGammaRamp, ?SDL_SetGamma +1).
-define(SDL_GetGammaRamp, ?SDL_SetGammaRamp +1).

-define(SDL_MapRGBA,              ?SDL_GetGammaRamp + 1).
-define(SDL_GetRGBA,              ?SDL_MapRGBA + 1).
-define(SDL_GetClipRect,          ?SDL_GetRGBA + 1).
-define(SDL_SetClipRect,          ?SDL_GetClipRect + 1).
-define(SDL_DisplayFormatAlpha,   ?SDL_SetClipRect + 1).
-define(SDL_WM_Maximize,          ?SDL_DisplayFormatAlpha +1).
