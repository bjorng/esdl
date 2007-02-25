%%  Copyright (c) 2007 Klas Johansson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_img_funcs.hrl
%%% Author  : Klas Johansson <klajo at users.sourceforge.net>
%%% Purpose : 
%%% Created : 18 Feb 2007 by Klas Johansson <klajo at users.sourceforge.net>
%%%----------------------------------------------------------------------

%%% Functions  
-include("esdl.hrl").

-define(SDL_IMG_LinkedVersion,         ?SDL_IMG_HRL + 1).
-define(SDL_IMG_LoadTypedRW,           ?SDL_IMG_LinkedVersion + 1).
-define(SDL_IMG_Load,                  ?SDL_IMG_LoadTypedRW + 1).
-define(SDL_IMG_LoadRW,                ?SDL_IMG_Load + 1).
-define(SDL_IMG_InvertAlpha,           ?SDL_IMG_LoadRW + 1).
-define(SDL_IMG_isBMP,                 ?SDL_IMG_InvertAlpha + 1).
-define(SDL_IMG_isPNM,                 ?SDL_IMG_isBMP + 1).
-define(SDL_IMG_isXPM,                 ?SDL_IMG_isPNM + 1).
-define(SDL_IMG_isXCF,                 ?SDL_IMG_isXPM + 1).
-define(SDL_IMG_isPCX,                 ?SDL_IMG_isXCF + 1).
-define(SDL_IMG_isGIF,                 ?SDL_IMG_isPCX + 1).
-define(SDL_IMG_isJPG,                 ?SDL_IMG_isGIF + 1).
-define(SDL_IMG_isTIF,                 ?SDL_IMG_isJPG + 1).
-define(SDL_IMG_isPNG,                 ?SDL_IMG_isTIF + 1).
-define(SDL_IMG_isLBM,                 ?SDL_IMG_isPNG + 1).
-define(SDL_IMG_LoadBMPRW,             ?SDL_IMG_isLBM + 1).
-define(SDL_IMG_LoadPNMRW,             ?SDL_IMG_LoadBMPRW + 1).
-define(SDL_IMG_LoadXPMRW,             ?SDL_IMG_LoadPNMRW + 1).
-define(SDL_IMG_LoadXCFRW,             ?SDL_IMG_LoadXPMRW + 1).
-define(SDL_IMG_LoadPCXRW,             ?SDL_IMG_LoadXCFRW + 1).
-define(SDL_IMG_LoadGIFRW,             ?SDL_IMG_LoadPCXRW + 1).
-define(SDL_IMG_LoadJPGRW,             ?SDL_IMG_LoadGIFRW + 1).
-define(SDL_IMG_LoadTIFRW,             ?SDL_IMG_LoadJPGRW + 1).
-define(SDL_IMG_LoadPNGRW,             ?SDL_IMG_LoadTIFRW + 1).
-define(SDL_IMG_LoadTGARW,             ?SDL_IMG_LoadPNGRW + 1).
-define(SDL_IMG_LoadLBMRW,             ?SDL_IMG_LoadTGARW + 1).
-define(SDL_IMG_ReadXPMFromArray,      ?SDL_IMG_LoadLBMRW + 1).
-define(SDL_IMG_SetError,              ?SDL_IMG_ReadXPMFromArray + 1).
-define(SDL_IMG_GetError,              ?SDL_IMG_SetError + 1).
