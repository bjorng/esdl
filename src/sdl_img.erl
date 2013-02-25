%%  Copyright (c) 2007 Klas Johansson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_img.erl
%%% Author  : Klas Johansson <klajo at users.sourceforge.net>
%%% Purpose : Provide access to SDL's image functions (tested with
%%%           SDL_image 1.2.4)
%%% Created : 18 Feb 2007 by Klas Johansson <klajo at users.sourceforge.net>
%%%----------------------------------------------------------------------

-module(sdl_img).

-include("sdl_img_funcs.hrl").
-include("sdl_util.hrl").
-include("sdl_video.hrl").

%%% ERL_SDL functions
-export([]).
%%% SDL_Functions
-export([linkedVersion/0]).

-export([loadTypedRW/0]).
-export([load/1]).
-export([loadRW/0]).

-export([invertAlpha/0]).

-export([isBMP/0]).
-export([isPNM/0]).
-export([isXPM/0]).
-export([isXCF/0]).
-export([isPCX/0]).
-export([isGIF/0]).
-export([isJPG/0]).
-export([isTIF/0]).
-export([isPNG/0]).
-export([isLBM/0]).

-export([loadBMPRW/0]).
-export([loadPNMRW/0]).
-export([loadXPMRW/0]).
-export([loadXCFRW/0]).
-export([loadPCXRW/0]).
-export([loadGIFRW/0]).
-export([loadJPGRW/0]).
-export([loadTIFRW/0]).
-export([loadPNGRW/0]).
-export([loadTGARW/0]).
-export([loadLBMRW/0]).

-export([readXPMFromArray/0]).

-export([setError/1]).
-export([getError/0]).

%% Imports
-import(sdl, [call/2, cast/2]).

%% Defines 
-define(INT,    16/signed).
-define(LONG,   32/signed).
-define(UINT8,   8/unsigned).
-define(UINT16, 16/unsigned).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ERL_SDL API

%% Func: linkedVersion
%% Args: -
%% Returns: {Major, Minor, Patch}
%% C-API func: const SDL_version * IMG_Linked_Version(void);
%% Desc: This function gets the version of the SDL_image library.
linkedVersion() ->
    <<Major:?UINT8, Minor:?UINT8, Patch:?UINT8>> = 
	call(?SDL_IMG_LinkedVersion, []),
    {Major, Minor, Patch}.

%% Func: loadTypedRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadTyped_RW(SDL_RWops *src, int freesrc, char *type);
%% Desc: Not implemented.
loadTypedRW() ->
    exit({not_implemented, loadTypedRW}).

%% Func: load
%% Args: File
%% Returns: Surface Ref
%% C-API func: SDL_Surface * IMG_Load(const char *file);
%% Desc: Load an image from an SDL data source.
load(File) ->
    <<SurfacePtr:?_PTR>> = call(?SDL_IMG_Load, [[File,0]]),
    case SurfacePtr of
	0 -> exit({load, returned_null_pointer});
	_ -> {surfacep, SurfacePtr}
    end.

%% Func: loadRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_Load_RW(SDL_RWops *src, int freesrc);
%% Desc: Not implemented.
loadRW() ->
    exit({not_implemented, loadRW}).

%% Func: invertAlpha
%% Args: N/A
%% Returns: N/A
%% C-API func: int IMG_InvertAlpha(int on);
%% Desc: Not implemented.
invertAlpha() ->
    exit({not_implemented, invertAlpha}).

%% Func: isBMP
%% Args: N/A
%% Returns: N/A
%% C-API func: int IMG_isBMP(SDL_RWops *src);
%% Desc: Not implemented.
isBMP() ->
    exit({not_implemented, isBMP}).

%% Func: isPNM
%% Args: N/A
%% Returns: N/A
%% C-API func: int IMG_isPNM(SDL_RWops *src);
%% Desc: Not implemented.
isPNM() ->
    exit({not_implemented, isPNM}).

%% Func: isXPM
%% Args: N/A
%% Returns: N/A
%% C-API func: int IMG_isXPM(SDL_RWops *src);
%% Desc: Not implemented.
isXPM() ->
    exit({not_implemented, isXPM}).

%% Func: isXCF
%% Args: N/A
%% Returns: N/A
%% C-API func: int IMG_isXCF(SDL_RWops *src);
%% Desc: Not implemented.
isXCF() ->
    exit({not_implemented, isXCF}).

%% Func: isPCX
%% Args: N/A
%% Returns: N/A
%% C-API func: int IMG_isPCX(SDL_RWops *src);
%% Desc: Not implemented.
isPCX() ->
    exit({not_implemented, isPCX}).

%% Func: isGIF
%% Args: N/A
%% Returns: N/A
%% C-API func: int IMG_isGIF(SDL_RWops *src);
%% Desc: Not implemented.
isGIF() ->
    exit({not_implemented, isGIF}).

%% Func: isJPG
%% Args: N/A
%% Returns: N/A
%% C-API func: int IMG_isJPG(SDL_RWops *src);
%% Desc: Not implemented.
isJPG() ->
    exit({not_implemented, isJPG}).

%% Func: isTIF
%% Args: N/A
%% Returns: N/A
%% C-API func: int IMG_isTIF(SDL_RWops *src);
%% Desc: Not implemented.
isTIF() ->
    exit({not_implemented, isTIF}).

%% Func: isPNG
%% Args: N/A
%% Returns: N/A
%% C-API func: int IMG_isPNG(SDL_RWops *src);
%% Desc: Not implemented.
isPNG() ->
    exit({not_implemented, isPNG}).

%% Func: isLBM
%% Args: N/A
%% Returns: N/A
%% C-API func: int IMG_isLBM(SDL_RWops *src);
%% Desc: Not implemented.
isLBM() ->
    exit({not_implemented, isLBM}).

%% Func: loadBMPRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadBMP_RW(SDL_RWops *src);
%% Desc: Not implemented.
loadBMPRW() ->
    exit({not_implemented, loadBMPRW}).

%% Func: loadPNMRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadPNM_RW(SDL_RWops *src);
%% Desc: Not implemented.
loadPNMRW() ->
    exit({not_implemented, loadPNMRW}).

%% Func: loadXPMRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadXPM_RW(SDL_RWops *src);
%% Desc: Not implemented.
loadXPMRW() ->
    exit({not_implemented, loadXPMRW}).

%% Func: loadXCFRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadXCF_RW(SDL_RWops *src);
%% Desc: Not implemented.
loadXCFRW() ->
    exit({not_implemented, loadXCFRW}).

%% Func: loadPCXRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadPCX_RW(SDL_RWops *src);
%% Desc: Not implemented.
loadPCXRW() ->
    exit({not_implemented, loadPCXRW}).

%% Func: loadGIFRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadGIF_RW(SDL_RWops *src);
%% Desc: Not implemented.
loadGIFRW() ->
    exit({not_implemented, loadGIFRW}).

%% Func: loadJPGRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadJPG_RW(SDL_RWops *src);
%% Desc: Not implemented.
loadJPGRW() ->
    exit({not_implemented, loadJPGRW}).

%% Func: loadTIFRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadTIF_RW(SDL_RWops *src);
%% Desc: Not implemented.
loadTIFRW() ->
    exit({not_implemented, loadTIFRW}).

%% Func: loadPNGRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadPNG_RW(SDL_RWops *src);
%% Desc: Not implemented.
loadPNGRW() ->
    exit({not_implemented, loadPNGRW}).

%% Func: loadTGARW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadTGA_RW(SDL_RWops *src);
%% Desc: Not implemented.
loadTGARW() ->
    exit({not_implemented, loadTGARW}).

%% Func: loadLBMRW
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_LoadLBM_RW(SDL_RWops *src);
%% Desc: Not implemented.
loadLBMRW() ->
    exit({not_implemented, loadLBMRW}).

%% Func: readXPMFromArray
%% Args: N/A
%% Returns: N/A
%% C-API func: SDL_Surface * IMG_ReadXPMFromArray(char **xpm);
%% Desc: Not implemented.
readXPMFromArray() ->
    exit({not_implemented, readXPMFromArray}).

%% Func: setError
%% Args: N/A
%% Returns: N/A
%% C-API func: void IMG_SetError(const char *fmt, ...)
%% Desc: Not implemented.
setError(_Error) ->
    exit({not_implemented, setError}).

%% Func: getError
%% Args: -
%% Returns: Error
%% C-API func: char * IMG_GetError()
%% Desc: Returns a (string) containing a human readable version or the
%%       reason for the last error that occured.
getError() ->
    Bin = call(?SDL_IMG_GetError, []),
    binary_to_list(Bin).

%%%%%%%%%%% Internal functions %%%%%%%%%%%    

