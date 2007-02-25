%%  Copyright (c) 2007 Klas Johansson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_ttf.hrl
%%% Author  : Klas Johansson <klajo at users.sourceforge.net>
%%% Purpose : defines from SDL_ttf.h
%%% Created : 29 Jan 2007 by Klas Johansson <klajo at users.sourceforge.net>
%%%----------------------------------------------------------------------

-record(ttf_font, {ptr}).

%% ZERO WIDTH NO-BREAKSPACE (Unicode byte order mark)
-define(UNICODE_BOM_NATIVE,      16#FEFF).
-define(UNICODE_BOM_SWAPPED,     16#FFFE).

-define(SDL_TTF_STYLE_NORMAL,    16#00).
-define(SDL_TTF_STYLE_BOLD,      16#01).
-define(SDL_TTF_STYLE_ITALIC,    16#02).
-define(SDL_TTF_STYLE_UNDERLINE, 16#04).


