%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_video.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : defines from SDL_audio.h
%%% Created : 9 Aug 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

%% Data Types 

-record(audiospec,
	{freq,      %% Int32   DSP frequency -- samples per second 
	 format,    %% Uint16  Audio data format 
	 channels,  %% Uint8   Number of channels: 1 mono, 2 stereo
	 silence,   %% Uint8   Audio buffer silence value (calculated)
	 samples,   %% Uint16  Audio buffer size in samples
	 padding,   %% Uint16  Necessary for some compile environments
	 size}).    %% Uint32  Audio buffer size in bytes (calculated)

%% Audio format flags (defaults to LSB byte order)
-define(AUDIO_U8,	16#0008). %% Unsigned 8-bit samples 
-define(AUDIO_S8,	16#8008). %% Signed 8-bit samples 
-define(AUDIO_U16LSB,	16#0010). %% Unsigned 16-bit samples 
-define(AUDIO_S16LSB,	16#8010). %% Signed 16-bit samples 
-define(AUDIO_U16MSB,	16#1010). %% As above, but big-endian byte order 
-define(AUDIO_S16MSB,	16#9010). %% As above, but big-endian byte order 

-define(AUDIO_U16SYS,	16#FFF1). %% Native audio ordering 
-define(AUDIO_S16SYS,	16#FFF0). 

