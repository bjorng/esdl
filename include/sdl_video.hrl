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
%%% Purpose : defines from SDL_video.h
%%% Created : 22 Jun 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

%% Statics

%% Data Types 
-record(sdl_rect,        {x, y, w, h}).
-record(sdl_color,       {r,g,b}). %% Where each color is 8bits
%% sdl_palette  =    [sdl_color]
-record(sdl_pixelformat,
	{
	  self = null,           %%  Ref to this record
	  palette = null,        %%  colorRef
	  bitsPerPixel,   %%  Uint8
	  bytesPerPixel,  %%  Uint8
	  rloss,          %%  Uint8
	  gloss,          %%  Uint8
	  bloss,          %%  Uint8
	  aloss,          %%  Uint8
	  rshift,         %%  Uint8
	  gshift,         %%  Uint8
	  bshift,         %%  Uint8
	  ashift,         %%  Uint8
	  rmask,          %%  Uint32         
	  gmask,          %%  Uint32
	  bmask,          %%  Uint32
	  amask,          %%  Uint32

	  %%/* RGB color key information */
	  colorkey,       %%  Uint32
	  %%/* Alpha value information (per-surface alpha) */
	  alpha           %%  Uint8
	 }).

-record(sdl_surface,
	{
	  self,    %%  Ref to this record
	  flags,   % Uint32		/* Read-only */
	  format,  % SDL_PixelFormat ref*  /* Read-only */
	  w, h,    % int		/* Read-only */
	  pitch,   % Uint16		/* Read-only */
	  pixels,  % void * ref		/* Read-write */
	  offset   % int		/* Private */

	  %%         /* Hardware-specific surface info */
	  %%hwdata,    % struct private_hwdata /* clipping information */
%	  clip_minx, % int		/* Read-only */
%	  clip_maxx, % int		/* Read-only */
%	  clip_miny, % int		/* Read-only */
%	  clip_maxy  % int		/* Read-only */

	  %%	/* info for fast blit mapping to other surfaces */
	  %%map,       % struct SDL_BlitMap /* Private */

	  %%	/* List of surfaces mapped */
	  %%mapped, % struct map_list /* Private */
	  
	  %% /* Reference count -- used when freeing surface */
	  %%refcount % int		/* Read-mostly */
	 }).

-record(sdl_videoinfo,
	{
	  hw_available , %boolean Flag: Can you create hardware surfaces? 
	  wm_available , %boolean Flag: Can you talk to a window manager? 
	  blit_hw      , %boolean Flag: Accelerated blits HW --> HW 
	  blit_hw_CC   , %boolean Flag: Accelerated blits with Colorkey 
	  blit_hw_A    , %boolean Flag: Accelerated blits with Alpha 
	  blit_sw      , %boolean Flag: Accelerated blits SW --> HW 
	  blit_sw_CC   , %boolean Flag: Accelerated blits with Colorkey 
	  blit_sw_A    , %boolean Flag: Accelerated blits with Alpha 
	  blit_fill    , %boolean Flag: Accelerated color fill 
	  video_mem    , % video memory in k bytes
	  vfmt           % Ref to SDL_PixelFormat 
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SDL_video.h  see file for documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SDL_GRAB_QUERY,     -1).
-define(SDL_GRAB_OFF,        0).
-define(SDL_GRAB_ON,         1).
-define(SDL_GRAB_FULLSCREEN, 2).

%%/* These are the currently supported flags for the SDL_surface */
%%/* Available for SDL_CreateRGBSurface() or SDL_SetVideoMode() */

-define( SDL_SWSURFACE,	16#00000000). %%	/* Surface is in system memory */
-define( SDL_HWSURFACE,	16#00000001). %%	/* Surface is in video memory */

-define( SDL_ASYNCBLIT,	16#00000004). %%	/* Use asynchronous blits if possible */

%% /* Available for SDL_SetVideoMode() */
-define( SDL_ANYFORMAT,	16#10000000). %%	/* Allow any video depth/pixel-format */
-define( SDL_HWPALETTE,	16#20000000). %%	/* Surface has exclusive palette */
-define( SDL_DOUBLEBUF,	16#40000000). %%	/* Set up double-buffered video mode */
-define( SDL_FULLSCREEN,16#80000000). %%	/* Surface is a full screen display */
-define( SDL_OPENGL,    16#00000002). %%        /* Create an OpenGL rendering context */
-define( SDL_RESIZABLE,	16#00000010). %%	/* This video mode may be resized */
-define( SDL_NOFRAME,	16#00000020). %%	/* No window caption or edge frame */

%% /* Used internally (read-only) */
-define( SDL_HWACCEL,	  16#00000100). %%/* Blit uses hardware acceleration */
-define( SDL_SRCCOLORKEY, 16#00001000). %%/* Blit uses a source color key */
-define( SDL_RLEACCELOK,  16#00002000). %%/* Private flag */
-define( SDL_RLEACCEL,	  16#00004000). %%/* Colorkey blit is RLE accelerated */
-define( SDL_SRCALPHA,	  16#00010000). %%/* Blit uses source alpha blending */
-define( SDL_SRCCLIPPING, 16#00100000). %%/* Blit uses source clipping */
-define( SDL_PREALLOC,	  16#01000000). %%/* Surface uses preallocated memory */

%% The OpenGL window attributes
-define(SDL_GL_RED_SIZE,            0).
-define(SDL_GL_GREEN_SIZE,          1).
-define(SDL_GL_BLUE_SIZE,           2).
-define(SDL_GL_ALPHA_SIZE,          3).
-define(SDL_GL_BUFFER_SIZE,         4).
-define(SDL_GL_DOUBLEBUFFER,        5).
-define(SDL_GL_DEPTH_SIZE,          6).
-define(SDL_GL_STENCIL_SIZE,        7).
-define(SDL_GL_ACCUM_RED_SIZE,      8).
-define(SDL_GL_ACCUM_GREEN_SIZE,    9).
-define(SDL_GL_ACCUM_BLUE_SIZE,    10).
-define(SDL_GL_ACCUM_ALPHA_SIZE,   11).


