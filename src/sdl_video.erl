%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_video.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : Provide access to SDL's video functions
%%% Created : 26 Jun 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(sdl_video).

-include("sdl.hrl").
-include("esdl.hrl").
-include("sdl_video.hrl").
-include("sdl_video_funcs.hrl").
-include("sdl_util.hrl").

%%% ERL_SDL functions
-export([getSurface/1, 
	 getPixelFormat/1, 
	 getPalette/1,
	 getPixels/2]).
%%% SDL_Functions
-export([
	 blitSurface/4,            
	 createRGBsurface/8,
	 createRGBsurface/4,
	 createRGBsurfaceFrom/9,
	 displayFormat/1,
	 displayFormatAlpha/1,
	 fillRect/3,  
	 flip/1,      
	 freeSurface/1,   
	 getColors/3,                   
	 getVideoSurface/0,             
	 getVideoinfo/0,                
	 listModes/2,                   
	 loadBMP/1,
	 lockSurface/1,
	 mapRGB/4,
	 mapRGBA/5,
	 mustLock/1,
	 saveBMP/2,
	 setAlpha/3,
	 setClipping/5,
	 setColorKey/3,
	 setColors/4,
	 setVideoMode/4,
	 unlockSurface/1,
	 updateRect/5,
	 updateRects/2,
	 videoDriverName/0,                              
	 videoModeOK/4,
	 setGamma/3,
	 setGammaRamp/3,
	 getGammaRamp/0,
	 %% Window Manager Funcs
	 wm_setCaption/2,
	 wm_getCaption/0,
	 wm_setIcon/2,
	 wm_iconifyWindow/0,
	 wm_toggleFullScreen/1,
	 wm_grabInput/1,
	 wm_getInfo/0,
	 wm_isMaximized/0,
	 wm_maximize/0,
	 wm_mac_file_dialog/1,
	 %% OpenGL Support funcs 
	 gl_setAttribute/2,
	 gl_getAttribute/1,
	 gl_swapBuffers/0,

	 getClipRect/1,
	 setClipRect/2
	]).

%% Imports
-import(sdl, [call/2,cast/2]).

%%-import(sdl, [call/1]).

%% Defines 
-define(mk_rectbin(Rect),    
	(<<(Rect#sdl_rect.x):16, 
	(Rect#sdl_rect.y):16, 
	(Rect#sdl_rect.w):16, 
	(Rect#sdl_rect.h):16>>)/binary ).

-define(mk_rectbinary(Rect),    
	<<(Rect#sdl_rect.x):16, 
	(Rect#sdl_rect.y):16, 
	(Rect#sdl_rect.w):16, 
	(Rect#sdl_rect.h):16>>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ERL_SDL API

%% Func: getSurface
%% Args: Surface Ref
%% Returns: sdl_surface record
%% C-API func: 
%% Desc: Copies data pointed to by Surface to sdl_surface record.
getSurface(Surface) when is_record(Surface, sdl_surface) ->
    getSurface(Surface#sdl_surface.self);
getSurface({surfacep, Ptr} = Ref) ->
    <<Flags:32, Format:?_PTR, W:32, H:32, Pitch:16, Pixels:?_PTR,
    Offset:32>> %% Cminx:32, Cmaxx:32, Cminy :32, Cmaxy:32>> 
	= call(?ESDL_getSurface, <<Ptr:?_PTR>>),
    #sdl_surface{self = Ref,
		 flags = Flags,
		 format = if 
			      Format /= 0 -> {formatp, Format};
			      true -> null
			  end,
		 w = W, h = H,
		 pitch = Pitch,
		 pixels = if 
			      Pixels /= 0 -> {pixelp, Pixels};
			      true -> null
			  end, 
		 offset = Offset}.

%% Func: getPixelFormat
%% Args: PixelFormatRef
%% Returns: a sdl_pixelformat record
%% C-API func: 
%% Desc: Copies data pointed to by Surface to sdl_pixelformat record.
getPixelFormat(Surface) when is_record(Surface, sdl_surface) ->
    getPixelFormat(Surface#sdl_surface.self);
getPixelFormat({surfacep, Ptr} = Ref) ->
    Bin = call(?ESDL_getPixelFormat, <<Ptr:?_PTR>>),
    <<Palette:?_PTR, BitsPP:8, BytesPP:8,  
      Rloss:8,  Gloss:8, Bloss:8, Aloss:8, 
      Rshift:8, Gshift:8, Bshift:8, Ashift:8, 
      Rmask:32, Gmask:32, Bmask:32, Amask:32, 
      Colorkey:32, Alpha:8 >> = Bin,
    #sdl_pixelformat{self = Ref, 
		     palette = if 
				   Palette /= 0 -> {palettep, Palette};
				   true -> null
			       end,
		     bitsPerPixel = BitsPP, bytesPerPixel= BytesPP,
		     rloss        = Rloss,  gloss        = Gloss,
		     bloss        = Bloss,  aloss        = Aloss,
		     rshift       = Rshift, gshift       = Gshift,
		     bshift       = Bshift, ashift       = Ashift,
		     rmask        = Rmask,  gmask        = Gmask,
		     bmask        = Bmask,  amask        = Amask,
		     colorkey     = Colorkey, alpha        = Alpha}.

%% Func: getPalette
%% Args: SurfaceRef ref
%% Returns: a sdl_palette record
%% C-API func: 
%% Desc: Copies data pointed to by Surface to sdl_palette record.
getPalette(Surface) when is_record(Surface, sdl_surface) ->
    getPalette(Surface#sdl_surface.self);
getPalette({surfacep, Ptr}) ->
    <<Length:16, Tail/binary>> = call(?ESDL_getPalette, <<Ptr:?_PTR>>),
    getColors(Tail, Length, []).

% local functions
getColors(_Bin, 0, Acc) ->
    lists:reverse(Acc);
getColors(<<R:8, G:8, B:8, Tail/binary>>, N, Acc) ->
    Color = #sdl_color{r = R, g = G, b = B},
    getColors(Tail, N-1, [Color | Acc]).

%% Func: getPixels
%% Args: Surface Ref, Size (a sdl_rectangle record)
%% Returns: A binary containing all pixles, size varies with pixel format
%% C-API func: 
%% Desc: Copies pixels (specified by Size) of Surface to a binary
getPixels(Surface, Size) when is_record(Surface, sdl_surface) ->
    getPixels(Surface#sdl_surface.self, Size);
getPixels({surfacep, Ref}, Size) ->
    Bin = call(?ESDL_getPixels, <<Ref:?_PTR, ?mk_rectbin(Size)>>),
    Bin.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SDL API

%% Func: setVideoMode
%% Args: 
%% Returns: a surface ref or the atom 'error'
%% C-API func:  SDL_Surface *SDL_SetVideoMode(int width, int height, 
%%                                            int bpp, Uint32 flags);
setVideoMode(W, H, Bpp, Type) ->
    call(?SDL_SetVideoMode, <<W:16,H:16,Bpp:16,Type:32>>),
    receive 
	{'_esdl_result_', 0} -> error;
	{'_esdl_result_', Pointer} -> 
	    case (Type band ?SDL_OPENGL) > 1 of
		true ->
		    DynLib = case os:type() of
				 {win32,_} -> "erl_gl.dll";
				 _ ->         "erl_gl.so"
			     end,
		    WXDL0 = filename:join(code:priv_dir(wx),DynLib),
		    WXDL = case filelib:is_file(WXDL0) of
			       true -> WXDL0;
			       false -> filename:join([code:priv_dir(wx),
						       erlang:system_info(system_architecture),
						       DynLib])
			   end,
		    call(?ESDL_Init_Opengl,  <<(list_to_binary(WXDL))/binary, 0:8>>),
		    receive 
			{'_esdl_result_', 0} ->
			    error(opengl_init_failed);
			{'_esdl_result_', GL} when GL > 0 ->
			    put(opengl_port, esdl_port),
			    ok
		    end;
		false -> ok
	    end,
	    {surfacep, Pointer}
    end.

%% Func: videoDriverName
%% Args: 
%% Returns: [Name] | []
%% C-API func:  char *SDL_VideoDriverName(char *namebuf, int maxlen);
videoDriverName() ->
    binary_to_list(call(?SDL_VideoDriverName, [])).

%% Func: getVideoSurface
%% Args: 
%% Returns: surface ref to the current display 
%% C-API func:  SDL_Surface * SDL_GetVideoSurface(void);
getVideoSurface() ->
    <<Bin:?_PTR>> = call(?SDL_GetVideoSurface, []),
    {surfacep, Bin}.

getVideoinfo() ->
    <<HW:8,WM:8, 
    BHW:8, BHWCC:8, BHWA:8, 
    BSW:8, BSWCC:8, BSWA:8, 
    BF:8,  VM:32, VF:?_PTR>> = call(?SDL_GetVideoInfo, []),
    #sdl_videoinfo{hw_available = HW  /= 0,
		   wm_available = WM  /= 0,
		   blit_hw      = BHW /= 0,
		   blit_hw_CC   = BHWCC /= 0,
		   blit_hw_A    = BHWA /= 0,
		   blit_sw      = BSW /= 0,
		   blit_sw_CC   = BSWCC /= 0, 
		   blit_sw_A    = BSWA /= 0,
		   blit_fill    = BF /= 0,
		   video_mem    = VM,
		   vfmt = {formatp, VF}}.
					  
%% Func: videoModeOK
%% Args: See sdl documentation
%% Returns:  true | false  (see sdl doc)
%% C-API func: int SDL_VideoModeOK(int width, int height, int bpp, Uint32 flags);
videoModeOK(W, H, Bpp, Type) ->
    call(?SDL_VideoModeOK, <<W:16, H:16, Bpp:16, Type:32>>),
    receive
	{'_esdl_result_', Res} -> Res /= 0
    end.

%% Func: listModes
%% Args: PixelFormat (either as sdl_pixelformat record or a PixelFormat Ref or null) 
%%       VideoFlags
%% Returns: [AvailableDimensions (in sdl_rect)] | all
%% C-API func: SDL_Rect ** SDL_ListModes(SDL_PixelFormat *format, Uint32 flags);
listModes({formatp, Ref}, Flags) ->
    <<M:8, Bin/binary>> = call(?SDL_ListModes,<<Flags:32, 0:8, Ref:?_PTR>>),
    listModesRet(M, Bin);
listModes(null, Flags) ->
    <<M:8, Bin/binary>> = call(?SDL_ListModes, <<Flags:32, 0:8, 0:?_PTR>>),
    listModesRet(M, Bin);
listModes(PF, Flags) when is_record(PF, sdl_pixelformat) ->
    <<M:8, Bin/binary>>	= call(?SDL_ListModes,
			       <<Flags:32, 1:8, 
				(case PF#sdl_pixelformat.palette of
				     null ->  0;
				     {palettep, Ref} -> Ref
				 end):?_PTR,
	       (PF#sdl_pixelformat.bitsPerPixel):8,
	       (PF#sdl_pixelformat.bytesPerPixel):8,
	       (PF#sdl_pixelformat.rloss):8,        
	       (PF#sdl_pixelformat.gloss):8,        
	       (PF#sdl_pixelformat.bloss):8,        
	       (PF#sdl_pixelformat.aloss):8,        
	       (PF#sdl_pixelformat.rshift):8,       
	       (PF#sdl_pixelformat.gshift):8,       
	       (PF#sdl_pixelformat.bshift):8,       
	       (PF#sdl_pixelformat.ashift):8,       
	       (PF#sdl_pixelformat.rmask):32,                 
	       (PF#sdl_pixelformat.gmask):32,        
	       (PF#sdl_pixelformat.bmask):32,        
	       (PF#sdl_pixelformat.amask):32,        
	       (PF#sdl_pixelformat.colorkey):32,     
	       (PF#sdl_pixelformat.alpha):8>>),
    listModesRet(M, Bin).
listModesRet(0, <<>>) ->	       
    [];
listModesRet(255, <<>>) ->
    all;
listModesRet(0, <<X:16, Y:16, W:16, H:16, Rest/binary>>) ->
    [#sdl_rect{x = X, y=Y, w=W, h=H} | listModesRet(0, Rest)].   

%% Func: displayFormat
%% Args: Surface Ref 
%% Returns: Surface Ref
%% C-API func: SDL_Surface * SDL_DisplayFormat(SDL_Surface *surface);
displayFormat(Surface) when is_record(Surface, sdl_surface) ->
    displayFormat(Surface#sdl_surface.self);
displayFormat({surfacep, Ref}) ->
    <<Res:?_PTR>> = call(?SDL_DisplayFormat, <<Ref:?_PTR>>),
    case Res of
	0 -> exit({sdl_displayformat, returned_null_pointer});
	_ -> {surfacep, Res}
    end.

%% Func: blitSurface
%% Args: SrcSurfaceRef, SrcRect | null, DestSurfaceRef, DestRect | null
%% Returns: {ClippedSrcRect | null, ClippedDestRect | null} or 
%%          {error, ErrorLevel}  Where ErrorLevel = -1 | -2
%% C-API func: int SDL_BlitSurface(SDL_Surface *src, SDL_Rect *srcrect,
%% 			           SDL_Surface *dst, SDL_Rect *dstrect);
blitSurface(SSurf, SRect, DSurf, DRect) 
  when is_record(SSurf, sdl_surface) ->
    blitSurface(SSurf#sdl_surface.self, SRect, DSurf, DRect);
blitSurface(SSurf, SRect, DSurf, DRect) 
  when is_record(DSurf, sdl_surface) ->
    blitSurface(SSurf, SRect, DSurf#sdl_surface.self, DRect);
blitSurface({surfacep, SrcRef}, null, {surfacep, DestRef}, null) ->
    blitSurface2(<<SrcRef:?_PTR, DestRef:?_PTR, 0:8>>);	       
blitSurface({surfacep, SrcRef}, SrcRect, {surfacep, DestRef}, null) ->
    blitSurface2(<<SrcRef:?_PTR, DestRef:?_PTR, 1:8, ?mk_rectbin(SrcRect)>>);
blitSurface({surfacep, SrcRef}, null, {surfacep, DestRef}, DestRect) ->
    blitSurface2(<<SrcRef:?_PTR, DestRef:?_PTR, 2:8, ?mk_rectbin(DestRect)>>);
blitSurface({surfacep, SrcRef}, SRect, %{sdl_rect, SX,SY,SW,SH}, 
	    {surfacep, DestRef}, DRect) -> %{sdl_rect, DX,DY,DW,DH}) ->
    blitSurface2(<<SrcRef:?_PTR, DestRef:?_PTR, 3:8, 
		 ?mk_rectbin(SRect),    %SX:16, SY:16, SW:16, SH:16,
		 ?mk_rectbin(DRect)>>). %DX:16, DY:16, DW:16, DH:16>>).

blitSurface2(Bin) ->
    <<Res:8, Type:8, Rest/binary>> = call(?SDL_BlitSurface, Bin),
    {SR, DR} = 
	case Type of
	    0 ->
		{null, null};
	    1 ->
		<<X:16, Y:16, W:16, H:16>> = Rest,
		{#sdl_rect{x = X, y = Y, w = W, h = H}, null};
	    2 -> 
		<<X:16, Y:16, W:16, H:16>> = Rest,
		{null, #sdl_rect{x = X, y = Y, w = W, h = H}};
	    3 ->
		<<Xs:16, Ys:16, Ws:16, Hs:16,
		Xd:16, Yd:16, Wd:16, Hd:16>> = Rest,
		{#sdl_rect{x = Xs, y = Ys, w = Ws, h = Hs}, 
		 #sdl_rect{x = Xd, y = Yd, w = Wd, h = Hd}}
	end,    
    case Res of
	0 -> 
	    {SR, DR};
	Error ->
	    {error, Error, SR, DR}
    end.

%% Func: fillRect
%% Args: SurfaceRef, sdl_rectangle|null, color
%% Returns: true | false (on failure)
%% C-API func: int SDL_FillRect(SDL_Surface *dst, SDL_Rect *dstrect, Uint32 color);
fillRect(S, R, C) when is_record(S, sdl_surface) ->
    fillRect(S#sdl_surface.self, R, C);
fillRect({surfacep, SRef}, null, Color) ->
    fillRect2(<<SRef:?_PTR, Color:32, 0:8>>);
fillRect({surfacep, SRef}, {sdl_rect, X, Y, W, H}, Color) ->
    fillRect2(<<SRef:?_PTR, Color:32, 1:8, X:16, Y:16, W:16, H:16>>).

fillRect2(Bin) ->
    <<Res:8>> = call(?SDL_FillRect, Bin),
    Res == 0.

%% Func: updateRect
%% Args: SurfaceRef, X, Y, W, H
%% Returns: ok
%% C-API func: void SDL_UpdateRect(SDL_Surface *screen, Sint32 x, Sint32 y, Uint32 w, Uint32 h); 
updateRect(S, X, Y, W, H) ->
    updateRects(S, [#sdl_rect{x = X, y=Y, w=W, h=H}]).

%% Func: updateRects
%% Args: SurfaceRef, [sdl_rect]
%% Returns: ok
%% C-API func:  void SDL_UpdateRects(SDL_Surface *screen, int numrects, SDL_Rect *rects);
updateRects(S, R) when is_record(S, sdl_surface) ->
    updateRects(S#sdl_surface.self, R);
updateRects({surfacep, Ref}, Rects) when is_list(Rects) ->
    Length   = length(Rects),
    BinRects = list_to_binary(mk_rectbins(Rects)), 
    cast(?SDL_UpdateRects, <<Ref:?_PTR, Length:16, BinRects/binary>>).

mk_rectbins([]) ->
    [] ;
mk_rectbins([H|T]) ->
    [?mk_rectbinary(H) | mk_rectbins(T)].

%% Func: flip
%% Args: Surface Ref
%% Returns: true | false (if error)
%% C-API func: int SDL_Flip(SDL_Surface *screen);
flip({surfacep, Ref}) ->
    <<Res:8>> = call(?SDL_Flip, <<Ref:?_PTR>>),
    Res == 0;
flip(S) when is_record(S, sdl_surface) ->
    flip(S#sdl_surface.self).

%% Func: setColors
%% Args: SurfaceRef, [ColorList], FirstColor, NoOfColors
%% Returns: true (when all colors where set) false (otherwise)
%% C-API func:  int SDL_SetColors(SDL_Surface *surface, SDL_Color *colors, int firstcolor, int ncolors);
setColors(S, Colors, FirstColor, Ncolors) when is_record(S, sdl_surface) ->
    setColors(S#sdl_surface.self, Colors, FirstColor, Ncolors);
setColors({surfacep, Ref}, Colors, FirstColor, Ncolors) when is_list(Colors) ->
    BinC = list_to_binary(mkbin_colors(Colors, Ncolors)),
    Length = size(BinC) div 3,
    <<Res:8>> = call(?SDL_SetColors, <<Ref:?_PTR, FirstColor:32, Length:32, BinC/binary>>),
    Res == 1.

mkbin_colors([], _) ->    <<>> ;
mkbin_colors(_,  0) ->    <<>> ;
mkbin_colors([H|R], N) ->
	Bb = mkbin_colors(R, N-1),
    <<(H#sdl_color.r):8, (H#sdl_color.g):8,(H#sdl_color.b):8, Bb/binary>>. 

%% Func: setColorKey
%% Args: Surface Ref, Flag(int), Key(int)
%% Returns: true (on success) | false on failure
%% C-API func: int SDL_SetColorKey(SDL_Surface *surface, Uint32 flag, Uint32 key);
setColorKey(Surface, Flag, Key) when is_record(Surface, sdl_surface) ->
    setColorKey(Surface#sdl_surface.self, Flag, Key);
setColorKey({surfacep, Ref}, Flag, Key) ->
    <<Res:8>> = call(?SDL_SetColorKey, <<Ref:?_PTR, Flag:32, Key:32>>),
    Res == 0.

%% Func: mapRGB
%% Args: Surface Ref, Int32, Int32, Int32
%% Returns: Int32 
%% C-API func:  Uint32 SDL_MapRGB(SDL_PixelFormat *format, Uint8 r, Uint8 g, Uint8 b);
mapRGB(Surface, R, G, B) when is_record(Surface, sdl_surface) -> 
    mapRGB(Surface#sdl_surface.self, R, G, B);
mapRGB({surfacep, Ref}, R, G, B) ->
    <<Pixel:32>> = call(?SDL_MapRGB, <<Ref:?_PTR, R:8, G:8, B:8>>),
    Pixel.

%% Func: createRGBsurface
%% Args: SurfaceFlags Width Height Depth Rmask:32 Gmask:32 Bmask:32 Amask:32
%% Returns: A surface ref
%% C-API func: SDL_Surface *SDL_CreateRGBSurface(Uint32 flags, int width, int height, int depth, Uint32 Rmask, Uint32 Gmask, Uint32 Bmask, Uint32 Amask);
createRGBsurface(Flags, W, H, D, RM, GM, BM, AM) ->
    <<Ref:?_PTR>> = call(?SDL_CreateRGBSurface,
		      <<Flags:32, W:16, H:16, D:8, 1:8,
		       RM:32, GM:32, BM:32, AM:32>>),
    case Ref of
	0 -> exit({createRGBsurface, returned_null_pointer});
	_ -> {surfacep, Ref}
    end.

createRGBsurface(Flags, W, H, D) ->
    <<Ref:?_PTR>> = call(?SDL_CreateRGBSurface,
		      <<Flags:32, W:16, H:16, D:8, 0:8>>),
    case Ref of
	0 -> exit({createRGBsurface, returned_null_pointer});
	_ -> {surfacep, Ref}
    end.


%% Func:    createRGBsurfaceFrom
%% Args:    Pixels(Binary) Width Height Depth Pitch 
%%          Rmask:32 Gmask:32 Bmask:32 Amask:32
%% Returns: A surface ref
%% C-API func: SDL_Surface *SDL_CreateRGBSurfaceFrom(void *pixels, int width, int height, int depth, int pitch,	Uint32 Rmask, Uint32 Gmask, Uint32 Bmask, Uint32 Amask);
createRGBsurfaceFrom(Pixels, W, H, D, P, RM, GM, BM, AM) 
  when is_binary(Pixels) ->
    <<Ref:?_PTR>> = call(?SDL_CreateRGBSurface,
			 <<W:16, H:16, D:8, P:16,
			  RM:32, GM:32, BM:32, AM:32, (size(Pixels)):32, Pixels/binary>>),
    case Ref of
	0 -> exit({createRGBsurface, returned_null_pointer});
	_ -> {surfacep, Ref}
    end;
createRGBsurfaceFrom({pixelp, PixelRef}, W, H, D, P, RM, GM, BM, AM)  ->
    <<Ref:?_PTR>> = call(?SDL_CreateRGBSurface,
			 <<W:16, H:16, D:8, P:16,
			  RM:32, GM:32, BM:32, AM:32, -1:32,  PixelRef:?_PTR>>),
    case Ref of
	0 -> exit({createRGBsurface, returned_null_pointer});
	_ -> {surfacep, Ref}
    end.

%% Func: freeSurface
%% Args: Surface Ref 
%% Returns: ok
%% C-API func: void SDL_FreeSurface(SDL_Surface *surface);
freeSurface(Surface) when is_record(Surface, sdl_surface) ->
    freeSurface(Surface#sdl_surface.self);
freeSurface({surfacep, Ref}) ->
    cast(?SDL_FreeSurface, <<Ref:?_PTR>>).

%% Func: mustLock  
%% Args: surfaceRef
%% Returns: Bool
%% C-API func: Macro
mustLock(Surface) when is_record(Surface, sdl_surface) ->
    Flag = Surface#sdl_surface.flags band 
	(?SDL_HWSURFACE bor ?SDL_ASYNCBLIT bor ?SDL_RLEACCEL),
    Flag /= 0.

%% Func:   lockSurface (I don't have any hardware to test this function on
%%         the manual says don't do any lib func calls when surface is locked
%%         we can't avoid that :)
%% Args:   Surface Ref
%% Returns: true (if locked) false otherwise
%% C-API func:  int SDL_LockSurface(SDL_Surface *surface);
lockSurface(Surface) when is_record(Surface, sdl_surface) ->
    lockSurface(Surface#sdl_surface.self);
lockSurface({surfacep, Ref}) ->
    <<Res:8>> = call(?SDL_LockSurface, <<Ref:?_PTR>>),
    Res == 0.

%% Func:    unLockSurface (I don't have any hardware to test this function on)
%% Args:    Surface Ref
%% Returns: ok
%% C-API func:  void SDL_UnlockSurface(SDL_Surface *surface);
unlockSurface(Surface) when is_record(Surface, sdl_surface) ->
    unlockSurface(Surface#sdl_surface.self);
unlockSurface({surfacep, Ref}) ->
    cast(?SDL_UnlockSurface, <<Ref:?_PTR>>).

%% Func: loadBMP
%% Args: Name of the BMP file (string)
%% Returns: a surface ref or null
%% C-API func: SDL_Surface * SDL_LoadBMP(char * file);
loadBMP(File) when is_list(File) ->
    <<Res:?_PTR>> = call(?SDL_LoadBMP, [File,0]),
    case Res of 
	0 -> 
	    null;
	_ -> 
	    {surfacep, Res}
    end.

%% Func: saveBMP
%% Args: Name of the BMP file (string)
%% Returns: true or false (if it failed)
%% C-API func: int SDL_SaveBMP(SDL_Surface *surface, char *file)
saveBMP(Surface, File) when is_record(Surface, sdl_surface) ->
    saveBMP(Surface#sdl_surface.self, File);
saveBMP({surfacep, Ref}, File) when is_list(File) ->
    <<Res:8>> = call(?SDL_SaveBMP, [<<Ref:?_PTR>>,File,0]),
    Res == 0.

%% Func: setAlpha  
%% Args: SurfaceRef, Flag:32, Alpha:32      
%% Returns: integer (undocumented in (my version of) SDL)
%% C-API func: int SDL_SetAlpha(SDL_Surface *surface, Uint32 flag, Uint8 alpha);
setAlpha(Surface, Flag, Alpha) when is_record(Surface, sdl_surface) ->
    setAlpha(Surface#sdl_surface.self, Flag, Alpha);
setAlpha({surfacep, Ref}, Flag, Alpha)->
    <<Res:32>> = call(?SDL_SetAlpha, <<Ref:?_PTR, Flag:32, Alpha:32>>),
    Res.

%% Func:  setClipping 
%% Args:  SurfaceRef, Top, Left, Botton, Right
%% Returns: ok
%% C-API func: 
%% Desc: Obsolete?? 
setClipping(Surface, Top, Left, Botton, Right) 
  when is_record(Surface, sdl_surface) ->
    setClipping(Surface#sdl_surface.self, Top, Left, Botton, Right);
setClipping({surfacep, SRef}, Top, Left, Botton, Right) ->
    cast(?SDL_SetClipping, <<SRef:?_PTR, Top:16, Left:16, Botton:16, Right:16>>).

%% Func:  setGamma
%% Args:  red, green, blue 
%% Returns: Bool
%% C-API func: int SDL_SetGamma(float red, float green, float blue)
%% Desc: 
setGamma(Red,Green,Blue) ->
    <<Res:32/native>> =
	call(?SDL_SetGamma,
	     <<Red:32/native-float,Green:32/native-float,Blue:32/native-float>>),
    Res == 0.

%% Func:  setGammaRamp
%% Args:  redList, greenList, blueList (where each list contains 256 16 bits value)
%% Returns: Bool
%% C-API func: int SDL_SetGammaRamp(Uint16 red[256], Uint16 green[256], Uint16 blue[256])
%% Desc: 
setGammaRamp(Red,Green,Blue) ->
    Args = sdl_util:term2bin(Red ++ Green ++ Blue, 256*3, ?SDL_UNSIGNED_SHORT),
    <<Res:32>> = call(?SDL_SetGammaRamp, Args),
    Res == 0.

%% Func:  getGammaRamp
%% Args:  none
%% Returns: {res, redList, greenList, blueList} (where each list contains 256 16 bits value)
%% C-API func: int SDL_GetGammaRamp(Uint16 *red, Uint16 *green, Uint16 *blue])
%% Desc: If res is == true, the lists contain valid information
getGammaRamp() ->
    <<Res:32, R:512/binary,G:512/binary,B:512/binary>> = call(?SDL_GetGammaRamp, []),
    {Res == 0, 
     sdl_util:bin2list(256, ?SDL_UNSIGNED_SHORT, R),  
     sdl_util:bin2list(256, ?SDL_UNSIGNED_SHORT, G),  
     sdl_util:bin2list(256, ?SDL_UNSIGNED_SHORT, B)}.

%%%%%%%%%%% WindowManager Functions %%%%%%%%%%%

%% Func: wm_setCaption  
%% Args: Title List, IconTitleList
%% Returns: ok
%% C-API func: void SDL_WM_SetCaption(const char *title, const char *icon);
wm_setCaption(Title, IconTitle) ->
    cast(?SDL_WM_SetCaption, [Title,0,IconTitle,0]).

%% Func:    wm_getCaption
%% Args:    
%% Returns: {TitleStr, IconTitleStr}
%% C-API func: void SDL_WM_GetCaption(char **title, char **icon);
wm_getCaption() ->
    <<TL:16, IL:1, Bin/binary>> = call(?SDL_WM_GetCaption, []),
    <<Title:TL/binary, Icon:IL/binary>> = Bin,
    {binary_to_list(Title), binary_to_list(Icon)}.

%% Func:    wm_setIcon
%% Args:    Icon (sdl_surface), Mask (list of bytes) | null 
%% Returns: ok
%% C-API func: void SDL_WM_SetIcon(SDL_Surface *icon, Uint8 *mask);
wm_setIcon(Icon, Mask) when is_record(Icon, sdl_surface) ->
    wm_setIcon(Icon#sdl_surface.self, Mask);
wm_setIcon({surfacep, Ref}, null) ->
    cast(?SDL_WM_SetIcon, <<Ref:?_PTR,0:16>>);
wm_setIcon({surfacep, Ref}, Mask) when is_binary(Mask) ->
    cast(?SDL_WM_SetIcon, <<Ref:?_PTR, (size(Mask)):16, Mask/binary>>).

%% Func:    WM_IconifyWindow
%% Args:    
%% Returns: ok
%% C-API func: int SDL_WM_IconifyWindow(void);
wm_iconifyWindow() ->
    cast(?SDL_WM_IconifyWindow, []).

%% Func:   SDL_WM_ToggleFullScreen
%% Args:   SurfaceRef
%% Returns: true or false (on failure)
%% C-API func: int SDL_WM_ToggleFullScreen(SDL_Surface *surface);
wm_toggleFullScreen(Surface) when is_record(Surface, sdl_surface) ->
    wm_toggleFullScreen(Surface#sdl_surface.self);
wm_toggleFullScreen({surfacep, Ref}) ->
    cast(?SDL_WM_ToggleFullScreen, <<Ref:?_PTR>>),
    receive
	{'_esdl_result_', Res} ->  Res == 1
    end.

%% Func:   wm_grabInput
%% Args:   GrabMode (?SDL_GRAB_QUERY | ?SDL_GRAB_OFF | ?SDL_GRAB_ON)
%% Returns:GrabMode (?SDL_GRAB_OFF | ?SDL_GRAB_ON)
%% C-API func: SDL_GrabMode SDL_WM_GrabInput(SDL_GrabMode mode);
wm_grabInput(GrabMode) ->
    <<Res:8>> = call(?SDL_WM_GrabInput, [GrabMode]),
    Res.

%% Func:   wm_getInfo
%% Args:   
%% Returns: {SDLVersion = {Major, Minor, Patch}, OSSpecBinary}
%% C-API func: 
%% Desc:  Only windows returns anything useful (window handle)
wm_getInfo() ->
    cast(?SDL_WM_GetInfo, []),
    PtrSize = 8*erlang:system_info(wordsize),
    receive 
	{'_esdl_result_', Major, Minor, Patch, Rest} ->
	    {{Major, Minor, Patch}, <<Rest:PtrSize/unsigned-native>>}
    end.

%% Func:   wm_isMaximized
%% Args:   
%% Returns: Boolean
%% C-API func: 
%% Desc:  Only windows returns anything useful (window handle)
wm_isMaximized() ->
    <<Bool:8>> = call(?SDL_WM_IsMaximized, []),
    Bool /= 0.

%% Func:   wm_maximize
%% Args:   
%% Returns: nothing
%% C-API func: 
%% Desc:  Only works on windows 
wm_maximize() ->
    cast(?SDL_WM_Maximize, []).

%% Func:   wm_maximize
%% Args:
%% Returns: nothing
%% C-API func:
%% Desc:  Only works on Mac
wm_mac_file_dialog(Props) ->
    Operation = case proplists:get_value(operation, Props) of
		    open -> 0;
		    save -> 1
		end,
    Title = proplists:get_value(title, Props),
    Dir = proplists:get_value(directory, Props),
    DefName = proplists:get_value(default_filename, Props),
    Filters0 = proplists:get_value(filters, Props),
    Filters = [[F,0] || "."++F <- Filters0] ++ [0],
    Data = [Operation,Dir,0,Title,0,DefName,0|Filters],
    call(?SDL_WM_MacFileDialog, Data),
    receive
	{'_esdl_result_',String} ->
	    String
    end.

%%%%%%%%%%%%%%%%%% GL support Funcs %%%%%%%%%%%%%%%%%%%%%

%% Func:    gl_setAttribute
%% Args:    
%% Returns: ok
%% C-API func: int SDL_GL_SetAttribute(SDL_GLattr attr, int value);
gl_setAttribute(Attr, Val) ->
    cast(?SDL_GL_SetAttribute, <<Attr:16,Val:32>>).

%% Func:    gl_getAttribute
%% Args:    
%% Returns: Attribute Value
%% C-API func: int SDL_GL_GetAttribute(SDL_GLattr attr, int* value);
gl_getAttribute(Attr) ->
    call(?SDL_GL_GetAttribute, <<Attr:16>>),
    receive 
	{'_esdl_result_', Res} ->
	    Res
    end.

%% Func:    gl_swapBuffers
%% Args:    
%% Returns: A timestamp generated with SDL_GetTicks
%% C-API func: void SDL_GL_SwapBuffers(void);
gl_swapBuffers() ->
    <<TS:32/unsigned>> = call(?SDL_GL_SwapBuffers, []),
    TS.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Func: mapRGBA
%% Args: Surface Ref, Int32, Int32, Int32, Int32
%% Returns: Int32 
%% C-API func:  Uint32 SDL_MapRGB(SDL_PixelFormat *format, Uint8 r, Uint8 g, Uint8 b, Uint8 a);
mapRGBA(Surface, R, G, B, A) when is_record(Surface, sdl_surface) -> 
    mapRGBA(Surface#sdl_surface.self, R, G, B, A);
mapRGBA({surfacep, Ref}, R, G, B, A) ->
    <<Pixel:32>> = call(?SDL_MapRGBA, <<Ref:?_PTR, R:8, G:8, B:8, A:8>>),
    Pixel.

%% Func: getClipRect
%% Args: Surface Ref 
%% Returns: Rect
%% C-API func: void SDL_GetClipRect(SDL_Surface *surface, SDL_Rect *rect);
getClipRect(Surface) when is_record(Surface, sdl_surface) ->
    getClipRect(Surface#sdl_surface.self);
getClipRect({surfacep, Ref}) ->
    <<X:16, Y:16, W:16, H:16>> = call(?SDL_GetClipRect, <<Ref:?_PTR>>),
    #sdl_rect{x=X, y=Y, w=W, h=H}.
    

%% Func: setClipRect
%% Args: Surface Ref, Rect
%% Returns: ok
%% C-API func: void SDL_SetClipRect(SDL_Surface *surface, SDL_Rect *rect);
setClipRect(Surface, Rect)  when is_record(Surface, sdl_surface) ->
    setClipRect(Surface#sdl_surface.self, Rect);
setClipRect({surfacep, Ref}, #sdl_rect{x=X, y=Y, w=W, h=H}) ->
    cast(?SDL_SetClipRect, <<Ref:?_PTR, X:16, Y:16, W:16, H:16>>).

%% Func: displayFormatAlpha
%% Args: Surface Ref 
%% Returns: Surface Ref
%% C-API func: SDL_Surface * SDL_DisplayFormatAlpha(SDL_Surface *surface);
displayFormatAlpha(Surface) when is_record(Surface, sdl_surface) ->
    displayFormat(Surface#sdl_surface.self);
displayFormatAlpha({surfacep, Ref}) ->
    <<Res:?_PTR>> = call(?SDL_DisplayFormatAlpha, <<Ref:?_PTR>>),
    case Res of
	0 -> exit({sdl_displayformatalpha, returned_null_pointer});
	_ -> {surfacep, Res}
    end.




%%%%%%%%%%% Internal functions %%%%%%%%%%%    

