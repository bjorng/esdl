%%  Copyright (c) 2005 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%

-module(gl).
-include("esdl.hrl").
-include("gl.hrl").
-include("gl_funcs.hrl").
-include("sdl_util.hrl").


-compile(export_all).
-import(sdl, [call/2,cast/2]).
-import(sdl_util, [bin2list/2,bin2list/3, term2bin/2, term2bin/3, matrix2bin/2]).

%% Some binary type shortcuts
-define(UN, unsigned-native).
-define(SN, signed-native).
-define(FN, float-native).

glGetIntegervLen(Type) ->
    glGetBooleanvLen(Type).
glGetDoublevLen(Type) ->
    glGetBooleanvLen(Type).
glGetFloatvLen(Type) ->
    glGetBooleanvLen(Type).
glGetBooleanvLen(Type) ->
    case Type of
	?GL_ACCUM_ALPHA_BITS     -> 1;
	?GL_ACCUM_BLUE_BITS      -> 1;
	?GL_ACCUM_CLEAR_VALUE    -> 4;
	?GL_ACCUM_GREEN_BITS     -> 1;
	?GL_ACCUM_RED_BITS       -> 1;
	?GL_ALPHA_BIAS           -> 1;
	?GL_ALPHA_BITS           -> 1;
	?GL_ALPHA_SCALE          -> 1;
	?GL_ALPHA_TEST           -> 1;
	?GL_ALPHA_TEST_FUNC      -> 1;
	?GL_ALPHA_TEST_REF       -> 1;
	?GL_ATTRIB_STACK_DEPTH   -> 1;
	?GL_AUTO_NORMAL          -> 1;
	?GL_AUX_BUFFERS          -> 1;
	?GL_BLEND                -> 1;
	?GL_BLEND_DST            -> 1;
	?GL_BLEND_SRC            -> 1;
	?GL_BLUE_BIAS            -> 1;
	?GL_BLUE_BITS            -> 1;
	?GL_BLUE_SCALE           -> 1;
	?GL_CLIENT_ATTRIB_STACK_DEPTH -> 1;
	?GL_CLIP_PLANE0          -> 1;
	?GL_CLIP_PLANE1          -> 1;
	?GL_CLIP_PLANE2          -> 1;
	?GL_CLIP_PLANE3          -> 1;
	?GL_CLIP_PLANE4          -> 1;
	?GL_CLIP_PLANE5          -> 1;
	?GL_COLOR_ARRAY          -> 1;
	?GL_COLOR_ARRAY_SIZE     -> 1;
	?GL_COLOR_ARRAY_STRIDE   -> 1;
	?GL_COLOR_ARRAY_TYPE     -> 1;
	?GL_COLOR_CLEAR_VALUE    -> 4;
	?GL_COLOR_LOGIC_OP       -> 1;
	?GL_COLOR_MATERIAL       -> 1;
	?GL_COLOR_MATERIAL_FACE  -> 1;
	?GL_COLOR_MATERIAL_PARAMETER -> 1;
	?GL_COLOR_WRITEMASK      -> 4;
% -ifdef(GL_CONVOLUTION_1D_EXT).
% 	?GL_CONVOLUTION_1D_EXT   -> 1;
% 	?GL_CONVOLUTION_2D_EXT   -> 1;
% -endif().
	?GL_CULL_FACE            -> 1;
	?GL_CULL_FACE_MODE       -> 1;
	?GL_CURRENT_COLOR        -> 4;
	?GL_CURRENT_INDEX        -> 1;
	?GL_CURRENT_NORMAL       -> 3;
	?GL_CURRENT_RASTER_COLOR -> 4;
	?GL_CURRENT_RASTER_DISTANCE -> 1;
	?GL_CURRENT_RASTER_INDEX -> 1;
	?GL_CURRENT_RASTER_POSITION -> 4;
	?GL_CURRENT_RASTER_POSITION_VALID -> 1;
	?GL_CURRENT_RASTER_TEXTURE_COORDS -> 4;
	?GL_CURRENT_TEXTURE_COORDS -> 4;
	?GL_DEPTH_BIAS           -> 1;
	?GL_DEPTH_BITS           -> 1;
	?GL_DEPTH_CLEAR_VALUE    -> 1;
	?GL_DEPTH_FUNC           -> 1;
	?GL_DEPTH_RANGE          -> 2;
	?GL_DEPTH_SCALE          -> 1;
	?GL_DEPTH_TEST           -> 1;
	?GL_DEPTH_WRITEMASK      -> 1;
	?GL_DITHER               -> 1;
	?GL_DOUBLEBUFFER         -> 1;
	?GL_DRAW_BUFFER          -> 1;
	?GL_EDGE_FLAG            -> 1;
	?GL_EDGE_FLAG_ARRAY      -> 1;
	?GL_EDGE_FLAG_ARRAY_STRIDE -> 1;
	?GL_FOG                  -> 1;
	?GL_FOG_COLOR            -> 4;
	?GL_FOG_DENSITY          -> 1;
	?GL_FOG_END              -> 1;
	?GL_FOG_HINT             -> 1;
	?GL_FOG_INDEX            -> 1;
	?GL_FOG_MODE             -> 1;
	?GL_FOG_START            -> 1;
	?GL_FRONT_FACE           -> 1;
%	?GL_GLOBAL_ALPHA_FACTOR_SUN -> 1;
	?GL_GREEN_BIAS           -> 1;
	?GL_GREEN_BITS           -> 1;
	?GL_GREEN_SCALE          -> 1;
% -ifdef(GL_HISTOGRAM_EXT).
% 	?GL_HISTOGRAM_EXT        -> 1;
% -endif().
	?GL_INDEX_ARRAY          -> 1;
	?GL_INDEX_ARRAY_STRIDE   -> 1;
	?GL_INDEX_ARRAY_TYPE     -> 1;
	?GL_INDEX_BITS           -> 1;
	?GL_INDEX_CLEAR_VALUE    -> 1;
	?GL_INDEX_LOGIC_OP       -> 1;
	?GL_INDEX_MODE           -> 1;
	?GL_INDEX_OFFSET         -> 1;
	?GL_INDEX_SHIFT          -> 1;
	?GL_INDEX_WRITEMASK      -> 1;
	?GL_LIGHT0               -> 1;
	?GL_LIGHT1               -> 1;
	?GL_LIGHT2               -> 1;
	?GL_LIGHT3               -> 1;
	?GL_LIGHT4               -> 1;
	?GL_LIGHT5               -> 1;
	?GL_LIGHT6               -> 1;
	?GL_LIGHT7               -> 1;
% 	?GL_LIGHT8               -> 1;
% 	?GL_LIGHT9               -> 1;
	?GL_LIGHTING             -> 1;
	?GL_LIGHT_MODEL_AMBIENT  -> 4;
	?GL_LIGHT_MODEL_LOCAL_VIEWER -> 1;
	?GL_LIGHT_MODEL_TWO_SIDE -> 1;
	?GL_LINE_SMOOTH          -> 1;
	?GL_LINE_SMOOTH_HINT     -> 1;
	?GL_LINE_STIPPLE         -> 1;
	?GL_LINE_STIPPLE_PATTERN -> 1;
	?GL_LINE_STIPPLE_REPEAT  -> 1;
	?GL_LINE_WIDTH           -> 1;
	?GL_LINE_WIDTH_GRANULARITY -> 1;
	?GL_LINE_WIDTH_RANGE     -> 2;
	?GL_LIST_BASE            -> 1;
	?GL_LIST_INDEX           -> 1;
	?GL_LIST_MODE            -> 1;
	?GL_LOGIC_OP_MODE        -> 1;
	?GL_MAP1_COLOR_4         -> 1;
	?GL_MAP1_GRID_DOMAIN     -> 2;
	?GL_MAP1_GRID_SEGMENTS   -> 1;
	?GL_MAP1_INDEX           -> 1;
	?GL_MAP1_NORMAL          -> 1;
	?GL_MAP1_TEXTURE_COORD_1 -> 1;
	?GL_MAP1_TEXTURE_COORD_2 -> 1;
	?GL_MAP1_TEXTURE_COORD_3 -> 1;
	?GL_MAP1_TEXTURE_COORD_4 -> 1;
	?GL_MAP1_VERTEX_3        -> 1;
	?GL_MAP1_VERTEX_4        -> 1;
	?GL_MAP2_COLOR_4         -> 1;
	?GL_MAP2_GRID_DOMAIN     -> 4;
	?GL_MAP2_GRID_SEGMENTS   -> 2;
	?GL_MAP2_INDEX           -> 1;
	?GL_MAP2_NORMAL          -> 1;
	?GL_MAP2_TEXTURE_COORD_1 -> 1;
	?GL_MAP2_TEXTURE_COORD_2 -> 1;
	?GL_MAP2_TEXTURE_COORD_3 -> 1;
	?GL_MAP2_TEXTURE_COORD_4 -> 1;
	?GL_MAP2_VERTEX_3        -> 1;
	?GL_MAP2_VERTEX_4        -> 1;
	?GL_MAP_COLOR            -> 1;
	?GL_MAP_STENCIL          -> 1;
	?GL_MATRIX_MODE          -> 1;
	?GL_MAX_3D_TEXTURE_SIZE  -> 1;
	?GL_MAX_CLIENT_ATTRIB_STACK_DEPTH -> 1;
	?GL_MAX_ATTRIB_STACK_DEPTH -> 1;
	?GL_MAX_CLIP_PLANES      -> 1;
	?GL_MAX_ELEMENTS_INDICES  -> 1;
	?GL_MAX_ELEMENTS_VERTICES -> 1;
	?GL_MAX_EVAL_ORDER       -> 1;
	?GL_MAX_LIGHTS           -> 1;
	?GL_MAX_LIST_NESTING     -> 1;
	?GL_MAX_MODELVIEW_STACK_DEPTH -> 1;
	?GL_MAX_NAME_STACK_DEPTH -> 1;
	?GL_MAX_PIXEL_MAP_TABLE  -> 1;
	?GL_MAX_PROJECTION_STACK_DEPTH -> 1;
	?GL_MAX_TEXTURE_SIZE     -> 1;
	?GL_MAX_TEXTURE_STACK_DEPTH -> 1;
	?GL_MAX_VERTEX_UNITS_ARB -> 1;
 	?GL_MAX_VIEWPORT_DIMS    -> 2;
	?GL_MODELVIEW_MATRIX     -> 16;
%%	?GL_MINMAX_EXT           -> 1;
	?GL_MODELVIEW_STACK_DEPTH -> 1;
	?GL_NAME_STACK_DEPTH     -> 1;
	?GL_NORMAL_ARRAY         -> 1;
	?GL_NORMAL_ARRAY_STRIDE  -> 1;
	?GL_NORMAL_ARRAY_TYPE    -> 1;
	?GL_NORMALIZE            -> 1;
	?GL_NUM_COMPRESSED_TEXTURE_FORMATS -> 1;
	?GL_PACK_ALIGNMENT       -> 1;
	?GL_PACK_LSB_FIRST       -> 1;
	?GL_PACK_ROW_LENGTH      -> 1;
	?GL_PACK_SKIP_PIXELS     -> 1;
	?GL_PACK_SKIP_ROWS       -> 1;
	?GL_PACK_SWAP_BYTES      -> 1;
	?GL_PERSPECTIVE_CORRECTION_HINT -> 1;
	?GL_PIXEL_MAP_A_TO_A_SIZE -> 1;
	?GL_PIXEL_MAP_B_TO_B_SIZE -> 1;
	?GL_PIXEL_MAP_G_TO_G_SIZE -> 1;
	?GL_PIXEL_MAP_I_TO_A_SIZE -> 1;
	?GL_PIXEL_MAP_I_TO_B_SIZE -> 1;
	?GL_PIXEL_MAP_I_TO_G_SIZE -> 1;
	?GL_PIXEL_MAP_I_TO_I_SIZE -> 1;
	?GL_PIXEL_MAP_I_TO_R_SIZE -> 1;
	?GL_PIXEL_MAP_R_TO_R_SIZE -> 1;
	?GL_PIXEL_MAP_S_TO_S_SIZE -> 1;
	?GL_POINT_SIZE           -> 1;
	?GL_POINT_SIZE_GRANULARITY -> 1;
	?GL_POINT_SIZE_RANGE     -> 2;
	?GL_POINT_SMOOTH         -> 1;
	?GL_POINT_SMOOTH_HINT    -> 1;
	?GL_POLYGON_MODE         -> 2;
	?GL_POLYGON_OFFSET_FACTOR -> 1;
	?GL_POLYGON_OFFSET_UNITS -> 1;
	?GL_POLYGON_OFFSET_FILL  -> 1;
	?GL_POLYGON_OFFSET_LINE  -> 1;
	?GL_POLYGON_OFFSET_POINT -> 1;
%	?GL_POLYGON_OFFSET_BIAS_EXT -> 1;
%	?GL_POLYGON_OFFSET_FACTOR_EXT -> 1;
%	?GL_POLYGON_OFFSET_EXT   -> 1;
	?GL_POLYGON_SMOOTH       -> 1;
	?GL_POLYGON_SMOOTH_HINT  -> 1;
	?GL_POLYGON_STIPPLE      -> 1;
% 	?GL_POST_CONVOLUTION_ALPHA_BIAS_EXT -> 1;
% 	?GL_POST_CONVOLUTION_BLUE_BIAS_EXT -> 1;
% 	?GL_POST_CONVOLUTION_GREEN_BIAS_EXT -> 1;
% 	?GL_POST_CONVOLUTION_RED_BIAS_EXT -> 1;
% 	?GL_POST_CONVOLUTION_ALPHA_SCALE_EXT -> 1;
% 	?GL_POST_CONVOLUTION_BLUE_SCALE_EXT -> 1;
% 	?GL_POST_CONVOLUTION_GREEN_SCALE_EXT -> 1;
% 	?GL_POST_CONVOLUTION_RED_SCALE_EXT -> 1;
	?GL_PROJECTION_MATRIX    -> 16;
	?GL_PROJECTION_STACK_DEPTH -> 1;
	?GL_READ_BUFFER          -> 1;
	?GL_RED_BIAS             -> 1;
	?GL_RED_BITS             -> 1;
	?GL_RED_SCALE            -> 1;
	?GL_RENDER_MODE          -> 1;
	?GL_RGBA_MODE            -> 1;
	?GL_SCISSOR_BOX          -> 4;
	?GL_SCISSOR_TEST         -> 1;
%	?GL_SEPARABLE_2D_EXT     -> 1;
	?GL_SHADE_MODEL          -> 1;
	?GL_STENCIL_BITS         -> 1;
	?GL_STENCIL_CLEAR_VALUE  -> 1;
	?GL_STENCIL_FAIL         -> 1;
	?GL_STENCIL_FUNC         -> 1;
	?GL_STENCIL_PASS_DEPTH_FAIL -> 1;
	?GL_STENCIL_PASS_DEPTH_PASS -> 1;
	?GL_STENCIL_REF          -> 1;
	?GL_STENCIL_TEST         -> 1;
	?GL_STENCIL_VALUE_MASK   -> 1;
	?GL_STENCIL_WRITEMASK    -> 1;
	?GL_STEREO               -> 1;
	?GL_SUBPIXEL_BITS        -> 1;
	?GL_TEXTURE_1D           -> 1;
%	?GL_TEXTURE_1D_BINDING   -> 1;
	?GL_TEXTURE_2D           -> 1;
%	?GL_TEXTURE_2D_BINDING   -> 1;
	?GL_TEXTURE_COORD_ARRAY  -> 1;
	?GL_TEXTURE_COORD_ARRAY_SIZE -> 1;
	?GL_TEXTURE_COORD_ARRAY_STRIDE -> 1;
	?GL_TEXTURE_COORD_ARRAY_TYPE -> 1;
	?GL_TEXTURE_GEN_Q        -> 1;
	?GL_TEXTURE_GEN_R        -> 1;
	?GL_TEXTURE_GEN_S        -> 1;
	?GL_TEXTURE_GEN_T        -> 1;
	?GL_TEXTURE_MATRIX       -> 16;
	?GL_TEXTURE_STACK_DEPTH  -> 1;
	?GL_UNPACK_ALIGNMENT     -> 1;
	?GL_UNPACK_LSB_FIRST     -> 1;
	?GL_UNPACK_ROW_LENGTH    -> 1;
	?GL_UNPACK_SKIP_PIXELS   -> 1;
	?GL_UNPACK_SKIP_ROWS     -> 1;
%	?GL_UNPACK_CONSTANT_DATA_SUNX -> 1;
	?GL_UNPACK_SWAP_BYTES    -> 1;
	?GL_VERTEX_ARRAY         -> 1;
	?GL_VERTEX_ARRAY_SIZE    -> 1;
	?GL_VERTEX_ARRAY_STRIDE  -> 1;
	?GL_VERTEX_ARRAY_TYPE    -> 1;
	?GL_VIEWPORT             -> 4;
	?GL_ZOOM_X               -> 1;
	?GL_ZOOM_Y               -> 1;
	?GL_MAX_TEXTURE_UNITS    -> 1;

	%% Unknown length use 16 so far 
	%% the greatest known return value
	_Else -> 16                  
    end.

glGetLightivLen(Type) ->	    
    glGetLightfvLen(Type).
glGetLightfvLen(Type) ->
    case Type of
	?GL_AMBIENT  -> 4;
	?GL_DIFFUSE  -> 4;
	?GL_SPECULAR -> 4;
	?GL_POSITION -> 4;
	?GL_SPOT_DIRECTION -> 3;
	?GL_SPOT_EXPONENT -> 1;
	?GL_SPOT_CUTOFF -> 1;
	?GL_CONSTANT_ATTENUATION -> 1;
	?GL_LINEAR_ATTENUATION -> 1;
	?GL_QUADRATIC_ATTENUATION -> 1
    end.

glGetMaterialivLen(Type) ->
    glGetMaterialfvLen(Type).
glGetMaterialfvLen(Type) ->
    case Type of 
	?GL_AMBIENT -> 4;
	?GL_DIFFUSE -> 4;  
	?GL_SPECULAR -> 4;  
	?GL_EMISSION -> 4;	    
	?GL_SHININESS -> 1; 
	?GL_COLOR_INDEXES -> 3;
	_ -> 4
    end.
glGetTexGendvLen(Type) ->
    glGetTexGenivLen(Type).
glGetTexGenfvLen(Type) ->
    glGetTexGenivLen(Type).
glGetTexGenivLen(Type) ->
    case Type of
	?GL_TEXTURE_GEN_MODE -> 1;
	?GL_OBJECT_PLANE -> 4;
	?GL_EYE_PLANE -> 4;
	_ -> 4
    end.

glGetTexEnvfvLen(Type) ->
    glGetTexEnvivLen(Type).
glGetTexEnvivLen(Type) ->
    case Type of
	?GL_TEXTURE_ENV_MODE -> 1;
	?GL_TEXTURE_ENV_COLOR  -> 4;
	_ -> 4
    end.

glGetTexParameterivLen(Type) ->	    
    glGetTexParameterfvLen(Type).
glGetTexParameterfvLen(Type) ->
    case Type of
	?GL_TEXTURE_MAG_FILTER -> 1;
        ?GL_TEXTURE_MIN_FILTER -> 1;
	?GL_TEXTURE_WRAP_S -> 1;
	?GL_TEXTURE_WRAP_T -> 1;
	?GL_TEXTURE_BORDER_COLOR -> 4;
	?GL_TEXTURE_PRIORITY -> 1;
	?GL_TEXTURE_RESIDENT -> 1;
	_ -> 4
    end.

%% Backward compability
glBegin(Arg) -> 'begin'(Arg). 
glEnd() -> 'end'().    
swapBuffers() -> sdl_video:gl_swapBuffers().

%% @spec accum(Op::integer(), Value::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glAccum">External manpage: accum</a>
%% C-API func: void glAccum(GLenum op, GLfloat value)
accum(Op, Value) -> 
 cast(?glAccum, <<Op:32/?UN, Value:32/?FN>>).

%% @spec alphaFunc(Func::integer(), Ref::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glAlphaFunc">External manpage: alphaFunc</a>
%% C-API func: void glAlphaFunc(GLenum func, GLclampf ref)
alphaFunc(Func, Ref) -> 
 cast(?glAlphaFunc, <<Func:32/?UN, Ref:32/?FN>>).

%% @spec areTexturesResident(N::integer(), Textures::binary() | [integer()]) -> {bool(), [Residences::bool()]}
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glAreTexturesResident">External manpage: areTexturesResident</a>
%% C-API func: GLboolean glAreTexturesResident(GLsizei n,  const GLuint * textures, GLboolean * residences)
areTexturesResident(N, Textures) -> 
 NewTextures = if
	is_list(Textures) ; is_tuple(Textures) -> term2bin(Textures, N, ?GL_UNSIGNED_INT);
	is_binary(Textures) -> Textures;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Textures})
 end, 
 Bin = call(?glAreTexturesResident, [<<N:32/?SN>>,NewTextures]), 
 case Bin of 
	<<Ret:8/unsigned, Residences:N/binary-unit:?GL_BYTE_SIZE>> -> 
	 {Ret /= ?GL_FALSE, bin2list(N, ?GL_BYTE, Residences)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec arrayElement(I::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glArrayElement">External manpage: arrayElement</a>
%% C-API func: void glArrayElement(GLint i)
arrayElement(I) -> 
 cast(?glArrayElement, <<I:32/?SN>>).

%% @spec 'begin'(Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBegin">External manpage: 'begin'</a>
%% C-API func: void glBegin(GLenum mode)
'begin'(Mode) -> 
 cast(?glBegin, <<Mode:32/?UN>>).

%% @spec bindTexture(Target::integer(), Texture::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBindTexture">External manpage: bindTexture</a>
%% C-API func: void glBindTexture(GLenum target, GLuint texture)
bindTexture(Target, Texture) -> 
 cast(?glBindTexture, <<Target:32/?UN, Texture:32/?UN>>).

%% @spec bitmap(Width::integer(), Height::integer(), Xorig::float(), Yorig::float(), Xmove::float(), Ymove::float(), Bitmap::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBitmap">External manpage: bitmap</a>
%% C-API func: void glBitmap(GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove,  const GLubyte * bitmap)
bitmap(Width, Height, Xorig, Yorig, Xmove, Ymove, Bitmap) -> 
%% Maybe NULL or offset sometimes
 NewBitmap =
   if is_integer(Bitmap) -> Bitmap;
      true ->
        sdl:send_bin(Bitmap, ?MODULE, ?LINE),
       0
   end,
 cast(?glBitmap, [<<Width:32/?SN, Height:32/?SN, Xorig:32/?FN, Yorig:32/?FN, Xmove:32/?FN, Ymove:32/?FN, NewBitmap:32/?SN>>]).

%% @spec blendFunc(Sfactor::integer(), Dfactor::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBlendFunc">External manpage: blendFunc</a>
%% C-API func: void glBlendFunc(GLenum sfactor, GLenum dfactor)
blendFunc(Sfactor, Dfactor) -> 
 cast(?glBlendFunc, <<Sfactor:32/?UN, Dfactor:32/?UN>>).

%% @spec callList(List::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCallList">External manpage: callList</a>
%% C-API func: void glCallList(GLuint list)
callList(List) -> 
 cast(?glCallList, <<List:32/?UN>>).

%% @spec callLists(N::integer(), Type::integer(), Lists::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCallLists">External manpage: callLists</a>
%% C-API func: void glCallLists(GLsizei n, GLenum type,  const GLvoid * lists)
callLists(N, Type, Lists) -> 
 NewLists = if
	is_list(Lists) ; is_tuple(Lists) -> term2bin(Lists, N, Type);
	is_binary(Lists) -> Lists;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Lists})
 end, 
 cast(?glCallLists, [<<N:32/?SN, Type:32/?UN>>,NewLists]).

%% @spec clear(Mask::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glClear">External manpage: clear</a>
%% C-API func: void glClear(GLbitfield mask)
clear(Mask) -> 
 cast(?glClear, <<Mask:32/?UN>>).

%% @spec clearAccum(Red::float(), Green::float(), Blue::float(), Alpha::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glClearAccum">External manpage: clearAccum</a>
%% C-API func: void glClearAccum(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
clearAccum(Red, Green, Blue, Alpha) -> 
 cast(?glClearAccum, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN, Alpha:32/?FN>>).

%% @spec clearColor(Red::float(), Green::float(), Blue::float(), Alpha::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glClearColor">External manpage: clearColor</a>
%% C-API func: void glClearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)
clearColor(Red, Green, Blue, Alpha) -> 
 cast(?glClearColor, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN, Alpha:32/?FN>>).

%% @spec clearDepth(Depth::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glClearDepth">External manpage: clearDepth</a>
%% C-API func: void glClearDepth(GLclampd depth)
clearDepth(Depth) -> 
 cast(?glClearDepth, <<Depth:64/?FN>>).

%% @spec clearIndex(C::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glClearIndex">External manpage: clearIndex</a>
%% C-API func: void glClearIndex(GLfloat c)
clearIndex(C) -> 
 cast(?glClearIndex, <<C:32/?FN>>).

%% @spec clearStencil(S::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glClearStencil">External manpage: clearStencil</a>
%% C-API func: void glClearStencil(GLint s)
clearStencil(S) -> 
 cast(?glClearStencil, <<S:32/?SN>>).

%% @spec clipPlane(Plane::integer(), Equation::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glClipPlane">External manpage: clipPlane</a>
%% C-API func: void glClipPlane(GLenum plane,  const GLdouble * equation)
clipPlane(Plane, Equation) -> 
 NewEquation = if
	is_list(Equation) ; is_tuple(Equation) -> term2bin(Equation, 4, ?GL_DOUBLE);
	binary(Equation) -> Equation;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Equation})
 end, 
 cast(?glClipPlane, [<<Plane:32/?UN>>,NewEquation]).

%% @spec color3b(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3b</a>
%% C-API func: void glColor3b(GLbyte red, GLbyte green, GLbyte blue)
color3b(Red, Green, Blue) -> 
 cast(?glColor3bv, <<Red:8/signed, Green:8/signed, Blue:8/signed>>).

%% @spec color3bv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3bv</a>
%% C-API func: void glColor3bv( const GLbyte * v)
color3bv({V1,V2,V3}) -> 
 cast(?glColor3bv, <<V1:8/signed,V2:8/signed,V3:8/signed>>).

%% @spec color3d(Red::float(), Green::float(), Blue::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3d</a>
%% C-API func: void glColor3d(GLdouble red, GLdouble green, GLdouble blue)
color3d(Red, Green, Blue) -> 
 cast(?glColor3dv, <<Red:64/?FN, Green:64/?FN, Blue:64/?FN>>).

%% @spec color3dv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3dv</a>
%% C-API func: void glColor3dv( const GLdouble * v)
color3dv({V1,V2,V3}) -> 
 cast(?glColor3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% @spec color3f(Red::float(), Green::float(), Blue::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3f</a>
%% C-API func: void glColor3f(GLfloat red, GLfloat green, GLfloat blue)
color3f(Red, Green, Blue) -> 
 cast(?glColor3fv, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN>>).

%% @spec color3fv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3fv</a>
%% C-API func: void glColor3fv( const GLfloat * v)
color3fv({V1,V2,V3}) -> 
 cast(?glColor3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% @spec color3i(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3i</a>
%% C-API func: void glColor3i(GLint red, GLint green, GLint blue)
color3i(Red, Green, Blue) -> 
 cast(?glColor3iv, <<Red:32/?SN, Green:32/?SN, Blue:32/?SN>>).

%% @spec color3iv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3iv</a>
%% C-API func: void glColor3iv( const GLint * v)
color3iv({V1,V2,V3}) -> 
 cast(?glColor3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% @spec color3s(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3s</a>
%% C-API func: void glColor3s(GLshort red, GLshort green, GLshort blue)
color3s(Red, Green, Blue) -> 
 cast(?glColor3sv, <<Red:16/?SN, Green:16/?SN, Blue:16/?SN>>).

%% @spec color3sv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3sv</a>
%% C-API func: void glColor3sv( const GLshort * v)
color3sv({V1,V2,V3}) -> 
 cast(?glColor3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% @spec color3ub(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3ub</a>
%% C-API func: void glColor3ub(GLubyte red, GLubyte green, GLubyte blue)
color3ub(Red, Green, Blue) -> 
 cast(?glColor3ubv, <<Red:8/unsigned, Green:8/unsigned, Blue:8/unsigned>>).

%% @spec color3ubv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3ubv</a>
%% C-API func: void glColor3ubv( const GLubyte * v)
color3ubv({V1,V2,V3}) -> 
 cast(?glColor3ubv, <<V1:8/unsigned,V2:8/unsigned,V3:8/unsigned>>).

%% @spec color3ui(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3ui</a>
%% C-API func: void glColor3ui(GLuint red, GLuint green, GLuint blue)
color3ui(Red, Green, Blue) -> 
 cast(?glColor3uiv, <<Red:32/?UN, Green:32/?UN, Blue:32/?UN>>).

%% @spec color3uiv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3uiv</a>
%% C-API func: void glColor3uiv( const GLuint * v)
color3uiv({V1,V2,V3}) -> 
 cast(?glColor3uiv, <<V1:32/?UN,V2:32/?UN,V3:32/?UN>>).

%% @spec color3us(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3us</a>
%% C-API func: void glColor3us(GLushort red, GLushort green, GLushort blue)
color3us(Red, Green, Blue) -> 
 cast(?glColor3usv, <<Red:16/?UN, Green:16/?UN, Blue:16/?UN>>).

%% @spec color3usv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color3usv</a>
%% C-API func: void glColor3usv( const GLushort * v)
color3usv({V1,V2,V3}) -> 
 cast(?glColor3usv, <<V1:16/?UN,V2:16/?UN,V3:16/?UN>>).

%% @spec color4b(Red::integer(), Green::integer(), Blue::integer(), Alpha::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4b</a>
%% C-API func: void glColor4b(GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha)
color4b(Red, Green, Blue, Alpha) -> 
 cast(?glColor4bv, <<Red:8/signed, Green:8/signed, Blue:8/signed, Alpha:8/signed>>).

%% @spec color4bv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4bv</a>
%% C-API func: void glColor4bv( const GLbyte * v)
color4bv({V1,V2,V3,V4}) -> 
 cast(?glColor4bv, <<V1:8/signed,V2:8/signed,V3:8/signed,V4:8/signed>>).

%% @spec color4d(Red::float(), Green::float(), Blue::float(), Alpha::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4d</a>
%% C-API func: void glColor4d(GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha)
color4d(Red, Green, Blue, Alpha) -> 
 cast(?glColor4dv, <<Red:64/?FN, Green:64/?FN, Blue:64/?FN, Alpha:64/?FN>>).

%% @spec color4dv({V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4dv</a>
%% C-API func: void glColor4dv( const GLdouble * v)
color4dv({V1,V2,V3,V4}) -> 
 cast(?glColor4dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN,V4:64/?FN>>).

%% @spec color4f(Red::float(), Green::float(), Blue::float(), Alpha::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4f</a>
%% C-API func: void glColor4f(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
color4f(Red, Green, Blue, Alpha) -> 
 cast(?glColor4fv, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN, Alpha:32/?FN>>).

%% @spec color4fv({V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4fv</a>
%% C-API func: void glColor4fv( const GLfloat * v)
color4fv({V1,V2,V3,V4}) -> 
 cast(?glColor4fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN,V4:32/?FN>>).

%% @spec color4i(Red::integer(), Green::integer(), Blue::integer(), Alpha::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4i</a>
%% C-API func: void glColor4i(GLint red, GLint green, GLint blue, GLint alpha)
color4i(Red, Green, Blue, Alpha) -> 
 cast(?glColor4iv, <<Red:32/?SN, Green:32/?SN, Blue:32/?SN, Alpha:32/?SN>>).

%% @spec color4iv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4iv</a>
%% C-API func: void glColor4iv( const GLint * v)
color4iv({V1,V2,V3,V4}) -> 
 cast(?glColor4iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN,V4:32/?SN>>).

%% @spec color4s(Red::integer(), Green::integer(), Blue::integer(), Alpha::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4s</a>
%% C-API func: void glColor4s(GLshort red, GLshort green, GLshort blue, GLshort alpha)
color4s(Red, Green, Blue, Alpha) -> 
 cast(?glColor4sv, <<Red:16/?SN, Green:16/?SN, Blue:16/?SN, Alpha:16/?SN>>).

%% @spec color4sv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4sv</a>
%% C-API func: void glColor4sv( const GLshort * v)
color4sv({V1,V2,V3,V4}) -> 
 cast(?glColor4sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN,V4:16/?SN>>).

%% @spec color4ub(Red::integer(), Green::integer(), Blue::integer(), Alpha::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4ub</a>
%% C-API func: void glColor4ub(GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha)
color4ub(Red, Green, Blue, Alpha) -> 
 cast(?glColor4ubv, <<Red:8/unsigned, Green:8/unsigned, Blue:8/unsigned, Alpha:8/unsigned>>).

%% @spec color4ubv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4ubv</a>
%% C-API func: void glColor4ubv( const GLubyte * v)
color4ubv({V1,V2,V3,V4}) -> 
 cast(?glColor4ubv, <<V1:8/unsigned,V2:8/unsigned,V3:8/unsigned,V4:8/unsigned>>).

%% @spec color4ui(Red::integer(), Green::integer(), Blue::integer(), Alpha::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4ui</a>
%% C-API func: void glColor4ui(GLuint red, GLuint green, GLuint blue, GLuint alpha)
color4ui(Red, Green, Blue, Alpha) -> 
 cast(?glColor4uiv, <<Red:32/?UN, Green:32/?UN, Blue:32/?UN, Alpha:32/?UN>>).

%% @spec color4uiv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4uiv</a>
%% C-API func: void glColor4uiv( const GLuint * v)
color4uiv({V1,V2,V3,V4}) -> 
 cast(?glColor4uiv, <<V1:32/?UN,V2:32/?UN,V3:32/?UN,V4:32/?UN>>).

%% @spec color4us(Red::integer(), Green::integer(), Blue::integer(), Alpha::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4us</a>
%% C-API func: void glColor4us(GLushort red, GLushort green, GLushort blue, GLushort alpha)
color4us(Red, Green, Blue, Alpha) -> 
 cast(?glColor4usv, <<Red:16/?UN, Green:16/?UN, Blue:16/?UN, Alpha:16/?UN>>).

%% @spec color4usv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColor">External manpage: color4usv</a>
%% C-API func: void glColor4usv( const GLushort * v)
color4usv({V1,V2,V3,V4}) -> 
 cast(?glColor4usv, <<V1:16/?UN,V2:16/?UN,V3:16/?UN,V4:16/?UN>>).

%% @spec colorMask(Red::bool(), Green::bool(), Blue::bool(), Alpha::bool()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColorMask">External manpage: colorMask</a>
%% C-API func: void glColorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha)
colorMask(Red, Green, Blue, Alpha) -> 
 cast(?glColorMask, <<Red:8/unsigned, Green:8/unsigned, Blue:8/unsigned, Alpha:8/unsigned>>).

%% @spec colorMaterial(Face::integer(), Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColorMaterial">External manpage: colorMaterial</a>
%% C-API func: void glColorMaterial(GLenum face, GLenum mode)
colorMaterial(Face, Mode) -> 
 cast(?glColorMaterial, <<Face:32/?UN, Mode:32/?UN>>).

%% @spec colorPointer(Size::integer(), Type::integer(), Stride::integer(), Pointer::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColorPointer">External manpage: colorPointer</a>
%% C-API func: void glColorPointer(GLint size, GLenum type, GLsizei stride,  const GLvoid * pointer)
colorPointer(Size, Type, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glColorPointer, [<<Size:32/?SN, Type:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec copyPixels(X::integer(), Y::integer(), Width::integer(), Height::integer(), Type::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCopyPixels">External manpage: copyPixels</a>
%% C-API func: void glCopyPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum type)
copyPixels(X, Y, Width, Height, Type) -> 
 cast(?glCopyPixels, <<X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN, Type:32/?UN>>).

%% @spec copyTexImage1D(Target::integer(), Level::integer(), Internalformat::integer(), X::integer(), Y::integer(), Width::integer(), Border::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCopyTexImage1D">External manpage: copyTexImage1D</a>
%% C-API func: void glCopyTexImage1D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border)
copyTexImage1D(Target, Level, Internalformat, X, Y, Width, Border) -> 
 cast(?glCopyTexImage1D, <<Target:32/?UN, Level:32/?SN, Internalformat:32/?UN, X:32/?SN, Y:32/?SN, Width:32/?SN, Border:32/?SN>>).

%% @spec copyTexImage2D(Target::integer(), Level::integer(), Internalformat::integer(), X::integer(), Y::integer(), Width::integer(), Height::integer(), Border::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCopyTexImage2D">External manpage: copyTexImage2D</a>
%% C-API func: void glCopyTexImage2D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border)
copyTexImage2D(Target, Level, Internalformat, X, Y, Width, Height, Border) -> 
 cast(?glCopyTexImage2D, <<Target:32/?UN, Level:32/?SN, Internalformat:32/?UN, X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN, Border:32/?SN>>).

%% @spec copyTexSubImage1D(Target::integer(), Level::integer(), Xoffset::integer(), X::integer(), Y::integer(), Width::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCopyTexSubImage1D">External manpage: copyTexSubImage1D</a>
%% C-API func: void glCopyTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width)
copyTexSubImage1D(Target, Level, Xoffset, X, Y, Width) -> 
 cast(?glCopyTexSubImage1D, <<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, X:32/?SN, Y:32/?SN, Width:32/?SN>>).

%% @spec copyTexSubImage2D(Target::integer(), Level::integer(), Xoffset::integer(), Yoffset::integer(), X::integer(), Y::integer(), Width::integer(), Height::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCopyTexSubImage2D">External manpage: copyTexSubImage2D</a>
%% C-API func: void glCopyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)
copyTexSubImage2D(Target, Level, Xoffset, Yoffset, X, Y, Width, Height) -> 
 cast(?glCopyTexSubImage2D, <<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, Yoffset:32/?SN, X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN>>).

%% @spec cullFace(Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCullFace">External manpage: cullFace</a>
%% C-API func: void glCullFace(GLenum mode)
cullFace(Mode) -> 
 cast(?glCullFace, <<Mode:32/?UN>>).

%% @spec deleteLists(List::integer(), Range::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDeleteLists">External manpage: deleteLists</a>
%% C-API func: void glDeleteLists(GLuint list, GLsizei range)
deleteLists(List, Range) -> 
 cast(?glDeleteLists, <<List:32/?UN, Range:32/?SN>>).

%% @spec deleteTextures(N::integer(), Textures::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDeleteTextures">External manpage: deleteTextures</a>
%% C-API func: void glDeleteTextures(GLsizei n,  const GLuint * textures)
deleteTextures(N, Textures) -> 
 NewTextures = if
	is_list(Textures) ; is_tuple(Textures) -> term2bin(Textures, N, ?GL_UNSIGNED_INT);
	is_binary(Textures) -> Textures;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Textures})
 end, 
 cast(?glDeleteTextures, [<<N:32/?SN>>,NewTextures]).

%% @spec depthFunc(Func::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDepthFunc">External manpage: depthFunc</a>
%% C-API func: void glDepthFunc(GLenum func)
depthFunc(Func) -> 
 cast(?glDepthFunc, <<Func:32/?UN>>).

%% @spec depthMask(Flag::bool()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDepthMask">External manpage: depthMask</a>
%% C-API func: void glDepthMask(GLboolean flag)
depthMask(Flag) -> 
 cast(?glDepthMask, <<Flag:8/unsigned>>).

%% @spec depthRange(ZNear::float(), ZFar::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDepthRange">External manpage: depthRange</a>
%% C-API func: void glDepthRange(GLclampd zNear, GLclampd zFar)
depthRange(ZNear, ZFar) -> 
 cast(?glDepthRange, <<ZNear:64/?FN, ZFar:64/?FN>>).

%% @spec disable(Cap::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDisable">External manpage: disable</a>
%% C-API func: void glDisable(GLenum cap)
disable(Cap) -> 
 cast(?glDisable, <<Cap:32/?UN>>).

%% @spec disableClientState(Array::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDisableClientState">External manpage: disableClientState</a>
%% C-API func: void glDisableClientState(GLenum array)
disableClientState(Array) -> 
 cast(?glDisableClientState, <<Array:32/?UN>>).

%% @spec drawArrays(Mode::integer(), First::integer(), Count::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDrawArrays">External manpage: drawArrays</a>
%% C-API func: void glDrawArrays(GLenum mode, GLint first, GLsizei count)
drawArrays(Mode, First, Count) -> 
 cast(?glDrawArrays, <<Mode:32/?UN, First:32/?SN, Count:32/?SN>>).

%% @spec drawBuffer(Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDrawBuffer">External manpage: drawBuffer</a>
%% C-API func: void glDrawBuffer(GLenum mode)
drawBuffer(Mode) -> 
 cast(?glDrawBuffer, <<Mode:32/?UN>>).

%% @spec drawElements(Mode::integer(), Count::integer(), Type::integer(), Indices::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDrawElements">External manpage: drawElements</a>
%% C-API func: void glDrawElements(GLenum mode, GLsizei count, GLenum type,  const GLvoid * indices)
drawElements(Mode, Count, Type, Indices) -> 
%% Maybe NULL or offset sometimes2
 NewIndices = if is_integer(Indices) -> Indices; 
	is_list(Indices) ; is_tuple(Indices) -> sdl:send_bin(list_to_binary(term2bin(Indices, Count, Type)),?MODULE,?LINE),0;
	is_binary(Indices) -> sdl:send_bin(Indices, ?MODULE, ?LINE),0;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Indices})
 end, 
 cast(?glDrawElements, [<<Mode:32/?UN, Count:32/?SN, Type:32/?UN, NewIndices:32/?SN>>]).

%% @spec drawPixels(Width::integer(), Height::integer(), Format::integer(), Type::integer(), Pixels::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDrawPixels">External manpage: drawPixels</a>
%% C-API func: void glDrawPixels(GLsizei width, GLsizei height, GLenum format, GLenum type,  const GLvoid * pixels)
drawPixels(Width, Height, Format, Type, Pixels) -> 
%% Maybe NULL or offset sometimes
 NewPixels =
   if is_integer(Pixels) -> Pixels;
      true ->
        sdl:send_bin(Pixels, ?MODULE, ?LINE),
       0
   end,
 cast(?glDrawPixels, [<<Width:32/?SN, Height:32/?SN, Format:32/?UN, Type:32/?UN, NewPixels:32/?SN>>]).

%% @spec edgeFlag(Flag::bool()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEdgeFlag">External manpage: edgeFlag</a>
%% C-API func: void glEdgeFlag(GLboolean flag)
edgeFlag(Flag) -> 
 cast(?glEdgeFlag, <<Flag:8/unsigned>>).

%% @spec edgeFlagPointer(Stride::integer(), Pointer::binary() | [bool()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEdgeFlagPointer">External manpage: edgeFlagPointer</a>
%% C-API func: void glEdgeFlagPointer(GLsizei stride,  const GLboolean * pointer)
edgeFlagPointer(Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glEdgeFlagPointer, [<<Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec edgeFlagv(Flag::binary() | [bool()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEdgeFlagv">External manpage: edgeFlagv</a>
%% C-API func: void glEdgeFlagv( const GLboolean * flag)
edgeFlagv(Flag) -> 
 NewFlag = if
	is_list(Flag) ; is_tuple(Flag) -> term2bin(Flag, 1, ?GL_BYTE);
	binary(Flag) -> Flag;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Flag})
 end, 
 cast(?glEdgeFlagv, [ NewFlag]).

%% @spec enable(Cap::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEnable">External manpage: enable</a>
%% C-API func: void glEnable(GLenum cap)
enable(Cap) -> 
 cast(?glEnable, <<Cap:32/?UN>>).

%% @spec enableClientState(Array::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEnableClientState">External manpage: enableClientState</a>
%% C-API func: void glEnableClientState(GLenum array)
enableClientState(Array) -> 
 cast(?glEnableClientState, <<Array:32/?UN>>).

%% @spec 'end'() -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEnd">External manpage: 'end'</a>
%% C-API func: void glEnd()
'end'() -> 
 cast(?glEnd, []).

%% @spec endList() -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEndList">External manpage: endList</a>
%% C-API func: void glEndList()
endList() -> 
 cast(?glEndList, []).

%% @spec evalCoord1d(U::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalCoord">External manpage: evalCoord1d</a>
%% C-API func: void glEvalCoord1d(GLdouble u)
evalCoord1d(U) -> 
 cast(?glEvalCoord1dv, <<U:64/?FN>>).

%% @spec evalCoord1dv({U1::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalCoord">External manpage: evalCoord1dv</a>
%% C-API func: void glEvalCoord1dv( const GLdouble * u)
evalCoord1dv({U1}) -> 
 cast(?glEvalCoord1dv, <<U1:64/?FN>>).

%% @spec evalCoord1f(U::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalCoord">External manpage: evalCoord1f</a>
%% C-API func: void glEvalCoord1f(GLfloat u)
evalCoord1f(U) -> 
 cast(?glEvalCoord1fv, <<U:32/?FN>>).

%% @spec evalCoord1fv({U1::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalCoord">External manpage: evalCoord1fv</a>
%% C-API func: void glEvalCoord1fv( const GLfloat * u)
evalCoord1fv({U1}) -> 
 cast(?glEvalCoord1fv, <<U1:32/?FN>>).

%% @spec evalCoord2d(U::float(), V::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalCoord">External manpage: evalCoord2d</a>
%% C-API func: void glEvalCoord2d(GLdouble u, GLdouble v)
evalCoord2d(U, V) -> 
 cast(?glEvalCoord2dv, <<U:64/?FN, V:64/?FN>>).

%% @spec evalCoord2dv({U1::float(),U2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalCoord">External manpage: evalCoord2dv</a>
%% C-API func: void glEvalCoord2dv( const GLdouble * u)
evalCoord2dv({U1,U2}) -> 
 cast(?glEvalCoord2dv, <<U1:64/?FN,U2:64/?FN>>).

%% @spec evalCoord2f(U::float(), V::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalCoord">External manpage: evalCoord2f</a>
%% C-API func: void glEvalCoord2f(GLfloat u, GLfloat v)
evalCoord2f(U, V) -> 
 cast(?glEvalCoord2fv, <<U:32/?FN, V:32/?FN>>).

%% @spec evalCoord2fv({U1::float(),U2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalCoord">External manpage: evalCoord2fv</a>
%% C-API func: void glEvalCoord2fv( const GLfloat * u)
evalCoord2fv({U1,U2}) -> 
 cast(?glEvalCoord2fv, <<U1:32/?FN,U2:32/?FN>>).

%% @spec evalMesh1(Mode::integer(), I1::integer(), I2::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalMesh1">External manpage: evalMesh1</a>
%% C-API func: void glEvalMesh1(GLenum mode, GLint i1, GLint i2)
evalMesh1(Mode, I1, I2) -> 
 cast(?glEvalMesh1, <<Mode:32/?UN, I1:32/?SN, I2:32/?SN>>).

%% @spec evalMesh2(Mode::integer(), I1::integer(), I2::integer(), J1::integer(), J2::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalMesh2">External manpage: evalMesh2</a>
%% C-API func: void glEvalMesh2(GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2)
evalMesh2(Mode, I1, I2, J1, J2) -> 
 cast(?glEvalMesh2, <<Mode:32/?UN, I1:32/?SN, I2:32/?SN, J1:32/?SN, J2:32/?SN>>).

%% @spec evalPoint1(I::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalPoint1">External manpage: evalPoint1</a>
%% C-API func: void glEvalPoint1(GLint i)
evalPoint1(I) -> 
 cast(?glEvalPoint1, <<I:32/?SN>>).

%% @spec evalPoint2(I::integer(), J::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEvalPoint2">External manpage: evalPoint2</a>
%% C-API func: void glEvalPoint2(GLint i, GLint j)
evalPoint2(I, J) -> 
 cast(?glEvalPoint2, <<I:32/?SN, J:32/?SN>>).

%% @spec feedbackBuffer(Size::integer(), Type::integer(), Buffer::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFeedbackBuffer">External manpage: feedbackBuffer</a>
%% C-API func: void glFeedbackBuffer(GLsizei size, GLenum type, GLfloat * buffer)
feedbackBuffer(Size, Type, #sdlmem{bin=Buffer}) -> 
 sdl:send_bin(Buffer, ?MODULE, ?LINE),
 cast(?glFeedbackBuffer, <<Size:32/?SN, Type:32/?UN>>).

%% @spec finish() -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFinish">External manpage: finish</a>
%% C-API func: void glFinish()
finish() -> 
 cast(?glFinish, []).

%% @spec flush() -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFlush">External manpage: flush</a>
%% C-API func: void glFlush()
flush() -> 
 cast(?glFlush, []).

%% @spec fogf(Pname::integer(), Param::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFogf">External manpage: fogf</a>
%% C-API func: void glFogf(GLenum pname, GLfloat param)
fogf(Pname, Param) -> 
 cast(?glFogf, <<Pname:32/?UN, Param:32/?FN>>).

%% @spec fogfv(Pname::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFogfv">External manpage: fogfv</a>
%% C-API func: void glFogfv(GLenum pname,  const GLfloat * params)
fogfv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glFogfv, [<<Pname:32/?UN>>,NewParams]).

%% @spec fogi(Pname::integer(), Param::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFogi">External manpage: fogi</a>
%% C-API func: void glFogi(GLenum pname, GLint param)
fogi(Pname, Param) -> 
 cast(?glFogi, <<Pname:32/?UN, Param:32/?SN>>).

%% @spec fogiv(Pname::integer(), Params::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFogiv">External manpage: fogiv</a>
%% C-API func: void glFogiv(GLenum pname,  const GLint * params)
fogiv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glFogiv, [<<Pname:32/?UN>>,NewParams]).

%% @spec frontFace(Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFrontFace">External manpage: frontFace</a>
%% C-API func: void glFrontFace(GLenum mode)
frontFace(Mode) -> 
 cast(?glFrontFace, <<Mode:32/?UN>>).

%% @spec frustum(Left::float(), Right::float(), Bottom::float(), Top::float(), ZNear::float(), ZFar::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFrustum">External manpage: frustum</a>
%% C-API func: void glFrustum(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)
frustum(Left, Right, Bottom, Top, ZNear, ZFar) -> 
 cast(?glFrustum, <<Left:64/?FN, Right:64/?FN, Bottom:64/?FN, Top:64/?FN, ZNear:64/?FN, ZFar:64/?FN>>).

%% @spec genLists(Range::integer()) -> integer()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGenLists">External manpage: genLists</a>
%% C-API func: GLuint glGenLists(GLsizei range)
genLists(Range) -> 
 Bin = call(?glGenLists, <<Range:32/?SN>>), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec genTextures(N::integer()) -> Textures::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGenTextures">External manpage: genTextures</a>
%% C-API func: void glGenTextures(GLsizei n, GLuint * textures)
genTextures(N) -> 
 Bin = call(?glGenTextures, <<N:32/?SN>>), 
 case Bin of 
	<<Textures:N/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 bin2list(N, ?GL_UNSIGNED_INT, Textures);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getBooleanv(Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetBooleanv">External manpage: getBooleanv</a>
%% C-API func: void glGetBooleanv(GLenum pname, GLboolean * params)
getBooleanv(Pname) -> 
 Bin = call(?glGetBooleanv, <<Pname:32/?UN>>), 
 ParamsLen = glGetBooleanvLen(Pname),
 ParamsBump = 16 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_BYTE_SIZE,_:ParamsBump/binary-unit:?GL_BYTE_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_BYTE, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getClipPlane(Plane::integer()) -> Equation::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetClipPlane">External manpage: getClipPlane</a>
%% C-API func: void glGetClipPlane(GLenum plane, GLdouble * equation)
getClipPlane(Plane) -> 
 Bin = call(?glGetClipPlane, <<Plane:32/?UN>>), 
 case Bin of 
	<<Equation:4/binary-unit:?GL_DOUBLE_SIZE>> -> 
	 bin2list(4, ?GL_DOUBLE, Equation);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getDoublev(Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetDoublev">External manpage: getDoublev</a>
%% C-API func: void glGetDoublev(GLenum pname, GLdouble * params)
getDoublev(Pname) -> 
 Bin = call(?glGetDoublev, <<Pname:32/?UN>>), 
 ParamsLen = glGetDoublevLen(Pname),
 ParamsBump = 16 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_DOUBLE_SIZE,_:ParamsBump/binary-unit:?GL_DOUBLE_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_DOUBLE, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getError() -> integer()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetError">External manpage: getError</a>
%% C-API func: GLenum glGetError()
getError() -> 
 Bin = call(?glGetError, []), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getFloatv(Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetFloatv">External manpage: getFloatv</a>
%% C-API func: void glGetFloatv(GLenum pname, GLfloat * params)
getFloatv(Pname) -> 
 Bin = call(?glGetFloatv, <<Pname:32/?UN>>), 
 ParamsLen = glGetFloatvLen(Pname),
 ParamsBump = 16 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_FLOAT_SIZE,_:ParamsBump/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getIntegerv(Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetIntegerv">External manpage: getIntegerv</a>
%% C-API func: void glGetIntegerv(GLenum pname, GLint * params)
getIntegerv(Pname) -> 
 Bin = call(?glGetIntegerv, <<Pname:32/?UN>>), 
 ParamsLen = glGetIntegervLen(Pname),
 ParamsBump = 16 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_INT_SIZE,_:ParamsBump/binary-unit:?GL_INT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_INT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getLightfv(Light::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetLightfv">External manpage: getLightfv</a>
%% C-API func: void glGetLightfv(GLenum light, GLenum pname, GLfloat * params)
getLightfv(Light, Pname) -> 
 Bin = call(?glGetLightfv, <<Light:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetLightfvLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_FLOAT_SIZE,_:ParamsBump/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getLightiv(Light::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetLightiv">External manpage: getLightiv</a>
%% C-API func: void glGetLightiv(GLenum light, GLenum pname, GLint * params)
getLightiv(Light, Pname) -> 
 Bin = call(?glGetLightiv, <<Light:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetLightivLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_INT_SIZE,_:ParamsBump/binary-unit:?GL_INT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_INT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getMapdv(Target::integer(), Query::integer(), V::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetMapdv">External manpage: getMapdv</a>
%% C-API func: void glGetMapdv(GLenum target, GLenum query, GLdouble * v)
getMapdv(Target, Query, #sdlmem{bin=V}) -> 
 sdl:send_bin(V, ?MODULE, ?LINE),
 cast(?glGetMapdv, <<Target:32/?UN, Query:32/?UN>>).

%% @spec getMapfv(Target::integer(), Query::integer(), V::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetMapfv">External manpage: getMapfv</a>
%% C-API func: void glGetMapfv(GLenum target, GLenum query, GLfloat * v)
getMapfv(Target, Query, #sdlmem{bin=V}) -> 
 sdl:send_bin(V, ?MODULE, ?LINE),
 cast(?glGetMapfv, <<Target:32/?UN, Query:32/?UN>>).

%% @spec getMapiv(Target::integer(), Query::integer(), V::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetMapiv">External manpage: getMapiv</a>
%% C-API func: void glGetMapiv(GLenum target, GLenum query, GLint * v)
getMapiv(Target, Query, #sdlmem{bin=V}) -> 
 sdl:send_bin(V, ?MODULE, ?LINE),
 cast(?glGetMapiv, <<Target:32/?UN, Query:32/?UN>>).

%% @spec getMaterialfv(Face::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetMaterialfv">External manpage: getMaterialfv</a>
%% C-API func: void glGetMaterialfv(GLenum face, GLenum pname, GLfloat * params)
getMaterialfv(Face, Pname) -> 
 Bin = call(?glGetMaterialfv, <<Face:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetMaterialfvLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_FLOAT_SIZE,_:ParamsBump/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getMaterialiv(Face::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetMaterialiv">External manpage: getMaterialiv</a>
%% C-API func: void glGetMaterialiv(GLenum face, GLenum pname, GLint * params)
getMaterialiv(Face, Pname) -> 
 Bin = call(?glGetMaterialiv, <<Face:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetMaterialivLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_INT_SIZE,_:ParamsBump/binary-unit:?GL_INT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_INT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getPixelMapfv(Map::integer(), Values::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetPixelMapfv">External manpage: getPixelMapfv</a>
%% C-API func: void glGetPixelMapfv(GLenum map, GLfloat * values)
getPixelMapfv(Map, #sdlmem{bin=Values}) -> 
 sdl:send_bin(Values, ?MODULE, ?LINE),
 cast(?glGetPixelMapfv, <<Map:32/?UN>>).

%% @spec getPixelMapuiv(Map::integer(), Values::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetPixelMapuiv">External manpage: getPixelMapuiv</a>
%% C-API func: void glGetPixelMapuiv(GLenum map, GLuint * values)
getPixelMapuiv(Map, #sdlmem{bin=Values}) -> 
 sdl:send_bin(Values, ?MODULE, ?LINE),
 cast(?glGetPixelMapuiv, <<Map:32/?UN>>).

%% @spec getPixelMapusv(Map::integer(), Values::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetPixelMapusv">External manpage: getPixelMapusv</a>
%% C-API func: void glGetPixelMapusv(GLenum map, GLushort * values)
getPixelMapusv(Map, #sdlmem{bin=Values}) -> 
 sdl:send_bin(Values, ?MODULE, ?LINE),
 cast(?glGetPixelMapusv, <<Map:32/?UN>>).

%% @spec getPointerv(Pname::integer()) -> Params::sdlmem()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetPointerv">External manpage: getPointerv</a>
%% C-API func: void glGetPointerv(GLenum pname,  GLvoid* *params)
getPointerv(Pname) -> 
 Bin = call(?glGetPointerv, <<Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/big-unsigned>> -> 
	 erlang:fault({nyi, ?MODULE,?LINE});
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getPolygonStipple() -> Mask::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetPolygonStipple">External manpage: getPolygonStipple</a>
%% C-API func: void glGetPolygonStipple(GLubyte * mask)
getPolygonStipple() -> 
 Bin = call(?glGetPolygonStipple, []), 
 case Bin of 
	<<Mask:128/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 bin2list(128, ?GL_UNSIGNED_BYTE, Mask);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getString(Name::integer()) -> [integer()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetString">External manpage: getString</a>
%% C-API func: GLubyte* glGetString(GLenum name)
getString(Name) -> 
 Bin = call(?glGetString, <<Name:32/?UN>>), 
 case Bin of 
	Ret -> bin2list(undefined,?GL_UNSIGNED_BYTE,Ret);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getTexEnvfv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetTexEnvfv">External manpage: getTexEnvfv</a>
%% C-API func: void glGetTexEnvfv(GLenum target, GLenum pname, GLfloat * params)
getTexEnvfv(Target, Pname) -> 
 Bin = call(?glGetTexEnvfv, <<Target:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetTexEnvfvLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_FLOAT_SIZE,_:ParamsBump/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getTexEnviv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetTexEnviv">External manpage: getTexEnviv</a>
%% C-API func: void glGetTexEnviv(GLenum target, GLenum pname, GLint * params)
getTexEnviv(Target, Pname) -> 
 Bin = call(?glGetTexEnviv, <<Target:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetTexEnvivLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_INT_SIZE,_:ParamsBump/binary-unit:?GL_INT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_INT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getTexGendv(Coord::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetTexGendv">External manpage: getTexGendv</a>
%% C-API func: void glGetTexGendv(GLenum coord, GLenum pname, GLdouble * params)
getTexGendv(Coord, Pname) -> 
 Bin = call(?glGetTexGendv, <<Coord:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetTexGendvLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_DOUBLE_SIZE,_:ParamsBump/binary-unit:?GL_DOUBLE_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_DOUBLE, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getTexGenfv(Coord::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetTexGenfv">External manpage: getTexGenfv</a>
%% C-API func: void glGetTexGenfv(GLenum coord, GLenum pname, GLfloat * params)
getTexGenfv(Coord, Pname) -> 
 Bin = call(?glGetTexGenfv, <<Coord:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetTexGenfvLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_FLOAT_SIZE,_:ParamsBump/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getTexGeniv(Coord::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetTexGeniv">External manpage: getTexGeniv</a>
%% C-API func: void glGetTexGeniv(GLenum coord, GLenum pname, GLint * params)
getTexGeniv(Coord, Pname) -> 
 Bin = call(?glGetTexGeniv, <<Coord:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetTexGenivLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_INT_SIZE,_:ParamsBump/binary-unit:?GL_INT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_INT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getTexImage(Target::integer(), Level::integer(), Format::integer(), Type::integer(), Pixels::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetTexImage">External manpage: getTexImage</a>
%% C-API func: void glGetTexImage(GLenum target, GLint level, GLenum format, GLenum type, GLvoid * pixels)
getTexImage(Target, Level, Format, Type, #sdlmem{bin=Pixels}) -> 
 sdl:send_bin(Pixels, ?MODULE, ?LINE),
 cast(?glGetTexImage, <<Target:32/?UN, Level:32/?SN, Format:32/?UN, Type:32/?UN>>).

%% @spec getTexLevelParameterfv(Target::integer(), Level::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetTexLevelParameterfv">External manpage: getTexLevelParameterfv</a>
%% C-API func: void glGetTexLevelParameterfv(GLenum target, GLint level, GLenum pname, GLfloat * params)
getTexLevelParameterfv(Target, Level, Pname) -> 
 Bin = call(?glGetTexLevelParameterfv, <<Target:32/?UN, Level:32/?SN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?FN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getTexLevelParameteriv(Target::integer(), Level::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetTexLevelParameteriv">External manpage: getTexLevelParameteriv</a>
%% C-API func: void glGetTexLevelParameteriv(GLenum target, GLint level, GLenum pname, GLint * params)
getTexLevelParameteriv(Target, Level, Pname) -> 
 Bin = call(?glGetTexLevelParameteriv, <<Target:32/?UN, Level:32/?SN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getTexParameterfv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetTexParameterfv">External manpage: getTexParameterfv</a>
%% C-API func: void glGetTexParameterfv(GLenum target, GLenum pname, GLfloat * params)
getTexParameterfv(Target, Pname) -> 
 Bin = call(?glGetTexParameterfv, <<Target:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetTexParameterfvLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_FLOAT_SIZE,_:ParamsBump/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getTexParameteriv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetTexParameteriv">External manpage: getTexParameteriv</a>
%% C-API func: void glGetTexParameteriv(GLenum target, GLenum pname, GLint * params)
getTexParameteriv(Target, Pname) -> 
 Bin = call(?glGetTexParameteriv, <<Target:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetTexParameterivLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_INT_SIZE,_:ParamsBump/binary-unit:?GL_INT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_INT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec hint(Target::integer(), Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glHint">External manpage: hint</a>
%% C-API func: void glHint(GLenum target, GLenum mode)
hint(Target, Mode) -> 
 cast(?glHint, <<Target:32/?UN, Mode:32/?UN>>).

%% @spec indexMask(Mask::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexMask">External manpage: indexMask</a>
%% C-API func: void glIndexMask(GLuint mask)
indexMask(Mask) -> 
 cast(?glIndexMask, <<Mask:32/?UN>>).

%% @spec indexPointer(Type::integer(), Stride::integer(), Pointer::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexPointer">External manpage: indexPointer</a>
%% C-API func: void glIndexPointer(GLenum type, GLsizei stride,  const GLvoid * pointer)
indexPointer(Type, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glIndexPointer, [<<Type:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec indexd(C::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexd">External manpage: indexd</a>
%% C-API func: void glIndexd(GLdouble c)
indexd(C) -> 
 cast(?glIndexd, <<C:64/?FN>>).

%% @spec indexdv(C::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexdv">External manpage: indexdv</a>
%% C-API func: void glIndexdv( const GLdouble * c)
indexdv(C) -> 
 NewC = if
	is_list(C) ; is_tuple(C) -> term2bin(C, 1, ?GL_DOUBLE);
	binary(C) -> C;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, C})
 end, 
 cast(?glIndexdv, [ NewC]).

%% @spec indexf(C::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexf">External manpage: indexf</a>
%% C-API func: void glIndexf(GLfloat c)
indexf(C) -> 
 cast(?glIndexf, <<C:32/?FN>>).

%% @spec indexfv(C::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexfv">External manpage: indexfv</a>
%% C-API func: void glIndexfv( const GLfloat * c)
indexfv(C) -> 
 NewC = if
	is_list(C) ; is_tuple(C) -> term2bin(C, 1, ?GL_FLOAT);
	binary(C) -> C;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, C})
 end, 
 cast(?glIndexfv, [ NewC]).

%% @spec indexi(C::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexi">External manpage: indexi</a>
%% C-API func: void glIndexi(GLint c)
indexi(C) -> 
 cast(?glIndexi, <<C:32/?SN>>).

%% @spec indexiv(C::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexiv">External manpage: indexiv</a>
%% C-API func: void glIndexiv( const GLint * c)
indexiv(C) -> 
 NewC = if
	is_list(C) ; is_tuple(C) -> term2bin(C, 1, ?GL_INT);
	binary(C) -> C;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, C})
 end, 
 cast(?glIndexiv, [ NewC]).

%% @spec indexs(C::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexs">External manpage: indexs</a>
%% C-API func: void glIndexs(GLshort c)
indexs(C) -> 
 cast(?glIndexs, <<C:16/?SN>>).

%% @spec indexsv(C::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexsv">External manpage: indexsv</a>
%% C-API func: void glIndexsv( const GLshort * c)
indexsv(C) -> 
 NewC = if
	is_list(C) ; is_tuple(C) -> term2bin(C, 1, ?GL_SHORT);
	binary(C) -> C;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, C})
 end, 
 cast(?glIndexsv, [ NewC]).

%% @spec indexub(C::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexub">External manpage: indexub</a>
%% C-API func: void glIndexub(GLubyte c)
indexub(C) -> 
 cast(?glIndexub, <<C:8/unsigned>>).

%% @spec indexubv(C::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIndexubv">External manpage: indexubv</a>
%% C-API func: void glIndexubv( const GLubyte * c)
indexubv(C) -> 
 NewC = if
	is_list(C) ; is_tuple(C) -> term2bin(C, 1, ?GL_UNSIGNED_BYTE);
	binary(C) -> C;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, C})
 end, 
 cast(?glIndexubv, [ NewC]).

%% @spec initNames() -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glInitNames">External manpage: initNames</a>
%% C-API func: void glInitNames()
initNames() -> 
 cast(?glInitNames, []).

%% @spec interleavedArrays(Format::integer(), Stride::integer(), Pointer::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glInterleavedArrays">External manpage: interleavedArrays</a>
%% C-API func: void glInterleavedArrays(GLenum format, GLsizei stride,  const GLvoid * pointer)
interleavedArrays(Format, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glInterleavedArrays, [<<Format:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec isEnabled(Cap::integer()) -> bool()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIsEnabled">External manpage: isEnabled</a>
%% C-API func: GLboolean glIsEnabled(GLenum cap)
isEnabled(Cap) -> 
 Bin = call(?glIsEnabled, <<Cap:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec isList(List::integer()) -> bool()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIsList">External manpage: isList</a>
%% C-API func: GLboolean glIsList(GLuint list)
isList(List) -> 
 Bin = call(?glIsList, <<List:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec isTexture(Texture::integer()) -> bool()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIsTexture">External manpage: isTexture</a>
%% C-API func: GLboolean glIsTexture(GLuint texture)
isTexture(Texture) -> 
 Bin = call(?glIsTexture, <<Texture:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec lightModelf(Pname::integer(), Param::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLightModelf">External manpage: lightModelf</a>
%% C-API func: void glLightModelf(GLenum pname, GLfloat param)
lightModelf(Pname, Param) -> 
 cast(?glLightModelf, <<Pname:32/?UN, Param:32/?FN>>).

%% @spec lightModelfv(Pname::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLightModelfv">External manpage: lightModelfv</a>
%% C-API func: void glLightModelfv(GLenum pname,  const GLfloat * params)
lightModelfv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glLightModelfv, [<<Pname:32/?UN>>,NewParams]).

%% @spec lightModeli(Pname::integer(), Param::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLightModeli">External manpage: lightModeli</a>
%% C-API func: void glLightModeli(GLenum pname, GLint param)
lightModeli(Pname, Param) -> 
 cast(?glLightModeli, <<Pname:32/?UN, Param:32/?SN>>).

%% @spec lightModeliv(Pname::integer(), Params::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLightModeliv">External manpage: lightModeliv</a>
%% C-API func: void glLightModeliv(GLenum pname,  const GLint * params)
lightModeliv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glLightModeliv, [<<Pname:32/?UN>>,NewParams]).

%% @spec lightf(Light::integer(), Pname::integer(), Param::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLightf">External manpage: lightf</a>
%% C-API func: void glLightf(GLenum light, GLenum pname, GLfloat param)
lightf(Light, Pname, Param) -> 
 cast(?glLightf, <<Light:32/?UN, Pname:32/?UN, Param:32/?FN>>).

%% @spec lightfv(Light::integer(), Pname::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLightfv">External manpage: lightfv</a>
%% C-API func: void glLightfv(GLenum light, GLenum pname,  const GLfloat * params)
lightfv(Light, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glLightfv, [<<Light:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec lighti(Light::integer(), Pname::integer(), Param::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLighti">External manpage: lighti</a>
%% C-API func: void glLighti(GLenum light, GLenum pname, GLint param)
lighti(Light, Pname, Param) -> 
 cast(?glLighti, <<Light:32/?UN, Pname:32/?UN, Param:32/?SN>>).

%% @spec lightiv(Light::integer(), Pname::integer(), Params::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLightiv">External manpage: lightiv</a>
%% C-API func: void glLightiv(GLenum light, GLenum pname,  const GLint * params)
lightiv(Light, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glLightiv, [<<Light:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec lineStipple(Factor::integer(), Pattern::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLineStipple">External manpage: lineStipple</a>
%% C-API func: void glLineStipple(GLint factor, GLushort pattern)
lineStipple(Factor, Pattern) -> 
 cast(?glLineStipple, <<Factor:32/?SN, Pattern:16/?UN>>).

%% @spec lineWidth(Width::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLineWidth">External manpage: lineWidth</a>
%% C-API func: void glLineWidth(GLfloat width)
lineWidth(Width) -> 
 cast(?glLineWidth, <<Width:32/?FN>>).

%% @spec listBase(Base::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glListBase">External manpage: listBase</a>
%% C-API func: void glListBase(GLuint base)
listBase(Base) -> 
 cast(?glListBase, <<Base:32/?UN>>).

%% @spec loadIdentity() -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLoadIdentity">External manpage: loadIdentity</a>
%% C-API func: void glLoadIdentity()
loadIdentity() -> 
 cast(?glLoadIdentity, []).

%% @spec loadMatrixd(M::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLoadMatrixd">External manpage: loadMatrixd</a>
%% C-API func: void glLoadMatrixd( const GLdouble * m)
loadMatrixd(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_DOUBLE);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glLoadMatrixd, [ NewM]).

%% @spec loadMatrixf(M::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLoadMatrixf">External manpage: loadMatrixf</a>
%% C-API func: void glLoadMatrixf( const GLfloat * m)
loadMatrixf(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_FLOAT);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glLoadMatrixf, [ NewM]).

%% @spec loadName(Name::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLoadName">External manpage: loadName</a>
%% C-API func: void glLoadName(GLuint name)
loadName(Name) -> 
 cast(?glLoadName, <<Name:32/?UN>>).

%% @spec logicOp(Opcode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLogicOp">External manpage: logicOp</a>
%% C-API func: void glLogicOp(GLenum opcode)
logicOp(Opcode) -> 
 cast(?glLogicOp, <<Opcode:32/?UN>>).

%% @spec map1d(Target::integer(), U1::float(), U2::float(), Stride::integer(), Order::integer(), Points::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMap1d">External manpage: map1d</a>
%% C-API func: void glMap1d(GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order,  const GLdouble * points)
map1d(Target, U1, U2, Stride, Order, Points) -> 
 NewPoints = if
	is_list(Points) ->  PointsLen = length(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_DOUBLE)];
	is_tuple(Points) ->  PointsLen = size(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_DOUBLE)];
	is_binary(Points) -> [<<(size(Points) div 8):32/native>>,Points];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Points})
 end, 
 cast(?glMap1d, [<<Target:32/?UN, U1:64/?FN, U2:64/?FN, Stride:32/?SN, Order:32/?SN>>,NewPoints]).

%% @spec map1f(Target::integer(), U1::float(), U2::float(), Stride::integer(), Order::integer(), Points::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMap1f">External manpage: map1f</a>
%% C-API func: void glMap1f(GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order,  const GLfloat * points)
map1f(Target, U1, U2, Stride, Order, Points) -> 
 NewPoints = if
	is_list(Points) ->  PointsLen = length(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_FLOAT)];
	is_tuple(Points) ->  PointsLen = size(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_FLOAT)];
	is_binary(Points) -> [<<(size(Points) div 4):32/native>>,Points];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Points})
 end, 
 cast(?glMap1f, [<<Target:32/?UN, U1:32/?FN, U2:32/?FN, Stride:32/?SN, Order:32/?SN>>,NewPoints]).

%% @spec map2d(Target::integer(), U1::float(), U2::float(), Ustride::integer(), Uorder::integer(), V1::float(), V2::float(), Vstride::integer(), Vorder::integer(), Points::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMap2d">External manpage: map2d</a>
%% C-API func: void glMap2d(GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder,  const GLdouble * points)
map2d(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 
 NewPoints = if
	is_list(Points) ->  PointsLen = length(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_DOUBLE)];
	is_tuple(Points) ->  PointsLen = size(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_DOUBLE)];
	is_binary(Points) -> [<<(size(Points) div 8):32/native>>,Points];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Points})
 end, 
 cast(?glMap2d, [<<Target:32/?UN, U1:64/?FN, U2:64/?FN, Ustride:32/?SN, Uorder:32/?SN, V1:64/?FN, V2:64/?FN, Vstride:32/?SN, Vorder:32/?SN>>,NewPoints]).

%% @spec map2f(Target::integer(), U1::float(), U2::float(), Ustride::integer(), Uorder::integer(), V1::float(), V2::float(), Vstride::integer(), Vorder::integer(), Points::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMap2f">External manpage: map2f</a>
%% C-API func: void glMap2f(GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder,  const GLfloat * points)
map2f(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 
 NewPoints = if
	is_list(Points) ->  PointsLen = length(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_FLOAT)];
	is_tuple(Points) ->  PointsLen = size(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_FLOAT)];
	is_binary(Points) -> [<<(size(Points) div 4):32/native>>,Points];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Points})
 end, 
 cast(?glMap2f, [<<Target:32/?UN, U1:32/?FN, U2:32/?FN, Ustride:32/?SN, Uorder:32/?SN, V1:32/?FN, V2:32/?FN, Vstride:32/?SN, Vorder:32/?SN>>,NewPoints]).

%% @spec mapGrid1d(Un::integer(), U1::float(), U2::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMapGrid1d">External manpage: mapGrid1d</a>
%% C-API func: void glMapGrid1d(GLint un, GLdouble u1, GLdouble u2)
mapGrid1d(Un, U1, U2) -> 
 cast(?glMapGrid1d, <<Un:32/?SN, U1:64/?FN, U2:64/?FN>>).

%% @spec mapGrid1f(Un::integer(), U1::float(), U2::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMapGrid1f">External manpage: mapGrid1f</a>
%% C-API func: void glMapGrid1f(GLint un, GLfloat u1, GLfloat u2)
mapGrid1f(Un, U1, U2) -> 
 cast(?glMapGrid1f, <<Un:32/?SN, U1:32/?FN, U2:32/?FN>>).

%% @spec mapGrid2d(Un::integer(), U1::float(), U2::float(), Vn::integer(), V1::float(), V2::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMapGrid2d">External manpage: mapGrid2d</a>
%% C-API func: void glMapGrid2d(GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, GLdouble v2)
mapGrid2d(Un, U1, U2, Vn, V1, V2) -> 
 cast(?glMapGrid2d, <<Un:32/?SN, U1:64/?FN, U2:64/?FN, Vn:32/?SN, V1:64/?FN, V2:64/?FN>>).

%% @spec mapGrid2f(Un::integer(), U1::float(), U2::float(), Vn::integer(), V1::float(), V2::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMapGrid2f">External manpage: mapGrid2f</a>
%% C-API func: void glMapGrid2f(GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2)
mapGrid2f(Un, U1, U2, Vn, V1, V2) -> 
 cast(?glMapGrid2f, <<Un:32/?SN, U1:32/?FN, U2:32/?FN, Vn:32/?SN, V1:32/?FN, V2:32/?FN>>).

%% @spec materialf(Face::integer(), Pname::integer(), Param::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMaterialf">External manpage: materialf</a>
%% C-API func: void glMaterialf(GLenum face, GLenum pname, GLfloat param)
materialf(Face, Pname, Param) -> 
 cast(?glMaterialf, <<Face:32/?UN, Pname:32/?UN, Param:32/?FN>>).

%% @spec materialfv(Face::integer(), Pname::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMaterialfv">External manpage: materialfv</a>
%% C-API func: void glMaterialfv(GLenum face, GLenum pname,  const GLfloat * params)
materialfv(Face, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glMaterialfv, [<<Face:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec materiali(Face::integer(), Pname::integer(), Param::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMateriali">External manpage: materiali</a>
%% C-API func: void glMateriali(GLenum face, GLenum pname, GLint param)
materiali(Face, Pname, Param) -> 
 cast(?glMateriali, <<Face:32/?UN, Pname:32/?UN, Param:32/?SN>>).

%% @spec materialiv(Face::integer(), Pname::integer(), Params::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMaterialiv">External manpage: materialiv</a>
%% C-API func: void glMaterialiv(GLenum face, GLenum pname,  const GLint * params)
materialiv(Face, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glMaterialiv, [<<Face:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec matrixMode(Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMatrixMode">External manpage: matrixMode</a>
%% C-API func: void glMatrixMode(GLenum mode)
matrixMode(Mode) -> 
 cast(?glMatrixMode, <<Mode:32/?UN>>).

%% @spec multMatrixd(M::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultMatrixd">External manpage: multMatrixd</a>
%% C-API func: void glMultMatrixd( const GLdouble * m)
multMatrixd(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_DOUBLE);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glMultMatrixd, [ NewM]).

%% @spec multMatrixf(M::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultMatrixf">External manpage: multMatrixf</a>
%% C-API func: void glMultMatrixf( const GLfloat * m)
multMatrixf(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_FLOAT);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glMultMatrixf, [ NewM]).

%% @spec newList(List::integer(), Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNewList">External manpage: newList</a>
%% C-API func: void glNewList(GLuint list, GLenum mode)
newList(List, Mode) -> 
 cast(?glNewList, <<List:32/?UN, Mode:32/?UN>>).

%% @spec normal3b(Nx::integer(), Ny::integer(), Nz::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNormal">External manpage: normal3b</a>
%% C-API func: void glNormal3b(GLbyte nx, GLbyte ny, GLbyte nz)
normal3b(Nx, Ny, Nz) -> 
 cast(?glNormal3bv, <<Nx:8/signed, Ny:8/signed, Nz:8/signed>>).

%% @spec normal3bv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNormal">External manpage: normal3bv</a>
%% C-API func: void glNormal3bv( const GLbyte * v)
normal3bv({V1,V2,V3}) -> 
 cast(?glNormal3bv, <<V1:8/signed,V2:8/signed,V3:8/signed>>).

%% @spec normal3d(Nx::float(), Ny::float(), Nz::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNormal">External manpage: normal3d</a>
%% C-API func: void glNormal3d(GLdouble nx, GLdouble ny, GLdouble nz)
normal3d(Nx, Ny, Nz) -> 
 cast(?glNormal3dv, <<Nx:64/?FN, Ny:64/?FN, Nz:64/?FN>>).

%% @spec normal3dv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNormal">External manpage: normal3dv</a>
%% C-API func: void glNormal3dv( const GLdouble * v)
normal3dv({V1,V2,V3}) -> 
 cast(?glNormal3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% @spec normal3f(Nx::float(), Ny::float(), Nz::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNormal">External manpage: normal3f</a>
%% C-API func: void glNormal3f(GLfloat nx, GLfloat ny, GLfloat nz)
normal3f(Nx, Ny, Nz) -> 
 cast(?glNormal3fv, <<Nx:32/?FN, Ny:32/?FN, Nz:32/?FN>>).

%% @spec normal3fv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNormal">External manpage: normal3fv</a>
%% C-API func: void glNormal3fv( const GLfloat * v)
normal3fv({V1,V2,V3}) -> 
 cast(?glNormal3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% @spec normal3i(Nx::integer(), Ny::integer(), Nz::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNormal">External manpage: normal3i</a>
%% C-API func: void glNormal3i(GLint nx, GLint ny, GLint nz)
normal3i(Nx, Ny, Nz) -> 
 cast(?glNormal3iv, <<Nx:32/?SN, Ny:32/?SN, Nz:32/?SN>>).

%% @spec normal3iv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNormal">External manpage: normal3iv</a>
%% C-API func: void glNormal3iv( const GLint * v)
normal3iv({V1,V2,V3}) -> 
 cast(?glNormal3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% @spec normal3s(Nx::integer(), Ny::integer(), Nz::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNormal">External manpage: normal3s</a>
%% C-API func: void glNormal3s(GLshort nx, GLshort ny, GLshort nz)
normal3s(Nx, Ny, Nz) -> 
 cast(?glNormal3sv, <<Nx:16/?SN, Ny:16/?SN, Nz:16/?SN>>).

%% @spec normal3sv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNormal">External manpage: normal3sv</a>
%% C-API func: void glNormal3sv( const GLshort * v)
normal3sv({V1,V2,V3}) -> 
 cast(?glNormal3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% @spec normalPointer(Type::integer(), Stride::integer(), Pointer::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glNormalPointer">External manpage: normalPointer</a>
%% C-API func: void glNormalPointer(GLenum type, GLsizei stride,  const GLvoid * pointer)
normalPointer(Type, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glNormalPointer, [<<Type:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec ortho(Left::float(), Right::float(), Bottom::float(), Top::float(), ZNear::float(), ZFar::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glOrtho">External manpage: ortho</a>
%% C-API func: void glOrtho(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)
ortho(Left, Right, Bottom, Top, ZNear, ZFar) -> 
 cast(?glOrtho, <<Left:64/?FN, Right:64/?FN, Bottom:64/?FN, Top:64/?FN, ZNear:64/?FN, ZFar:64/?FN>>).

%% @spec passThrough(Token::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPassThrough">External manpage: passThrough</a>
%% C-API func: void glPassThrough(GLfloat token)
passThrough(Token) -> 
 cast(?glPassThrough, <<Token:32/?FN>>).

%% @spec pixelMapfv(Map::integer(), Mapsize::integer(), Values::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPixelMapfv">External manpage: pixelMapfv</a>
%% C-API func: void glPixelMapfv(GLenum map, GLint mapsize,  const GLfloat * values)
pixelMapfv(Map, Mapsize, Values) -> 
 NewValues = if
	is_list(Values) ; is_tuple(Values) -> term2bin(Values, Mapsize, ?GL_FLOAT);
	is_binary(Values) -> Values;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Values})
 end, 
 cast(?glPixelMapfv, [<<Map:32/?UN, Mapsize:32/?SN>>,NewValues]).

%% @spec pixelMapuiv(Map::integer(), Mapsize::integer(), Values::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPixelMapuiv">External manpage: pixelMapuiv</a>
%% C-API func: void glPixelMapuiv(GLenum map, GLint mapsize,  const GLuint * values)
pixelMapuiv(Map, Mapsize, Values) -> 
 NewValues = if
	is_list(Values) ; is_tuple(Values) -> term2bin(Values, Mapsize, ?GL_UNSIGNED_INT);
	is_binary(Values) -> Values;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Values})
 end, 
 cast(?glPixelMapuiv, [<<Map:32/?UN, Mapsize:32/?SN>>,NewValues]).

%% @spec pixelMapusv(Map::integer(), Mapsize::integer(), Values::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPixelMapusv">External manpage: pixelMapusv</a>
%% C-API func: void glPixelMapusv(GLenum map, GLint mapsize,  const GLushort * values)
pixelMapusv(Map, Mapsize, Values) -> 
 NewValues = if
	is_list(Values) ; is_tuple(Values) -> term2bin(Values, Mapsize, ?GL_UNSIGNED_SHORT);
	is_binary(Values) -> Values;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Values})
 end, 
 cast(?glPixelMapusv, [<<Map:32/?UN, Mapsize:32/?SN>>,NewValues]).

%% @spec pixelStoref(Pname::integer(), Param::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPixelStoref">External manpage: pixelStoref</a>
%% C-API func: void glPixelStoref(GLenum pname, GLfloat param)
pixelStoref(Pname, Param) -> 
 cast(?glPixelStoref, <<Pname:32/?UN, Param:32/?FN>>).

%% @spec pixelStorei(Pname::integer(), Param::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPixelStorei">External manpage: pixelStorei</a>
%% C-API func: void glPixelStorei(GLenum pname, GLint param)
pixelStorei(Pname, Param) -> 
 cast(?glPixelStorei, <<Pname:32/?UN, Param:32/?SN>>).

%% @spec pixelTransferf(Pname::integer(), Param::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPixelTransferf">External manpage: pixelTransferf</a>
%% C-API func: void glPixelTransferf(GLenum pname, GLfloat param)
pixelTransferf(Pname, Param) -> 
 cast(?glPixelTransferf, <<Pname:32/?UN, Param:32/?FN>>).

%% @spec pixelTransferi(Pname::integer(), Param::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPixelTransferi">External manpage: pixelTransferi</a>
%% C-API func: void glPixelTransferi(GLenum pname, GLint param)
pixelTransferi(Pname, Param) -> 
 cast(?glPixelTransferi, <<Pname:32/?UN, Param:32/?SN>>).

%% @spec pixelZoom(Xfactor::float(), Yfactor::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPixelZoom">External manpage: pixelZoom</a>
%% C-API func: void glPixelZoom(GLfloat xfactor, GLfloat yfactor)
pixelZoom(Xfactor, Yfactor) -> 
 cast(?glPixelZoom, <<Xfactor:32/?FN, Yfactor:32/?FN>>).

%% @spec pointSize(Size::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPointSize">External manpage: pointSize</a>
%% C-API func: void glPointSize(GLfloat size)
pointSize(Size) -> 
 cast(?glPointSize, <<Size:32/?FN>>).

%% @spec polygonMode(Face::integer(), Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPolygonMode">External manpage: polygonMode</a>
%% C-API func: void glPolygonMode(GLenum face, GLenum mode)
polygonMode(Face, Mode) -> 
 cast(?glPolygonMode, <<Face:32/?UN, Mode:32/?UN>>).

%% @spec polygonOffset(Factor::float(), Units::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPolygonOffset">External manpage: polygonOffset</a>
%% C-API func: void glPolygonOffset(GLfloat factor, GLfloat units)
polygonOffset(Factor, Units) -> 
 cast(?glPolygonOffset, <<Factor:32/?FN, Units:32/?FN>>).

%% @spec polygonStipple(Mask::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPolygonStipple">External manpage: polygonStipple</a>
%% C-API func: void glPolygonStipple( const GLubyte * mask)
polygonStipple(Mask) -> 
 NewMask = if
	is_list(Mask) ; is_tuple(Mask) -> term2bin(Mask, 128, ?GL_UNSIGNED_BYTE);
	binary(Mask) -> Mask;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Mask})
 end, 
 cast(?glPolygonStipple, [ NewMask]).

%% @spec popAttrib() -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPopAttrib">External manpage: popAttrib</a>
%% C-API func: void glPopAttrib()
popAttrib() -> 
 cast(?glPopAttrib, []).

%% @spec popClientAttrib() -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPopClientAttrib">External manpage: popClientAttrib</a>
%% C-API func: void glPopClientAttrib()
popClientAttrib() -> 
 cast(?glPopClientAttrib, []).

%% @spec popMatrix() -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPopMatrix">External manpage: popMatrix</a>
%% C-API func: void glPopMatrix()
popMatrix() -> 
 cast(?glPopMatrix, []).

%% @spec popName() -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPopName">External manpage: popName</a>
%% C-API func: void glPopName()
popName() -> 
 cast(?glPopName, []).

%% @spec prioritizeTextures(N::integer(), Textures::binary() | [integer()], Priorities::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPrioritizeTextures">External manpage: prioritizeTextures</a>
%% C-API func: void glPrioritizeTextures(GLsizei n,  const GLuint * textures,  const GLclampf * priorities)
prioritizeTextures(N, Textures, Priorities) -> 
 NewTextures = if
	is_list(Textures) ; is_tuple(Textures) -> term2bin(Textures, N, ?GL_UNSIGNED_INT);
	is_binary(Textures) -> Textures;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Textures})
 end, 
 NewPriorities = if
	is_list(Priorities) ; is_tuple(Priorities) -> term2bin(Priorities, N, ?GL_FLOAT);
	is_binary(Priorities) -> Priorities;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Priorities})
 end, 
 cast(?glPrioritizeTextures, [<<N:32/?SN>>,NewTextures, NewPriorities]).

%% @spec pushAttrib(Mask::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPushAttrib">External manpage: pushAttrib</a>
%% C-API func: void glPushAttrib(GLbitfield mask)
pushAttrib(Mask) -> 
 cast(?glPushAttrib, <<Mask:32/?UN>>).

%% @spec pushClientAttrib(Mask::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPushClientAttrib">External manpage: pushClientAttrib</a>
%% C-API func: void glPushClientAttrib(GLbitfield mask)
pushClientAttrib(Mask) -> 
 cast(?glPushClientAttrib, <<Mask:32/?UN>>).

%% @spec pushMatrix() -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPushMatrix">External manpage: pushMatrix</a>
%% C-API func: void glPushMatrix()
pushMatrix() -> 
 cast(?glPushMatrix, []).

%% @spec pushName(Name::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPushName">External manpage: pushName</a>
%% C-API func: void glPushName(GLuint name)
pushName(Name) -> 
 cast(?glPushName, <<Name:32/?UN>>).

%% @spec rasterPos2d(X::float(), Y::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos2d</a>
%% C-API func: void glRasterPos2d(GLdouble x, GLdouble y)
rasterPos2d(X, Y) -> 
 cast(?glRasterPos2dv, <<X:64/?FN, Y:64/?FN>>).

%% @spec rasterPos2dv({V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos2dv</a>
%% C-API func: void glRasterPos2dv( const GLdouble * v)
rasterPos2dv({V1,V2}) -> 
 cast(?glRasterPos2dv, <<V1:64/?FN,V2:64/?FN>>).

%% @spec rasterPos2f(X::float(), Y::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos2f</a>
%% C-API func: void glRasterPos2f(GLfloat x, GLfloat y)
rasterPos2f(X, Y) -> 
 cast(?glRasterPos2fv, <<X:32/?FN, Y:32/?FN>>).

%% @spec rasterPos2fv({V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos2fv</a>
%% C-API func: void glRasterPos2fv( const GLfloat * v)
rasterPos2fv({V1,V2}) -> 
 cast(?glRasterPos2fv, <<V1:32/?FN,V2:32/?FN>>).

%% @spec rasterPos2i(X::integer(), Y::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos2i</a>
%% C-API func: void glRasterPos2i(GLint x, GLint y)
rasterPos2i(X, Y) -> 
 cast(?glRasterPos2iv, <<X:32/?SN, Y:32/?SN>>).

%% @spec rasterPos2iv({V1::integer(),V2::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos2iv</a>
%% C-API func: void glRasterPos2iv( const GLint * v)
rasterPos2iv({V1,V2}) -> 
 cast(?glRasterPos2iv, <<V1:32/?SN,V2:32/?SN>>).

%% @spec rasterPos2s(X::integer(), Y::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos2s</a>
%% C-API func: void glRasterPos2s(GLshort x, GLshort y)
rasterPos2s(X, Y) -> 
 cast(?glRasterPos2sv, <<X:16/?SN, Y:16/?SN>>).

%% @spec rasterPos2sv({V1::integer(),V2::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos2sv</a>
%% C-API func: void glRasterPos2sv( const GLshort * v)
rasterPos2sv({V1,V2}) -> 
 cast(?glRasterPos2sv, <<V1:16/?SN,V2:16/?SN>>).

%% @spec rasterPos3d(X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos3d</a>
%% C-API func: void glRasterPos3d(GLdouble x, GLdouble y, GLdouble z)
rasterPos3d(X, Y, Z) -> 
 cast(?glRasterPos3dv, <<X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% @spec rasterPos3dv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos3dv</a>
%% C-API func: void glRasterPos3dv( const GLdouble * v)
rasterPos3dv({V1,V2,V3}) -> 
 cast(?glRasterPos3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% @spec rasterPos3f(X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos3f</a>
%% C-API func: void glRasterPos3f(GLfloat x, GLfloat y, GLfloat z)
rasterPos3f(X, Y, Z) -> 
 cast(?glRasterPos3fv, <<X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% @spec rasterPos3fv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos3fv</a>
%% C-API func: void glRasterPos3fv( const GLfloat * v)
rasterPos3fv({V1,V2,V3}) -> 
 cast(?glRasterPos3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% @spec rasterPos3i(X::integer(), Y::integer(), Z::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos3i</a>
%% C-API func: void glRasterPos3i(GLint x, GLint y, GLint z)
rasterPos3i(X, Y, Z) -> 
 cast(?glRasterPos3iv, <<X:32/?SN, Y:32/?SN, Z:32/?SN>>).

%% @spec rasterPos3iv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos3iv</a>
%% C-API func: void glRasterPos3iv( const GLint * v)
rasterPos3iv({V1,V2,V3}) -> 
 cast(?glRasterPos3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% @spec rasterPos3s(X::integer(), Y::integer(), Z::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos3s</a>
%% C-API func: void glRasterPos3s(GLshort x, GLshort y, GLshort z)
rasterPos3s(X, Y, Z) -> 
 cast(?glRasterPos3sv, <<X:16/?SN, Y:16/?SN, Z:16/?SN>>).

%% @spec rasterPos3sv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos3sv</a>
%% C-API func: void glRasterPos3sv( const GLshort * v)
rasterPos3sv({V1,V2,V3}) -> 
 cast(?glRasterPos3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% @spec rasterPos4d(X::float(), Y::float(), Z::float(), W::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos4d</a>
%% C-API func: void glRasterPos4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)
rasterPos4d(X, Y, Z, W) -> 
 cast(?glRasterPos4dv, <<X:64/?FN, Y:64/?FN, Z:64/?FN, W:64/?FN>>).

%% @spec rasterPos4dv({V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos4dv</a>
%% C-API func: void glRasterPos4dv( const GLdouble * v)
rasterPos4dv({V1,V2,V3,V4}) -> 
 cast(?glRasterPos4dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN,V4:64/?FN>>).

%% @spec rasterPos4f(X::float(), Y::float(), Z::float(), W::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos4f</a>
%% C-API func: void glRasterPos4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)
rasterPos4f(X, Y, Z, W) -> 
 cast(?glRasterPos4fv, <<X:32/?FN, Y:32/?FN, Z:32/?FN, W:32/?FN>>).

%% @spec rasterPos4fv({V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos4fv</a>
%% C-API func: void glRasterPos4fv( const GLfloat * v)
rasterPos4fv({V1,V2,V3,V4}) -> 
 cast(?glRasterPos4fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN,V4:32/?FN>>).

%% @spec rasterPos4i(X::integer(), Y::integer(), Z::integer(), W::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos4i</a>
%% C-API func: void glRasterPos4i(GLint x, GLint y, GLint z, GLint w)
rasterPos4i(X, Y, Z, W) -> 
 cast(?glRasterPos4iv, <<X:32/?SN, Y:32/?SN, Z:32/?SN, W:32/?SN>>).

%% @spec rasterPos4iv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos4iv</a>
%% C-API func: void glRasterPos4iv( const GLint * v)
rasterPos4iv({V1,V2,V3,V4}) -> 
 cast(?glRasterPos4iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN,V4:32/?SN>>).

%% @spec rasterPos4s(X::integer(), Y::integer(), Z::integer(), W::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos4s</a>
%% C-API func: void glRasterPos4s(GLshort x, GLshort y, GLshort z, GLshort w)
rasterPos4s(X, Y, Z, W) -> 
 cast(?glRasterPos4sv, <<X:16/?SN, Y:16/?SN, Z:16/?SN, W:16/?SN>>).

%% @spec rasterPos4sv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRasterPos">External manpage: rasterPos4sv</a>
%% C-API func: void glRasterPos4sv( const GLshort * v)
rasterPos4sv({V1,V2,V3,V4}) -> 
 cast(?glRasterPos4sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN,V4:16/?SN>>).

%% @spec readBuffer(Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glReadBuffer">External manpage: readBuffer</a>
%% C-API func: void glReadBuffer(GLenum mode)
readBuffer(Mode) -> 
 cast(?glReadBuffer, <<Mode:32/?UN>>).

%% @spec readPixels(X::integer(), Y::integer(), Width::integer(), Height::integer(), Format::integer(), Type::integer(), Pixels::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glReadPixels">External manpage: readPixels</a>
%% C-API func: void glReadPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid * pixels)
readPixels(X, Y, Width, Height, Format, Type, #sdlmem{bin=Pixels}) -> 
 sdl:send_bin(Pixels, ?MODULE, ?LINE),
 cast(?glReadPixels, <<X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN, Format:32/?UN, Type:32/?UN>>).

%% @spec rectd(X1::float(), Y1::float(), X2::float(), Y2::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRectd">External manpage: rectd</a>
%% C-API func: void glRectd(GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2)
rectd(X1, Y1, X2, Y2) -> 
 cast(?glRectd, <<X1:64/?FN, Y1:64/?FN, X2:64/?FN, Y2:64/?FN>>).

%% @spec rectdv(V1::binary() | [float()], V2::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRectdv">External manpage: rectdv</a>
%% C-API func: void glRectdv( const GLdouble * v1,  const GLdouble * v2)
rectdv(V1, V2) -> 
 NewV1 = if
	is_list(V1) ; is_tuple(V1) -> term2bin(V1, 2, ?GL_DOUBLE);
	binary(V1) -> V1;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V1})
 end, 
 NewV2 = if
	is_list(V2) ; is_tuple(V2) -> term2bin(V2, 2, ?GL_DOUBLE);
	binary(V2) -> V2;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V2})
 end, 
 cast(?glRectdv, [ NewV1, NewV2]).

%% @spec rectf(X1::float(), Y1::float(), X2::float(), Y2::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRectf">External manpage: rectf</a>
%% C-API func: void glRectf(GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2)
rectf(X1, Y1, X2, Y2) -> 
 cast(?glRectf, <<X1:32/?FN, Y1:32/?FN, X2:32/?FN, Y2:32/?FN>>).

%% @spec rectfv(V1::binary() | [float()], V2::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRectfv">External manpage: rectfv</a>
%% C-API func: void glRectfv( const GLfloat * v1,  const GLfloat * v2)
rectfv(V1, V2) -> 
 NewV1 = if
	is_list(V1) ; is_tuple(V1) -> term2bin(V1, 2, ?GL_FLOAT);
	binary(V1) -> V1;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V1})
 end, 
 NewV2 = if
	is_list(V2) ; is_tuple(V2) -> term2bin(V2, 2, ?GL_FLOAT);
	binary(V2) -> V2;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V2})
 end, 
 cast(?glRectfv, [ NewV1, NewV2]).

%% @spec recti(X1::integer(), Y1::integer(), X2::integer(), Y2::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRecti">External manpage: recti</a>
%% C-API func: void glRecti(GLint x1, GLint y1, GLint x2, GLint y2)
recti(X1, Y1, X2, Y2) -> 
 cast(?glRecti, <<X1:32/?SN, Y1:32/?SN, X2:32/?SN, Y2:32/?SN>>).

%% @spec rectiv(V1::binary() | [integer()], V2::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRectiv">External manpage: rectiv</a>
%% C-API func: void glRectiv( const GLint * v1,  const GLint * v2)
rectiv(V1, V2) -> 
 NewV1 = if
	is_list(V1) ; is_tuple(V1) -> term2bin(V1, 2, ?GL_INT);
	binary(V1) -> V1;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V1})
 end, 
 NewV2 = if
	is_list(V2) ; is_tuple(V2) -> term2bin(V2, 2, ?GL_INT);
	binary(V2) -> V2;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V2})
 end, 
 cast(?glRectiv, [ NewV1, NewV2]).

%% @spec rects(X1::integer(), Y1::integer(), X2::integer(), Y2::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRects">External manpage: rects</a>
%% C-API func: void glRects(GLshort x1, GLshort y1, GLshort x2, GLshort y2)
rects(X1, Y1, X2, Y2) -> 
 cast(?glRects, <<X1:16/?SN, Y1:16/?SN, X2:16/?SN, Y2:16/?SN>>).

%% @spec rectsv(V1::binary() | [integer()], V2::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRectsv">External manpage: rectsv</a>
%% C-API func: void glRectsv( const GLshort * v1,  const GLshort * v2)
rectsv(V1, V2) -> 
 NewV1 = if
	is_list(V1) ; is_tuple(V1) -> term2bin(V1, 2, ?GL_SHORT);
	binary(V1) -> V1;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V1})
 end, 
 NewV2 = if
	is_list(V2) ; is_tuple(V2) -> term2bin(V2, 2, ?GL_SHORT);
	binary(V2) -> V2;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V2})
 end, 
 cast(?glRectsv, [ NewV1, NewV2]).

%% @spec renderMode(Mode::integer()) -> integer()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRenderMode">External manpage: renderMode</a>
%% C-API func: GLint glRenderMode(GLenum mode)
renderMode(Mode) -> 
 Bin = call(?glRenderMode, <<Mode:32/?UN>>), 
 case Bin of 
	<<Ret:32/?SN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec rotated(Angle::float(), X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRotated">External manpage: rotated</a>
%% C-API func: void glRotated(GLdouble angle, GLdouble x, GLdouble y, GLdouble z)
rotated(Angle, X, Y, Z) -> 
 cast(?glRotated, <<Angle:64/?FN, X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% @spec rotatef(Angle::float(), X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRotatef">External manpage: rotatef</a>
%% C-API func: void glRotatef(GLfloat angle, GLfloat x, GLfloat y, GLfloat z)
rotatef(Angle, X, Y, Z) -> 
 cast(?glRotatef, <<Angle:32/?FN, X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% @spec scaled(X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glScaled">External manpage: scaled</a>
%% C-API func: void glScaled(GLdouble x, GLdouble y, GLdouble z)
scaled(X, Y, Z) -> 
 cast(?glScaled, <<X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% @spec scalef(X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glScalef">External manpage: scalef</a>
%% C-API func: void glScalef(GLfloat x, GLfloat y, GLfloat z)
scalef(X, Y, Z) -> 
 cast(?glScalef, <<X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% @spec scissor(X::integer(), Y::integer(), Width::integer(), Height::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glScissor">External manpage: scissor</a>
%% C-API func: void glScissor(GLint x, GLint y, GLsizei width, GLsizei height)
scissor(X, Y, Width, Height) -> 
 cast(?glScissor, <<X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN>>).

%% @spec selectBuffer(Size::integer(), Buffer::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSelectBuffer">External manpage: selectBuffer</a>
%% C-API func: void glSelectBuffer(GLsizei size, GLuint * buffer)
selectBuffer(Size, #sdlmem{bin=Buffer}) -> 
 sdl:send_bin(Buffer, ?MODULE, ?LINE),
 cast(?glSelectBuffer, <<Size:32/?SN>>).

%% @spec shadeModel(Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glShadeModel">External manpage: shadeModel</a>
%% C-API func: void glShadeModel(GLenum mode)
shadeModel(Mode) -> 
 cast(?glShadeModel, <<Mode:32/?UN>>).

%% @spec stencilFunc(Func::integer(), Ref::integer(), Mask::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glStencilFunc">External manpage: stencilFunc</a>
%% C-API func: void glStencilFunc(GLenum func, GLint ref, GLuint mask)
stencilFunc(Func, Ref, Mask) -> 
 cast(?glStencilFunc, <<Func:32/?UN, Ref:32/?SN, Mask:32/?UN>>).

%% @spec stencilMask(Mask::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glStencilMask">External manpage: stencilMask</a>
%% C-API func: void glStencilMask(GLuint mask)
stencilMask(Mask) -> 
 cast(?glStencilMask, <<Mask:32/?UN>>).

%% @spec stencilOp(Fail::integer(), Zfail::integer(), Zpass::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glStencilOp">External manpage: stencilOp</a>
%% C-API func: void glStencilOp(GLenum fail, GLenum zfail, GLenum zpass)
stencilOp(Fail, Zfail, Zpass) -> 
 cast(?glStencilOp, <<Fail:32/?UN, Zfail:32/?UN, Zpass:32/?UN>>).

%% @spec texCoord1d(S::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord1d</a>
%% C-API func: void glTexCoord1d(GLdouble s)
texCoord1d(S) -> 
 cast(?glTexCoord1dv, <<S:64/?FN>>).

%% @spec texCoord1dv({V1::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord1dv</a>
%% C-API func: void glTexCoord1dv( const GLdouble * v)
texCoord1dv({V1}) -> 
 cast(?glTexCoord1dv, <<V1:64/?FN>>).

%% @spec texCoord1f(S::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord1f</a>
%% C-API func: void glTexCoord1f(GLfloat s)
texCoord1f(S) -> 
 cast(?glTexCoord1fv, <<S:32/?FN>>).

%% @spec texCoord1fv({V1::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord1fv</a>
%% C-API func: void glTexCoord1fv( const GLfloat * v)
texCoord1fv({V1}) -> 
 cast(?glTexCoord1fv, <<V1:32/?FN>>).

%% @spec texCoord1i(S::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord1i</a>
%% C-API func: void glTexCoord1i(GLint s)
texCoord1i(S) -> 
 cast(?glTexCoord1iv, <<S:32/?SN>>).

%% @spec texCoord1iv({V1::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord1iv</a>
%% C-API func: void glTexCoord1iv( const GLint * v)
texCoord1iv({V1}) -> 
 cast(?glTexCoord1iv, <<V1:32/?SN>>).

%% @spec texCoord1s(S::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord1s</a>
%% C-API func: void glTexCoord1s(GLshort s)
texCoord1s(S) -> 
 cast(?glTexCoord1sv, <<S:16/?SN>>).

%% @spec texCoord1sv({V1::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord1sv</a>
%% C-API func: void glTexCoord1sv( const GLshort * v)
texCoord1sv({V1}) -> 
 cast(?glTexCoord1sv, <<V1:16/?SN>>).

%% @spec texCoord2d(S::float(), T::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord2d</a>
%% C-API func: void glTexCoord2d(GLdouble s, GLdouble t)
texCoord2d(S, T) -> 
 cast(?glTexCoord2dv, <<S:64/?FN, T:64/?FN>>).

%% @spec texCoord2dv({V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord2dv</a>
%% C-API func: void glTexCoord2dv( const GLdouble * v)
texCoord2dv({V1,V2}) -> 
 cast(?glTexCoord2dv, <<V1:64/?FN,V2:64/?FN>>).

%% @spec texCoord2f(S::float(), T::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord2f</a>
%% C-API func: void glTexCoord2f(GLfloat s, GLfloat t)
texCoord2f(S, T) -> 
 cast(?glTexCoord2fv, <<S:32/?FN, T:32/?FN>>).

%% @spec texCoord2fv({V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord2fv</a>
%% C-API func: void glTexCoord2fv( const GLfloat * v)
texCoord2fv({V1,V2}) -> 
 cast(?glTexCoord2fv, <<V1:32/?FN,V2:32/?FN>>).

%% @spec texCoord2i(S::integer(), T::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord2i</a>
%% C-API func: void glTexCoord2i(GLint s, GLint t)
texCoord2i(S, T) -> 
 cast(?glTexCoord2iv, <<S:32/?SN, T:32/?SN>>).

%% @spec texCoord2iv({V1::integer(),V2::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord2iv</a>
%% C-API func: void glTexCoord2iv( const GLint * v)
texCoord2iv({V1,V2}) -> 
 cast(?glTexCoord2iv, <<V1:32/?SN,V2:32/?SN>>).

%% @spec texCoord2s(S::integer(), T::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord2s</a>
%% C-API func: void glTexCoord2s(GLshort s, GLshort t)
texCoord2s(S, T) -> 
 cast(?glTexCoord2sv, <<S:16/?SN, T:16/?SN>>).

%% @spec texCoord2sv({V1::integer(),V2::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord2sv</a>
%% C-API func: void glTexCoord2sv( const GLshort * v)
texCoord2sv({V1,V2}) -> 
 cast(?glTexCoord2sv, <<V1:16/?SN,V2:16/?SN>>).

%% @spec texCoord3d(S::float(), T::float(), R::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord3d</a>
%% C-API func: void glTexCoord3d(GLdouble s, GLdouble t, GLdouble r)
texCoord3d(S, T, R) -> 
 cast(?glTexCoord3dv, <<S:64/?FN, T:64/?FN, R:64/?FN>>).

%% @spec texCoord3dv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord3dv</a>
%% C-API func: void glTexCoord3dv( const GLdouble * v)
texCoord3dv({V1,V2,V3}) -> 
 cast(?glTexCoord3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% @spec texCoord3f(S::float(), T::float(), R::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord3f</a>
%% C-API func: void glTexCoord3f(GLfloat s, GLfloat t, GLfloat r)
texCoord3f(S, T, R) -> 
 cast(?glTexCoord3fv, <<S:32/?FN, T:32/?FN, R:32/?FN>>).

%% @spec texCoord3fv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord3fv</a>
%% C-API func: void glTexCoord3fv( const GLfloat * v)
texCoord3fv({V1,V2,V3}) -> 
 cast(?glTexCoord3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% @spec texCoord3i(S::integer(), T::integer(), R::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord3i</a>
%% C-API func: void glTexCoord3i(GLint s, GLint t, GLint r)
texCoord3i(S, T, R) -> 
 cast(?glTexCoord3iv, <<S:32/?SN, T:32/?SN, R:32/?SN>>).

%% @spec texCoord3iv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord3iv</a>
%% C-API func: void glTexCoord3iv( const GLint * v)
texCoord3iv({V1,V2,V3}) -> 
 cast(?glTexCoord3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% @spec texCoord3s(S::integer(), T::integer(), R::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord3s</a>
%% C-API func: void glTexCoord3s(GLshort s, GLshort t, GLshort r)
texCoord3s(S, T, R) -> 
 cast(?glTexCoord3sv, <<S:16/?SN, T:16/?SN, R:16/?SN>>).

%% @spec texCoord3sv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord3sv</a>
%% C-API func: void glTexCoord3sv( const GLshort * v)
texCoord3sv({V1,V2,V3}) -> 
 cast(?glTexCoord3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% @spec texCoord4d(S::float(), T::float(), R::float(), Q::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord4d</a>
%% C-API func: void glTexCoord4d(GLdouble s, GLdouble t, GLdouble r, GLdouble q)
texCoord4d(S, T, R, Q) -> 
 cast(?glTexCoord4dv, <<S:64/?FN, T:64/?FN, R:64/?FN, Q:64/?FN>>).

%% @spec texCoord4dv({V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord4dv</a>
%% C-API func: void glTexCoord4dv( const GLdouble * v)
texCoord4dv({V1,V2,V3,V4}) -> 
 cast(?glTexCoord4dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN,V4:64/?FN>>).

%% @spec texCoord4f(S::float(), T::float(), R::float(), Q::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord4f</a>
%% C-API func: void glTexCoord4f(GLfloat s, GLfloat t, GLfloat r, GLfloat q)
texCoord4f(S, T, R, Q) -> 
 cast(?glTexCoord4fv, <<S:32/?FN, T:32/?FN, R:32/?FN, Q:32/?FN>>).

%% @spec texCoord4fv({V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord4fv</a>
%% C-API func: void glTexCoord4fv( const GLfloat * v)
texCoord4fv({V1,V2,V3,V4}) -> 
 cast(?glTexCoord4fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN,V4:32/?FN>>).

%% @spec texCoord4i(S::integer(), T::integer(), R::integer(), Q::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord4i</a>
%% C-API func: void glTexCoord4i(GLint s, GLint t, GLint r, GLint q)
texCoord4i(S, T, R, Q) -> 
 cast(?glTexCoord4iv, <<S:32/?SN, T:32/?SN, R:32/?SN, Q:32/?SN>>).

%% @spec texCoord4iv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord4iv</a>
%% C-API func: void glTexCoord4iv( const GLint * v)
texCoord4iv({V1,V2,V3,V4}) -> 
 cast(?glTexCoord4iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN,V4:32/?SN>>).

%% @spec texCoord4s(S::integer(), T::integer(), R::integer(), Q::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord4s</a>
%% C-API func: void glTexCoord4s(GLshort s, GLshort t, GLshort r, GLshort q)
texCoord4s(S, T, R, Q) -> 
 cast(?glTexCoord4sv, <<S:16/?SN, T:16/?SN, R:16/?SN, Q:16/?SN>>).

%% @spec texCoord4sv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoord">External manpage: texCoord4sv</a>
%% C-API func: void glTexCoord4sv( const GLshort * v)
texCoord4sv({V1,V2,V3,V4}) -> 
 cast(?glTexCoord4sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN,V4:16/?SN>>).

%% @spec texCoordPointer(Size::integer(), Type::integer(), Stride::integer(), Pointer::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexCoordPointer">External manpage: texCoordPointer</a>
%% C-API func: void glTexCoordPointer(GLint size, GLenum type, GLsizei stride,  const GLvoid * pointer)
texCoordPointer(Size, Type, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glTexCoordPointer, [<<Size:32/?SN, Type:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec texEnvf(Target::integer(), Pname::integer(), Param::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexEnvf">External manpage: texEnvf</a>
%% C-API func: void glTexEnvf(GLenum target, GLenum pname, GLfloat param)
texEnvf(Target, Pname, Param) -> 
 cast(?glTexEnvf, <<Target:32/?UN, Pname:32/?UN, Param:32/?FN>>).

%% @spec texEnvfv(Target::integer(), Pname::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexEnvfv">External manpage: texEnvfv</a>
%% C-API func: void glTexEnvfv(GLenum target, GLenum pname,  const GLfloat * params)
texEnvfv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexEnvfv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec texEnvi(Target::integer(), Pname::integer(), Param::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexEnvi">External manpage: texEnvi</a>
%% C-API func: void glTexEnvi(GLenum target, GLenum pname, GLint param)
texEnvi(Target, Pname, Param) -> 
 cast(?glTexEnvi, <<Target:32/?UN, Pname:32/?UN, Param:32/?SN>>).

%% @spec texEnviv(Target::integer(), Pname::integer(), Params::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexEnviv">External manpage: texEnviv</a>
%% C-API func: void glTexEnviv(GLenum target, GLenum pname,  const GLint * params)
texEnviv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexEnviv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec texGend(Coord::integer(), Pname::integer(), Param::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexGend">External manpage: texGend</a>
%% C-API func: void glTexGend(GLenum coord, GLenum pname, GLdouble param)
texGend(Coord, Pname, Param) -> 
 cast(?glTexGend, <<Coord:32/?UN, Pname:32/?UN, Param:64/?FN>>).

%% @spec texGendv(Coord::integer(), Pname::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexGendv">External manpage: texGendv</a>
%% C-API func: void glTexGendv(GLenum coord, GLenum pname,  const GLdouble * params)
texGendv(Coord, Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_DOUBLE);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexGendv, [<<Coord:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec texGenf(Coord::integer(), Pname::integer(), Param::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexGenf">External manpage: texGenf</a>
%% C-API func: void glTexGenf(GLenum coord, GLenum pname, GLfloat param)
texGenf(Coord, Pname, Param) -> 
 cast(?glTexGenf, <<Coord:32/?UN, Pname:32/?UN, Param:32/?FN>>).

%% @spec texGenfv(Coord::integer(), Pname::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexGenfv">External manpage: texGenfv</a>
%% C-API func: void glTexGenfv(GLenum coord, GLenum pname,  const GLfloat * params)
texGenfv(Coord, Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_FLOAT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexGenfv, [<<Coord:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec texGeni(Coord::integer(), Pname::integer(), Param::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexGeni">External manpage: texGeni</a>
%% C-API func: void glTexGeni(GLenum coord, GLenum pname, GLint param)
texGeni(Coord, Pname, Param) -> 
 cast(?glTexGeni, <<Coord:32/?UN, Pname:32/?UN, Param:32/?SN>>).

%% @spec texGeniv(Coord::integer(), Pname::integer(), Params::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexGeniv">External manpage: texGeniv</a>
%% C-API func: void glTexGeniv(GLenum coord, GLenum pname,  const GLint * params)
texGeniv(Coord, Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_INT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexGeniv, [<<Coord:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec texImage1D(Target::integer(), Level::integer(), Internalformat::integer(), Width::integer(), Border::integer(), Format::integer(), Type::integer(), Pixels::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexImage1D">External manpage: texImage1D</a>
%% C-API func: void glTexImage1D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type,  const GLvoid * pixels)
texImage1D(Target, Level, Internalformat, Width, Border, Format, Type, Pixels) -> 
%% Maybe NULL or offset sometimes
 NewPixels =
   if is_integer(Pixels) -> Pixels;
      true ->
        sdl:send_bin(Pixels, ?MODULE, ?LINE),
       0
   end,
 cast(?glTexImage1D, [<<Target:32/?UN, Level:32/?SN, Internalformat:32/?SN, Width:32/?SN, Border:32/?SN, Format:32/?UN, Type:32/?UN, NewPixels:32/?SN>>]).

%% @spec texImage2D(Target::integer(), Level::integer(), Internalformat::integer(), Width::integer(), Height::integer(), Border::integer(), Format::integer(), Type::integer(), Pixels::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexImage2D">External manpage: texImage2D</a>
%% C-API func: void glTexImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type,  const GLvoid * pixels)
texImage2D(Target, Level, Internalformat, Width, Height, Border, Format, Type, Pixels) -> 
%% Maybe NULL or offset sometimes
 NewPixels =
   if is_integer(Pixels) -> Pixels;
      true ->
        sdl:send_bin(Pixels, ?MODULE, ?LINE),
       0
   end,
 cast(?glTexImage2D, [<<Target:32/?UN, Level:32/?SN, Internalformat:32/?SN, Width:32/?SN, Height:32/?SN, Border:32/?SN, Format:32/?UN, Type:32/?UN, NewPixels:32/?SN>>]).

%% @spec texParameterf(Target::integer(), Pname::integer(), Param::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexParameterf">External manpage: texParameterf</a>
%% C-API func: void glTexParameterf(GLenum target, GLenum pname, GLfloat param)
texParameterf(Target, Pname, Param) -> 
 cast(?glTexParameterf, <<Target:32/?UN, Pname:32/?UN, Param:32/?FN>>).

%% @spec texParameterfv(Target::integer(), Pname::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexParameterfv">External manpage: texParameterfv</a>
%% C-API func: void glTexParameterfv(GLenum target, GLenum pname,  const GLfloat * params)
texParameterfv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexParameterfv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec texParameteri(Target::integer(), Pname::integer(), Param::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexParameteri">External manpage: texParameteri</a>
%% C-API func: void glTexParameteri(GLenum target, GLenum pname, GLint param)
texParameteri(Target, Pname, Param) -> 
 cast(?glTexParameteri, <<Target:32/?UN, Pname:32/?UN, Param:32/?SN>>).

%% @spec texParameteriv(Target::integer(), Pname::integer(), Params::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexParameteriv">External manpage: texParameteriv</a>
%% C-API func: void glTexParameteriv(GLenum target, GLenum pname,  const GLint * params)
texParameteriv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexParameteriv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec texSubImage1D(Target::integer(), Level::integer(), Xoffset::integer(), Width::integer(), Format::integer(), Type::integer(), Pixels::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexSubImage1D">External manpage: texSubImage1D</a>
%% C-API func: void glTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type,  const GLvoid * pixels)
texSubImage1D(Target, Level, Xoffset, Width, Format, Type, Pixels) -> 
%% Maybe NULL or offset sometimes
 NewPixels =
   if is_integer(Pixels) -> Pixels;
      true ->
        sdl:send_bin(Pixels, ?MODULE, ?LINE),
       0
   end,
 cast(?glTexSubImage1D, [<<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, Width:32/?SN, Format:32/?UN, Type:32/?UN, NewPixels:32/?SN>>]).

%% @spec texSubImage2D(Target::integer(), Level::integer(), Xoffset::integer(), Yoffset::integer(), Width::integer(), Height::integer(), Format::integer(), Type::integer(), Pixels::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexSubImage2D">External manpage: texSubImage2D</a>
%% C-API func: void glTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type,  const GLvoid * pixels)
texSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, Type, Pixels) -> 
%% Maybe NULL or offset sometimes
 NewPixels =
   if is_integer(Pixels) -> Pixels;
      true ->
        sdl:send_bin(Pixels, ?MODULE, ?LINE),
       0
   end,
 cast(?glTexSubImage2D, [<<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, Yoffset:32/?SN, Width:32/?SN, Height:32/?SN, Format:32/?UN, Type:32/?UN, NewPixels:32/?SN>>]).

%% @spec translated(X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTranslated">External manpage: translated</a>
%% C-API func: void glTranslated(GLdouble x, GLdouble y, GLdouble z)
translated(X, Y, Z) -> 
 cast(?glTranslated, <<X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% @spec translatef(X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTranslatef">External manpage: translatef</a>
%% C-API func: void glTranslatef(GLfloat x, GLfloat y, GLfloat z)
translatef(X, Y, Z) -> 
 cast(?glTranslatef, <<X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% @spec vertex2d(X::float(), Y::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex2d</a>
%% C-API func: void glVertex2d(GLdouble x, GLdouble y)
vertex2d(X, Y) -> 
 cast(?glVertex2dv, <<X:64/?FN, Y:64/?FN>>).

%% @spec vertex2dv({V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex2dv</a>
%% C-API func: void glVertex2dv( const GLdouble * v)
vertex2dv({V1,V2}) -> 
 cast(?glVertex2dv, <<V1:64/?FN,V2:64/?FN>>).

%% @spec vertex2f(X::float(), Y::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex2f</a>
%% C-API func: void glVertex2f(GLfloat x, GLfloat y)
vertex2f(X, Y) -> 
 cast(?glVertex2fv, <<X:32/?FN, Y:32/?FN>>).

%% @spec vertex2fv({V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex2fv</a>
%% C-API func: void glVertex2fv( const GLfloat * v)
vertex2fv({V1,V2}) -> 
 cast(?glVertex2fv, <<V1:32/?FN,V2:32/?FN>>).

%% @spec vertex2i(X::integer(), Y::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex2i</a>
%% C-API func: void glVertex2i(GLint x, GLint y)
vertex2i(X, Y) -> 
 cast(?glVertex2iv, <<X:32/?SN, Y:32/?SN>>).

%% @spec vertex2iv({V1::integer(),V2::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex2iv</a>
%% C-API func: void glVertex2iv( const GLint * v)
vertex2iv({V1,V2}) -> 
 cast(?glVertex2iv, <<V1:32/?SN,V2:32/?SN>>).

%% @spec vertex2s(X::integer(), Y::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex2s</a>
%% C-API func: void glVertex2s(GLshort x, GLshort y)
vertex2s(X, Y) -> 
 cast(?glVertex2sv, <<X:16/?SN, Y:16/?SN>>).

%% @spec vertex2sv({V1::integer(),V2::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex2sv</a>
%% C-API func: void glVertex2sv( const GLshort * v)
vertex2sv({V1,V2}) -> 
 cast(?glVertex2sv, <<V1:16/?SN,V2:16/?SN>>).

%% @spec vertex3d(X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex3d</a>
%% C-API func: void glVertex3d(GLdouble x, GLdouble y, GLdouble z)
vertex3d(X, Y, Z) -> 
 cast(?glVertex3dv, <<X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% @spec vertex3dv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex3dv</a>
%% C-API func: void glVertex3dv( const GLdouble * v)
vertex3dv({V1,V2,V3}) -> 
 cast(?glVertex3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% @spec vertex3f(X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex3f</a>
%% C-API func: void glVertex3f(GLfloat x, GLfloat y, GLfloat z)
vertex3f(X, Y, Z) -> 
 cast(?glVertex3fv, <<X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% @spec vertex3fv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex3fv</a>
%% C-API func: void glVertex3fv( const GLfloat * v)
vertex3fv({V1,V2,V3}) -> 
 cast(?glVertex3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% @spec vertex3i(X::integer(), Y::integer(), Z::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex3i</a>
%% C-API func: void glVertex3i(GLint x, GLint y, GLint z)
vertex3i(X, Y, Z) -> 
 cast(?glVertex3iv, <<X:32/?SN, Y:32/?SN, Z:32/?SN>>).

%% @spec vertex3iv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex3iv</a>
%% C-API func: void glVertex3iv( const GLint * v)
vertex3iv({V1,V2,V3}) -> 
 cast(?glVertex3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% @spec vertex3s(X::integer(), Y::integer(), Z::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex3s</a>
%% C-API func: void glVertex3s(GLshort x, GLshort y, GLshort z)
vertex3s(X, Y, Z) -> 
 cast(?glVertex3sv, <<X:16/?SN, Y:16/?SN, Z:16/?SN>>).

%% @spec vertex3sv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex3sv</a>
%% C-API func: void glVertex3sv( const GLshort * v)
vertex3sv({V1,V2,V3}) -> 
 cast(?glVertex3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% @spec vertex4d(X::float(), Y::float(), Z::float(), W::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex4d</a>
%% C-API func: void glVertex4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)
vertex4d(X, Y, Z, W) -> 
 cast(?glVertex4dv, <<X:64/?FN, Y:64/?FN, Z:64/?FN, W:64/?FN>>).

%% @spec vertex4dv({V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex4dv</a>
%% C-API func: void glVertex4dv( const GLdouble * v)
vertex4dv({V1,V2,V3,V4}) -> 
 cast(?glVertex4dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN,V4:64/?FN>>).

%% @spec vertex4f(X::float(), Y::float(), Z::float(), W::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex4f</a>
%% C-API func: void glVertex4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)
vertex4f(X, Y, Z, W) -> 
 cast(?glVertex4fv, <<X:32/?FN, Y:32/?FN, Z:32/?FN, W:32/?FN>>).

%% @spec vertex4fv({V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex4fv</a>
%% C-API func: void glVertex4fv( const GLfloat * v)
vertex4fv({V1,V2,V3,V4}) -> 
 cast(?glVertex4fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN,V4:32/?FN>>).

%% @spec vertex4i(X::integer(), Y::integer(), Z::integer(), W::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex4i</a>
%% C-API func: void glVertex4i(GLint x, GLint y, GLint z, GLint w)
vertex4i(X, Y, Z, W) -> 
 cast(?glVertex4iv, <<X:32/?SN, Y:32/?SN, Z:32/?SN, W:32/?SN>>).

%% @spec vertex4iv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex4iv</a>
%% C-API func: void glVertex4iv( const GLint * v)
vertex4iv({V1,V2,V3,V4}) -> 
 cast(?glVertex4iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN,V4:32/?SN>>).

%% @spec vertex4s(X::integer(), Y::integer(), Z::integer(), W::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex4s</a>
%% C-API func: void glVertex4s(GLshort x, GLshort y, GLshort z, GLshort w)
vertex4s(X, Y, Z, W) -> 
 cast(?glVertex4sv, <<X:16/?SN, Y:16/?SN, Z:16/?SN, W:16/?SN>>).

%% @spec vertex4sv({V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertex">External manpage: vertex4sv</a>
%% C-API func: void glVertex4sv( const GLshort * v)
vertex4sv({V1,V2,V3,V4}) -> 
 cast(?glVertex4sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN,V4:16/?SN>>).

%% @spec vertexPointer(Size::integer(), Type::integer(), Stride::integer(), Pointer::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexPointer">External manpage: vertexPointer</a>
%% C-API func: void glVertexPointer(GLint size, GLenum type, GLsizei stride,  const GLvoid * pointer)
vertexPointer(Size, Type, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glVertexPointer, [<<Size:32/?SN, Type:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec viewport(X::integer(), Y::integer(), Width::integer(), Height::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glViewport">External manpage: viewport</a>
%% C-API func: void glViewport(GLint x, GLint y, GLsizei width, GLsizei height)
viewport(X, Y, Width, Height) -> 
 cast(?glViewport, <<X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN>>).

%%% OPENGL 1.2 and later with selected ARB's and extensions
%%-module(glext).
-include("glext_funcs.hrl").

glGetConvolutionParameterivLen(Pname) ->
    glGetConvolutionParameterfvLen(Pname).
glGetConvolutionParameterfvLen(Pname) ->
    case Pname of
	?GL_CONVOLUTION_BORDER_MODE  ->	    1;
	?GL_CONVOLUTION_BORDER_COLOR ->     1;
	?GL_CONVOLUTION_FILTER_SCALE ->     4;
	?GL_CONVOLUTION_FILTER_BIAS  ->     4;
	?GL_CONVOLUTION_FORMAT       ->	    4;
	?GL_CONVOLUTION_WIDTH        ->	    1;
	?GL_CONVOLUTION_HEIGHT       ->     1;
	?GL_MAX_CONVOLUTION_WIDTH    ->     1;
	?GL_MAX_CONVOLUTION_HEIGHT   ->     1;
	_ -> 4
    end.
%% @spec blendColor(Red::float(), Green::float(), Blue::float(), Alpha::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBlendColor">External manpage: blendColor</a>
%% C-API func: void glBlendColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)
blendColor(Red, Green, Blue, Alpha) -> 
 cast(?glBlendColor, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN, Alpha:32/?FN>>).

%% @spec blendEquation(Mode::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBlendEquation">External manpage: blendEquation</a>
%% C-API func: void glBlendEquation(GLenum mode)
blendEquation(Mode) -> 
 cast(?glBlendEquation, <<Mode:32/?UN>>).

%% @spec drawRangeElements(Mode::integer(), Start::integer(), End::integer(), Count::integer(), Type::integer(), Indices::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDrawRangeElements">External manpage: drawRangeElements</a>
%% C-API func: void glDrawRangeElements(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type,  const GLvoid * indices)
drawRangeElements(Mode, Start, End, Count, Type, Indices) -> 
%% Maybe NULL or offset sometimes2
 NewIndices = if is_integer(Indices) -> Indices; 
	is_list(Indices) ; is_tuple(Indices) -> sdl:send_bin(list_to_binary(term2bin(Indices, Count, Type)),?MODULE,?LINE),0;
	is_binary(Indices) -> sdl:send_bin(Indices, ?MODULE, ?LINE),0;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Indices})
 end, 
 cast(?glDrawRangeElements, [<<Mode:32/?UN, Start:32/?UN, End:32/?UN, Count:32/?SN, Type:32/?UN, NewIndices:32/?SN>>]).

%% @spec colorTable(Target::integer(), Internalformat::integer(), Width::integer(), Format::integer(), Type::integer(), Table::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColorTable">External manpage: colorTable</a>
%% C-API func: void glColorTable(GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type,  const GLvoid * table)
colorTable(Target, Internalformat, Width, Format, Type, Table) -> 
%% Maybe NULL or offset sometimes
 NewTable =
   if is_integer(Table) -> Table;
      true ->
        sdl:send_bin(Table, ?MODULE, ?LINE),
       0
   end,
 cast(?glColorTable, [<<Target:32/?UN, Internalformat:32/?UN, Width:32/?SN, Format:32/?UN, Type:32/?UN, NewTable:32/?SN>>]).

%% @spec colorTableParameterfv(Target::integer(), Pname::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColorTableParameterfv">External manpage: colorTableParameterfv</a>
%% C-API func: void glColorTableParameterfv(GLenum target, GLenum pname,  const GLfloat * params)
colorTableParameterfv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_FLOAT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glColorTableParameterfv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec colorTableParameteriv(Target::integer(), Pname::integer(), Params::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColorTableParameteriv">External manpage: colorTableParameteriv</a>
%% C-API func: void glColorTableParameteriv(GLenum target, GLenum pname,  const GLint * params)
colorTableParameteriv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_INT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glColorTableParameteriv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec copyColorTable(Target::integer(), Internalformat::integer(), X::integer(), Y::integer(), Width::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCopyColorTable">External manpage: copyColorTable</a>
%% C-API func: void glCopyColorTable(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width)
copyColorTable(Target, Internalformat, X, Y, Width) -> 
 cast(?glCopyColorTable, <<Target:32/?UN, Internalformat:32/?UN, X:32/?SN, Y:32/?SN, Width:32/?SN>>).

%% @spec getColorTable(Target::integer(), Format::integer(), Type::integer(), Table::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetColorTable">External manpage: getColorTable</a>
%% C-API func: void glGetColorTable(GLenum target, GLenum format, GLenum type, GLvoid * table)
getColorTable(Target, Format, Type, #sdlmem{bin=Table}) -> 
 sdl:send_bin(Table, ?MODULE, ?LINE),
 cast(?glGetColorTable, <<Target:32/?UN, Format:32/?UN, Type:32/?UN>>).

%% @spec getColorTableParameterfv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetColorTableParameterfv">External manpage: getColorTableParameterfv</a>
%% C-API func: void glGetColorTableParameterfv(GLenum target, GLenum pname, GLfloat * params)
getColorTableParameterfv(Target, Pname) -> 
 Bin = call(?glGetColorTableParameterfv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(4, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getColorTableParameteriv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetColorTableParameteriv">External manpage: getColorTableParameteriv</a>
%% C-API func: void glGetColorTableParameteriv(GLenum target, GLenum pname, GLint * params)
getColorTableParameteriv(Target, Pname) -> 
 Bin = call(?glGetColorTableParameteriv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_INT_SIZE>> -> 
	 bin2list(4, ?GL_INT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec colorSubTable(Target::integer(), Start::integer(), Count::integer(), Format::integer(), Type::integer(), Data::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glColorSubTable">External manpage: colorSubTable</a>
%% C-API func: void glColorSubTable(GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type,  const GLvoid * data)
colorSubTable(Target, Start, Count, Format, Type, Data) -> 
%% Maybe NULL or offset sometimes
 NewData =
   if is_integer(Data) -> Data;
      true ->
        sdl:send_bin(Data, ?MODULE, ?LINE),
       0
   end,
 cast(?glColorSubTable, [<<Target:32/?UN, Start:32/?SN, Count:32/?SN, Format:32/?UN, Type:32/?UN, NewData:32/?SN>>]).

%% @spec copyColorSubTable(Target::integer(), Start::integer(), X::integer(), Y::integer(), Width::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCopyColorSubTable">External manpage: copyColorSubTable</a>
%% C-API func: void glCopyColorSubTable(GLenum target, GLsizei start, GLint x, GLint y, GLsizei width)
copyColorSubTable(Target, Start, X, Y, Width) -> 
 cast(?glCopyColorSubTable, <<Target:32/?UN, Start:32/?SN, X:32/?SN, Y:32/?SN, Width:32/?SN>>).

%% @spec convolutionFilter1D(Target::integer(), Internalformat::integer(), Width::integer(), Format::integer(), Type::integer(), Image::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glConvolutionFilter1D">External manpage: convolutionFilter1D</a>
%% C-API func: void glConvolutionFilter1D(GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type,  const GLvoid * image)
convolutionFilter1D(Target, Internalformat, Width, Format, Type, Image) -> 
%% Maybe NULL or offset sometimes
 NewImage =
   if is_integer(Image) -> Image;
      true ->
        sdl:send_bin(Image, ?MODULE, ?LINE),
       0
   end,
 cast(?glConvolutionFilter1D, [<<Target:32/?UN, Internalformat:32/?UN, Width:32/?SN, Format:32/?UN, Type:32/?UN, NewImage:32/?SN>>]).

%% @spec convolutionFilter2D(Target::integer(), Internalformat::integer(), Width::integer(), Height::integer(), Format::integer(), Type::integer(), Image::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glConvolutionFilter2D">External manpage: convolutionFilter2D</a>
%% C-API func: void glConvolutionFilter2D(GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type,  const GLvoid * image)
convolutionFilter2D(Target, Internalformat, Width, Height, Format, Type, Image) -> 
%% Maybe NULL or offset sometimes
 NewImage =
   if is_integer(Image) -> Image;
      true ->
        sdl:send_bin(Image, ?MODULE, ?LINE),
       0
   end,
 cast(?glConvolutionFilter2D, [<<Target:32/?UN, Internalformat:32/?UN, Width:32/?SN, Height:32/?SN, Format:32/?UN, Type:32/?UN, NewImage:32/?SN>>]).

%% @spec convolutionParameterf(Target::integer(), Pname::integer(), Params::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glConvolutionParameterf">External manpage: convolutionParameterf</a>
%% C-API func: void glConvolutionParameterf(GLenum target, GLenum pname, GLfloat params)
convolutionParameterf(Target, Pname, Params) -> 
 cast(?glConvolutionParameterf, <<Target:32/?UN, Pname:32/?UN, Params:32/?FN>>).

%% @spec convolutionParameterfv(Target::integer(), Pname::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glConvolutionParameterfv">External manpage: convolutionParameterfv</a>
%% C-API func: void glConvolutionParameterfv(GLenum target, GLenum pname,  const GLfloat * params)
convolutionParameterfv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glConvolutionParameterfv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec convolutionParameteri(Target::integer(), Pname::integer(), Params::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glConvolutionParameteri">External manpage: convolutionParameteri</a>
%% C-API func: void glConvolutionParameteri(GLenum target, GLenum pname, GLint params)
convolutionParameteri(Target, Pname, Params) -> 
 cast(?glConvolutionParameteri, <<Target:32/?UN, Pname:32/?UN, Params:32/?SN>>).

%% @spec convolutionParameteriv(Target::integer(), Pname::integer(), Params::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glConvolutionParameteriv">External manpage: convolutionParameteriv</a>
%% C-API func: void glConvolutionParameteriv(GLenum target, GLenum pname,  const GLint * params)
convolutionParameteriv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glConvolutionParameteriv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% @spec copyConvolutionFilter1D(Target::integer(), Internalformat::integer(), X::integer(), Y::integer(), Width::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCopyConvolutionFilter1D">External manpage: copyConvolutionFilter1D</a>
%% C-API func: void glCopyConvolutionFilter1D(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width)
copyConvolutionFilter1D(Target, Internalformat, X, Y, Width) -> 
 cast(?glCopyConvolutionFilter1D, <<Target:32/?UN, Internalformat:32/?UN, X:32/?SN, Y:32/?SN, Width:32/?SN>>).

%% @spec copyConvolutionFilter2D(Target::integer(), Internalformat::integer(), X::integer(), Y::integer(), Width::integer(), Height::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCopyConvolutionFilter2D">External manpage: copyConvolutionFilter2D</a>
%% C-API func: void glCopyConvolutionFilter2D(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height)
copyConvolutionFilter2D(Target, Internalformat, X, Y, Width, Height) -> 
 cast(?glCopyConvolutionFilter2D, <<Target:32/?UN, Internalformat:32/?UN, X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN>>).

%% @spec getConvolutionFilter(Target::integer(), Format::integer(), Type::integer(), Image::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetConvolutionFilter">External manpage: getConvolutionFilter</a>
%% C-API func: void glGetConvolutionFilter(GLenum target, GLenum format, GLenum type, GLvoid * image)
getConvolutionFilter(Target, Format, Type, #sdlmem{bin=Image}) -> 
 sdl:send_bin(Image, ?MODULE, ?LINE),
 cast(?glGetConvolutionFilter, <<Target:32/?UN, Format:32/?UN, Type:32/?UN>>).

%% @spec getConvolutionParameterfv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetConvolutionParameterfv">External manpage: getConvolutionParameterfv</a>
%% C-API func: void glGetConvolutionParameterfv(GLenum target, GLenum pname, GLfloat * params)
getConvolutionParameterfv(Target, Pname) -> 
 Bin = call(?glGetConvolutionParameterfv, <<Target:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetConvolutionParameterfvLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_FLOAT_SIZE,_:ParamsBump/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getConvolutionParameteriv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetConvolutionParameteriv">External manpage: getConvolutionParameteriv</a>
%% C-API func: void glGetConvolutionParameteriv(GLenum target, GLenum pname, GLint * params)
getConvolutionParameteriv(Target, Pname) -> 
 Bin = call(?glGetConvolutionParameteriv, <<Target:32/?UN, Pname:32/?UN>>), 
 ParamsLen = glGetConvolutionParameterivLen(Pname),
 ParamsBump = 4 - ParamsLen,
 case Bin of 
	<<Params:ParamsLen/binary-unit:?GL_INT_SIZE,_:ParamsBump/binary-unit:?GL_INT_SIZE>> -> 
	 bin2list(ParamsLen, ?GL_INT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getSeparableFilter(Target::integer(), Format::integer(), Type::integer(), Row::sdlmem(), Column::sdlmem(), Span::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetSeparableFilter">External manpage: getSeparableFilter</a>
%% C-API func: void glGetSeparableFilter(GLenum target, GLenum format, GLenum type, GLvoid * row, GLvoid * column, GLvoid * span)
getSeparableFilter(Target, Format, Type, #sdlmem{bin=Row}, #sdlmem{bin=Column}, #sdlmem{bin=Span}) -> 
 sdl:send_bin(Row, ?MODULE, ?LINE),
 sdl:send_bin(Column, ?MODULE, ?LINE),
 sdl:send_bin(Span, ?MODULE, ?LINE),
 cast(?glGetSeparableFilter, <<Target:32/?UN, Format:32/?UN, Type:32/?UN>>).

%% @spec separableFilter2D(Target::integer(), Internalformat::integer(), Width::integer(), Height::integer(), Format::integer(), Type::integer(), Row::binary() | [number()], Column::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSeparableFilter2D">External manpage: separableFilter2D</a>
%% C-API func: void glSeparableFilter2D(GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type,  const GLvoid * row,  const GLvoid * column)
separableFilter2D(Target, Internalformat, Width, Height, Format, Type, Row, Column) -> 
%% Maybe NULL or offset sometimes
 NewRow =
   if is_integer(Row) -> Row;
      true ->
        sdl:send_bin(Row, ?MODULE, ?LINE),
       0
   end,
%% Maybe NULL or offset sometimes
 NewColumn =
   if is_integer(Column) -> Column;
      true ->
        sdl:send_bin(Column, ?MODULE, ?LINE),
       0
   end,
 cast(?glSeparableFilter2D, [<<Target:32/?UN, Internalformat:32/?UN, Width:32/?SN, Height:32/?SN, Format:32/?UN, Type:32/?UN, NewRow:32/?SN, NewColumn:32/?SN>>]).

%% @spec getHistogram(Target::integer(), Reset::bool(), Format::integer(), Type::integer(), Values::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetHistogram">External manpage: getHistogram</a>
%% C-API func: void glGetHistogram(GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid * values)
getHistogram(Target, Reset, Format, Type, #sdlmem{bin=Values}) -> 
 sdl:send_bin(Values, ?MODULE, ?LINE),
 cast(?glGetHistogram, <<Target:32/?UN, Reset:8/unsigned, 0:24, Format:32/?UN, Type:32/?UN>>).

%% @spec getHistogramParameterfv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetHistogramParameterfv">External manpage: getHistogramParameterfv</a>
%% C-API func: void glGetHistogramParameterfv(GLenum target, GLenum pname, GLfloat * params)
getHistogramParameterfv(Target, Pname) -> 
 Bin = call(?glGetHistogramParameterfv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?FN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getHistogramParameteriv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetHistogramParameteriv">External manpage: getHistogramParameteriv</a>
%% C-API func: void glGetHistogramParameteriv(GLenum target, GLenum pname, GLint * params)
getHistogramParameteriv(Target, Pname) -> 
 Bin = call(?glGetHistogramParameteriv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getMinmax(Target::integer(), Reset::bool(), Format::integer(), Type::integer(), Values::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetMinmax">External manpage: getMinmax</a>
%% C-API func: void glGetMinmax(GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid * values)
getMinmax(Target, Reset, Format, Type, #sdlmem{bin=Values}) -> 
 sdl:send_bin(Values, ?MODULE, ?LINE),
 cast(?glGetMinmax, <<Target:32/?UN, Reset:8/unsigned, 0:24, Format:32/?UN, Type:32/?UN>>).

%% @spec getMinmaxParameterfv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetMinmaxParameterfv">External manpage: getMinmaxParameterfv</a>
%% C-API func: void glGetMinmaxParameterfv(GLenum target, GLenum pname, GLfloat * params)
getMinmaxParameterfv(Target, Pname) -> 
 Bin = call(?glGetMinmaxParameterfv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?FN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getMinmaxParameteriv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetMinmaxParameteriv">External manpage: getMinmaxParameteriv</a>
%% C-API func: void glGetMinmaxParameteriv(GLenum target, GLenum pname, GLint * params)
getMinmaxParameteriv(Target, Pname) -> 
 Bin = call(?glGetMinmaxParameteriv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec histogram(Target::integer(), Width::integer(), Internalformat::integer(), Sink::bool()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glHistogram">External manpage: histogram</a>
%% C-API func: void glHistogram(GLenum target, GLsizei width, GLenum internalformat, GLboolean sink)
histogram(Target, Width, Internalformat, Sink) -> 
 cast(?glHistogram, <<Target:32/?UN, Width:32/?SN, Internalformat:32/?UN, Sink:8/unsigned>>).

%% @spec minmax(Target::integer(), Internalformat::integer(), Sink::bool()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMinmax">External manpage: minmax</a>
%% C-API func: void glMinmax(GLenum target, GLenum internalformat, GLboolean sink)
minmax(Target, Internalformat, Sink) -> 
 cast(?glMinmax, <<Target:32/?UN, Internalformat:32/?UN, Sink:8/unsigned>>).

%% @spec resetHistogram(Target::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glResetHistogram">External manpage: resetHistogram</a>
%% C-API func: void glResetHistogram(GLenum target)
resetHistogram(Target) -> 
 cast(?glResetHistogram, <<Target:32/?UN>>).

%% @spec resetMinmax(Target::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glResetMinmax">External manpage: resetMinmax</a>
%% C-API func: void glResetMinmax(GLenum target)
resetMinmax(Target) -> 
 cast(?glResetMinmax, <<Target:32/?UN>>).

%% @spec texImage3D(Target::integer(), Level::integer(), Internalformat::integer(), Width::integer(), Height::integer(), Depth::integer(), Border::integer(), Format::integer(), Type::integer(), Pixels::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexImage3D">External manpage: texImage3D</a>
%% C-API func: void glTexImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type,  const GLvoid * pixels)
texImage3D(Target, Level, Internalformat, Width, Height, Depth, Border, Format, Type, Pixels) -> 
%% Maybe NULL or offset sometimes
 NewPixels =
   if is_integer(Pixels) -> Pixels;
      true ->
        sdl:send_bin(Pixels, ?MODULE, ?LINE),
       0
   end,
 cast(?glTexImage3D, [<<Target:32/?UN, Level:32/?SN, Internalformat:32/?SN, Width:32/?SN, Height:32/?SN, Depth:32/?SN, Border:32/?SN, Format:32/?UN, Type:32/?UN, NewPixels:32/?SN>>]).

%% @spec texSubImage3D(Target::integer(), Level::integer(), Xoffset::integer(), Yoffset::integer(), Zoffset::integer(), Width::integer(), Height::integer(), Depth::integer(), Format::integer(), Type::integer(), Pixels::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glTexSubImage3D">External manpage: texSubImage3D</a>
%% C-API func: void glTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,  const GLvoid * pixels)
texSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type, Pixels) -> 
%% Maybe NULL or offset sometimes
 NewPixels =
   if is_integer(Pixels) -> Pixels;
      true ->
        sdl:send_bin(Pixels, ?MODULE, ?LINE),
       0
   end,
 cast(?glTexSubImage3D, [<<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, Yoffset:32/?SN, Zoffset:32/?SN, Width:32/?SN, Height:32/?SN, Depth:32/?SN, Format:32/?UN, Type:32/?UN, NewPixels:32/?SN>>]).

%% @spec copyTexSubImage3D(Target::integer(), Level::integer(), Xoffset::integer(), Yoffset::integer(), Zoffset::integer(), X::integer(), Y::integer(), Width::integer(), Height::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCopyTexSubImage3D">External manpage: copyTexSubImage3D</a>
%% C-API func: void glCopyTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)
copyTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, X, Y, Width, Height) -> 
 cast(?glCopyTexSubImage3D, <<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, Yoffset:32/?SN, Zoffset:32/?SN, X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN>>).

%% @spec activeTexture(Texture::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glActiveTexture">External manpage: activeTexture</a>
%% C-API func: void glActiveTexture(GLenum texture)
activeTexture(Texture) -> 
 cast(?glActiveTexture, <<Texture:32/?UN>>).

%% @spec clientActiveTexture(Texture::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glClientActiveTexture">External manpage: clientActiveTexture</a>
%% C-API func: void glClientActiveTexture(GLenum texture)
clientActiveTexture(Texture) -> 
 cast(?glClientActiveTexture, <<Texture:32/?UN>>).

%% @spec multiTexCoord1d(Target::integer(), S::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord1d</a>
%% C-API func: void glMultiTexCoord1d(GLenum target, GLdouble s)
multiTexCoord1d(Target, S) -> 
 cast(?glMultiTexCoord1dv, <<Target:32/?UN, S:64/?FN>>).

%% @spec multiTexCoord1dv(Target::integer(), {V1::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord1dv</a>
%% C-API func: void glMultiTexCoord1dv(GLenum target,  const GLdouble * v)
multiTexCoord1dv(Target, {V1}) -> 
 cast(?glMultiTexCoord1dv, <<Target:32/?UN, V1:64/?FN>>).

%% @spec multiTexCoord1f(Target::integer(), S::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord1f</a>
%% C-API func: void glMultiTexCoord1f(GLenum target, GLfloat s)
multiTexCoord1f(Target, S) -> 
 cast(?glMultiTexCoord1fv, <<Target:32/?UN, S:32/?FN>>).

%% @spec multiTexCoord1fv(Target::integer(), {V1::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord1fv</a>
%% C-API func: void glMultiTexCoord1fv(GLenum target,  const GLfloat * v)
multiTexCoord1fv(Target, {V1}) -> 
 cast(?glMultiTexCoord1fv, <<Target:32/?UN, V1:32/?FN>>).

%% @spec multiTexCoord1i(Target::integer(), S::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord1i</a>
%% C-API func: void glMultiTexCoord1i(GLenum target, GLint s)
multiTexCoord1i(Target, S) -> 
 cast(?glMultiTexCoord1iv, <<Target:32/?UN, S:32/?SN>>).

%% @spec multiTexCoord1iv(Target::integer(), {V1::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord1iv</a>
%% C-API func: void glMultiTexCoord1iv(GLenum target,  const GLint * v)
multiTexCoord1iv(Target, {V1}) -> 
 cast(?glMultiTexCoord1iv, <<Target:32/?UN, V1:32/?SN>>).

%% @spec multiTexCoord1s(Target::integer(), S::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord1s</a>
%% C-API func: void glMultiTexCoord1s(GLenum target, GLshort s)
multiTexCoord1s(Target, S) -> 
 cast(?glMultiTexCoord1sv, <<Target:32/?UN, S:16/?SN>>).

%% @spec multiTexCoord1sv(Target::integer(), {V1::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord1sv</a>
%% C-API func: void glMultiTexCoord1sv(GLenum target,  const GLshort * v)
multiTexCoord1sv(Target, {V1}) -> 
 cast(?glMultiTexCoord1sv, <<Target:32/?UN, V1:16/?SN>>).

%% @spec multiTexCoord2d(Target::integer(), S::float(), T::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord2d</a>
%% C-API func: void glMultiTexCoord2d(GLenum target, GLdouble s, GLdouble t)
multiTexCoord2d(Target, S, T) -> 
 cast(?glMultiTexCoord2dv, <<Target:32/?UN, S:64/?FN, T:64/?FN>>).

%% @spec multiTexCoord2dv(Target::integer(), {V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord2dv</a>
%% C-API func: void glMultiTexCoord2dv(GLenum target,  const GLdouble * v)
multiTexCoord2dv(Target, {V1,V2}) -> 
 cast(?glMultiTexCoord2dv, <<Target:32/?UN, V1:64/?FN,V2:64/?FN>>).

%% @spec multiTexCoord2f(Target::integer(), S::float(), T::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord2f</a>
%% C-API func: void glMultiTexCoord2f(GLenum target, GLfloat s, GLfloat t)
multiTexCoord2f(Target, S, T) -> 
 cast(?glMultiTexCoord2fv, <<Target:32/?UN, S:32/?FN, T:32/?FN>>).

%% @spec multiTexCoord2fv(Target::integer(), {V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord2fv</a>
%% C-API func: void glMultiTexCoord2fv(GLenum target,  const GLfloat * v)
multiTexCoord2fv(Target, {V1,V2}) -> 
 cast(?glMultiTexCoord2fv, <<Target:32/?UN, V1:32/?FN,V2:32/?FN>>).

%% @spec multiTexCoord2i(Target::integer(), S::integer(), T::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord2i</a>
%% C-API func: void glMultiTexCoord2i(GLenum target, GLint s, GLint t)
multiTexCoord2i(Target, S, T) -> 
 cast(?glMultiTexCoord2iv, <<Target:32/?UN, S:32/?SN, T:32/?SN>>).

%% @spec multiTexCoord2iv(Target::integer(), {V1::integer(),V2::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord2iv</a>
%% C-API func: void glMultiTexCoord2iv(GLenum target,  const GLint * v)
multiTexCoord2iv(Target, {V1,V2}) -> 
 cast(?glMultiTexCoord2iv, <<Target:32/?UN, V1:32/?SN,V2:32/?SN>>).

%% @spec multiTexCoord2s(Target::integer(), S::integer(), T::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord2s</a>
%% C-API func: void glMultiTexCoord2s(GLenum target, GLshort s, GLshort t)
multiTexCoord2s(Target, S, T) -> 
 cast(?glMultiTexCoord2sv, <<Target:32/?UN, S:16/?SN, T:16/?SN>>).

%% @spec multiTexCoord2sv(Target::integer(), {V1::integer(),V2::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord2sv</a>
%% C-API func: void glMultiTexCoord2sv(GLenum target,  const GLshort * v)
multiTexCoord2sv(Target, {V1,V2}) -> 
 cast(?glMultiTexCoord2sv, <<Target:32/?UN, V1:16/?SN,V2:16/?SN>>).

%% @spec multiTexCoord3d(Target::integer(), S::float(), T::float(), R::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord3d</a>
%% C-API func: void glMultiTexCoord3d(GLenum target, GLdouble s, GLdouble t, GLdouble r)
multiTexCoord3d(Target, S, T, R) -> 
 cast(?glMultiTexCoord3dv, <<Target:32/?UN, S:64/?FN, T:64/?FN, R:64/?FN>>).

%% @spec multiTexCoord3dv(Target::integer(), {V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord3dv</a>
%% C-API func: void glMultiTexCoord3dv(GLenum target,  const GLdouble * v)
multiTexCoord3dv(Target, {V1,V2,V3}) -> 
 cast(?glMultiTexCoord3dv, <<Target:32/?UN, V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% @spec multiTexCoord3f(Target::integer(), S::float(), T::float(), R::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord3f</a>
%% C-API func: void glMultiTexCoord3f(GLenum target, GLfloat s, GLfloat t, GLfloat r)
multiTexCoord3f(Target, S, T, R) -> 
 cast(?glMultiTexCoord3fv, <<Target:32/?UN, S:32/?FN, T:32/?FN, R:32/?FN>>).

%% @spec multiTexCoord3fv(Target::integer(), {V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord3fv</a>
%% C-API func: void glMultiTexCoord3fv(GLenum target,  const GLfloat * v)
multiTexCoord3fv(Target, {V1,V2,V3}) -> 
 cast(?glMultiTexCoord3fv, <<Target:32/?UN, V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% @spec multiTexCoord3i(Target::integer(), S::integer(), T::integer(), R::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord3i</a>
%% C-API func: void glMultiTexCoord3i(GLenum target, GLint s, GLint t, GLint r)
multiTexCoord3i(Target, S, T, R) -> 
 cast(?glMultiTexCoord3iv, <<Target:32/?UN, S:32/?SN, T:32/?SN, R:32/?SN>>).

%% @spec multiTexCoord3iv(Target::integer(), {V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord3iv</a>
%% C-API func: void glMultiTexCoord3iv(GLenum target,  const GLint * v)
multiTexCoord3iv(Target, {V1,V2,V3}) -> 
 cast(?glMultiTexCoord3iv, <<Target:32/?UN, V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% @spec multiTexCoord3s(Target::integer(), S::integer(), T::integer(), R::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord3s</a>
%% C-API func: void glMultiTexCoord3s(GLenum target, GLshort s, GLshort t, GLshort r)
multiTexCoord3s(Target, S, T, R) -> 
 cast(?glMultiTexCoord3sv, <<Target:32/?UN, S:16/?SN, T:16/?SN, R:16/?SN>>).

%% @spec multiTexCoord3sv(Target::integer(), {V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord3sv</a>
%% C-API func: void glMultiTexCoord3sv(GLenum target,  const GLshort * v)
multiTexCoord3sv(Target, {V1,V2,V3}) -> 
 cast(?glMultiTexCoord3sv, <<Target:32/?UN, V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% @spec multiTexCoord4d(Target::integer(), S::float(), T::float(), R::float(), Q::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord4d</a>
%% C-API func: void glMultiTexCoord4d(GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q)
multiTexCoord4d(Target, S, T, R, Q) -> 
 cast(?glMultiTexCoord4dv, <<Target:32/?UN, S:64/?FN, T:64/?FN, R:64/?FN, Q:64/?FN>>).

%% @spec multiTexCoord4dv(Target::integer(), {V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord4dv</a>
%% C-API func: void glMultiTexCoord4dv(GLenum target,  const GLdouble * v)
multiTexCoord4dv(Target, {V1,V2,V3,V4}) -> 
 cast(?glMultiTexCoord4dv, <<Target:32/?UN, V1:64/?FN,V2:64/?FN,V3:64/?FN,V4:64/?FN>>).

%% @spec multiTexCoord4f(Target::integer(), S::float(), T::float(), R::float(), Q::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord4f</a>
%% C-API func: void glMultiTexCoord4f(GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q)
multiTexCoord4f(Target, S, T, R, Q) -> 
 cast(?glMultiTexCoord4fv, <<Target:32/?UN, S:32/?FN, T:32/?FN, R:32/?FN, Q:32/?FN>>).

%% @spec multiTexCoord4fv(Target::integer(), {V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord4fv</a>
%% C-API func: void glMultiTexCoord4fv(GLenum target,  const GLfloat * v)
multiTexCoord4fv(Target, {V1,V2,V3,V4}) -> 
 cast(?glMultiTexCoord4fv, <<Target:32/?UN, V1:32/?FN,V2:32/?FN,V3:32/?FN,V4:32/?FN>>).

%% @spec multiTexCoord4i(Target::integer(), S::integer(), T::integer(), R::integer(), Q::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord4i</a>
%% C-API func: void glMultiTexCoord4i(GLenum target, GLint s, GLint t, GLint r, GLint q)
multiTexCoord4i(Target, S, T, R, Q) -> 
 cast(?glMultiTexCoord4iv, <<Target:32/?UN, S:32/?SN, T:32/?SN, R:32/?SN, Q:32/?SN>>).

%% @spec multiTexCoord4iv(Target::integer(), {V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord4iv</a>
%% C-API func: void glMultiTexCoord4iv(GLenum target,  const GLint * v)
multiTexCoord4iv(Target, {V1,V2,V3,V4}) -> 
 cast(?glMultiTexCoord4iv, <<Target:32/?UN, V1:32/?SN,V2:32/?SN,V3:32/?SN,V4:32/?SN>>).

%% @spec multiTexCoord4s(Target::integer(), S::integer(), T::integer(), R::integer(), Q::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord4s</a>
%% C-API func: void glMultiTexCoord4s(GLenum target, GLshort s, GLshort t, GLshort r, GLshort q)
multiTexCoord4s(Target, S, T, R, Q) -> 
 cast(?glMultiTexCoord4sv, <<Target:32/?UN, S:16/?SN, T:16/?SN, R:16/?SN, Q:16/?SN>>).

%% @spec multiTexCoord4sv(Target::integer(), {V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiTexCoord">External manpage: multiTexCoord4sv</a>
%% C-API func: void glMultiTexCoord4sv(GLenum target,  const GLshort * v)
multiTexCoord4sv(Target, {V1,V2,V3,V4}) -> 
 cast(?glMultiTexCoord4sv, <<Target:32/?UN, V1:16/?SN,V2:16/?SN,V3:16/?SN,V4:16/?SN>>).

%% @spec loadTransposeMatrixf(M::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLoadTransposeMatrixf">External manpage: loadTransposeMatrixf</a>
%% C-API func: void glLoadTransposeMatrixf( const GLfloat * m)
loadTransposeMatrixf(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_FLOAT);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glLoadTransposeMatrixf, [ NewM]).

%% @spec loadTransposeMatrixd(M::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLoadTransposeMatrixd">External manpage: loadTransposeMatrixd</a>
%% C-API func: void glLoadTransposeMatrixd( const GLdouble * m)
loadTransposeMatrixd(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_DOUBLE);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glLoadTransposeMatrixd, [ NewM]).

%% @spec multTransposeMatrixf(M::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultTransposeMatrixf">External manpage: multTransposeMatrixf</a>
%% C-API func: void glMultTransposeMatrixf( const GLfloat * m)
multTransposeMatrixf(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_FLOAT);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glMultTransposeMatrixf, [ NewM]).

%% @spec multTransposeMatrixd(M::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultTransposeMatrixd">External manpage: multTransposeMatrixd</a>
%% C-API func: void glMultTransposeMatrixd( const GLdouble * m)
multTransposeMatrixd(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_DOUBLE);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glMultTransposeMatrixd, [ NewM]).

%% @spec sampleCoverage(Value::float(), Invert::bool()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSampleCoverage">External manpage: sampleCoverage</a>
%% C-API func: void glSampleCoverage(GLclampf value, GLboolean invert)
sampleCoverage(Value, Invert) -> 
 cast(?glSampleCoverage, <<Value:32/?FN, Invert:8/unsigned>>).

%% @spec compressedTexImage3D(Target::integer(), Level::integer(), Internalformat::integer(), Width::integer(), Height::integer(), Depth::integer(), Border::integer(), ImageSize::integer(), Data::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCompressedTexImage3D">External manpage: compressedTexImage3D</a>
%% C-API func: void glCompressedTexImage3D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize,  const GLvoid * data)
compressedTexImage3D(Target, Level, Internalformat, Width, Height, Depth, Border, ImageSize, Data) -> 
%% Maybe NULL or offset sometimes
 NewData =
   if is_integer(Data) -> Data;
      true ->
        sdl:send_bin(Data, ?MODULE, ?LINE),
       0
   end,
 cast(?glCompressedTexImage3D, [<<Target:32/?UN, Level:32/?SN, Internalformat:32/?UN, Width:32/?SN, Height:32/?SN, Depth:32/?SN, Border:32/?SN, ImageSize:32/?SN, NewData:32/?SN>>]).

%% @spec compressedTexImage2D(Target::integer(), Level::integer(), Internalformat::integer(), Width::integer(), Height::integer(), Border::integer(), ImageSize::integer(), Data::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCompressedTexImage2D">External manpage: compressedTexImage2D</a>
%% C-API func: void glCompressedTexImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize,  const GLvoid * data)
compressedTexImage2D(Target, Level, Internalformat, Width, Height, Border, ImageSize, Data) -> 
%% Maybe NULL or offset sometimes
 NewData =
   if is_integer(Data) -> Data;
      true ->
        sdl:send_bin(Data, ?MODULE, ?LINE),
       0
   end,
 cast(?glCompressedTexImage2D, [<<Target:32/?UN, Level:32/?SN, Internalformat:32/?UN, Width:32/?SN, Height:32/?SN, Border:32/?SN, ImageSize:32/?SN, NewData:32/?SN>>]).

%% @spec compressedTexImage1D(Target::integer(), Level::integer(), Internalformat::integer(), Width::integer(), Border::integer(), ImageSize::integer(), Data::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCompressedTexImage1D">External manpage: compressedTexImage1D</a>
%% C-API func: void glCompressedTexImage1D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize,  const GLvoid * data)
compressedTexImage1D(Target, Level, Internalformat, Width, Border, ImageSize, Data) -> 
%% Maybe NULL or offset sometimes
 NewData =
   if is_integer(Data) -> Data;
      true ->
        sdl:send_bin(Data, ?MODULE, ?LINE),
       0
   end,
 cast(?glCompressedTexImage1D, [<<Target:32/?UN, Level:32/?SN, Internalformat:32/?UN, Width:32/?SN, Border:32/?SN, ImageSize:32/?SN, NewData:32/?SN>>]).

%% @spec compressedTexSubImage3D(Target::integer(), Level::integer(), Xoffset::integer(), Yoffset::integer(), Zoffset::integer(), Width::integer(), Height::integer(), Depth::integer(), Format::integer(), ImageSize::integer(), Data::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCompressedTexSubImage3D">External manpage: compressedTexSubImage3D</a>
%% C-API func: void glCompressedTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize,  const GLvoid * data)
compressedTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, ImageSize, Data) -> 
%% Maybe NULL or offset sometimes
 NewData =
   if is_integer(Data) -> Data;
      true ->
        sdl:send_bin(Data, ?MODULE, ?LINE),
       0
   end,
 cast(?glCompressedTexSubImage3D, [<<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, Yoffset:32/?SN, Zoffset:32/?SN, Width:32/?SN, Height:32/?SN, Depth:32/?SN, Format:32/?UN, ImageSize:32/?SN, NewData:32/?SN>>]).

%% @spec compressedTexSubImage2D(Target::integer(), Level::integer(), Xoffset::integer(), Yoffset::integer(), Width::integer(), Height::integer(), Format::integer(), ImageSize::integer(), Data::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCompressedTexSubImage2D">External manpage: compressedTexSubImage2D</a>
%% C-API func: void glCompressedTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize,  const GLvoid * data)
compressedTexSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize, Data) -> 
%% Maybe NULL or offset sometimes
 NewData =
   if is_integer(Data) -> Data;
      true ->
        sdl:send_bin(Data, ?MODULE, ?LINE),
       0
   end,
 cast(?glCompressedTexSubImage2D, [<<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, Yoffset:32/?SN, Width:32/?SN, Height:32/?SN, Format:32/?UN, ImageSize:32/?SN, NewData:32/?SN>>]).

%% @spec compressedTexSubImage1D(Target::integer(), Level::integer(), Xoffset::integer(), Width::integer(), Format::integer(), ImageSize::integer(), Data::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCompressedTexSubImage1D">External manpage: compressedTexSubImage1D</a>
%% C-API func: void glCompressedTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize,  const GLvoid * data)
compressedTexSubImage1D(Target, Level, Xoffset, Width, Format, ImageSize, Data) -> 
%% Maybe NULL or offset sometimes
 NewData =
   if is_integer(Data) -> Data;
      true ->
        sdl:send_bin(Data, ?MODULE, ?LINE),
       0
   end,
 cast(?glCompressedTexSubImage1D, [<<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, Width:32/?SN, Format:32/?UN, ImageSize:32/?SN, NewData:32/?SN>>]).

%% @spec getCompressedTexImage(Target::integer(), Level::integer(), Img::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetCompressedTexImage">External manpage: getCompressedTexImage</a>
%% C-API func: void glGetCompressedTexImage(GLenum target, GLint level, GLvoid * img)
getCompressedTexImage(Target, Level, #sdlmem{bin=Img}) -> 
 sdl:send_bin(Img, ?MODULE, ?LINE),
 cast(?glGetCompressedTexImage, <<Target:32/?UN, Level:32/?SN>>).

%% @spec blendFuncSeparate(SfactorRGB::integer(), DfactorRGB::integer(), SfactorAlpha::integer(), DfactorAlpha::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBlendFuncSeparate">External manpage: blendFuncSeparate</a>
%% C-API func: void glBlendFuncSeparate(GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha)
blendFuncSeparate(SfactorRGB, DfactorRGB, SfactorAlpha, DfactorAlpha) -> 
 cast(?glBlendFuncSeparate, <<SfactorRGB:32/?UN, DfactorRGB:32/?UN, SfactorAlpha:32/?UN, DfactorAlpha:32/?UN>>).

%% @spec fogCoordf(Coord::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFogCoordf">External manpage: fogCoordf</a>
%% C-API func: void glFogCoordf(GLfloat coord)
fogCoordf(Coord) -> 
 cast(?glFogCoordf, <<Coord:32/?FN>>).

%% @spec fogCoordfv(Coord::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFogCoordfv">External manpage: fogCoordfv</a>
%% C-API func: void glFogCoordfv( const GLfloat * coord)
fogCoordfv(Coord) -> 
 NewCoord = if
	is_list(Coord) ; is_tuple(Coord) -> term2bin(Coord, 1, ?GL_FLOAT);
	binary(Coord) -> Coord;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Coord})
 end, 
 cast(?glFogCoordfv, [ NewCoord]).

%% @spec fogCoordd(Coord::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFogCoordd">External manpage: fogCoordd</a>
%% C-API func: void glFogCoordd(GLdouble coord)
fogCoordd(Coord) -> 
 cast(?glFogCoordd, <<Coord:64/?FN>>).

%% @spec fogCoorddv(Coord::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFogCoorddv">External manpage: fogCoorddv</a>
%% C-API func: void glFogCoorddv( const GLdouble * coord)
fogCoorddv(Coord) -> 
 NewCoord = if
	is_list(Coord) ; is_tuple(Coord) -> term2bin(Coord, 1, ?GL_DOUBLE);
	binary(Coord) -> Coord;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Coord})
 end, 
 cast(?glFogCoorddv, [ NewCoord]).

%% @spec fogCoordPointer(Type::integer(), Stride::integer(), Pointer::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFogCoordPointer">External manpage: fogCoordPointer</a>
%% C-API func: void glFogCoordPointer(GLenum type, GLsizei stride,  const GLvoid * pointer)
fogCoordPointer(Type, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glFogCoordPointer, [<<Type:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec multiDrawArrays(Mode::integer(), First::binary() | [integer()], Count::binary() | [integer()], Primcount::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMultiDrawArrays">External manpage: multiDrawArrays</a>
%% C-API func: void glMultiDrawArrays(GLenum mode,  const GLint * first,  const GLsizei * count, GLsizei primcount)
multiDrawArrays(Mode, First, Count, Primcount) -> 
 NewFirst = if
	is_list(First) ; is_tuple(First) -> term2bin(First, Primcount, ?GL_INT);
	is_binary(First) -> First;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, First})
 end, 
 NewCount = if
	is_list(Count) ; is_tuple(Count) -> term2bin(Count, Primcount, ?GL_UNSIGNED_INT);
	is_binary(Count) -> Count;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Count})
 end, 
 cast(?glMultiDrawArrays, [<<Primcount:32/?SN, Mode:32/?UN>>,NewFirst, NewCount]).

%% @spec pointParameterf(Pname::integer(), Param::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPointParameterf">External manpage: pointParameterf</a>
%% C-API func: void glPointParameterf(GLenum pname, GLfloat param)
pointParameterf(Pname, Param) -> 
 cast(?glPointParameterf, <<Pname:32/?UN, Param:32/?FN>>).

%% @spec pointParameterfv(Pname::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPointParameterfv">External manpage: pointParameterfv</a>
%% C-API func: void glPointParameterfv(GLenum pname,  const GLfloat * params)
pointParameterfv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 3, ?GL_FLOAT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glPointParameterfv, [<<Pname:32/?UN>>,NewParams]).

%% @spec pointParameteri(Pname::integer(), Param::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPointParameteri">External manpage: pointParameteri</a>
%% C-API func: void glPointParameteri(GLenum pname, GLint param)
pointParameteri(Pname, Param) -> 
 cast(?glPointParameteri, <<Pname:32/?UN, Param:32/?SN>>).

%% @spec pointParameteriv(Pname::integer(), Params::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glPointParameteriv">External manpage: pointParameteriv</a>
%% C-API func: void glPointParameteriv(GLenum pname,  const GLint * params)
pointParameteriv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 3, ?GL_INT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glPointParameteriv, [<<Pname:32/?UN>>,NewParams]).

%% @spec secondaryColor3b(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3b</a>
%% C-API func: void glSecondaryColor3b(GLbyte red, GLbyte green, GLbyte blue)
secondaryColor3b(Red, Green, Blue) -> 
 cast(?glSecondaryColor3bv, <<Red:8/signed, Green:8/signed, Blue:8/signed>>).

%% @spec secondaryColor3bv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3bv</a>
%% C-API func: void glSecondaryColor3bv( const GLbyte * v)
secondaryColor3bv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3bv, <<V1:8/signed,V2:8/signed,V3:8/signed>>).

%% @spec secondaryColor3d(Red::float(), Green::float(), Blue::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3d</a>
%% C-API func: void glSecondaryColor3d(GLdouble red, GLdouble green, GLdouble blue)
secondaryColor3d(Red, Green, Blue) -> 
 cast(?glSecondaryColor3dv, <<Red:64/?FN, Green:64/?FN, Blue:64/?FN>>).

%% @spec secondaryColor3dv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3dv</a>
%% C-API func: void glSecondaryColor3dv( const GLdouble * v)
secondaryColor3dv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% @spec secondaryColor3f(Red::float(), Green::float(), Blue::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3f</a>
%% C-API func: void glSecondaryColor3f(GLfloat red, GLfloat green, GLfloat blue)
secondaryColor3f(Red, Green, Blue) -> 
 cast(?glSecondaryColor3fv, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN>>).

%% @spec secondaryColor3fv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3fv</a>
%% C-API func: void glSecondaryColor3fv( const GLfloat * v)
secondaryColor3fv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% @spec secondaryColor3i(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3i</a>
%% C-API func: void glSecondaryColor3i(GLint red, GLint green, GLint blue)
secondaryColor3i(Red, Green, Blue) -> 
 cast(?glSecondaryColor3iv, <<Red:32/?SN, Green:32/?SN, Blue:32/?SN>>).

%% @spec secondaryColor3iv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3iv</a>
%% C-API func: void glSecondaryColor3iv( const GLint * v)
secondaryColor3iv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% @spec secondaryColor3s(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3s</a>
%% C-API func: void glSecondaryColor3s(GLshort red, GLshort green, GLshort blue)
secondaryColor3s(Red, Green, Blue) -> 
 cast(?glSecondaryColor3sv, <<Red:16/?SN, Green:16/?SN, Blue:16/?SN>>).

%% @spec secondaryColor3sv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3sv</a>
%% C-API func: void glSecondaryColor3sv( const GLshort * v)
secondaryColor3sv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% @spec secondaryColor3ub(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3ub</a>
%% C-API func: void glSecondaryColor3ub(GLubyte red, GLubyte green, GLubyte blue)
secondaryColor3ub(Red, Green, Blue) -> 
 cast(?glSecondaryColor3ubv, <<Red:8/unsigned, Green:8/unsigned, Blue:8/unsigned>>).

%% @spec secondaryColor3ubv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3ubv</a>
%% C-API func: void glSecondaryColor3ubv( const GLubyte * v)
secondaryColor3ubv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3ubv, <<V1:8/unsigned,V2:8/unsigned,V3:8/unsigned>>).

%% @spec secondaryColor3ui(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3ui</a>
%% C-API func: void glSecondaryColor3ui(GLuint red, GLuint green, GLuint blue)
secondaryColor3ui(Red, Green, Blue) -> 
 cast(?glSecondaryColor3uiv, <<Red:32/?UN, Green:32/?UN, Blue:32/?UN>>).

%% @spec secondaryColor3uiv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3uiv</a>
%% C-API func: void glSecondaryColor3uiv( const GLuint * v)
secondaryColor3uiv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3uiv, <<V1:32/?UN,V2:32/?UN,V3:32/?UN>>).

%% @spec secondaryColor3us(Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3us</a>
%% C-API func: void glSecondaryColor3us(GLushort red, GLushort green, GLushort blue)
secondaryColor3us(Red, Green, Blue) -> 
 cast(?glSecondaryColor3usv, <<Red:16/?UN, Green:16/?UN, Blue:16/?UN>>).

%% @spec secondaryColor3usv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColor">External manpage: secondaryColor3usv</a>
%% C-API func: void glSecondaryColor3usv( const GLushort * v)
secondaryColor3usv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3usv, <<V1:16/?UN,V2:16/?UN,V3:16/?UN>>).

%% @spec secondaryColorPointer(Size::integer(), Type::integer(), Stride::integer(), Pointer::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glSecondaryColorPointer">External manpage: secondaryColorPointer</a>
%% C-API func: void glSecondaryColorPointer(GLint size, GLenum type, GLsizei stride,  const GLvoid * pointer)
secondaryColorPointer(Size, Type, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glSecondaryColorPointer, [<<Size:32/?SN, Type:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec windowPos2d(X::float(), Y::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos2d</a>
%% C-API func: void glWindowPos2d(GLdouble x, GLdouble y)
windowPos2d(X, Y) -> 
 cast(?glWindowPos2dv, <<X:64/?FN, Y:64/?FN>>).

%% @spec windowPos2dv({V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos2dv</a>
%% C-API func: void glWindowPos2dv( const GLdouble * v)
windowPos2dv({V1,V2}) -> 
 cast(?glWindowPos2dv, <<V1:64/?FN,V2:64/?FN>>).

%% @spec windowPos2f(X::float(), Y::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos2f</a>
%% C-API func: void glWindowPos2f(GLfloat x, GLfloat y)
windowPos2f(X, Y) -> 
 cast(?glWindowPos2fv, <<X:32/?FN, Y:32/?FN>>).

%% @spec windowPos2fv({V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos2fv</a>
%% C-API func: void glWindowPos2fv( const GLfloat * v)
windowPos2fv({V1,V2}) -> 
 cast(?glWindowPos2fv, <<V1:32/?FN,V2:32/?FN>>).

%% @spec windowPos2i(X::integer(), Y::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos2i</a>
%% C-API func: void glWindowPos2i(GLint x, GLint y)
windowPos2i(X, Y) -> 
 cast(?glWindowPos2iv, <<X:32/?SN, Y:32/?SN>>).

%% @spec windowPos2iv({V1::integer(),V2::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos2iv</a>
%% C-API func: void glWindowPos2iv( const GLint * v)
windowPos2iv({V1,V2}) -> 
 cast(?glWindowPos2iv, <<V1:32/?SN,V2:32/?SN>>).

%% @spec windowPos2s(X::integer(), Y::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos2s</a>
%% C-API func: void glWindowPos2s(GLshort x, GLshort y)
windowPos2s(X, Y) -> 
 cast(?glWindowPos2sv, <<X:16/?SN, Y:16/?SN>>).

%% @spec windowPos2sv({V1::integer(),V2::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos2sv</a>
%% C-API func: void glWindowPos2sv( const GLshort * v)
windowPos2sv({V1,V2}) -> 
 cast(?glWindowPos2sv, <<V1:16/?SN,V2:16/?SN>>).

%% @spec windowPos3d(X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos3d</a>
%% C-API func: void glWindowPos3d(GLdouble x, GLdouble y, GLdouble z)
windowPos3d(X, Y, Z) -> 
 cast(?glWindowPos3dv, <<X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% @spec windowPos3dv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos3dv</a>
%% C-API func: void glWindowPos3dv( const GLdouble * v)
windowPos3dv({V1,V2,V3}) -> 
 cast(?glWindowPos3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% @spec windowPos3f(X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos3f</a>
%% C-API func: void glWindowPos3f(GLfloat x, GLfloat y, GLfloat z)
windowPos3f(X, Y, Z) -> 
 cast(?glWindowPos3fv, <<X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% @spec windowPos3fv({V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos3fv</a>
%% C-API func: void glWindowPos3fv( const GLfloat * v)
windowPos3fv({V1,V2,V3}) -> 
 cast(?glWindowPos3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% @spec windowPos3i(X::integer(), Y::integer(), Z::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos3i</a>
%% C-API func: void glWindowPos3i(GLint x, GLint y, GLint z)
windowPos3i(X, Y, Z) -> 
 cast(?glWindowPos3iv, <<X:32/?SN, Y:32/?SN, Z:32/?SN>>).

%% @spec windowPos3iv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos3iv</a>
%% C-API func: void glWindowPos3iv( const GLint * v)
windowPos3iv({V1,V2,V3}) -> 
 cast(?glWindowPos3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% @spec windowPos3s(X::integer(), Y::integer(), Z::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos3s</a>
%% C-API func: void glWindowPos3s(GLshort x, GLshort y, GLshort z)
windowPos3s(X, Y, Z) -> 
 cast(?glWindowPos3sv, <<X:16/?SN, Y:16/?SN, Z:16/?SN>>).

%% @spec windowPos3sv({V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWindowPos">External manpage: windowPos3sv</a>
%% C-API func: void glWindowPos3sv( const GLshort * v)
windowPos3sv({V1,V2,V3}) -> 
 cast(?glWindowPos3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% @spec genQueries(N::integer()) -> Ids::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGenQueries">External manpage: genQueries</a>
%% C-API func: void glGenQueries(GLsizei n, GLuint * ids)
genQueries(N) -> 
 Bin = call(?glGenQueries, <<N:32/?SN>>), 
 case Bin of 
	<<Ids:N/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 bin2list(N, ?GL_UNSIGNED_INT, Ids);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec deleteQueries(N::integer(), Ids::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDeleteQueries">External manpage: deleteQueries</a>
%% C-API func: void glDeleteQueries(GLsizei n,  const GLuint * ids)
deleteQueries(N, Ids) -> 
 NewIds = if
	is_list(Ids) ; is_tuple(Ids) -> term2bin(Ids, N, ?GL_UNSIGNED_INT);
	is_binary(Ids) -> Ids;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Ids})
 end, 
 cast(?glDeleteQueries, [<<N:32/?SN>>,NewIds]).

%% @spec isQuery(Id::integer()) -> bool()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIsQuery">External manpage: isQuery</a>
%% C-API func: GLboolean glIsQuery(GLuint id)
isQuery(Id) -> 
 Bin = call(?glIsQuery, <<Id:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec beginQuery(Target::integer(), Id::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBeginQuery">External manpage: beginQuery</a>
%% C-API func: void glBeginQuery(GLenum target, GLuint id)
beginQuery(Target, Id) -> 
 cast(?glBeginQuery, <<Target:32/?UN, Id:32/?UN>>).

%% @spec endQuery(Target::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEndQuery">External manpage: endQuery</a>
%% C-API func: void glEndQuery(GLenum target)
endQuery(Target) -> 
 cast(?glEndQuery, <<Target:32/?UN>>).

%% @spec getQueryiv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetQueryiv">External manpage: getQueryiv</a>
%% C-API func: void glGetQueryiv(GLenum target, GLenum pname, GLint * params)
getQueryiv(Target, Pname) -> 
 Bin = call(?glGetQueryiv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getQueryObjectiv(Id::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetQueryObjectiv">External manpage: getQueryObjectiv</a>
%% C-API func: void glGetQueryObjectiv(GLuint id, GLenum pname, GLint * params)
getQueryObjectiv(Id, Pname) -> 
 Bin = call(?glGetQueryObjectiv, <<Id:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getQueryObjectuiv(Id::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetQueryObjectuiv">External manpage: getQueryObjectuiv</a>
%% C-API func: void glGetQueryObjectuiv(GLuint id, GLenum pname, GLuint * params)
getQueryObjectuiv(Id, Pname) -> 
 Bin = call(?glGetQueryObjectuiv, <<Id:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?UN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec bindBuffer(Target::integer(), Buffer::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBindBuffer">External manpage: bindBuffer</a>
%% C-API func: void glBindBuffer(GLenum target, GLuint buffer)
bindBuffer(Target, Buffer) -> 
 cast(?glBindBuffer, <<Target:32/?UN, Buffer:32/?UN>>).

%% @spec deleteBuffers(N::integer(), Buffers::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDeleteBuffers">External manpage: deleteBuffers</a>
%% C-API func: void glDeleteBuffers(GLsizei n,  const GLuint * buffers)
deleteBuffers(N, Buffers) -> 
 NewBuffers = if
	is_list(Buffers) ; is_tuple(Buffers) -> term2bin(Buffers, N, ?GL_UNSIGNED_INT);
	is_binary(Buffers) -> Buffers;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Buffers})
 end, 
 cast(?glDeleteBuffers, [<<N:32/?SN>>,NewBuffers]).

%% @spec genBuffers(N::integer()) -> Buffers::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGenBuffers">External manpage: genBuffers</a>
%% C-API func: void glGenBuffers(GLsizei n, GLuint * buffers)
genBuffers(N) -> 
 Bin = call(?glGenBuffers, <<N:32/?SN>>), 
 case Bin of 
	<<Buffers:N/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 bin2list(N, ?GL_UNSIGNED_INT, Buffers);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec isBuffer(Buffer::integer()) -> bool()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIsBuffer">External manpage: isBuffer</a>
%% C-API func: GLboolean glIsBuffer(GLuint buffer)
isBuffer(Buffer) -> 
 Bin = call(?glIsBuffer, <<Buffer:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec bufferData(Target::integer(), Size::integer(), Data::binary() | [number()], Usage::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBufferData">External manpage: bufferData</a>
%% C-API func: void glBufferData(GLenum target, GLsizeiptr size,  const GLvoid * data, GLenum usage)
bufferData(Target, Size, Data, Usage) -> 
%% Maybe NULL or offset sometimes
 NewData =
   if is_integer(Data) -> Data;
      true ->
        sdl:send_bin(Data, ?MODULE, ?LINE),
       0
   end,
 cast(?glBufferData, [<<Target:32/?UN, Size:32/?UN, NewData:32/?SN, Usage:32/?UN>>]).

%% @spec bufferSubData(Target::integer(), Offset::integer(), Size::integer(), Data::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBufferSubData">External manpage: bufferSubData</a>
%% C-API func: void glBufferSubData(GLenum target, GLintptr offset, GLsizeiptr size,  const GLvoid * data)
bufferSubData(Target, Offset, Size, Data) -> 
%% Maybe NULL or offset sometimes
 NewData =
   if is_integer(Data) -> Data;
      true ->
        sdl:send_bin(Data, ?MODULE, ?LINE),
       0
   end,
 cast(?glBufferSubData, [<<Target:32/?UN, Offset:32/?UN, Size:32/?UN, NewData:32/?SN>>]).

%% @spec getBufferSubData(Target::integer(), Offset::integer(), Size::integer(), Data::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetBufferSubData">External manpage: getBufferSubData</a>
%% C-API func: void glGetBufferSubData(GLenum target, GLintptr offset, GLsizeiptr size, GLvoid * data)
getBufferSubData(Target, Offset, Size, #sdlmem{bin=Data}) -> 
 sdl:send_bin(Data, ?MODULE, ?LINE),
 cast(?glGetBufferSubData, <<Target:32/?UN, Offset:32/?UN, Size:32/?UN>>).

%% @spec unmapBuffer(Target::integer()) -> bool()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUnmapBuffer">External manpage: unmapBuffer</a>
%% C-API func: GLboolean glUnmapBuffer(GLenum target)
unmapBuffer(Target) -> 
 Bin = call(?glUnmapBuffer, <<Target:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getBufferParameteriv(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetBufferParameteriv">External manpage: getBufferParameteriv</a>
%% C-API func: void glGetBufferParameteriv(GLenum target, GLenum pname, GLint * params)
getBufferParameteriv(Target, Pname) -> 
 Bin = call(?glGetBufferParameteriv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getBufferPointerv(Target::integer(), Pname::integer()) -> Params::sdlmem()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetBufferPointerv">External manpage: getBufferPointerv</a>
%% C-API func: void glGetBufferPointerv(GLenum target, GLenum pname,  GLvoid* *params)
getBufferPointerv(Target, Pname) -> 
 Bin = call(?glGetBufferPointerv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/big-unsigned>> -> 
	 erlang:fault({nyi, ?MODULE,?LINE});
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec blendEquationSeparate(ModeRGB::integer(), ModeAlpha::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBlendEquationSeparate">External manpage: blendEquationSeparate</a>
%% C-API func: void glBlendEquationSeparate(GLenum modeRGB, GLenum modeAlpha)
blendEquationSeparate(ModeRGB, ModeAlpha) -> 
 cast(?glBlendEquationSeparate, <<ModeRGB:32/?UN, ModeAlpha:32/?UN>>).

%% @spec drawBuffers(N::integer(), Bufs::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDrawBuffers">External manpage: drawBuffers</a>
%% C-API func: void glDrawBuffers(GLsizei n,  const GLenum * bufs)
drawBuffers(N, Bufs) -> 
 NewBufs = if
	is_list(Bufs) ; is_tuple(Bufs) -> term2bin(Bufs, N, ?GL_INT);
	is_binary(Bufs) -> Bufs;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Bufs})
 end, 
 cast(?glDrawBuffers, [<<N:32/?SN>>,NewBufs]).

%% @spec stencilOpSeparate(Face::integer(), Sfail::integer(), Dpfail::integer(), Dppass::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glStencilOpSeparate">External manpage: stencilOpSeparate</a>
%% C-API func: void glStencilOpSeparate(GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass)
stencilOpSeparate(Face, Sfail, Dpfail, Dppass) -> 
 cast(?glStencilOpSeparate, <<Face:32/?UN, Sfail:32/?UN, Dpfail:32/?UN, Dppass:32/?UN>>).

%% @spec stencilFuncSeparate(Frontfunc::integer(), Backfunc::integer(), Ref::integer(), Mask::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glStencilFuncSeparate">External manpage: stencilFuncSeparate</a>
%% C-API func: void glStencilFuncSeparate(GLenum frontfunc, GLenum backfunc, GLint ref, GLuint mask)
stencilFuncSeparate(Frontfunc, Backfunc, Ref, Mask) -> 
 cast(?glStencilFuncSeparate, <<Frontfunc:32/?UN, Backfunc:32/?UN, Ref:32/?SN, Mask:32/?UN>>).

%% @spec stencilMaskSeparate(Face::integer(), Mask::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glStencilMaskSeparate">External manpage: stencilMaskSeparate</a>
%% C-API func: void glStencilMaskSeparate(GLenum face, GLuint mask)
stencilMaskSeparate(Face, Mask) -> 
 cast(?glStencilMaskSeparate, <<Face:32/?UN, Mask:32/?UN>>).

%% @spec attachShader(Program::integer(), Shader::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glAttachShader">External manpage: attachShader</a>
%% C-API func: void glAttachShader(GLuint program, GLuint shader)
attachShader(Program, Shader) -> 
 cast(?glAttachShader, <<Program:32/?UN, Shader:32/?UN>>).

%% @spec bindAttribLocation(Program::integer(), Index::integer(), Name::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBindAttribLocation">External manpage: bindAttribLocation</a>
%% C-API func: void glBindAttribLocation(GLuint program, GLuint index,  const GLchar * name)
bindAttribLocation(Program, Index, Name) -> 
 sdl:send_bin(list_to_binary([Name,0]), ?MODULE, ?LINE),
 cast(?glBindAttribLocation, <<Program:32/?UN, Index:32/?UN>>).

%% @spec compileShader(Shader::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCompileShader">External manpage: compileShader</a>
%% C-API func: void glCompileShader(GLuint shader)
compileShader(Shader) -> 
 cast(?glCompileShader, <<Shader:32/?UN>>).

%% @spec createProgram() -> integer()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCreateProgram">External manpage: createProgram</a>
%% C-API func: GLuint glCreateProgram()
createProgram() -> 
 Bin = call(?glCreateProgram, []), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec createShader(Type::integer()) -> integer()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCreateShader">External manpage: createShader</a>
%% C-API func: GLuint glCreateShader(GLenum type)
createShader(Type) -> 
 Bin = call(?glCreateShader, <<Type:32/?UN>>), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec deleteProgram(Program::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDeleteProgram">External manpage: deleteProgram</a>
%% C-API func: void glDeleteProgram(GLuint program)
deleteProgram(Program) -> 
 cast(?glDeleteProgram, <<Program:32/?UN>>).

%% @spec deleteShader(Shader::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDeleteShader">External manpage: deleteShader</a>
%% C-API func: void glDeleteShader(GLuint shader)
deleteShader(Shader) -> 
 cast(?glDeleteShader, <<Shader:32/?UN>>).

%% @spec detachShader(Program::integer(), Shader::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDetachShader">External manpage: detachShader</a>
%% C-API func: void glDetachShader(GLuint program, GLuint shader)
detachShader(Program, Shader) -> 
 cast(?glDetachShader, <<Program:32/?UN, Shader:32/?UN>>).

%% @spec disableVertexAttribArray(Index::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDisableVertexAttribArray">External manpage: disableVertexAttribArray</a>
%% C-API func: void glDisableVertexAttribArray(GLuint index)
disableVertexAttribArray(Index) -> 
 cast(?glDisableVertexAttribArray, <<Index:32/?UN>>).

%% @spec enableVertexAttribArray(Index::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glEnableVertexAttribArray">External manpage: enableVertexAttribArray</a>
%% C-API func: void glEnableVertexAttribArray(GLuint index)
enableVertexAttribArray(Index) -> 
 cast(?glEnableVertexAttribArray, <<Index:32/?UN>>).

%% @spec getActiveAttrib(Program::integer(), Index::integer(), BufSize::integer()) -> {[Length::integer()], [Size::integer()], [Type::integer()], [Name::integer()]}
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetActiveAttrib">External manpage: getActiveAttrib</a>
%% C-API func: void glGetActiveAttrib(GLuint program, GLuint index, GLsizei bufSize, GLsizei * length, GLint * size, GLenum * type, GLchar * name)
getActiveAttrib(Program, Index, BufSize) -> 
 Bin = call(?glGetActiveAttrib, <<Program:32/?UN, Index:32/?UN, BufSize:32/?SN>>), 
 case Bin of 
	<<Length:32/?SN, Size:32/?SN, Type:32/?UN, Name:Length/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 {Length, Size, Type, bin2list(Length, ?GL_UNSIGNED_BYTE, Name)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getActiveUniform(Program::integer(), Index::integer(), BufSize::integer()) -> {[Length::integer()], [Size::integer()], [Type::integer()], [Name::integer()]}
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetActiveUniform">External manpage: getActiveUniform</a>
%% C-API func: void glGetActiveUniform(GLuint program, GLuint index, GLsizei bufSize, GLsizei * length, GLint * size, GLenum * type, GLchar * name)
getActiveUniform(Program, Index, BufSize) -> 
 Bin = call(?glGetActiveUniform, <<Program:32/?UN, Index:32/?UN, BufSize:32/?SN>>), 
 case Bin of 
	<<Length:32/?SN, Size:32/?SN, Type:32/?UN, Name:Length/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 {Length, Size, Type, bin2list(Length, ?GL_UNSIGNED_BYTE, Name)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getAttachedShaders(Program::integer(), MaxCount::integer()) -> {[Count::integer()], [Obj::integer()]}
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetAttachedShaders">External manpage: getAttachedShaders</a>
%% C-API func: void glGetAttachedShaders(GLuint program, GLsizei maxCount, GLsizei * count, GLuint * obj)
getAttachedShaders(Program, MaxCount) -> 
 Bin = call(?glGetAttachedShaders, <<Program:32/?UN, MaxCount:32/?SN>>), 
 case Bin of 
	<<Count:32/?SN, Obj:Count/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 {Count, bin2list(Count, ?GL_UNSIGNED_INT, Obj)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getAttribLocation(Program::integer(), Name::binary() | [integer()]) -> integer()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetAttribLocation">External manpage: getAttribLocation</a>
%% C-API func: GLint glGetAttribLocation(GLuint program,  const GLchar * name)
getAttribLocation(Program, Name) -> 
 sdl:send_bin(list_to_binary([Name,0]), ?MODULE, ?LINE),
 Bin = call(?glGetAttribLocation, <<Program:32/?UN>>), 
 case Bin of 
	<<Ret:32/?SN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getProgramiv(Program::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetProgramiv">External manpage: getProgramiv</a>
%% C-API func: void glGetProgramiv(GLuint program, GLenum pname, GLint * params)
getProgramiv(Program, Pname) -> 
 Bin = call(?glGetProgramiv, <<Program:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getProgramInfoLog(Program::integer(), BufSize::integer()) -> {[Length::integer()], [InfoLog::integer()]}
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetProgramInfoLog">External manpage: getProgramInfoLog</a>
%% C-API func: void glGetProgramInfoLog(GLuint program, GLsizei bufSize, GLsizei * length, GLchar * infoLog)
getProgramInfoLog(Program, BufSize) -> 
 Bin = call(?glGetProgramInfoLog, <<Program:32/?UN, BufSize:32/?SN>>), 
 case Bin of 
	<<Length:32/?SN, InfoLog:Length/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 {Length, bin2list(Length, ?GL_UNSIGNED_BYTE, InfoLog)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getShaderiv(Shader::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetShaderiv">External manpage: getShaderiv</a>
%% C-API func: void glGetShaderiv(GLuint shader, GLenum pname, GLint * params)
getShaderiv(Shader, Pname) -> 
 Bin = call(?glGetShaderiv, <<Shader:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getShaderInfoLog(Shader::integer(), BufSize::integer()) -> {[Length::integer()], [InfoLog::integer()]}
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetShaderInfoLog">External manpage: getShaderInfoLog</a>
%% C-API func: void glGetShaderInfoLog(GLuint shader, GLsizei bufSize, GLsizei * length, GLchar * infoLog)
getShaderInfoLog(Shader, BufSize) -> 
 Bin = call(?glGetShaderInfoLog, <<Shader:32/?UN, BufSize:32/?SN>>), 
 case Bin of 
	<<Length:32/?SN, InfoLog:Length/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 {Length, bin2list(Length, ?GL_UNSIGNED_BYTE, InfoLog)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getShaderSource(Shader::integer(), BufSize::integer()) -> {[Length::integer()], [Source::integer()]}
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetShaderSource">External manpage: getShaderSource</a>
%% C-API func: void glGetShaderSource(GLuint shader, GLsizei bufSize, GLsizei * length, GLchar * source)
getShaderSource(Shader, BufSize) -> 
 Bin = call(?glGetShaderSource, <<Shader:32/?UN, BufSize:32/?SN>>), 
 case Bin of 
	<<Length:32/?SN, Source:Length/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 {Length, bin2list(Length, ?GL_UNSIGNED_BYTE, Source)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getUniformLocation(Program::integer(), Name::binary() | [integer()]) -> integer()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetUniformLocation">External manpage: getUniformLocation</a>
%% C-API func: GLint glGetUniformLocation(GLuint program,  const GLchar * name)
getUniformLocation(Program, Name) -> 
 sdl:send_bin(list_to_binary([Name,0]), ?MODULE, ?LINE),
 Bin = call(?glGetUniformLocation, <<Program:32/?UN>>), 
 case Bin of 
	<<Ret:32/?SN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getUniformfv(Program::integer(), Location::integer(), Params::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetUniformfv">External manpage: getUniformfv</a>
%% C-API func: void glGetUniformfv(GLuint program, GLint location, GLfloat * params)
getUniformfv(Program, Location, #sdlmem{bin=Params}) -> 
 sdl:send_bin(Params, ?MODULE, ?LINE),
 cast(?glGetUniformfv, <<Program:32/?UN, Location:32/?SN>>).

%% @spec getUniformiv(Program::integer(), Location::integer(), Params::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetUniformiv">External manpage: getUniformiv</a>
%% C-API func: void glGetUniformiv(GLuint program, GLint location, GLint * params)
getUniformiv(Program, Location, #sdlmem{bin=Params}) -> 
 sdl:send_bin(Params, ?MODULE, ?LINE),
 cast(?glGetUniformiv, <<Program:32/?UN, Location:32/?SN>>).

%% @spec getVertexAttribdv(Index::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetVertexAttribdv">External manpage: getVertexAttribdv</a>
%% C-API func: void glGetVertexAttribdv(GLuint index, GLenum pname, GLdouble * params)
getVertexAttribdv(Index, Pname) -> 
 Bin = call(?glGetVertexAttribdv, <<Index:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_DOUBLE_SIZE>> -> 
	 bin2list(4, ?GL_DOUBLE, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getVertexAttribfv(Index::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetVertexAttribfv">External manpage: getVertexAttribfv</a>
%% C-API func: void glGetVertexAttribfv(GLuint index, GLenum pname, GLfloat * params)
getVertexAttribfv(Index, Pname) -> 
 Bin = call(?glGetVertexAttribfv, <<Index:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(4, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getVertexAttribiv(Index::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetVertexAttribiv">External manpage: getVertexAttribiv</a>
%% C-API func: void glGetVertexAttribiv(GLuint index, GLenum pname, GLint * params)
getVertexAttribiv(Index, Pname) -> 
 Bin = call(?glGetVertexAttribiv, <<Index:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_INT_SIZE>> -> 
	 bin2list(4, ?GL_INT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getVertexAttribPointerv(Index::integer(), Pname::integer()) -> Pointer::sdlmem()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetVertexAttribPointerv">External manpage: getVertexAttribPointerv</a>
%% C-API func: void glGetVertexAttribPointerv(GLuint index, GLenum pname,  GLvoid* *pointer)
getVertexAttribPointerv(Index, Pname) -> 
 Bin = call(?glGetVertexAttribPointerv, <<Index:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Pointer:32/big-unsigned>> -> 
	 erlang:fault({nyi, ?MODULE,?LINE});
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec isProgram(Program::integer()) -> bool()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIsProgram">External manpage: isProgram</a>
%% C-API func: GLboolean glIsProgram(GLuint program)
isProgram(Program) -> 
 Bin = call(?glIsProgram, <<Program:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec isShader(Shader::integer()) -> bool()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIsShader">External manpage: isShader</a>
%% C-API func: GLboolean glIsShader(GLuint shader)
isShader(Shader) -> 
 Bin = call(?glIsShader, <<Shader:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec linkProgram(Program::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glLinkProgram">External manpage: linkProgram</a>
%% C-API func: void glLinkProgram(GLuint program)
linkProgram(Program) -> 
 cast(?glLinkProgram, <<Program:32/?UN>>).

%% @spec shaderSource(Shader::integer(), Count::integer(), String::[binary()], Length::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glShaderSource">External manpage: shaderSource</a>
%% C-API func: void glShaderSource(GLuint shader, GLsizei count,  const GLchar* *string,  const GLint * length)
shaderSource(Shader, Count, String, Length) -> 
 lists:foreach(fun(Values) -> sdl:send_bin(list_to_binary([Values,0]), ?MODULE, ?LINE) end, String),
 NewLength = if
	is_list(Length) ; is_tuple(Length) -> term2bin(Length, Count, ?GL_INT);
	is_binary(Length) -> Length;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Length})
 end, 
 cast(?glShaderSource, [<<Shader:32/?UN, Count:32/?SN>>,NewLength]).

%% @spec useProgram(Program::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUseProgram">External manpage: useProgram</a>
%% C-API func: void glUseProgram(GLuint program)
useProgram(Program) -> 
 cast(?glUseProgram, <<Program:32/?UN>>).

%% @spec uniform1f(Location::integer(), V0::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform1f">External manpage: uniform1f</a>
%% C-API func: void glUniform1f(GLint location, GLfloat v0)
uniform1f(Location, V0) -> 
 cast(?glUniform1f, <<Location:32/?SN, V0:32/?FN>>).

%% @spec uniform2f(Location::integer(), V0::float(), V1::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform2f">External manpage: uniform2f</a>
%% C-API func: void glUniform2f(GLint location, GLfloat v0, GLfloat v1)
uniform2f(Location, V0, V1) -> 
 cast(?glUniform2f, <<Location:32/?SN, V0:32/?FN, V1:32/?FN>>).

%% @spec uniform3f(Location::integer(), V0::float(), V1::float(), V2::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform3f">External manpage: uniform3f</a>
%% C-API func: void glUniform3f(GLint location, GLfloat v0, GLfloat v1, GLfloat v2)
uniform3f(Location, V0, V1, V2) -> 
 cast(?glUniform3f, <<Location:32/?SN, V0:32/?FN, V1:32/?FN, V2:32/?FN>>).

%% @spec uniform4f(Location::integer(), V0::float(), V1::float(), V2::float(), V3::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform4f">External manpage: uniform4f</a>
%% C-API func: void glUniform4f(GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3)
uniform4f(Location, V0, V1, V2, V3) -> 
 cast(?glUniform4f, <<Location:32/?SN, V0:32/?FN, V1:32/?FN, V2:32/?FN, V3:32/?FN>>).

%% @spec uniform1i(Location::integer(), V0::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform1i">External manpage: uniform1i</a>
%% C-API func: void glUniform1i(GLint location, GLint v0)
uniform1i(Location, V0) -> 
 cast(?glUniform1i, <<Location:32/?SN, V0:32/?SN>>).

%% @spec uniform2i(Location::integer(), V0::integer(), V1::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform2i">External manpage: uniform2i</a>
%% C-API func: void glUniform2i(GLint location, GLint v0, GLint v1)
uniform2i(Location, V0, V1) -> 
 cast(?glUniform2i, <<Location:32/?SN, V0:32/?SN, V1:32/?SN>>).

%% @spec uniform3i(Location::integer(), V0::integer(), V1::integer(), V2::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform3i">External manpage: uniform3i</a>
%% C-API func: void glUniform3i(GLint location, GLint v0, GLint v1, GLint v2)
uniform3i(Location, V0, V1, V2) -> 
 cast(?glUniform3i, <<Location:32/?SN, V0:32/?SN, V1:32/?SN, V2:32/?SN>>).

%% @spec uniform4i(Location::integer(), V0::integer(), V1::integer(), V2::integer(), V3::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform4i">External manpage: uniform4i</a>
%% C-API func: void glUniform4i(GLint location, GLint v0, GLint v1, GLint v2, GLint v3)
uniform4i(Location, V0, V1, V2, V3) -> 
 cast(?glUniform4i, <<Location:32/?SN, V0:32/?SN, V1:32/?SN, V2:32/?SN, V3:32/?SN>>).

%% @spec uniform1fv(Location::integer(), Count::integer(), [Value1::float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform">External manpage: uniform1fv</a>
%% C-API func: void glUniform1fv(GLint location, GLsizei count,  const GLfloat * value)
uniform1fv(Location, Count, Value) -> 
 sdl:send_bin(list_to_binary(term2bin(Value,Count,?GL_FLOAT)), ?MODULE, ?LINE),
 cast(?glUniform1fv, <<Location:32/?SN, Count:32/?SN>>).

%% @spec uniform2fv(Location::integer(), Count::integer(), [{Value1::float(),Value2::float()}]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform">External manpage: uniform2fv</a>
%% C-API func: void glUniform2fv(GLint location, GLsizei count,  const GLfloat * value)
uniform2fv(Location, Count, Value) -> 
 sdl:send_bin(sdl_util:tuplelist2bin(2,?GL_FLOAT,Value), ?MODULE, ?LINE),
 cast(?glUniform2fv, <<Location:32/?SN, Count:32/?SN>>).

%% @spec uniform3fv(Location::integer(), Count::integer(), [{Value1::float(),Value2::float(),Value3::float()}]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform">External manpage: uniform3fv</a>
%% C-API func: void glUniform3fv(GLint location, GLsizei count,  const GLfloat * value)
uniform3fv(Location, Count, Value) -> 
 sdl:send_bin(sdl_util:tuplelist2bin(3,?GL_FLOAT,Value), ?MODULE, ?LINE),
 cast(?glUniform3fv, <<Location:32/?SN, Count:32/?SN>>).

%% @spec uniform4fv(Location::integer(), Count::integer(), [{Value1::float(),Value2::float(),Value3::float(),Value4::float()}]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform">External manpage: uniform4fv</a>
%% C-API func: void glUniform4fv(GLint location, GLsizei count,  const GLfloat * value)
uniform4fv(Location, Count, Value) -> 
 sdl:send_bin(sdl_util:tuplelist2bin(4,?GL_FLOAT,Value), ?MODULE, ?LINE),
 cast(?glUniform4fv, <<Location:32/?SN, Count:32/?SN>>).

%% @spec uniform1iv(Location::integer(), Count::integer(), [Value1::integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform">External manpage: uniform1iv</a>
%% C-API func: void glUniform1iv(GLint location, GLsizei count,  const GLint * value)
uniform1iv(Location, Count, Value) -> 
 sdl:send_bin(list_to_binary(term2bin(Value,Count,?GL_INT)), ?MODULE, ?LINE),
 cast(?glUniform1iv, <<Location:32/?SN, Count:32/?SN>>).

%% @spec uniform2iv(Location::integer(), Count::integer(), [{Value1::integer(),Value2::integer()}]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform">External manpage: uniform2iv</a>
%% C-API func: void glUniform2iv(GLint location, GLsizei count,  const GLint * value)
uniform2iv(Location, Count, Value) -> 
 sdl:send_bin(sdl_util:tuplelist2bin(2,?GL_INT,Value), ?MODULE, ?LINE),
 cast(?glUniform2iv, <<Location:32/?SN, Count:32/?SN>>).

%% @spec uniform3iv(Location::integer(), Count::integer(), [{Value1::integer(),Value2::integer(),Value3::integer()}]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform">External manpage: uniform3iv</a>
%% C-API func: void glUniform3iv(GLint location, GLsizei count,  const GLint * value)
uniform3iv(Location, Count, Value) -> 
 sdl:send_bin(sdl_util:tuplelist2bin(3,?GL_INT,Value), ?MODULE, ?LINE),
 cast(?glUniform3iv, <<Location:32/?SN, Count:32/?SN>>).

%% @spec uniform4iv(Location::integer(), Count::integer(), [{Value1::integer(),Value2::integer(),Value3::integer(),Value4::integer()}]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniform">External manpage: uniform4iv</a>
%% C-API func: void glUniform4iv(GLint location, GLsizei count,  const GLint * value)
uniform4iv(Location, Count, Value) -> 
 sdl:send_bin(sdl_util:tuplelist2bin(4,?GL_INT,Value), ?MODULE, ?LINE),
 cast(?glUniform4iv, <<Location:32/?SN, Count:32/?SN>>).

%% @spec uniformMatrix2fv(Location::integer(), Count::integer(), Transpose::bool(), [{Value1::float(),Value2::float(),Value3::float(),Value4::float()}]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniformMatrix">External manpage: uniformMatrix2fv</a>
%% C-API func: void glUniformMatrix2fv(GLint location, GLsizei count, GLboolean transpose,  const GLfloat * value)
uniformMatrix2fv(Location, Count, Transpose, Value) -> 
 sdl:send_bin(sdl_util:tuplelist2bin(4,?GL_FLOAT,Value), ?MODULE, ?LINE),
 cast(?glUniformMatrix2fv, <<Location:32/?SN, Count:32/?SN, Transpose:8/unsigned>>).

%% @spec uniformMatrix3fv(Location::integer(), Count::integer(), Transpose::bool(), [{Value1::float(),Value2::float(),Value3::float(),Value4::float(),Value5::float(),Value6::float(),Value7::float(),Value8::float(),Value9::float()}]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniformMatrix">External manpage: uniformMatrix3fv</a>
%% C-API func: void glUniformMatrix3fv(GLint location, GLsizei count, GLboolean transpose,  const GLfloat * value)
uniformMatrix3fv(Location, Count, Transpose, Value) -> 
 sdl:send_bin(sdl_util:tuplelist2bin(9,?GL_FLOAT,Value), ?MODULE, ?LINE),
 cast(?glUniformMatrix3fv, <<Location:32/?SN, Count:32/?SN, Transpose:8/unsigned>>).

%% @spec uniformMatrix4fv(Location::integer(), Count::integer(), Transpose::bool(), [{Value1::float(),Value2::float(),Value3::float(),Value4::float(),Value5::float(),Value6::float(),Value7::float(),Value8::float(),Value9::float(),Value10::float(),Value11::float(),Value12::float(),Value13::float(),Value14::float(),Value15::float(),Value16::float()}]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUniformMatrix">External manpage: uniformMatrix4fv</a>
%% C-API func: void glUniformMatrix4fv(GLint location, GLsizei count, GLboolean transpose,  const GLfloat * value)
uniformMatrix4fv(Location, Count, Transpose, Value) -> 
 sdl:send_bin(sdl_util:tuplelist2bin(16,?GL_FLOAT,Value), ?MODULE, ?LINE),
 cast(?glUniformMatrix4fv, <<Location:32/?SN, Count:32/?SN, Transpose:8/unsigned>>).

%% @spec validateProgram(Program::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glValidateProgram">External manpage: validateProgram</a>
%% C-API func: void glValidateProgram(GLuint program)
validateProgram(Program) -> 
 cast(?glValidateProgram, <<Program:32/?UN>>).

%% @spec vertexAttrib1d(Index::integer(), X::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib1d</a>
%% C-API func: void glVertexAttrib1d(GLuint index, GLdouble x)
vertexAttrib1d(Index, X) -> 
 cast(?glVertexAttrib1dv, <<Index:32/?UN, X:64/?FN>>).

%% @spec vertexAttrib1dv(Index::integer(), {V1::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib1dv</a>
%% C-API func: void glVertexAttrib1dv(GLuint index,  const GLdouble * v)
vertexAttrib1dv(Index, {V1}) -> 
 cast(?glVertexAttrib1dv, <<Index:32/?UN, V1:64/?FN>>).

%% @spec vertexAttrib1f(Index::integer(), X::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib1f</a>
%% C-API func: void glVertexAttrib1f(GLuint index, GLfloat x)
vertexAttrib1f(Index, X) -> 
 cast(?glVertexAttrib1fv, <<Index:32/?UN, X:32/?FN>>).

%% @spec vertexAttrib1fv(Index::integer(), {V1::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib1fv</a>
%% C-API func: void glVertexAttrib1fv(GLuint index,  const GLfloat * v)
vertexAttrib1fv(Index, {V1}) -> 
 cast(?glVertexAttrib1fv, <<Index:32/?UN, V1:32/?FN>>).

%% @spec vertexAttrib1s(Index::integer(), X::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib1s</a>
%% C-API func: void glVertexAttrib1s(GLuint index, GLshort x)
vertexAttrib1s(Index, X) -> 
 cast(?glVertexAttrib1sv, <<Index:32/?UN, X:16/?SN>>).

%% @spec vertexAttrib1sv(Index::integer(), {V1::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib1sv</a>
%% C-API func: void glVertexAttrib1sv(GLuint index,  const GLshort * v)
vertexAttrib1sv(Index, {V1}) -> 
 cast(?glVertexAttrib1sv, <<Index:32/?UN, V1:16/?SN>>).

%% @spec vertexAttrib2d(Index::integer(), X::float(), Y::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib2d</a>
%% C-API func: void glVertexAttrib2d(GLuint index, GLdouble x, GLdouble y)
vertexAttrib2d(Index, X, Y) -> 
 cast(?glVertexAttrib2dv, <<Index:32/?UN, X:64/?FN, Y:64/?FN>>).

%% @spec vertexAttrib2dv(Index::integer(), {V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib2dv</a>
%% C-API func: void glVertexAttrib2dv(GLuint index,  const GLdouble * v)
vertexAttrib2dv(Index, {V1,V2}) -> 
 cast(?glVertexAttrib2dv, <<Index:32/?UN, V1:64/?FN,V2:64/?FN>>).

%% @spec vertexAttrib2f(Index::integer(), X::float(), Y::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib2f</a>
%% C-API func: void glVertexAttrib2f(GLuint index, GLfloat x, GLfloat y)
vertexAttrib2f(Index, X, Y) -> 
 cast(?glVertexAttrib2fv, <<Index:32/?UN, X:32/?FN, Y:32/?FN>>).

%% @spec vertexAttrib2fv(Index::integer(), {V1::float(),V2::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib2fv</a>
%% C-API func: void glVertexAttrib2fv(GLuint index,  const GLfloat * v)
vertexAttrib2fv(Index, {V1,V2}) -> 
 cast(?glVertexAttrib2fv, <<Index:32/?UN, V1:32/?FN,V2:32/?FN>>).

%% @spec vertexAttrib2s(Index::integer(), X::integer(), Y::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib2s</a>
%% C-API func: void glVertexAttrib2s(GLuint index, GLshort x, GLshort y)
vertexAttrib2s(Index, X, Y) -> 
 cast(?glVertexAttrib2sv, <<Index:32/?UN, X:16/?SN, Y:16/?SN>>).

%% @spec vertexAttrib2sv(Index::integer(), {V1::integer(),V2::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib2sv</a>
%% C-API func: void glVertexAttrib2sv(GLuint index,  const GLshort * v)
vertexAttrib2sv(Index, {V1,V2}) -> 
 cast(?glVertexAttrib2sv, <<Index:32/?UN, V1:16/?SN,V2:16/?SN>>).

%% @spec vertexAttrib3d(Index::integer(), X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib3d</a>
%% C-API func: void glVertexAttrib3d(GLuint index, GLdouble x, GLdouble y, GLdouble z)
vertexAttrib3d(Index, X, Y, Z) -> 
 cast(?glVertexAttrib3dv, <<Index:32/?UN, X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% @spec vertexAttrib3dv(Index::integer(), {V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib3dv</a>
%% C-API func: void glVertexAttrib3dv(GLuint index,  const GLdouble * v)
vertexAttrib3dv(Index, {V1,V2,V3}) -> 
 cast(?glVertexAttrib3dv, <<Index:32/?UN, V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% @spec vertexAttrib3f(Index::integer(), X::float(), Y::float(), Z::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib3f</a>
%% C-API func: void glVertexAttrib3f(GLuint index, GLfloat x, GLfloat y, GLfloat z)
vertexAttrib3f(Index, X, Y, Z) -> 
 cast(?glVertexAttrib3fv, <<Index:32/?UN, X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% @spec vertexAttrib3fv(Index::integer(), {V1::float(),V2::float(),V3::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib3fv</a>
%% C-API func: void glVertexAttrib3fv(GLuint index,  const GLfloat * v)
vertexAttrib3fv(Index, {V1,V2,V3}) -> 
 cast(?glVertexAttrib3fv, <<Index:32/?UN, V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% @spec vertexAttrib3s(Index::integer(), X::integer(), Y::integer(), Z::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib3s</a>
%% C-API func: void glVertexAttrib3s(GLuint index, GLshort x, GLshort y, GLshort z)
vertexAttrib3s(Index, X, Y, Z) -> 
 cast(?glVertexAttrib3sv, <<Index:32/?UN, X:16/?SN, Y:16/?SN, Z:16/?SN>>).

%% @spec vertexAttrib3sv(Index::integer(), {V1::integer(),V2::integer(),V3::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib3sv</a>
%% C-API func: void glVertexAttrib3sv(GLuint index,  const GLshort * v)
vertexAttrib3sv(Index, {V1,V2,V3}) -> 
 cast(?glVertexAttrib3sv, <<Index:32/?UN, V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% @spec vertexAttrib4Nbv(Index::integer(), V::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib4Nbv">External manpage: vertexAttrib4Nbv</a>
%% C-API func: void glVertexAttrib4Nbv(GLuint index,  const GLbyte * v)
vertexAttrib4Nbv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_BYTE);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4Nbv, [<<Index:32/?UN>>,NewV]).

%% @spec vertexAttrib4Niv(Index::integer(), V::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib4Niv">External manpage: vertexAttrib4Niv</a>
%% C-API func: void glVertexAttrib4Niv(GLuint index,  const GLint * v)
vertexAttrib4Niv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_INT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4Niv, [<<Index:32/?UN>>,NewV]).

%% @spec vertexAttrib4Nsv(Index::integer(), V::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib4Nsv">External manpage: vertexAttrib4Nsv</a>
%% C-API func: void glVertexAttrib4Nsv(GLuint index,  const GLshort * v)
vertexAttrib4Nsv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_SHORT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4Nsv, [<<Index:32/?UN>>,NewV]).

%% @spec vertexAttrib4Nub(Index::integer(), X::integer(), Y::integer(), Z::integer(), W::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib4Nub">External manpage: vertexAttrib4Nub</a>
%% C-API func: void glVertexAttrib4Nub(GLuint index, GLubyte x, GLubyte y, GLubyte z, GLubyte w)
vertexAttrib4Nub(Index, X, Y, Z, W) -> 
 cast(?glVertexAttrib4Nub, <<Index:32/?UN, X:8/unsigned, Y:8/unsigned, Z:8/unsigned, W:8/unsigned>>).

%% @spec vertexAttrib4Nubv(Index::integer(), V::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib4Nubv">External manpage: vertexAttrib4Nubv</a>
%% C-API func: void glVertexAttrib4Nubv(GLuint index,  const GLubyte * v)
vertexAttrib4Nubv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_UNSIGNED_BYTE);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4Nubv, [<<Index:32/?UN>>,NewV]).

%% @spec vertexAttrib4Nuiv(Index::integer(), V::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib4Nuiv">External manpage: vertexAttrib4Nuiv</a>
%% C-API func: void glVertexAttrib4Nuiv(GLuint index,  const GLuint * v)
vertexAttrib4Nuiv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_UNSIGNED_INT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4Nuiv, [<<Index:32/?UN>>,NewV]).

%% @spec vertexAttrib4Nusv(Index::integer(), V::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib4Nusv">External manpage: vertexAttrib4Nusv</a>
%% C-API func: void glVertexAttrib4Nusv(GLuint index,  const GLushort * v)
vertexAttrib4Nusv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_UNSIGNED_SHORT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4Nusv, [<<Index:32/?UN>>,NewV]).

%% @spec vertexAttrib4bv(Index::integer(), {V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib4bv</a>
%% C-API func: void glVertexAttrib4bv(GLuint index,  const GLbyte * v)
vertexAttrib4bv(Index, {V1,V2,V3,V4}) -> 
 cast(?glVertexAttrib4bv, <<Index:32/?UN, V1:8/signed,V2:8/signed,V3:8/signed,V4:8/signed>>).

%% @spec vertexAttrib4d(Index::integer(), X::float(), Y::float(), Z::float(), W::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib4d</a>
%% C-API func: void glVertexAttrib4d(GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
vertexAttrib4d(Index, X, Y, Z, W) -> 
 cast(?glVertexAttrib4dv, <<Index:32/?UN, X:64/?FN, Y:64/?FN, Z:64/?FN, W:64/?FN>>).

%% @spec vertexAttrib4dv(Index::integer(), {V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib4dv</a>
%% C-API func: void glVertexAttrib4dv(GLuint index,  const GLdouble * v)
vertexAttrib4dv(Index, {V1,V2,V3,V4}) -> 
 cast(?glVertexAttrib4dv, <<Index:32/?UN, V1:64/?FN,V2:64/?FN,V3:64/?FN,V4:64/?FN>>).

%% @spec vertexAttrib4f(Index::integer(), X::float(), Y::float(), Z::float(), W::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib4f</a>
%% C-API func: void glVertexAttrib4f(GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)
vertexAttrib4f(Index, X, Y, Z, W) -> 
 cast(?glVertexAttrib4fv, <<Index:32/?UN, X:32/?FN, Y:32/?FN, Z:32/?FN, W:32/?FN>>).

%% @spec vertexAttrib4fv(Index::integer(), {V1::float(),V2::float(),V3::float(),V4::float()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib4fv</a>
%% C-API func: void glVertexAttrib4fv(GLuint index,  const GLfloat * v)
vertexAttrib4fv(Index, {V1,V2,V3,V4}) -> 
 cast(?glVertexAttrib4fv, <<Index:32/?UN, V1:32/?FN,V2:32/?FN,V3:32/?FN,V4:32/?FN>>).

%% @spec vertexAttrib4iv(Index::integer(), {V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib4iv</a>
%% C-API func: void glVertexAttrib4iv(GLuint index,  const GLint * v)
vertexAttrib4iv(Index, {V1,V2,V3,V4}) -> 
 cast(?glVertexAttrib4iv, <<Index:32/?UN, V1:32/?SN,V2:32/?SN,V3:32/?SN,V4:32/?SN>>).

%% @spec vertexAttrib4s(Index::integer(), X::integer(), Y::integer(), Z::integer(), W::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib4s</a>
%% C-API func: void glVertexAttrib4s(GLuint index, GLshort x, GLshort y, GLshort z, GLshort w)
vertexAttrib4s(Index, X, Y, Z, W) -> 
 cast(?glVertexAttrib4sv, <<Index:32/?UN, X:16/?SN, Y:16/?SN, Z:16/?SN, W:16/?SN>>).

%% @spec vertexAttrib4sv(Index::integer(), {V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib4sv</a>
%% C-API func: void glVertexAttrib4sv(GLuint index,  const GLshort * v)
vertexAttrib4sv(Index, {V1,V2,V3,V4}) -> 
 cast(?glVertexAttrib4sv, <<Index:32/?UN, V1:16/?SN,V2:16/?SN,V3:16/?SN,V4:16/?SN>>).

%% @spec vertexAttrib4ubv(Index::integer(), {V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib4ubv</a>
%% C-API func: void glVertexAttrib4ubv(GLuint index,  const GLubyte * v)
vertexAttrib4ubv(Index, {V1,V2,V3,V4}) -> 
 cast(?glVertexAttrib4ubv, <<Index:32/?UN, V1:8/unsigned,V2:8/unsigned,V3:8/unsigned,V4:8/unsigned>>).

%% @spec vertexAttrib4uiv(Index::integer(), {V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib4uiv</a>
%% C-API func: void glVertexAttrib4uiv(GLuint index,  const GLuint * v)
vertexAttrib4uiv(Index, {V1,V2,V3,V4}) -> 
 cast(?glVertexAttrib4uiv, <<Index:32/?UN, V1:32/?UN,V2:32/?UN,V3:32/?UN,V4:32/?UN>>).

%% @spec vertexAttrib4usv(Index::integer(), {V1::integer(),V2::integer(),V3::integer(),V4::integer()}) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttrib">External manpage: vertexAttrib4usv</a>
%% C-API func: void glVertexAttrib4usv(GLuint index,  const GLushort * v)
vertexAttrib4usv(Index, {V1,V2,V3,V4}) -> 
 cast(?glVertexAttrib4usv, <<Index:32/?UN, V1:16/?UN,V2:16/?UN,V3:16/?UN,V4:16/?UN>>).

%% @spec vertexAttribPointer(Index::integer(), Size::integer(), Type::integer(), Normalized::bool(), Stride::integer(), Pointer::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexAttribPointer">External manpage: vertexAttribPointer</a>
%% C-API func: void glVertexAttribPointer(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride,  const GLvoid * pointer)
vertexAttribPointer(Index, Size, Type, Normalized, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glVertexAttribPointer, [<<Index:32/?UN, Size:32/?SN, Type:32/?UN, Normalized:8/unsigned, 0:24, Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec weightbvARB(Size::integer(), Weights::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWeightbvARB">External manpage: weightbvARB</a>
%% C-API func: void glWeightbvARB(GLint size,  const GLbyte * weights)
weightbvARB(Size, Weights) -> 
 weightbv(Size, Weights).
weightbv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_BYTE);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightbv, [<<Size:32/?SN>>,NewWeights]).

%% @spec weightsvARB(Size::integer(), Weights::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWeightsvARB">External manpage: weightsvARB</a>
%% C-API func: void glWeightsvARB(GLint size,  const GLshort * weights)
weightsvARB(Size, Weights) -> 
 weightsv(Size, Weights).
weightsv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_SHORT);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightsv, [<<Size:32/?SN>>,NewWeights]).

%% @spec weightivARB(Size::integer(), Weights::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWeightivARB">External manpage: weightivARB</a>
%% C-API func: void glWeightivARB(GLint size,  const GLint * weights)
weightivARB(Size, Weights) -> 
 weightiv(Size, Weights).
weightiv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_INT);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightiv, [<<Size:32/?SN>>,NewWeights]).

%% @spec weightfvARB(Size::integer(), Weights::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWeightfvARB">External manpage: weightfvARB</a>
%% C-API func: void glWeightfvARB(GLint size,  const GLfloat * weights)
weightfvARB(Size, Weights) -> 
 weightfv(Size, Weights).
weightfv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_FLOAT);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightfv, [<<Size:32/?SN>>,NewWeights]).

%% @spec weightdvARB(Size::integer(), Weights::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWeightdvARB">External manpage: weightdvARB</a>
%% C-API func: void glWeightdvARB(GLint size,  const GLdouble * weights)
weightdvARB(Size, Weights) -> 
 weightdv(Size, Weights).
weightdv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_DOUBLE);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightdv, [<<Size:32/?SN>>,NewWeights]).

%% @spec weightubvARB(Size::integer(), Weights::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWeightubvARB">External manpage: weightubvARB</a>
%% C-API func: void glWeightubvARB(GLint size,  const GLubyte * weights)
weightubvARB(Size, Weights) -> 
 weightubv(Size, Weights).
weightubv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_UNSIGNED_BYTE);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightubv, [<<Size:32/?SN>>,NewWeights]).

%% @spec weightusvARB(Size::integer(), Weights::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWeightusvARB">External manpage: weightusvARB</a>
%% C-API func: void glWeightusvARB(GLint size,  const GLushort * weights)
weightusvARB(Size, Weights) -> 
 weightusv(Size, Weights).
weightusv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_UNSIGNED_SHORT);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightusv, [<<Size:32/?SN>>,NewWeights]).

%% @spec weightuivARB(Size::integer(), Weights::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWeightuivARB">External manpage: weightuivARB</a>
%% C-API func: void glWeightuivARB(GLint size,  const GLuint * weights)
weightuivARB(Size, Weights) -> 
 weightuiv(Size, Weights).
weightuiv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_UNSIGNED_INT);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightuiv, [<<Size:32/?SN>>,NewWeights]).

%% @spec weightPointerARB(Size::integer(), Type::integer(), Stride::integer(), Pointer::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glWeightPointerARB">External manpage: weightPointerARB</a>
%% C-API func: void glWeightPointerARB(GLint size, GLenum type, GLsizei stride,  const GLvoid * pointer)
weightPointerARB(Size, Type, Stride, Pointer) -> 
 weightPointer(Size, Type, Stride, Pointer).
weightPointer(Size, Type, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glWeightPointer, [<<Size:32/?SN, Type:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec vertexBlendARB(Count::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glVertexBlendARB">External manpage: vertexBlendARB</a>
%% C-API func: void glVertexBlendARB(GLint count)
vertexBlendARB(Count) -> 
 vertexBlend(Count).
vertexBlend(Count) -> 
 cast(?glVertexBlend, <<Count:32/?SN>>).

%% @spec currentPaletteMatrixARB(Index::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCurrentPaletteMatrixARB">External manpage: currentPaletteMatrixARB</a>
%% C-API func: void glCurrentPaletteMatrixARB(GLint index)
currentPaletteMatrixARB(Index) -> 
 currentPaletteMatrix(Index).
currentPaletteMatrix(Index) -> 
 cast(?glCurrentPaletteMatrix, <<Index:32/?SN>>).

%% @spec matrixIndexubvARB(Size::integer(), Indices::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMatrixIndexubvARB">External manpage: matrixIndexubvARB</a>
%% C-API func: void glMatrixIndexubvARB(GLint size,  const GLubyte * indices)
matrixIndexubvARB(Size, Indices) -> 
 matrixIndexubv(Size, Indices).
matrixIndexubv(Size, Indices) -> 
 NewIndices = if
	is_list(Indices) ; is_tuple(Indices) -> term2bin(Indices, Size, ?GL_UNSIGNED_BYTE);
	is_binary(Indices) -> Indices;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Indices})
 end, 
 cast(?glMatrixIndexubv, [<<Size:32/?SN>>,NewIndices]).

%% @spec matrixIndexusvARB(Size::integer(), Indices::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMatrixIndexusvARB">External manpage: matrixIndexusvARB</a>
%% C-API func: void glMatrixIndexusvARB(GLint size,  const GLushort * indices)
matrixIndexusvARB(Size, Indices) -> 
 matrixIndexusv(Size, Indices).
matrixIndexusv(Size, Indices) -> 
 NewIndices = if
	is_list(Indices) ; is_tuple(Indices) -> term2bin(Indices, Size, ?GL_UNSIGNED_SHORT);
	is_binary(Indices) -> Indices;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Indices})
 end, 
 cast(?glMatrixIndexusv, [<<Size:32/?SN>>,NewIndices]).

%% @spec matrixIndexuivARB(Size::integer(), Indices::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMatrixIndexuivARB">External manpage: matrixIndexuivARB</a>
%% C-API func: void glMatrixIndexuivARB(GLint size,  const GLuint * indices)
matrixIndexuivARB(Size, Indices) -> 
 matrixIndexuiv(Size, Indices).
matrixIndexuiv(Size, Indices) -> 
 NewIndices = if
	is_list(Indices) ; is_tuple(Indices) -> term2bin(Indices, Size, ?GL_UNSIGNED_INT);
	is_binary(Indices) -> Indices;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Indices})
 end, 
 cast(?glMatrixIndexuiv, [<<Size:32/?SN>>,NewIndices]).

%% @spec matrixIndexPointerARB(Size::integer(), Type::integer(), Stride::integer(), Pointer::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glMatrixIndexPointerARB">External manpage: matrixIndexPointerARB</a>
%% C-API func: void glMatrixIndexPointerARB(GLint size, GLenum type, GLsizei stride,  const GLvoid * pointer)
matrixIndexPointerARB(Size, Type, Stride, Pointer) -> 
 matrixIndexPointer(Size, Type, Stride, Pointer).
matrixIndexPointer(Size, Type, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glMatrixIndexPointer, [<<Size:32/?SN, Type:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% @spec programStringARB(Target::integer(), Format::integer(), Len::integer(), String::binary() | [number()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glProgramStringARB">External manpage: programStringARB</a>
%% C-API func: void glProgramStringARB(GLenum target, GLenum format, GLsizei len,  const GLvoid * string)
programStringARB(Target, Format, Len, String) -> 
 programString(Target, Format, Len, String).
programString(Target, Format, Len, String) -> 
%% Maybe NULL or offset sometimes
 NewString =
   if is_integer(String) -> String;
      true ->
        sdl:send_bin(String, ?MODULE, ?LINE),
       0
   end,
 cast(?glProgramString, [<<Target:32/?UN, Format:32/?UN, Len:32/?SN, NewString:32/?SN>>]).

%% @spec bindProgramARB(Target::integer(), Program::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBindProgramARB">External manpage: bindProgramARB</a>
%% C-API func: void glBindProgramARB(GLenum target, GLuint program)
bindProgramARB(Target, Program) -> 
 bindProgram(Target, Program).
bindProgram(Target, Program) -> 
 cast(?glBindProgram, <<Target:32/?UN, Program:32/?UN>>).

%% @spec deleteProgramsARB(N::integer(), Programs::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDeleteProgramsARB">External manpage: deleteProgramsARB</a>
%% C-API func: void glDeleteProgramsARB(GLsizei n,  const GLuint * programs)
deleteProgramsARB(N, Programs) -> 
 deletePrograms(N, Programs).
deletePrograms(N, Programs) -> 
 NewPrograms = if
	is_list(Programs) ; is_tuple(Programs) -> term2bin(Programs, N, ?GL_UNSIGNED_INT);
	is_binary(Programs) -> Programs;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Programs})
 end, 
 cast(?glDeletePrograms, [<<N:32/?SN>>,NewPrograms]).

%% @spec genProgramsARB(N::integer()) -> Programs::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGenProgramsARB">External manpage: genProgramsARB</a>
%% C-API func: void glGenProgramsARB(GLsizei n, GLuint * programs)
genProgramsARB(N) -> 
 genPrograms(N).
genPrograms(N) -> 
 Bin = call(?glGenPrograms, <<N:32/?SN>>), 
 case Bin of 
	<<Programs:N/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 bin2list(N, ?GL_UNSIGNED_INT, Programs);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec programEnvParameter4dARB(Target::integer(), Index::integer(), X::float(), Y::float(), Z::float(), W::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glProgramEnvParameterARB">External manpage: programEnvParameter4dARB</a>
%% C-API func: void glProgramEnvParameter4dARB(GLenum target, GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
programEnvParameter4dARB(Target, Index, X, Y, Z, W) -> 
 programEnvParameter4d(Target, Index, X, Y, Z, W).
programEnvParameter4d(Target, Index, X, Y, Z, W) -> 
 cast(?glProgramEnvParameter4dv, <<Target:32/?UN, Index:32/?UN, X:64/?FN, Y:64/?FN, Z:64/?FN, W:64/?FN>>).

%% @spec programEnvParameter4dvARB(Target::integer(), Index::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glProgramEnvParameter4dvARB">External manpage: programEnvParameter4dvARB</a>
%% C-API func: void glProgramEnvParameter4dvARB(GLenum target, GLuint index,  const GLdouble * params)
programEnvParameter4dvARB(Target, Index, Params) -> 
 programEnvParameter4dv(Target, Index, Params).
programEnvParameter4dv(Target, Index, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_DOUBLE);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glProgramEnvParameter4dv, [<<Target:32/?UN, Index:32/?UN>>,NewParams]).

%% @spec programEnvParameter4fARB(Target::integer(), Index::integer(), X::float(), Y::float(), Z::float(), W::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glProgramEnvParameterARB">External manpage: programEnvParameter4fARB</a>
%% C-API func: void glProgramEnvParameter4fARB(GLenum target, GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)
programEnvParameter4fARB(Target, Index, X, Y, Z, W) -> 
 programEnvParameter4f(Target, Index, X, Y, Z, W).
programEnvParameter4f(Target, Index, X, Y, Z, W) -> 
 cast(?glProgramEnvParameter4fv, <<Target:32/?UN, Index:32/?UN, X:32/?FN, Y:32/?FN, Z:32/?FN, W:32/?FN>>).

%% @spec programEnvParameter4fvARB(Target::integer(), Index::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glProgramEnvParameter4fvARB">External manpage: programEnvParameter4fvARB</a>
%% C-API func: void glProgramEnvParameter4fvARB(GLenum target, GLuint index,  const GLfloat * params)
programEnvParameter4fvARB(Target, Index, Params) -> 
 programEnvParameter4fv(Target, Index, Params).
programEnvParameter4fv(Target, Index, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_FLOAT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glProgramEnvParameter4fv, [<<Target:32/?UN, Index:32/?UN>>,NewParams]).

%% @spec programLocalParameter4dARB(Target::integer(), Index::integer(), X::float(), Y::float(), Z::float(), W::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glProgramLocalParameterARB">External manpage: programLocalParameter4dARB</a>
%% C-API func: void glProgramLocalParameter4dARB(GLenum target, GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
programLocalParameter4dARB(Target, Index, X, Y, Z, W) -> 
 programLocalParameter4d(Target, Index, X, Y, Z, W).
programLocalParameter4d(Target, Index, X, Y, Z, W) -> 
 cast(?glProgramLocalParameter4dv, <<Target:32/?UN, Index:32/?UN, X:64/?FN, Y:64/?FN, Z:64/?FN, W:64/?FN>>).

%% @spec programLocalParameter4dvARB(Target::integer(), Index::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glProgramLocalParameter4dvARB">External manpage: programLocalParameter4dvARB</a>
%% C-API func: void glProgramLocalParameter4dvARB(GLenum target, GLuint index,  const GLdouble * params)
programLocalParameter4dvARB(Target, Index, Params) -> 
 programLocalParameter4dv(Target, Index, Params).
programLocalParameter4dv(Target, Index, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_DOUBLE);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glProgramLocalParameter4dv, [<<Target:32/?UN, Index:32/?UN>>,NewParams]).

%% @spec programLocalParameter4fARB(Target::integer(), Index::integer(), X::float(), Y::float(), Z::float(), W::float()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glProgramLocalParameterARB">External manpage: programLocalParameter4fARB</a>
%% C-API func: void glProgramLocalParameter4fARB(GLenum target, GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)
programLocalParameter4fARB(Target, Index, X, Y, Z, W) -> 
 programLocalParameter4f(Target, Index, X, Y, Z, W).
programLocalParameter4f(Target, Index, X, Y, Z, W) -> 
 cast(?glProgramLocalParameter4fv, <<Target:32/?UN, Index:32/?UN, X:32/?FN, Y:32/?FN, Z:32/?FN, W:32/?FN>>).

%% @spec programLocalParameter4fvARB(Target::integer(), Index::integer(), Params::binary() | [float()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glProgramLocalParameter4fvARB">External manpage: programLocalParameter4fvARB</a>
%% C-API func: void glProgramLocalParameter4fvARB(GLenum target, GLuint index,  const GLfloat * params)
programLocalParameter4fvARB(Target, Index, Params) -> 
 programLocalParameter4fv(Target, Index, Params).
programLocalParameter4fv(Target, Index, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_FLOAT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glProgramLocalParameter4fv, [<<Target:32/?UN, Index:32/?UN>>,NewParams]).

%% @spec getProgramEnvParameterdvARB(Target::integer(), Index::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetProgramEnvParameterdvARB">External manpage: getProgramEnvParameterdvARB</a>
%% C-API func: void glGetProgramEnvParameterdvARB(GLenum target, GLuint index, GLdouble * params)
getProgramEnvParameterdvARB(Target, Index) -> 
 getProgramEnvParameterdv(Target, Index).
getProgramEnvParameterdv(Target, Index) -> 
 Bin = call(?glGetProgramEnvParameterdv, <<Target:32/?UN, Index:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_DOUBLE_SIZE>> -> 
	 bin2list(4, ?GL_DOUBLE, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getProgramEnvParameterfvARB(Target::integer(), Index::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetProgramEnvParameterfvARB">External manpage: getProgramEnvParameterfvARB</a>
%% C-API func: void glGetProgramEnvParameterfvARB(GLenum target, GLuint index, GLfloat * params)
getProgramEnvParameterfvARB(Target, Index) -> 
 getProgramEnvParameterfv(Target, Index).
getProgramEnvParameterfv(Target, Index) -> 
 Bin = call(?glGetProgramEnvParameterfv, <<Target:32/?UN, Index:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(4, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getProgramLocalParameterdvARB(Target::integer(), Index::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetProgramLocalParameterdvARB">External manpage: getProgramLocalParameterdvARB</a>
%% C-API func: void glGetProgramLocalParameterdvARB(GLenum target, GLuint index, GLdouble * params)
getProgramLocalParameterdvARB(Target, Index) -> 
 getProgramLocalParameterdv(Target, Index).
getProgramLocalParameterdv(Target, Index) -> 
 Bin = call(?glGetProgramLocalParameterdv, <<Target:32/?UN, Index:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_DOUBLE_SIZE>> -> 
	 bin2list(4, ?GL_DOUBLE, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getProgramLocalParameterfvARB(Target::integer(), Index::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetProgramLocalParameterfvARB">External manpage: getProgramLocalParameterfvARB</a>
%% C-API func: void glGetProgramLocalParameterfvARB(GLenum target, GLuint index, GLfloat * params)
getProgramLocalParameterfvARB(Target, Index) -> 
 getProgramLocalParameterfv(Target, Index).
getProgramLocalParameterfv(Target, Index) -> 
 Bin = call(?glGetProgramLocalParameterfv, <<Target:32/?UN, Index:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(4, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getProgramStringARB(Target::integer(), Pname::integer(), String::sdlmem()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetProgramStringARB">External manpage: getProgramStringARB</a>
%% C-API func: void glGetProgramStringARB(GLenum target, GLenum pname, GLvoid * string)
getProgramStringARB(Target, Pname, #sdlmem{bin=String}) -> 
 getProgramString(Target, Pname, #sdlmem{bin=String}).
getProgramString(Target, Pname, #sdlmem{bin=String}) -> 
 sdl:send_bin(String, ?MODULE, ?LINE),
 cast(?glGetProgramString, <<Target:32/?UN, Pname:32/?UN>>).

%% @spec deleteObjectARB(Obj::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDeleteObjectARB">External manpage: deleteObjectARB</a>
%% C-API func: void glDeleteObjectARB(GLhandleARB obj)
deleteObjectARB(Obj) -> 
 try deleteProgram(Obj)
 catch error:_ -> glDeleteObjectARB_fallback(Obj) end.
%% @hidden
glDeleteObjectARB_fallback(Obj) -> 
 cast(?glDeleteObjectARB, <<Obj:32/?UN>>).

%% @spec getHandleARB(Pname::integer()) -> integer()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetHandleARB">External manpage: getHandleARB</a>
%% C-API func: GLhandleARB glGetHandleARB(GLenum pname)
getHandleARB(Pname) -> 
 getHandle(Pname).
getHandle(Pname) -> 
 Bin = call(?glGetHandle, <<Pname:32/?UN>>), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec detachObjectARB(ContainerObj::integer(), AttachedObj::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDetachObjectARB">External manpage: detachObjectARB</a>
%% C-API func: void glDetachObjectARB(GLhandleARB containerObj, GLhandleARB attachedObj)
detachObjectARB(ContainerObj, AttachedObj) -> 
 try detachShader(ContainerObj, AttachedObj)
 catch error:_ -> glDetachObjectARB_fallback(ContainerObj, AttachedObj) end.
%% @hidden
glDetachObjectARB_fallback(ContainerObj, AttachedObj) -> 
 cast(?glDetachObjectARB, <<ContainerObj:32/?UN, AttachedObj:32/?UN>>).

%% @spec createShaderObjectARB(ShaderType::integer()) -> integer()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCreateShaderObjectARB">External manpage: createShaderObjectARB</a>
%% C-API func: GLhandleARB glCreateShaderObjectARB(GLenum shaderType)
createShaderObjectARB(ShaderType) -> 
 try createShader(ShaderType)
 catch error:_ -> glCreateShaderObjectARB_fallback(ShaderType) end.
%% @hidden
glCreateShaderObjectARB_fallback(ShaderType) -> 
 Bin = call(?glCreateShaderObjectARB, <<ShaderType:32/?UN>>), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec createProgramObjectARB() -> integer()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCreateProgramObjectARB">External manpage: createProgramObjectARB</a>
%% C-API func: GLhandleARB glCreateProgramObjectARB()
createProgramObjectARB() -> 
 try createProgram()
 catch error:_ -> glCreateProgramObjectARB_fallback() end.
%% @hidden
glCreateProgramObjectARB_fallback() -> 
 Bin = call(?glCreateProgramObjectARB, []), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec attachObjectARB(ContainerObj::integer(), Obj::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glAttachObjectARB">External manpage: attachObjectARB</a>
%% C-API func: void glAttachObjectARB(GLhandleARB containerObj, GLhandleARB obj)
attachObjectARB(ContainerObj, Obj) -> 
 try attachShader(ContainerObj, Obj)
 catch error:_ -> glAttachObjectARB_fallback(ContainerObj, Obj) end.
%% @hidden
glAttachObjectARB_fallback(ContainerObj, Obj) -> 
 cast(?glAttachObjectARB, <<ContainerObj:32/?UN, Obj:32/?UN>>).

%% @spec useProgramObjectARB(ProgramObj::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glUseProgramObjectARB">External manpage: useProgramObjectARB</a>
%% C-API func: void glUseProgramObjectARB(GLhandleARB programObj)
useProgramObjectARB(ProgramObj) -> 
 try useProgram(ProgramObj)
 catch error:_ -> glUseProgramObjectARB_fallback(ProgramObj) end.
%% @hidden
glUseProgramObjectARB_fallback(ProgramObj) -> 
 cast(?glUseProgramObjectARB, <<ProgramObj:32/?UN>>).

%% @spec getObjectParameterfvARB(Obj::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetObjectParameterfvARB">External manpage: getObjectParameterfvARB</a>
%% C-API func: void glGetObjectParameterfvARB(GLhandleARB obj, GLenum pname, GLfloat * params)
getObjectParameterfvARB(Obj, Pname) -> 
 getObjectParameterfv(Obj, Pname).
getObjectParameterfv(Obj, Pname) -> 
 Bin = call(?glGetObjectParameterfv, <<Obj:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?FN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getObjectParameterivARB(Obj::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetObjectParameterivARB">External manpage: getObjectParameterivARB</a>
%% C-API func: void glGetObjectParameterivARB(GLhandleARB obj, GLenum pname, GLint * params)
getObjectParameterivARB(Obj, Pname) -> 
 getObjectParameteriv(Obj, Pname).
getObjectParameteriv(Obj, Pname) -> 
 Bin = call(?glGetObjectParameteriv, <<Obj:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getInfoLogARB(Obj::integer(), MaxLength::integer()) -> {[Length::integer()], [InfoLog::integer()]}
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetInfoLogARB">External manpage: getInfoLogARB</a>
%% C-API func: void glGetInfoLogARB(GLhandleARB obj, GLsizei maxLength, GLsizei * length, GLcharARB * infoLog)
getInfoLogARB(Obj, MaxLength) -> 
 getInfoLog(Obj, MaxLength).
getInfoLog(Obj, MaxLength) -> 
 Bin = call(?glGetInfoLog, <<Obj:32/?UN, MaxLength:32/?SN>>), 
 case Bin of 
	<<Length:32/?SN, InfoLog:Length/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 {Length, bin2list(Length, ?GL_UNSIGNED_BYTE, InfoLog)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec getAttachedObjectsARB(ContainerObj::integer(), MaxCount::integer()) -> {[Count::integer()], [Obj::integer()]}
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetAttachedObjectsARB">External manpage: getAttachedObjectsARB</a>
%% C-API func: void glGetAttachedObjectsARB(GLhandleARB containerObj, GLsizei maxCount, GLsizei * count, GLhandleARB * obj)
getAttachedObjectsARB(ContainerObj, MaxCount) -> 
 try getAttachedShaders(ContainerObj, MaxCount)
 catch error:_ -> glGetAttachedObjectsARB_fallback(ContainerObj, MaxCount) end.
%% @hidden
glGetAttachedObjectsARB_fallback(ContainerObj, MaxCount) -> 
 Bin = call(?glGetAttachedObjectsARB, <<ContainerObj:32/?UN, MaxCount:32/?SN>>), 
 case Bin of 
	<<Count:32/?SN, Obj:Count/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 {Count, bin2list(Count, ?GL_UNSIGNED_INT, Obj)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec stencilOpSeparateATI(Face::integer(), Sfail::integer(), Dpfail::integer(), Dppass::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glStencilOpSeparateATI">External manpage: stencilOpSeparateATI</a>
%% C-API func: void glStencilOpSeparateATI(GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass)
stencilOpSeparateATI(Face, Sfail, Dpfail, Dppass) -> 
 cast(?glStencilOpSeparateATI, <<Face:32/?UN, Sfail:32/?UN, Dpfail:32/?UN, Dppass:32/?UN>>).

%% @spec stencilFuncSeparateATI(Frontfunc::integer(), Backfunc::integer(), Ref::integer(), Mask::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glStencilFuncSeparateATI">External manpage: stencilFuncSeparateATI</a>
%% C-API func: void glStencilFuncSeparateATI(GLenum frontfunc, GLenum backfunc, GLint ref, GLuint mask)
stencilFuncSeparateATI(Frontfunc, Backfunc, Ref, Mask) -> 
 cast(?glStencilFuncSeparateATI, <<Frontfunc:32/?UN, Backfunc:32/?UN, Ref:32/?SN, Mask:32/?UN>>).

%% @spec isRenderbufferEXT(Renderbuffer::integer()) -> bool()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIsRenderbufferEXT">External manpage: isRenderbufferEXT</a>
%% C-API func: GLboolean glIsRenderbufferEXT(GLuint renderbuffer)
isRenderbufferEXT(Renderbuffer) -> 
 Bin = call(?glIsRenderbufferEXT, <<Renderbuffer:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec bindRenderbufferEXT(Target::integer(), Renderbuffer::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBindRenderbufferEXT">External manpage: bindRenderbufferEXT</a>
%% C-API func: void glBindRenderbufferEXT(GLenum target, GLuint renderbuffer)
bindRenderbufferEXT(Target, Renderbuffer) -> 
 cast(?glBindRenderbufferEXT, <<Target:32/?UN, Renderbuffer:32/?UN>>).

%% @spec deleteRenderbuffersEXT(N::integer(), Renderbuffers::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDeleteRenderbuffersEXT">External manpage: deleteRenderbuffersEXT</a>
%% C-API func: void glDeleteRenderbuffersEXT(GLsizei n,  const GLuint * renderbuffers)
deleteRenderbuffersEXT(N, Renderbuffers) -> 
 NewRenderbuffers = if
	is_list(Renderbuffers) ; is_tuple(Renderbuffers) -> term2bin(Renderbuffers, N, ?GL_UNSIGNED_INT);
	is_binary(Renderbuffers) -> Renderbuffers;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Renderbuffers})
 end, 
 cast(?glDeleteRenderbuffersEXT, [<<N:32/?SN>>,NewRenderbuffers]).

%% @spec genRenderbuffersEXT(N::integer()) -> Renderbuffers::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGenRenderbuffersEXT">External manpage: genRenderbuffersEXT</a>
%% C-API func: void glGenRenderbuffersEXT(GLsizei n, GLuint * renderbuffers)
genRenderbuffersEXT(N) -> 
 Bin = call(?glGenRenderbuffersEXT, <<N:32/?SN>>), 
 case Bin of 
	<<Renderbuffers:N/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 bin2list(N, ?GL_UNSIGNED_INT, Renderbuffers);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec renderbufferStorageEXT(Target::integer(), Internalformat::integer(), Width::integer(), Height::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glRenderbufferStorageEXT">External manpage: renderbufferStorageEXT</a>
%% C-API func: void glRenderbufferStorageEXT(GLenum target, GLenum internalformat, GLsizei width, GLsizei height)
renderbufferStorageEXT(Target, Internalformat, Width, Height) -> 
 cast(?glRenderbufferStorageEXT, <<Target:32/?UN, Internalformat:32/?UN, Width:32/?SN, Height:32/?SN>>).

%% @spec getRenderbufferParameterivEXT(Target::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetRenderbufferParameterivEXT">External manpage: getRenderbufferParameterivEXT</a>
%% C-API func: void glGetRenderbufferParameterivEXT(GLenum target, GLenum pname, GLint * params)
getRenderbufferParameterivEXT(Target, Pname) -> 
 Bin = call(?glGetRenderbufferParameterivEXT, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec isFramebufferEXT(Framebuffer::integer()) -> bool()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glIsFramebufferEXT">External manpage: isFramebufferEXT</a>
%% C-API func: GLboolean glIsFramebufferEXT(GLuint framebuffer)
isFramebufferEXT(Framebuffer) -> 
 Bin = call(?glIsFramebufferEXT, <<Framebuffer:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec bindFramebufferEXT(Target::integer(), Framebuffer::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glBindFramebufferEXT">External manpage: bindFramebufferEXT</a>
%% C-API func: void glBindFramebufferEXT(GLenum target, GLuint framebuffer)
bindFramebufferEXT(Target, Framebuffer) -> 
 cast(?glBindFramebufferEXT, <<Target:32/?UN, Framebuffer:32/?UN>>).

%% @spec deleteFramebuffersEXT(N::integer(), Framebuffers::binary() | [integer()]) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glDeleteFramebuffersEXT">External manpage: deleteFramebuffersEXT</a>
%% C-API func: void glDeleteFramebuffersEXT(GLsizei n,  const GLuint * framebuffers)
deleteFramebuffersEXT(N, Framebuffers) -> 
 NewFramebuffers = if
	is_list(Framebuffers) ; is_tuple(Framebuffers) -> term2bin(Framebuffers, N, ?GL_UNSIGNED_INT);
	is_binary(Framebuffers) -> Framebuffers;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Framebuffers})
 end, 
 cast(?glDeleteFramebuffersEXT, [<<N:32/?SN>>,NewFramebuffers]).

%% @spec genFramebuffersEXT(N::integer()) -> Framebuffers::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGenFramebuffersEXT">External manpage: genFramebuffersEXT</a>
%% C-API func: void glGenFramebuffersEXT(GLsizei n, GLuint * framebuffers)
genFramebuffersEXT(N) -> 
 Bin = call(?glGenFramebuffersEXT, <<N:32/?SN>>), 
 case Bin of 
	<<Framebuffers:N/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 bin2list(N, ?GL_UNSIGNED_INT, Framebuffers);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec checkFramebufferStatusEXT(Target::integer()) -> integer()
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glCheckFramebufferStatusEXT">External manpage: checkFramebufferStatusEXT</a>
%% C-API func: GLenum glCheckFramebufferStatusEXT(GLenum target)
checkFramebufferStatusEXT(Target) -> 
 Bin = call(?glCheckFramebufferStatusEXT, <<Target:32/?UN>>), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec framebufferTexture1DEXT(Target::integer(), Attachment::integer(), Textarget::integer(), Texture::integer(), Level::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFramebufferTexture1DEXT">External manpage: framebufferTexture1DEXT</a>
%% C-API func: void glFramebufferTexture1DEXT(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level)
framebufferTexture1DEXT(Target, Attachment, Textarget, Texture, Level) -> 
 cast(?glFramebufferTexture1DEXT, <<Target:32/?UN, Attachment:32/?UN, Textarget:32/?UN, Texture:32/?UN, Level:32/?SN>>).

%% @spec framebufferTexture2DEXT(Target::integer(), Attachment::integer(), Textarget::integer(), Texture::integer(), Level::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFramebufferTexture2DEXT">External manpage: framebufferTexture2DEXT</a>
%% C-API func: void glFramebufferTexture2DEXT(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level)
framebufferTexture2DEXT(Target, Attachment, Textarget, Texture, Level) -> 
 cast(?glFramebufferTexture2DEXT, <<Target:32/?UN, Attachment:32/?UN, Textarget:32/?UN, Texture:32/?UN, Level:32/?SN>>).

%% @spec framebufferTexture3DEXT(Target::integer(), Attachment::integer(), Textarget::integer(), Texture::integer(), Level::integer(), Zoffset::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFramebufferTexture3DEXT">External manpage: framebufferTexture3DEXT</a>
%% C-API func: void glFramebufferTexture3DEXT(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level, GLint zoffset)
framebufferTexture3DEXT(Target, Attachment, Textarget, Texture, Level, Zoffset) -> 
 cast(?glFramebufferTexture3DEXT, <<Target:32/?UN, Attachment:32/?UN, Textarget:32/?UN, Texture:32/?UN, Level:32/?SN, Zoffset:32/?SN>>).

%% @spec framebufferRenderbufferEXT(Target::integer(), Attachment::integer(), Renderbuffertarget::integer(), Renderbuffer::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glFramebufferRenderbufferEXT">External manpage: framebufferRenderbufferEXT</a>
%% C-API func: void glFramebufferRenderbufferEXT(GLenum target, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer)
framebufferRenderbufferEXT(Target, Attachment, Renderbuffertarget, Renderbuffer) -> 
 cast(?glFramebufferRenderbufferEXT, <<Target:32/?UN, Attachment:32/?UN, Renderbuffertarget:32/?UN, Renderbuffer:32/?UN>>).

%% @spec getFramebufferAttachmentParameterivEXT(Target::integer(), Attachment::integer(), Pname::integer()) -> Params::[term()]
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGetFramebufferAttachmentParameterivEXT">External manpage: getFramebufferAttachmentParameterivEXT</a>
%% C-API func: void glGetFramebufferAttachmentParameterivEXT(GLenum target, GLenum attachment, GLenum pname, GLint * params)
getFramebufferAttachmentParameterivEXT(Target, Attachment, Pname) -> 
 Bin = call(?glGetFramebufferAttachmentParameterivEXT, <<Target:32/?UN, Attachment:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% @spec generateMipmapEXT(Target::integer()) -> ok
%% @doc <a href="http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+glGenerateMipmapEXT">External manpage: generateMipmapEXT</a>
%% C-API func: void glGenerateMipmapEXT(GLenum target)
generateMipmapEXT(Target) -> 
 cast(?glGenerateMipmapEXT, <<Target:32/?UN>>).

