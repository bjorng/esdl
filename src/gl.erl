%%  Copyright (c) 2001 Dan Gudmundsson
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

%% Func:    accum 
%% Args:    Op, Value
%% Returns: ok
%% C-API func: void glAccum(GLenum op, GLfloat value)
accum(Op, Value) -> 
 cast(?glAccum, <<Op:32/?UN, Value:32/?FN>>).

%% Func:    alphaFunc 
%% Args:    Func, Ref
%% Returns: ok
%% C-API func: void glAlphaFunc(GLenum func, GLclampf ref)
alphaFunc(Func, Ref) -> 
 cast(?glAlphaFunc, <<Func:32/?UN, Ref:32/?FN>>).

%% Func:    areTexturesResident 
%% Args:    N, <<[Textures]>>
%% Returns: {?GL_BYTE, [Residences]}
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

%% Func:    arrayElement 
%% Args:    I
%% Returns: ok
%% C-API func: void glArrayElement(GLint i)
arrayElement(I) -> 
 cast(?glArrayElement, <<I:32/?SN>>).

%% Func:    'begin' 
%% Args:    Mode
%% Returns: ok
%% C-API func: void glBegin(GLenum mode)
'begin'(Mode) -> 
 cast(?glBegin, <<Mode:32/?UN>>).

%% Func:    bindTexture 
%% Args:    Target, Texture
%% Returns: ok
%% C-API func: void glBindTexture(GLenum target, GLuint texture)
bindTexture(Target, Texture) -> 
 cast(?glBindTexture, <<Target:32/?UN, Texture:32/?UN>>).

%% Func:    bitmap 
%% Args:    Width, Height, Xorig, Yorig, Xmove, Ymove, <<[Bitmap]>>
%% Returns: ok
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

%% Func:    blendFunc 
%% Args:    Sfactor, Dfactor
%% Returns: ok
%% C-API func: void glBlendFunc(GLenum sfactor, GLenum dfactor)
blendFunc(Sfactor, Dfactor) -> 
 cast(?glBlendFunc, <<Sfactor:32/?UN, Dfactor:32/?UN>>).

%% Func:    callList 
%% Args:    List
%% Returns: ok
%% C-API func: void glCallList(GLuint list)
callList(List) -> 
 cast(?glCallList, <<List:32/?UN>>).

%% Func:    callLists 
%% Args:    N, Type, <<[Lists]>>
%% Returns: ok
%% C-API func: void glCallLists(GLsizei n, GLenum type,  const GLvoid * lists)
callLists(N, Type, Lists) -> 
 NewLists = if
	is_list(Lists) ; is_tuple(Lists) -> term2bin(Lists, N, Type);
	is_binary(Lists) -> Lists;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Lists})
 end, 
 cast(?glCallLists, [<<N:32/?SN, Type:32/?UN>>,NewLists]).

%% Func:    clear 
%% Args:    Mask
%% Returns: ok
%% C-API func: void glClear(GLbitfield mask)
clear(Mask) -> 
 cast(?glClear, <<Mask:32/?UN>>).

%% Func:    clearAccum 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glClearAccum(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
clearAccum(Red, Green, Blue, Alpha) -> 
 cast(?glClearAccum, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN, Alpha:32/?FN>>).

%% Func:    clearColor 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glClearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)
clearColor(Red, Green, Blue, Alpha) -> 
 cast(?glClearColor, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN, Alpha:32/?FN>>).

%% Func:    clearDepth 
%% Args:    Depth
%% Returns: ok
%% C-API func: void glClearDepth(GLclampd depth)
clearDepth(Depth) -> 
 cast(?glClearDepth, <<Depth:64/?FN>>).

%% Func:    clearIndex 
%% Args:    C
%% Returns: ok
%% C-API func: void glClearIndex(GLfloat c)
clearIndex(C) -> 
 cast(?glClearIndex, <<C:32/?FN>>).

%% Func:    clearStencil 
%% Args:    S
%% Returns: ok
%% C-API func: void glClearStencil(GLint s)
clearStencil(S) -> 
 cast(?glClearStencil, <<S:32/?SN>>).

%% Func:    clipPlane 
%% Args:    Plane, <<[Equation]>>
%% Returns: ok
%% C-API func: void glClipPlane(GLenum plane,  const GLdouble * equation)
clipPlane(Plane, Equation) -> 
 NewEquation = if
	is_list(Equation) ; is_tuple(Equation) -> term2bin(Equation, 4, ?GL_DOUBLE);
	binary(Equation) -> Equation;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Equation})
 end, 
 cast(?glClipPlane, [<<Plane:32/?UN>>,NewEquation]).

%% Func:    color3b 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glColor3b(GLbyte red, GLbyte green, GLbyte blue)
color3b(Red, Green, Blue) -> 
 cast(?glColor3bv, <<Red:8/signed, Green:8/signed, Blue:8/signed>>).

%% Func:    color3bv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glColor3bv( const GLbyte * v)
color3bv({V1,V2,V3}) -> 
 cast(?glColor3bv, <<V1:8/signed,V2:8/signed,V3:8/signed>>).

%% Func:    color3d 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glColor3d(GLdouble red, GLdouble green, GLdouble blue)
color3d(Red, Green, Blue) -> 
 cast(?glColor3dv, <<Red:64/?FN, Green:64/?FN, Blue:64/?FN>>).

%% Func:    color3dv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glColor3dv( const GLdouble * v)
color3dv({V1,V2,V3}) -> 
 cast(?glColor3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% Func:    color3f 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glColor3f(GLfloat red, GLfloat green, GLfloat blue)
color3f(Red, Green, Blue) -> 
 cast(?glColor3fv, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN>>).

%% Func:    color3fv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glColor3fv( const GLfloat * v)
color3fv({V1,V2,V3}) -> 
 cast(?glColor3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% Func:    color3i 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glColor3i(GLint red, GLint green, GLint blue)
color3i(Red, Green, Blue) -> 
 cast(?glColor3iv, <<Red:32/?SN, Green:32/?SN, Blue:32/?SN>>).

%% Func:    color3iv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glColor3iv( const GLint * v)
color3iv({V1,V2,V3}) -> 
 cast(?glColor3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% Func:    color3s 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glColor3s(GLshort red, GLshort green, GLshort blue)
color3s(Red, Green, Blue) -> 
 cast(?glColor3sv, <<Red:16/?SN, Green:16/?SN, Blue:16/?SN>>).

%% Func:    color3sv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glColor3sv( const GLshort * v)
color3sv({V1,V2,V3}) -> 
 cast(?glColor3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% Func:    color3ub 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glColor3ub(GLubyte red, GLubyte green, GLubyte blue)
color3ub(Red, Green, Blue) -> 
 cast(?glColor3ubv, <<Red:8/unsigned, Green:8/unsigned, Blue:8/unsigned>>).

%% Func:    color3ubv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glColor3ubv( const GLubyte * v)
color3ubv({V1,V2,V3}) -> 
 cast(?glColor3ubv, <<V1:8/unsigned,V2:8/unsigned,V3:8/unsigned>>).

%% Func:    color3ui 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glColor3ui(GLuint red, GLuint green, GLuint blue)
color3ui(Red, Green, Blue) -> 
 cast(?glColor3uiv, <<Red:32/?UN, Green:32/?UN, Blue:32/?UN>>).

%% Func:    color3uiv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glColor3uiv( const GLuint * v)
color3uiv({V1,V2,V3}) -> 
 cast(?glColor3uiv, <<V1:32/?UN,V2:32/?UN,V3:32/?UN>>).

%% Func:    color3us 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glColor3us(GLushort red, GLushort green, GLushort blue)
color3us(Red, Green, Blue) -> 
 cast(?glColor3usv, <<Red:16/?UN, Green:16/?UN, Blue:16/?UN>>).

%% Func:    color3usv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glColor3usv( const GLushort * v)
color3usv({V1,V2,V3}) -> 
 cast(?glColor3usv, <<V1:16/?UN,V2:16/?UN,V3:16/?UN>>).

%% Func:    color4b 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glColor4b(GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha)
color4b(Red, Green, Blue, Alpha) -> 
 cast(?glColor4bv, <<Red:8/signed, Green:8/signed, Blue:8/signed, Alpha:8/signed>>).

%% Func:    color4bv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glColor4bv( const GLbyte * v)
color4bv({V1,V2,V3,V4}) -> 
 cast(?glColor4bv, <<V1:8/signed,V2:8/signed,V3:8/signed,V4:8/signed>>).

%% Func:    color4d 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glColor4d(GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha)
color4d(Red, Green, Blue, Alpha) -> 
 cast(?glColor4dv, <<Red:64/?FN, Green:64/?FN, Blue:64/?FN, Alpha:64/?FN>>).

%% Func:    color4dv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glColor4dv( const GLdouble * v)
color4dv({V1,V2,V3,V4}) -> 
 cast(?glColor4dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN,V4:64/?FN>>).

%% Func:    color4f 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glColor4f(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
color4f(Red, Green, Blue, Alpha) -> 
 cast(?glColor4fv, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN, Alpha:32/?FN>>).

%% Func:    color4fv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glColor4fv( const GLfloat * v)
color4fv({V1,V2,V3,V4}) -> 
 cast(?glColor4fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN,V4:32/?FN>>).

%% Func:    color4i 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glColor4i(GLint red, GLint green, GLint blue, GLint alpha)
color4i(Red, Green, Blue, Alpha) -> 
 cast(?glColor4iv, <<Red:32/?SN, Green:32/?SN, Blue:32/?SN, Alpha:32/?SN>>).

%% Func:    color4iv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glColor4iv( const GLint * v)
color4iv({V1,V2,V3,V4}) -> 
 cast(?glColor4iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN,V4:32/?SN>>).

%% Func:    color4s 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glColor4s(GLshort red, GLshort green, GLshort blue, GLshort alpha)
color4s(Red, Green, Blue, Alpha) -> 
 cast(?glColor4sv, <<Red:16/?SN, Green:16/?SN, Blue:16/?SN, Alpha:16/?SN>>).

%% Func:    color4sv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glColor4sv( const GLshort * v)
color4sv({V1,V2,V3,V4}) -> 
 cast(?glColor4sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN,V4:16/?SN>>).

%% Func:    color4ub 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glColor4ub(GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha)
color4ub(Red, Green, Blue, Alpha) -> 
 cast(?glColor4ubv, <<Red:8/unsigned, Green:8/unsigned, Blue:8/unsigned, Alpha:8/unsigned>>).

%% Func:    color4ubv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glColor4ubv( const GLubyte * v)
color4ubv({V1,V2,V3,V4}) -> 
 cast(?glColor4ubv, <<V1:8/unsigned,V2:8/unsigned,V3:8/unsigned,V4:8/unsigned>>).

%% Func:    color4ui 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glColor4ui(GLuint red, GLuint green, GLuint blue, GLuint alpha)
color4ui(Red, Green, Blue, Alpha) -> 
 cast(?glColor4uiv, <<Red:32/?UN, Green:32/?UN, Blue:32/?UN, Alpha:32/?UN>>).

%% Func:    color4uiv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glColor4uiv( const GLuint * v)
color4uiv({V1,V2,V3,V4}) -> 
 cast(?glColor4uiv, <<V1:32/?UN,V2:32/?UN,V3:32/?UN,V4:32/?UN>>).

%% Func:    color4us 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glColor4us(GLushort red, GLushort green, GLushort blue, GLushort alpha)
color4us(Red, Green, Blue, Alpha) -> 
 cast(?glColor4usv, <<Red:16/?UN, Green:16/?UN, Blue:16/?UN, Alpha:16/?UN>>).

%% Func:    color4usv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glColor4usv( const GLushort * v)
color4usv({V1,V2,V3,V4}) -> 
 cast(?glColor4usv, <<V1:16/?UN,V2:16/?UN,V3:16/?UN,V4:16/?UN>>).

%% Func:    colorMask 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glColorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha)
colorMask(Red, Green, Blue, Alpha) -> 
 cast(?glColorMask, <<Red:8/unsigned, Green:8/unsigned, Blue:8/unsigned, Alpha:8/unsigned>>).

%% Func:    colorMaterial 
%% Args:    Face, Mode
%% Returns: ok
%% C-API func: void glColorMaterial(GLenum face, GLenum mode)
colorMaterial(Face, Mode) -> 
 cast(?glColorMaterial, <<Face:32/?UN, Mode:32/?UN>>).

%% Func:    colorPointer 
%% Args:    Size, Type, Stride, <<[Pointer]>>
%% Returns: ok
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

%% Func:    copyPixels 
%% Args:    X, Y, Width, Height, Type
%% Returns: ok
%% C-API func: void glCopyPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum type)
copyPixels(X, Y, Width, Height, Type) -> 
 cast(?glCopyPixels, <<X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN, Type:32/?UN>>).

%% Func:    copyTexImage1D 
%% Args:    Target, Level, Internalformat, X, Y, Width, Border
%% Returns: ok
%% C-API func: void glCopyTexImage1D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border)
copyTexImage1D(Target, Level, Internalformat, X, Y, Width, Border) -> 
 cast(?glCopyTexImage1D, <<Target:32/?UN, Level:32/?SN, Internalformat:32/?UN, X:32/?SN, Y:32/?SN, Width:32/?SN, Border:32/?SN>>).

%% Func:    copyTexImage2D 
%% Args:    Target, Level, Internalformat, X, Y, Width, Height, Border
%% Returns: ok
%% C-API func: void glCopyTexImage2D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border)
copyTexImage2D(Target, Level, Internalformat, X, Y, Width, Height, Border) -> 
 cast(?glCopyTexImage2D, <<Target:32/?UN, Level:32/?SN, Internalformat:32/?UN, X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN, Border:32/?SN>>).

%% Func:    copyTexSubImage1D 
%% Args:    Target, Level, Xoffset, X, Y, Width
%% Returns: ok
%% C-API func: void glCopyTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width)
copyTexSubImage1D(Target, Level, Xoffset, X, Y, Width) -> 
 cast(?glCopyTexSubImage1D, <<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, X:32/?SN, Y:32/?SN, Width:32/?SN>>).

%% Func:    copyTexSubImage2D 
%% Args:    Target, Level, Xoffset, Yoffset, X, Y, Width, Height
%% Returns: ok
%% C-API func: void glCopyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)
copyTexSubImage2D(Target, Level, Xoffset, Yoffset, X, Y, Width, Height) -> 
 cast(?glCopyTexSubImage2D, <<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, Yoffset:32/?SN, X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN>>).

%% Func:    cullFace 
%% Args:    Mode
%% Returns: ok
%% C-API func: void glCullFace(GLenum mode)
cullFace(Mode) -> 
 cast(?glCullFace, <<Mode:32/?UN>>).

%% Func:    deleteLists 
%% Args:    List, Range
%% Returns: ok
%% C-API func: void glDeleteLists(GLuint list, GLsizei range)
deleteLists(List, Range) -> 
 cast(?glDeleteLists, <<List:32/?UN, Range:32/?SN>>).

%% Func:    deleteTextures 
%% Args:    N, <<[Textures]>>
%% Returns: ok
%% C-API func: void glDeleteTextures(GLsizei n,  const GLuint * textures)
deleteTextures(N, Textures) -> 
 NewTextures = if
	is_list(Textures) ; is_tuple(Textures) -> term2bin(Textures, N, ?GL_UNSIGNED_INT);
	is_binary(Textures) -> Textures;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Textures})
 end, 
 cast(?glDeleteTextures, [<<N:32/?SN>>,NewTextures]).

%% Func:    depthFunc 
%% Args:    Func
%% Returns: ok
%% C-API func: void glDepthFunc(GLenum func)
depthFunc(Func) -> 
 cast(?glDepthFunc, <<Func:32/?UN>>).

%% Func:    depthMask 
%% Args:    Flag
%% Returns: ok
%% C-API func: void glDepthMask(GLboolean flag)
depthMask(Flag) -> 
 cast(?glDepthMask, <<Flag:8/unsigned>>).

%% Func:    depthRange 
%% Args:    ZNear, ZFar
%% Returns: ok
%% C-API func: void glDepthRange(GLclampd zNear, GLclampd zFar)
depthRange(ZNear, ZFar) -> 
 cast(?glDepthRange, <<ZNear:64/?FN, ZFar:64/?FN>>).

%% Func:    disable 
%% Args:    Cap
%% Returns: ok
%% C-API func: void glDisable(GLenum cap)
disable(Cap) -> 
 cast(?glDisable, <<Cap:32/?UN>>).

%% Func:    disableClientState 
%% Args:    Array
%% Returns: ok
%% C-API func: void glDisableClientState(GLenum array)
disableClientState(Array) -> 
 cast(?glDisableClientState, <<Array:32/?UN>>).

%% Func:    drawArrays 
%% Args:    Mode, First, Count
%% Returns: ok
%% C-API func: void glDrawArrays(GLenum mode, GLint first, GLsizei count)
drawArrays(Mode, First, Count) -> 
 cast(?glDrawArrays, <<Mode:32/?UN, First:32/?SN, Count:32/?SN>>).

%% Func:    drawBuffer 
%% Args:    Mode
%% Returns: ok
%% C-API func: void glDrawBuffer(GLenum mode)
drawBuffer(Mode) -> 
 cast(?glDrawBuffer, <<Mode:32/?UN>>).

%% Func:    drawElements 
%% Args:    Mode, Count, Type, <<[Indices]>>
%% Returns: ok
%% C-API func: void glDrawElements(GLenum mode, GLsizei count, GLenum type,  const GLvoid * indices)
drawElements(Mode, Count, Type, Indices) -> 
 NewIndices = if
	is_list(Indices) ; is_tuple(Indices) -> term2bin(Indices, Count, Type);
	is_binary(Indices) -> Indices;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Indices})
 end, 
 cast(?glDrawElements, [<<Mode:32/?UN, Count:32/?SN, Type:32/?UN>>,NewIndices]).

%% Func:    drawPixels 
%% Args:    Width, Height, Format, Type, <<[Pixels]>>
%% Returns: ok
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

%% Func:    edgeFlag 
%% Args:    Flag
%% Returns: ok
%% C-API func: void glEdgeFlag(GLboolean flag)
edgeFlag(Flag) -> 
 cast(?glEdgeFlag, <<Flag:8/unsigned>>).

%% Func:    edgeFlagPointer 
%% Args:    Stride, <<[Pointer]>>
%% Returns: ok
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

%% Func:    edgeFlagv 
%% Args:    <<[Flag]>>
%% Returns: ok
%% C-API func: void glEdgeFlagv( const GLboolean * flag)
edgeFlagv(Flag) -> 
 NewFlag = if
	is_list(Flag) ; is_tuple(Flag) -> term2bin(Flag, 1, ?GL_BYTE);
	binary(Flag) -> Flag;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Flag})
 end, 
 cast(?glEdgeFlagv, [ NewFlag]).

%% Func:    enable 
%% Args:    Cap
%% Returns: ok
%% C-API func: void glEnable(GLenum cap)
enable(Cap) -> 
 cast(?glEnable, <<Cap:32/?UN>>).

%% Func:    enableClientState 
%% Args:    Array
%% Returns: ok
%% C-API func: void glEnableClientState(GLenum array)
enableClientState(Array) -> 
 cast(?glEnableClientState, <<Array:32/?UN>>).

%% Func:    'end' 
%% Args:    
%% Returns: ok
%% C-API func: void glEnd()
'end'() -> 
 cast(?glEnd, []).

%% Func:    endList 
%% Args:    
%% Returns: ok
%% C-API func: void glEndList()
endList() -> 
 cast(?glEndList, []).

%% Func:    evalCoord1d 
%% Args:    U
%% Returns: ok
%% C-API func: void glEvalCoord1d(GLdouble u)
evalCoord1d(U) -> 
 cast(?glEvalCoord1dv, <<U:64/?FN>>).

%% Func:    evalCoord1dv 
%% Args:    {U1}
%% Returns: ok
%% C-API func: void glEvalCoord1dv( const GLdouble * u)
evalCoord1dv({U1}) -> 
 cast(?glEvalCoord1dv, <<U1:64/?FN>>).

%% Func:    evalCoord1f 
%% Args:    U
%% Returns: ok
%% C-API func: void glEvalCoord1f(GLfloat u)
evalCoord1f(U) -> 
 cast(?glEvalCoord1fv, <<U:32/?FN>>).

%% Func:    evalCoord1fv 
%% Args:    {U1}
%% Returns: ok
%% C-API func: void glEvalCoord1fv( const GLfloat * u)
evalCoord1fv({U1}) -> 
 cast(?glEvalCoord1fv, <<U1:32/?FN>>).

%% Func:    evalCoord2d 
%% Args:    U, V
%% Returns: ok
%% C-API func: void glEvalCoord2d(GLdouble u, GLdouble v)
evalCoord2d(U, V) -> 
 cast(?glEvalCoord2dv, <<U:64/?FN, V:64/?FN>>).

%% Func:    evalCoord2dv 
%% Args:    {U1,U2}
%% Returns: ok
%% C-API func: void glEvalCoord2dv( const GLdouble * u)
evalCoord2dv({U1,U2}) -> 
 cast(?glEvalCoord2dv, <<U1:64/?FN,U2:64/?FN>>).

%% Func:    evalCoord2f 
%% Args:    U, V
%% Returns: ok
%% C-API func: void glEvalCoord2f(GLfloat u, GLfloat v)
evalCoord2f(U, V) -> 
 cast(?glEvalCoord2fv, <<U:32/?FN, V:32/?FN>>).

%% Func:    evalCoord2fv 
%% Args:    {U1,U2}
%% Returns: ok
%% C-API func: void glEvalCoord2fv( const GLfloat * u)
evalCoord2fv({U1,U2}) -> 
 cast(?glEvalCoord2fv, <<U1:32/?FN,U2:32/?FN>>).

%% Func:    evalMesh1 
%% Args:    Mode, I1, I2
%% Returns: ok
%% C-API func: void glEvalMesh1(GLenum mode, GLint i1, GLint i2)
evalMesh1(Mode, I1, I2) -> 
 cast(?glEvalMesh1, <<Mode:32/?UN, I1:32/?SN, I2:32/?SN>>).

%% Func:    evalMesh2 
%% Args:    Mode, I1, I2, J1, J2
%% Returns: ok
%% C-API func: void glEvalMesh2(GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2)
evalMesh2(Mode, I1, I2, J1, J2) -> 
 cast(?glEvalMesh2, <<Mode:32/?UN, I1:32/?SN, I2:32/?SN, J1:32/?SN, J2:32/?SN>>).

%% Func:    evalPoint1 
%% Args:    I
%% Returns: ok
%% C-API func: void glEvalPoint1(GLint i)
evalPoint1(I) -> 
 cast(?glEvalPoint1, <<I:32/?SN>>).

%% Func:    evalPoint2 
%% Args:    I, J
%% Returns: ok
%% C-API func: void glEvalPoint2(GLint i, GLint j)
evalPoint2(I, J) -> 
 cast(?glEvalPoint2, <<I:32/?SN, J:32/?SN>>).

%% Func:    feedbackBuffer 
%% Args:    Size, Type, #sdlmem{} = Buffer
%% Returns: ok
%% C-API func: void glFeedbackBuffer(GLsizei size, GLenum type, GLfloat * buffer)
feedbackBuffer(Size, Type, #sdlmem{bin=Buffer}) -> 
 sdl:send_bin(Buffer, ?MODULE, ?LINE),
 cast(?glFeedbackBuffer, <<Size:32/?SN, Type:32/?UN>>).

%% Func:    finish 
%% Args:    
%% Returns: ok
%% C-API func: void glFinish()
finish() -> 
 cast(?glFinish, []).

%% Func:    flush 
%% Args:    
%% Returns: ok
%% C-API func: void glFlush()
flush() -> 
 cast(?glFlush, []).

%% Func:    fogf 
%% Args:    Pname, Param
%% Returns: ok
%% C-API func: void glFogf(GLenum pname, GLfloat param)
fogf(Pname, Param) -> 
 cast(?glFogf, <<Pname:32/?UN, Param:32/?FN>>).

%% Func:    fogfv 
%% Args:    Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glFogfv(GLenum pname,  const GLfloat * params)
fogfv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glFogfv, [<<Pname:32/?UN>>,NewParams]).

%% Func:    fogi 
%% Args:    Pname, Param
%% Returns: ok
%% C-API func: void glFogi(GLenum pname, GLint param)
fogi(Pname, Param) -> 
 cast(?glFogi, <<Pname:32/?UN, Param:32/?SN>>).

%% Func:    fogiv 
%% Args:    Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glFogiv(GLenum pname,  const GLint * params)
fogiv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glFogiv, [<<Pname:32/?UN>>,NewParams]).

%% Func:    frontFace 
%% Args:    Mode
%% Returns: ok
%% C-API func: void glFrontFace(GLenum mode)
frontFace(Mode) -> 
 cast(?glFrontFace, <<Mode:32/?UN>>).

%% Func:    frustum 
%% Args:    Left, Right, Bottom, Top, ZNear, ZFar
%% Returns: ok
%% C-API func: void glFrustum(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)
frustum(Left, Right, Bottom, Top, ZNear, ZFar) -> 
 cast(?glFrustum, <<Left:64/?FN, Right:64/?FN, Bottom:64/?FN, Top:64/?FN, ZNear:64/?FN, ZFar:64/?FN>>).

%% Func:    genLists 
%% Args:    Range
%% Returns: ?GL_UNSIGNED_INT
%% C-API func: GLuint glGenLists(GLsizei range)
genLists(Range) -> 
 Bin = call(?glGenLists, <<Range:32/?SN>>), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    genTextures 
%% Args:    N
%% Returns: [Textures]
%% C-API func: void glGenTextures(GLsizei n, GLuint * textures)
genTextures(N) -> 
 Bin = call(?glGenTextures, <<N:32/?SN>>), 
 case Bin of 
	<<Textures:N/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 bin2list(N, ?GL_UNSIGNED_INT, Textures);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getBooleanv 
%% Args:    Pname
%% Returns: [Params]
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

%% Func:    getClipPlane 
%% Args:    Plane
%% Returns: [Equation]
%% C-API func: void glGetClipPlane(GLenum plane, GLdouble * equation)
getClipPlane(Plane) -> 
 Bin = call(?glGetClipPlane, <<Plane:32/?UN>>), 
 case Bin of 
	<<Equation:4/binary-unit:?GL_DOUBLE_SIZE>> -> 
	 bin2list(4, ?GL_DOUBLE, Equation);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getDoublev 
%% Args:    Pname
%% Returns: [Params]
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

%% Func:    getError 
%% Args:    
%% Returns: ?GL_INT
%% C-API func: GLenum glGetError()
getError() -> 
 Bin = call(?glGetError, []), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getFloatv 
%% Args:    Pname
%% Returns: [Params]
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

%% Func:    getIntegerv 
%% Args:    Pname
%% Returns: [Params]
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

%% Func:    getLightfv 
%% Args:    Light, Pname
%% Returns: [Params]
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

%% Func:    getLightiv 
%% Args:    Light, Pname
%% Returns: [Params]
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

%% Func:    getMapdv 
%% Args:    Target, Query, #sdlmem{} = V
%% Returns: ok
%% C-API func: void glGetMapdv(GLenum target, GLenum query, GLdouble * v)
getMapdv(Target, Query, #sdlmem{bin=V}) -> 
 sdl:send_bin(V, ?MODULE, ?LINE),
 cast(?glGetMapdv, <<Target:32/?UN, Query:32/?UN>>).

%% Func:    getMapfv 
%% Args:    Target, Query, #sdlmem{} = V
%% Returns: ok
%% C-API func: void glGetMapfv(GLenum target, GLenum query, GLfloat * v)
getMapfv(Target, Query, #sdlmem{bin=V}) -> 
 sdl:send_bin(V, ?MODULE, ?LINE),
 cast(?glGetMapfv, <<Target:32/?UN, Query:32/?UN>>).

%% Func:    getMapiv 
%% Args:    Target, Query, #sdlmem{} = V
%% Returns: ok
%% C-API func: void glGetMapiv(GLenum target, GLenum query, GLint * v)
getMapiv(Target, Query, #sdlmem{bin=V}) -> 
 sdl:send_bin(V, ?MODULE, ?LINE),
 cast(?glGetMapiv, <<Target:32/?UN, Query:32/?UN>>).

%% Func:    getMaterialfv 
%% Args:    Face, Pname
%% Returns: [Params]
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

%% Func:    getMaterialiv 
%% Args:    Face, Pname
%% Returns: [Params]
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

%% Func:    getPixelMapfv 
%% Args:    Map, #sdlmem{} = Values
%% Returns: ok
%% C-API func: void glGetPixelMapfv(GLenum map, GLfloat * values)
getPixelMapfv(Map, #sdlmem{bin=Values}) -> 
 sdl:send_bin(Values, ?MODULE, ?LINE),
 cast(?glGetPixelMapfv, <<Map:32/?UN>>).

%% Func:    getPixelMapuiv 
%% Args:    Map, #sdlmem{} = Values
%% Returns: ok
%% C-API func: void glGetPixelMapuiv(GLenum map, GLuint * values)
getPixelMapuiv(Map, #sdlmem{bin=Values}) -> 
 sdl:send_bin(Values, ?MODULE, ?LINE),
 cast(?glGetPixelMapuiv, <<Map:32/?UN>>).

%% Func:    getPixelMapusv 
%% Args:    Map, #sdlmem{} = Values
%% Returns: ok
%% C-API func: void glGetPixelMapusv(GLenum map, GLushort * values)
getPixelMapusv(Map, #sdlmem{bin=Values}) -> 
 sdl:send_bin(Values, ?MODULE, ?LINE),
 cast(?glGetPixelMapusv, <<Map:32/?UN>>).

%% Func:    getPointerv 
%% Args:    Pname
%% Returns: Params=#sdlmem{}
%% C-API func: void glGetPointerv(GLenum pname,  GLvoid* *params)
getPointerv(Pname) -> 
 Bin = call(?glGetPointerv, <<Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/big-unsigned>> -> 
	 erlang:fault({nyi, ?MODULE,?LINE});
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getPolygonStipple 
%% Args:    
%% Returns: [Mask]
%% C-API func: void glGetPolygonStipple(GLubyte * mask)
getPolygonStipple() -> 
 Bin = call(?glGetPolygonStipple, []), 
 case Bin of 
	<<Mask:128/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 bin2list(128, ?GL_UNSIGNED_BYTE, Mask);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getString 
%% Args:    Name
%% Returns: [?GL_UNSIGNED_BYTE]
%% C-API func: GLubyte* glGetString(GLenum name)
getString(Name) -> 
 Bin = call(?glGetString, <<Name:32/?UN>>), 
 case Bin of 
	Ret -> bin2list(undefined,?GL_UNSIGNED_BYTE,Ret);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getTexEnvfv 
%% Args:    Target, Pname
%% Returns: [Params]
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

%% Func:    getTexEnviv 
%% Args:    Target, Pname
%% Returns: [Params]
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

%% Func:    getTexGendv 
%% Args:    Coord, Pname
%% Returns: [Params]
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

%% Func:    getTexGenfv 
%% Args:    Coord, Pname
%% Returns: [Params]
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

%% Func:    getTexGeniv 
%% Args:    Coord, Pname
%% Returns: [Params]
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

%% Func:    getTexImage 
%% Args:    Target, Level, Format, Type, #sdlmem{} = Pixels
%% Returns: ok
%% C-API func: void glGetTexImage(GLenum target, GLint level, GLenum format, GLenum type, GLvoid * pixels)
getTexImage(Target, Level, Format, Type, #sdlmem{bin=Pixels}) -> 
 sdl:send_bin(Pixels, ?MODULE, ?LINE),
 cast(?glGetTexImage, <<Target:32/?UN, Level:32/?SN, Format:32/?UN, Type:32/?UN>>).

%% Func:    getTexLevelParameterfv 
%% Args:    Target, Level, Pname
%% Returns: [Params]
%% C-API func: void glGetTexLevelParameterfv(GLenum target, GLint level, GLenum pname, GLfloat * params)
getTexLevelParameterfv(Target, Level, Pname) -> 
 Bin = call(?glGetTexLevelParameterfv, <<Target:32/?UN, Level:32/?SN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?FN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getTexLevelParameteriv 
%% Args:    Target, Level, Pname
%% Returns: [Params]
%% C-API func: void glGetTexLevelParameteriv(GLenum target, GLint level, GLenum pname, GLint * params)
getTexLevelParameteriv(Target, Level, Pname) -> 
 Bin = call(?glGetTexLevelParameteriv, <<Target:32/?UN, Level:32/?SN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getTexParameterfv 
%% Args:    Target, Pname
%% Returns: [Params]
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

%% Func:    getTexParameteriv 
%% Args:    Target, Pname
%% Returns: [Params]
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

%% Func:    hint 
%% Args:    Target, Mode
%% Returns: ok
%% C-API func: void glHint(GLenum target, GLenum mode)
hint(Target, Mode) -> 
 cast(?glHint, <<Target:32/?UN, Mode:32/?UN>>).

%% Func:    indexMask 
%% Args:    Mask
%% Returns: ok
%% C-API func: void glIndexMask(GLuint mask)
indexMask(Mask) -> 
 cast(?glIndexMask, <<Mask:32/?UN>>).

%% Func:    indexPointer 
%% Args:    Type, Stride, <<[Pointer]>>
%% Returns: ok
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

%% Func:    indexd 
%% Args:    C
%% Returns: ok
%% C-API func: void glIndexd(GLdouble c)
indexd(C) -> 
 cast(?glIndexd, <<C:64/?FN>>).

%% Func:    indexdv 
%% Args:    <<[C]>>
%% Returns: ok
%% C-API func: void glIndexdv( const GLdouble * c)
indexdv(C) -> 
 NewC = if
	is_list(C) ; is_tuple(C) -> term2bin(C, 1, ?GL_DOUBLE);
	binary(C) -> C;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, C})
 end, 
 cast(?glIndexdv, [ NewC]).

%% Func:    indexf 
%% Args:    C
%% Returns: ok
%% C-API func: void glIndexf(GLfloat c)
indexf(C) -> 
 cast(?glIndexf, <<C:32/?FN>>).

%% Func:    indexfv 
%% Args:    <<[C]>>
%% Returns: ok
%% C-API func: void glIndexfv( const GLfloat * c)
indexfv(C) -> 
 NewC = if
	is_list(C) ; is_tuple(C) -> term2bin(C, 1, ?GL_FLOAT);
	binary(C) -> C;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, C})
 end, 
 cast(?glIndexfv, [ NewC]).

%% Func:    indexi 
%% Args:    C
%% Returns: ok
%% C-API func: void glIndexi(GLint c)
indexi(C) -> 
 cast(?glIndexi, <<C:32/?SN>>).

%% Func:    indexiv 
%% Args:    <<[C]>>
%% Returns: ok
%% C-API func: void glIndexiv( const GLint * c)
indexiv(C) -> 
 NewC = if
	is_list(C) ; is_tuple(C) -> term2bin(C, 1, ?GL_INT);
	binary(C) -> C;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, C})
 end, 
 cast(?glIndexiv, [ NewC]).

%% Func:    indexs 
%% Args:    C
%% Returns: ok
%% C-API func: void glIndexs(GLshort c)
indexs(C) -> 
 cast(?glIndexs, <<C:16/?SN>>).

%% Func:    indexsv 
%% Args:    <<[C]>>
%% Returns: ok
%% C-API func: void glIndexsv( const GLshort * c)
indexsv(C) -> 
 NewC = if
	is_list(C) ; is_tuple(C) -> term2bin(C, 1, ?GL_SHORT);
	binary(C) -> C;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, C})
 end, 
 cast(?glIndexsv, [ NewC]).

%% Func:    indexub 
%% Args:    C
%% Returns: ok
%% C-API func: void glIndexub(GLubyte c)
indexub(C) -> 
 cast(?glIndexub, <<C:8/unsigned>>).

%% Func:    indexubv 
%% Args:    <<[C]>>
%% Returns: ok
%% C-API func: void glIndexubv( const GLubyte * c)
indexubv(C) -> 
 NewC = if
	is_list(C) ; is_tuple(C) -> term2bin(C, 1, ?GL_UNSIGNED_BYTE);
	binary(C) -> C;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, C})
 end, 
 cast(?glIndexubv, [ NewC]).

%% Func:    initNames 
%% Args:    
%% Returns: ok
%% C-API func: void glInitNames()
initNames() -> 
 cast(?glInitNames, []).

%% Func:    interleavedArrays 
%% Args:    Format, Stride, <<[Pointer]>>
%% Returns: ok
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

%% Func:    isEnabled 
%% Args:    Cap
%% Returns: ?GL_BYTE
%% C-API func: GLboolean glIsEnabled(GLenum cap)
isEnabled(Cap) -> 
 Bin = call(?glIsEnabled, <<Cap:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    isList 
%% Args:    List
%% Returns: ?GL_BYTE
%% C-API func: GLboolean glIsList(GLuint list)
isList(List) -> 
 Bin = call(?glIsList, <<List:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    isTexture 
%% Args:    Texture
%% Returns: ?GL_BYTE
%% C-API func: GLboolean glIsTexture(GLuint texture)
isTexture(Texture) -> 
 Bin = call(?glIsTexture, <<Texture:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    lightModelf 
%% Args:    Pname, Param
%% Returns: ok
%% C-API func: void glLightModelf(GLenum pname, GLfloat param)
lightModelf(Pname, Param) -> 
 cast(?glLightModelf, <<Pname:32/?UN, Param:32/?FN>>).

%% Func:    lightModelfv 
%% Args:    Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glLightModelfv(GLenum pname,  const GLfloat * params)
lightModelfv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glLightModelfv, [<<Pname:32/?UN>>,NewParams]).

%% Func:    lightModeli 
%% Args:    Pname, Param
%% Returns: ok
%% C-API func: void glLightModeli(GLenum pname, GLint param)
lightModeli(Pname, Param) -> 
 cast(?glLightModeli, <<Pname:32/?UN, Param:32/?SN>>).

%% Func:    lightModeliv 
%% Args:    Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glLightModeliv(GLenum pname,  const GLint * params)
lightModeliv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glLightModeliv, [<<Pname:32/?UN>>,NewParams]).

%% Func:    lightf 
%% Args:    Light, Pname, Param
%% Returns: ok
%% C-API func: void glLightf(GLenum light, GLenum pname, GLfloat param)
lightf(Light, Pname, Param) -> 
 cast(?glLightf, <<Light:32/?UN, Pname:32/?UN, Param:32/?FN>>).

%% Func:    lightfv 
%% Args:    Light, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glLightfv(GLenum light, GLenum pname,  const GLfloat * params)
lightfv(Light, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glLightfv, [<<Light:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    lighti 
%% Args:    Light, Pname, Param
%% Returns: ok
%% C-API func: void glLighti(GLenum light, GLenum pname, GLint param)
lighti(Light, Pname, Param) -> 
 cast(?glLighti, <<Light:32/?UN, Pname:32/?UN, Param:32/?SN>>).

%% Func:    lightiv 
%% Args:    Light, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glLightiv(GLenum light, GLenum pname,  const GLint * params)
lightiv(Light, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glLightiv, [<<Light:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    lineStipple 
%% Args:    Factor, Pattern
%% Returns: ok
%% C-API func: void glLineStipple(GLint factor, GLushort pattern)
lineStipple(Factor, Pattern) -> 
 cast(?glLineStipple, <<Factor:32/?SN, Pattern:16/?UN>>).

%% Func:    lineWidth 
%% Args:    Width
%% Returns: ok
%% C-API func: void glLineWidth(GLfloat width)
lineWidth(Width) -> 
 cast(?glLineWidth, <<Width:32/?FN>>).

%% Func:    listBase 
%% Args:    Base
%% Returns: ok
%% C-API func: void glListBase(GLuint base)
listBase(Base) -> 
 cast(?glListBase, <<Base:32/?UN>>).

%% Func:    loadIdentity 
%% Args:    
%% Returns: ok
%% C-API func: void glLoadIdentity()
loadIdentity() -> 
 cast(?glLoadIdentity, []).

%% Func:    loadMatrixd 
%% Args:    <<[M]>>
%% Returns: ok
%% C-API func: void glLoadMatrixd( const GLdouble * m)
loadMatrixd(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_DOUBLE);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glLoadMatrixd, [ NewM]).

%% Func:    loadMatrixf 
%% Args:    <<[M]>>
%% Returns: ok
%% C-API func: void glLoadMatrixf( const GLfloat * m)
loadMatrixf(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_FLOAT);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glLoadMatrixf, [ NewM]).

%% Func:    loadName 
%% Args:    Name
%% Returns: ok
%% C-API func: void glLoadName(GLuint name)
loadName(Name) -> 
 cast(?glLoadName, <<Name:32/?UN>>).

%% Func:    logicOp 
%% Args:    Opcode
%% Returns: ok
%% C-API func: void glLogicOp(GLenum opcode)
logicOp(Opcode) -> 
 cast(?glLogicOp, <<Opcode:32/?UN>>).

%% Func:    map1d 
%% Args:    Target, U1, U2, Stride, Order, <<[Points]>>
%% Returns: ok
%% C-API func: void glMap1d(GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order,  const GLdouble * points)
map1d(Target, U1, U2, Stride, Order, Points) -> 
 NewPoints = if
	is_list(Points) ->  PointsLen = length(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_DOUBLE)];
	is_tuple(Points) ->  PointsLen = size(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_DOUBLE)];
	is_binary(Points) -> [<<(size(Points) div 8):32/native>>,Points/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Points})
 end, 
 cast(?glMap1d, [<<Target:32/?UN, U1:64/?FN, U2:64/?FN, Stride:32/?SN, Order:32/?SN>>,NewPoints]).

%% Func:    map1f 
%% Args:    Target, U1, U2, Stride, Order, <<[Points]>>
%% Returns: ok
%% C-API func: void glMap1f(GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order,  const GLfloat * points)
map1f(Target, U1, U2, Stride, Order, Points) -> 
 NewPoints = if
	is_list(Points) ->  PointsLen = length(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_FLOAT)];
	is_tuple(Points) ->  PointsLen = size(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_FLOAT)];
	is_binary(Points) -> [<<(size(Points) div 4):32/native>>,Points/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Points})
 end, 
 cast(?glMap1f, [<<Target:32/?UN, U1:32/?FN, U2:32/?FN, Stride:32/?SN, Order:32/?SN>>,NewPoints]).

%% Func:    map2d 
%% Args:    Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, <<[Points]>>
%% Returns: ok
%% C-API func: void glMap2d(GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder,  const GLdouble * points)
map2d(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 
 NewPoints = if
	is_list(Points) ->  PointsLen = length(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_DOUBLE)];
	is_tuple(Points) ->  PointsLen = size(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_DOUBLE)];
	is_binary(Points) -> [<<(size(Points) div 8):32/native>>,Points/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Points})
 end, 
 cast(?glMap2d, [<<Target:32/?UN, U1:64/?FN, U2:64/?FN, Ustride:32/?SN, Uorder:32/?SN, V1:64/?FN, V2:64/?FN, Vstride:32/?SN, Vorder:32/?SN>>,NewPoints]).

%% Func:    map2f 
%% Args:    Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, <<[Points]>>
%% Returns: ok
%% C-API func: void glMap2f(GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder,  const GLfloat * points)
map2f(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 
 NewPoints = if
	is_list(Points) ->  PointsLen = length(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_FLOAT)];
	is_tuple(Points) ->  PointsLen = size(Points), 
	  [<<PointsLen:32/native>>, term2bin(Points, PointsLen, ?GL_FLOAT)];
	is_binary(Points) -> [<<(size(Points) div 4):32/native>>,Points/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Points})
 end, 
 cast(?glMap2f, [<<Target:32/?UN, U1:32/?FN, U2:32/?FN, Ustride:32/?SN, Uorder:32/?SN, V1:32/?FN, V2:32/?FN, Vstride:32/?SN, Vorder:32/?SN>>,NewPoints]).

%% Func:    mapGrid1d 
%% Args:    Un, U1, U2
%% Returns: ok
%% C-API func: void glMapGrid1d(GLint un, GLdouble u1, GLdouble u2)
mapGrid1d(Un, U1, U2) -> 
 cast(?glMapGrid1d, <<Un:32/?SN, U1:64/?FN, U2:64/?FN>>).

%% Func:    mapGrid1f 
%% Args:    Un, U1, U2
%% Returns: ok
%% C-API func: void glMapGrid1f(GLint un, GLfloat u1, GLfloat u2)
mapGrid1f(Un, U1, U2) -> 
 cast(?glMapGrid1f, <<Un:32/?SN, U1:32/?FN, U2:32/?FN>>).

%% Func:    mapGrid2d 
%% Args:    Un, U1, U2, Vn, V1, V2
%% Returns: ok
%% C-API func: void glMapGrid2d(GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, GLdouble v2)
mapGrid2d(Un, U1, U2, Vn, V1, V2) -> 
 cast(?glMapGrid2d, <<Un:32/?SN, U1:64/?FN, U2:64/?FN, Vn:32/?SN, V1:64/?FN, V2:64/?FN>>).

%% Func:    mapGrid2f 
%% Args:    Un, U1, U2, Vn, V1, V2
%% Returns: ok
%% C-API func: void glMapGrid2f(GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2)
mapGrid2f(Un, U1, U2, Vn, V1, V2) -> 
 cast(?glMapGrid2f, <<Un:32/?SN, U1:32/?FN, U2:32/?FN, Vn:32/?SN, V1:32/?FN, V2:32/?FN>>).

%% Func:    materialf 
%% Args:    Face, Pname, Param
%% Returns: ok
%% C-API func: void glMaterialf(GLenum face, GLenum pname, GLfloat param)
materialf(Face, Pname, Param) -> 
 cast(?glMaterialf, <<Face:32/?UN, Pname:32/?UN, Param:32/?FN>>).

%% Func:    materialfv 
%% Args:    Face, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glMaterialfv(GLenum face, GLenum pname,  const GLfloat * params)
materialfv(Face, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glMaterialfv, [<<Face:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    materiali 
%% Args:    Face, Pname, Param
%% Returns: ok
%% C-API func: void glMateriali(GLenum face, GLenum pname, GLint param)
materiali(Face, Pname, Param) -> 
 cast(?glMateriali, <<Face:32/?UN, Pname:32/?UN, Param:32/?SN>>).

%% Func:    materialiv 
%% Args:    Face, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glMaterialiv(GLenum face, GLenum pname,  const GLint * params)
materialiv(Face, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glMaterialiv, [<<Face:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    matrixMode 
%% Args:    Mode
%% Returns: ok
%% C-API func: void glMatrixMode(GLenum mode)
matrixMode(Mode) -> 
 cast(?glMatrixMode, <<Mode:32/?UN>>).

%% Func:    multMatrixd 
%% Args:    <<[M]>>
%% Returns: ok
%% C-API func: void glMultMatrixd( const GLdouble * m)
multMatrixd(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_DOUBLE);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glMultMatrixd, [ NewM]).

%% Func:    multMatrixf 
%% Args:    <<[M]>>
%% Returns: ok
%% C-API func: void glMultMatrixf( const GLfloat * m)
multMatrixf(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_FLOAT);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glMultMatrixf, [ NewM]).

%% Func:    newList 
%% Args:    List, Mode
%% Returns: ok
%% C-API func: void glNewList(GLuint list, GLenum mode)
newList(List, Mode) -> 
 cast(?glNewList, <<List:32/?UN, Mode:32/?UN>>).

%% Func:    normal3b 
%% Args:    Nx, Ny, Nz
%% Returns: ok
%% C-API func: void glNormal3b(GLbyte nx, GLbyte ny, GLbyte nz)
normal3b(Nx, Ny, Nz) -> 
 cast(?glNormal3bv, <<Nx:8/signed, Ny:8/signed, Nz:8/signed>>).

%% Func:    normal3bv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glNormal3bv( const GLbyte * v)
normal3bv({V1,V2,V3}) -> 
 cast(?glNormal3bv, <<V1:8/signed,V2:8/signed,V3:8/signed>>).

%% Func:    normal3d 
%% Args:    Nx, Ny, Nz
%% Returns: ok
%% C-API func: void glNormal3d(GLdouble nx, GLdouble ny, GLdouble nz)
normal3d(Nx, Ny, Nz) -> 
 cast(?glNormal3dv, <<Nx:64/?FN, Ny:64/?FN, Nz:64/?FN>>).

%% Func:    normal3dv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glNormal3dv( const GLdouble * v)
normal3dv({V1,V2,V3}) -> 
 cast(?glNormal3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% Func:    normal3f 
%% Args:    Nx, Ny, Nz
%% Returns: ok
%% C-API func: void glNormal3f(GLfloat nx, GLfloat ny, GLfloat nz)
normal3f(Nx, Ny, Nz) -> 
 cast(?glNormal3fv, <<Nx:32/?FN, Ny:32/?FN, Nz:32/?FN>>).

%% Func:    normal3fv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glNormal3fv( const GLfloat * v)
normal3fv({V1,V2,V3}) -> 
 cast(?glNormal3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% Func:    normal3i 
%% Args:    Nx, Ny, Nz
%% Returns: ok
%% C-API func: void glNormal3i(GLint nx, GLint ny, GLint nz)
normal3i(Nx, Ny, Nz) -> 
 cast(?glNormal3iv, <<Nx:32/?SN, Ny:32/?SN, Nz:32/?SN>>).

%% Func:    normal3iv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glNormal3iv( const GLint * v)
normal3iv({V1,V2,V3}) -> 
 cast(?glNormal3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% Func:    normal3s 
%% Args:    Nx, Ny, Nz
%% Returns: ok
%% C-API func: void glNormal3s(GLshort nx, GLshort ny, GLshort nz)
normal3s(Nx, Ny, Nz) -> 
 cast(?glNormal3sv, <<Nx:16/?SN, Ny:16/?SN, Nz:16/?SN>>).

%% Func:    normal3sv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glNormal3sv( const GLshort * v)
normal3sv({V1,V2,V3}) -> 
 cast(?glNormal3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% Func:    normalPointer 
%% Args:    Type, Stride, <<[Pointer]>>
%% Returns: ok
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

%% Func:    ortho 
%% Args:    Left, Right, Bottom, Top, ZNear, ZFar
%% Returns: ok
%% C-API func: void glOrtho(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)
ortho(Left, Right, Bottom, Top, ZNear, ZFar) -> 
 cast(?glOrtho, <<Left:64/?FN, Right:64/?FN, Bottom:64/?FN, Top:64/?FN, ZNear:64/?FN, ZFar:64/?FN>>).

%% Func:    passThrough 
%% Args:    Token
%% Returns: ok
%% C-API func: void glPassThrough(GLfloat token)
passThrough(Token) -> 
 cast(?glPassThrough, <<Token:32/?FN>>).

%% Func:    pixelMapfv 
%% Args:    Map, Mapsize, <<[Values]>>
%% Returns: ok
%% C-API func: void glPixelMapfv(GLenum map, GLint mapsize,  const GLfloat * values)
pixelMapfv(Map, Mapsize, Values) -> 
 NewValues = if
	is_list(Values) ; is_tuple(Values) -> term2bin(Values, Mapsize, ?GL_FLOAT);
	is_binary(Values) -> Values;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Values})
 end, 
 cast(?glPixelMapfv, [<<Map:32/?UN, Mapsize:32/?SN>>,NewValues]).

%% Func:    pixelMapuiv 
%% Args:    Map, Mapsize, <<[Values]>>
%% Returns: ok
%% C-API func: void glPixelMapuiv(GLenum map, GLint mapsize,  const GLuint * values)
pixelMapuiv(Map, Mapsize, Values) -> 
 NewValues = if
	is_list(Values) ; is_tuple(Values) -> term2bin(Values, Mapsize, ?GL_UNSIGNED_INT);
	is_binary(Values) -> Values;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Values})
 end, 
 cast(?glPixelMapuiv, [<<Map:32/?UN, Mapsize:32/?SN>>,NewValues]).

%% Func:    pixelMapusv 
%% Args:    Map, Mapsize, <<[Values]>>
%% Returns: ok
%% C-API func: void glPixelMapusv(GLenum map, GLint mapsize,  const GLushort * values)
pixelMapusv(Map, Mapsize, Values) -> 
 NewValues = if
	is_list(Values) ; is_tuple(Values) -> term2bin(Values, Mapsize, ?GL_UNSIGNED_SHORT);
	is_binary(Values) -> Values;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Values})
 end, 
 cast(?glPixelMapusv, [<<Map:32/?UN, Mapsize:32/?SN>>,NewValues]).

%% Func:    pixelStoref 
%% Args:    Pname, Param
%% Returns: ok
%% C-API func: void glPixelStoref(GLenum pname, GLfloat param)
pixelStoref(Pname, Param) -> 
 cast(?glPixelStoref, <<Pname:32/?UN, Param:32/?FN>>).

%% Func:    pixelStorei 
%% Args:    Pname, Param
%% Returns: ok
%% C-API func: void glPixelStorei(GLenum pname, GLint param)
pixelStorei(Pname, Param) -> 
 cast(?glPixelStorei, <<Pname:32/?UN, Param:32/?SN>>).

%% Func:    pixelTransferf 
%% Args:    Pname, Param
%% Returns: ok
%% C-API func: void glPixelTransferf(GLenum pname, GLfloat param)
pixelTransferf(Pname, Param) -> 
 cast(?glPixelTransferf, <<Pname:32/?UN, Param:32/?FN>>).

%% Func:    pixelTransferi 
%% Args:    Pname, Param
%% Returns: ok
%% C-API func: void glPixelTransferi(GLenum pname, GLint param)
pixelTransferi(Pname, Param) -> 
 cast(?glPixelTransferi, <<Pname:32/?UN, Param:32/?SN>>).

%% Func:    pixelZoom 
%% Args:    Xfactor, Yfactor
%% Returns: ok
%% C-API func: void glPixelZoom(GLfloat xfactor, GLfloat yfactor)
pixelZoom(Xfactor, Yfactor) -> 
 cast(?glPixelZoom, <<Xfactor:32/?FN, Yfactor:32/?FN>>).

%% Func:    pointSize 
%% Args:    Size
%% Returns: ok
%% C-API func: void glPointSize(GLfloat size)
pointSize(Size) -> 
 cast(?glPointSize, <<Size:32/?FN>>).

%% Func:    polygonMode 
%% Args:    Face, Mode
%% Returns: ok
%% C-API func: void glPolygonMode(GLenum face, GLenum mode)
polygonMode(Face, Mode) -> 
 cast(?glPolygonMode, <<Face:32/?UN, Mode:32/?UN>>).

%% Func:    polygonOffset 
%% Args:    Factor, Units
%% Returns: ok
%% C-API func: void glPolygonOffset(GLfloat factor, GLfloat units)
polygonOffset(Factor, Units) -> 
 cast(?glPolygonOffset, <<Factor:32/?FN, Units:32/?FN>>).

%% Func:    polygonStipple 
%% Args:    <<[Mask]>>
%% Returns: ok
%% C-API func: void glPolygonStipple( const GLubyte * mask)
polygonStipple(Mask) -> 
 NewMask = if
	is_list(Mask) ; is_tuple(Mask) -> term2bin(Mask, 128, ?GL_UNSIGNED_BYTE);
	binary(Mask) -> Mask;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Mask})
 end, 
 cast(?glPolygonStipple, [ NewMask]).

%% Func:    popAttrib 
%% Args:    
%% Returns: ok
%% C-API func: void glPopAttrib()
popAttrib() -> 
 cast(?glPopAttrib, []).

%% Func:    popClientAttrib 
%% Args:    
%% Returns: ok
%% C-API func: void glPopClientAttrib()
popClientAttrib() -> 
 cast(?glPopClientAttrib, []).

%% Func:    popMatrix 
%% Args:    
%% Returns: ok
%% C-API func: void glPopMatrix()
popMatrix() -> 
 cast(?glPopMatrix, []).

%% Func:    popName 
%% Args:    
%% Returns: ok
%% C-API func: void glPopName()
popName() -> 
 cast(?glPopName, []).

%% Func:    prioritizeTextures 
%% Args:    N, <<[Textures]>>, <<[Priorities]>>
%% Returns: ok
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

%% Func:    pushAttrib 
%% Args:    Mask
%% Returns: ok
%% C-API func: void glPushAttrib(GLbitfield mask)
pushAttrib(Mask) -> 
 cast(?glPushAttrib, <<Mask:32/?UN>>).

%% Func:    pushClientAttrib 
%% Args:    Mask
%% Returns: ok
%% C-API func: void glPushClientAttrib(GLbitfield mask)
pushClientAttrib(Mask) -> 
 cast(?glPushClientAttrib, <<Mask:32/?UN>>).

%% Func:    pushMatrix 
%% Args:    
%% Returns: ok
%% C-API func: void glPushMatrix()
pushMatrix() -> 
 cast(?glPushMatrix, []).

%% Func:    pushName 
%% Args:    Name
%% Returns: ok
%% C-API func: void glPushName(GLuint name)
pushName(Name) -> 
 cast(?glPushName, <<Name:32/?UN>>).

%% Func:    rasterPos2d 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glRasterPos2d(GLdouble x, GLdouble y)
rasterPos2d(X, Y) -> 
 cast(?glRasterPos2dv, <<X:64/?FN, Y:64/?FN>>).

%% Func:    rasterPos2dv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glRasterPos2dv( const GLdouble * v)
rasterPos2dv({V1,V2}) -> 
 cast(?glRasterPos2dv, <<V1:64/?FN,V2:64/?FN>>).

%% Func:    rasterPos2f 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glRasterPos2f(GLfloat x, GLfloat y)
rasterPos2f(X, Y) -> 
 cast(?glRasterPos2fv, <<X:32/?FN, Y:32/?FN>>).

%% Func:    rasterPos2fv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glRasterPos2fv( const GLfloat * v)
rasterPos2fv({V1,V2}) -> 
 cast(?glRasterPos2fv, <<V1:32/?FN,V2:32/?FN>>).

%% Func:    rasterPos2i 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glRasterPos2i(GLint x, GLint y)
rasterPos2i(X, Y) -> 
 cast(?glRasterPos2iv, <<X:32/?SN, Y:32/?SN>>).

%% Func:    rasterPos2iv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glRasterPos2iv( const GLint * v)
rasterPos2iv({V1,V2}) -> 
 cast(?glRasterPos2iv, <<V1:32/?SN,V2:32/?SN>>).

%% Func:    rasterPos2s 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glRasterPos2s(GLshort x, GLshort y)
rasterPos2s(X, Y) -> 
 cast(?glRasterPos2sv, <<X:16/?SN, Y:16/?SN>>).

%% Func:    rasterPos2sv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glRasterPos2sv( const GLshort * v)
rasterPos2sv({V1,V2}) -> 
 cast(?glRasterPos2sv, <<V1:16/?SN,V2:16/?SN>>).

%% Func:    rasterPos3d 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glRasterPos3d(GLdouble x, GLdouble y, GLdouble z)
rasterPos3d(X, Y, Z) -> 
 cast(?glRasterPos3dv, <<X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% Func:    rasterPos3dv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glRasterPos3dv( const GLdouble * v)
rasterPos3dv({V1,V2,V3}) -> 
 cast(?glRasterPos3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% Func:    rasterPos3f 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glRasterPos3f(GLfloat x, GLfloat y, GLfloat z)
rasterPos3f(X, Y, Z) -> 
 cast(?glRasterPos3fv, <<X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% Func:    rasterPos3fv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glRasterPos3fv( const GLfloat * v)
rasterPos3fv({V1,V2,V3}) -> 
 cast(?glRasterPos3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% Func:    rasterPos3i 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glRasterPos3i(GLint x, GLint y, GLint z)
rasterPos3i(X, Y, Z) -> 
 cast(?glRasterPos3iv, <<X:32/?SN, Y:32/?SN, Z:32/?SN>>).

%% Func:    rasterPos3iv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glRasterPos3iv( const GLint * v)
rasterPos3iv({V1,V2,V3}) -> 
 cast(?glRasterPos3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% Func:    rasterPos3s 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glRasterPos3s(GLshort x, GLshort y, GLshort z)
rasterPos3s(X, Y, Z) -> 
 cast(?glRasterPos3sv, <<X:16/?SN, Y:16/?SN, Z:16/?SN>>).

%% Func:    rasterPos3sv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glRasterPos3sv( const GLshort * v)
rasterPos3sv({V1,V2,V3}) -> 
 cast(?glRasterPos3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% Func:    rasterPos4d 
%% Args:    X, Y, Z, W
%% Returns: ok
%% C-API func: void glRasterPos4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)
rasterPos4d(X, Y, Z, W) -> 
 cast(?glRasterPos4dv, <<X:64/?FN, Y:64/?FN, Z:64/?FN, W:64/?FN>>).

%% Func:    rasterPos4dv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glRasterPos4dv( const GLdouble * v)
rasterPos4dv({V1,V2,V3,V4}) -> 
 cast(?glRasterPos4dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN,V4:64/?FN>>).

%% Func:    rasterPos4f 
%% Args:    X, Y, Z, W
%% Returns: ok
%% C-API func: void glRasterPos4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)
rasterPos4f(X, Y, Z, W) -> 
 cast(?glRasterPos4fv, <<X:32/?FN, Y:32/?FN, Z:32/?FN, W:32/?FN>>).

%% Func:    rasterPos4fv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glRasterPos4fv( const GLfloat * v)
rasterPos4fv({V1,V2,V3,V4}) -> 
 cast(?glRasterPos4fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN,V4:32/?FN>>).

%% Func:    rasterPos4i 
%% Args:    X, Y, Z, W
%% Returns: ok
%% C-API func: void glRasterPos4i(GLint x, GLint y, GLint z, GLint w)
rasterPos4i(X, Y, Z, W) -> 
 cast(?glRasterPos4iv, <<X:32/?SN, Y:32/?SN, Z:32/?SN, W:32/?SN>>).

%% Func:    rasterPos4iv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glRasterPos4iv( const GLint * v)
rasterPos4iv({V1,V2,V3,V4}) -> 
 cast(?glRasterPos4iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN,V4:32/?SN>>).

%% Func:    rasterPos4s 
%% Args:    X, Y, Z, W
%% Returns: ok
%% C-API func: void glRasterPos4s(GLshort x, GLshort y, GLshort z, GLshort w)
rasterPos4s(X, Y, Z, W) -> 
 cast(?glRasterPos4sv, <<X:16/?SN, Y:16/?SN, Z:16/?SN, W:16/?SN>>).

%% Func:    rasterPos4sv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glRasterPos4sv( const GLshort * v)
rasterPos4sv({V1,V2,V3,V4}) -> 
 cast(?glRasterPos4sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN,V4:16/?SN>>).

%% Func:    readBuffer 
%% Args:    Mode
%% Returns: ok
%% C-API func: void glReadBuffer(GLenum mode)
readBuffer(Mode) -> 
 cast(?glReadBuffer, <<Mode:32/?UN>>).

%% Func:    readPixels 
%% Args:    X, Y, Width, Height, Format, Type, #sdlmem{} = Pixels
%% Returns: ok
%% C-API func: void glReadPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid * pixels)
readPixels(X, Y, Width, Height, Format, Type, #sdlmem{bin=Pixels}) -> 
 sdl:send_bin(Pixels, ?MODULE, ?LINE),
 cast(?glReadPixels, <<X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN, Format:32/?UN, Type:32/?UN>>).

%% Func:    rectd 
%% Args:    X1, Y1, X2, Y2
%% Returns: ok
%% C-API func: void glRectd(GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2)
rectd(X1, Y1, X2, Y2) -> 
 cast(?glRectd, <<X1:64/?FN, Y1:64/?FN, X2:64/?FN, Y2:64/?FN>>).

%% Func:    rectdv 
%% Args:    <<[V1]>>, <<[V2]>>
%% Returns: ok
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

%% Func:    rectf 
%% Args:    X1, Y1, X2, Y2
%% Returns: ok
%% C-API func: void glRectf(GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2)
rectf(X1, Y1, X2, Y2) -> 
 cast(?glRectf, <<X1:32/?FN, Y1:32/?FN, X2:32/?FN, Y2:32/?FN>>).

%% Func:    rectfv 
%% Args:    <<[V1]>>, <<[V2]>>
%% Returns: ok
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

%% Func:    recti 
%% Args:    X1, Y1, X2, Y2
%% Returns: ok
%% C-API func: void glRecti(GLint x1, GLint y1, GLint x2, GLint y2)
recti(X1, Y1, X2, Y2) -> 
 cast(?glRecti, <<X1:32/?SN, Y1:32/?SN, X2:32/?SN, Y2:32/?SN>>).

%% Func:    rectiv 
%% Args:    <<[V1]>>, <<[V2]>>
%% Returns: ok
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

%% Func:    rects 
%% Args:    X1, Y1, X2, Y2
%% Returns: ok
%% C-API func: void glRects(GLshort x1, GLshort y1, GLshort x2, GLshort y2)
rects(X1, Y1, X2, Y2) -> 
 cast(?glRects, <<X1:16/?SN, Y1:16/?SN, X2:16/?SN, Y2:16/?SN>>).

%% Func:    rectsv 
%% Args:    <<[V1]>>, <<[V2]>>
%% Returns: ok
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

%% Func:    renderMode 
%% Args:    Mode
%% Returns: ?GL_INT
%% C-API func: GLint glRenderMode(GLenum mode)
renderMode(Mode) -> 
 Bin = call(?glRenderMode, <<Mode:32/?UN>>), 
 case Bin of 
	<<Ret:32/?SN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    rotated 
%% Args:    Angle, X, Y, Z
%% Returns: ok
%% C-API func: void glRotated(GLdouble angle, GLdouble x, GLdouble y, GLdouble z)
rotated(Angle, X, Y, Z) -> 
 cast(?glRotated, <<Angle:64/?FN, X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% Func:    rotatef 
%% Args:    Angle, X, Y, Z
%% Returns: ok
%% C-API func: void glRotatef(GLfloat angle, GLfloat x, GLfloat y, GLfloat z)
rotatef(Angle, X, Y, Z) -> 
 cast(?glRotatef, <<Angle:32/?FN, X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% Func:    scaled 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glScaled(GLdouble x, GLdouble y, GLdouble z)
scaled(X, Y, Z) -> 
 cast(?glScaled, <<X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% Func:    scalef 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glScalef(GLfloat x, GLfloat y, GLfloat z)
scalef(X, Y, Z) -> 
 cast(?glScalef, <<X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% Func:    scissor 
%% Args:    X, Y, Width, Height
%% Returns: ok
%% C-API func: void glScissor(GLint x, GLint y, GLsizei width, GLsizei height)
scissor(X, Y, Width, Height) -> 
 cast(?glScissor, <<X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN>>).

%% Func:    selectBuffer 
%% Args:    Size, #sdlmem{} = Buffer
%% Returns: ok
%% C-API func: void glSelectBuffer(GLsizei size, GLuint * buffer)
selectBuffer(Size, #sdlmem{bin=Buffer}) -> 
 sdl:send_bin(Buffer, ?MODULE, ?LINE),
 cast(?glSelectBuffer, <<Size:32/?SN>>).

%% Func:    shadeModel 
%% Args:    Mode
%% Returns: ok
%% C-API func: void glShadeModel(GLenum mode)
shadeModel(Mode) -> 
 cast(?glShadeModel, <<Mode:32/?UN>>).

%% Func:    stencilFunc 
%% Args:    Func, Ref, Mask
%% Returns: ok
%% C-API func: void glStencilFunc(GLenum func, GLint ref, GLuint mask)
stencilFunc(Func, Ref, Mask) -> 
 cast(?glStencilFunc, <<Func:32/?UN, Ref:32/?SN, Mask:32/?UN>>).

%% Func:    stencilMask 
%% Args:    Mask
%% Returns: ok
%% C-API func: void glStencilMask(GLuint mask)
stencilMask(Mask) -> 
 cast(?glStencilMask, <<Mask:32/?UN>>).

%% Func:    stencilOp 
%% Args:    Fail, Zfail, Zpass
%% Returns: ok
%% C-API func: void glStencilOp(GLenum fail, GLenum zfail, GLenum zpass)
stencilOp(Fail, Zfail, Zpass) -> 
 cast(?glStencilOp, <<Fail:32/?UN, Zfail:32/?UN, Zpass:32/?UN>>).

%% Func:    texCoord1d 
%% Args:    S
%% Returns: ok
%% C-API func: void glTexCoord1d(GLdouble s)
texCoord1d(S) -> 
 cast(?glTexCoord1dv, <<S:64/?FN>>).

%% Func:    texCoord1dv 
%% Args:    {V1}
%% Returns: ok
%% C-API func: void glTexCoord1dv( const GLdouble * v)
texCoord1dv({V1}) -> 
 cast(?glTexCoord1dv, <<V1:64/?FN>>).

%% Func:    texCoord1f 
%% Args:    S
%% Returns: ok
%% C-API func: void glTexCoord1f(GLfloat s)
texCoord1f(S) -> 
 cast(?glTexCoord1fv, <<S:32/?FN>>).

%% Func:    texCoord1fv 
%% Args:    {V1}
%% Returns: ok
%% C-API func: void glTexCoord1fv( const GLfloat * v)
texCoord1fv({V1}) -> 
 cast(?glTexCoord1fv, <<V1:32/?FN>>).

%% Func:    texCoord1i 
%% Args:    S
%% Returns: ok
%% C-API func: void glTexCoord1i(GLint s)
texCoord1i(S) -> 
 cast(?glTexCoord1iv, <<S:32/?SN>>).

%% Func:    texCoord1iv 
%% Args:    {V1}
%% Returns: ok
%% C-API func: void glTexCoord1iv( const GLint * v)
texCoord1iv({V1}) -> 
 cast(?glTexCoord1iv, <<V1:32/?SN>>).

%% Func:    texCoord1s 
%% Args:    S
%% Returns: ok
%% C-API func: void glTexCoord1s(GLshort s)
texCoord1s(S) -> 
 cast(?glTexCoord1sv, <<S:16/?SN>>).

%% Func:    texCoord1sv 
%% Args:    {V1}
%% Returns: ok
%% C-API func: void glTexCoord1sv( const GLshort * v)
texCoord1sv({V1}) -> 
 cast(?glTexCoord1sv, <<V1:16/?SN>>).

%% Func:    texCoord2d 
%% Args:    S, T
%% Returns: ok
%% C-API func: void glTexCoord2d(GLdouble s, GLdouble t)
texCoord2d(S, T) -> 
 cast(?glTexCoord2dv, <<S:64/?FN, T:64/?FN>>).

%% Func:    texCoord2dv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glTexCoord2dv( const GLdouble * v)
texCoord2dv({V1,V2}) -> 
 cast(?glTexCoord2dv, <<V1:64/?FN,V2:64/?FN>>).

%% Func:    texCoord2f 
%% Args:    S, T
%% Returns: ok
%% C-API func: void glTexCoord2f(GLfloat s, GLfloat t)
texCoord2f(S, T) -> 
 cast(?glTexCoord2fv, <<S:32/?FN, T:32/?FN>>).

%% Func:    texCoord2fv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glTexCoord2fv( const GLfloat * v)
texCoord2fv({V1,V2}) -> 
 cast(?glTexCoord2fv, <<V1:32/?FN,V2:32/?FN>>).

%% Func:    texCoord2i 
%% Args:    S, T
%% Returns: ok
%% C-API func: void glTexCoord2i(GLint s, GLint t)
texCoord2i(S, T) -> 
 cast(?glTexCoord2iv, <<S:32/?SN, T:32/?SN>>).

%% Func:    texCoord2iv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glTexCoord2iv( const GLint * v)
texCoord2iv({V1,V2}) -> 
 cast(?glTexCoord2iv, <<V1:32/?SN,V2:32/?SN>>).

%% Func:    texCoord2s 
%% Args:    S, T
%% Returns: ok
%% C-API func: void glTexCoord2s(GLshort s, GLshort t)
texCoord2s(S, T) -> 
 cast(?glTexCoord2sv, <<S:16/?SN, T:16/?SN>>).

%% Func:    texCoord2sv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glTexCoord2sv( const GLshort * v)
texCoord2sv({V1,V2}) -> 
 cast(?glTexCoord2sv, <<V1:16/?SN,V2:16/?SN>>).

%% Func:    texCoord3d 
%% Args:    S, T, R
%% Returns: ok
%% C-API func: void glTexCoord3d(GLdouble s, GLdouble t, GLdouble r)
texCoord3d(S, T, R) -> 
 cast(?glTexCoord3dv, <<S:64/?FN, T:64/?FN, R:64/?FN>>).

%% Func:    texCoord3dv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glTexCoord3dv( const GLdouble * v)
texCoord3dv({V1,V2,V3}) -> 
 cast(?glTexCoord3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% Func:    texCoord3f 
%% Args:    S, T, R
%% Returns: ok
%% C-API func: void glTexCoord3f(GLfloat s, GLfloat t, GLfloat r)
texCoord3f(S, T, R) -> 
 cast(?glTexCoord3fv, <<S:32/?FN, T:32/?FN, R:32/?FN>>).

%% Func:    texCoord3fv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glTexCoord3fv( const GLfloat * v)
texCoord3fv({V1,V2,V3}) -> 
 cast(?glTexCoord3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% Func:    texCoord3i 
%% Args:    S, T, R
%% Returns: ok
%% C-API func: void glTexCoord3i(GLint s, GLint t, GLint r)
texCoord3i(S, T, R) -> 
 cast(?glTexCoord3iv, <<S:32/?SN, T:32/?SN, R:32/?SN>>).

%% Func:    texCoord3iv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glTexCoord3iv( const GLint * v)
texCoord3iv({V1,V2,V3}) -> 
 cast(?glTexCoord3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% Func:    texCoord3s 
%% Args:    S, T, R
%% Returns: ok
%% C-API func: void glTexCoord3s(GLshort s, GLshort t, GLshort r)
texCoord3s(S, T, R) -> 
 cast(?glTexCoord3sv, <<S:16/?SN, T:16/?SN, R:16/?SN>>).

%% Func:    texCoord3sv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glTexCoord3sv( const GLshort * v)
texCoord3sv({V1,V2,V3}) -> 
 cast(?glTexCoord3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% Func:    texCoord4d 
%% Args:    S, T, R, Q
%% Returns: ok
%% C-API func: void glTexCoord4d(GLdouble s, GLdouble t, GLdouble r, GLdouble q)
texCoord4d(S, T, R, Q) -> 
 cast(?glTexCoord4dv, <<S:64/?FN, T:64/?FN, R:64/?FN, Q:64/?FN>>).

%% Func:    texCoord4dv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glTexCoord4dv( const GLdouble * v)
texCoord4dv({V1,V2,V3,V4}) -> 
 cast(?glTexCoord4dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN,V4:64/?FN>>).

%% Func:    texCoord4f 
%% Args:    S, T, R, Q
%% Returns: ok
%% C-API func: void glTexCoord4f(GLfloat s, GLfloat t, GLfloat r, GLfloat q)
texCoord4f(S, T, R, Q) -> 
 cast(?glTexCoord4fv, <<S:32/?FN, T:32/?FN, R:32/?FN, Q:32/?FN>>).

%% Func:    texCoord4fv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glTexCoord4fv( const GLfloat * v)
texCoord4fv({V1,V2,V3,V4}) -> 
 cast(?glTexCoord4fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN,V4:32/?FN>>).

%% Func:    texCoord4i 
%% Args:    S, T, R, Q
%% Returns: ok
%% C-API func: void glTexCoord4i(GLint s, GLint t, GLint r, GLint q)
texCoord4i(S, T, R, Q) -> 
 cast(?glTexCoord4iv, <<S:32/?SN, T:32/?SN, R:32/?SN, Q:32/?SN>>).

%% Func:    texCoord4iv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glTexCoord4iv( const GLint * v)
texCoord4iv({V1,V2,V3,V4}) -> 
 cast(?glTexCoord4iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN,V4:32/?SN>>).

%% Func:    texCoord4s 
%% Args:    S, T, R, Q
%% Returns: ok
%% C-API func: void glTexCoord4s(GLshort s, GLshort t, GLshort r, GLshort q)
texCoord4s(S, T, R, Q) -> 
 cast(?glTexCoord4sv, <<S:16/?SN, T:16/?SN, R:16/?SN, Q:16/?SN>>).

%% Func:    texCoord4sv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glTexCoord4sv( const GLshort * v)
texCoord4sv({V1,V2,V3,V4}) -> 
 cast(?glTexCoord4sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN,V4:16/?SN>>).

%% Func:    texCoordPointer 
%% Args:    Size, Type, Stride, <<[Pointer]>>
%% Returns: ok
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

%% Func:    texEnvf 
%% Args:    Target, Pname, Param
%% Returns: ok
%% C-API func: void glTexEnvf(GLenum target, GLenum pname, GLfloat param)
texEnvf(Target, Pname, Param) -> 
 cast(?glTexEnvf, <<Target:32/?UN, Pname:32/?UN, Param:32/?FN>>).

%% Func:    texEnvfv 
%% Args:    Target, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glTexEnvfv(GLenum target, GLenum pname,  const GLfloat * params)
texEnvfv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexEnvfv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    texEnvi 
%% Args:    Target, Pname, Param
%% Returns: ok
%% C-API func: void glTexEnvi(GLenum target, GLenum pname, GLint param)
texEnvi(Target, Pname, Param) -> 
 cast(?glTexEnvi, <<Target:32/?UN, Pname:32/?UN, Param:32/?SN>>).

%% Func:    texEnviv 
%% Args:    Target, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glTexEnviv(GLenum target, GLenum pname,  const GLint * params)
texEnviv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexEnviv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    texGend 
%% Args:    Coord, Pname, Param
%% Returns: ok
%% C-API func: void glTexGend(GLenum coord, GLenum pname, GLdouble param)
texGend(Coord, Pname, Param) -> 
 cast(?glTexGend, <<Coord:32/?UN, Pname:32/?UN, Param:64/?FN>>).

%% Func:    texGendv 
%% Args:    Coord, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glTexGendv(GLenum coord, GLenum pname,  const GLdouble * params)
texGendv(Coord, Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_DOUBLE);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexGendv, [<<Coord:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    texGenf 
%% Args:    Coord, Pname, Param
%% Returns: ok
%% C-API func: void glTexGenf(GLenum coord, GLenum pname, GLfloat param)
texGenf(Coord, Pname, Param) -> 
 cast(?glTexGenf, <<Coord:32/?UN, Pname:32/?UN, Param:32/?FN>>).

%% Func:    texGenfv 
%% Args:    Coord, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glTexGenfv(GLenum coord, GLenum pname,  const GLfloat * params)
texGenfv(Coord, Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_FLOAT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexGenfv, [<<Coord:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    texGeni 
%% Args:    Coord, Pname, Param
%% Returns: ok
%% C-API func: void glTexGeni(GLenum coord, GLenum pname, GLint param)
texGeni(Coord, Pname, Param) -> 
 cast(?glTexGeni, <<Coord:32/?UN, Pname:32/?UN, Param:32/?SN>>).

%% Func:    texGeniv 
%% Args:    Coord, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glTexGeniv(GLenum coord, GLenum pname,  const GLint * params)
texGeniv(Coord, Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_INT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexGeniv, [<<Coord:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    texImage1D 
%% Args:    Target, Level, Internalformat, Width, Border, Format, Type, <<[Pixels]>>
%% Returns: ok
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

%% Func:    texImage2D 
%% Args:    Target, Level, Internalformat, Width, Height, Border, Format, Type, <<[Pixels]>>
%% Returns: ok
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

%% Func:    texParameterf 
%% Args:    Target, Pname, Param
%% Returns: ok
%% C-API func: void glTexParameterf(GLenum target, GLenum pname, GLfloat param)
texParameterf(Target, Pname, Param) -> 
 cast(?glTexParameterf, <<Target:32/?UN, Pname:32/?UN, Param:32/?FN>>).

%% Func:    texParameterfv 
%% Args:    Target, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glTexParameterfv(GLenum target, GLenum pname,  const GLfloat * params)
texParameterfv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexParameterfv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    texParameteri 
%% Args:    Target, Pname, Param
%% Returns: ok
%% C-API func: void glTexParameteri(GLenum target, GLenum pname, GLint param)
texParameteri(Target, Pname, Param) -> 
 cast(?glTexParameteri, <<Target:32/?UN, Pname:32/?UN, Param:32/?SN>>).

%% Func:    texParameteriv 
%% Args:    Target, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glTexParameteriv(GLenum target, GLenum pname,  const GLint * params)
texParameteriv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glTexParameteriv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    texSubImage1D 
%% Args:    Target, Level, Xoffset, Width, Format, Type, <<[Pixels]>>
%% Returns: ok
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

%% Func:    texSubImage2D 
%% Args:    Target, Level, Xoffset, Yoffset, Width, Height, Format, Type, <<[Pixels]>>
%% Returns: ok
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

%% Func:    translated 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glTranslated(GLdouble x, GLdouble y, GLdouble z)
translated(X, Y, Z) -> 
 cast(?glTranslated, <<X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% Func:    translatef 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glTranslatef(GLfloat x, GLfloat y, GLfloat z)
translatef(X, Y, Z) -> 
 cast(?glTranslatef, <<X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% Func:    vertex2d 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glVertex2d(GLdouble x, GLdouble y)
vertex2d(X, Y) -> 
 cast(?glVertex2dv, <<X:64/?FN, Y:64/?FN>>).

%% Func:    vertex2dv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glVertex2dv( const GLdouble * v)
vertex2dv({V1,V2}) -> 
 cast(?glVertex2dv, <<V1:64/?FN,V2:64/?FN>>).

%% Func:    vertex2f 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glVertex2f(GLfloat x, GLfloat y)
vertex2f(X, Y) -> 
 cast(?glVertex2fv, <<X:32/?FN, Y:32/?FN>>).

%% Func:    vertex2fv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glVertex2fv( const GLfloat * v)
vertex2fv({V1,V2}) -> 
 cast(?glVertex2fv, <<V1:32/?FN,V2:32/?FN>>).

%% Func:    vertex2i 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glVertex2i(GLint x, GLint y)
vertex2i(X, Y) -> 
 cast(?glVertex2iv, <<X:32/?SN, Y:32/?SN>>).

%% Func:    vertex2iv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glVertex2iv( const GLint * v)
vertex2iv({V1,V2}) -> 
 cast(?glVertex2iv, <<V1:32/?SN,V2:32/?SN>>).

%% Func:    vertex2s 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glVertex2s(GLshort x, GLshort y)
vertex2s(X, Y) -> 
 cast(?glVertex2sv, <<X:16/?SN, Y:16/?SN>>).

%% Func:    vertex2sv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glVertex2sv( const GLshort * v)
vertex2sv({V1,V2}) -> 
 cast(?glVertex2sv, <<V1:16/?SN,V2:16/?SN>>).

%% Func:    vertex3d 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glVertex3d(GLdouble x, GLdouble y, GLdouble z)
vertex3d(X, Y, Z) -> 
 cast(?glVertex3dv, <<X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% Func:    vertex3dv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glVertex3dv( const GLdouble * v)
vertex3dv({V1,V2,V3}) -> 
 cast(?glVertex3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% Func:    vertex3f 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glVertex3f(GLfloat x, GLfloat y, GLfloat z)
vertex3f(X, Y, Z) -> 
 cast(?glVertex3fv, <<X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% Func:    vertex3fv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glVertex3fv( const GLfloat * v)
vertex3fv({V1,V2,V3}) -> 
 cast(?glVertex3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% Func:    vertex3i 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glVertex3i(GLint x, GLint y, GLint z)
vertex3i(X, Y, Z) -> 
 cast(?glVertex3iv, <<X:32/?SN, Y:32/?SN, Z:32/?SN>>).

%% Func:    vertex3iv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glVertex3iv( const GLint * v)
vertex3iv({V1,V2,V3}) -> 
 cast(?glVertex3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% Func:    vertex3s 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glVertex3s(GLshort x, GLshort y, GLshort z)
vertex3s(X, Y, Z) -> 
 cast(?glVertex3sv, <<X:16/?SN, Y:16/?SN, Z:16/?SN>>).

%% Func:    vertex3sv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glVertex3sv( const GLshort * v)
vertex3sv({V1,V2,V3}) -> 
 cast(?glVertex3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% Func:    vertex4d 
%% Args:    X, Y, Z, W
%% Returns: ok
%% C-API func: void glVertex4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)
vertex4d(X, Y, Z, W) -> 
 cast(?glVertex4dv, <<X:64/?FN, Y:64/?FN, Z:64/?FN, W:64/?FN>>).

%% Func:    vertex4dv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glVertex4dv( const GLdouble * v)
vertex4dv({V1,V2,V3,V4}) -> 
 cast(?glVertex4dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN,V4:64/?FN>>).

%% Func:    vertex4f 
%% Args:    X, Y, Z, W
%% Returns: ok
%% C-API func: void glVertex4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)
vertex4f(X, Y, Z, W) -> 
 cast(?glVertex4fv, <<X:32/?FN, Y:32/?FN, Z:32/?FN, W:32/?FN>>).

%% Func:    vertex4fv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glVertex4fv( const GLfloat * v)
vertex4fv({V1,V2,V3,V4}) -> 
 cast(?glVertex4fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN,V4:32/?FN>>).

%% Func:    vertex4i 
%% Args:    X, Y, Z, W
%% Returns: ok
%% C-API func: void glVertex4i(GLint x, GLint y, GLint z, GLint w)
vertex4i(X, Y, Z, W) -> 
 cast(?glVertex4iv, <<X:32/?SN, Y:32/?SN, Z:32/?SN, W:32/?SN>>).

%% Func:    vertex4iv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glVertex4iv( const GLint * v)
vertex4iv({V1,V2,V3,V4}) -> 
 cast(?glVertex4iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN,V4:32/?SN>>).

%% Func:    vertex4s 
%% Args:    X, Y, Z, W
%% Returns: ok
%% C-API func: void glVertex4s(GLshort x, GLshort y, GLshort z, GLshort w)
vertex4s(X, Y, Z, W) -> 
 cast(?glVertex4sv, <<X:16/?SN, Y:16/?SN, Z:16/?SN, W:16/?SN>>).

%% Func:    vertex4sv 
%% Args:    {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glVertex4sv( const GLshort * v)
vertex4sv({V1,V2,V3,V4}) -> 
 cast(?glVertex4sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN,V4:16/?SN>>).

%% Func:    vertexPointer 
%% Args:    Size, Type, Stride, <<[Pointer]>>
%% Returns: ok
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

%% Func:    viewport 
%% Args:    X, Y, Width, Height
%% Returns: ok
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
%% Func:    blendColor 
%% Args:    Red, Green, Blue, Alpha
%% Returns: ok
%% C-API func: void glBlendColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)
blendColor(Red, Green, Blue, Alpha) -> 
 cast(?glBlendColor, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN, Alpha:32/?FN>>).

%% Func:    blendEquation 
%% Args:    Mode
%% Returns: ok
%% C-API func: void glBlendEquation(GLenum mode)
blendEquation(Mode) -> 
 cast(?glBlendEquation, <<Mode:32/?UN>>).

%% Func:    drawRangeElements 
%% Args:    Mode, Start, End, Count, Type, <<[Indices]>>
%% Returns: ok
%% C-API func: void glDrawRangeElements(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type,  const GLvoid * indices)
drawRangeElements(Mode, Start, End, Count, Type, Indices) -> 
 NewIndices = if
	is_list(Indices) ; is_tuple(Indices) -> term2bin(Indices, Count, Type);
	is_binary(Indices) -> Indices;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Indices})
 end, 
 cast(?glDrawRangeElements, [<<Mode:32/?UN, Start:32/?UN, End:32/?UN, Count:32/?SN, Type:32/?UN>>,NewIndices]).

%% Func:    colorTable 
%% Args:    Target, Internalformat, Width, Format, Type, <<[Table]>>
%% Returns: ok
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

%% Func:    colorTableParameterfv 
%% Args:    Target, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glColorTableParameterfv(GLenum target, GLenum pname,  const GLfloat * params)
colorTableParameterfv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_FLOAT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glColorTableParameterfv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    colorTableParameteriv 
%% Args:    Target, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glColorTableParameteriv(GLenum target, GLenum pname,  const GLint * params)
colorTableParameteriv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_INT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glColorTableParameteriv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    copyColorTable 
%% Args:    Target, Internalformat, X, Y, Width
%% Returns: ok
%% C-API func: void glCopyColorTable(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width)
copyColorTable(Target, Internalformat, X, Y, Width) -> 
 cast(?glCopyColorTable, <<Target:32/?UN, Internalformat:32/?UN, X:32/?SN, Y:32/?SN, Width:32/?SN>>).

%% Func:    getColorTable 
%% Args:    Target, Format, Type, #sdlmem{} = Table
%% Returns: ok
%% C-API func: void glGetColorTable(GLenum target, GLenum format, GLenum type, GLvoid * table)
getColorTable(Target, Format, Type, #sdlmem{bin=Table}) -> 
 sdl:send_bin(Table, ?MODULE, ?LINE),
 cast(?glGetColorTable, <<Target:32/?UN, Format:32/?UN, Type:32/?UN>>).

%% Func:    getColorTableParameterfv 
%% Args:    Target, Pname
%% Returns: [Params]
%% C-API func: void glGetColorTableParameterfv(GLenum target, GLenum pname, GLfloat * params)
getColorTableParameterfv(Target, Pname) -> 
 Bin = call(?glGetColorTableParameterfv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(4, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getColorTableParameteriv 
%% Args:    Target, Pname
%% Returns: [Params]
%% C-API func: void glGetColorTableParameteriv(GLenum target, GLenum pname, GLint * params)
getColorTableParameteriv(Target, Pname) -> 
 Bin = call(?glGetColorTableParameteriv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_INT_SIZE>> -> 
	 bin2list(4, ?GL_INT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    colorSubTable 
%% Args:    Target, Start, Count, Format, Type, <<[Data]>>
%% Returns: ok
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

%% Func:    copyColorSubTable 
%% Args:    Target, Start, X, Y, Width
%% Returns: ok
%% C-API func: void glCopyColorSubTable(GLenum target, GLsizei start, GLint x, GLint y, GLsizei width)
copyColorSubTable(Target, Start, X, Y, Width) -> 
 cast(?glCopyColorSubTable, <<Target:32/?UN, Start:32/?SN, X:32/?SN, Y:32/?SN, Width:32/?SN>>).

%% Func:    convolutionFilter1D 
%% Args:    Target, Internalformat, Width, Format, Type, <<[Image]>>
%% Returns: ok
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

%% Func:    convolutionFilter2D 
%% Args:    Target, Internalformat, Width, Height, Format, Type, <<[Image]>>
%% Returns: ok
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

%% Func:    convolutionParameterf 
%% Args:    Target, Pname, Params
%% Returns: ok
%% C-API func: void glConvolutionParameterf(GLenum target, GLenum pname, GLfloat params)
convolutionParameterf(Target, Pname, Params) -> 
 cast(?glConvolutionParameterf, <<Target:32/?UN, Pname:32/?UN, Params:32/?FN>>).

%% Func:    convolutionParameterfv 
%% Args:    Target, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glConvolutionParameterfv(GLenum target, GLenum pname,  const GLfloat * params)
convolutionParameterfv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_FLOAT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glConvolutionParameterfv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    convolutionParameteri 
%% Args:    Target, Pname, Params
%% Returns: ok
%% C-API func: void glConvolutionParameteri(GLenum target, GLenum pname, GLint params)
convolutionParameteri(Target, Pname, Params) -> 
 cast(?glConvolutionParameteri, <<Target:32/?UN, Pname:32/?UN, Params:32/?SN>>).

%% Func:    convolutionParameteriv 
%% Args:    Target, Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glConvolutionParameteriv(GLenum target, GLenum pname,  const GLint * params)
convolutionParameteriv(Target, Pname, Params) -> 
 NewParams = if
	is_list(Params) ->  ParamsLen = length(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_tuple(Params) ->  ParamsLen = size(Params), 
	  [<<ParamsLen:32/native>>, term2bin(Params, ParamsLen, ?GL_INT)];
	is_binary(Params) -> [<<(size(Params) div 4):32/native>>,Params/binary];
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glConvolutionParameteriv, [<<Target:32/?UN, Pname:32/?UN>>,NewParams]).

%% Func:    copyConvolutionFilter1D 
%% Args:    Target, Internalformat, X, Y, Width
%% Returns: ok
%% C-API func: void glCopyConvolutionFilter1D(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width)
copyConvolutionFilter1D(Target, Internalformat, X, Y, Width) -> 
 cast(?glCopyConvolutionFilter1D, <<Target:32/?UN, Internalformat:32/?UN, X:32/?SN, Y:32/?SN, Width:32/?SN>>).

%% Func:    copyConvolutionFilter2D 
%% Args:    Target, Internalformat, X, Y, Width, Height
%% Returns: ok
%% C-API func: void glCopyConvolutionFilter2D(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height)
copyConvolutionFilter2D(Target, Internalformat, X, Y, Width, Height) -> 
 cast(?glCopyConvolutionFilter2D, <<Target:32/?UN, Internalformat:32/?UN, X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN>>).

%% Func:    getConvolutionFilter 
%% Args:    Target, Format, Type, #sdlmem{} = Image
%% Returns: ok
%% C-API func: void glGetConvolutionFilter(GLenum target, GLenum format, GLenum type, GLvoid * image)
getConvolutionFilter(Target, Format, Type, #sdlmem{bin=Image}) -> 
 sdl:send_bin(Image, ?MODULE, ?LINE),
 cast(?glGetConvolutionFilter, <<Target:32/?UN, Format:32/?UN, Type:32/?UN>>).

%% Func:    getConvolutionParameterfv 
%% Args:    Target, Pname
%% Returns: [Params]
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

%% Func:    getConvolutionParameteriv 
%% Args:    Target, Pname
%% Returns: [Params]
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

%% Func:    getSeparableFilter 
%% Args:    Target, Format, Type, #sdlmem{} = Row, #sdlmem{} = Column, #sdlmem{} = Span
%% Returns: ok
%% C-API func: void glGetSeparableFilter(GLenum target, GLenum format, GLenum type, GLvoid * row, GLvoid * column, GLvoid * span)
getSeparableFilter(Target, Format, Type, #sdlmem{bin=Row}, #sdlmem{bin=Column}, #sdlmem{bin=Span}) -> 
 sdl:send_bin(Row, ?MODULE, ?LINE),
 sdl:send_bin(Column, ?MODULE, ?LINE),
 sdl:send_bin(Span, ?MODULE, ?LINE),
 cast(?glGetSeparableFilter, <<Target:32/?UN, Format:32/?UN, Type:32/?UN>>).

%% Func:    separableFilter2D 
%% Args:    Target, Internalformat, Width, Height, Format, Type, <<[Row]>>, <<[Column]>>
%% Returns: ok
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

%% Func:    getHistogram 
%% Args:    Target, Reset, Format, Type, #sdlmem{} = Values
%% Returns: ok
%% C-API func: void glGetHistogram(GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid * values)
getHistogram(Target, Reset, Format, Type, #sdlmem{bin=Values}) -> 
 sdl:send_bin(Values, ?MODULE, ?LINE),
 cast(?glGetHistogram, <<Target:32/?UN, Reset:8/unsigned, 0:24, Format:32/?UN, Type:32/?UN>>).

%% Func:    getHistogramParameterfv 
%% Args:    Target, Pname
%% Returns: [Params]
%% C-API func: void glGetHistogramParameterfv(GLenum target, GLenum pname, GLfloat * params)
getHistogramParameterfv(Target, Pname) -> 
 Bin = call(?glGetHistogramParameterfv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?FN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getHistogramParameteriv 
%% Args:    Target, Pname
%% Returns: [Params]
%% C-API func: void glGetHistogramParameteriv(GLenum target, GLenum pname, GLint * params)
getHistogramParameteriv(Target, Pname) -> 
 Bin = call(?glGetHistogramParameteriv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getMinmax 
%% Args:    Target, Reset, Format, Type, #sdlmem{} = Values
%% Returns: ok
%% C-API func: void glGetMinmax(GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid * values)
getMinmax(Target, Reset, Format, Type, #sdlmem{bin=Values}) -> 
 sdl:send_bin(Values, ?MODULE, ?LINE),
 cast(?glGetMinmax, <<Target:32/?UN, Reset:8/unsigned, 0:24, Format:32/?UN, Type:32/?UN>>).

%% Func:    getMinmaxParameterfv 
%% Args:    Target, Pname
%% Returns: [Params]
%% C-API func: void glGetMinmaxParameterfv(GLenum target, GLenum pname, GLfloat * params)
getMinmaxParameterfv(Target, Pname) -> 
 Bin = call(?glGetMinmaxParameterfv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?FN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getMinmaxParameteriv 
%% Args:    Target, Pname
%% Returns: [Params]
%% C-API func: void glGetMinmaxParameteriv(GLenum target, GLenum pname, GLint * params)
getMinmaxParameteriv(Target, Pname) -> 
 Bin = call(?glGetMinmaxParameteriv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    histogram 
%% Args:    Target, Width, Internalformat, Sink
%% Returns: ok
%% C-API func: void glHistogram(GLenum target, GLsizei width, GLenum internalformat, GLboolean sink)
histogram(Target, Width, Internalformat, Sink) -> 
 cast(?glHistogram, <<Target:32/?UN, Width:32/?SN, Internalformat:32/?UN, Sink:8/unsigned>>).

%% Func:    minmax 
%% Args:    Target, Internalformat, Sink
%% Returns: ok
%% C-API func: void glMinmax(GLenum target, GLenum internalformat, GLboolean sink)
minmax(Target, Internalformat, Sink) -> 
 cast(?glMinmax, <<Target:32/?UN, Internalformat:32/?UN, Sink:8/unsigned>>).

%% Func:    resetHistogram 
%% Args:    Target
%% Returns: ok
%% C-API func: void glResetHistogram(GLenum target)
resetHistogram(Target) -> 
 cast(?glResetHistogram, <<Target:32/?UN>>).

%% Func:    resetMinmax 
%% Args:    Target
%% Returns: ok
%% C-API func: void glResetMinmax(GLenum target)
resetMinmax(Target) -> 
 cast(?glResetMinmax, <<Target:32/?UN>>).

%% Func:    texImage3D 
%% Args:    Target, Level, Internalformat, Width, Height, Depth, Border, Format, Type, <<[Pixels]>>
%% Returns: ok
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

%% Func:    texSubImage3D 
%% Args:    Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type, <<[Pixels]>>
%% Returns: ok
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

%% Func:    copyTexSubImage3D 
%% Args:    Target, Level, Xoffset, Yoffset, Zoffset, X, Y, Width, Height
%% Returns: ok
%% C-API func: void glCopyTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)
copyTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, X, Y, Width, Height) -> 
 cast(?glCopyTexSubImage3D, <<Target:32/?UN, Level:32/?SN, Xoffset:32/?SN, Yoffset:32/?SN, Zoffset:32/?SN, X:32/?SN, Y:32/?SN, Width:32/?SN, Height:32/?SN>>).

%% Func:    activeTexture 
%% Args:    Texture
%% Returns: ok
%% C-API func: void glActiveTexture(GLenum texture)
activeTexture(Texture) -> 
 cast(?glActiveTexture, <<Texture:32/?UN>>).

%% Func:    clientActiveTexture 
%% Args:    Texture
%% Returns: ok
%% C-API func: void glClientActiveTexture(GLenum texture)
clientActiveTexture(Texture) -> 
 cast(?glClientActiveTexture, <<Texture:32/?UN>>).

%% Func:    multiTexCoord1d 
%% Args:    Target, S
%% Returns: ok
%% C-API func: void glMultiTexCoord1d(GLenum target, GLdouble s)
multiTexCoord1d(Target, S) -> 
 cast(?glMultiTexCoord1dv, <<Target:32/?UN, S:64/?FN>>).

%% Func:    multiTexCoord1dv 
%% Args:    Target, {V1}
%% Returns: ok
%% C-API func: void glMultiTexCoord1dv(GLenum target,  const GLdouble * v)
multiTexCoord1dv(Target, {V1}) -> 
 cast(?glMultiTexCoord1dv, <<Target:32/?UN, V1:64/?FN>>).

%% Func:    multiTexCoord1f 
%% Args:    Target, S
%% Returns: ok
%% C-API func: void glMultiTexCoord1f(GLenum target, GLfloat s)
multiTexCoord1f(Target, S) -> 
 cast(?glMultiTexCoord1fv, <<Target:32/?UN, S:32/?FN>>).

%% Func:    multiTexCoord1fv 
%% Args:    Target, {V1}
%% Returns: ok
%% C-API func: void glMultiTexCoord1fv(GLenum target,  const GLfloat * v)
multiTexCoord1fv(Target, {V1}) -> 
 cast(?glMultiTexCoord1fv, <<Target:32/?UN, V1:32/?FN>>).

%% Func:    multiTexCoord1i 
%% Args:    Target, S
%% Returns: ok
%% C-API func: void glMultiTexCoord1i(GLenum target, GLint s)
multiTexCoord1i(Target, S) -> 
 cast(?glMultiTexCoord1iv, <<Target:32/?UN, S:32/?SN>>).

%% Func:    multiTexCoord1iv 
%% Args:    Target, {V1}
%% Returns: ok
%% C-API func: void glMultiTexCoord1iv(GLenum target,  const GLint * v)
multiTexCoord1iv(Target, {V1}) -> 
 cast(?glMultiTexCoord1iv, <<Target:32/?UN, V1:32/?SN>>).

%% Func:    multiTexCoord1s 
%% Args:    Target, S
%% Returns: ok
%% C-API func: void glMultiTexCoord1s(GLenum target, GLshort s)
multiTexCoord1s(Target, S) -> 
 cast(?glMultiTexCoord1sv, <<Target:32/?UN, S:16/?SN>>).

%% Func:    multiTexCoord1sv 
%% Args:    Target, {V1}
%% Returns: ok
%% C-API func: void glMultiTexCoord1sv(GLenum target,  const GLshort * v)
multiTexCoord1sv(Target, {V1}) -> 
 cast(?glMultiTexCoord1sv, <<Target:32/?UN, V1:16/?SN>>).

%% Func:    multiTexCoord2d 
%% Args:    Target, S, T
%% Returns: ok
%% C-API func: void glMultiTexCoord2d(GLenum target, GLdouble s, GLdouble t)
multiTexCoord2d(Target, S, T) -> 
 cast(?glMultiTexCoord2dv, <<Target:32/?UN, S:64/?FN, T:64/?FN>>).

%% Func:    multiTexCoord2dv 
%% Args:    Target, {V1,V2}
%% Returns: ok
%% C-API func: void glMultiTexCoord2dv(GLenum target,  const GLdouble * v)
multiTexCoord2dv(Target, {V1,V2}) -> 
 cast(?glMultiTexCoord2dv, <<Target:32/?UN, V1:64/?FN,V2:64/?FN>>).

%% Func:    multiTexCoord2f 
%% Args:    Target, S, T
%% Returns: ok
%% C-API func: void glMultiTexCoord2f(GLenum target, GLfloat s, GLfloat t)
multiTexCoord2f(Target, S, T) -> 
 cast(?glMultiTexCoord2fv, <<Target:32/?UN, S:32/?FN, T:32/?FN>>).

%% Func:    multiTexCoord2fv 
%% Args:    Target, {V1,V2}
%% Returns: ok
%% C-API func: void glMultiTexCoord2fv(GLenum target,  const GLfloat * v)
multiTexCoord2fv(Target, {V1,V2}) -> 
 cast(?glMultiTexCoord2fv, <<Target:32/?UN, V1:32/?FN,V2:32/?FN>>).

%% Func:    multiTexCoord2i 
%% Args:    Target, S, T
%% Returns: ok
%% C-API func: void glMultiTexCoord2i(GLenum target, GLint s, GLint t)
multiTexCoord2i(Target, S, T) -> 
 cast(?glMultiTexCoord2iv, <<Target:32/?UN, S:32/?SN, T:32/?SN>>).

%% Func:    multiTexCoord2iv 
%% Args:    Target, {V1,V2}
%% Returns: ok
%% C-API func: void glMultiTexCoord2iv(GLenum target,  const GLint * v)
multiTexCoord2iv(Target, {V1,V2}) -> 
 cast(?glMultiTexCoord2iv, <<Target:32/?UN, V1:32/?SN,V2:32/?SN>>).

%% Func:    multiTexCoord2s 
%% Args:    Target, S, T
%% Returns: ok
%% C-API func: void glMultiTexCoord2s(GLenum target, GLshort s, GLshort t)
multiTexCoord2s(Target, S, T) -> 
 cast(?glMultiTexCoord2sv, <<Target:32/?UN, S:16/?SN, T:16/?SN>>).

%% Func:    multiTexCoord2sv 
%% Args:    Target, {V1,V2}
%% Returns: ok
%% C-API func: void glMultiTexCoord2sv(GLenum target,  const GLshort * v)
multiTexCoord2sv(Target, {V1,V2}) -> 
 cast(?glMultiTexCoord2sv, <<Target:32/?UN, V1:16/?SN,V2:16/?SN>>).

%% Func:    multiTexCoord3d 
%% Args:    Target, S, T, R
%% Returns: ok
%% C-API func: void glMultiTexCoord3d(GLenum target, GLdouble s, GLdouble t, GLdouble r)
multiTexCoord3d(Target, S, T, R) -> 
 cast(?glMultiTexCoord3dv, <<Target:32/?UN, S:64/?FN, T:64/?FN, R:64/?FN>>).

%% Func:    multiTexCoord3dv 
%% Args:    Target, {V1,V2,V3}
%% Returns: ok
%% C-API func: void glMultiTexCoord3dv(GLenum target,  const GLdouble * v)
multiTexCoord3dv(Target, {V1,V2,V3}) -> 
 cast(?glMultiTexCoord3dv, <<Target:32/?UN, V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% Func:    multiTexCoord3f 
%% Args:    Target, S, T, R
%% Returns: ok
%% C-API func: void glMultiTexCoord3f(GLenum target, GLfloat s, GLfloat t, GLfloat r)
multiTexCoord3f(Target, S, T, R) -> 
 cast(?glMultiTexCoord3fv, <<Target:32/?UN, S:32/?FN, T:32/?FN, R:32/?FN>>).

%% Func:    multiTexCoord3fv 
%% Args:    Target, {V1,V2,V3}
%% Returns: ok
%% C-API func: void glMultiTexCoord3fv(GLenum target,  const GLfloat * v)
multiTexCoord3fv(Target, {V1,V2,V3}) -> 
 cast(?glMultiTexCoord3fv, <<Target:32/?UN, V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% Func:    multiTexCoord3i 
%% Args:    Target, S, T, R
%% Returns: ok
%% C-API func: void glMultiTexCoord3i(GLenum target, GLint s, GLint t, GLint r)
multiTexCoord3i(Target, S, T, R) -> 
 cast(?glMultiTexCoord3iv, <<Target:32/?UN, S:32/?SN, T:32/?SN, R:32/?SN>>).

%% Func:    multiTexCoord3iv 
%% Args:    Target, {V1,V2,V3}
%% Returns: ok
%% C-API func: void glMultiTexCoord3iv(GLenum target,  const GLint * v)
multiTexCoord3iv(Target, {V1,V2,V3}) -> 
 cast(?glMultiTexCoord3iv, <<Target:32/?UN, V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% Func:    multiTexCoord3s 
%% Args:    Target, S, T, R
%% Returns: ok
%% C-API func: void glMultiTexCoord3s(GLenum target, GLshort s, GLshort t, GLshort r)
multiTexCoord3s(Target, S, T, R) -> 
 cast(?glMultiTexCoord3sv, <<Target:32/?UN, S:16/?SN, T:16/?SN, R:16/?SN>>).

%% Func:    multiTexCoord3sv 
%% Args:    Target, {V1,V2,V3}
%% Returns: ok
%% C-API func: void glMultiTexCoord3sv(GLenum target,  const GLshort * v)
multiTexCoord3sv(Target, {V1,V2,V3}) -> 
 cast(?glMultiTexCoord3sv, <<Target:32/?UN, V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% Func:    multiTexCoord4d 
%% Args:    Target, S, T, R, Q
%% Returns: ok
%% C-API func: void glMultiTexCoord4d(GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q)
multiTexCoord4d(Target, S, T, R, Q) -> 
 cast(?glMultiTexCoord4dv, <<Target:32/?UN, S:64/?FN, T:64/?FN, R:64/?FN, Q:64/?FN>>).

%% Func:    multiTexCoord4dv 
%% Args:    Target, {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glMultiTexCoord4dv(GLenum target,  const GLdouble * v)
multiTexCoord4dv(Target, {V1,V2,V3,V4}) -> 
 cast(?glMultiTexCoord4dv, <<Target:32/?UN, V1:64/?FN,V2:64/?FN,V3:64/?FN,V4:64/?FN>>).

%% Func:    multiTexCoord4f 
%% Args:    Target, S, T, R, Q
%% Returns: ok
%% C-API func: void glMultiTexCoord4f(GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q)
multiTexCoord4f(Target, S, T, R, Q) -> 
 cast(?glMultiTexCoord4fv, <<Target:32/?UN, S:32/?FN, T:32/?FN, R:32/?FN, Q:32/?FN>>).

%% Func:    multiTexCoord4fv 
%% Args:    Target, {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glMultiTexCoord4fv(GLenum target,  const GLfloat * v)
multiTexCoord4fv(Target, {V1,V2,V3,V4}) -> 
 cast(?glMultiTexCoord4fv, <<Target:32/?UN, V1:32/?FN,V2:32/?FN,V3:32/?FN,V4:32/?FN>>).

%% Func:    multiTexCoord4i 
%% Args:    Target, S, T, R, Q
%% Returns: ok
%% C-API func: void glMultiTexCoord4i(GLenum target, GLint s, GLint t, GLint r, GLint q)
multiTexCoord4i(Target, S, T, R, Q) -> 
 cast(?glMultiTexCoord4iv, <<Target:32/?UN, S:32/?SN, T:32/?SN, R:32/?SN, Q:32/?SN>>).

%% Func:    multiTexCoord4iv 
%% Args:    Target, {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glMultiTexCoord4iv(GLenum target,  const GLint * v)
multiTexCoord4iv(Target, {V1,V2,V3,V4}) -> 
 cast(?glMultiTexCoord4iv, <<Target:32/?UN, V1:32/?SN,V2:32/?SN,V3:32/?SN,V4:32/?SN>>).

%% Func:    multiTexCoord4s 
%% Args:    Target, S, T, R, Q
%% Returns: ok
%% C-API func: void glMultiTexCoord4s(GLenum target, GLshort s, GLshort t, GLshort r, GLshort q)
multiTexCoord4s(Target, S, T, R, Q) -> 
 cast(?glMultiTexCoord4sv, <<Target:32/?UN, S:16/?SN, T:16/?SN, R:16/?SN, Q:16/?SN>>).

%% Func:    multiTexCoord4sv 
%% Args:    Target, {V1,V2,V3,V4}
%% Returns: ok
%% C-API func: void glMultiTexCoord4sv(GLenum target,  const GLshort * v)
multiTexCoord4sv(Target, {V1,V2,V3,V4}) -> 
 cast(?glMultiTexCoord4sv, <<Target:32/?UN, V1:16/?SN,V2:16/?SN,V3:16/?SN,V4:16/?SN>>).

%% Func:    loadTransposeMatrixf 
%% Args:    <<[M]>>
%% Returns: ok
%% C-API func: void glLoadTransposeMatrixf( const GLfloat * m)
loadTransposeMatrixf(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_FLOAT);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glLoadTransposeMatrixf, [ NewM]).

%% Func:    loadTransposeMatrixd 
%% Args:    <<[M]>>
%% Returns: ok
%% C-API func: void glLoadTransposeMatrixd( const GLdouble * m)
loadTransposeMatrixd(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_DOUBLE);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glLoadTransposeMatrixd, [ NewM]).

%% Func:    multTransposeMatrixf 
%% Args:    <<[M]>>
%% Returns: ok
%% C-API func: void glMultTransposeMatrixf( const GLfloat * m)
multTransposeMatrixf(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_FLOAT);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glMultTransposeMatrixf, [ NewM]).

%% Func:    multTransposeMatrixd 
%% Args:    <<[M]>>
%% Returns: ok
%% C-API func: void glMultTransposeMatrixd( const GLdouble * m)
multTransposeMatrixd(M) -> 
 NewM = if
	is_list(M) ; is_tuple(M) -> matrix2bin(M, ?GL_DOUBLE);
	binary(M) -> M;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, M})
 end, 
 cast(?glMultTransposeMatrixd, [ NewM]).

%% Func:    sampleCoverage 
%% Args:    Value, Invert
%% Returns: ok
%% C-API func: void glSampleCoverage(GLclampf value, GLboolean invert)
sampleCoverage(Value, Invert) -> 
 cast(?glSampleCoverage, <<Value:32/?FN, Invert:8/unsigned>>).

%% Func:    compressedTexImage3D 
%% Args:    Target, Level, Internalformat, Width, Height, Depth, Border, ImageSize, <<[Data]>>
%% Returns: ok
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

%% Func:    compressedTexImage2D 
%% Args:    Target, Level, Internalformat, Width, Height, Border, ImageSize, <<[Data]>>
%% Returns: ok
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

%% Func:    compressedTexImage1D 
%% Args:    Target, Level, Internalformat, Width, Border, ImageSize, <<[Data]>>
%% Returns: ok
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

%% Func:    compressedTexSubImage3D 
%% Args:    Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, ImageSize, <<[Data]>>
%% Returns: ok
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

%% Func:    compressedTexSubImage2D 
%% Args:    Target, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize, <<[Data]>>
%% Returns: ok
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

%% Func:    compressedTexSubImage1D 
%% Args:    Target, Level, Xoffset, Width, Format, ImageSize, <<[Data]>>
%% Returns: ok
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

%% Func:    getCompressedTexImage 
%% Args:    Target, Level, #sdlmem{} = Img
%% Returns: ok
%% C-API func: void glGetCompressedTexImage(GLenum target, GLint level, GLvoid * img)
getCompressedTexImage(Target, Level, #sdlmem{bin=Img}) -> 
 sdl:send_bin(Img, ?MODULE, ?LINE),
 cast(?glGetCompressedTexImage, <<Target:32/?UN, Level:32/?SN>>).

%% Func:    blendFuncSeparate 
%% Args:    SfactorRGB, DfactorRGB, SfactorAlpha, DfactorAlpha
%% Returns: ok
%% C-API func: void glBlendFuncSeparate(GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha)
blendFuncSeparate(SfactorRGB, DfactorRGB, SfactorAlpha, DfactorAlpha) -> 
 cast(?glBlendFuncSeparate, <<SfactorRGB:32/?UN, DfactorRGB:32/?UN, SfactorAlpha:32/?UN, DfactorAlpha:32/?UN>>).

%% Func:    fogCoordf 
%% Args:    Coord
%% Returns: ok
%% C-API func: void glFogCoordf(GLfloat coord)
fogCoordf(Coord) -> 
 cast(?glFogCoordf, <<Coord:32/?FN>>).

%% Func:    fogCoordfv 
%% Args:    <<[Coord]>>
%% Returns: ok
%% C-API func: void glFogCoordfv( const GLfloat * coord)
fogCoordfv(Coord) -> 
 NewCoord = if
	is_list(Coord) ; is_tuple(Coord) -> term2bin(Coord, 1, ?GL_FLOAT);
	binary(Coord) -> Coord;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Coord})
 end, 
 cast(?glFogCoordfv, [ NewCoord]).

%% Func:    fogCoordd 
%% Args:    Coord
%% Returns: ok
%% C-API func: void glFogCoordd(GLdouble coord)
fogCoordd(Coord) -> 
 cast(?glFogCoordd, <<Coord:64/?FN>>).

%% Func:    fogCoorddv 
%% Args:    <<[Coord]>>
%% Returns: ok
%% C-API func: void glFogCoorddv( const GLdouble * coord)
fogCoorddv(Coord) -> 
 NewCoord = if
	is_list(Coord) ; is_tuple(Coord) -> term2bin(Coord, 1, ?GL_DOUBLE);
	binary(Coord) -> Coord;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Coord})
 end, 
 cast(?glFogCoorddv, [ NewCoord]).

%% Func:    fogCoordPointer 
%% Args:    Type, Stride, <<[Pointer]>>
%% Returns: ok
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

%% Func:    multiDrawArrays 
%% Args:    Mode, <<[First]>>, <<[Count]>>, Primcount
%% Returns: ok
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

%% Func:    pointParameterf 
%% Args:    Pname, Param
%% Returns: ok
%% C-API func: void glPointParameterf(GLenum pname, GLfloat param)
pointParameterf(Pname, Param) -> 
 cast(?glPointParameterf, <<Pname:32/?UN, Param:32/?FN>>).

%% Func:    pointParameterfv 
%% Args:    Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glPointParameterfv(GLenum pname,  const GLfloat * params)
pointParameterfv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 3, ?GL_FLOAT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glPointParameterfv, [<<Pname:32/?UN>>,NewParams]).

%% Func:    pointParameteri 
%% Args:    Pname, Param
%% Returns: ok
%% C-API func: void glPointParameteri(GLenum pname, GLint param)
pointParameteri(Pname, Param) -> 
 cast(?glPointParameteri, <<Pname:32/?UN, Param:32/?SN>>).

%% Func:    pointParameteriv 
%% Args:    Pname, <<[Params]>>
%% Returns: ok
%% C-API func: void glPointParameteriv(GLenum pname,  const GLint * params)
pointParameteriv(Pname, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 3, ?GL_INT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glPointParameteriv, [<<Pname:32/?UN>>,NewParams]).

%% Func:    secondaryColor3b 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glSecondaryColor3b(GLbyte red, GLbyte green, GLbyte blue)
secondaryColor3b(Red, Green, Blue) -> 
 cast(?glSecondaryColor3bv, <<Red:8/signed, Green:8/signed, Blue:8/signed>>).

%% Func:    secondaryColor3bv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glSecondaryColor3bv( const GLbyte * v)
secondaryColor3bv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3bv, <<V1:8/signed,V2:8/signed,V3:8/signed>>).

%% Func:    secondaryColor3d 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glSecondaryColor3d(GLdouble red, GLdouble green, GLdouble blue)
secondaryColor3d(Red, Green, Blue) -> 
 cast(?glSecondaryColor3dv, <<Red:64/?FN, Green:64/?FN, Blue:64/?FN>>).

%% Func:    secondaryColor3dv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glSecondaryColor3dv( const GLdouble * v)
secondaryColor3dv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% Func:    secondaryColor3f 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glSecondaryColor3f(GLfloat red, GLfloat green, GLfloat blue)
secondaryColor3f(Red, Green, Blue) -> 
 cast(?glSecondaryColor3fv, <<Red:32/?FN, Green:32/?FN, Blue:32/?FN>>).

%% Func:    secondaryColor3fv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glSecondaryColor3fv( const GLfloat * v)
secondaryColor3fv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% Func:    secondaryColor3i 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glSecondaryColor3i(GLint red, GLint green, GLint blue)
secondaryColor3i(Red, Green, Blue) -> 
 cast(?glSecondaryColor3iv, <<Red:32/?SN, Green:32/?SN, Blue:32/?SN>>).

%% Func:    secondaryColor3iv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glSecondaryColor3iv( const GLint * v)
secondaryColor3iv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% Func:    secondaryColor3s 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glSecondaryColor3s(GLshort red, GLshort green, GLshort blue)
secondaryColor3s(Red, Green, Blue) -> 
 cast(?glSecondaryColor3sv, <<Red:16/?SN, Green:16/?SN, Blue:16/?SN>>).

%% Func:    secondaryColor3sv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glSecondaryColor3sv( const GLshort * v)
secondaryColor3sv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% Func:    secondaryColor3ub 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glSecondaryColor3ub(GLubyte red, GLubyte green, GLubyte blue)
secondaryColor3ub(Red, Green, Blue) -> 
 cast(?glSecondaryColor3ubv, <<Red:8/unsigned, Green:8/unsigned, Blue:8/unsigned>>).

%% Func:    secondaryColor3ubv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glSecondaryColor3ubv( const GLubyte * v)
secondaryColor3ubv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3ubv, <<V1:8/unsigned,V2:8/unsigned,V3:8/unsigned>>).

%% Func:    secondaryColor3ui 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glSecondaryColor3ui(GLuint red, GLuint green, GLuint blue)
secondaryColor3ui(Red, Green, Blue) -> 
 cast(?glSecondaryColor3uiv, <<Red:32/?UN, Green:32/?UN, Blue:32/?UN>>).

%% Func:    secondaryColor3uiv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glSecondaryColor3uiv( const GLuint * v)
secondaryColor3uiv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3uiv, <<V1:32/?UN,V2:32/?UN,V3:32/?UN>>).

%% Func:    secondaryColor3us 
%% Args:    Red, Green, Blue
%% Returns: ok
%% C-API func: void glSecondaryColor3us(GLushort red, GLushort green, GLushort blue)
secondaryColor3us(Red, Green, Blue) -> 
 cast(?glSecondaryColor3usv, <<Red:16/?UN, Green:16/?UN, Blue:16/?UN>>).

%% Func:    secondaryColor3usv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glSecondaryColor3usv( const GLushort * v)
secondaryColor3usv({V1,V2,V3}) -> 
 cast(?glSecondaryColor3usv, <<V1:16/?UN,V2:16/?UN,V3:16/?UN>>).

%% Func:    secondaryColorPointer 
%% Args:    Size, Type, Stride, <<[Pointer]>>
%% Returns: ok
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

%% Func:    windowPos2d 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glWindowPos2d(GLdouble x, GLdouble y)
windowPos2d(X, Y) -> 
 cast(?glWindowPos2dv, <<X:64/?FN, Y:64/?FN>>).

%% Func:    windowPos2dv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glWindowPos2dv( const GLdouble * v)
windowPos2dv({V1,V2}) -> 
 cast(?glWindowPos2dv, <<V1:64/?FN,V2:64/?FN>>).

%% Func:    windowPos2f 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glWindowPos2f(GLfloat x, GLfloat y)
windowPos2f(X, Y) -> 
 cast(?glWindowPos2fv, <<X:32/?FN, Y:32/?FN>>).

%% Func:    windowPos2fv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glWindowPos2fv( const GLfloat * v)
windowPos2fv({V1,V2}) -> 
 cast(?glWindowPos2fv, <<V1:32/?FN,V2:32/?FN>>).

%% Func:    windowPos2i 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glWindowPos2i(GLint x, GLint y)
windowPos2i(X, Y) -> 
 cast(?glWindowPos2iv, <<X:32/?SN, Y:32/?SN>>).

%% Func:    windowPos2iv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glWindowPos2iv( const GLint * v)
windowPos2iv({V1,V2}) -> 
 cast(?glWindowPos2iv, <<V1:32/?SN,V2:32/?SN>>).

%% Func:    windowPos2s 
%% Args:    X, Y
%% Returns: ok
%% C-API func: void glWindowPos2s(GLshort x, GLshort y)
windowPos2s(X, Y) -> 
 cast(?glWindowPos2sv, <<X:16/?SN, Y:16/?SN>>).

%% Func:    windowPos2sv 
%% Args:    {V1,V2}
%% Returns: ok
%% C-API func: void glWindowPos2sv( const GLshort * v)
windowPos2sv({V1,V2}) -> 
 cast(?glWindowPos2sv, <<V1:16/?SN,V2:16/?SN>>).

%% Func:    windowPos3d 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glWindowPos3d(GLdouble x, GLdouble y, GLdouble z)
windowPos3d(X, Y, Z) -> 
 cast(?glWindowPos3dv, <<X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% Func:    windowPos3dv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glWindowPos3dv( const GLdouble * v)
windowPos3dv({V1,V2,V3}) -> 
 cast(?glWindowPos3dv, <<V1:64/?FN,V2:64/?FN,V3:64/?FN>>).

%% Func:    windowPos3f 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glWindowPos3f(GLfloat x, GLfloat y, GLfloat z)
windowPos3f(X, Y, Z) -> 
 cast(?glWindowPos3fv, <<X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% Func:    windowPos3fv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glWindowPos3fv( const GLfloat * v)
windowPos3fv({V1,V2,V3}) -> 
 cast(?glWindowPos3fv, <<V1:32/?FN,V2:32/?FN,V3:32/?FN>>).

%% Func:    windowPos3i 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glWindowPos3i(GLint x, GLint y, GLint z)
windowPos3i(X, Y, Z) -> 
 cast(?glWindowPos3iv, <<X:32/?SN, Y:32/?SN, Z:32/?SN>>).

%% Func:    windowPos3iv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glWindowPos3iv( const GLint * v)
windowPos3iv({V1,V2,V3}) -> 
 cast(?glWindowPos3iv, <<V1:32/?SN,V2:32/?SN,V3:32/?SN>>).

%% Func:    windowPos3s 
%% Args:    X, Y, Z
%% Returns: ok
%% C-API func: void glWindowPos3s(GLshort x, GLshort y, GLshort z)
windowPos3s(X, Y, Z) -> 
 cast(?glWindowPos3sv, <<X:16/?SN, Y:16/?SN, Z:16/?SN>>).

%% Func:    windowPos3sv 
%% Args:    {V1,V2,V3}
%% Returns: ok
%% C-API func: void glWindowPos3sv( const GLshort * v)
windowPos3sv({V1,V2,V3}) -> 
 cast(?glWindowPos3sv, <<V1:16/?SN,V2:16/?SN,V3:16/?SN>>).

%% Func:    genQueries 
%% Args:    N
%% Returns: [Ids]
%% C-API func: void glGenQueries(GLsizei n, GLuint * ids)
genQueries(N) -> 
 Bin = call(?glGenQueries, <<N:32/?SN>>), 
 case Bin of 
	<<Ids:N/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 bin2list(N, ?GL_UNSIGNED_INT, Ids);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    deleteQueries 
%% Args:    N, <<[Ids]>>
%% Returns: ok
%% C-API func: void glDeleteQueries(GLsizei n,  const GLuint * ids)
deleteQueries(N, Ids) -> 
 NewIds = if
	is_list(Ids) ; is_tuple(Ids) -> term2bin(Ids, N, ?GL_UNSIGNED_INT);
	is_binary(Ids) -> Ids;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Ids})
 end, 
 cast(?glDeleteQueries, [<<N:32/?SN>>,NewIds]).

%% Func:    isQuery 
%% Args:    Id
%% Returns: ?GL_BYTE
%% C-API func: GLboolean glIsQuery(GLuint id)
isQuery(Id) -> 
 Bin = call(?glIsQuery, <<Id:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    beginQuery 
%% Args:    Target, Id
%% Returns: ok
%% C-API func: void glBeginQuery(GLenum target, GLuint id)
beginQuery(Target, Id) -> 
 cast(?glBeginQuery, <<Target:32/?UN, Id:32/?UN>>).

%% Func:    endQuery 
%% Args:    Target
%% Returns: ok
%% C-API func: void glEndQuery(GLenum target)
endQuery(Target) -> 
 cast(?glEndQuery, <<Target:32/?UN>>).

%% Func:    getQueryiv 
%% Args:    Target, Pname
%% Returns: [Params]
%% C-API func: void glGetQueryiv(GLenum target, GLenum pname, GLint * params)
getQueryiv(Target, Pname) -> 
 Bin = call(?glGetQueryiv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getQueryObjectiv 
%% Args:    Id, Pname
%% Returns: [Params]
%% C-API func: void glGetQueryObjectiv(GLuint id, GLenum pname, GLint * params)
getQueryObjectiv(Id, Pname) -> 
 Bin = call(?glGetQueryObjectiv, <<Id:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getQueryObjectuiv 
%% Args:    Id, Pname
%% Returns: [Params]
%% C-API func: void glGetQueryObjectuiv(GLuint id, GLenum pname, GLuint * params)
getQueryObjectuiv(Id, Pname) -> 
 Bin = call(?glGetQueryObjectuiv, <<Id:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?UN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    bindBuffer 
%% Args:    Target, Buffer
%% Returns: ok
%% C-API func: void glBindBuffer(GLenum target, GLuint buffer)
bindBuffer(Target, Buffer) -> 
 cast(?glBindBuffer, <<Target:32/?UN, Buffer:32/?UN>>).

%% Func:    deleteBuffers 
%% Args:    N, <<[Buffers]>>
%% Returns: ok
%% C-API func: void glDeleteBuffers(GLsizei n,  const GLuint * buffers)
deleteBuffers(N, Buffers) -> 
 NewBuffers = if
	is_list(Buffers) ; is_tuple(Buffers) -> term2bin(Buffers, N, ?GL_UNSIGNED_INT);
	is_binary(Buffers) -> Buffers;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Buffers})
 end, 
 cast(?glDeleteBuffers, [<<N:32/?SN>>,NewBuffers]).

%% Func:    genBuffers 
%% Args:    N
%% Returns: [Buffers]
%% C-API func: void glGenBuffers(GLsizei n, GLuint * buffers)
genBuffers(N) -> 
 Bin = call(?glGenBuffers, <<N:32/?SN>>), 
 case Bin of 
	<<Buffers:N/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 bin2list(N, ?GL_UNSIGNED_INT, Buffers);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    isBuffer 
%% Args:    Buffer
%% Returns: ?GL_BYTE
%% C-API func: GLboolean glIsBuffer(GLuint buffer)
isBuffer(Buffer) -> 
 Bin = call(?glIsBuffer, <<Buffer:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    bufferData 
%% Args:    Target, Size, <<[Data]>>, Usage
%% Returns: ok
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

%% Func:    bufferSubData 
%% Args:    Target, Offset, Size, <<[Data]>>
%% Returns: ok
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

%% Func:    getBufferSubData 
%% Args:    Target, Offset, Size, #sdlmem{} = Data
%% Returns: ok
%% C-API func: void glGetBufferSubData(GLenum target, GLintptr offset, GLsizeiptr size, GLvoid * data)
getBufferSubData(Target, Offset, Size, #sdlmem{bin=Data}) -> 
 sdl:send_bin(Data, ?MODULE, ?LINE),
 cast(?glGetBufferSubData, <<Target:32/?UN, Offset:32/?UN, Size:32/?UN>>).

%% Func:    unmapBuffer 
%% Args:    Target
%% Returns: ?GL_BYTE
%% C-API func: GLboolean glUnmapBuffer(GLenum target)
unmapBuffer(Target) -> 
 Bin = call(?glUnmapBuffer, <<Target:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getBufferParameteriv 
%% Args:    Target, Pname
%% Returns: [Params]
%% C-API func: void glGetBufferParameteriv(GLenum target, GLenum pname, GLint * params)
getBufferParameteriv(Target, Pname) -> 
 Bin = call(?glGetBufferParameteriv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getBufferPointerv 
%% Args:    Target, Pname
%% Returns: Params=#sdlmem{}
%% C-API func: void glGetBufferPointerv(GLenum target, GLenum pname,  GLvoid* *params)
getBufferPointerv(Target, Pname) -> 
 Bin = call(?glGetBufferPointerv, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/big-unsigned>> -> 
	 erlang:fault({nyi, ?MODULE,?LINE});
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    weightbvARB 
%% Args:    Size, <<[Weights]>>
%% Returns: ok
%% C-API func: void glWeightbvARB(GLint size,  const GLbyte * weights)
weightbvARB(Size, Weights) -> 
 weightbv(Size, Weights).
weightbv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_BYTE);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightbvARB, [<<Size:32/?SN>>,NewWeights]).

%% Func:    weightsvARB 
%% Args:    Size, <<[Weights]>>
%% Returns: ok
%% C-API func: void glWeightsvARB(GLint size,  const GLshort * weights)
weightsvARB(Size, Weights) -> 
 weightsv(Size, Weights).
weightsv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_SHORT);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightsvARB, [<<Size:32/?SN>>,NewWeights]).

%% Func:    weightivARB 
%% Args:    Size, <<[Weights]>>
%% Returns: ok
%% C-API func: void glWeightivARB(GLint size,  const GLint * weights)
weightivARB(Size, Weights) -> 
 weightiv(Size, Weights).
weightiv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_INT);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightivARB, [<<Size:32/?SN>>,NewWeights]).

%% Func:    weightfvARB 
%% Args:    Size, <<[Weights]>>
%% Returns: ok
%% C-API func: void glWeightfvARB(GLint size,  const GLfloat * weights)
weightfvARB(Size, Weights) -> 
 weightfv(Size, Weights).
weightfv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_FLOAT);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightfvARB, [<<Size:32/?SN>>,NewWeights]).

%% Func:    weightdvARB 
%% Args:    Size, <<[Weights]>>
%% Returns: ok
%% C-API func: void glWeightdvARB(GLint size,  const GLdouble * weights)
weightdvARB(Size, Weights) -> 
 weightdv(Size, Weights).
weightdv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_DOUBLE);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightdvARB, [<<Size:32/?SN>>,NewWeights]).

%% Func:    weightubvARB 
%% Args:    Size, <<[Weights]>>
%% Returns: ok
%% C-API func: void glWeightubvARB(GLint size,  const GLubyte * weights)
weightubvARB(Size, Weights) -> 
 weightubv(Size, Weights).
weightubv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_UNSIGNED_BYTE);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightubvARB, [<<Size:32/?SN>>,NewWeights]).

%% Func:    weightusvARB 
%% Args:    Size, <<[Weights]>>
%% Returns: ok
%% C-API func: void glWeightusvARB(GLint size,  const GLushort * weights)
weightusvARB(Size, Weights) -> 
 weightusv(Size, Weights).
weightusv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_UNSIGNED_SHORT);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightusvARB, [<<Size:32/?SN>>,NewWeights]).

%% Func:    weightuivARB 
%% Args:    Size, <<[Weights]>>
%% Returns: ok
%% C-API func: void glWeightuivARB(GLint size,  const GLuint * weights)
weightuivARB(Size, Weights) -> 
 weightuiv(Size, Weights).
weightuiv(Size, Weights) -> 
 NewWeights = if
	is_list(Weights) ; is_tuple(Weights) -> term2bin(Weights, Size, ?GL_UNSIGNED_INT);
	is_binary(Weights) -> Weights;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Weights})
 end, 
 cast(?glWeightuivARB, [<<Size:32/?SN>>,NewWeights]).

%% Func:    weightPointerARB 
%% Args:    Size, Type, Stride, <<[Pointer]>>
%% Returns: ok
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
 cast(?glWeightPointerARB, [<<Size:32/?SN, Type:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% Func:    vertexBlendARB 
%% Args:    Count
%% Returns: ok
%% C-API func: void glVertexBlendARB(GLint count)
vertexBlendARB(Count) -> 
 vertexBlend(Count).
vertexBlend(Count) -> 
 cast(?glVertexBlendARB, <<Count:32/?SN>>).

%% Func:    currentPaletteMatrixARB 
%% Args:    Index
%% Returns: ok
%% C-API func: void glCurrentPaletteMatrixARB(GLint index)
currentPaletteMatrixARB(Index) -> 
 currentPaletteMatrix(Index).
currentPaletteMatrix(Index) -> 
 cast(?glCurrentPaletteMatrixARB, <<Index:32/?SN>>).

%% Func:    matrixIndexubvARB 
%% Args:    Size, <<[Indices]>>
%% Returns: ok
%% C-API func: void glMatrixIndexubvARB(GLint size,  const GLubyte * indices)
matrixIndexubvARB(Size, Indices) -> 
 matrixIndexubv(Size, Indices).
matrixIndexubv(Size, Indices) -> 
 NewIndices = if
	is_list(Indices) ; is_tuple(Indices) -> term2bin(Indices, Size, ?GL_UNSIGNED_BYTE);
	is_binary(Indices) -> Indices;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Indices})
 end, 
 cast(?glMatrixIndexubvARB, [<<Size:32/?SN>>,NewIndices]).

%% Func:    matrixIndexusvARB 
%% Args:    Size, <<[Indices]>>
%% Returns: ok
%% C-API func: void glMatrixIndexusvARB(GLint size,  const GLushort * indices)
matrixIndexusvARB(Size, Indices) -> 
 matrixIndexusv(Size, Indices).
matrixIndexusv(Size, Indices) -> 
 NewIndices = if
	is_list(Indices) ; is_tuple(Indices) -> term2bin(Indices, Size, ?GL_UNSIGNED_SHORT);
	is_binary(Indices) -> Indices;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Indices})
 end, 
 cast(?glMatrixIndexusvARB, [<<Size:32/?SN>>,NewIndices]).

%% Func:    matrixIndexuivARB 
%% Args:    Size, <<[Indices]>>
%% Returns: ok
%% C-API func: void glMatrixIndexuivARB(GLint size,  const GLuint * indices)
matrixIndexuivARB(Size, Indices) -> 
 matrixIndexuiv(Size, Indices).
matrixIndexuiv(Size, Indices) -> 
 NewIndices = if
	is_list(Indices) ; is_tuple(Indices) -> term2bin(Indices, Size, ?GL_UNSIGNED_INT);
	is_binary(Indices) -> Indices;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Indices})
 end, 
 cast(?glMatrixIndexuivARB, [<<Size:32/?SN>>,NewIndices]).

%% Func:    matrixIndexPointerARB 
%% Args:    Size, Type, Stride, <<[Pointer]>>
%% Returns: ok
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
 cast(?glMatrixIndexPointerARB, [<<Size:32/?SN, Type:32/?UN, Stride:32/?SN, NewPointer:32/?SN>>]).

%% Func:    vertexAttrib1dARB 
%% Args:    Index, X
%% Returns: ok
%% C-API func: void glVertexAttrib1dARB(GLuint index, GLdouble x)
vertexAttrib1dARB(Index, X) -> 
 vertexAttrib1d(Index, X).
vertexAttrib1d(Index, X) -> 
 cast(?glVertexAttrib1dvARB, <<Index:32/?UN, X:64/?FN>>).

%% Func:    vertexAttrib1dvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib1dvARB(GLuint index,  const GLdouble * v)
vertexAttrib1dvARB(Index, V) -> 
 vertexAttrib1dv(Index, V).
vertexAttrib1dv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 1, ?GL_DOUBLE);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib1dvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib1fARB 
%% Args:    Index, X
%% Returns: ok
%% C-API func: void glVertexAttrib1fARB(GLuint index, GLfloat x)
vertexAttrib1fARB(Index, X) -> 
 vertexAttrib1f(Index, X).
vertexAttrib1f(Index, X) -> 
 cast(?glVertexAttrib1fvARB, <<Index:32/?UN, X:32/?FN>>).

%% Func:    vertexAttrib1fvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib1fvARB(GLuint index,  const GLfloat * v)
vertexAttrib1fvARB(Index, V) -> 
 vertexAttrib1fv(Index, V).
vertexAttrib1fv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 1, ?GL_FLOAT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib1fvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib1sARB 
%% Args:    Index, X
%% Returns: ok
%% C-API func: void glVertexAttrib1sARB(GLuint index, GLshort x)
vertexAttrib1sARB(Index, X) -> 
 vertexAttrib1s(Index, X).
vertexAttrib1s(Index, X) -> 
 cast(?glVertexAttrib1svARB, <<Index:32/?UN, X:16/?SN>>).

%% Func:    vertexAttrib1svARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib1svARB(GLuint index,  const GLshort * v)
vertexAttrib1svARB(Index, V) -> 
 vertexAttrib1sv(Index, V).
vertexAttrib1sv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 1, ?GL_SHORT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib1svARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib2dARB 
%% Args:    Index, X, Y
%% Returns: ok
%% C-API func: void glVertexAttrib2dARB(GLuint index, GLdouble x, GLdouble y)
vertexAttrib2dARB(Index, X, Y) -> 
 vertexAttrib2d(Index, X, Y).
vertexAttrib2d(Index, X, Y) -> 
 cast(?glVertexAttrib2dvARB, <<Index:32/?UN, X:64/?FN, Y:64/?FN>>).

%% Func:    vertexAttrib2dvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib2dvARB(GLuint index,  const GLdouble * v)
vertexAttrib2dvARB(Index, V) -> 
 vertexAttrib2dv(Index, V).
vertexAttrib2dv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 2, ?GL_DOUBLE);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib2dvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib2fARB 
%% Args:    Index, X, Y
%% Returns: ok
%% C-API func: void glVertexAttrib2fARB(GLuint index, GLfloat x, GLfloat y)
vertexAttrib2fARB(Index, X, Y) -> 
 vertexAttrib2f(Index, X, Y).
vertexAttrib2f(Index, X, Y) -> 
 cast(?glVertexAttrib2fvARB, <<Index:32/?UN, X:32/?FN, Y:32/?FN>>).

%% Func:    vertexAttrib2fvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib2fvARB(GLuint index,  const GLfloat * v)
vertexAttrib2fvARB(Index, V) -> 
 vertexAttrib2fv(Index, V).
vertexAttrib2fv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 2, ?GL_FLOAT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib2fvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib2sARB 
%% Args:    Index, X, Y
%% Returns: ok
%% C-API func: void glVertexAttrib2sARB(GLuint index, GLshort x, GLshort y)
vertexAttrib2sARB(Index, X, Y) -> 
 vertexAttrib2s(Index, X, Y).
vertexAttrib2s(Index, X, Y) -> 
 cast(?glVertexAttrib2svARB, <<Index:32/?UN, X:16/?SN, Y:16/?SN>>).

%% Func:    vertexAttrib2svARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib2svARB(GLuint index,  const GLshort * v)
vertexAttrib2svARB(Index, V) -> 
 vertexAttrib2sv(Index, V).
vertexAttrib2sv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 2, ?GL_SHORT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib2svARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib3dARB 
%% Args:    Index, X, Y, Z
%% Returns: ok
%% C-API func: void glVertexAttrib3dARB(GLuint index, GLdouble x, GLdouble y, GLdouble z)
vertexAttrib3dARB(Index, X, Y, Z) -> 
 vertexAttrib3d(Index, X, Y, Z).
vertexAttrib3d(Index, X, Y, Z) -> 
 cast(?glVertexAttrib3dvARB, <<Index:32/?UN, X:64/?FN, Y:64/?FN, Z:64/?FN>>).

%% Func:    vertexAttrib3dvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib3dvARB(GLuint index,  const GLdouble * v)
vertexAttrib3dvARB(Index, V) -> 
 vertexAttrib3dv(Index, V).
vertexAttrib3dv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 3, ?GL_DOUBLE);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib3dvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib3fARB 
%% Args:    Index, X, Y, Z
%% Returns: ok
%% C-API func: void glVertexAttrib3fARB(GLuint index, GLfloat x, GLfloat y, GLfloat z)
vertexAttrib3fARB(Index, X, Y, Z) -> 
 vertexAttrib3f(Index, X, Y, Z).
vertexAttrib3f(Index, X, Y, Z) -> 
 cast(?glVertexAttrib3fvARB, <<Index:32/?UN, X:32/?FN, Y:32/?FN, Z:32/?FN>>).

%% Func:    vertexAttrib3fvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib3fvARB(GLuint index,  const GLfloat * v)
vertexAttrib3fvARB(Index, V) -> 
 vertexAttrib3fv(Index, V).
vertexAttrib3fv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 3, ?GL_FLOAT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib3fvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib3sARB 
%% Args:    Index, X, Y, Z
%% Returns: ok
%% C-API func: void glVertexAttrib3sARB(GLuint index, GLshort x, GLshort y, GLshort z)
vertexAttrib3sARB(Index, X, Y, Z) -> 
 vertexAttrib3s(Index, X, Y, Z).
vertexAttrib3s(Index, X, Y, Z) -> 
 cast(?glVertexAttrib3svARB, <<Index:32/?UN, X:16/?SN, Y:16/?SN, Z:16/?SN>>).

%% Func:    vertexAttrib3svARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib3svARB(GLuint index,  const GLshort * v)
vertexAttrib3svARB(Index, V) -> 
 vertexAttrib3sv(Index, V).
vertexAttrib3sv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 3, ?GL_SHORT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib3svARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4NbvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4NbvARB(GLuint index,  const GLbyte * v)
vertexAttrib4NbvARB(Index, V) -> 
 vertexAttrib4Nbv(Index, V).
vertexAttrib4Nbv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_BYTE);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4NbvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4NivARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4NivARB(GLuint index,  const GLint * v)
vertexAttrib4NivARB(Index, V) -> 
 vertexAttrib4Niv(Index, V).
vertexAttrib4Niv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_INT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4NivARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4NsvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4NsvARB(GLuint index,  const GLshort * v)
vertexAttrib4NsvARB(Index, V) -> 
 vertexAttrib4Nsv(Index, V).
vertexAttrib4Nsv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_SHORT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4NsvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4NubARB 
%% Args:    Index, X, Y, Z, W
%% Returns: ok
%% C-API func: void glVertexAttrib4NubARB(GLuint index, GLubyte x, GLubyte y, GLubyte z, GLubyte w)
vertexAttrib4NubARB(Index, X, Y, Z, W) -> 
 vertexAttrib4Nub(Index, X, Y, Z, W).
vertexAttrib4Nub(Index, X, Y, Z, W) -> 
 cast(?glVertexAttrib4NubARB, <<Index:32/?UN, X:8/unsigned, Y:8/unsigned, Z:8/unsigned, W:8/unsigned>>).

%% Func:    vertexAttrib4NubvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4NubvARB(GLuint index,  const GLubyte * v)
vertexAttrib4NubvARB(Index, V) -> 
 vertexAttrib4Nubv(Index, V).
vertexAttrib4Nubv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_UNSIGNED_BYTE);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4NubvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4NuivARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4NuivARB(GLuint index,  const GLuint * v)
vertexAttrib4NuivARB(Index, V) -> 
 vertexAttrib4Nuiv(Index, V).
vertexAttrib4Nuiv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_UNSIGNED_INT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4NuivARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4NusvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4NusvARB(GLuint index,  const GLushort * v)
vertexAttrib4NusvARB(Index, V) -> 
 vertexAttrib4Nusv(Index, V).
vertexAttrib4Nusv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_UNSIGNED_SHORT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4NusvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4bvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4bvARB(GLuint index,  const GLbyte * v)
vertexAttrib4bvARB(Index, V) -> 
 vertexAttrib4bv(Index, V).
vertexAttrib4bv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_BYTE);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4bvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4dARB 
%% Args:    Index, X, Y, Z, W
%% Returns: ok
%% C-API func: void glVertexAttrib4dARB(GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
vertexAttrib4dARB(Index, X, Y, Z, W) -> 
 vertexAttrib4d(Index, X, Y, Z, W).
vertexAttrib4d(Index, X, Y, Z, W) -> 
 cast(?glVertexAttrib4dvARB, <<Index:32/?UN, X:64/?FN, Y:64/?FN, Z:64/?FN, W:64/?FN>>).

%% Func:    vertexAttrib4dvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4dvARB(GLuint index,  const GLdouble * v)
vertexAttrib4dvARB(Index, V) -> 
 vertexAttrib4dv(Index, V).
vertexAttrib4dv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_DOUBLE);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4dvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4fARB 
%% Args:    Index, X, Y, Z, W
%% Returns: ok
%% C-API func: void glVertexAttrib4fARB(GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)
vertexAttrib4fARB(Index, X, Y, Z, W) -> 
 vertexAttrib4f(Index, X, Y, Z, W).
vertexAttrib4f(Index, X, Y, Z, W) -> 
 cast(?glVertexAttrib4fvARB, <<Index:32/?UN, X:32/?FN, Y:32/?FN, Z:32/?FN, W:32/?FN>>).

%% Func:    vertexAttrib4fvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4fvARB(GLuint index,  const GLfloat * v)
vertexAttrib4fvARB(Index, V) -> 
 vertexAttrib4fv(Index, V).
vertexAttrib4fv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_FLOAT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4fvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4ivARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4ivARB(GLuint index,  const GLint * v)
vertexAttrib4ivARB(Index, V) -> 
 vertexAttrib4iv(Index, V).
vertexAttrib4iv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_INT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4ivARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4sARB 
%% Args:    Index, X, Y, Z, W
%% Returns: ok
%% C-API func: void glVertexAttrib4sARB(GLuint index, GLshort x, GLshort y, GLshort z, GLshort w)
vertexAttrib4sARB(Index, X, Y, Z, W) -> 
 vertexAttrib4s(Index, X, Y, Z, W).
vertexAttrib4s(Index, X, Y, Z, W) -> 
 cast(?glVertexAttrib4svARB, <<Index:32/?UN, X:16/?SN, Y:16/?SN, Z:16/?SN, W:16/?SN>>).

%% Func:    vertexAttrib4svARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4svARB(GLuint index,  const GLshort * v)
vertexAttrib4svARB(Index, V) -> 
 vertexAttrib4sv(Index, V).
vertexAttrib4sv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_SHORT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4svARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4ubvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4ubvARB(GLuint index,  const GLubyte * v)
vertexAttrib4ubvARB(Index, V) -> 
 vertexAttrib4ubv(Index, V).
vertexAttrib4ubv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_UNSIGNED_BYTE);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4ubvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4uivARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4uivARB(GLuint index,  const GLuint * v)
vertexAttrib4uivARB(Index, V) -> 
 vertexAttrib4uiv(Index, V).
vertexAttrib4uiv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_UNSIGNED_INT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4uivARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttrib4usvARB 
%% Args:    Index, <<[V]>>
%% Returns: ok
%% C-API func: void glVertexAttrib4usvARB(GLuint index,  const GLushort * v)
vertexAttrib4usvARB(Index, V) -> 
 vertexAttrib4usv(Index, V).
vertexAttrib4usv(Index, V) -> 
 NewV = if
	is_list(V) ; is_tuple(V) -> term2bin(V, 4, ?GL_UNSIGNED_SHORT);
	binary(V) -> V;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, V})
 end, 
 cast(?glVertexAttrib4usvARB, [<<Index:32/?UN>>,NewV]).

%% Func:    vertexAttribPointerARB 
%% Args:    Index, Size, Type, Normalized, Stride, <<[Pointer]>>
%% Returns: ok
%% C-API func: void glVertexAttribPointerARB(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride,  const GLvoid * pointer)
vertexAttribPointerARB(Index, Size, Type, Normalized, Stride, Pointer) -> 
 vertexAttribPointer(Index, Size, Type, Normalized, Stride, Pointer).
vertexAttribPointer(Index, Size, Type, Normalized, Stride, Pointer) -> 
%% Maybe NULL or offset sometimes
 NewPointer =
   if is_integer(Pointer) -> Pointer;
      true ->
        sdl:send_bin(Pointer, ?MODULE, ?LINE),
       0
   end,
 cast(?glVertexAttribPointerARB, [<<Index:32/?UN, Size:32/?SN, Type:32/?UN, Normalized:8/unsigned, 0:24, Stride:32/?SN, NewPointer:32/?SN>>]).

%% Func:    enableVertexAttribArrayARB 
%% Args:    Index
%% Returns: ok
%% C-API func: void glEnableVertexAttribArrayARB(GLuint index)
enableVertexAttribArrayARB(Index) -> 
 enableVertexAttribArray(Index).
enableVertexAttribArray(Index) -> 
 cast(?glEnableVertexAttribArrayARB, <<Index:32/?UN>>).

%% Func:    disableVertexAttribArrayARB 
%% Args:    Index
%% Returns: ok
%% C-API func: void glDisableVertexAttribArrayARB(GLuint index)
disableVertexAttribArrayARB(Index) -> 
 disableVertexAttribArray(Index).
disableVertexAttribArray(Index) -> 
 cast(?glDisableVertexAttribArrayARB, <<Index:32/?UN>>).

%% Func:    programStringARB 
%% Args:    Target, Format, Len, <<[String]>>
%% Returns: ok
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
 cast(?glProgramStringARB, [<<Target:32/?UN, Format:32/?UN, Len:32/?SN, NewString:32/?SN>>]).

%% Func:    bindProgramARB 
%% Args:    Target, Program
%% Returns: ok
%% C-API func: void glBindProgramARB(GLenum target, GLuint program)
bindProgramARB(Target, Program) -> 
 bindProgram(Target, Program).
bindProgram(Target, Program) -> 
 cast(?glBindProgramARB, <<Target:32/?UN, Program:32/?UN>>).

%% Func:    deleteProgramsARB 
%% Args:    N, <<[Programs]>>
%% Returns: ok
%% C-API func: void glDeleteProgramsARB(GLsizei n,  const GLuint * programs)
deleteProgramsARB(N, Programs) -> 
 deletePrograms(N, Programs).
deletePrograms(N, Programs) -> 
 NewPrograms = if
	is_list(Programs) ; is_tuple(Programs) -> term2bin(Programs, N, ?GL_UNSIGNED_INT);
	is_binary(Programs) -> Programs;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Programs})
 end, 
 cast(?glDeleteProgramsARB, [<<N:32/?SN>>,NewPrograms]).

%% Func:    genProgramsARB 
%% Args:    N
%% Returns: [Programs]
%% C-API func: void glGenProgramsARB(GLsizei n, GLuint * programs)
genProgramsARB(N) -> 
 genPrograms(N).
genPrograms(N) -> 
 Bin = call(?glGenProgramsARB, <<N:32/?SN>>), 
 case Bin of 
	<<Programs:N/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 bin2list(N, ?GL_UNSIGNED_INT, Programs);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    programEnvParameter4dARB 
%% Args:    Target, Index, X, Y, Z, W
%% Returns: ok
%% C-API func: void glProgramEnvParameter4dARB(GLenum target, GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
programEnvParameter4dARB(Target, Index, X, Y, Z, W) -> 
 programEnvParameter4d(Target, Index, X, Y, Z, W).
programEnvParameter4d(Target, Index, X, Y, Z, W) -> 
 cast(?glProgramEnvParameter4dvARB, <<Target:32/?UN, Index:32/?UN, X:64/?FN, Y:64/?FN, Z:64/?FN, W:64/?FN>>).

%% Func:    programEnvParameter4dvARB 
%% Args:    Target, Index, <<[Params]>>
%% Returns: ok
%% C-API func: void glProgramEnvParameter4dvARB(GLenum target, GLuint index,  const GLdouble * params)
programEnvParameter4dvARB(Target, Index, Params) -> 
 programEnvParameter4dv(Target, Index, Params).
programEnvParameter4dv(Target, Index, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_DOUBLE);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glProgramEnvParameter4dvARB, [<<Target:32/?UN, Index:32/?UN>>,NewParams]).

%% Func:    programEnvParameter4fARB 
%% Args:    Target, Index, X, Y, Z, W
%% Returns: ok
%% C-API func: void glProgramEnvParameter4fARB(GLenum target, GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)
programEnvParameter4fARB(Target, Index, X, Y, Z, W) -> 
 programEnvParameter4f(Target, Index, X, Y, Z, W).
programEnvParameter4f(Target, Index, X, Y, Z, W) -> 
 cast(?glProgramEnvParameter4fvARB, <<Target:32/?UN, Index:32/?UN, X:32/?FN, Y:32/?FN, Z:32/?FN, W:32/?FN>>).

%% Func:    programEnvParameter4fvARB 
%% Args:    Target, Index, <<[Params]>>
%% Returns: ok
%% C-API func: void glProgramEnvParameter4fvARB(GLenum target, GLuint index,  const GLfloat * params)
programEnvParameter4fvARB(Target, Index, Params) -> 
 programEnvParameter4fv(Target, Index, Params).
programEnvParameter4fv(Target, Index, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_FLOAT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glProgramEnvParameter4fvARB, [<<Target:32/?UN, Index:32/?UN>>,NewParams]).

%% Func:    programLocalParameter4dARB 
%% Args:    Target, Index, X, Y, Z, W
%% Returns: ok
%% C-API func: void glProgramLocalParameter4dARB(GLenum target, GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
programLocalParameter4dARB(Target, Index, X, Y, Z, W) -> 
 programLocalParameter4d(Target, Index, X, Y, Z, W).
programLocalParameter4d(Target, Index, X, Y, Z, W) -> 
 cast(?glProgramLocalParameter4dvARB, <<Target:32/?UN, Index:32/?UN, X:64/?FN, Y:64/?FN, Z:64/?FN, W:64/?FN>>).

%% Func:    programLocalParameter4dvARB 
%% Args:    Target, Index, <<[Params]>>
%% Returns: ok
%% C-API func: void glProgramLocalParameter4dvARB(GLenum target, GLuint index,  const GLdouble * params)
programLocalParameter4dvARB(Target, Index, Params) -> 
 programLocalParameter4dv(Target, Index, Params).
programLocalParameter4dv(Target, Index, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_DOUBLE);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glProgramLocalParameter4dvARB, [<<Target:32/?UN, Index:32/?UN>>,NewParams]).

%% Func:    programLocalParameter4fARB 
%% Args:    Target, Index, X, Y, Z, W
%% Returns: ok
%% C-API func: void glProgramLocalParameter4fARB(GLenum target, GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)
programLocalParameter4fARB(Target, Index, X, Y, Z, W) -> 
 programLocalParameter4f(Target, Index, X, Y, Z, W).
programLocalParameter4f(Target, Index, X, Y, Z, W) -> 
 cast(?glProgramLocalParameter4fvARB, <<Target:32/?UN, Index:32/?UN, X:32/?FN, Y:32/?FN, Z:32/?FN, W:32/?FN>>).

%% Func:    programLocalParameter4fvARB 
%% Args:    Target, Index, <<[Params]>>
%% Returns: ok
%% C-API func: void glProgramLocalParameter4fvARB(GLenum target, GLuint index,  const GLfloat * params)
programLocalParameter4fvARB(Target, Index, Params) -> 
 programLocalParameter4fv(Target, Index, Params).
programLocalParameter4fv(Target, Index, Params) -> 
 NewParams = if
	is_list(Params) ; is_tuple(Params) -> term2bin(Params, 4, ?GL_FLOAT);
	binary(Params) -> Params;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Params})
 end, 
 cast(?glProgramLocalParameter4fvARB, [<<Target:32/?UN, Index:32/?UN>>,NewParams]).

%% Func:    getProgramEnvParameterdvARB 
%% Args:    Target, Index
%% Returns: [Params]
%% C-API func: void glGetProgramEnvParameterdvARB(GLenum target, GLuint index, GLdouble * params)
getProgramEnvParameterdvARB(Target, Index) -> 
 getProgramEnvParameterdv(Target, Index).
getProgramEnvParameterdv(Target, Index) -> 
 Bin = call(?glGetProgramEnvParameterdvARB, <<Target:32/?UN, Index:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_DOUBLE_SIZE>> -> 
	 bin2list(4, ?GL_DOUBLE, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getProgramEnvParameterfvARB 
%% Args:    Target, Index
%% Returns: [Params]
%% C-API func: void glGetProgramEnvParameterfvARB(GLenum target, GLuint index, GLfloat * params)
getProgramEnvParameterfvARB(Target, Index) -> 
 getProgramEnvParameterfv(Target, Index).
getProgramEnvParameterfv(Target, Index) -> 
 Bin = call(?glGetProgramEnvParameterfvARB, <<Target:32/?UN, Index:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(4, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getProgramLocalParameterdvARB 
%% Args:    Target, Index
%% Returns: [Params]
%% C-API func: void glGetProgramLocalParameterdvARB(GLenum target, GLuint index, GLdouble * params)
getProgramLocalParameterdvARB(Target, Index) -> 
 getProgramLocalParameterdv(Target, Index).
getProgramLocalParameterdv(Target, Index) -> 
 Bin = call(?glGetProgramLocalParameterdvARB, <<Target:32/?UN, Index:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_DOUBLE_SIZE>> -> 
	 bin2list(4, ?GL_DOUBLE, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getProgramLocalParameterfvARB 
%% Args:    Target, Index
%% Returns: [Params]
%% C-API func: void glGetProgramLocalParameterfvARB(GLenum target, GLuint index, GLfloat * params)
getProgramLocalParameterfvARB(Target, Index) -> 
 getProgramLocalParameterfv(Target, Index).
getProgramLocalParameterfv(Target, Index) -> 
 Bin = call(?glGetProgramLocalParameterfvARB, <<Target:32/?UN, Index:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(4, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getProgramivARB 
%% Args:    Target, Pname
%% Returns: [Params]
%% C-API func: void glGetProgramivARB(GLenum target, GLenum pname, GLint * params)
getProgramivARB(Target, Pname) -> 
 getProgramiv(Target, Pname).
getProgramiv(Target, Pname) -> 
 Bin = call(?glGetProgramivARB, <<Target:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getProgramStringARB 
%% Args:    Target, Pname, #sdlmem{} = String
%% Returns: ok
%% C-API func: void glGetProgramStringARB(GLenum target, GLenum pname, GLvoid * string)
getProgramStringARB(Target, Pname, #sdlmem{bin=String}) -> 
 getProgramString(Target, Pname, #sdlmem{bin=String}).
getProgramString(Target, Pname, #sdlmem{bin=String}) -> 
 sdl:send_bin(String, ?MODULE, ?LINE),
 cast(?glGetProgramStringARB, <<Target:32/?UN, Pname:32/?UN>>).

%% Func:    getVertexAttribdvARB 
%% Args:    Index, Pname
%% Returns: [Params]
%% C-API func: void glGetVertexAttribdvARB(GLuint index, GLenum pname, GLdouble * params)
getVertexAttribdvARB(Index, Pname) -> 
 getVertexAttribdv(Index, Pname).
getVertexAttribdv(Index, Pname) -> 
 Bin = call(?glGetVertexAttribdvARB, <<Index:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_DOUBLE_SIZE>> -> 
	 bin2list(4, ?GL_DOUBLE, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getVertexAttribfvARB 
%% Args:    Index, Pname
%% Returns: [Params]
%% C-API func: void glGetVertexAttribfvARB(GLuint index, GLenum pname, GLfloat * params)
getVertexAttribfvARB(Index, Pname) -> 
 getVertexAttribfv(Index, Pname).
getVertexAttribfv(Index, Pname) -> 
 Bin = call(?glGetVertexAttribfvARB, <<Index:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_FLOAT_SIZE>> -> 
	 bin2list(4, ?GL_FLOAT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getVertexAttribivARB 
%% Args:    Index, Pname
%% Returns: [Params]
%% C-API func: void glGetVertexAttribivARB(GLuint index, GLenum pname, GLint * params)
getVertexAttribivARB(Index, Pname) -> 
 getVertexAttribiv(Index, Pname).
getVertexAttribiv(Index, Pname) -> 
 Bin = call(?glGetVertexAttribivARB, <<Index:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:4/binary-unit:?GL_INT_SIZE>> -> 
	 bin2list(4, ?GL_INT, Params);
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getVertexAttribPointervARB 
%% Args:    Index, Pname
%% Returns: Pointer=#sdlmem{}
%% C-API func: void glGetVertexAttribPointervARB(GLuint index, GLenum pname,  GLvoid* *pointer)
getVertexAttribPointervARB(Index, Pname) -> 
 getVertexAttribPointerv(Index, Pname).
getVertexAttribPointerv(Index, Pname) -> 
 Bin = call(?glGetVertexAttribPointervARB, <<Index:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Pointer:32/big-unsigned>> -> 
	 erlang:fault({nyi, ?MODULE,?LINE});
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    isProgramARB 
%% Args:    Program
%% Returns: ?GL_BYTE
%% C-API func: GLboolean glIsProgramARB(GLuint program)
isProgramARB(Program) -> 
 isProgram(Program).
isProgram(Program) -> 
 Bin = call(?glIsProgramARB, <<Program:32/?UN>>), 
 case Bin of 
	<<Ret:8/unsigned>> -> 
   Ret /= ?GL_FALSE;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    deleteObjectARB 
%% Args:    Obj
%% Returns: ok
%% C-API func: void glDeleteObjectARB(GLhandleARB obj)
deleteObjectARB(Obj) -> 
 deleteObject(Obj).
deleteObject(Obj) -> 
 cast(?glDeleteObjectARB, <<Obj:32/?UN>>).

%% Func:    getHandleARB 
%% Args:    Pname
%% Returns: ?GL_UNSIGNED_INT
%% C-API func: GLhandleARB glGetHandleARB(GLenum pname)
getHandleARB(Pname) -> 
 getHandle(Pname).
getHandle(Pname) -> 
 Bin = call(?glGetHandleARB, <<Pname:32/?UN>>), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    detachObjectARB 
%% Args:    ContainerObj, AttachedObj
%% Returns: ok
%% C-API func: void glDetachObjectARB(GLhandleARB containerObj, GLhandleARB attachedObj)
detachObjectARB(ContainerObj, AttachedObj) -> 
 detachObject(ContainerObj, AttachedObj).
detachObject(ContainerObj, AttachedObj) -> 
 cast(?glDetachObjectARB, <<ContainerObj:32/?UN, AttachedObj:32/?UN>>).

%% Func:    createShaderObjectARB 
%% Args:    ShaderType
%% Returns: ?GL_UNSIGNED_INT
%% C-API func: GLhandleARB glCreateShaderObjectARB(GLenum shaderType)
createShaderObjectARB(ShaderType) -> 
 createShaderObject(ShaderType).
createShaderObject(ShaderType) -> 
 Bin = call(?glCreateShaderObjectARB, <<ShaderType:32/?UN>>), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    shaderSourceARB 
%% Args:    ShaderObj, Count, <<[Length]>>
%% Returns: ok
%% C-API func: void glShaderSourceARB(GLhandleARB shaderObj, GLsizei count,  const GLcharARB* *string,  const GLint * length)
shaderSourceARB(ShaderObj, Count, String, Length) -> 
 shaderSource(ShaderObj, Count, String, Length).
shaderSource(ShaderObj, Count, String, Length) -> 
 lists:foreach(fun(Values) -> sdl:send_bin(list_to_binary([Values,0]), ?MODULE, ?LINE) end, String),
 NewLength = if
	is_list(Length) ; is_tuple(Length) -> term2bin(Length, Count, ?GL_INT);
	is_binary(Length) -> Length;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Length})
 end, 
 cast(?glShaderSourceARB, [<<ShaderObj:32/?UN, Count:32/?SN>>,NewLength]).

%% Func:    compileShaderARB 
%% Args:    ShaderObj
%% Returns: ok
%% C-API func: void glCompileShaderARB(GLhandleARB shaderObj)
compileShaderARB(ShaderObj) -> 
 compileShader(ShaderObj).
compileShader(ShaderObj) -> 
 cast(?glCompileShaderARB, <<ShaderObj:32/?UN>>).

%% Func:    createProgramObjectARB 
%% Args:    
%% Returns: ?GL_UNSIGNED_INT
%% C-API func: GLhandleARB glCreateProgramObjectARB()
createProgramObjectARB() -> 
 createProgramObject().
createProgramObject() -> 
 Bin = call(?glCreateProgramObjectARB, []), 
 case Bin of 
	<<Ret:32/?UN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    attachObjectARB 
%% Args:    ContainerObj, Obj
%% Returns: ok
%% C-API func: void glAttachObjectARB(GLhandleARB containerObj, GLhandleARB obj)
attachObjectARB(ContainerObj, Obj) -> 
 attachObject(ContainerObj, Obj).
attachObject(ContainerObj, Obj) -> 
 cast(?glAttachObjectARB, <<ContainerObj:32/?UN, Obj:32/?UN>>).

%% Func:    linkProgramARB 
%% Args:    ProgramObj
%% Returns: ok
%% C-API func: void glLinkProgramARB(GLhandleARB programObj)
linkProgramARB(ProgramObj) -> 
 linkProgram(ProgramObj).
linkProgram(ProgramObj) -> 
 cast(?glLinkProgramARB, <<ProgramObj:32/?UN>>).

%% Func:    useProgramObjectARB 
%% Args:    ProgramObj
%% Returns: ok
%% C-API func: void glUseProgramObjectARB(GLhandleARB programObj)
useProgramObjectARB(ProgramObj) -> 
 useProgramObject(ProgramObj).
useProgramObject(ProgramObj) -> 
 cast(?glUseProgramObjectARB, <<ProgramObj:32/?UN>>).

%% Func:    validateProgramARB 
%% Args:    ProgramObj
%% Returns: ok
%% C-API func: void glValidateProgramARB(GLhandleARB programObj)
validateProgramARB(ProgramObj) -> 
 validateProgram(ProgramObj).
validateProgram(ProgramObj) -> 
 cast(?glValidateProgramARB, <<ProgramObj:32/?UN>>).

%% Func:    uniform1fARB 
%% Args:    Location, V0
%% Returns: ok
%% C-API func: void glUniform1fARB(GLint location, GLfloat v0)
uniform1fARB(Location, V0) -> 
 uniform1f(Location, V0).
uniform1f(Location, V0) -> 
 cast(?glUniform1fARB, <<Location:32/?SN, V0:32/?FN>>).

%% Func:    uniform2fARB 
%% Args:    Location, V0, V1
%% Returns: ok
%% C-API func: void glUniform2fARB(GLint location, GLfloat v0, GLfloat v1)
uniform2fARB(Location, V0, V1) -> 
 uniform2f(Location, V0, V1).
uniform2f(Location, V0, V1) -> 
 cast(?glUniform2fARB, <<Location:32/?SN, V0:32/?FN, V1:32/?FN>>).

%% Func:    uniform3fARB 
%% Args:    Location, V0, V1, V2
%% Returns: ok
%% C-API func: void glUniform3fARB(GLint location, GLfloat v0, GLfloat v1, GLfloat v2)
uniform3fARB(Location, V0, V1, V2) -> 
 uniform3f(Location, V0, V1, V2).
uniform3f(Location, V0, V1, V2) -> 
 cast(?glUniform3fARB, <<Location:32/?SN, V0:32/?FN, V1:32/?FN, V2:32/?FN>>).

%% Func:    uniform4fARB 
%% Args:    Location, V0, V1, V2, V3
%% Returns: ok
%% C-API func: void glUniform4fARB(GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3)
uniform4fARB(Location, V0, V1, V2, V3) -> 
 uniform4f(Location, V0, V1, V2, V3).
uniform4f(Location, V0, V1, V2, V3) -> 
 cast(?glUniform4fARB, <<Location:32/?SN, V0:32/?FN, V1:32/?FN, V2:32/?FN, V3:32/?FN>>).

%% Func:    uniform1iARB 
%% Args:    Location, V0
%% Returns: ok
%% C-API func: void glUniform1iARB(GLint location, GLint v0)
uniform1iARB(Location, V0) -> 
 uniform1i(Location, V0).
uniform1i(Location, V0) -> 
 cast(?glUniform1iARB, <<Location:32/?SN, V0:32/?SN>>).

%% Func:    uniform2iARB 
%% Args:    Location, V0, V1
%% Returns: ok
%% C-API func: void glUniform2iARB(GLint location, GLint v0, GLint v1)
uniform2iARB(Location, V0, V1) -> 
 uniform2i(Location, V0, V1).
uniform2i(Location, V0, V1) -> 
 cast(?glUniform2iARB, <<Location:32/?SN, V0:32/?SN, V1:32/?SN>>).

%% Func:    uniform3iARB 
%% Args:    Location, V0, V1, V2
%% Returns: ok
%% C-API func: void glUniform3iARB(GLint location, GLint v0, GLint v1, GLint v2)
uniform3iARB(Location, V0, V1, V2) -> 
 uniform3i(Location, V0, V1, V2).
uniform3i(Location, V0, V1, V2) -> 
 cast(?glUniform3iARB, <<Location:32/?SN, V0:32/?SN, V1:32/?SN, V2:32/?SN>>).

%% Func:    uniform4iARB 
%% Args:    Location, V0, V1, V2, V3
%% Returns: ok
%% C-API func: void glUniform4iARB(GLint location, GLint v0, GLint v1, GLint v2, GLint v3)
uniform4iARB(Location, V0, V1, V2, V3) -> 
 uniform4i(Location, V0, V1, V2, V3).
uniform4i(Location, V0, V1, V2, V3) -> 
 cast(?glUniform4iARB, <<Location:32/?SN, V0:32/?SN, V1:32/?SN, V2:32/?SN, V3:32/?SN>>).

%% Func:    uniform1fvARB 
%% Args:    Location, Count, <<[Value]>>
%% Returns: ok
%% C-API func: void glUniform1fvARB(GLint location, GLsizei count,  const GLfloat * value)
uniform1fvARB(Location, Count, Value) -> 
 uniform1fv(Location, Count, Value).
uniform1fv(Location, Count, Value) -> 
 NewValue = if
	is_list(Value) ; is_tuple(Value) -> term2bin(Value, Count, ?GL_FLOAT);
	is_binary(Value) -> Value;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Value})
 end, 
 cast(?glUniform1fvARB, [<<Location:32/?SN, Count:32/?SN>>,NewValue]).

%% Func:    uniform2fvARB 
%% Args:    Location, Count, <<[Value]>>
%% Returns: ok
%% C-API func: void glUniform2fvARB(GLint location, GLsizei count,  const GLfloat * value)
uniform2fvARB(Location, Count, Value) -> 
 uniform2fv(Location, Count, Value).
uniform2fv(Location, Count, Value) -> 
 NewValue = if
	is_list(Value) ; is_tuple(Value) -> term2bin(Value, Count*2, ?GL_FLOAT);
	is_binary(Value) -> Value;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Value})
 end, 
 cast(?glUniform2fvARB, [<<Location:32/?SN, Count:32/?SN>>,NewValue]).

%% Func:    uniform3fvARB 
%% Args:    Location, Count, <<[Value]>>
%% Returns: ok
%% C-API func: void glUniform3fvARB(GLint location, GLsizei count,  const GLfloat * value)
uniform3fvARB(Location, Count, Value) -> 
 uniform3fv(Location, Count, Value).
uniform3fv(Location, Count, Value) -> 
 NewValue = if
	is_list(Value) ; is_tuple(Value) -> term2bin(Value, Count*3, ?GL_FLOAT);
	is_binary(Value) -> Value;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Value})
 end, 
 cast(?glUniform3fvARB, [<<Location:32/?SN, Count:32/?SN>>,NewValue]).

%% Func:    uniform4fvARB 
%% Args:    Location, Count, <<[Value]>>
%% Returns: ok
%% C-API func: void glUniform4fvARB(GLint location, GLsizei count,  const GLfloat * value)
uniform4fvARB(Location, Count, Value) -> 
 uniform4fv(Location, Count, Value).
uniform4fv(Location, Count, Value) -> 
 NewValue = if
	is_list(Value) ; is_tuple(Value) -> term2bin(Value, Count*4, ?GL_FLOAT);
	is_binary(Value) -> Value;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Value})
 end, 
 cast(?glUniform4fvARB, [<<Location:32/?SN, Count:32/?SN>>,NewValue]).

%% Func:    uniform1ivARB 
%% Args:    Location, Count, <<[Value]>>
%% Returns: ok
%% C-API func: void glUniform1ivARB(GLint location, GLsizei count,  const GLint * value)
uniform1ivARB(Location, Count, Value) -> 
 uniform1iv(Location, Count, Value).
uniform1iv(Location, Count, Value) -> 
 NewValue = if
	is_list(Value) ; is_tuple(Value) -> term2bin(Value, Count, ?GL_INT);
	is_binary(Value) -> Value;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Value})
 end, 
 cast(?glUniform1ivARB, [<<Location:32/?SN, Count:32/?SN>>,NewValue]).

%% Func:    uniform2ivARB 
%% Args:    Location, Count, <<[Value]>>
%% Returns: ok
%% C-API func: void glUniform2ivARB(GLint location, GLsizei count,  const GLint * value)
uniform2ivARB(Location, Count, Value) -> 
 uniform2iv(Location, Count, Value).
uniform2iv(Location, Count, Value) -> 
 NewValue = if
	is_list(Value) ; is_tuple(Value) -> term2bin(Value, Count*2, ?GL_INT);
	is_binary(Value) -> Value;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Value})
 end, 
 cast(?glUniform2ivARB, [<<Location:32/?SN, Count:32/?SN>>,NewValue]).

%% Func:    uniform3ivARB 
%% Args:    Location, Count, <<[Value]>>
%% Returns: ok
%% C-API func: void glUniform3ivARB(GLint location, GLsizei count,  const GLint * value)
uniform3ivARB(Location, Count, Value) -> 
 uniform3iv(Location, Count, Value).
uniform3iv(Location, Count, Value) -> 
 NewValue = if
	is_list(Value) ; is_tuple(Value) -> term2bin(Value, Count*3, ?GL_INT);
	is_binary(Value) -> Value;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Value})
 end, 
 cast(?glUniform3ivARB, [<<Location:32/?SN, Count:32/?SN>>,NewValue]).

%% Func:    uniform4ivARB 
%% Args:    Location, Count, <<[Value]>>
%% Returns: ok
%% C-API func: void glUniform4ivARB(GLint location, GLsizei count,  const GLint * value)
uniform4ivARB(Location, Count, Value) -> 
 uniform4iv(Location, Count, Value).
uniform4iv(Location, Count, Value) -> 
 NewValue = if
	is_list(Value) ; is_tuple(Value) -> term2bin(Value, Count*4, ?GL_INT);
	is_binary(Value) -> Value;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Value})
 end, 
 cast(?glUniform4ivARB, [<<Location:32/?SN, Count:32/?SN>>,NewValue]).

%% Func:    uniformMatrix2fvARB 
%% Args:    Location, Count, Transpose, <<[Value]>>
%% Returns: ok
%% C-API func: void glUniformMatrix2fvARB(GLint location, GLsizei count, GLboolean transpose,  const GLfloat * value)
uniformMatrix2fvARB(Location, Count, Transpose, Value) -> 
 uniformMatrix2fv(Location, Count, Transpose, Value).
uniformMatrix2fv(Location, Count, Transpose, Value) -> 
 NewValue = if
	is_list(Value) ; is_tuple(Value) -> term2bin(Value, Count*4, ?GL_FLOAT);
	is_binary(Value) -> Value;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Value})
 end, 
 cast(?glUniformMatrix2fvARB, [<<Location:32/?SN, Count:32/?SN, Transpose:8/unsigned, 0:24>>,NewValue]).

%% Func:    uniformMatrix3fvARB 
%% Args:    Location, Count, Transpose, <<[Value]>>
%% Returns: ok
%% C-API func: void glUniformMatrix3fvARB(GLint location, GLsizei count, GLboolean transpose,  const GLfloat * value)
uniformMatrix3fvARB(Location, Count, Transpose, Value) -> 
 uniformMatrix3fv(Location, Count, Transpose, Value).
uniformMatrix3fv(Location, Count, Transpose, Value) -> 
 NewValue = if
	is_list(Value) ; is_tuple(Value) -> term2bin(Value, Count*9, ?GL_FLOAT);
	is_binary(Value) -> Value;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Value})
 end, 
 cast(?glUniformMatrix3fvARB, [<<Location:32/?SN, Count:32/?SN, Transpose:8/unsigned, 0:24>>,NewValue]).

%% Func:    uniformMatrix4fvARB 
%% Args:    Location, Count, Transpose, <<[Value]>>
%% Returns: ok
%% C-API func: void glUniformMatrix4fvARB(GLint location, GLsizei count, GLboolean transpose,  const GLfloat * value)
uniformMatrix4fvARB(Location, Count, Transpose, Value) -> 
 uniformMatrix4fv(Location, Count, Transpose, Value).
uniformMatrix4fv(Location, Count, Transpose, Value) -> 
 NewValue = if
	is_list(Value) ; is_tuple(Value) -> term2bin(Value, Count*16, ?GL_FLOAT);
	is_binary(Value) -> Value;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Value})
 end, 
 cast(?glUniformMatrix4fvARB, [<<Location:32/?SN, Count:32/?SN, Transpose:8/unsigned, 0:24>>,NewValue]).

%% Func:    getObjectParameterfvARB 
%% Args:    Obj, Pname
%% Returns: [Params]
%% C-API func: void glGetObjectParameterfvARB(GLhandleARB obj, GLenum pname, GLfloat * params)
getObjectParameterfvARB(Obj, Pname) -> 
 getObjectParameterfv(Obj, Pname).
getObjectParameterfv(Obj, Pname) -> 
 Bin = call(?glGetObjectParameterfvARB, <<Obj:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?FN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getObjectParameterivARB 
%% Args:    Obj, Pname
%% Returns: [Params]
%% C-API func: void glGetObjectParameterivARB(GLhandleARB obj, GLenum pname, GLint * params)
getObjectParameterivARB(Obj, Pname) -> 
 getObjectParameteriv(Obj, Pname).
getObjectParameteriv(Obj, Pname) -> 
 Bin = call(?glGetObjectParameterivARB, <<Obj:32/?UN, Pname:32/?UN>>), 
 case Bin of 
	<<Params:32/?SN>> -> 
	 Params;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getInfoLogARB 
%% Args:    Obj, MaxLength
%% Returns: {[Length], [InfoLog]}
%% C-API func: void glGetInfoLogARB(GLhandleARB obj, GLsizei maxLength, GLsizei * length, GLcharARB * infoLog)
getInfoLogARB(Obj, MaxLength) -> 
 getInfoLog(Obj, MaxLength).
getInfoLog(Obj, MaxLength) -> 
 Bin = call(?glGetInfoLogARB, <<Obj:32/?UN, MaxLength:32/?SN>>), 
 case Bin of 
	<<Length:32/?SN, InfoLog:Length/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 {Length, bin2list(Length, ?GL_UNSIGNED_BYTE, InfoLog)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getAttachedObjectsARB 
%% Args:    ContainerObj, MaxCount
%% Returns: {[Count], [Obj]}
%% C-API func: void glGetAttachedObjectsARB(GLhandleARB containerObj, GLsizei maxCount, GLsizei * count, GLhandleARB * obj)
getAttachedObjectsARB(ContainerObj, MaxCount) -> 
 getAttachedObjects(ContainerObj, MaxCount).
getAttachedObjects(ContainerObj, MaxCount) -> 
 Bin = call(?glGetAttachedObjectsARB, <<ContainerObj:32/?UN, MaxCount:32/?SN>>), 
 case Bin of 
	<<Count:32/?SN, Obj:Count/binary-unit:?GL_UNSIGNED_INT_SIZE>> -> 
	 {Count, bin2list(Count, ?GL_UNSIGNED_INT, Obj)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getUniformLocationARB 
%% Args:    ProgramObj, <<[Name]>>
%% Returns: ?GL_INT
%% C-API func: GLint glGetUniformLocationARB(GLhandleARB programObj,  const GLcharARB * name)
getUniformLocationARB(ProgramObj, Name) -> 
 getUniformLocation(ProgramObj, Name).
getUniformLocation(ProgramObj, Name) -> 
 sdl:send_bin(list_to_binary([Name,0]), ?MODULE, ?LINE),
 Bin = call(?glGetUniformLocationARB, <<ProgramObj:32/?UN>>), 
 case Bin of 
	<<Ret:32/?SN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getActiveUniformARB 
%% Args:    ProgramObj, Index, MaxLength
%% Returns: {[Length], [Size], [Type], [Name]}
%% C-API func: void glGetActiveUniformARB(GLhandleARB programObj, GLuint index, GLsizei maxLength, GLsizei * length, GLint * size, GLenum * type, GLcharARB * name)
getActiveUniformARB(ProgramObj, Index, MaxLength) -> 
 getActiveUniform(ProgramObj, Index, MaxLength).
getActiveUniform(ProgramObj, Index, MaxLength) -> 
 Bin = call(?glGetActiveUniformARB, <<ProgramObj:32/?UN, Index:32/?UN, MaxLength:32/?SN>>), 
 case Bin of 
	<<Length:32/?SN, Size:32/?SN, Type:32/?UN, Name:Length/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 {Length, Size, Type, bin2list(Length, ?GL_UNSIGNED_BYTE, Name)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getUniformfvARB 
%% Args:    ProgramObj, Location, #sdlmem{} = Params
%% Returns: ok
%% C-API func: void glGetUniformfvARB(GLhandleARB programObj, GLint location, GLfloat * params)
getUniformfvARB(ProgramObj, Location, #sdlmem{bin=Params}) -> 
 getUniformfv(ProgramObj, Location, #sdlmem{bin=Params}).
getUniformfv(ProgramObj, Location, #sdlmem{bin=Params}) -> 
 sdl:send_bin(Params, ?MODULE, ?LINE),
 cast(?glGetUniformfvARB, <<ProgramObj:32/?UN, Location:32/?SN>>).

%% Func:    getUniformivARB 
%% Args:    ProgramObj, Location, #sdlmem{} = Params
%% Returns: ok
%% C-API func: void glGetUniformivARB(GLhandleARB programObj, GLint location, GLint * params)
getUniformivARB(ProgramObj, Location, #sdlmem{bin=Params}) -> 
 getUniformiv(ProgramObj, Location, #sdlmem{bin=Params}).
getUniformiv(ProgramObj, Location, #sdlmem{bin=Params}) -> 
 sdl:send_bin(Params, ?MODULE, ?LINE),
 cast(?glGetUniformivARB, <<ProgramObj:32/?UN, Location:32/?SN>>).

%% Func:    getShaderSourceARB 
%% Args:    Obj, MaxLength
%% Returns: {[Length], [Source]}
%% C-API func: void glGetShaderSourceARB(GLhandleARB obj, GLsizei maxLength, GLsizei * length, GLcharARB * source)
getShaderSourceARB(Obj, MaxLength) -> 
 getShaderSource(Obj, MaxLength).
getShaderSource(Obj, MaxLength) -> 
 Bin = call(?glGetShaderSourceARB, <<Obj:32/?UN, MaxLength:32/?SN>>), 
 case Bin of 
	<<Length:32/?SN, Source:Length/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 {Length, bin2list(Length, ?GL_UNSIGNED_BYTE, Source)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    bindAttribLocationARB 
%% Args:    ProgramObj, Index, <<[Name]>>
%% Returns: ok
%% C-API func: void glBindAttribLocationARB(GLhandleARB programObj, GLuint index,  const GLcharARB * name)
bindAttribLocationARB(ProgramObj, Index, Name) -> 
 bindAttribLocation(ProgramObj, Index, Name).
bindAttribLocation(ProgramObj, Index, Name) -> 
 sdl:send_bin(list_to_binary([Name,0]), ?MODULE, ?LINE),
 cast(?glBindAttribLocationARB, <<ProgramObj:32/?UN, Index:32/?UN>>).

%% Func:    getActiveAttribARB 
%% Args:    ProgramObj, Index, MaxLength
%% Returns: {[Length], [Size], [Type], [Name]}
%% C-API func: void glGetActiveAttribARB(GLhandleARB programObj, GLuint index, GLsizei maxLength, GLsizei * length, GLint * size, GLenum * type, GLcharARB * name)
getActiveAttribARB(ProgramObj, Index, MaxLength) -> 
 getActiveAttrib(ProgramObj, Index, MaxLength).
getActiveAttrib(ProgramObj, Index, MaxLength) -> 
 Bin = call(?glGetActiveAttribARB, <<ProgramObj:32/?UN, Index:32/?UN, MaxLength:32/?SN>>), 
 case Bin of 
	<<Length:32/?SN, Size:32/?SN, Type:32/?UN, Name:Length/binary-unit:?GL_UNSIGNED_BYTE_SIZE>> -> 
	 {Length, Size, Type, bin2list(Length, ?GL_UNSIGNED_BYTE, Name)};
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    getAttribLocationARB 
%% Args:    ProgramObj, <<[Name]>>
%% Returns: ?GL_INT
%% C-API func: GLint glGetAttribLocationARB(GLhandleARB programObj,  const GLcharARB * name)
getAttribLocationARB(ProgramObj, Name) -> 
 getAttribLocation(ProgramObj, Name).
getAttribLocation(ProgramObj, Name) -> 
 sdl:send_bin(list_to_binary([Name,0]), ?MODULE, ?LINE),
 Bin = call(?glGetAttribLocationARB, <<ProgramObj:32/?UN>>), 
 case Bin of 
	<<Ret:32/?SN>> -> 
   Ret;
	Else -> erlang:fault({?MODULE, ?LINE, badtype, Else})
 end.

%% Func:    drawBuffersARB 
%% Args:    N, <<[Bufs]>>
%% Returns: ok
%% C-API func: void glDrawBuffersARB(GLsizei n,  const GLenum * bufs)
drawBuffersARB(N, Bufs) -> 
 drawBuffers(N, Bufs).
drawBuffers(N, Bufs) -> 
 NewBufs = if
	is_list(Bufs) ; is_tuple(Bufs) -> term2bin(Bufs, N, ?GL_INT);
	is_binary(Bufs) -> Bufs;
	true -> erlang:fault({?MODULE, ?LINE, unsupported_type, Bufs})
 end, 
 cast(?glDrawBuffersARB, [<<N:32/?SN>>,NewBufs]).

%% Func:    stencilOpSeparateATI 
%% Args:    Face, Sfail, Dpfail, Dppass
%% Returns: ok
%% C-API func: void glStencilOpSeparateATI(GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass)
stencilOpSeparateATI(Face, Sfail, Dpfail, Dppass) -> 
 cast(?glStencilOpSeparateATI, <<Face:32/?UN, Sfail:32/?UN, Dpfail:32/?UN, Dppass:32/?UN>>).

%% Func:    stencilFuncSeparateATI 
%% Args:    Frontfunc, Backfunc, Ref, Mask
%% Returns: ok
%% C-API func: void glStencilFuncSeparateATI(GLenum frontfunc, GLenum backfunc, GLint ref, GLuint mask)
stencilFuncSeparateATI(Frontfunc, Backfunc, Ref, Mask) -> 
 cast(?glStencilFuncSeparateATI, <<Frontfunc:32/?UN, Backfunc:32/?UN, Ref:32/?SN, Mask:32/?UN>>).

