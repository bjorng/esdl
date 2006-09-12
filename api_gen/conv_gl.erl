%%%----------------------------------------------------------------------
%%% File    : conv.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created : 11 Oct 2001 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

%% Some knowledge about lengths of arrays and similar stuff.

-module(conv_gl).
-export([add/0]).
-compile(export_all).

add() ->
    [
     {"glAreTexturesResident",{[{"residences","n"},{"textures","n"}],[]}},
     {"glBitmap", {[{"bitmap", pointer}], []}},
     {"glCallLists", {[{"lists", "n", "type"}], []}},
     {"glClipPlane", {[{"equation", 4}], []}},

     {"glDeleteTextures", {[{"textures", "n"}], []}},
     {"glDrawElements", {[{"indices", {index_or_list,"count"}, "type"}], []}},
     {"glDrawPixels", {[{"pixels", pointer, "type"}], []}},
     {"glEdgeFlagv", {[{"flag", 1}], []}},

     {"glFogfv", {[{"params", {undefined, 4,x}}], []}},
     {"glFogiv", {[{"params", {undefined, 4,x}}], []}},

     {"glGenTextures", {[{"textures", "n"}], []}},
     {"glGetBooleanv", {[{"params", {undefined,16,"pname"}}], []}},%% Variable
     {"glGetDoublev", {[{"params",  {undefined,16,"pname"}}], []}},%% Variable
     {"glGetFloatv", {[{"params",   {undefined,16,"pname"}}], []}},%% Variable
     {"glGetIntegerv", {[{"params", {undefined,16,"pname"}}], []}},%% Variable
     {"glGetClipPlane", {[{"equation", 4}], []}},
     {"glGetLightfv", {[{"params", {undefined, 4,"pname"}}], []}}, %% Variable
     {"glGetLightiv", {[{"params", {undefined, 4,"pname"}}], []}}, %% Variable
     {"glGetMapdv", {[{"v", sdlmem}], []}},%% undefined Variable ?
     {"glGetMapfv", {[{"v", sdlmem}], []}},%% undefined Variable ?
     {"glGetMapiv", {[{"v", sdlmem}], []}},%% undefined Variable ?
     {"glGetPointerv", {[{"params", sdlmem}], []}},%% undefined Variable ?
     {"glGetMaterialfv", {[{"params", {undefined, 4, "pname"}}], []}},%% Variable
     {"glGetMaterialiv", {[{"params", {undefined, 4, "pname"}}], []}},%% Variable
     {"glGetPixelMapfv", {[{"values",  sdlmem}], []}},
     {"glGetPixelMapuiv", {[{"values", sdlmem}], []}},
     {"glGetPixelMapusv", {[{"values", sdlmem}], []}},
     {"glGetPolygonStipple", {[{"mask", 128}], []}},	 
     {"glGetTexGendv", {[{"params", {undefined, 4, "pname"}}], []}},%% Variable
     {"glGetTexGenfv", {[{"params", {undefined, 4, "pname"}}], []}},%% Variable
     {"glGetTexGeniv", {[{"params", {undefined, 4, "pname"}}], []}},%% Variable
     {"glGetTexEnvfv", {[{"params", {undefined, 4, "pname"}}], []}},%% Variable Max 4
     {"glGetTexEnviv", {[{"params", {undefined, 4, "pname"}}], []}}, %% Variable Max 4
     {"glGetTexParameterfv", {[{"params", {undefined, 4, "pname"}}], []}},%% Variable Max 4
     {"glGetTexParameteriv", {[{"params", {undefined, 4, "pname"}}], []}},%% Variable Max 4
     {"glGetTexLevelParameterfv", {[{"params", 1}], []}},
     {"glGetTexLevelParameteriv", {[{"params", 1}], []}},

     {"glIndexdv", {[{"c", 1}], []}},
     {"glIndexfv", {[{"c", 1}], []}},
     {"glIndexiv", {[{"c", 1}], []}},
     {"glIndexsv", {[{"c", 1}], []}},
     {"glIndexubv", {[{"c", 1}], []}},
     {"glPixelMapfv", {[{"values", "mapsize"}], []}},
     {"glPixelMapuiv", {[{"values", "mapsize"}], []}},
     {"glPixelMapusv", {[{"values", "mapsize"}], []}},
     {"glLightModelfv", {[{"params", {undefined, 4,x}}], []}},
     {"glLightModeliv", {[{"params", {undefined, 4,x}}], []}},
     {"glLightfv", {[{"params", {undefined, 4,x}}], []}},
     {"glLightiv", {[{"params", {undefined, 4,x}}], []}},
     {"glLoadMatrixd", {[{"m", 16}], []}},
     {"glLoadMatrixf", {[{"m", 16}], []}},
     {"glMaterialfv", {[{"params", {undefined, 4,x}}], []}},
     {"glMaterialiv", {[{"params", {undefined, 4,x}}], []}},
     {"glMultMatrixd", {[{"m", 16}], []}},
     {"glMultMatrixf", {[{"m", 16}], []}},
     {"glPolygonStipple", {[{"mask", 128}], []}},
     {"glPrioritizeTextures", {[{"priorities", "n"}, {"textures", "n"}], []}},

     {"glRectdv", {[{"v1", 2},{"v2", 2}], []}},
     {"glRectfv", {[{"v1", 2},{"v2", 2}], []}},
     {"glRectiv", {[{"v1", 2},{"v2", 2}], []}},
     {"glRectsv", {[{"v1", 2},{"v2", 2}], []}},

     {"glTexEnvfv", {[{"params", {undefined, 4,x}}], []}},
     {"glTexEnviv", {[{"params", {undefined, 4,x}}], []}},
     {"glTexGendv", {[{"params", 4}], []}},
     {"glTexGenfv", {[{"params", 4}], []}},
     {"glTexGeniv", {[{"params", 4}], []}},
     {"glTexImage1D", {[{"pixels", pointer, "type"}], []}},
     {"glTexImage2D", {[{"pixels", pointer, "type"}], []}},
     {"glTexParameterfv", {[{"params", {undefined, 4,x}}], []}},
     {"glTexParameteriv", {[{"params", {undefined, 4,x}}], []}},
     {"glTexSubImage1D", {[{"pixels", pointer, "type"}], []}},
     {"glTexSubImage2D", {[{"pixels", pointer, "type"}], []}},

     {"glFeedbackBuffer", {[{"buffer", sdlmem}], []}},
     {"glSelectBuffer", {[{"buffer", sdlmem}], []}},
     {"glReadPixels", {[{"pixels", sdlmem}], []}},
     {"glGetTexImage", {[{"pixels", sdlmem}], []}},  %% sdlmem actually possible to count the size of the correct buffer using the args see man page.
     {"glColorPointer", {[{"pointer", pointer, "type"}], []}},
     {"glEdgeFlagPointer", {[{"pointer", pointer, "?GLboolean"}], []}},
     {"glIndexPointer", {[{"pointer", pointer, "type"}], []}},
     {"glInterleavedArrays", {[{"pointer", pointer, "format"}], []}},
     {"glNormalPointer", {[{"pointer", pointer, "type"}], []}},
     {"glTexCoordPointer", {[{"pointer", pointer, "type"}], []}},
     {"glVertexPointer", {[{"pointer", pointer, "type"}, {"ptr", pointer, "type"}], []}},
     
     {"glMap1d", {[{"points", {undefined, "order * 4",x}}], []}},
     {"glMap1f", {[{"points", {undefined, "order * 4",x}}], []}},
     {"glMap2d", {[{"points", {undefined, "uorder * vorder * 4",x}}], []}},
     {"glMap2f", {[{"points", {undefined, "uorder * vorder * 4",x}}], []}}
    ].

init_erl(Fd) ->
    io:format(Fd, "
-module(gl).
-include(\"esdl.hrl\").
-include(\"gl.hrl\").
-include(\"gl_funcs.hrl\").
-include(\"sdl_util.hrl\").
~n
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

", []).

init_hrl(Fd) ->
    io:format(Fd, "
-define(GL_BYTE_SIZE, 8).
-define(GL_UNSIGNED_BYTE_SIZE, 8).
-define(GL_SHORT_SIZE, 16).
-define(GL_UNSIGNED_SHORT_SIZE, 16).
-define(GL_INT_SIZE, 32).
-define(GL_UNSIGNED_INT_SIZE, 32).
-define(GL_FLOAT_SIZE, 32).
-define(GL_DOUBLE_SIZE, 64).

-define(gl_type_size(TYPE), 
	case (TYPE) of 
	    ?GL_BYTE ->           ?GL_BYTE_SIZE;
	    ?GL_UNSIGNED_BYTE->   ?GL_UNSIGNED_BYTE_SIZE;
	    ?GL_SHORT ->          ?GL_SHORT_SIZE;
	    ?GL_UNSIGNED_SHORT -> ?GL_UNSIGNED_SHORT_SIZE;
	    ?GL_INT ->            ?GL_INT_SIZE;
	    ?GL_UNSIGNED_INT ->   ?GL_UNSIGNED_INT_SIZE;
	    ?GL_FLOAT ->          ?GL_FLOAT_SIZE;
	    ?GL_DOUBLE ->         ?GL_DOUBLE_SIZE
	end).
",[]).

init_c(Fd) ->
    io:format(Fd, "
#include <stdlib.h>
#include <string.h>
#include \"esdl.h\"
", []).

% post_erl(Fd) ->
%     io:format(Fd, "
% -include(\"glext.erl\").
% ", []).
% post_hrl(Fd) ->
%     io:format(Fd, "
% -include(\"glext.hrl\").
% ", []).
