%%%-------------------------------------------------------------------
%%% File    : conv_glext.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : glext specific stuff
%%%
%%% Created : 10 Jan 2003 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(conv_glext).

-compile(export_all).

add() ->
    [
     {"glDrawRangeElements", {[{"indices", "count","type"}], []}},
     {"glColorTable", {[{"table", pointer, "type"}], []}},
     {"glColorTableParameterfv", {[{"params", 4}], []}},
     {"glColorTableParameteriv", {[{"params", 4}], []}},
     {"glGetColorTable", {[{"table", sdlmem}], []}},
     {"glGetColorTableParameterfv", {[{"params", 4}], []}},
     {"glGetColorTableParameteriv", {[{"params", 4}], []}},
     {"glColorSubTable", {[{"data", pointer, "type"}], []}},
     {"glConvolutionFilter1D", {[{"image", pointer, "type"}], []}},
     {"glConvolutionFilter2D", {[{"image", pointer, "type"}], []}},
     {"glConvolutionParameterfv", {[{"params", {undefined, 4, xx}}], []}},
     {"glConvolutionParameteriv", {[{"params", {undefined, 4, xx}}], []}},
     {"glGetConvolutionFilter", {[{"image", sdlmem}], []}},
     {"glGetConvolutionParameterfv", {[{"params", {undefined, 4, "pname"}}], []}},
     {"glGetConvolutionParameteriv", {[{"params", {undefined, 4, "pname"}}], []}},
     {"glGetSeparableFilter", {[{"row", sdlmem}, {"column", sdlmem},{"span", sdlmem}],[]}},
     {"glSeparableFilter2D", {[{"column",pointer,"type"}, {"row",pointer,"type"}],[]}},
     {"glGetHistogram", {[{"values", sdlmem}], []}},
     {"glGetHistogramParameterfv", {[{"params", 1}], []}},
     {"glGetHistogramParameteriv", {[{"params", 1}], []}},
     {"glGetMinmax", {[{"values", sdlmem}], []}},
     {"glGetMinmaxParameterfv", {[{"params", 1}], []}},
     {"glGetMinmaxParameteriv", {[{"params", 1}], []}},
     {"glTexImage3D", {[{"pixels", pointer, "type"}], []}},
     {"glTexSubImage3D", {[{"pixels", pointer, "type"}], []}},
     {"glLoadTransposeMatrixf", {[{"m", 16}], []}},
     {"glLoadTransposeMatrixd", {[{"m", 16}], []}},
     {"glMultTransposeMatrixf", {[{"m", 16}], []}},
     {"glMultTransposeMatrixd", {[{"m", 16}], []}},
     {"glCompressedTexImage3D", {[{"data", pointer, "?GL_UNSIGNED_BYTE"}], []}}, 
     {"glCompressedTexImage2D", {[{"data", pointer, "?GL_UNSIGNED_BYTE"}], []}}, %?
     {"glCompressedTexImage1D", {[{"data", pointer, "?GL_UNSIGNED_BYTE"}], []}}, %?
     {"glCompressedTexSubImage1D", {[{"data", pointer, "?GL_UNSIGNED_BYTE"}], []}},
     {"glCompressedTexSubImage2D", {[{"data", pointer, "?GL_UNSIGNED_BYTE"}], []}},
     {"glCompressedTexSubImage3D", {[{"data", pointer, "?GL_UNSIGNED_BYTE"}], []}},
     {"glGetCompressedTexImage", {[{"img", sdlmem}], []}},
     {"glFogCoordfv", {[{"coord", 1}], []}},  %% ?? Guess
     {"glFogCoorddv", {[{"coord", 1}], []}},  %% ?? Guess
     {"glFogCoordPointer", {[{"pointer", pointer, "type"}], []}},
     {"glMultiDrawArrays", {[{"first", {const, "primcount"}},
			     {"count",{const,"primcount"}}], 
			    ["primcount","mode","first","count"]}},
     {"glPointParameterfv", {[{"params", 3}], []}},
     {"glPointParameteriv", {[{"params", 3}], []}},
     {"glSecondaryColorPointer", {[{"pointer", pointer, "type"}], []}},
     {"glMultiDrawElements", {skip, []}}, %% Not supported
     %% ARBs
     {"glWeightbvARB", {[{"weights", "size"}], []}},
     {"glWeightsvARB", {[{"weights", "size"}], []}},
     {"glWeightivARB", {[{"weights", "size"}], []}},
     {"glWeightfvARB", {[{"weights", "size"}], []}},
     {"glWeightdvARB", {[{"weights", "size"}], []}},
     {"glWeightubvARB", {[{"weights", "size"}], []}},
     {"glWeightusvARB", {[{"weights", "size"}], []}},
     {"glWeightuivARB", {[{"weights", "size"}], []}},
     {"glWeightPointerARB", {[{"pointer", pointer, "type"}], []}},
     {"glMatrixIndexubvARB", {[{"indices", "size"}], []}},
     {"glMatrixIndexusvARB", {[{"indices", "size"}], []}},
     {"glMatrixIndexuivARB", {[{"indices", "size"}], []}},
     {"glMatrixIndexPointerARB", {[{"pointer", pointer, "type"}], []}},
     {"glVertexAttrib1dvARB", {[{"v", 1}], []}},
     {"glVertexAttrib1fvARB", {[{"v", 1}], []}},
     {"glVertexAttrib1svARB", {[{"v", 1}], []}},
     {"glVertexAttrib2dvARB", {[{"v", 2}], []}},
     {"glVertexAttrib2fvARB", {[{"v", 2}], []}},
     {"glVertexAttrib2svARB", {[{"v", 2}], []}},
     {"glVertexAttrib3dvARB", {[{"v", 3}], []}},
     {"glVertexAttrib3fvARB", {[{"v", 3}], []}},
     {"glVertexAttrib3svARB", {[{"v", 3}], []}},
     {"glVertexAttrib4NbvARB", {[{"v", 4}], []}},
     {"glVertexAttrib4NivARB", {[{"v", 4}], []}},
     {"glVertexAttrib4NsvARB", {[{"v", 4}], []}},
     {"glVertexAttrib4NubvARB", {[{"v", 4}], []}},
     {"glVertexAttrib4NuivARB", {[{"v", 4}], []}},
     {"glVertexAttrib4NusvARB", {[{"v", 4}], []}},
     {"glVertexAttrib4dvARB", {[{"v", 4}], []}},
     {"glVertexAttrib4fvARB", {[{"v", 4}], []}},
     {"glVertexAttrib4svARB", {[{"v", 4}], []}},
     {"glVertexAttrib4bvARB", {[{"v", 4}], []}},
     {"glVertexAttrib4ivARB", {[{"v", 4}], []}},
     {"glVertexAttrib4usvARB", {[{"v", 4}], []}},
     {"glVertexAttrib4ubvARB", {[{"v", 4}], []}},
     {"glVertexAttrib4uivARB", {[{"v", 4}], []}},
     {"glVertexAttribPointerARB", {[{"pointer", pointer, "type"}], []}},
     {"glProgramStringARB", {[{"string", pointer, "GL_UNSIGNED_BYTE"}], []}},
     {"glDeleteProgramsARB", {[{"programs", "n"}], []}},
     {"glGenProgramsARB", {[{"programs", "n"}], []}},
     {"glProgramEnvParameter4dvARB", {[{"params", 4}], []}},
     {"glProgramEnvParameter4fvARB", {[{"params", 4}], []}},
     {"glProgramLocalParameter4dvARB", {[{"params", 4}], []}},
     {"glProgramLocalParameter4fvARB", {[{"params", 4}], []}},
     {"glGetProgramEnvParameterdvARB", {[{"params", 4}], []}},
     {"glGetProgramEnvParameterfvARB", {[{"params", 4}], []}},
     {"glGetProgramLocalParameterdvARB", {[{"params", 4}], []}},
     {"glGetProgramLocalParameterfvARB", {[{"params", 4}], []}},
     {"glGetProgramivARB", {[{"params", 1}], []}},
     {"glGetProgramStringARB",  {[{"string", sdlmem}], []}},
     {"glGetVertexAttribdvARB", {[{"params", 4}], []}},  % {undefined, 4, "pname"} ?
     {"glGetVertexAttribfvARB", {[{"params", 4}], []}},  % {undefined, 4, "pname"} ?
     {"glGetVertexAttribivARB", {[{"params", 4}], []}}   % {undefined, 4, "pname"} ?
    ].

init_erl(Fd) -> 
    io:format(Fd, "
%%% OPENGL 1.2 and later with selected ARB's and extensions
%%-module(glext).
-include(\"glext_funcs.hrl\").

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
", []),
    ok.
init_c(Fd) -> 
    io:format(Fd, "
#include <stdlib.h>
#include <string.h>
#include \"esdl.h\"
#ifndef APIENTRY
#define APIENTRY
#endif
#include \"esdl_glext.h\"
~n", []).
init_hrl(Fd) -> 
%    io:format(Fd, "#include <GL/glext.h>~n", []).
    ok.

init_h(Fd) ->
    io:format(Fd, "
#ifdef ESDL_DEFINE_EXTS
# define ESDL_EXTERN
#else
# define ESDL_EXTERN extern
#endif

", []).

