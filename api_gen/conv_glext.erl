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
     {"glVertexAttrib1dv", {[{"v", 1}], []}},
     {"glVertexAttrib1fv", {[{"v", 1}], []}},
     {"glVertexAttrib1sv", {[{"v", 1}], []}},
     {"glVertexAttrib2dv", {[{"v", 2}], []}},
     {"glVertexAttrib2fv", {[{"v", 2}], []}},
     {"glVertexAttrib2sv", {[{"v", 2}], []}},
     {"glVertexAttrib3dv", {[{"v", 3}], []}},
     {"glVertexAttrib3fv", {[{"v", 3}], []}},
     {"glVertexAttrib3sv", {[{"v", 3}], []}},
     {"glVertexAttrib4Nbv", {[{"v", 4}], []}},
     {"glVertexAttrib4Niv", {[{"v", 4}], []}},
     {"glVertexAttrib4Nsv", {[{"v", 4}], []}},
     {"glVertexAttrib4Nubv", {[{"v", 4}], []}},
     {"glVertexAttrib4Nuiv", {[{"v", 4}], []}},
     {"glVertexAttrib4Nusv", {[{"v", 4}], []}},
     {"glVertexAttrib4dv", {[{"v", 4}], []}},
     {"glVertexAttrib4fv", {[{"v", 4}], []}},
     {"glVertexAttrib4sv", {[{"v", 4}], []}},
     {"glVertexAttrib4bv", {[{"v", 4}], []}},
     {"glVertexAttrib4iv", {[{"v", 4}], []}},
     {"glVertexAttrib4usv", {[{"v", 4}], []}},
     {"glVertexAttrib4ubv", {[{"v", 4}], []}},
     {"glVertexAttrib4uiv", {[{"v", 4}], []}},
     %% Skip
     {"glVertexAttrib1dARB", {skip, []}},
     {"glVertexAttrib1fARB", {skip, []}},
     {"glVertexAttrib1sARB", {skip, []}},
     {"glVertexAttrib1dvARB", {skip, []}},
     {"glVertexAttrib1fvARB", {skip, []}},
     {"glVertexAttrib1svARB", {skip, []}},
     {"glVertexAttrib2dvARB", {skip, []}},
     {"glVertexAttrib2fvARB", {skip, []}},
     {"glVertexAttrib2svARB", {skip, []}},
     {"glVertexAttrib2dARB", {skip, []}},
     {"glVertexAttrib2fARB", {skip, []}},
     {"glVertexAttrib2sARB", {skip, []}},
     {"glVertexAttrib3dvARB", {skip, []}},
     {"glVertexAttrib3fvARB", {skip, []}},
     {"glVertexAttrib3svARB", {skip, []}},
     {"glVertexAttrib3dARB", {skip, []}},
     {"glVertexAttrib3fARB", {skip, []}},
     {"glVertexAttrib3sARB", {skip, []}},
     {"glVertexAttrib4NbvARB", {skip, []}},
     {"glVertexAttrib4NivARB", {skip, []}},
     {"glVertexAttrib4NsvARB", {skip, []}},
     {"glVertexAttrib4dARB", {skip, []}},
     {"glVertexAttrib4fARB", {skip, []}},
     {"glVertexAttrib4sARB", {skip, []}},
     {"glVertexAttrib4NubvARB", {skip, []}},
     {"glVertexAttrib4NuivARB", {skip, []}},
     {"glVertexAttrib4NusvARB", {skip, []}},
     {"glVertexAttrib4dvARB", {skip, []}},
     {"glVertexAttrib4fvARB", {skip, []}},
     {"glVertexAttrib4svARB", {skip, []}},
     {"glVertexAttrib4bvARB", {skip, []}},
     {"glVertexAttrib4ivARB", {skip, []}},
     {"glVertexAttrib4usvARB", {skip, []}},
     {"glVertexAttrib4ubvARB", {skip, []}},
     {"glVertexAttrib4uivARB", {skip, []}},
     {"glVertexAttrib4NubARB", {skip, []}},
     {"glEnableVertexAttribArrayARB", {skip, []}},
     {"glDisableVertexAttribArrayARB", {skip, []}},
     {"glIsProgramARB", {skip, []}},
     {"glGetVertexAttribPointervARB", {skip, []}},

     {"glVertexAttribPointer", {[{"pointer", pointer, "type"}], []}},
     {"glVertexAttribPointerARB", {skip, []}},
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
     {"glGetProgramiv", {[{"params", 1}], []}},
     {"glGetProgramivARB", {skip, []}},
     {"glGetProgramStringARB",  {[{"string", sdlmem}], []}},
     {"glGetVertexAttribdv", {[{"params", 4}], []}},  % {undefined, 4, "pname"} ?
     {"glGetVertexAttribfv", {[{"params", 4}], []}},  % {undefined, 4, "pname"} ?
     {"glGetVertexAttribiv", {[{"params", 4}], []}},   % {undefined, 4, "pname"} ?
     {"glGetVertexAttribdvARB", {skip, []}}, 
     {"glGetVertexAttribfvARB", {skip, []}}, 
     {"glGetVertexAttribivARB", {skip, []}}, 

     %% 1.5 and 2.0 
     {"glGenQueries", {[{"ids", "n"}], []}},
     {"glDeleteQueries", {[{"ids", "n"}], []}},
     {"glGetQueryiv", {[{"params", 1}], []}},
     {"glGetQueryObjectiv", {[{"params", 1}], []}},
     {"glGetQueryObjectuiv", {[{"params", 1}], []}},
     {"glDeleteBuffers", {[{"buffers", "n"}], []}},
     {"glGenBuffers", {[{"buffers", "n"}], []}},
     {"glBufferData", {[{"data", pointer, "GL_UNSIGNED_BYTE"}], []}},
     {"glBufferSubData", {[{"data", pointer, "GL_UNSIGNED_BYTE"}], []}},
     {"glGetBufferSubData", {[{"data", sdlmem}], []}},
     {"glGetBufferParameteriv", {[{"params", 1}], []}},
     {"glShaderSource", {[{"length", "count"}, {"string", "count"}], []}},
     {"glUniform1fv", {[{"value", "count"}], []}},
     {"glUniform2fv", {[{"value", "count*2"}], []}},
     {"glUniform3fv", {[{"value", "count*3"}], []}},
     {"glUniform4fv", {[{"value", "count*4"}], []}},
     {"glUniform1iv", {[{"value", "count"}], []}},
     {"glUniform2iv", {[{"value", "count*2"}], []}},
     {"glUniform3iv", {[{"value", "count*3"}], []}},
     {"glUniform4iv", {[{"value", "count*4"}], []}},
     {"glUniformMatrix2fvARB", {[{"value", "count*4"}], []}},
     {"glUniformMatrix3fvARB", {[{"value", "count*9"}], []}},
     {"glUniformMatrix4fvARB", {[{"value", "count*16"}], []}},
     {"glGetObjectParameterfvARB", {[{"params", 1}], []}},
     {"glGetObjectParameterivARB", {[{"params", 1}], []}},
     {"glGetInfoLog", {[{"length", 1}, {"infoLog", {"length", "maxLength"}}], []}},
     {"glGetProgramInfoLog", {[{"length", 1}, {"infoLog", {"length", "bufSize"}}], []}},
     {"glGetShaderInfoLog", {[{"length", 1}, {"infoLog", {"length", "bufSize"}}], []}},
     {"glGetAttachedObjectsARB", {[{"count", 1}, {"obj", {"count", "maxCount"}}], []}},
     {"glGetUniformLocation", {[{"name", pointer, string}], []}},
     {"glGetActiveUniform", {[{"length", 1}, {"size", 1}, {"type", 1}, {"name", {"length", "bufSize"}}], []}},
     {"glGetUniformfv", {[{"params", sdlmem}], []}},
     {"glGetUniformiv", {[{"params", sdlmem}], []}},
     {"glGetShaderSource", {[{"length", 1},{"source", {"length", "bufSize"}}], []}},
     {"glBindAttribLocation", {[{"name", pointer, string}], []}},
     {"glGetActiveAttrib", {[{"length", 1},{"size", 1},{"type", 1},{"name", {"length", "bufSize"}}], []}}, 
     {"glGetAttribLocation", {[{"name", pointer, string}], []}},
     {"glDrawBuffers", {[{"bufs", "n"}], []}},
     {"glGetAttachedShaders", {[{"count", 1},{"obj", {"count","maxCount"}}], []}},
     {"glGetShaderiv", {[{"params", 1}], []}},
     %% GL_EXT_framebuffer_object
     {"glDeleteRenderbuffersEXT", {[{"renderbuffers", "n"}], []}},
     {"glGenRenderbuffersEXT", {[{"renderbuffers", "n"}], []}},
     {"glGetRenderbufferParameterivEXT", {[{"params", 1}], []}},
     {"glDeleteFramebuffersEXT", {[{"framebuffers", "n"}], []}},
     {"glGenFramebuffersEXT", {[{"framebuffers", "n"}], []}},
     {"glGetFramebufferAttachmentParameterivEXT", {[{"params", 1}], []}}
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
init_hrl(_Fd) -> 
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

