%%%----------------------------------------------------------------------
%%% File    : conv.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created : 11 Oct 2001 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

%% Some knowledge about lengths of arrays and similar stuff.

-define(array_lengths, 
	[
	 {"glAreTexturesResident",{[{"residences","n"},{"textures","n"}],[]}},
	 {"glBitmap", {[{"bitmap", pointer}], []}},
	 {"glCallLists", {[{"lists", "n", "type"}], []}},
	 {"glClipPlane", {[{"equation", 4}], []}},
	 {"glColor3bv", {[{"v", 3}], []}},
	 {"glColor3dv", {[{"v", 3}], []}},
	 {"glColor3fv", {[{"v", 3}], []}},
	 {"glColor3iv", {[{"v", 3}], []}},
	 {"glColor3sv", {[{"v", 3}], []}},
	 {"glColor3ubv", {[{"v", 3}], []}},
	 {"glColor3uiv", {[{"v", 3}], []}},
	 {"glColor3usv", {[{"v", 3}], []}},
	 {"glColor4bv", {[{"v", 4}], []}},
	 {"glColor4dv", {[{"v", 4}], []}},
	 {"glColor4fv", {[{"v", 4}], []}},
	 {"glColor4iv", {[{"v", 4}], []}},
	 {"glColor4sv", {[{"v", 4}], []}},
	 {"glColor4ubv", {[{"v", 4}], []}},
	 {"glColor4uiv", {[{"v", 4}], []}},
	 {"glColor4usv", {[{"v", 4}], []}},
	 {"glNormal3bv", {[{"v", 3}], []}},
	 {"glNormal3dv", {[{"v", 3}], []}},
	 {"glNormal3fv", {[{"v", 3}], []}},
	 {"glNormal3iv", {[{"v", 3}], []}},
	 {"glNormal3sv", {[{"v", 3}], []}},
	 {"glRasterPos3dv", {[{"v", 3}], []}},
	 {"glRasterPos3fv", {[{"v", 3}], []}},
	 {"glRasterPos3iv", {[{"v", 3}], []}},
	 {"glRasterPos3sv", {[{"v", 3}], []}},
	 {"glRasterPos4dv", {[{"v", 4}], []}},
	 {"glRasterPos4fv", {[{"v", 4}], []}},
	 {"glRasterPos4iv", {[{"v", 4}], []}},
	 {"glRasterPos4sv", {[{"v", 4}], []}},
	 {"glDeleteTextures", {[{"textures", "n"}], []}},
	 {"glDrawElements", {[{"indices", "count", "type"}], []}},
	 {"glDrawPixels", {[{"pixels", pointer, "type"}], []}},
	 {"glEdgeFlagv", {[{"flag", 1}], []}},
	 {"glEvalCoord1dv", {[{"u", 1}], []}},
	 {"glEvalCoord1fv", {[{"u", 1}], []}},
	 {"glEvalCoord2dv", {[{"u", 2}], []}},
	 {"glEvalCoord2fv", {[{"u", 2}], []}},
	 {"glFogfv", {[{"params", {undefined, 4}}], []}},
	 {"glFogiv", {[{"params", {undefined, 4}}], []}},
	 {"glGenTextures", {[{"textures", "n"}], []}},
	 {"glGetBooleanv", {[{"params", {undefined,16}}], []}},%% Variable
	 {"glGetDoublev", {[{"params",  {undefined,16}}], []}},%% Variable
	 {"glGetFloatv", {[{"params",   {undefined,16}}], []}},%% Variable
	 {"glGetIntegerv", {[{"params", {undefined,16}}], []}},%% Variable
	 {"glGetClipPlane", {[{"equation", 4}], []}},
	 {"glGetLightfv", {[{"params", {undefined, 4}}], []}}, %% Variable
	 {"glGetLightiv", {[{"params", {undefined, 4}}], []}}, %% Variable
	 {"glGetMapdv", {[{"v", sdlmem}], []}},%% undefined Variable ?
	 {"glGetMapfv", {[{"v", sdlmem}], []}},%% undefined Variable ?
	 {"glGetMapiv", {[{"v", sdlmem}], []}},%% undefined Variable ?
	 {"glGetPointerv", {[{"params", sdlmem}], []}},%% undefined Variable ?
	 {"glGetMaterialfv", {[{"params", {undefined, 4}}], []}},%% Variable
	 {"glGetMaterialiv", {[{"params", {undefined, 4}}], []}},%% Variable
	 {"glGetPixelMapfv", {[{"values",  sdlmem}], []}},
	 {"glGetPixelMapuiv", {[{"values", sdlmem}], []}},
	 {"glGetPixelMapusv", {[{"values", sdlmem}], []}},
	 {"glGetPolygonStipple", {[{"mask", 128}], []}},	 
	 {"glGetTexGendv", {[{"params", {undefined, 4}}], []}},%% Variable
	 {"glGetTexGenfv", {[{"params", {undefined, 4}}], []}},%% Variable
	 {"glGetTexGeniv", {[{"params", {undefined, 4}}], []}},%% Variable
	 {"glGetTexEnvfv", {[{"params", {undefined, 4}}], []}},%% Variable Max 4
	 {"glGetTexEnviv", {[{"params", {undefined, 4}}], []}}, %% Variable Max 4
	 {"glGetTexParameterfv", {[{"params", {undefined, 4}}], []}},%% Variable Max 4
	 {"glGetTexParameteriv", {[{"params", {undefined, 4}}], []}},%% Variable Max 4
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
	 {"glLightModelfv", {[{"params", {undefined, 4}}], []}},
	 {"glLightModeliv", {[{"params", {undefined, 4}}], []}},
	 {"glLightfv", {[{"params", {undefined, 4}}], []}},
	 {"glLightiv", {[{"params", {undefined, 4}}], []}},
	 {"glLoadMatrixd", {[{"m", 16}], []}},
	 {"glLoadMatrixf", {[{"m", 16}], []}},
	 {"glMaterialfv", {[{"params", {undefined, 4}}], []}},
	 {"glMaterialiv", {[{"params", {undefined, 4}}], []}},
	 {"glMultMatrixd", {[{"m", 16}], []}},
	 {"glMultMatrixf", {[{"m", 16}], []}},
	 {"glPolygonStipple", {[{"mask", 128}], []}},
	 {"glPrioritizeTextures", {[{"priorities", "n"}, {"textures", "n"}], []}},
	 {"glRasterPos2dv", {[{"v", 2}], []}},
	 {"glRasterPos2fv", {[{"v", 2}], []}},
	 {"glRasterPos2iv", {[{"v", 2}], []}},
	 {"glRasterPos2sv", {[{"v", 2}], []}},
	 {"glRectdv", {[{"v1", 2},{"v2", 2}], []}},
	 {"glRectfv", {[{"v1", 2},{"v2", 2}], []}},
	 {"glRectiv", {[{"v1", 2},{"v2", 2}], []}},
	 {"glRectsv", {[{"v1", 2},{"v2", 2}], []}},
	 {"glTexCoord1dv", {[{"v", 1}], []}},
	 {"glTexCoord1fv", {[{"v", 1}], []}},
	 {"glTexCoord1iv", {[{"v", 1}], []}},
	 {"glTexCoord1sv", {[{"v", 1}], []}},
	 {"glTexCoord2dv", {[{"v", 2}], []}},
	 {"glTexCoord2fv", {[{"v", 2}], []}},
	 {"glTexCoord2iv", {[{"v", 2}], []}},
	 {"glTexCoord2sv", {[{"v", 2}], []}},
	 {"glTexCoord3dv", {[{"v", 3}], []}},
	 {"glTexCoord3fv", {[{"v", 3}], []}},
	 {"glTexCoord3iv", {[{"v", 3}], []}},
	 {"glTexCoord3sv", {[{"v", 3}], []}},
	 {"glTexCoord4dv", {[{"v", 4}], []}},
	 {"glTexCoord4fv", {[{"v", 4}], []}},
	 {"glTexCoord4iv", {[{"v", 4}], []}},
	 {"glTexCoord4sv", {[{"v", 4}], []}},
	 {"glTexEnvfv", {[{"params", {undefined, 4}}], []}},
	 {"glTexEnviv", {[{"params", {undefined, 4}}], []}},
	 {"glTexGendv", {[{"params", 4}], []}},
	 {"glTexGenfv", {[{"params", 4}], []}},
	 {"glTexGeniv", {[{"params", 4}], []}},
	 {"glTexImage1D", {[{"pixels", pointer, "type"}], []}},
	 {"glTexImage2D", {[{"pixels", pointer, "type"}], []}},
	 {"glTexParameterfv", {[{"params", {undefined, 4}}], []}},
	 {"glTexParameteriv", {[{"params", {undefined, 4}}], []}},
	 {"glTexSubImage1D", {[{"pixels", pointer, "type"}], []}},
	 {"glTexSubImage2D", {[{"pixels", pointer, "type"}], []}},
	 {"glVertex2dv", {[{"v", 2}], []}},
	 {"glVertex2fv", {[{"v", 2}], []}},
	 {"glVertex2iv", {[{"v", 2}], []}},
	 {"glVertex2sv", {[{"v", 2}], []}},
	 {"glVertex3dv", {[{"v", 3}], []}},
	 {"glVertex3fv", {[{"v", 3}], []}},
	 {"glVertex3iv", {[{"v", 3}], []}},
	 {"glVertex3sv", {[{"v", 3}], []}},
	 {"glVertex4dv", {[{"v", 4}], []}},
	 {"glVertex4fv", {[{"v", 4}], []}},
	 {"glVertex4iv", {[{"v", 4}], []}},
	 {"glVertex4sv", {[{"v", 4}], []}},
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
	 {"glVertexPointer", {[{"pointer", pointer, "type"}], []}},
	 
	 {"glMap1d", {[{"points", {undefined, "order * 4"}}], []}},
	 {"glMap1f", {[{"points", {undefined, "order * 4"}}], []}},
	 {"glMap2d", {[{"points", {undefined, "uorder * vorder * 4"}}], []}},
	 {"glMap2f", {[{"points", {undefined, "uorder * vorder * 4"}}], []}}
	]).


