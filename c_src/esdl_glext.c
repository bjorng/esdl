/*  
 *  Copyright (c) 2003 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id$
 *
 */

#include <stdlib.h>
#include <string.h>
#include "esdl.h"
#ifndef APIENTRY
#define APIENTRY
#endif
#include "esdl_glext.h"

void egl_blendColor(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLclampf * red;
 GLclampf * green;
 GLclampf * blue;
 GLclampf * alpha;
 bp = egl_buff;
 red = (GLclampf *) bp; bp += sizeof(GLclampf); 
 green = (GLclampf *) bp; bp += sizeof(GLclampf); 
 blue = (GLclampf *) bp; bp += sizeof(GLclampf); 
 alpha = (GLclampf *) bp; 
 esdl_glBlendColor(*red, *green, *blue, *alpha);
}


void egl_blendEquation(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 bp = egl_buff;
 mode = (GLenum *) bp; 
 esdl_glBlendEquation(*mode);
}


void egl_drawRangeElements(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 GLuint * start;
 GLuint * end;
 GLsizei * count;
 GLenum * type;
 GLvoid * indices = NULL; 
 bp = egl_buff;
 mode = (GLenum *) bp; bp += sizeof(GLenum); 
 start = (GLuint *) bp; bp += sizeof(GLuint); 
 end = (GLuint *) bp; bp += sizeof(GLuint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 indices = (GLvoid *) bp;
 esdl_glDrawRangeElements(*mode, *start, *end, *count, *type, indices);
}


void egl_colorTable(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * internalformat;
 GLsizei * width;
 GLenum * format;
 GLenum * type;
 GLvoid * table = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 table = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glColorTable(*target, *internalformat, *width, *format, *type, table);
 sdl_free_binaries(egl_sd);
}


void egl_colorTableParameterfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * pname;
 GLfloat * params;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 params = (GLfloat *) bp;
 esdl_glColorTableParameterfv(*target, *pname, params);
}


void egl_colorTableParameteriv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * pname;
 GLint * params;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 params = (GLint *) bp;
 esdl_glColorTableParameteriv(*target, *pname, params);
}


void egl_copyColorTable(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * internalformat;
 GLint * x;
 GLint * y;
 GLsizei * width;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; 
 esdl_glCopyColorTable(*target, *internalformat, *x, *y, *width);
}


void egl_getColorTable(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * format;
 GLenum * type;
 GLvoid * table = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 table = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glGetColorTable(*target, *format, *type, table);
 sdl_free_binaries(egl_sd);
}


void egl_getColorTableParameterfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLenum * pname;
 GLfloat params[4]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetColorTableParameterfv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *4);
 memcpy(bp, params, sizeof(GLfloat)*4);
 bp += sizeof(GLfloat)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getColorTableParameteriv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLenum * pname;
 GLint params[4]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetColorTableParameteriv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *4);
 memcpy(bp, params, sizeof(GLint)*4);
 bp += sizeof(GLint)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_colorSubTable(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLsizei * start;
 GLsizei * count;
 GLenum * format;
 GLenum * type;
 GLvoid * data = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 start = (GLsizei *) bp; bp += sizeof(GLsizei); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 data = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glColorSubTable(*target, *start, *count, *format, *type, data);
 sdl_free_binaries(egl_sd);
}


void egl_copyColorSubTable(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLsizei * start;
 GLint * x;
 GLint * y;
 GLsizei * width;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 start = (GLsizei *) bp; bp += sizeof(GLsizei); 
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; 
 esdl_glCopyColorSubTable(*target, *start, *x, *y, *width);
}


void egl_convolutionFilter1D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * internalformat;
 GLsizei * width;
 GLenum * format;
 GLenum * type;
 GLvoid * image = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 image = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glConvolutionFilter1D(*target, *internalformat, *width, *format, *type, image);
 sdl_free_binaries(egl_sd);
}


void egl_convolutionFilter2D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * internalformat;
 GLsizei * width;
 GLsizei * height;
 GLenum * format;
 GLenum * type;
 GLvoid * image = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 image = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glConvolutionFilter2D(*target, *internalformat, *width, *height, *format, *type, image);
 sdl_free_binaries(egl_sd);
}


void egl_convolutionParameterf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * pname;
 GLfloat * params;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 params = (GLfloat *) bp; 
 esdl_glConvolutionParameterf(*target, *pname, *params);
}


void egl_convolutionParameterfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * pname;
 GLfloat * params;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 bp += sizeof(int); 
 params = (GLfloat *) bp; 
 esdl_glConvolutionParameterfv(*target, *pname, params);
}


void egl_convolutionParameteri(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * pname;
 GLint * params;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 params = (GLint *) bp; 
 esdl_glConvolutionParameteri(*target, *pname, *params);
}


void egl_convolutionParameteriv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * pname;
 GLint * params;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 bp += sizeof(int); 
 params = (GLint *) bp; 
 esdl_glConvolutionParameteriv(*target, *pname, params);
}


void egl_copyConvolutionFilter1D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * internalformat;
 GLint * x;
 GLint * y;
 GLsizei * width;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; 
 esdl_glCopyConvolutionFilter1D(*target, *internalformat, *x, *y, *width);
}


void egl_copyConvolutionFilter2D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * internalformat;
 GLint * x;
 GLint * y;
 GLsizei * width;
 GLsizei * height;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; 
 esdl_glCopyConvolutionFilter2D(*target, *internalformat, *x, *y, *width, *height);
}


void egl_getConvolutionFilter(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * format;
 GLenum * type;
 GLvoid * image = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 image = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glGetConvolutionFilter(*target, *format, *type, image);
 sdl_free_binaries(egl_sd);
}


void egl_getConvolutionParameterfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLenum * pname;
 GLfloat params[4];
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetConvolutionParameterfv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *4);
 memcpy(bp, params, sizeof(GLfloat)*4);
 bp += sizeof(GLfloat)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getConvolutionParameteriv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLenum * pname;
 GLint params[4];
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetConvolutionParameteriv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *4);
 memcpy(bp, params, sizeof(GLint)*4);
 bp += sizeof(GLint)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getSeparableFilter(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * format;
 GLenum * type;
 GLvoid * row = NULL;
 GLvoid * column = NULL;
 GLvoid * span = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 row = (GLvoid *) egl_sd->bin[0].base; 
 column = (GLvoid *) egl_sd->bin[1].base; 
 span = (GLvoid *) egl_sd->bin[2].base; 
 esdl_glGetSeparableFilter(*target, *format, *type, row, column, span);
 sdl_free_binaries(egl_sd);
}


void egl_separableFilter2D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * internalformat;
 GLsizei * width;
 GLsizei * height;
 GLenum * format;
 GLenum * type;
 GLvoid * row = NULL;
 GLvoid * column = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 row = (GLvoid *) egl_sd->bin[0].base; 
 column = (GLvoid *) egl_sd->bin[1].base; 
 esdl_glSeparableFilter2D(*target, *internalformat, *width, *height, *format, *type, row, column);
 sdl_free_binaries(egl_sd);
}


void egl_getHistogram(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLboolean * reset;
 GLenum * format;
 GLenum * type;
 GLvoid * values = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 reset = (GLboolean *) bp; bp += sizeof(GLboolean); 
 bp += 3;
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 values = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glGetHistogram(*target, *reset, *format, *type, values);
 sdl_free_binaries(egl_sd);
}


void egl_getHistogramParameterfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLenum * pname;
 GLfloat params[1]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetHistogramParameterfv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *1);
 * (GLfloat *)bp = params[0]; bp += sizeof(GLfloat);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getHistogramParameteriv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLenum * pname;
 GLint params[1]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetHistogramParameteriv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *1);
 * (GLint *)bp = params[0]; bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getMinmax(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLboolean * reset;
 GLenum * format;
 GLenum * type;
 GLvoid * values = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 reset = (GLboolean *) bp; bp += sizeof(GLboolean); 
 bp += 3;
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 values = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glGetMinmax(*target, *reset, *format, *type, values);
 sdl_free_binaries(egl_sd);
}


void egl_getMinmaxParameterfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLenum * pname;
 GLfloat params[1]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetMinmaxParameterfv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *1);
 * (GLfloat *)bp = params[0]; bp += sizeof(GLfloat);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getMinmaxParameteriv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLenum * pname;
 GLint params[1]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetMinmaxParameteriv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *1);
 * (GLint *)bp = params[0]; bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_histogram(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLsizei * width;
 GLenum * internalformat;
 GLboolean * sink;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 sink = (GLboolean *) bp; 
 esdl_glHistogram(*target, *width, *internalformat, *sink);
}


void egl_minmax(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * internalformat;
 GLboolean * sink;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 sink = (GLboolean *) bp; 
 esdl_glMinmax(*target, *internalformat, *sink);
}


void egl_resetHistogram(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 bp = egl_buff;
 target = (GLenum *) bp; 
 esdl_glResetHistogram(*target);
}


void egl_resetMinmax(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 bp = egl_buff;
 target = (GLenum *) bp; 
 esdl_glResetMinmax(*target);
}


void egl_texImage3D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * internalformat;
 GLsizei * width;
 GLsizei * height;
 GLsizei * depth;
 GLint * border;
 GLenum * format;
 GLenum * type;
 GLvoid * pixels = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 internalformat = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 depth = (GLsizei *) bp; bp += sizeof(GLsizei); 
 border = (GLint *) bp; bp += sizeof(GLint); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 pixels = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glTexImage3D(*target, *level, *internalformat, *width, *height, *depth, *border, *format, *type, pixels);
 sdl_free_binaries(egl_sd);
}


void egl_texSubImage3D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * xoffset;
 GLint * yoffset;
 GLint * zoffset;
 GLsizei * width;
 GLsizei * height;
 GLsizei * depth;
 GLenum * format;
 GLenum * type;
 GLvoid * pixels = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 xoffset = (GLint *) bp; bp += sizeof(GLint); 
 yoffset = (GLint *) bp; bp += sizeof(GLint); 
 zoffset = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 depth = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 pixels = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glTexSubImage3D(*target, *level, *xoffset, *yoffset, *zoffset, *width, *height, *depth, *format, *type, pixels);
 sdl_free_binaries(egl_sd);
}


void egl_copyTexSubImage3D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * xoffset;
 GLint * yoffset;
 GLint * zoffset;
 GLint * x;
 GLint * y;
 GLsizei * width;
 GLsizei * height;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 xoffset = (GLint *) bp; bp += sizeof(GLint); 
 yoffset = (GLint *) bp; bp += sizeof(GLint); 
 zoffset = (GLint *) bp; bp += sizeof(GLint); 
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; 
 esdl_glCopyTexSubImage3D(*target, *level, *xoffset, *yoffset, *zoffset, *x, *y, *width, *height);
}


void egl_activeTexture(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * texture;
 bp = egl_buff;
 texture = (GLenum *) bp; 
 esdl_glActiveTexture(*texture);
}


void egl_clientActiveTexture(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * texture;
 bp = egl_buff;
 texture = (GLenum *) bp; 
 esdl_glClientActiveTexture(*texture);
}


void egl_multiTexCoord1dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLdouble v[1];
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 memcpy(v,bp,sizeof(GLdouble)*1); 
 esdl_glMultiTexCoord1dv(*target, v);
}


void egl_multiTexCoord1fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLfloat * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLfloat *) bp;
 esdl_glMultiTexCoord1fv(*target, v);
}


void egl_multiTexCoord1iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLint *) bp;
 esdl_glMultiTexCoord1iv(*target, v);
}


void egl_multiTexCoord1sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLshort * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLshort *) bp;
 esdl_glMultiTexCoord1sv(*target, v);
}


void egl_multiTexCoord2dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLdouble v[2];
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 memcpy(v,bp,sizeof(GLdouble)*2); 
 esdl_glMultiTexCoord2dv(*target, v);
}


void egl_multiTexCoord2fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLfloat * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLfloat *) bp;
 esdl_glMultiTexCoord2fv(*target, v);
}


void egl_multiTexCoord2iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLint *) bp;
 esdl_glMultiTexCoord2iv(*target, v);
}


void egl_multiTexCoord2sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLshort * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLshort *) bp;
 esdl_glMultiTexCoord2sv(*target, v);
}


void egl_multiTexCoord3dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLdouble v[3];
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 memcpy(v,bp,sizeof(GLdouble)*3); 
 esdl_glMultiTexCoord3dv(*target, v);
}


void egl_multiTexCoord3fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLfloat * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLfloat *) bp;
 esdl_glMultiTexCoord3fv(*target, v);
}


void egl_multiTexCoord3iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLint *) bp;
 esdl_glMultiTexCoord3iv(*target, v);
}


void egl_multiTexCoord3sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLshort * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLshort *) bp;
 esdl_glMultiTexCoord3sv(*target, v);
}


void egl_multiTexCoord4dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLdouble v[4];
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 memcpy(v,bp,sizeof(GLdouble)*4); 
 esdl_glMultiTexCoord4dv(*target, v);
}


void egl_multiTexCoord4fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLfloat * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLfloat *) bp;
 esdl_glMultiTexCoord4fv(*target, v);
}


void egl_multiTexCoord4iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLint *) bp;
 esdl_glMultiTexCoord4iv(*target, v);
}


void egl_multiTexCoord4sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLshort * v;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLshort *) bp;
 esdl_glMultiTexCoord4sv(*target, v);
}


void egl_loadTransposeMatrixf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * m;
 bp = egl_buff;
 m = (GLfloat *) bp;
 esdl_glLoadTransposeMatrixf(m);
}


void egl_loadTransposeMatrixd(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble m[16];
 bp = egl_buff;
 memcpy(m,bp,sizeof(GLdouble)*16); 
 esdl_glLoadTransposeMatrixd(m);
}


void egl_multTransposeMatrixf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * m;
 bp = egl_buff;
 m = (GLfloat *) bp;
 esdl_glMultTransposeMatrixf(m);
}


void egl_multTransposeMatrixd(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble m[16];
 bp = egl_buff;
 memcpy(m,bp,sizeof(GLdouble)*16); 
 esdl_glMultTransposeMatrixd(m);
}


void egl_sampleCoverage(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLclampf * value;
 GLboolean * invert;
 bp = egl_buff;
 value = (GLclampf *) bp; bp += sizeof(GLclampf); 
 invert = (GLboolean *) bp; 
 esdl_glSampleCoverage(*value, *invert);
}


void egl_compressedTexImage3D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLenum * internalformat;
 GLsizei * width;
 GLsizei * height;
 GLsizei * depth;
 GLint * border;
 GLsizei * imageSize;
 GLvoid * data = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 depth = (GLsizei *) bp; bp += sizeof(GLsizei); 
 border = (GLint *) bp; bp += sizeof(GLint); 
 imageSize = (GLsizei *) bp; bp += sizeof(GLsizei); 
 data = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glCompressedTexImage3D(*target, *level, *internalformat, *width, *height, *depth, *border, *imageSize, data);
 sdl_free_binaries(egl_sd);
}


void egl_compressedTexImage2D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLenum * internalformat;
 GLsizei * width;
 GLsizei * height;
 GLint * border;
 GLsizei * imageSize;
 GLvoid * data = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 border = (GLint *) bp; bp += sizeof(GLint); 
 imageSize = (GLsizei *) bp; bp += sizeof(GLsizei); 
 data = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glCompressedTexImage2D(*target, *level, *internalformat, *width, *height, *border, *imageSize, data);
 sdl_free_binaries(egl_sd);
}


void egl_compressedTexImage1D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLenum * internalformat;
 GLsizei * width;
 GLint * border;
 GLsizei * imageSize;
 GLvoid * data = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 border = (GLint *) bp; bp += sizeof(GLint); 
 imageSize = (GLsizei *) bp; bp += sizeof(GLsizei); 
 data = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glCompressedTexImage1D(*target, *level, *internalformat, *width, *border, *imageSize, data);
 sdl_free_binaries(egl_sd);
}


void egl_compressedTexSubImage3D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * xoffset;
 GLint * yoffset;
 GLint * zoffset;
 GLsizei * width;
 GLsizei * height;
 GLsizei * depth;
 GLenum * format;
 GLsizei * imageSize;
 GLvoid * data = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 xoffset = (GLint *) bp; bp += sizeof(GLint); 
 yoffset = (GLint *) bp; bp += sizeof(GLint); 
 zoffset = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 depth = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 imageSize = (GLsizei *) bp; bp += sizeof(GLsizei); 
 data = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glCompressedTexSubImage3D(*target, *level, *xoffset, *yoffset, *zoffset, *width, *height, *depth, *format, *imageSize, data);
 sdl_free_binaries(egl_sd);
}


void egl_compressedTexSubImage2D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * xoffset;
 GLint * yoffset;
 GLsizei * width;
 GLsizei * height;
 GLenum * format;
 GLsizei * imageSize;
 GLvoid * data = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 xoffset = (GLint *) bp; bp += sizeof(GLint); 
 yoffset = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 imageSize = (GLsizei *) bp; bp += sizeof(GLsizei); 
 data = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glCompressedTexSubImage2D(*target, *level, *xoffset, *yoffset, *width, *height, *format, *imageSize, data);
 sdl_free_binaries(egl_sd);
}


void egl_compressedTexSubImage1D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * xoffset;
 GLsizei * width;
 GLenum * format;
 GLsizei * imageSize;
 GLvoid * data = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 xoffset = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 imageSize = (GLsizei *) bp; bp += sizeof(GLsizei); 
 data = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glCompressedTexSubImage1D(*target, *level, *xoffset, *width, *format, *imageSize, data);
 sdl_free_binaries(egl_sd);
}


void egl_getCompressedTexImage(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLvoid * img = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 img = (void *) egl_sd->bin[0].base; 
 esdl_glGetCompressedTexImage(*target, *level, img);
 sdl_free_binaries(egl_sd);
}


void egl_blendFuncSeparate(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * sfactorRGB;
 GLenum * dfactorRGB;
 GLenum * sfactorAlpha;
 GLenum * dfactorAlpha;
 bp = egl_buff;
 sfactorRGB = (GLenum *) bp; bp += sizeof(GLenum); 
 dfactorRGB = (GLenum *) bp; bp += sizeof(GLenum); 
 sfactorAlpha = (GLenum *) bp; bp += sizeof(GLenum); 
 dfactorAlpha = (GLenum *) bp; 
 esdl_glBlendFuncSeparate(*sfactorRGB, *dfactorRGB, *sfactorAlpha, *dfactorAlpha);
}


void egl_fogCoordf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * coord;
 bp = egl_buff;
 coord = (GLfloat *) bp; 
 esdl_glFogCoordf(*coord);
}


void egl_fogCoordfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * coord;
 bp = egl_buff;
 coord = (GLfloat *) bp;
 esdl_glFogCoordfv(coord);
}


void egl_fogCoordd(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble coord;
 bp = egl_buff;
 coord = * (GLdouble *) bp; 
 esdl_glFogCoordd(coord);
}


void egl_fogCoorddv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble coord[1];
 bp = egl_buff;
 memcpy(coord,bp,sizeof(GLdouble)*1); 
 esdl_glFogCoorddv(coord);
}


void egl_fogCoordPointer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * type;
 GLsizei * stride;
 GLvoid * pointer = NULL;
 bp = egl_buff;
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 stride = (GLsizei *) bp; bp += sizeof(GLsizei); 
 pointer = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glFogCoordPointer(*type, *stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_multiDrawArrays(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 GLint * first = NULL; 
 GLsizei * count = NULL; 
 GLsizei * primcount;
 bp = egl_buff;
 primcount = (GLsizei *) bp; bp += sizeof(GLsizei); 
 mode = (GLenum *) bp; bp += sizeof(GLenum); 
 first = (GLint *) bp;
 bp += sizeof(GLint)*(*primcount); 
 count = (GLsizei *) bp;
 esdl_glMultiDrawArrays(*mode, first, count, *primcount);
}


void egl_pointParameterf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLfloat * param;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLfloat *) bp; 
 esdl_glPointParameterf(*pname, *param);
}


void egl_pointParameterfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLfloat * params;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 params = (GLfloat *) bp;
 esdl_glPointParameterfv(*pname, params);
}


void egl_pointParameteri(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLint * param;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLint *) bp; 
 esdl_glPointParameteri(*pname, *param);
}


void egl_pointParameteriv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLint * params;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 params = (GLint *) bp;
 esdl_glPointParameteriv(*pname, params);
}


void egl_secondaryColor3bv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLbyte * v;
 bp = egl_buff;
 v = (GLbyte *) bp;
 esdl_glSecondaryColor3bv(v);
}


void egl_secondaryColor3dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[3];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*3); 
 esdl_glSecondaryColor3dv(v);
}


void egl_secondaryColor3fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 esdl_glSecondaryColor3fv(v);
}


void egl_secondaryColor3iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 esdl_glSecondaryColor3iv(v);
}


void egl_secondaryColor3sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 esdl_glSecondaryColor3sv(v);
}


void egl_secondaryColor3ubv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLubyte * v;
 bp = egl_buff;
 v = (GLubyte *) bp;
 esdl_glSecondaryColor3ubv(v);
}


void egl_secondaryColor3uiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * v;
 bp = egl_buff;
 v = (GLuint *) bp;
 esdl_glSecondaryColor3uiv(v);
}


void egl_secondaryColor3usv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLushort * v;
 bp = egl_buff;
 v = (GLushort *) bp;
 esdl_glSecondaryColor3usv(v);
}


void egl_secondaryColorPointer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLenum * type;
 GLsizei * stride;
 GLvoid * pointer = NULL;
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 stride = (GLsizei *) bp; bp += sizeof(GLsizei); 
 pointer = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glSecondaryColorPointer(*size, *type, *stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_windowPos2dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[2];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*2); 
 esdl_glWindowPos2dv(v);
}


void egl_windowPos2fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 esdl_glWindowPos2fv(v);
}


void egl_windowPos2iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 esdl_glWindowPos2iv(v);
}


void egl_windowPos2sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 esdl_glWindowPos2sv(v);
}


void egl_windowPos3dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[3];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*3); 
 esdl_glWindowPos3dv(v);
}


void egl_windowPos3fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 esdl_glWindowPos3fv(v);
}


void egl_windowPos3iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 esdl_glWindowPos3iv(v);
}


void egl_windowPos3sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 esdl_glWindowPos3sv(v);
}


void egl_weightbvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLbyte * weights = NULL; 
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 weights = (GLbyte *) bp;
 esdl_glWeightbvARB(*size, weights);
}


void egl_weightsvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLshort * weights = NULL; 
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 weights = (GLshort *) bp;
 esdl_glWeightsvARB(*size, weights);
}


void egl_weightivARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLint * weights = NULL; 
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 weights = (GLint *) bp;
 esdl_glWeightivARB(*size, weights);
}


void egl_weightfvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLfloat * weights = NULL; 
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 weights = (GLfloat *) bp;
 esdl_glWeightfvARB(*size, weights);
}


void egl_weightdvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLdouble * weights = NULL; 
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 weights = (GLdouble*) malloc(sizeof(GLdouble)*(*size));
 memcpy(weights,bp,sizeof(GLdouble)*(*size));
 esdl_glWeightdvARB(*size, weights);
 free(weights);
}


void egl_weightubvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLubyte * weights = NULL; 
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 weights = (GLubyte *) bp;
 esdl_glWeightubvARB(*size, weights);
}


void egl_weightusvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLushort * weights = NULL; 
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 weights = (GLushort *) bp;
 esdl_glWeightusvARB(*size, weights);
}


void egl_weightuivARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLuint * weights = NULL; 
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 weights = (GLuint *) bp;
 esdl_glWeightuivARB(*size, weights);
}


void egl_weightPointerARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLenum * type;
 GLsizei * stride;
 GLvoid * pointer = NULL;
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 stride = (GLsizei *) bp; bp += sizeof(GLsizei); 
 pointer = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glWeightPointerARB(*size, *type, *stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_vertexBlendARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * count;
 bp = egl_buff;
 count = (GLint *) bp; 
 esdl_glVertexBlendARB(*count);
}


void egl_currentPaletteMatrixARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * index;
 bp = egl_buff;
 index = (GLint *) bp; 
 esdl_glCurrentPaletteMatrixARB(*index);
}


void egl_matrixIndexubvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLubyte * indices = NULL; 
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 indices = (GLubyte *) bp;
 esdl_glMatrixIndexubvARB(*size, indices);
}


void egl_matrixIndexusvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLushort * indices = NULL; 
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 indices = (GLushort *) bp;
 esdl_glMatrixIndexusvARB(*size, indices);
}


void egl_matrixIndexuivARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLuint * indices = NULL; 
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 indices = (GLuint *) bp;
 esdl_glMatrixIndexuivARB(*size, indices);
}


void egl_matrixIndexPointerARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * size;
 GLenum * type;
 GLsizei * stride;
 GLvoid * pointer = NULL;
 bp = egl_buff;
 size = (GLint *) bp; bp += sizeof(GLint); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 stride = (GLsizei *) bp; bp += sizeof(GLsizei); 
 pointer = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glMatrixIndexPointerARB(*size, *type, *stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_vertexAttrib1dvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLdouble v[1];
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 memcpy(v,bp,sizeof(GLdouble)*1); 
 esdl_glVertexAttrib1dvARB(*index, v);
}


void egl_vertexAttrib1fvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLfloat * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLfloat *) bp;
 esdl_glVertexAttrib1fvARB(*index, v);
}


void egl_vertexAttrib1svARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLshort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLshort *) bp;
 esdl_glVertexAttrib1svARB(*index, v);
}


void egl_vertexAttrib2dvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLdouble v[2];
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 memcpy(v,bp,sizeof(GLdouble)*2); 
 esdl_glVertexAttrib2dvARB(*index, v);
}


void egl_vertexAttrib2fvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLfloat * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLfloat *) bp;
 esdl_glVertexAttrib2fvARB(*index, v);
}


void egl_vertexAttrib2svARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLshort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLshort *) bp;
 esdl_glVertexAttrib2svARB(*index, v);
}


void egl_vertexAttrib3dvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLdouble v[3];
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 memcpy(v,bp,sizeof(GLdouble)*3); 
 esdl_glVertexAttrib3dvARB(*index, v);
}


void egl_vertexAttrib3fvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLfloat * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLfloat *) bp;
 esdl_glVertexAttrib3fvARB(*index, v);
}


void egl_vertexAttrib3svARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLshort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLshort *) bp;
 esdl_glVertexAttrib3svARB(*index, v);
}


void egl_vertexAttrib4NbvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLbyte * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLbyte *) bp;
 esdl_glVertexAttrib4NbvARB(*index, v);
}


void egl_vertexAttrib4NivARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLint * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLint *) bp;
 esdl_glVertexAttrib4NivARB(*index, v);
}


void egl_vertexAttrib4NsvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLshort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLshort *) bp;
 esdl_glVertexAttrib4NsvARB(*index, v);
}


void egl_vertexAttrib4NubARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLubyte * x;
 GLubyte * y;
 GLubyte * z;
 GLubyte * w;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 x = (GLubyte *) bp; bp += sizeof(GLubyte); 
 y = (GLubyte *) bp; bp += sizeof(GLubyte); 
 z = (GLubyte *) bp; bp += sizeof(GLubyte); 
 w = (GLubyte *) bp; 
 esdl_glVertexAttrib4NubARB(*index, *x, *y, *z, *w);
}


void egl_vertexAttrib4NubvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLubyte * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLubyte *) bp;
 esdl_glVertexAttrib4NubvARB(*index, v);
}


void egl_vertexAttrib4NuivARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLuint * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLuint *) bp;
 esdl_glVertexAttrib4NuivARB(*index, v);
}


void egl_vertexAttrib4NusvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLushort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLushort *) bp;
 esdl_glVertexAttrib4NusvARB(*index, v);
}


void egl_vertexAttrib4bvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLbyte * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLbyte *) bp;
 esdl_glVertexAttrib4bvARB(*index, v);
}


void egl_vertexAttrib4dvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLdouble v[4];
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 memcpy(v,bp,sizeof(GLdouble)*4); 
 esdl_glVertexAttrib4dvARB(*index, v);
}


void egl_vertexAttrib4fvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLfloat * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLfloat *) bp;
 esdl_glVertexAttrib4fvARB(*index, v);
}


void egl_vertexAttrib4ivARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLint * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLint *) bp;
 esdl_glVertexAttrib4ivARB(*index, v);
}


void egl_vertexAttrib4svARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLshort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLshort *) bp;
 esdl_glVertexAttrib4svARB(*index, v);
}


void egl_vertexAttrib4ubvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLubyte * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLubyte *) bp;
 esdl_glVertexAttrib4ubvARB(*index, v);
}


void egl_vertexAttrib4uivARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLuint * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLuint *) bp;
 esdl_glVertexAttrib4uivARB(*index, v);
}


void egl_vertexAttrib4usvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLushort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLushort *) bp;
 esdl_glVertexAttrib4usvARB(*index, v);
}


void egl_vertexAttribPointerARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLint * size;
 GLenum * type;
 GLboolean * normalized;
 GLsizei * stride;
 GLvoid * pointer = NULL;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 size = (GLint *) bp; bp += sizeof(GLint); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 normalized = (GLboolean *) bp; bp += sizeof(GLboolean); 
 bp += 3;
 stride = (GLsizei *) bp; bp += sizeof(GLsizei); 
 pointer = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glVertexAttribPointerARB(*index, *size, *type, *normalized, *stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_enableVertexAttribArrayARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 bp = egl_buff;
 index = (GLuint *) bp; 
 esdl_glEnableVertexAttribArrayARB(*index);
}


void egl_disableVertexAttribArrayARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 bp = egl_buff;
 index = (GLuint *) bp; 
 esdl_glDisableVertexAttribArrayARB(*index);
}


void egl_programStringARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * format;
 GLsizei * len;
 GLvoid * string = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 len = (GLsizei *) bp; bp += sizeof(GLsizei); 
 string = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glProgramStringARB(*target, *format, *len, string);
 sdl_free_binaries(egl_sd);
}


void egl_bindProgramARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLuint * program;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 program = (GLuint *) bp; 
 esdl_glBindProgramARB(*target, *program);
}


void egl_deleteProgramsARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * n;
 GLuint * programs = NULL; 
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 programs = (GLuint *) bp;
 esdl_glDeleteProgramsARB(*n, programs);
}


void egl_genProgramsARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLsizei * n;
 GLuint * programs = NULL;
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 programs = (GLuint*) malloc(sizeof(GLuint)*(*n));
 esdl_glGenProgramsARB(*n, programs);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLuint) * (*n));
 memcpy(bp, programs, sizeof(GLuint)*(*n));
 bp += sizeof(GLuint)*(*n);
 free(programs);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_programEnvParameter4dvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLuint * index;
 GLdouble params[4];
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 memcpy(params,bp,sizeof(GLdouble)*4); 
 esdl_glProgramEnvParameter4dvARB(*target, *index, params);
}


void egl_programEnvParameter4fvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLuint * index;
 GLfloat * params;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 params = (GLfloat *) bp;
 esdl_glProgramEnvParameter4fvARB(*target, *index, params);
}


void egl_programLocalParameter4dvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLuint * index;
 GLdouble params[4];
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 memcpy(params,bp,sizeof(GLdouble)*4); 
 esdl_glProgramLocalParameter4dvARB(*target, *index, params);
}


void egl_programLocalParameter4fvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLuint * index;
 GLfloat * params;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 params = (GLfloat *) bp;
 esdl_glProgramLocalParameter4fvARB(*target, *index, params);
}


void egl_getProgramEnvParameterdvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLuint * index;
 GLdouble params[4]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 esdl_glGetProgramEnvParameterdvARB(*target, *index, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLdouble) *4);
 memcpy(bp, params, sizeof(GLdouble)*4);
 bp += sizeof(GLdouble)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getProgramEnvParameterfvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLuint * index;
 GLfloat params[4]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 esdl_glGetProgramEnvParameterfvARB(*target, *index, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *4);
 memcpy(bp, params, sizeof(GLfloat)*4);
 bp += sizeof(GLfloat)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getProgramLocalParameterdvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLuint * index;
 GLdouble params[4]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 esdl_glGetProgramLocalParameterdvARB(*target, *index, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLdouble) *4);
 memcpy(bp, params, sizeof(GLdouble)*4);
 bp += sizeof(GLdouble)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getProgramLocalParameterfvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLuint * index;
 GLfloat params[4]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 esdl_glGetProgramLocalParameterfvARB(*target, *index, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *4);
 memcpy(bp, params, sizeof(GLfloat)*4);
 bp += sizeof(GLfloat)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getProgramivARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLenum * pname;
 GLint params[1]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetProgramivARB(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *1);
 * (GLint *)bp = params[0]; bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getProgramStringARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * pname;
 GLvoid * string = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 string = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glGetProgramStringARB(*target, *pname, string);
 sdl_free_binaries(egl_sd);
}


void egl_getVertexAttribdvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * index;
 GLenum * pname;
 GLdouble params[4]; 
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetVertexAttribdvARB(*index, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLdouble) *4);
 memcpy(bp, params, sizeof(GLdouble)*4);
 bp += sizeof(GLdouble)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getVertexAttribfvARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * index;
 GLenum * pname;
 GLfloat params[4]; 
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetVertexAttribfvARB(*index, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *4);
 memcpy(bp, params, sizeof(GLfloat)*4);
 bp += sizeof(GLfloat)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getVertexAttribivARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * index;
 GLenum * pname;
 GLint params[4]; 
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetVertexAttribivARB(*index, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *4);
 memcpy(bp, params, sizeof(GLint)*4);
 bp += sizeof(GLint)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getVertexAttribPointervARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * index;
 GLenum * pname;
 GLvoid *pointer = NULL;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetVertexAttribPointervARB(*index, *pname, &pointer);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLvoid*));
 putPointer(bp, pointer);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_isProgramARB(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLuint * program;
 bp = egl_buff;
 program = (GLuint *) bp; 
 egl_res =  esdl_glIsProgramARB(*program);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) );
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


