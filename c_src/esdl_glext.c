/*  
 *  Copyright (c) 2005 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id$
 *
 */

#include <stdlib.h>
#include <string.h>
#ifdef FLAVOUR_WOGGLE
#include <woggle_driver.h>
#else
#include "esdl.h"
#endif
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
 if(egl_sd->next_bin == 0) {
  table = (GLvoid *) *(GLint *)bp;
 } else {
  table = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  data = (GLvoid *) *(GLint *)bp;
 } else {
  data = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  image = (GLvoid *) *(GLint *)bp;
 } else {
  image = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  image = (GLvoid *) *(GLint *)bp;
 } else {
  image = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  row = (GLvoid *) *(GLint *)bp;
 } else {
  row = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 if(egl_sd->next_bin == 1) {
  column = (GLvoid *) *(GLint *)bp;
 } else {
  column = (GLvoid *) egl_sd->bin[1].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  pixels = (GLvoid *) *(GLint *)bp;
 } else {
  pixels = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  pixels = (GLvoid *) *(GLint *)bp;
 } else {
  pixels = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  data = (GLvoid *) *(GLint *)bp;
 } else {
  data = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  data = (GLvoid *) *(GLint *)bp;
 } else {
  data = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  data = (GLvoid *) *(GLint *)bp;
 } else {
  data = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  data = (GLvoid *) *(GLint *)bp;
 } else {
  data = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  data = (GLvoid *) *(GLint *)bp;
 } else {
  data = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  data = (GLvoid *) *(GLint *)bp;
 } else {
  data = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 img = (GLvoid *) egl_sd->bin[0].base; 
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
 memcpy(&coord, bp, sizeof(GLdouble)); 
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
 if(egl_sd->next_bin == 0) {
  pointer = (GLvoid *) *(GLint *)bp;
 } else {
  pointer = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  pointer = (GLvoid *) *(GLint *)bp;
 } else {
  pointer = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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


void egl_genQueries(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLsizei * n;
 GLuint * ids = NULL;
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 ids = (GLuint*) malloc(sizeof(GLuint)*(*n));
 esdl_glGenQueries(*n, ids);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLuint) * (*n));
 memcpy(bp, ids, sizeof(GLuint)*(*n));
 bp += sizeof(GLuint)*(*n);
 free(ids);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_deleteQueries(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * n;
 GLuint * ids = NULL; 
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 ids = (GLuint *) bp;
 esdl_glDeleteQueries(*n, ids);
}


void egl_isQuery(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLuint * id;
 bp = egl_buff;
 id = (GLuint *) bp; 
 egl_res =  esdl_glIsQuery(*id);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) );
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_beginQuery(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLuint * id;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 id = (GLuint *) bp; 
 esdl_glBeginQuery(*target, *id);
}


void egl_endQuery(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 bp = egl_buff;
 target = (GLenum *) bp; 
 esdl_glEndQuery(*target);
}


void egl_getQueryiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 esdl_glGetQueryiv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *1);
 * (GLint *)bp = params[0]; bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getQueryObjectiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * id;
 GLenum * pname;
 GLint params[1]; 
 bp = egl_buff;
 id = (GLuint *) bp; bp += sizeof(GLuint); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetQueryObjectiv(*id, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *1);
 * (GLint *)bp = params[0]; bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getQueryObjectuiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * id;
 GLenum * pname;
 GLuint params[1]; 
 bp = egl_buff;
 id = (GLuint *) bp; bp += sizeof(GLuint); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetQueryObjectuiv(*id, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLuint) *1);
 * (GLuint *)bp = params[0]; bp += sizeof(GLuint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_bindBuffer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLuint * buffer;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 buffer = (GLuint *) bp; 
 esdl_glBindBuffer(*target, *buffer);
}


void egl_deleteBuffers(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * n;
 GLuint * buffers = NULL; 
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 buffers = (GLuint *) bp;
 esdl_glDeleteBuffers(*n, buffers);
}


void egl_genBuffers(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLsizei * n;
 GLuint * buffers = NULL;
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 buffers = (GLuint*) malloc(sizeof(GLuint)*(*n));
 esdl_glGenBuffers(*n, buffers);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLuint) * (*n));
 memcpy(bp, buffers, sizeof(GLuint)*(*n));
 bp += sizeof(GLuint)*(*n);
 free(buffers);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_isBuffer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLuint * buffer;
 bp = egl_buff;
 buffer = (GLuint *) bp; 
 egl_res =  esdl_glIsBuffer(*buffer);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) );
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_bufferData(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLsizeiptr * size;
 GLvoid * data = NULL;
 GLenum * usage;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 size = (GLsizeiptr *) bp; bp += sizeof(GLsizeiptr); 
 if(egl_sd->next_bin == 0) {
  data = (GLvoid *) *(GLint *)bp;
 } else {
  data = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 usage = (GLenum *) bp; 
 esdl_glBufferData(*target, *size, data, *usage);
 sdl_free_binaries(egl_sd);
}


void egl_bufferSubData(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLintptr * offset;
 GLsizeiptr * size;
 GLvoid * data = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 offset = (GLintptr *) bp; bp += sizeof(GLintptr); 
 size = (GLsizeiptr *) bp; bp += sizeof(GLsizeiptr); 
 if(egl_sd->next_bin == 0) {
  data = (GLvoid *) *(GLint *)bp;
 } else {
  data = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 esdl_glBufferSubData(*target, *offset, *size, data);
 sdl_free_binaries(egl_sd);
}


void egl_getBufferSubData(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLintptr * offset;
 GLsizeiptr * size;
 GLvoid * data = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 offset = (GLintptr *) bp; bp += sizeof(GLintptr); 
 size = (GLsizeiptr *) bp; bp += sizeof(GLsizeiptr); 
 data = (GLvoid *) egl_sd->bin[0].base; 
 esdl_glGetBufferSubData(*target, *offset, *size, data);
 sdl_free_binaries(egl_sd);
}


void egl_unmapBuffer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLenum * target;
 bp = egl_buff;
 target = (GLenum *) bp; 
 egl_res =  esdl_glUnmapBuffer(*target);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) );
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getBufferParameteriv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 esdl_glGetBufferParameteriv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *1);
 * (GLint *)bp = params[0]; bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getBufferPointerv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLenum * pname;
 GLvoid *params = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetBufferPointerv(*target, *pname, &params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLvoid*));
 putPointer(bp, params);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_blendEquationSeparate(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * modeRGB;
 GLenum * modeAlpha;
 bp = egl_buff;
 modeRGB = (GLenum *) bp; bp += sizeof(GLenum); 
 modeAlpha = (GLenum *) bp; 
 esdl_glBlendEquationSeparate(*modeRGB, *modeAlpha);
}


void egl_drawBuffers(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * n;
 GLenum * bufs = NULL; 
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 bufs = (GLenum *) bp;
 esdl_glDrawBuffers(*n, bufs);
}


void egl_stencilOpSeparate(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * face;
 GLenum * sfail;
 GLenum * dpfail;
 GLenum * dppass;
 bp = egl_buff;
 face = (GLenum *) bp; bp += sizeof(GLenum); 
 sfail = (GLenum *) bp; bp += sizeof(GLenum); 
 dpfail = (GLenum *) bp; bp += sizeof(GLenum); 
 dppass = (GLenum *) bp; 
 esdl_glStencilOpSeparate(*face, *sfail, *dpfail, *dppass);
}


void egl_stencilFuncSeparate(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * frontfunc;
 GLenum * backfunc;
 GLint * ref;
 GLuint * mask;
 bp = egl_buff;
 frontfunc = (GLenum *) bp; bp += sizeof(GLenum); 
 backfunc = (GLenum *) bp; bp += sizeof(GLenum); 
 ref = (GLint *) bp; bp += sizeof(GLint); 
 mask = (GLuint *) bp; 
 esdl_glStencilFuncSeparate(*frontfunc, *backfunc, *ref, *mask);
}


void egl_stencilMaskSeparate(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * face;
 GLuint * mask;
 bp = egl_buff;
 face = (GLenum *) bp; bp += sizeof(GLenum); 
 mask = (GLuint *) bp; 
 esdl_glStencilMaskSeparate(*face, *mask);
}


void egl_attachShader(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * program;
 GLuint * shader;
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 shader = (GLuint *) bp; 
 esdl_glAttachShader(*program, *shader);
}


void egl_bindAttribLocation(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * program;
 GLuint * index;
 GLchar * name = NULL;
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 if(egl_sd->next_bin == 0) {
  name = (GLchar *) *(GLint *)bp;
 } else {
  name = (GLchar *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 esdl_glBindAttribLocation(*program, *index, name);
 sdl_free_binaries(egl_sd);
}


void egl_compileShader(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * shader;
 bp = egl_buff;
 shader = (GLuint *) bp; 
 esdl_glCompileShader(*shader);
}


void egl_createProgram(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint egl_res; 
 bp = egl_buff;
 egl_res =  esdl_glCreateProgram();
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLuint) );
 * (GLuint *) bp = egl_res;
 bp += sizeof(GLuint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_createShader(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint egl_res; 
 GLenum * type;
 bp = egl_buff;
 type = (GLenum *) bp; 
 egl_res =  esdl_glCreateShader(*type);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLuint) );
 * (GLuint *) bp = egl_res;
 bp += sizeof(GLuint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_deleteProgram(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * program;
 bp = egl_buff;
 program = (GLuint *) bp; 
 esdl_glDeleteProgram(*program);
}


void egl_deleteShader(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * shader;
 bp = egl_buff;
 shader = (GLuint *) bp; 
 esdl_glDeleteShader(*shader);
}


void egl_detachShader(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * program;
 GLuint * shader;
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 shader = (GLuint *) bp; 
 esdl_glDetachShader(*program, *shader);
}


void egl_disableVertexAttribArray(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 bp = egl_buff;
 index = (GLuint *) bp; 
 esdl_glDisableVertexAttribArray(*index);
}


void egl_enableVertexAttribArray(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 bp = egl_buff;
 index = (GLuint *) bp; 
 esdl_glEnableVertexAttribArray(*index);
}


void egl_getActiveAttrib(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * program;
 GLuint * index;
 GLsizei * bufSize;
 GLsizei length[1]; 
 GLint size[1]; 
 GLenum type[1]; 
 GLchar * name = NULL;
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 bufSize = (GLsizei *) bp; bp += sizeof(GLsizei); 
 name = (GLchar*) malloc(sizeof(GLchar)*(*bufSize));
 esdl_glGetActiveAttrib(*program, *index, *bufSize, length, size, type, name);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLsizei) *1+ sizeof(GLint) *1+ sizeof(GLenum) *1+ sizeof(GLchar) * (*length));
 * (GLsizei *)bp = length[0]; bp += sizeof(GLsizei);
 * (GLint *)bp = size[0]; bp += sizeof(GLint);
 * (GLenum *)bp = type[0]; bp += sizeof(GLenum);
 memcpy(bp, name, sizeof(GLchar)*(*length));
 bp += sizeof(GLchar)*(*length);
 free(name);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getActiveUniform(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * program;
 GLuint * index;
 GLsizei * bufSize;
 GLsizei length[1]; 
 GLint size[1]; 
 GLenum type[1]; 
 GLchar * name = NULL;
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 bufSize = (GLsizei *) bp; bp += sizeof(GLsizei); 
 name = (GLchar*) malloc(sizeof(GLchar)*(*bufSize));
 esdl_glGetActiveUniform(*program, *index, *bufSize, length, size, type, name);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLsizei) *1+ sizeof(GLint) *1+ sizeof(GLenum) *1+ sizeof(GLchar) * (*length));
 * (GLsizei *)bp = length[0]; bp += sizeof(GLsizei);
 * (GLint *)bp = size[0]; bp += sizeof(GLint);
 * (GLenum *)bp = type[0]; bp += sizeof(GLenum);
 memcpy(bp, name, sizeof(GLchar)*(*length));
 bp += sizeof(GLchar)*(*length);
 free(name);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getAttachedShaders(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * program;
 GLsizei * maxCount;
 GLsizei count[1]; 
 GLuint * obj = NULL;
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 maxCount = (GLsizei *) bp; bp += sizeof(GLsizei); 
 obj = (GLuint*) malloc(sizeof(GLuint)*(*maxCount));
 esdl_glGetAttachedShaders(*program, *maxCount, count, obj);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLsizei) *1+ sizeof(GLuint) * (*count));
 * (GLsizei *)bp = count[0]; bp += sizeof(GLsizei);
 memcpy(bp, obj, sizeof(GLuint)*(*count));
 bp += sizeof(GLuint)*(*count);
 free(obj);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getAttribLocation(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLint egl_res; 
 GLuint * program;
 GLchar * name = NULL;
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 if(egl_sd->next_bin == 0) {
  name = (GLchar *) *(GLint *)bp;
 } else {
  name = (GLchar *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 egl_res =  esdl_glGetAttribLocation(*program, name);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) );
 * (GLint *) bp = egl_res;
 bp += sizeof(GLint);
 sdl_free_binaries(egl_sd);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getProgramiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * program;
 GLenum * pname;
 GLint params[1]; 
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetProgramiv(*program, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *1);
 * (GLint *)bp = params[0]; bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getProgramInfoLog(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * program;
 GLsizei * bufSize;
 GLsizei length[1]; 
 GLchar * infoLog = NULL;
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 bufSize = (GLsizei *) bp; bp += sizeof(GLsizei); 
 infoLog = (GLchar*) malloc(sizeof(GLchar)*(*bufSize));
 esdl_glGetProgramInfoLog(*program, *bufSize, length, infoLog);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLsizei) *1+ sizeof(GLchar) * (*length));
 * (GLsizei *)bp = length[0]; bp += sizeof(GLsizei);
 memcpy(bp, infoLog, sizeof(GLchar)*(*length));
 bp += sizeof(GLchar)*(*length);
 free(infoLog);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getShaderiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * shader;
 GLenum * pname;
 GLint params[1]; 
 bp = egl_buff;
 shader = (GLuint *) bp; bp += sizeof(GLuint); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetShaderiv(*shader, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *1);
 * (GLint *)bp = params[0]; bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getShaderInfoLog(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * shader;
 GLsizei * bufSize;
 GLsizei length[1]; 
 GLchar * infoLog = NULL;
 bp = egl_buff;
 shader = (GLuint *) bp; bp += sizeof(GLuint); 
 bufSize = (GLsizei *) bp; bp += sizeof(GLsizei); 
 infoLog = (GLchar*) malloc(sizeof(GLchar)*(*bufSize));
 esdl_glGetShaderInfoLog(*shader, *bufSize, length, infoLog);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLsizei) *1+ sizeof(GLchar) * (*length));
 * (GLsizei *)bp = length[0]; bp += sizeof(GLsizei);
 memcpy(bp, infoLog, sizeof(GLchar)*(*length));
 bp += sizeof(GLchar)*(*length);
 free(infoLog);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getShaderSource(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint * shader;
 GLsizei * bufSize;
 GLsizei length[1]; 
 GLchar * source = NULL;
 bp = egl_buff;
 shader = (GLuint *) bp; bp += sizeof(GLuint); 
 bufSize = (GLsizei *) bp; bp += sizeof(GLsizei); 
 source = (GLchar*) malloc(sizeof(GLchar)*(*bufSize));
 esdl_glGetShaderSource(*shader, *bufSize, length, source);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLsizei) *1+ sizeof(GLchar) * (*length));
 * (GLsizei *)bp = length[0]; bp += sizeof(GLsizei);
 memcpy(bp, source, sizeof(GLchar)*(*length));
 bp += sizeof(GLchar)*(*length);
 free(source);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getUniformLocation(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLint egl_res; 
 GLuint * program;
 GLchar * name = NULL;
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 if(egl_sd->next_bin == 0) {
  name = (GLchar *) *(GLint *)bp;
 } else {
  name = (GLchar *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 egl_res =  esdl_glGetUniformLocation(*program, name);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) );
 * (GLint *) bp = egl_res;
 bp += sizeof(GLint);
 sdl_free_binaries(egl_sd);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getUniformfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * program;
 GLint * location;
 GLvoid * params = NULL;
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 location = (GLint *) bp; bp += sizeof(GLint); 
 params = (GLfloat *) egl_sd->bin[0].base; 
 esdl_glGetUniformfv(*program, *location, params);
 sdl_free_binaries(egl_sd);
}


void egl_getUniformiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * program;
 GLint * location;
 GLvoid * params = NULL;
 bp = egl_buff;
 program = (GLuint *) bp; bp += sizeof(GLuint); 
 location = (GLint *) bp; bp += sizeof(GLint); 
 params = (GLint *) egl_sd->bin[0].base; 
 esdl_glGetUniformiv(*program, *location, params);
 sdl_free_binaries(egl_sd);
}


void egl_getVertexAttribdv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 esdl_glGetVertexAttribdv(*index, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLdouble) *4);
 memcpy(bp, params, sizeof(GLdouble)*4);
 bp += sizeof(GLdouble)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getVertexAttribfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 esdl_glGetVertexAttribfv(*index, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *4);
 memcpy(bp, params, sizeof(GLfloat)*4);
 bp += sizeof(GLfloat)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getVertexAttribiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 esdl_glGetVertexAttribiv(*index, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *4);
 memcpy(bp, params, sizeof(GLint)*4);
 bp += sizeof(GLint)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getVertexAttribPointerv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 esdl_glGetVertexAttribPointerv(*index, *pname, &pointer);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLvoid*));
 putPointer(bp, pointer);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_isProgram(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLuint * program;
 bp = egl_buff;
 program = (GLuint *) bp; 
 egl_res =  esdl_glIsProgram(*program);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) );
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_isShader(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLuint * shader;
 bp = egl_buff;
 shader = (GLuint *) bp; 
 egl_res =  esdl_glIsShader(*shader);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) );
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_linkProgram(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * program;
 bp = egl_buff;
 program = (GLuint *) bp; 
 esdl_glLinkProgram(*program);
}


void egl_shaderSource(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * shader;
 GLsizei * count;
 const GLchar* *string;
 int index;
 GLint * length = NULL; 
 bp = egl_buff;
 shader = (GLuint *) bp; bp += sizeof(GLuint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 string = (const GLchar* *) malloc(sizeof(GLchar*)*(*count));
 for(index=0; index < *count; index++) 
    string[index] = (GLchar *) egl_sd->bin[index+0].base;
 length = (GLint *) bp;
 esdl_glShaderSource(*shader, *count, string, length);
 sdl_free_binaries(egl_sd);
}


void egl_useProgram(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * program;
 bp = egl_buff;
 program = (GLuint *) bp; 
 esdl_glUseProgram(*program);
}


void egl_uniform1f(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLfloat * v0;
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 v0 = (GLfloat *) bp; 
 esdl_glUniform1f(*location, *v0);
}


void egl_uniform2f(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLfloat * v0;
 GLfloat * v1;
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 v0 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 v1 = (GLfloat *) bp; 
 esdl_glUniform2f(*location, *v0, *v1);
}


void egl_uniform3f(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLfloat * v0;
 GLfloat * v1;
 GLfloat * v2;
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 v0 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 v1 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 v2 = (GLfloat *) bp; 
 esdl_glUniform3f(*location, *v0, *v1, *v2);
}


void egl_uniform4f(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLfloat * v0;
 GLfloat * v1;
 GLfloat * v2;
 GLfloat * v3;
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 v0 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 v1 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 v2 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 v3 = (GLfloat *) bp; 
 esdl_glUniform4f(*location, *v0, *v1, *v2, *v3);
}


void egl_uniform1i(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLint * v0;
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 v0 = (GLint *) bp; 
 esdl_glUniform1i(*location, *v0);
}


void egl_uniform2i(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLint * v0;
 GLint * v1;
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 v0 = (GLint *) bp; bp += sizeof(GLint); 
 v1 = (GLint *) bp; 
 esdl_glUniform2i(*location, *v0, *v1);
}


void egl_uniform3i(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLint * v0;
 GLint * v1;
 GLint * v2;
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 v0 = (GLint *) bp; bp += sizeof(GLint); 
 v1 = (GLint *) bp; bp += sizeof(GLint); 
 v2 = (GLint *) bp; 
 esdl_glUniform3i(*location, *v0, *v1, *v2);
}


void egl_uniform4i(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLint * v0;
 GLint * v1;
 GLint * v2;
 GLint * v3;
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 v0 = (GLint *) bp; bp += sizeof(GLint); 
 v1 = (GLint *) bp; bp += sizeof(GLint); 
 v2 = (GLint *) bp; bp += sizeof(GLint); 
 v3 = (GLint *) bp; 
 esdl_glUniform4i(*location, *v0, *v1, *v2, *v3);
}


void egl_uniform1fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLsizei * count;
 GLfloat * value = NULL; 
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 value = (GLfloat *) bp;
 esdl_glUniform1fv(*location, *count, value);
}


void egl_uniform2fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLsizei * count;
 GLfloat * value = NULL; 
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 value = (GLfloat *) bp;
 esdl_glUniform2fv(*location, *count, value);
}


void egl_uniform3fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLsizei * count;
 GLfloat * value = NULL; 
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 value = (GLfloat *) bp;
 esdl_glUniform3fv(*location, *count, value);
}


void egl_uniform4fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLsizei * count;
 GLfloat * value = NULL; 
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 value = (GLfloat *) bp;
 esdl_glUniform4fv(*location, *count, value);
}


void egl_uniform1iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLsizei * count;
 GLint * value = NULL; 
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 value = (GLint *) bp;
 esdl_glUniform1iv(*location, *count, value);
}


void egl_uniform2iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLsizei * count;
 GLint * value = NULL; 
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 value = (GLint *) bp;
 esdl_glUniform2iv(*location, *count, value);
}


void egl_uniform3iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLsizei * count;
 GLint * value = NULL; 
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 value = (GLint *) bp;
 esdl_glUniform3iv(*location, *count, value);
}


void egl_uniform4iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLsizei * count;
 GLint * value = NULL; 
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 value = (GLint *) bp;
 esdl_glUniform4iv(*location, *count, value);
}


void egl_uniformMatrix2fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLsizei * count;
 GLboolean * transpose;
 GLfloat * value;
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 transpose = (GLboolean *) bp; bp += sizeof(GLboolean); 
 bp += 3;
 value = (GLfloat *) bp;
 esdl_glUniformMatrix2fv(*location, *count, *transpose, value);
}


void egl_uniformMatrix3fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLsizei * count;
 GLboolean * transpose;
 GLfloat * value;
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 transpose = (GLboolean *) bp; bp += sizeof(GLboolean); 
 bp += 3;
 value = (GLfloat *) bp;
 esdl_glUniformMatrix3fv(*location, *count, *transpose, value);
}


void egl_uniformMatrix4fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * location;
 GLsizei * count;
 GLboolean * transpose;
 GLfloat * value;
 bp = egl_buff;
 location = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 transpose = (GLboolean *) bp; bp += sizeof(GLboolean); 
 bp += 3;
 value = (GLfloat *) bp;
 esdl_glUniformMatrix4fv(*location, *count, *transpose, value);
}


void egl_validateProgram(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * program;
 bp = egl_buff;
 program = (GLuint *) bp; 
 esdl_glValidateProgram(*program);
}


void egl_vertexAttrib1dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLdouble v[1];
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 memcpy(v,bp,sizeof(GLdouble)*1); 
 esdl_glVertexAttrib1dv(*index, v);
}


void egl_vertexAttrib1fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLfloat * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLfloat *) bp;
 esdl_glVertexAttrib1fv(*index, v);
}


void egl_vertexAttrib1sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLshort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLshort *) bp;
 esdl_glVertexAttrib1sv(*index, v);
}


void egl_vertexAttrib2dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLdouble v[2];
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 memcpy(v,bp,sizeof(GLdouble)*2); 
 esdl_glVertexAttrib2dv(*index, v);
}


void egl_vertexAttrib2fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLfloat * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLfloat *) bp;
 esdl_glVertexAttrib2fv(*index, v);
}


void egl_vertexAttrib2sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLshort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLshort *) bp;
 esdl_glVertexAttrib2sv(*index, v);
}


void egl_vertexAttrib3dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLdouble v[3];
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 memcpy(v,bp,sizeof(GLdouble)*3); 
 esdl_glVertexAttrib3dv(*index, v);
}


void egl_vertexAttrib3fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLfloat * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLfloat *) bp;
 esdl_glVertexAttrib3fv(*index, v);
}


void egl_vertexAttrib3sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLshort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLshort *) bp;
 esdl_glVertexAttrib3sv(*index, v);
}


void egl_vertexAttrib4Nbv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLbyte * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLbyte *) bp;
 esdl_glVertexAttrib4Nbv(*index, v);
}


void egl_vertexAttrib4Niv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLint * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLint *) bp;
 esdl_glVertexAttrib4Niv(*index, v);
}


void egl_vertexAttrib4Nsv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLshort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLshort *) bp;
 esdl_glVertexAttrib4Nsv(*index, v);
}


void egl_vertexAttrib4Nub(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 esdl_glVertexAttrib4Nub(*index, *x, *y, *z, *w);
}


void egl_vertexAttrib4Nubv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLubyte * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLubyte *) bp;
 esdl_glVertexAttrib4Nubv(*index, v);
}


void egl_vertexAttrib4Nuiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLuint * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLuint *) bp;
 esdl_glVertexAttrib4Nuiv(*index, v);
}


void egl_vertexAttrib4Nusv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLushort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLushort *) bp;
 esdl_glVertexAttrib4Nusv(*index, v);
}


void egl_vertexAttrib4bv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLbyte * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLbyte *) bp;
 esdl_glVertexAttrib4bv(*index, v);
}


void egl_vertexAttrib4dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLdouble v[4];
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 memcpy(v,bp,sizeof(GLdouble)*4); 
 esdl_glVertexAttrib4dv(*index, v);
}


void egl_vertexAttrib4fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLfloat * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLfloat *) bp;
 esdl_glVertexAttrib4fv(*index, v);
}


void egl_vertexAttrib4iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLint * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLint *) bp;
 esdl_glVertexAttrib4iv(*index, v);
}


void egl_vertexAttrib4sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLshort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLshort *) bp;
 esdl_glVertexAttrib4sv(*index, v);
}


void egl_vertexAttrib4ubv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLubyte * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLubyte *) bp;
 esdl_glVertexAttrib4ubv(*index, v);
}


void egl_vertexAttrib4uiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLuint * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLuint *) bp;
 esdl_glVertexAttrib4uiv(*index, v);
}


void egl_vertexAttrib4usv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * index;
 GLushort * v;
 bp = egl_buff;
 index = (GLuint *) bp; bp += sizeof(GLuint); 
 v = (GLushort *) bp;
 esdl_glVertexAttrib4usv(*index, v);
}


void egl_vertexAttribPointer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 if(egl_sd->next_bin == 0) {
  pointer = (GLvoid *) *(GLint *)bp;
 } else {
  pointer = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 esdl_glVertexAttribPointer(*index, *size, *type, *normalized, *stride, pointer);
 sdl_free_binaries(egl_sd);
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
 if(egl_sd->next_bin == 0) {
  pointer = (GLvoid *) *(GLint *)bp;
 } else {
  pointer = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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
 if(egl_sd->next_bin == 0) {
  pointer = (GLvoid *) *(GLint *)bp;
 } else {
  pointer = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 esdl_glMatrixIndexPointerARB(*size, *type, *stride, pointer);
 sdl_free_binaries(egl_sd);
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
 if(egl_sd->next_bin == 0) {
  string = (GLvoid *) *(GLint *)bp;
 } else {
  string = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
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


void egl_stencilOpSeparateATI(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * face;
 GLenum * sfail;
 GLenum * dpfail;
 GLenum * dppass;
 bp = egl_buff;
 face = (GLenum *) bp; bp += sizeof(GLenum); 
 sfail = (GLenum *) bp; bp += sizeof(GLenum); 
 dpfail = (GLenum *) bp; bp += sizeof(GLenum); 
 dppass = (GLenum *) bp; 
 esdl_glStencilOpSeparateATI(*face, *sfail, *dpfail, *dppass);
}


void egl_stencilFuncSeparateATI(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * frontfunc;
 GLenum * backfunc;
 GLint * ref;
 GLuint * mask;
 bp = egl_buff;
 frontfunc = (GLenum *) bp; bp += sizeof(GLenum); 
 backfunc = (GLenum *) bp; bp += sizeof(GLenum); 
 ref = (GLint *) bp; bp += sizeof(GLint); 
 mask = (GLuint *) bp; 
 esdl_glStencilFuncSeparateATI(*frontfunc, *backfunc, *ref, *mask);
}


void egl_isRenderbufferEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLuint * renderbuffer;
 bp = egl_buff;
 renderbuffer = (GLuint *) bp; 
 egl_res =  esdl_glIsRenderbufferEXT(*renderbuffer);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) );
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_bindRenderbufferEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLuint * renderbuffer;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 renderbuffer = (GLuint *) bp; 
 esdl_glBindRenderbufferEXT(*target, *renderbuffer);
}


void egl_deleteRenderbuffersEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * n;
 GLuint * renderbuffers = NULL; 
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 renderbuffers = (GLuint *) bp;
 esdl_glDeleteRenderbuffersEXT(*n, renderbuffers);
}


void egl_genRenderbuffersEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLsizei * n;
 GLuint * renderbuffers = NULL;
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 renderbuffers = (GLuint*) malloc(sizeof(GLuint)*(*n));
 esdl_glGenRenderbuffersEXT(*n, renderbuffers);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLuint) * (*n));
 memcpy(bp, renderbuffers, sizeof(GLuint)*(*n));
 bp += sizeof(GLuint)*(*n);
 free(renderbuffers);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_renderbufferStorageEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * internalformat;
 GLsizei * width;
 GLsizei * height;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; 
 esdl_glRenderbufferStorageEXT(*target, *internalformat, *width, *height);
}


void egl_getRenderbufferParameterivEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 esdl_glGetRenderbufferParameterivEXT(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *1);
 * (GLint *)bp = params[0]; bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_isFramebufferEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLuint * framebuffer;
 bp = egl_buff;
 framebuffer = (GLuint *) bp; 
 egl_res =  esdl_glIsFramebufferEXT(*framebuffer);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) );
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_bindFramebufferEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLuint * framebuffer;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 framebuffer = (GLuint *) bp; 
 esdl_glBindFramebufferEXT(*target, *framebuffer);
}


void egl_deleteFramebuffersEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * n;
 GLuint * framebuffers = NULL; 
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 framebuffers = (GLuint *) bp;
 esdl_glDeleteFramebuffersEXT(*n, framebuffers);
}


void egl_genFramebuffersEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLsizei * n;
 GLuint * framebuffers = NULL;
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 framebuffers = (GLuint*) malloc(sizeof(GLuint)*(*n));
 esdl_glGenFramebuffersEXT(*n, framebuffers);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLuint) * (*n));
 memcpy(bp, framebuffers, sizeof(GLuint)*(*n));
 bp += sizeof(GLuint)*(*n);
 free(framebuffers);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_checkFramebufferStatusEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum egl_res; 
 GLenum * target;
 bp = egl_buff;
 target = (GLenum *) bp; 
 egl_res =  esdl_glCheckFramebufferStatusEXT(*target);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLenum) );
 * (GLenum *) bp = egl_res;
 bp += sizeof(GLenum);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_framebufferTexture1DEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * attachment;
 GLenum * textarget;
 GLuint * texture;
 GLint * level;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 attachment = (GLenum *) bp; bp += sizeof(GLenum); 
 textarget = (GLenum *) bp; bp += sizeof(GLenum); 
 texture = (GLuint *) bp; bp += sizeof(GLuint); 
 level = (GLint *) bp; 
 esdl_glFramebufferTexture1DEXT(*target, *attachment, *textarget, *texture, *level);
}


void egl_framebufferTexture2DEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * attachment;
 GLenum * textarget;
 GLuint * texture;
 GLint * level;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 attachment = (GLenum *) bp; bp += sizeof(GLenum); 
 textarget = (GLenum *) bp; bp += sizeof(GLenum); 
 texture = (GLuint *) bp; bp += sizeof(GLuint); 
 level = (GLint *) bp; 
 esdl_glFramebufferTexture2DEXT(*target, *attachment, *textarget, *texture, *level);
}


void egl_framebufferTexture3DEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * attachment;
 GLenum * textarget;
 GLuint * texture;
 GLint * level;
 GLint * zoffset;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 attachment = (GLenum *) bp; bp += sizeof(GLenum); 
 textarget = (GLenum *) bp; bp += sizeof(GLenum); 
 texture = (GLuint *) bp; bp += sizeof(GLuint); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 zoffset = (GLint *) bp; 
 esdl_glFramebufferTexture3DEXT(*target, *attachment, *textarget, *texture, *level, *zoffset);
}


void egl_framebufferRenderbufferEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * attachment;
 GLenum * renderbuffertarget;
 GLuint * renderbuffer;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 attachment = (GLenum *) bp; bp += sizeof(GLenum); 
 renderbuffertarget = (GLenum *) bp; bp += sizeof(GLenum); 
 renderbuffer = (GLuint *) bp; 
 esdl_glFramebufferRenderbufferEXT(*target, *attachment, *renderbuffertarget, *renderbuffer);
}


void egl_getFramebufferAttachmentParameterivEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLenum * attachment;
 GLenum * pname;
 GLint params[1]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 attachment = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 esdl_glGetFramebufferAttachmentParameterivEXT(*target, *attachment, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *1);
 * (GLint *)bp = params[0]; bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_generateMipmapEXT(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 bp = egl_buff;
 target = (GLenum *) bp; 
 esdl_glGenerateMipmapEXT(*target);
}


