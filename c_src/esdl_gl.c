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

#ifdef FLAVOUR_WOGGLE
#include <woggle_driver.h>
#else
#include "esdl.h"
#endif
void egl_accum(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * op;
 GLfloat * value;
 bp = egl_buff;
 op = (GLenum *) bp; bp += sizeof(GLenum); 
 value = (GLfloat *) bp; 
 glAccum(*op, *value);
}


void egl_alphaFunc(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * func;
 GLclampf * ref;
 bp = egl_buff;
 func = (GLenum *) bp; bp += sizeof(GLenum); 
 ref = (GLclampf *) bp; 
 glAlphaFunc(*func, *ref);
}


void egl_areTexturesResident(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLsizei * n;
 GLuint * textures = NULL; 
 GLboolean * residences = NULL;
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 textures = (GLuint *) bp;
 bp += sizeof(GLuint)*(*n); 
 residences = (GLboolean*) malloc(sizeof(GLboolean)*(*n));
 egl_res =  glAreTexturesResident(*n, textures, residences);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) + sizeof(GLboolean) * (*n));
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 memcpy(bp, residences, sizeof(GLboolean)*(*n));
 bp += sizeof(GLboolean)*(*n);
 free(residences);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_arrayElement(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * i;
 bp = egl_buff;
 i = (GLint *) bp; 
 glArrayElement(*i);
}


void egl_begin(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 bp = egl_buff;
 mode = (GLenum *) bp; 
 glBegin(*mode);
}


void egl_bindTexture(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLuint * texture;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 texture = (GLuint *) bp; 
 glBindTexture(*target, *texture);
}


void egl_bitmap(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * width;
 GLsizei * height;
 GLfloat * xorig;
 GLfloat * yorig;
 GLfloat * xmove;
 GLfloat * ymove;
 GLubyte * bitmap = NULL;
 bp = egl_buff;
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 xorig = (GLfloat *) bp; bp += sizeof(GLfloat); 
 yorig = (GLfloat *) bp; bp += sizeof(GLfloat); 
 xmove = (GLfloat *) bp; bp += sizeof(GLfloat); 
 ymove = (GLfloat *) bp; bp += sizeof(GLfloat); 
 if(egl_sd->next_bin == 0) {
  bitmap = (GLubyte *) *(GLint *)bp;
 } else {
  bitmap = (GLubyte *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 glBitmap(*width, *height, *xorig, *yorig, *xmove, *ymove, bitmap);
 sdl_free_binaries(egl_sd);
}


void egl_blendFunc(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * sfactor;
 GLenum * dfactor;
 bp = egl_buff;
 sfactor = (GLenum *) bp; bp += sizeof(GLenum); 
 dfactor = (GLenum *) bp; 
 glBlendFunc(*sfactor, *dfactor);
}


void egl_callList(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * list;
 bp = egl_buff;
 list = (GLuint *) bp; 
 glCallList(*list);
}


void egl_callLists(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * n;
 GLenum * type;
 GLvoid * lists = NULL; 
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 lists = (GLvoid *) bp;
 glCallLists(*n, *type, lists);
}


void egl_clear(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLbitfield * mask;
 bp = egl_buff;
 mask = (GLbitfield *) bp; 
 glClear(*mask);
}


void egl_clearAccum(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * red;
 GLfloat * green;
 GLfloat * blue;
 GLfloat * alpha;
 bp = egl_buff;
 red = (GLfloat *) bp; bp += sizeof(GLfloat); 
 green = (GLfloat *) bp; bp += sizeof(GLfloat); 
 blue = (GLfloat *) bp; bp += sizeof(GLfloat); 
 alpha = (GLfloat *) bp; 
 glClearAccum(*red, *green, *blue, *alpha);
}


void egl_clearColor(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glClearColor(*red, *green, *blue, *alpha);
}


void egl_clearDepth(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLclampd depth;
 bp = egl_buff;
 memcpy(&depth, bp, sizeof(GLclampd)); 
 glClearDepth(depth);
}


void egl_clearIndex(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * c;
 bp = egl_buff;
 c = (GLfloat *) bp; 
 glClearIndex(*c);
}


void egl_clearStencil(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * s;
 bp = egl_buff;
 s = (GLint *) bp; 
 glClearStencil(*s);
}


void egl_clipPlane(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * plane;
 GLdouble equation[4];
 bp = egl_buff;
 plane = (GLenum *) bp; bp += sizeof(GLenum); 
 memcpy(equation,bp,sizeof(GLdouble)*4); 
 glClipPlane(*plane, equation);
}


void egl_color3bv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLbyte * v;
 bp = egl_buff;
 v = (GLbyte *) bp;
 glColor3bv(v);
}


void egl_color3dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[3];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*3); 
 glColor3dv(v);
}


void egl_color3fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glColor3fv(v);
}


void egl_color3iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glColor3iv(v);
}


void egl_color3sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glColor3sv(v);
}


void egl_color3ubv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLubyte * v;
 bp = egl_buff;
 v = (GLubyte *) bp;
 glColor3ubv(v);
}


void egl_color3uiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * v;
 bp = egl_buff;
 v = (GLuint *) bp;
 glColor3uiv(v);
}


void egl_color3usv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLushort * v;
 bp = egl_buff;
 v = (GLushort *) bp;
 glColor3usv(v);
}


void egl_color4bv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLbyte * v;
 bp = egl_buff;
 v = (GLbyte *) bp;
 glColor4bv(v);
}


void egl_color4dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[4];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*4); 
 glColor4dv(v);
}


void egl_color4fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glColor4fv(v);
}


void egl_color4iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glColor4iv(v);
}


void egl_color4sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glColor4sv(v);
}


void egl_color4ubv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLubyte * v;
 bp = egl_buff;
 v = (GLubyte *) bp;
 glColor4ubv(v);
}


void egl_color4uiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * v;
 bp = egl_buff;
 v = (GLuint *) bp;
 glColor4uiv(v);
}


void egl_color4usv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLushort * v;
 bp = egl_buff;
 v = (GLushort *) bp;
 glColor4usv(v);
}


void egl_colorMask(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLboolean * red;
 GLboolean * green;
 GLboolean * blue;
 GLboolean * alpha;
 bp = egl_buff;
 red = (GLboolean *) bp; bp += sizeof(GLboolean); 
 green = (GLboolean *) bp; bp += sizeof(GLboolean); 
 blue = (GLboolean *) bp; bp += sizeof(GLboolean); 
 alpha = (GLboolean *) bp; 
 glColorMask(*red, *green, *blue, *alpha);
}


void egl_colorMaterial(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * face;
 GLenum * mode;
 bp = egl_buff;
 face = (GLenum *) bp; bp += sizeof(GLenum); 
 mode = (GLenum *) bp; 
 glColorMaterial(*face, *mode);
}


void egl_colorPointer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glColorPointer(*size, *type, *stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_copyPixels(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * x;
 GLint * y;
 GLsizei * width;
 GLsizei * height;
 GLenum * type;
 bp = egl_buff;
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 type = (GLenum *) bp; 
 glCopyPixels(*x, *y, *width, *height, *type);
}


void egl_copyTexImage1D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLenum * internalformat;
 GLint * x;
 GLint * y;
 GLsizei * width;
 GLint * border;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 border = (GLint *) bp; 
 glCopyTexImage1D(*target, *level, *internalformat, *x, *y, *width, *border);
}


void egl_copyTexImage2D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLenum * internalformat;
 GLint * x;
 GLint * y;
 GLsizei * width;
 GLsizei * height;
 GLint * border;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 internalformat = (GLenum *) bp; bp += sizeof(GLenum); 
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 border = (GLint *) bp; 
 glCopyTexImage2D(*target, *level, *internalformat, *x, *y, *width, *height, *border);
}


void egl_copyTexSubImage1D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * xoffset;
 GLint * x;
 GLint * y;
 GLsizei * width;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 xoffset = (GLint *) bp; bp += sizeof(GLint); 
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; 
 glCopyTexSubImage1D(*target, *level, *xoffset, *x, *y, *width);
}


void egl_copyTexSubImage2D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * xoffset;
 GLint * yoffset;
 GLint * x;
 GLint * y;
 GLsizei * width;
 GLsizei * height;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 xoffset = (GLint *) bp; bp += sizeof(GLint); 
 yoffset = (GLint *) bp; bp += sizeof(GLint); 
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; 
 glCopyTexSubImage2D(*target, *level, *xoffset, *yoffset, *x, *y, *width, *height);
}


void egl_cullFace(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 bp = egl_buff;
 mode = (GLenum *) bp; 
 glCullFace(*mode);
}


void egl_deleteLists(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * list;
 GLsizei * range;
 bp = egl_buff;
 list = (GLuint *) bp; bp += sizeof(GLuint); 
 range = (GLsizei *) bp; 
 glDeleteLists(*list, *range);
}


void egl_deleteTextures(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * n;
 GLuint * textures = NULL; 
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 textures = (GLuint *) bp;
 glDeleteTextures(*n, textures);
}


void egl_depthFunc(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * func;
 bp = egl_buff;
 func = (GLenum *) bp; 
 glDepthFunc(*func);
}


void egl_depthMask(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLboolean * flag;
 bp = egl_buff;
 flag = (GLboolean *) bp; 
 glDepthMask(*flag);
}


void egl_depthRange(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLclampd zNear;
 GLclampd zFar;
 bp = egl_buff;
 memcpy(&zNear, bp, sizeof(GLclampd)); bp += sizeof(GLclampd); 
 memcpy(&zFar, bp, sizeof(GLclampd)); 
 glDepthRange(zNear, zFar);
}


void egl_disable(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * cap;
 bp = egl_buff;
 cap = (GLenum *) bp; 
 glDisable(*cap);
}


void egl_disableClientState(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * array;
 bp = egl_buff;
 array = (GLenum *) bp; 
 glDisableClientState(*array);
}


void egl_drawArrays(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 GLint * first;
 GLsizei * count;
 bp = egl_buff;
 mode = (GLenum *) bp; bp += sizeof(GLenum); 
 first = (GLint *) bp; bp += sizeof(GLint); 
 count = (GLsizei *) bp; 
 glDrawArrays(*mode, *first, *count);
}


void egl_drawBuffer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 bp = egl_buff;
 mode = (GLenum *) bp; 
 glDrawBuffer(*mode);
}


void egl_drawElements(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 GLsizei * count;
 GLenum * type;
 GLvoid * indices = NULL; 
 bp = egl_buff;
 mode = (GLenum *) bp; bp += sizeof(GLenum); 
 count = (GLsizei *) bp; bp += sizeof(GLsizei); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 indices = (GLvoid *) bp;
 glDrawElements(*mode, *count, *type, indices);
}


void egl_drawPixels(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * width;
 GLsizei * height;
 GLenum * format;
 GLenum * type;
 GLvoid * pixels = NULL;
 bp = egl_buff;
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 if(egl_sd->next_bin == 0) {
  pixels = (GLvoid *) *(GLint *)bp;
 } else {
  pixels = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 glDrawPixels(*width, *height, *format, *type, pixels);
 sdl_free_binaries(egl_sd);
}


void egl_edgeFlag(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLboolean * flag;
 bp = egl_buff;
 flag = (GLboolean *) bp; 
 glEdgeFlag(*flag);
}


void egl_edgeFlagPointer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * stride;
 GLboolean * pointer = NULL;
 bp = egl_buff;
 stride = (GLsizei *) bp; bp += sizeof(GLsizei); 
 if(egl_sd->next_bin == 0) {
  pointer = (GLboolean *) *(GLint *)bp;
 } else {
  pointer = (GLboolean *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 glEdgeFlagPointer(*stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_edgeFlagv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLboolean * flag;
 bp = egl_buff;
 flag = (GLboolean *) bp;
 glEdgeFlagv(flag);
}


void egl_enable(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * cap;
 bp = egl_buff;
 cap = (GLenum *) bp; 
 glEnable(*cap);
}


void egl_enableClientState(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * array;
 bp = egl_buff;
 array = (GLenum *) bp; 
 glEnableClientState(*array);
}


void egl_end(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 bp = egl_buff;
 glEnd();
}


void egl_endList(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 bp = egl_buff;
 glEndList();
}


void egl_evalCoord1dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble u[1];
 bp = egl_buff;
 memcpy(u,bp,sizeof(GLdouble)*1); 
 glEvalCoord1dv(u);
}


void egl_evalCoord1fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * u;
 bp = egl_buff;
 u = (GLfloat *) bp;
 glEvalCoord1fv(u);
}


void egl_evalCoord2dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble u[2];
 bp = egl_buff;
 memcpy(u,bp,sizeof(GLdouble)*2); 
 glEvalCoord2dv(u);
}


void egl_evalCoord2fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * u;
 bp = egl_buff;
 u = (GLfloat *) bp;
 glEvalCoord2fv(u);
}


void egl_evalMesh1(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 GLint * i1;
 GLint * i2;
 bp = egl_buff;
 mode = (GLenum *) bp; bp += sizeof(GLenum); 
 i1 = (GLint *) bp; bp += sizeof(GLint); 
 i2 = (GLint *) bp; 
 glEvalMesh1(*mode, *i1, *i2);
}


void egl_evalMesh2(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 GLint * i1;
 GLint * i2;
 GLint * j1;
 GLint * j2;
 bp = egl_buff;
 mode = (GLenum *) bp; bp += sizeof(GLenum); 
 i1 = (GLint *) bp; bp += sizeof(GLint); 
 i2 = (GLint *) bp; bp += sizeof(GLint); 
 j1 = (GLint *) bp; bp += sizeof(GLint); 
 j2 = (GLint *) bp; 
 glEvalMesh2(*mode, *i1, *i2, *j1, *j2);
}


void egl_evalPoint1(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * i;
 bp = egl_buff;
 i = (GLint *) bp; 
 glEvalPoint1(*i);
}


void egl_evalPoint2(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * i;
 GLint * j;
 bp = egl_buff;
 i = (GLint *) bp; bp += sizeof(GLint); 
 j = (GLint *) bp; 
 glEvalPoint2(*i, *j);
}


void egl_feedbackBuffer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * size;
 GLenum * type;
 GLvoid * buffer = NULL;
 bp = egl_buff;
 size = (GLsizei *) bp; bp += sizeof(GLsizei); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 buffer = (GLfloat *) egl_sd->bin[0].base; 
 glFeedbackBuffer(*size, *type, buffer);
 sdl_free_binaries(egl_sd);
}


void egl_finish(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 bp = egl_buff;
 glFinish();
}


void egl_flush(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 bp = egl_buff;
 glFlush();
}


void egl_fogf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLfloat * param;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLfloat *) bp; 
 glFogf(*pname, *param);
}


void egl_fogfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLfloat * params;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 bp += sizeof(int); 
 params = (GLfloat *) bp; 
 glFogfv(*pname, params);
}


void egl_fogi(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLint * param;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLint *) bp; 
 glFogi(*pname, *param);
}


void egl_fogiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLint * params;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 bp += sizeof(int); 
 params = (GLint *) bp; 
 glFogiv(*pname, params);
}


void egl_frontFace(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 bp = egl_buff;
 mode = (GLenum *) bp; 
 glFrontFace(*mode);
}


void egl_frustum(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble left;
 GLdouble right;
 GLdouble bottom;
 GLdouble top;
 GLdouble zNear;
 GLdouble zFar;
 bp = egl_buff;
 memcpy(&left, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&right, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&bottom, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&top, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&zNear, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&zFar, bp, sizeof(GLdouble)); 
 glFrustum(left, right, bottom, top, zNear, zFar);
}


void egl_genLists(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLuint egl_res; 
 GLsizei * range;
 bp = egl_buff;
 range = (GLsizei *) bp; 
 egl_res =  glGenLists(*range);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLuint) );
 * (GLuint *) bp = egl_res;
 bp += sizeof(GLuint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_genTextures(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLsizei * n;
 GLuint * textures = NULL;
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 textures = (GLuint*) malloc(sizeof(GLuint)*(*n));
 glGenTextures(*n, textures);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLuint) * (*n));
 memcpy(bp, textures, sizeof(GLuint)*(*n));
 bp += sizeof(GLuint)*(*n);
 free(textures);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getBooleanv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * pname;
 GLboolean params[16];
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetBooleanv(*pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) *16);
 memcpy(bp, params, sizeof(GLboolean)*16);
 bp += sizeof(GLboolean)*16;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getClipPlane(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * plane;
 GLdouble equation[4]; 
 bp = egl_buff;
 plane = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetClipPlane(*plane, equation);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLdouble) *4);
 memcpy(bp, equation, sizeof(GLdouble)*4);
 bp += sizeof(GLdouble)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getDoublev(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * pname;
 GLdouble params[16];
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetDoublev(*pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLdouble) *16);
 memcpy(bp, params, sizeof(GLdouble)*16);
 bp += sizeof(GLdouble)*16;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getError(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum egl_res; 
 bp = egl_buff;
 egl_res =  glGetError();
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLenum) );
 * (GLenum *) bp = egl_res;
 bp += sizeof(GLenum);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getFloatv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * pname;
 GLfloat params[16];
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetFloatv(*pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *16);
 memcpy(bp, params, sizeof(GLfloat)*16);
 bp += sizeof(GLfloat)*16;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getIntegerv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * pname;
 GLint params[16];
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetIntegerv(*pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *16);
 memcpy(bp, params, sizeof(GLint)*16);
 bp += sizeof(GLint)*16;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getLightfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * light;
 GLenum * pname;
 GLfloat params[4];
 bp = egl_buff;
 light = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetLightfv(*light, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *4);
 memcpy(bp, params, sizeof(GLfloat)*4);
 bp += sizeof(GLfloat)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getLightiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * light;
 GLenum * pname;
 GLint params[4];
 bp = egl_buff;
 light = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetLightiv(*light, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *4);
 memcpy(bp, params, sizeof(GLint)*4);
 bp += sizeof(GLint)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getMapdv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * query;
 GLvoid * v = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 query = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLdouble *) egl_sd->bin[0].base; 
 glGetMapdv(*target, *query, v);
 sdl_free_binaries(egl_sd);
}


void egl_getMapfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * query;
 GLvoid * v = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 query = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLfloat *) egl_sd->bin[0].base; 
 glGetMapfv(*target, *query, v);
 sdl_free_binaries(egl_sd);
}


void egl_getMapiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * query;
 GLvoid * v = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 query = (GLenum *) bp; bp += sizeof(GLenum); 
 v = (GLint *) egl_sd->bin[0].base; 
 glGetMapiv(*target, *query, v);
 sdl_free_binaries(egl_sd);
}


void egl_getMaterialfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * face;
 GLenum * pname;
 GLfloat params[4];
 bp = egl_buff;
 face = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetMaterialfv(*face, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *4);
 memcpy(bp, params, sizeof(GLfloat)*4);
 bp += sizeof(GLfloat)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getMaterialiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * face;
 GLenum * pname;
 GLint params[4];
 bp = egl_buff;
 face = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetMaterialiv(*face, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *4);
 memcpy(bp, params, sizeof(GLint)*4);
 bp += sizeof(GLint)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getPixelMapfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * map;
 GLvoid * values = NULL;
 bp = egl_buff;
 map = (GLenum *) bp; bp += sizeof(GLenum); 
 values = (GLfloat *) egl_sd->bin[0].base; 
 glGetPixelMapfv(*map, values);
 sdl_free_binaries(egl_sd);
}


void egl_getPixelMapuiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * map;
 GLvoid * values = NULL;
 bp = egl_buff;
 map = (GLenum *) bp; bp += sizeof(GLenum); 
 values = (GLuint *) egl_sd->bin[0].base; 
 glGetPixelMapuiv(*map, values);
 sdl_free_binaries(egl_sd);
}


void egl_getPixelMapusv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * map;
 GLvoid * values = NULL;
 bp = egl_buff;
 map = (GLenum *) bp; bp += sizeof(GLenum); 
 values = (GLushort *) egl_sd->bin[0].base; 
 glGetPixelMapusv(*map, values);
 sdl_free_binaries(egl_sd);
}


void egl_getPointerv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * pname;
 GLvoid *params = NULL;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetPointerv(*pname, &params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLvoid*));
 putPointer(bp, params);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getPolygonStipple(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLubyte mask[128]; 
 bp = egl_buff;
 glGetPolygonStipple(mask);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLubyte) *128);
 memcpy(bp, mask, sizeof(GLubyte)*128);
 bp += sizeof(GLubyte)*128;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getString(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 const GLubyte *egl_res; 
 GLenum * name;
 bp = egl_buff;
 name = (GLenum *) bp; 
 egl_res =  glGetString(*name);
 bp = egl_start = sdl_get_temp_buff(egl_sd, strlen(egl_res) );
 strcpy((GLubyte *)bp, egl_res);
 bp += strlen(egl_res);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getTexEnvfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glGetTexEnvfv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *4);
 memcpy(bp, params, sizeof(GLfloat)*4);
 bp += sizeof(GLfloat)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getTexEnviv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glGetTexEnviv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *4);
 memcpy(bp, params, sizeof(GLint)*4);
 bp += sizeof(GLint)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getTexGendv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * coord;
 GLenum * pname;
 GLdouble params[4];
 bp = egl_buff;
 coord = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetTexGendv(*coord, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLdouble) *4);
 memcpy(bp, params, sizeof(GLdouble)*4);
 bp += sizeof(GLdouble)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getTexGenfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * coord;
 GLenum * pname;
 GLfloat params[4];
 bp = egl_buff;
 coord = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetTexGenfv(*coord, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *4);
 memcpy(bp, params, sizeof(GLfloat)*4);
 bp += sizeof(GLfloat)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getTexGeniv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * coord;
 GLenum * pname;
 GLint params[4];
 bp = egl_buff;
 coord = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetTexGeniv(*coord, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *4);
 memcpy(bp, params, sizeof(GLint)*4);
 bp += sizeof(GLint)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getTexImage(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLenum * format;
 GLenum * type;
 GLvoid * pixels = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 pixels = (GLvoid *) egl_sd->bin[0].base; 
 glGetTexImage(*target, *level, *format, *type, pixels);
 sdl_free_binaries(egl_sd);
}


void egl_getTexLevelParameterfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLint * level;
 GLenum * pname;
 GLfloat params[1]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetTexLevelParameterfv(*target, *level, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *1);
 * (GLfloat *)bp = params[0]; bp += sizeof(GLfloat);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getTexLevelParameteriv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLenum * target;
 GLint * level;
 GLenum * pname;
 GLint params[1]; 
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 glGetTexLevelParameteriv(*target, *level, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *1);
 * (GLint *)bp = params[0]; bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getTexParameterfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glGetTexParameterfv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLfloat) *4);
 memcpy(bp, params, sizeof(GLfloat)*4);
 bp += sizeof(GLfloat)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_getTexParameteriv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glGetTexParameteriv(*target, *pname, params);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) *4);
 memcpy(bp, params, sizeof(GLint)*4);
 bp += sizeof(GLint)*4;
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_hint(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * mode;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 mode = (GLenum *) bp; 
 glHint(*target, *mode);
}


void egl_indexMask(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * mask;
 bp = egl_buff;
 mask = (GLuint *) bp; 
 glIndexMask(*mask);
}


void egl_indexPointer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glIndexPointer(*type, *stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_indexd(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble c;
 bp = egl_buff;
 memcpy(&c, bp, sizeof(GLdouble)); 
 glIndexd(c);
}


void egl_indexdv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble c[1];
 bp = egl_buff;
 memcpy(c,bp,sizeof(GLdouble)*1); 
 glIndexdv(c);
}


void egl_indexf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * c;
 bp = egl_buff;
 c = (GLfloat *) bp; 
 glIndexf(*c);
}


void egl_indexfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * c;
 bp = egl_buff;
 c = (GLfloat *) bp;
 glIndexfv(c);
}


void egl_indexi(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * c;
 bp = egl_buff;
 c = (GLint *) bp; 
 glIndexi(*c);
}


void egl_indexiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * c;
 bp = egl_buff;
 c = (GLint *) bp;
 glIndexiv(c);
}


void egl_indexs(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * c;
 bp = egl_buff;
 c = (GLshort *) bp; 
 glIndexs(*c);
}


void egl_indexsv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * c;
 bp = egl_buff;
 c = (GLshort *) bp;
 glIndexsv(c);
}


void egl_indexub(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLubyte * c;
 bp = egl_buff;
 c = (GLubyte *) bp; 
 glIndexub(*c);
}


void egl_indexubv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLubyte * c;
 bp = egl_buff;
 c = (GLubyte *) bp;
 glIndexubv(c);
}


void egl_initNames(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 bp = egl_buff;
 glInitNames();
}


void egl_interleavedArrays(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * format;
 GLsizei * stride;
 GLvoid * pointer = NULL;
 bp = egl_buff;
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 stride = (GLsizei *) bp; bp += sizeof(GLsizei); 
 if(egl_sd->next_bin == 0) {
  pointer = (GLvoid *) *(GLint *)bp;
 } else {
  pointer = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 glInterleavedArrays(*format, *stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_isEnabled(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLenum * cap;
 bp = egl_buff;
 cap = (GLenum *) bp; 
 egl_res =  glIsEnabled(*cap);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) );
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_isList(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLuint * list;
 bp = egl_buff;
 list = (GLuint *) bp; 
 egl_res =  glIsList(*list);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) );
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_isTexture(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLboolean egl_res; 
 GLuint * texture;
 bp = egl_buff;
 texture = (GLuint *) bp; 
 egl_res =  glIsTexture(*texture);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLboolean) );
 * (GLboolean *) bp = egl_res;
 bp += sizeof(GLboolean);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_lightModelf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLfloat * param;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLfloat *) bp; 
 glLightModelf(*pname, *param);
}


void egl_lightModelfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLfloat * params;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 bp += sizeof(int); 
 params = (GLfloat *) bp; 
 glLightModelfv(*pname, params);
}


void egl_lightModeli(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLint * param;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLint *) bp; 
 glLightModeli(*pname, *param);
}


void egl_lightModeliv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLint * params;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 bp += sizeof(int); 
 params = (GLint *) bp; 
 glLightModeliv(*pname, params);
}


void egl_lightf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * light;
 GLenum * pname;
 GLfloat * param;
 bp = egl_buff;
 light = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLfloat *) bp; 
 glLightf(*light, *pname, *param);
}


void egl_lightfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * light;
 GLenum * pname;
 GLfloat * params;
 bp = egl_buff;
 light = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 bp += sizeof(int); 
 params = (GLfloat *) bp; 
 glLightfv(*light, *pname, params);
}


void egl_lighti(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * light;
 GLenum * pname;
 GLint * param;
 bp = egl_buff;
 light = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLint *) bp; 
 glLighti(*light, *pname, *param);
}


void egl_lightiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * light;
 GLenum * pname;
 GLint * params;
 bp = egl_buff;
 light = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 bp += sizeof(int); 
 params = (GLint *) bp; 
 glLightiv(*light, *pname, params);
}


void egl_lineStipple(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * factor;
 GLushort * pattern;
 bp = egl_buff;
 factor = (GLint *) bp; bp += sizeof(GLint); 
 pattern = (GLushort *) bp; 
 glLineStipple(*factor, *pattern);
}


void egl_lineWidth(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * width;
 bp = egl_buff;
 width = (GLfloat *) bp; 
 glLineWidth(*width);
}


void egl_listBase(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * base;
 bp = egl_buff;
 base = (GLuint *) bp; 
 glListBase(*base);
}


void egl_loadIdentity(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 bp = egl_buff;
 glLoadIdentity();
}


void egl_loadMatrixd(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble m[16];
 bp = egl_buff;
 memcpy(m,bp,sizeof(GLdouble)*16); 
 glLoadMatrixd(m);
}


void egl_loadMatrixf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * m;
 bp = egl_buff;
 m = (GLfloat *) bp;
 glLoadMatrixf(m);
}


void egl_loadName(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * name;
 bp = egl_buff;
 name = (GLuint *) bp; 
 glLoadName(*name);
}


void egl_logicOp(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * opcode;
 bp = egl_buff;
 opcode = (GLenum *) bp; 
 glLogicOp(*opcode);
}


void egl_map1d(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLdouble u1;
 GLdouble u2;
 GLint * stride;
 GLint * order;
 GLdouble *points;int * pointsLen;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 memcpy(&u1, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&u2, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 stride = (GLint *) bp; bp += sizeof(GLint); 
 order = (GLint *) bp; bp += sizeof(GLint); 
 pointsLen  = (int *) bp; bp += sizeof(int); 
 points = (GLdouble*) malloc(sizeof(GLdouble)*(*pointsLen));
 memcpy(points,bp,sizeof(GLdouble)*(*pointsLen));
 bp += sizeof(GLdouble)*(*pointsLen); 
 glMap1d(*target, u1, u2, *stride, *order, points);
 free(points);
}


void egl_map1f(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLfloat * u1;
 GLfloat * u2;
 GLint * stride;
 GLint * order;
 GLfloat * points;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 u1 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 u2 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 stride = (GLint *) bp; bp += sizeof(GLint); 
 order = (GLint *) bp; bp += sizeof(GLint); 
 bp += sizeof(int); 
 points = (GLfloat *) bp; 
 glMap1f(*target, *u1, *u2, *stride, *order, points);
}


void egl_map2d(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLdouble u1;
 GLdouble u2;
 GLint * ustride;
 GLint * uorder;
 GLdouble v1;
 GLdouble v2;
 GLint * vstride;
 GLint * vorder;
 GLdouble *points;int * pointsLen;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 memcpy(&u1, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&u2, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 ustride = (GLint *) bp; bp += sizeof(GLint); 
 uorder = (GLint *) bp; bp += sizeof(GLint); 
 memcpy(&v1, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&v2, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 vstride = (GLint *) bp; bp += sizeof(GLint); 
 vorder = (GLint *) bp; bp += sizeof(GLint); 
 pointsLen  = (int *) bp; bp += sizeof(int); 
 points = (GLdouble*) malloc(sizeof(GLdouble)*(*pointsLen));
 memcpy(points,bp,sizeof(GLdouble)*(*pointsLen));
 bp += sizeof(GLdouble)*(*pointsLen); 
 glMap2d(*target, u1, u2, *ustride, *uorder, v1, v2, *vstride, *vorder, points);
 free(points);
}


void egl_map2f(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLfloat * u1;
 GLfloat * u2;
 GLint * ustride;
 GLint * uorder;
 GLfloat * v1;
 GLfloat * v2;
 GLint * vstride;
 GLint * vorder;
 GLfloat * points;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 u1 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 u2 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 ustride = (GLint *) bp; bp += sizeof(GLint); 
 uorder = (GLint *) bp; bp += sizeof(GLint); 
 v1 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 v2 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 vstride = (GLint *) bp; bp += sizeof(GLint); 
 vorder = (GLint *) bp; bp += sizeof(GLint); 
 bp += sizeof(int); 
 points = (GLfloat *) bp; 
 glMap2f(*target, *u1, *u2, *ustride, *uorder, *v1, *v2, *vstride, *vorder, points);
}


void egl_mapGrid1d(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * un;
 GLdouble u1;
 GLdouble u2;
 bp = egl_buff;
 un = (GLint *) bp; bp += sizeof(GLint); 
 memcpy(&u1, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&u2, bp, sizeof(GLdouble)); 
 glMapGrid1d(*un, u1, u2);
}


void egl_mapGrid1f(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * un;
 GLfloat * u1;
 GLfloat * u2;
 bp = egl_buff;
 un = (GLint *) bp; bp += sizeof(GLint); 
 u1 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 u2 = (GLfloat *) bp; 
 glMapGrid1f(*un, *u1, *u2);
}


void egl_mapGrid2d(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * un;
 GLdouble u1;
 GLdouble u2;
 GLint * vn;
 GLdouble v1;
 GLdouble v2;
 bp = egl_buff;
 un = (GLint *) bp; bp += sizeof(GLint); 
 memcpy(&u1, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&u2, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 vn = (GLint *) bp; bp += sizeof(GLint); 
 memcpy(&v1, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&v2, bp, sizeof(GLdouble)); 
 glMapGrid2d(*un, u1, u2, *vn, v1, v2);
}


void egl_mapGrid2f(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * un;
 GLfloat * u1;
 GLfloat * u2;
 GLint * vn;
 GLfloat * v1;
 GLfloat * v2;
 bp = egl_buff;
 un = (GLint *) bp; bp += sizeof(GLint); 
 u1 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 u2 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 vn = (GLint *) bp; bp += sizeof(GLint); 
 v1 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 v2 = (GLfloat *) bp; 
 glMapGrid2f(*un, *u1, *u2, *vn, *v1, *v2);
}


void egl_materialf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * face;
 GLenum * pname;
 GLfloat * param;
 bp = egl_buff;
 face = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLfloat *) bp; 
 glMaterialf(*face, *pname, *param);
}


void egl_materialfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * face;
 GLenum * pname;
 GLfloat * params;
 bp = egl_buff;
 face = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 bp += sizeof(int); 
 params = (GLfloat *) bp; 
 glMaterialfv(*face, *pname, params);
}


void egl_materiali(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * face;
 GLenum * pname;
 GLint * param;
 bp = egl_buff;
 face = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLint *) bp; 
 glMateriali(*face, *pname, *param);
}


void egl_materialiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * face;
 GLenum * pname;
 GLint * params;
 bp = egl_buff;
 face = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 bp += sizeof(int); 
 params = (GLint *) bp; 
 glMaterialiv(*face, *pname, params);
}


void egl_matrixMode(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 bp = egl_buff;
 mode = (GLenum *) bp; 
 glMatrixMode(*mode);
}


void egl_multMatrixd(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble m[16];
 bp = egl_buff;
 memcpy(m,bp,sizeof(GLdouble)*16); 
 glMultMatrixd(m);
}


void egl_multMatrixf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * m;
 bp = egl_buff;
 m = (GLfloat *) bp;
 glMultMatrixf(m);
}


void egl_newList(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * list;
 GLenum * mode;
 bp = egl_buff;
 list = (GLuint *) bp; bp += sizeof(GLuint); 
 mode = (GLenum *) bp; 
 glNewList(*list, *mode);
}


void egl_normal3bv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLbyte * v;
 bp = egl_buff;
 v = (GLbyte *) bp;
 glNormal3bv(v);
}


void egl_normal3dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[3];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*3); 
 glNormal3dv(v);
}


void egl_normal3fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glNormal3fv(v);
}


void egl_normal3iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glNormal3iv(v);
}


void egl_normal3sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glNormal3sv(v);
}


void egl_normalPointer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glNormalPointer(*type, *stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_ortho(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble left;
 GLdouble right;
 GLdouble bottom;
 GLdouble top;
 GLdouble zNear;
 GLdouble zFar;
 bp = egl_buff;
 memcpy(&left, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&right, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&bottom, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&top, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&zNear, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&zFar, bp, sizeof(GLdouble)); 
 glOrtho(left, right, bottom, top, zNear, zFar);
}


void egl_passThrough(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * token;
 bp = egl_buff;
 token = (GLfloat *) bp; 
 glPassThrough(*token);
}


void egl_pixelMapfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * map;
 GLint * mapsize;
 GLfloat * values = NULL; 
 bp = egl_buff;
 map = (GLenum *) bp; bp += sizeof(GLenum); 
 mapsize = (GLint *) bp; bp += sizeof(GLint); 
 values = (GLfloat *) bp;
 glPixelMapfv(*map, *mapsize, values);
}


void egl_pixelMapuiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * map;
 GLint * mapsize;
 GLuint * values = NULL; 
 bp = egl_buff;
 map = (GLenum *) bp; bp += sizeof(GLenum); 
 mapsize = (GLint *) bp; bp += sizeof(GLint); 
 values = (GLuint *) bp;
 glPixelMapuiv(*map, *mapsize, values);
}


void egl_pixelMapusv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * map;
 GLint * mapsize;
 GLushort * values = NULL; 
 bp = egl_buff;
 map = (GLenum *) bp; bp += sizeof(GLenum); 
 mapsize = (GLint *) bp; bp += sizeof(GLint); 
 values = (GLushort *) bp;
 glPixelMapusv(*map, *mapsize, values);
}


void egl_pixelStoref(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLfloat * param;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLfloat *) bp; 
 glPixelStoref(*pname, *param);
}


void egl_pixelStorei(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLint * param;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLint *) bp; 
 glPixelStorei(*pname, *param);
}


void egl_pixelTransferf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLfloat * param;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLfloat *) bp; 
 glPixelTransferf(*pname, *param);
}


void egl_pixelTransferi(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * pname;
 GLint * param;
 bp = egl_buff;
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLint *) bp; 
 glPixelTransferi(*pname, *param);
}


void egl_pixelZoom(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * xfactor;
 GLfloat * yfactor;
 bp = egl_buff;
 xfactor = (GLfloat *) bp; bp += sizeof(GLfloat); 
 yfactor = (GLfloat *) bp; 
 glPixelZoom(*xfactor, *yfactor);
}


void egl_pointSize(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * size;
 bp = egl_buff;
 size = (GLfloat *) bp; 
 glPointSize(*size);
}


void egl_polygonMode(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * face;
 GLenum * mode;
 bp = egl_buff;
 face = (GLenum *) bp; bp += sizeof(GLenum); 
 mode = (GLenum *) bp; 
 glPolygonMode(*face, *mode);
}


void egl_polygonOffset(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * factor;
 GLfloat * units;
 bp = egl_buff;
 factor = (GLfloat *) bp; bp += sizeof(GLfloat); 
 units = (GLfloat *) bp; 
 glPolygonOffset(*factor, *units);
}


void egl_polygonStipple(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLubyte * mask;
 bp = egl_buff;
 mask = (GLubyte *) bp;
 glPolygonStipple(mask);
}


void egl_popAttrib(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 bp = egl_buff;
 glPopAttrib();
}


void egl_popClientAttrib(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 bp = egl_buff;
 glPopClientAttrib();
}


void egl_popMatrix(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 bp = egl_buff;
 glPopMatrix();
}


void egl_popName(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 bp = egl_buff;
 glPopName();
}


void egl_prioritizeTextures(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * n;
 GLuint * textures = NULL; 
 GLclampf * priorities = NULL; 
 bp = egl_buff;
 n = (GLsizei *) bp; bp += sizeof(GLsizei); 
 textures = (GLuint *) bp;
 bp += sizeof(GLuint)*(*n); 
 priorities = (GLclampf *) bp;
 glPrioritizeTextures(*n, textures, priorities);
}


void egl_pushAttrib(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLbitfield * mask;
 bp = egl_buff;
 mask = (GLbitfield *) bp; 
 glPushAttrib(*mask);
}


void egl_pushClientAttrib(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLbitfield * mask;
 bp = egl_buff;
 mask = (GLbitfield *) bp; 
 glPushClientAttrib(*mask);
}


void egl_pushMatrix(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 bp = egl_buff;
 glPushMatrix();
}


void egl_pushName(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * name;
 bp = egl_buff;
 name = (GLuint *) bp; 
 glPushName(*name);
}


void egl_rasterPos2dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[2];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*2); 
 glRasterPos2dv(v);
}


void egl_rasterPos2fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glRasterPos2fv(v);
}


void egl_rasterPos2iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glRasterPos2iv(v);
}


void egl_rasterPos2sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glRasterPos2sv(v);
}


void egl_rasterPos3dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[3];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*3); 
 glRasterPos3dv(v);
}


void egl_rasterPos3fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glRasterPos3fv(v);
}


void egl_rasterPos3iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glRasterPos3iv(v);
}


void egl_rasterPos3sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glRasterPos3sv(v);
}


void egl_rasterPos4dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[4];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*4); 
 glRasterPos4dv(v);
}


void egl_rasterPos4fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glRasterPos4fv(v);
}


void egl_rasterPos4iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glRasterPos4iv(v);
}


void egl_rasterPos4sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glRasterPos4sv(v);
}


void egl_readBuffer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 bp = egl_buff;
 mode = (GLenum *) bp; 
 glReadBuffer(*mode);
}


void egl_readPixels(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * x;
 GLint * y;
 GLsizei * width;
 GLsizei * height;
 GLenum * format;
 GLenum * type;
 GLvoid * pixels = NULL;
 bp = egl_buff;
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 pixels = (GLvoid *) egl_sd->bin[0].base; 
 glReadPixels(*x, *y, *width, *height, *format, *type, pixels);
 sdl_free_binaries(egl_sd);
}


void egl_rectd(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble x1;
 GLdouble y1;
 GLdouble x2;
 GLdouble y2;
 bp = egl_buff;
 memcpy(&x1, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&y1, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&x2, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&y2, bp, sizeof(GLdouble)); 
 glRectd(x1, y1, x2, y2);
}


void egl_rectdv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v1[2];
 GLdouble v2[2];
 bp = egl_buff;
 memcpy(v1,bp,sizeof(GLdouble)*2); 
 bp += sizeof(GLdouble)*(2); 
 memcpy(v2,bp,sizeof(GLdouble)*2); 
 glRectdv(v1, v2);
}


void egl_rectf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * x1;
 GLfloat * y1;
 GLfloat * x2;
 GLfloat * y2;
 bp = egl_buff;
 x1 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 y1 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 x2 = (GLfloat *) bp; bp += sizeof(GLfloat); 
 y2 = (GLfloat *) bp; 
 glRectf(*x1, *y1, *x2, *y2);
}


void egl_rectfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v1;
 GLfloat * v2;
 bp = egl_buff;
 v1 = (GLfloat *) bp;
 bp += sizeof(GLfloat)*(2); 
 v2 = (GLfloat *) bp;
 glRectfv(v1, v2);
}


void egl_recti(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * x1;
 GLint * y1;
 GLint * x2;
 GLint * y2;
 bp = egl_buff;
 x1 = (GLint *) bp; bp += sizeof(GLint); 
 y1 = (GLint *) bp; bp += sizeof(GLint); 
 x2 = (GLint *) bp; bp += sizeof(GLint); 
 y2 = (GLint *) bp; 
 glRecti(*x1, *y1, *x2, *y2);
}


void egl_rectiv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v1;
 GLint * v2;
 bp = egl_buff;
 v1 = (GLint *) bp;
 bp += sizeof(GLint)*(2); 
 v2 = (GLint *) bp;
 glRectiv(v1, v2);
}


void egl_rects(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * x1;
 GLshort * y1;
 GLshort * x2;
 GLshort * y2;
 bp = egl_buff;
 x1 = (GLshort *) bp; bp += sizeof(GLshort); 
 y1 = (GLshort *) bp; bp += sizeof(GLshort); 
 x2 = (GLshort *) bp; bp += sizeof(GLshort); 
 y2 = (GLshort *) bp; 
 glRects(*x1, *y1, *x2, *y2);
}


void egl_rectsv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v1;
 GLshort * v2;
 bp = egl_buff;
 v1 = (GLshort *) bp;
 bp += sizeof(GLshort)*(2); 
 v2 = (GLshort *) bp;
 glRectsv(v1, v2);
}


void egl_renderMode(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 char * egl_start; 
 int egl_sendlen; 
 GLint egl_res; 
 GLenum * mode;
 bp = egl_buff;
 mode = (GLenum *) bp; 
 egl_res =  glRenderMode(*mode);
 bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint) );
 * (GLint *) bp = egl_res;
 bp += sizeof(GLint);
 egl_sendlen = bp - egl_start;
 sdl_send(egl_sd, egl_sendlen);
}


void egl_rotated(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble angle;
 GLdouble x;
 GLdouble y;
 GLdouble z;
 bp = egl_buff;
 memcpy(&angle, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&x, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&y, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&z, bp, sizeof(GLdouble)); 
 glRotated(angle, x, y, z);
}


void egl_rotatef(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * angle;
 GLfloat * x;
 GLfloat * y;
 GLfloat * z;
 bp = egl_buff;
 angle = (GLfloat *) bp; bp += sizeof(GLfloat); 
 x = (GLfloat *) bp; bp += sizeof(GLfloat); 
 y = (GLfloat *) bp; bp += sizeof(GLfloat); 
 z = (GLfloat *) bp; 
 glRotatef(*angle, *x, *y, *z);
}


void egl_scaled(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble x;
 GLdouble y;
 GLdouble z;
 bp = egl_buff;
 memcpy(&x, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&y, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&z, bp, sizeof(GLdouble)); 
 glScaled(x, y, z);
}


void egl_scalef(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * x;
 GLfloat * y;
 GLfloat * z;
 bp = egl_buff;
 x = (GLfloat *) bp; bp += sizeof(GLfloat); 
 y = (GLfloat *) bp; bp += sizeof(GLfloat); 
 z = (GLfloat *) bp; 
 glScalef(*x, *y, *z);
}


void egl_scissor(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * x;
 GLint * y;
 GLsizei * width;
 GLsizei * height;
 bp = egl_buff;
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; 
 glScissor(*x, *y, *width, *height);
}


void egl_selectBuffer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLsizei * size;
 GLvoid * buffer = NULL;
 bp = egl_buff;
 size = (GLsizei *) bp; bp += sizeof(GLsizei); 
 buffer = (GLuint *) egl_sd->bin[0].base; 
 glSelectBuffer(*size, buffer);
 sdl_free_binaries(egl_sd);
}


void egl_shadeModel(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * mode;
 bp = egl_buff;
 mode = (GLenum *) bp; 
 glShadeModel(*mode);
}


void egl_stencilFunc(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * func;
 GLint * ref;
 GLuint * mask;
 bp = egl_buff;
 func = (GLenum *) bp; bp += sizeof(GLenum); 
 ref = (GLint *) bp; bp += sizeof(GLint); 
 mask = (GLuint *) bp; 
 glStencilFunc(*func, *ref, *mask);
}


void egl_stencilMask(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLuint * mask;
 bp = egl_buff;
 mask = (GLuint *) bp; 
 glStencilMask(*mask);
}


void egl_stencilOp(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * fail;
 GLenum * zfail;
 GLenum * zpass;
 bp = egl_buff;
 fail = (GLenum *) bp; bp += sizeof(GLenum); 
 zfail = (GLenum *) bp; bp += sizeof(GLenum); 
 zpass = (GLenum *) bp; 
 glStencilOp(*fail, *zfail, *zpass);
}


void egl_texCoord1dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[1];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*1); 
 glTexCoord1dv(v);
}


void egl_texCoord1fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glTexCoord1fv(v);
}


void egl_texCoord1iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glTexCoord1iv(v);
}


void egl_texCoord1sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glTexCoord1sv(v);
}


void egl_texCoord2dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[2];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*2); 
 glTexCoord2dv(v);
}


void egl_texCoord2fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glTexCoord2fv(v);
}


void egl_texCoord2iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glTexCoord2iv(v);
}


void egl_texCoord2sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glTexCoord2sv(v);
}


void egl_texCoord3dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[3];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*3); 
 glTexCoord3dv(v);
}


void egl_texCoord3fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glTexCoord3fv(v);
}


void egl_texCoord3iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glTexCoord3iv(v);
}


void egl_texCoord3sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glTexCoord3sv(v);
}


void egl_texCoord4dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[4];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*4); 
 glTexCoord4dv(v);
}


void egl_texCoord4fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glTexCoord4fv(v);
}


void egl_texCoord4iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glTexCoord4iv(v);
}


void egl_texCoord4sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glTexCoord4sv(v);
}


void egl_texCoordPointer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glTexCoordPointer(*size, *type, *stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_texEnvf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * pname;
 GLfloat * param;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLfloat *) bp; 
 glTexEnvf(*target, *pname, *param);
}


void egl_texEnvfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glTexEnvfv(*target, *pname, params);
}


void egl_texEnvi(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * pname;
 GLint * param;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLint *) bp; 
 glTexEnvi(*target, *pname, *param);
}


void egl_texEnviv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glTexEnviv(*target, *pname, params);
}


void egl_texGend(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * coord;
 GLenum * pname;
 GLdouble param;
 bp = egl_buff;
 coord = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 memcpy(&param, bp, sizeof(GLdouble)); 
 glTexGend(*coord, *pname, param);
}


void egl_texGendv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * coord;
 GLenum * pname;
 GLdouble params[4];
 bp = egl_buff;
 coord = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 memcpy(params,bp,sizeof(GLdouble)*4); 
 glTexGendv(*coord, *pname, params);
}


void egl_texGenf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * coord;
 GLenum * pname;
 GLfloat * param;
 bp = egl_buff;
 coord = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLfloat *) bp; 
 glTexGenf(*coord, *pname, *param);
}


void egl_texGenfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * coord;
 GLenum * pname;
 GLfloat * params;
 bp = egl_buff;
 coord = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 params = (GLfloat *) bp;
 glTexGenfv(*coord, *pname, params);
}


void egl_texGeni(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * coord;
 GLenum * pname;
 GLint * param;
 bp = egl_buff;
 coord = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLint *) bp; 
 glTexGeni(*coord, *pname, *param);
}


void egl_texGeniv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * coord;
 GLenum * pname;
 GLint * params;
 bp = egl_buff;
 coord = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 params = (GLint *) bp;
 glTexGeniv(*coord, *pname, params);
}


void egl_texImage1D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * internalformat;
 GLsizei * width;
 GLint * border;
 GLenum * format;
 GLenum * type;
 GLvoid * pixels = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 internalformat = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 border = (GLint *) bp; bp += sizeof(GLint); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 if(egl_sd->next_bin == 0) {
  pixels = (GLvoid *) *(GLint *)bp;
 } else {
  pixels = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 glTexImage1D(*target, *level, *internalformat, *width, *border, *format, *type, pixels);
 sdl_free_binaries(egl_sd);
}


void egl_texImage2D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * internalformat;
 GLsizei * width;
 GLsizei * height;
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
 border = (GLint *) bp; bp += sizeof(GLint); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 if(egl_sd->next_bin == 0) {
  pixels = (GLvoid *) *(GLint *)bp;
 } else {
  pixels = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 glTexImage2D(*target, *level, *internalformat, *width, *height, *border, *format, *type, pixels);
 sdl_free_binaries(egl_sd);
}


void egl_texParameterf(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * pname;
 GLfloat * param;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLfloat *) bp; 
 glTexParameterf(*target, *pname, *param);
}


void egl_texParameterfv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glTexParameterfv(*target, *pname, params);
}


void egl_texParameteri(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLenum * pname;
 GLint * param;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 pname = (GLenum *) bp; bp += sizeof(GLenum); 
 param = (GLint *) bp; 
 glTexParameteri(*target, *pname, *param);
}


void egl_texParameteriv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glTexParameteriv(*target, *pname, params);
}


void egl_texSubImage1D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * xoffset;
 GLsizei * width;
 GLenum * format;
 GLenum * type;
 GLvoid * pixels = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 xoffset = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 if(egl_sd->next_bin == 0) {
  pixels = (GLvoid *) *(GLint *)bp;
 } else {
  pixels = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 glTexSubImage1D(*target, *level, *xoffset, *width, *format, *type, pixels);
 sdl_free_binaries(egl_sd);
}


void egl_texSubImage2D(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLenum * target;
 GLint * level;
 GLint * xoffset;
 GLint * yoffset;
 GLsizei * width;
 GLsizei * height;
 GLenum * format;
 GLenum * type;
 GLvoid * pixels = NULL;
 bp = egl_buff;
 target = (GLenum *) bp; bp += sizeof(GLenum); 
 level = (GLint *) bp; bp += sizeof(GLint); 
 xoffset = (GLint *) bp; bp += sizeof(GLint); 
 yoffset = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; bp += sizeof(GLsizei); 
 format = (GLenum *) bp; bp += sizeof(GLenum); 
 type = (GLenum *) bp; bp += sizeof(GLenum); 
 if(egl_sd->next_bin == 0) {
  pixels = (GLvoid *) *(GLint *)bp;
 } else {
  pixels = (GLvoid *) egl_sd->bin[0].base;
 };
 bp += sizeof(GLint);
 glTexSubImage2D(*target, *level, *xoffset, *yoffset, *width, *height, *format, *type, pixels);
 sdl_free_binaries(egl_sd);
}


void egl_translated(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble x;
 GLdouble y;
 GLdouble z;
 bp = egl_buff;
 memcpy(&x, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&y, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
 memcpy(&z, bp, sizeof(GLdouble)); 
 glTranslated(x, y, z);
}


void egl_translatef(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * x;
 GLfloat * y;
 GLfloat * z;
 bp = egl_buff;
 x = (GLfloat *) bp; bp += sizeof(GLfloat); 
 y = (GLfloat *) bp; bp += sizeof(GLfloat); 
 z = (GLfloat *) bp; 
 glTranslatef(*x, *y, *z);
}


void egl_vertex2dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[2];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*2); 
 glVertex2dv(v);
}


void egl_vertex2fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glVertex2fv(v);
}


void egl_vertex2iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glVertex2iv(v);
}


void egl_vertex2sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glVertex2sv(v);
}


void egl_vertex3dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[3];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*3); 
 glVertex3dv(v);
}


void egl_vertex3fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glVertex3fv(v);
}


void egl_vertex3iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glVertex3iv(v);
}


void egl_vertex3sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glVertex3sv(v);
}


void egl_vertex4dv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLdouble v[4];
 bp = egl_buff;
 memcpy(v,bp,sizeof(GLdouble)*4); 
 glVertex4dv(v);
}


void egl_vertex4fv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLfloat * v;
 bp = egl_buff;
 v = (GLfloat *) bp;
 glVertex4fv(v);
}


void egl_vertex4iv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * v;
 bp = egl_buff;
 v = (GLint *) bp;
 glVertex4iv(v);
}


void egl_vertex4sv(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLshort * v;
 bp = egl_buff;
 v = (GLshort *) bp;
 glVertex4sv(v);
}


void egl_vertexPointer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
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
 glVertexPointer(*size, *type, *stride, pointer);
 sdl_free_binaries(egl_sd);
}


void egl_viewport(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
 char * bp; 
 GLint * x;
 GLint * y;
 GLsizei * width;
 GLsizei * height;
 bp = egl_buff;
 x = (GLint *) bp; bp += sizeof(GLint); 
 y = (GLint *) bp; bp += sizeof(GLint); 
 width = (GLsizei *) bp; bp += sizeof(GLsizei); 
 height = (GLsizei *) bp; 
 glViewport(*x, *y, *width, *height);
}


