/*  
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id$
 *
 *  
 *  GLU wrapper 
 *
 */

#include <stdio.h>  /* malloc */
#include <stdlib.h>  /* malloc */
#include <string.h>  /* malloc */
#include <math.h>  /* malloc */
#ifdef FLAVOUR_WOGGLE
#include <woggle_driver.h>
#else
#include "esdl.h"
#endif
#ifdef _OSX_COCOA
#include <OpenGL/glu.h>
#else
#include <GL/glu.h>
#endif
#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

#define GetNativePtr(res, T, s) \
do { res = T *(void **)(s); s += sizeof(void *); } while (0)

typedef struct _tessdata3 * eglu_tessdata_ptr;

typedef struct _tessdata3 {
		 eglu_tessdata_ptr next;
		 GLdouble data[3];
		} eglu_tessdata; 

typedef struct _tessobj {   
		 GLUtesselator *tess;
		 eglu_tessdata *data;
		 GLdouble* freep;
		 GLdouble def_heap[64];
		} eglu_tessobj;

#ifndef CALLBACK
# define CALLBACK
#endif

#define ESDL_TESSCB_NONE        0       /* No callback                         */
#define ESDL_TESSCB_GLBEGIN     1       /* calls gl:begin(Type)                */
#define ESDL_TESSCB_GLEND       2       /* calls gl:end()                      */
#define ESDL_TESSCB_GLVERTEX    3       /* calls gl:3dv(vertex)                */
#define ESDL_TESSCB_ERROR_PRINT 4       /* Prints error on stderr.             */
#define ESDL_TESSCB_COMBINE     5       /* Adds vertex                   */
#define ESDL_TESSCB_GLEDGEFLAG  6       /* calls gl:edgeFlag(Flag)       */

#define ESDL_TESSCB_UDATA_VERTEX  7 /* calls various gl functions
				     * depending on user data
				     */
#define ESDL_TESSCB_NOP          8       /* does nothing */

#define ESDL_TESS_VTXDATA_MATERIAL     1
#define ESDL_TESS_VTXDATA_TEXCOORD2    2
#define ESDL_TESS_VTXDATA_NORMAL       4
#define ESDL_TESS_VTXDATA_COLOR        8

void CALLBACK esdl_nop_callback(GLboolean flag)
{
}

void CALLBACK errorCallback(GLenum errorCode)
{
   const GLubyte *err;
   err = gluErrorString(errorCode);
   fprintf(stderr, "Tesselation error: %d: %s\r\n", (int)errorCode, err);
}

void CALLBACK esdl_combine(GLdouble coords[3], 
			   void *vertex_data[4],
			   GLfloat w[4], 
			   void **outData,
			   void *polygon_data
			  )
{
   int size = 0;
   int max = 0;
   eglu_tessobj *eobj = polygon_data;
   eglu_tessdata* mycoords = NULL; 
   unsigned char* combined  = NULL; 
   unsigned char* datap[4] = {NULL, NULL, NULL, NULL};
   unsigned flags;

   flags = ((unsigned char *)vertex_data[0])[-1];
   for (max = 0; max < 4 && vertex_data[max] != NULL; max++) {
      if (((unsigned char *)vertex_data[max])[-1] != flags) {
	 flags = 0;
      }
      datap[max] = (unsigned char *) (((GLdouble *)vertex_data[max])+3);
   }

   if (flags & ESDL_TESS_VTXDATA_MATERIAL) {
      size += 5*4;
   }
   if (flags & ESDL_TESS_VTXDATA_TEXCOORD2) {
      size += 2*4;
   }
   if (flags & ESDL_TESS_VTXDATA_NORMAL) {
      size += 3*4;
   }
   if (flags & ESDL_TESS_VTXDATA_COLOR) {
      size += 2*4;
   }

   mycoords = (eglu_tessdata*) malloc(sizeof(GLdouble) + 
				      sizeof(eglu_tessdata) + size);
   combined = (unsigned char *) (mycoords->data+4);

   mycoords->next = eobj->data;
   eobj->data = mycoords;
   
   mycoords->data[1] = coords[0];
   mycoords->data[2] = coords[1];
   mycoords->data[3] = coords[2];
   
   *outData = mycoords->data + 1;
   flags = 0;
   ((char *) (mycoords->data+1))[-1] = flags;
}

void CALLBACK
esdl_udata_vertex(GLdouble* coords)
{
   GLfloat* datap = (GLfloat *) (coords+3);
   unsigned flags = ((unsigned char *) coords)[-1];

   /*   fprintf(stderr, "Flags: %d\r\n", flags); */
   if (flags & ESDL_TESS_VTXDATA_MATERIAL) {
      GLenum face = ((unsigned short *) datap)[0];
      GLenum pname = ((unsigned short *) datap)[1];
      datap++;
      glMaterialfv(face, pname, datap);
      datap += 4;
   }
   if (flags & ESDL_TESS_VTXDATA_TEXCOORD2) {
      glTexCoord2fv(datap);
      datap += 2;
   }
   if (flags & ESDL_TESS_VTXDATA_NORMAL) {
      glNormal3fv(datap);
      datap += 3;
   }
   if (flags & ESDL_TESS_VTXDATA_COLOR) {
      glColor4fv(datap);
      datap += 4;
   }
   glVertex3dv(coords);
}



void eglu_newTess (sdl_data *sd, int len, char * buff) 
{
   char *bp, *start;
   int sendlen; 
   GLUtesselator* tobj;
   eglu_tessobj * eobj;
    
   tobj = gluNewTess();
   eobj = (eglu_tessobj *) malloc(sizeof(eglu_tessobj));
   eobj->tess = tobj;
   eobj->data = NULL;
   eobj->freep = eobj->def_heap;

/*    fprintf(stderr, "New tess: %d -> %d \r\n", (int) eobj, (int) tobj);  */

   /*
    * Send back result.
    */
   bp = start = sdl_get_temp_buff(sd, 4);
   /* putPointer(bp, eobj); */
   * (eglu_tessobj **) bp = eobj;
   bp += sizeof(eglu_tessobj *);

   sendlen = bp - start;
   sdl_send(sd, sendlen);
}

void eglu_deleteTess (sdl_data *sd, int len, char * buff) 
{
   char *bp;
   eglu_tessobj *eobj;
   bp = buff;
   GetNativePtr(eobj, (eglu_tessobj *), bp);
   gluDeleteTess(eobj->tess);
   free(eobj);
/*    fprintf(stderr, "Deleting tess: %d\r\n", (int) eobj); */
}

void eglu_tessBeginPolygon (sdl_data *sd, int len, char* bp) 
{
   eglu_tessobj *eobj;

   GetNativePtr(eobj, (eglu_tessobj *), bp);
   gluTessBeginPolygon(eobj->tess, (void *) eobj);
}

void eglu_tessVertex(sdl_data *sd, int len, char* bp) 
{
   eglu_tessobj *eobj;
   eglu_tessdata* coords; 
   size_t size;
   size_t ndoubles;
   size_t nbytes;
   char* extrap;

   GetNativePtr(eobj, (eglu_tessobj *), bp);
   size = len - sizeof(void *);
   nbytes = sizeof(GLdouble) + sizeof(eglu_tessdata) + size;
   ndoubles = (nbytes-1)/sizeof(GLdouble) + 1;
   if (ndoubles <= eobj->def_heap+ASIZE(eobj->def_heap)-eobj->freep) {
      coords = (eglu_tessdata*) eobj->freep;
      eobj->freep += ndoubles;
   } else {
      coords = (eglu_tessdata*) malloc(nbytes);
      coords->next = eobj->data;
      eobj->data = coords;
   }
   memcpy(coords->data+1, bp, size);
   extrap = (char *) (coords->data+1);
   if (size > 3*sizeof(GLdouble)) {
      extrap[-1] = bp[size-1];
   } else {
      extrap[-1] = 0;
   }
/*     fprintf(stderr, "tessVertex: %d %g %g %g\r\n", */
/* 	    (int) eobj, coords->data[0], coords->data[1], coords->data[2]); */
    gluTessVertex(eobj->tess, coords->data+1, coords->data+1);
}

void eglu_tessEndPolygon (sdl_data *sd, int len, char * buff) 
{
   char *bp;
   eglu_tessobj *eobj;
   eglu_tessdata *remove, *temp;

   bp = buff;
   GetNativePtr(eobj, (eglu_tessobj *), bp);
   gluTessEndPolygon(eobj->tess);
   remove = eobj->data;
   while (remove != NULL) {
      temp = remove->next;
      free(remove);
      remove = temp;
   }
   eobj->data = NULL;
   eobj->freep = eobj->def_heap;

/*    fprintf(stderr, "End Polygon: %d\r\n", (int) eobj); */

}

void eglu_tessCallback(sdl_data *sd, int len, char * buff) 
{
   char *bp;
   eglu_tessobj *eobj;
   GLenum *which;
   GLint cbId;
   GLvoid (CALLBACK *cbfn)();
   bp = buff;
   GetNativePtr(eobj, (eglu_tessobj *), bp);
   which = (GLenum *) bp; bp += sizeof(GLenum); 
   cbId = * (GLint *) bp; bp += sizeof(GLint); 

   switch(*which) {
   case GLU_TESS_COMBINE:
      *which = GLU_TESS_COMBINE_DATA;
      break;      
   default:
      break;
   };
      
   switch (cbId) {
   case ESDL_TESSCB_NONE:
      cbfn = NULL;
      break;
   case ESDL_TESSCB_GLBEGIN:
      cbfn = (GLvoid (CALLBACK *)()) glBegin;
      break;
   case ESDL_TESSCB_GLEND:
      cbfn = (GLvoid (CALLBACK *)()) glEnd;
      break;
   case ESDL_TESSCB_GLVERTEX:
      cbfn = (GLvoid (CALLBACK *)()) glVertex3dv;
      break;
   case ESDL_TESSCB_GLEDGEFLAG:
      cbfn = (GLvoid (CALLBACK *)()) glEdgeFlag;
      break;
   case ESDL_TESSCB_ERROR_PRINT:
      cbfn = (GLvoid (CALLBACK *)()) errorCallback;
      break;
   case ESDL_TESSCB_COMBINE:
      cbfn = (GLvoid (CALLBACK *)()) esdl_combine;
      break;
   case ESDL_TESSCB_UDATA_VERTEX:
      cbfn = (GLvoid (CALLBACK *)()) esdl_udata_vertex;
      break;
   case ESDL_TESSCB_NOP:
      cbfn = (GLvoid (CALLBACK *)()) esdl_nop_callback;
      break;
   default:
       cbfn = NULL;
       break;
   };
   gluTessCallback(eobj->tess, *which, cbfn);
/*    fprintf(stderr, "Tess Callback: %d %d\r\n", (int) eobj, *cbId); */
}
void eglu_beginCurve(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUnurbs * nurb;
   bp = egl_buff;
   GetNativePtr(nurb, (GLUnurbs *),bp);
   gluBeginCurve(nurb);

}

void eglu_beginSurface(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUnurbs * nurb;
   bp = egl_buff;
   GetNativePtr(nurb, (GLUnurbs *),bp);
   gluBeginSurface(nurb);

}

void eglu_beginTrim(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUnurbs * nurb;
   bp = egl_buff;
   GetNativePtr(nurb, (GLUnurbs *),bp);
   gluBeginTrim(nurb);

}

void eglu_build1DMipmaps(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
  char * bp; 
  char * egl_start; 
  int egl_sendlen; 
  GLint egl_res; 
  GLenum * target;
  GLint * internalFormat;
  GLsizei * width;
  GLenum * format;
  GLenum * type;
  bp = egl_buff;
  target = (GLenum *) bp; bp += sizeof(GLenum); 
  internalFormat = (GLint *) bp; bp += sizeof(GLint); 
  width = (GLsizei *) bp; bp += sizeof(GLsizei); 
  format = (GLenum *) bp; bp += sizeof(GLenum); 
  type = (GLenum *) bp; bp += sizeof(GLenum); 
  if (egl_sd->next_bin == 1) {
    GLvoid* data = egl_sd->bin[0].base;
    egl_res =  gluBuild1DMipmaps(*target, *internalFormat, *width, *format, *type, data);
    sdl_free_binaries(egl_sd);
    bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint));
    * (GLint *) bp = egl_res;
    bp += sizeof(GLint);
    egl_sendlen = bp - egl_start;
    sdl_send(egl_sd, egl_sendlen);
  }
}

void eglu_build2DMipmaps(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   char * egl_start; 
   int egl_sendlen; 
   GLint egl_res; 
   GLenum * target;
   GLint * internalFormat;
   GLsizei * width;
   GLsizei * height;
   GLenum * format;
   GLenum * type;
   bp = egl_buff;
   target = (GLenum *) bp; bp += sizeof(GLenum); 
   internalFormat = (GLint *) bp; bp += sizeof(GLint); 
   width = (GLsizei *) bp; bp += sizeof(GLsizei); 
   height = (GLsizei *) bp; bp += sizeof(GLsizei); 
   format = (GLenum *) bp; bp += sizeof(GLenum); 
   type = (GLenum *) bp; bp += sizeof(GLenum); 
   if (egl_sd->next_bin == 1) {
     GLvoid* data = egl_sd->bin[0].base;
     egl_res = gluBuild2DMipmaps(*target, *internalFormat, *width, 
				 *height, *format, *type, data);
     sdl_free_binaries(egl_sd);
     bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint));
     * (GLint *) bp = egl_res;
     bp += sizeof(GLint);
     egl_sendlen = bp - egl_start;
     sdl_send(egl_sd, egl_sendlen);
   }
}

void eglu_cylinder(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUquadric * quad;
   GLdouble arg[3];
   GLint * slices;
   GLint * stacks;
   bp = egl_buff;
   GetNativePtr(quad, (GLUquadric *),bp);
   memcpy(arg, bp, sizeof(arg)); bp += sizeof(arg);
   slices = (GLint *) bp; bp += sizeof(GLint); 
   stacks = (GLint *) bp;
   gluCylinder(quad, arg[0], arg[1], arg[2], *slices, *stacks);
}

void eglu_deleteNurbsRenderer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUnurbs * nurb;
   bp = egl_buff;
   GetNativePtr(nurb, (GLUnurbs *),bp);
   gluDeleteNurbsRenderer(nurb);
}

void eglu_deleteQuadric(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUquadric * quad;
   bp = egl_buff;
   GetNativePtr(quad, (GLUquadric *),bp);
   gluDeleteQuadric(quad);
}

void eglu_disk(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUquadric * quad;
   GLdouble inner;
   GLdouble outer;
   GLint * slices;
   GLint * loops;
   bp = egl_buff;
   GetNativePtr(quad, (GLUquadric *),bp);
   memcpy(&inner, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
   memcpy(&outer, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
   slices = (GLint *) bp; bp += sizeof(GLint); 
   loops = (GLint *) bp; bp += sizeof(GLint); 
   gluDisk(quad, inner, outer, *slices, *loops);
}

void eglu_endCurve(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUnurbs * nurb;
   bp = egl_buff;
   GetNativePtr(nurb, (GLUnurbs *),bp);
   gluEndCurve(nurb);
}

void eglu_endSurface(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUnurbs * nurb;
   bp = egl_buff;
   GetNativePtr(nurb, (GLUnurbs *),bp);
   gluEndSurface(nurb);
}

void eglu_endTrim(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUnurbs * nurb;
   bp = egl_buff;
   GetNativePtr(nurb, (GLUnurbs *),bp);
   gluEndTrim(nurb);
}

void eglu_errorString(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   char * egl_start; 
   int egl_sendlen; 
   const GLubyte *egl_res; 
   GLenum * error;
   bp = egl_buff;
   error = (GLenum *) bp; bp += sizeof(GLenum); 
   egl_res = gluErrorString(*error);
   bp = egl_start = sdl_get_temp_buff(egl_sd, strlen(egl_res));
   strcpy((GLubyte *)bp, egl_res);
   bp += strlen(egl_res);
   egl_sendlen = bp - egl_start;
   sdl_send(egl_sd, egl_sendlen);
}

void eglu_getNurbsProperty(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   char * egl_start; 
   int egl_sendlen; 
   GLUnurbs * nurb;
   GLenum * property;
   GLfloat data[1]; 
   bp = egl_buff;
   GetNativePtr(nurb, (GLUnurbs *),bp);
   property = (GLenum *) bp; bp += sizeof(GLenum); 
   gluGetNurbsProperty(nurb, *property, data);
   bp = egl_start = sdl_getbuff(egl_sd, sizeof(GLfloat) *1);
   * (GLfloat *)bp = data[0]; bp += sizeof(GLfloat);
   egl_sendlen = bp - egl_start;
   sdl_send(egl_sd, egl_sendlen);
}

void eglu_getString(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   char * egl_start; 
   int egl_sendlen; 
   const GLubyte *egl_res; 
   GLenum * name;
   bp = egl_buff;
   name = (GLenum *) bp; bp += sizeof(GLenum); 
   egl_res = gluGetString(*name);
   bp = egl_start = sdl_get_temp_buff(egl_sd, strlen(egl_res));
   strcpy((GLubyte *)bp, egl_res);
   bp += strlen(egl_res);
   egl_sendlen = bp - egl_start;
   sdl_send(egl_sd, egl_sendlen);
}

void eglu_getTessProperty(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   char * egl_start; 
   int egl_sendlen; 
   eglu_tessobj * tess;
   GLenum * which;
   GLdouble data[1]; 
   bp = egl_buff;
   GetNativePtr(tess, (eglu_tessobj *),bp);
   which = (GLenum *) bp; bp += sizeof(GLenum); 
   gluGetTessProperty(tess->tess, *which, data);
   bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLdouble) *1);
   * (GLdouble *)bp = data[0]; bp += sizeof(GLdouble);
   egl_sendlen = bp - egl_start;
   sdl_send(egl_sd, egl_sendlen);
}

void eglu_loadSamplingMatrices(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUnurbs * nurb;
   GLfloat * model;
   GLfloat * perspective;
   GLint * view;
   bp = egl_buff;
   GetNativePtr(nurb, (GLUnurbs *),bp);
   model = (GLfloat *) bp; bp += sizeof(GLfloat)*16; 
   perspective = (GLfloat *) bp; bp += sizeof(GLfloat)*16; 
   view = (GLint *) bp; bp += sizeof(GLint)*16; 
   gluLoadSamplingMatrices(nurb, model, perspective, view);
}

void eglu_lookAt(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLdouble data[9];
   bp = egl_buff;
   memcpy(data, bp, sizeof(GLdouble)*9); bp += sizeof(GLdouble)*9; 
   gluLookAt(data[0],data[1],data[2],
	     data[3],data[4],data[5],
	     data[6],data[7],data[8]);
}

void eglu_newNurbsRenderer(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   char * egl_start; 
   int egl_sendlen; 
   GLUnurbs *egl_res; 
   bp = egl_buff;
   egl_res =  gluNewNurbsRenderer();
   bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLUnurbs*));
   * (GLUnurbs **) bp = egl_res;
   bp += sizeof(GLUnurbs *);
   egl_sendlen = bp - egl_start;
   sdl_send(egl_sd, egl_sendlen);
}

void eglu_newQuadric(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   char * egl_start; 
   int egl_sendlen; 
   GLUquadric *egl_res; 
   bp = egl_buff;
   egl_res =  gluNewQuadric();
   bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLUquadric*));
   * (GLUquadric **) bp = egl_res;
   bp += sizeof(GLUquadric *);
   egl_sendlen = bp - egl_start;
   sdl_send(egl_sd, egl_sendlen);
}

void eglu_nurbsCurve(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
  char * bp; 
  GLUnurbs * nurb;
  GLint knotCount;
  GLfloat * knots = NULL;
  GLint * stride;
  GLfloat * control = NULL;
  GLint * order;
  GLenum * type;
  bp = egl_buff;
  GetNativePtr(nurb, (GLUnurbs *),bp);
  knotCount = * (GLint *) bp; bp += sizeof(GLint); 
  knots = (GLfloat *) bp; bp += sizeof(GLfloat)*(knotCount); 
  stride = (GLint *) bp; bp += sizeof(GLint); 
  order = (GLint *) bp; bp += sizeof(GLint); 
  type = (GLenum *) bp; bp += sizeof(GLenum); 
  if (egl_sd->next_bin == 1) {
    control = (GLfloat *) egl_sd->bin[0].base;
    gluNurbsCurve(nurb, knotCount, knots, *stride, control, *order, *type);
    sdl_free_binaries(egl_sd);
  }
}

void eglu_nurbsProperty(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUnurbs * nurb;
   GLenum * property;
   GLfloat * value;
   bp = egl_buff;
   GetNativePtr(nurb, (GLUnurbs *),bp);
   property = (GLenum *) bp; bp += sizeof(GLenum); 
   value = (GLfloat *) bp; bp += sizeof(GLfloat); 
   gluNurbsProperty(nurb, *property, *value);
}

void eglu_nurbsSurface(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
  char * bp; 
  GLUnurbs * nurb;
  GLint sKnotCount;
  GLfloat * sKnots = NULL;
  GLint tKnotCount;
  GLfloat * tKnots = NULL;
  GLint * sStride;
  GLint * tStride;
  GLfloat * control = NULL;
  GLint * sOrder;
  GLint * tOrder;
  GLenum * type;
  bp = egl_buff;
  GetNativePtr(nurb, (GLUnurbs *),bp);
  sKnotCount = * (GLint *) bp; bp += sizeof(GLint); 
  sKnots = (GLfloat *) bp; bp += sizeof(GLfloat)*(sKnotCount);
  tKnotCount = *(GLint *) bp; bp += sizeof(GLint); 
  tKnots = (GLfloat *) bp; bp += sizeof(GLfloat)*(tKnotCount); 
  sStride = (GLint *) bp; bp += sizeof(GLint); 
  tStride = (GLint *) bp; bp += sizeof(GLint); 
  sOrder = (GLint *) bp; bp += sizeof(GLint); 
  tOrder = (GLint *) bp; bp += sizeof(GLint); 
  type = (GLenum *) bp; bp += sizeof(GLenum); 
  if (egl_sd->next_bin == 1) {
    control = (GLfloat *) egl_sd->bin[0].base;
    gluNurbsSurface(nurb, sKnotCount, sKnots, tKnotCount, tKnots,
		    *sStride, *tStride, control, *sOrder, *tOrder, *type);
    sdl_free_binaries(egl_sd);
  }
}

void eglu_ortho2D(sdl_data *egl_sd, int egl_len, char *bp) 
{
  GLdouble arg[4];
  memcpy(arg, bp, sizeof(arg));
  gluOrtho2D(arg[0], arg[1], arg[2], arg[3]);
}

void eglu_partialDisk(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUquadric * quad;
   GLdouble inner;
   GLdouble outer;
   GLint * slices;
   GLint * loops;
   GLdouble start;
   GLdouble sweep;
   bp = egl_buff;
   GetNativePtr(quad, (GLUquadric *),bp);
   memcpy(&inner, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
   memcpy(&outer, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
   slices = (GLint *) bp; bp += sizeof(GLint); 
   loops = (GLint *) bp; bp += sizeof(GLint); 
   memcpy(&start, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
   memcpy(&sweep, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
   gluPartialDisk(quad, inner, outer, *slices, *loops, start, sweep);
}

void eglu_perspective(sdl_data *egl_sd, int egl_len, char *bp) 
{
  GLdouble arg[4];

  memcpy(arg, bp, sizeof(arg));
  gluPerspective(arg[0], arg[1], arg[2], arg[3]);
}

void eglu_pickMatrix(sdl_data *egl_sd, int egl_len, char *bp) 
{
  GLdouble arg[4];
  GLint* viewport;

  memcpy(arg, bp, sizeof(arg)); bp += sizeof(arg);
  viewport = (GLint *) bp;
  gluPickMatrix(arg[0], arg[1], arg[2], arg[3], viewport);
}

void eglu_project(sdl_data *egl_sd, int egl_len, char *bp) 
{
  GLint egl_res; 
  GLdouble arg[3+16+16];
  GLint* view;
  GLdouble res[3];

  memcpy(arg, bp, sizeof(arg)); bp += sizeof(arg);
  view = (GLint *) bp;
  egl_res = gluProject(arg[0], arg[1], arg[2], arg+3, arg+3+16, view,
		       res+0, res+1, res+2);
  if (egl_res) {
    bp = sdl_get_temp_buff(egl_sd, sizeof(res));
    memcpy(bp, res, sizeof(res));
    sdl_send(egl_sd, sizeof(res));
  }
}

void eglu_pwlCurve(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
  char * bp; 
  GLUnurbs * nurb;
  GLint * count;
  GLfloat * data = NULL;
  GLint * stride;
  GLenum * type;
  bp = egl_buff;
  GetNativePtr(nurb, (GLUnurbs *),bp);
  count = (GLint *) bp; bp += sizeof(GLint); 
  stride = (GLint *) bp; bp += sizeof(GLint); 
  type = (GLenum *) bp; bp += sizeof(GLenum); 
  gluPwlCurve(nurb, *count, data, *stride, *type);
  if (egl_sd->next_bin == 1) {
    data = (GLfloat *) egl_sd->bin[0].base;
    gluPwlCurve(nurb, *count, data, *stride, *type);
    sdl_free_binaries(egl_sd);
  }
}

void eglu_quadricDrawStyle(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUquadric * quad;
   GLenum * draw;
   bp = egl_buff;
   GetNativePtr(quad, (GLUquadric *),bp);
   draw = (GLenum *) bp; bp += sizeof(GLenum); 
   gluQuadricDrawStyle(quad, *draw);
}

void eglu_quadricNormals(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUquadric * quad;
   GLenum * normal;
   bp = egl_buff;
   GetNativePtr(quad, (GLUquadric *),bp);
   normal = (GLenum *) bp; bp += sizeof(GLenum); 
   gluQuadricNormals(quad, *normal);
}

void eglu_quadricOrientation(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUquadric * quad;
   GLenum * orientation;
   bp = egl_buff;
   GetNativePtr(quad, (GLUquadric *),bp);
   orientation = (GLenum *) bp; bp += sizeof(GLenum); 
   gluQuadricOrientation(quad, *orientation);
}

void eglu_quadricTexture(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUquadric * quad;
   GLboolean * texture;
   bp = egl_buff;
   GetNativePtr(quad, (GLUquadric *),bp);
   texture = (GLboolean *) bp; bp += sizeof(GLboolean); 
   gluQuadricTexture(quad, *texture);
}

void eglu_scaleImage(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
  char * bp; 
  char * egl_start; 
  int egl_sendlen; 
  GLint egl_res; 
  GLenum * format;
  GLsizei * wIn;
  GLsizei * hIn;
  GLenum * typeIn;
  GLsizei * wOut;
  GLsizei * hOut;
  GLenum * typeOut;
  bp = egl_buff;
  format = (GLenum *) bp; bp += sizeof(GLenum); 
  wIn = (GLsizei *) bp; bp += sizeof(GLsizei); 
  hIn = (GLsizei *) bp; bp += sizeof(GLsizei); 
  typeIn = (GLenum *) bp; bp += sizeof(GLenum); 
  wOut = (GLsizei *) bp; bp += sizeof(GLsizei); 
  hOut = (GLsizei *) bp; bp += sizeof(GLsizei); 
  typeOut = (GLenum *) bp; bp += sizeof(GLenum); 
  if (egl_sd->next_bin == 2) {
    void* dataIn = egl_sd->bin[0].base;
    void* dataOut = egl_sd->bin[1].base;
    egl_res =  gluScaleImage(*format, *wIn, *hIn, *typeIn, dataIn,
			     *wOut, *hOut, *typeOut, dataOut);
    sdl_free_binaries(egl_sd);
    bp = egl_start = sdl_get_temp_buff(egl_sd, sizeof(GLint));
    * (GLint *) bp = egl_res;
    bp += sizeof(GLint);
    egl_sendlen = bp - egl_start;
    sdl_send(egl_sd, egl_sendlen);
  }
}

void eglu_sphere(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   GLUquadric * quad;
   GLdouble radius;
   GLint * slices;
   GLint * stacks;
   bp = egl_buff;
   GetNativePtr(quad, (GLUquadric *),bp);
   memcpy(&radius, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
   slices = (GLint *) bp; bp += sizeof(GLint); 
   stacks = (GLint *) bp; bp += sizeof(GLint); 
   gluSphere(quad, radius, *slices, *stacks);
}

void eglu_tessBeginContour(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   eglu_tessobj * tess;
   bp = egl_buff;
   GetNativePtr(tess, (eglu_tessobj *),bp);
   gluTessBeginContour(tess->tess);
}

void eglu_tessEndContour(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   eglu_tessobj * tess;
   bp = egl_buff;
   GetNativePtr(tess, (eglu_tessobj *),bp);
   gluTessEndContour(tess->tess);
/*    fprintf(stderr, "Begin Contour: %d\r\n", (int) tess); */
}


void eglu_tessNormal(sdl_data *egl_sd, int egl_len, char *bp) 
{
   eglu_tessobj* tess;
   GLdouble values[3];

   GetNativePtr(tess, (eglu_tessobj *), bp);
   memcpy(values, bp, sizeof(values));
   gluTessNormal(tess->tess, values[0], values[1], values[2]);
}

void eglu_tessProperty(sdl_data *egl_sd, int egl_len, char *egl_buff) 
{
   char * bp; 
   eglu_tessobj * tess;
   GLenum * which;
   GLdouble data;
   bp = egl_buff;
   GetNativePtr(tess, (eglu_tessobj *),bp);
   which = (GLenum *) bp; bp += sizeof(GLenum); 
   memcpy(&data, bp, sizeof(GLdouble)); bp += sizeof(GLdouble); 
   gluTessProperty(tess->tess, *which, data);
}

void eglu_unProject(sdl_data *egl_sd, int egl_len, char *bp) 
{
  GLint egl_res; 
  GLdouble arg[3+16+16];
  GLdouble res[3];
  GLint* view;

  memcpy(arg, bp, sizeof(arg)); bp += sizeof(arg);
  view = (GLint *) bp;
  egl_res =  gluUnProject(arg[0], arg[1], arg[2], arg+3, arg+3+16, view,
			  res+0, res+1, res+2);
  if (egl_res) {
    bp = sdl_get_temp_buff(egl_sd, sizeof(res));
    memcpy(bp, res, sizeof(res));
    sdl_send(egl_sd, sizeof(res));
  }
}
