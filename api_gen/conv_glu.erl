%%%----------------------------------------------------------------------
%%% File    : conv_glu.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created : 11 Oct 2001 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

%% Some knowledge about lengths of arrays and similar stuff.

-module(conv_glu).
-compile(export_all).

add() ->
    [
     {"gluNurbsCallback", {skip, []}}, %% Not supported
     {"gluNurbsCallbackData", {skip, []}}, %% Not supported
     {"gluNurbsCallbackDataEXT", {skip, []}}, %% Not supported
     {"gluQuadricCallback", {skip, []}}, %% Not supported
%%     {"gluTessCallback", {skip, []}}, %% Not supported
     {"gluTessBeginPolygon", {[{"data", {const, pointer}}], []}},
     {"gluTessVertex", {[{"location", {const, pointer}}, 
			 {"data", {const, pointer}}], []}},
     

     {"gluBuild1DMipmapLevels", {skip, [erl,c]}},
     {"gluBuild1DMipmaps", {[{"data", pointer, "type"}], []}},
     {"gluBuild2DMipmapLevels", {skip, [erl,c]}},
     {"gluBuild2DMipmaps", {[{"data", pointer, "type"}], []}},
     {"gluBuild3DMipmapLevels", {skip, [erl,c]}},
     {"gluBuild3DMipmaps", {[{"data", pointer, "type"}], []}},
     {"gluCheckExtension", {skip, [erl,c]}},     
     {"gluGetNurbsProperty",  {[{"data", 1}], []}},
     {"gluGetTessProperty",  {[{"data", 1}], []}},
     {"gluLoadSamplingMatrices",  
      {[{"model", 16},{"view", 16}, {"perspective", 16}], []}},
     {"gluNurbsCurve", {[{"knots", {const,"knotCount"}}, {"control", {const, pointer}}],[]}},
     {"gluNurbsSurface", {[{"sKnots", {const,"sKnotCount"}},{"tKnots", {const,"tKnotCount"}},
			   {"control", {const, pointer}}],[]}},
     {"gluPickMatrix", {[{"viewport", {const, 4}}],[]}},
     {"gluProject", 
      {[{"model",16},{"view",16},{"proj",16},{"winX",1},{"winY",1},{"winZ",1}],[]}},
     {"gluPwlCurve", {[{"data", {const, pointer}}],[]}},
     {"gluScaleImage", {[{"dataOut", sdlmem}, {"dataIn", pointer,"typeIn"}], []}},
     {"gluUnProject", 
      {[{"model",16},{"view",16},{"proj",16},{"objX",1},{"objY",1},{"objZ",1}],[]}},
     {"gluUnProject4", {skip, 
			{[{"model",16},{"view",16},{"proj",16},
			  {"objX",1},{"objY",1},{"objZ",1},{"objW",1}],[]}}},
     {last, []}
    ].

init_hrl(Fd) ->
    io:format(Fd,
	      "~n%% Available ESDL Tessalator callbacks"
	      "~n-define(ESDL_TESSCB_NONE,        0).       %% No callback"
	      "~n-define(ESDL_TESSCB_GLBEGIN,     1).       %% calls gl:begin(Type) "
	      "~n-define(ESDL_TESSCB_GLEND,       2).       %% calls gl:end()"
	      "~n-define(ESDL_TESSCB_GLVERTEX,    3).       %% calls gl:vertex3dv(vertex)"
	      "~n-define(ESDL_TESSCB_ERROR_PRINT, 4).       %% Prints error on stderr."
	      "~n-define(ESDL_TESSCB_COMBINE,     5).       %% Adds vector, and adjusts user data"
	      "~n-define(ESDL_TESSCB_GLEDGEFLAG,  6).       %% calls gl:edgeFlag(Flag)"
	      "~n-define(ESDL_TESSCB_VERTEX_DATA, 7).       %% calls various"
	      "~n"
	      ,[]).

init_erl(Fd) ->
    io:format(Fd, 
	      "~n-module(glu)."
	      "~n-author('dgud@erix.ericsson.se')."
	      "~n-compile(export_all)."
	      "~n%%-export([Function/Arity, ...])."
	      "~n-include(\"esdl.hrl\")."
	      "~n-include(\"glu_funcs.hrl\")."
	      "~n-include(\"sdl_util.hrl\")."
	      "~n-include(\"gl.hrl\" )."
	      "~n-import(sdl, [call/2, cast/2])."
	      "~n-import(sdl_util,  [bin2list/2,bin2list/3, term2bin/2, term2bin/3])."
	      "~n"
	      "~n%% Internal datatypes"
	      "~n-define(getPointer(REC), element(2, REC))."
	      "~n-record(quadricPtr, {ptr})."
	      "~n-record(nurbsPtr, {ptr})."
	      "~n-record(tessPtr, {ptr})."
	      "
-define(ESDL_TESS_VTXDATA_MATERIAL, 1).
-define(ESDL_TESS_VTXDATA_TEXCOORD2, 2).
-define(ESDL_TESS_VTXDATA_NORMAL, 4).
-define(ESDL_TESS_VTXDATA_COLOR, 8).

convert_vtxdata([{normal,{X,Y,Z}}|T], Ops,Acc) ->
    convert_vtxdata(T, Ops bor ?ESDL_TESS_VTXDATA_NORMAL,
		    [<<X:32/native-float,Y:32/native-float,
		      Z:32/native-float>>|Acc]);
convert_vtxdata([{material,Face,Pname,{R,G,B}}|T], Ops, Acc) ->
    convert_vtxdata(T, Ops bor ?ESDL_TESS_VTXDATA_MATERIAL,
		    [<<Face:16/native-unsigned,Pname:16/native-unsigned,
		      R:32/native-float,G:32/native-float,
		      B:32/native-float,1:32/native-float>>|Acc]);
convert_vtxdata([{material,Face,Pname,{R,G,B,A}}|T], Ops, Acc) ->
    convert_vtxdata(T, Ops bor ?ESDL_TESS_VTXDATA_MATERIAL,
		    [<<Face:16/native-unsigned,Pname:16/native-unsigned,
		      R:32/native-float,G:32/native-float,
		      B:32/native-float,A:32/native-float>>|Acc]);
convert_vtxdata([{color,{R,G,B}}|T], Ops, Acc) ->
    convert_vtxdata(T, Ops bor ?ESDL_TESS_VTXDATA_COLOR,
		    [<<R:32/native-float,G:32/native-float,
		      B:32/native-float, 1:32/native-float>>|Acc]);
convert_vtxdata([{color,{R,G,B,A}}|T], Ops, Acc) ->
    convert_vtxdata(T, Ops bor ?ESDL_TESS_VTXDATA_COLOR,
		    [<<R:32/native-float,G:32/native-float,
		      B:32/native-float, A:32/native-float>>|Acc]);
convert_vtxdata([{texcoord2,{U,V}}|T], Ops, Acc) ->
    convert_vtxdata(T, Ops bor ?ESDL_TESS_VTXDATA_TEXCOORD2,
		    [<<U:32/native-float,V:32/native-float>>|Acc]);
convert_vtxdata([], Ops, Acc) -> [Acc,Ops].

%% Func:    tessBeginPolygon 
%% Args:    TessObject
%% Returns: ok
%% C-API func: void gluTessBeginPolygon(GLUtesselator *tobj)
tessBeginPolygon(#tessPtr{ptr=Tobj}) ->
    cast(?gluTessBeginPolygon, <<Tobj:32/native,0:32>>).

%% Func:    tessCallback 
%% Args:    Which, ESDL_TESSCB one of the callback functions defined in glu.hrl
%% Returns:  ok
%% C-API func: void gluTessCallback(GLUtesselator *tobj, GLenum which, void (GLCALLBACK *fn)())
%% Desc:    ESDL have some predefined functions which must be supplied as callbacks funcs,
%%          see glu.hrl. New callbacks may be defined in the future, input and or 
%%          implementations are welcomed.

tessCallback(#tessPtr{ptr=Tobj}, Which, ESDL_TESSCB) -> 
    cast(?gluTessCallback, <<Tobj:32/native,
			    Which:32/unsigned-native, 
			    ESDL_TESSCB:32/unsigned-native>>).

%% Func:    tessVertex 
%% Args:    Tobj, Coords[, VtxData]
%% Returns: ok
%% C-API func: void gluTessVertex(GLUtesselator *tobj, GLdouble coords[3], void *vertex_data)
%% Desc: VtxData is a tuple-list of extra data supported types are 
%%       [{normal, {X,Y,Z}}, {color, {R,G,B[,A]}}, {texcoord2, {U,V}}]
%%       Use ?ESDL_TESSCB_VERTEX_DATA callback to call apropriate gl functions
%%       on the extra data.

tessVertex(#tessPtr{ptr=Tobj}, {X,Y,Z}) ->
    cast(?gluTessVertex,
	 <<Tobj:32/native,X:64/native-float,Y:64/native-float,Z:64/native-float>>).

tessVertex(#tessPtr{ptr=Tobj}, {X,Y,Z}, VtxData0) ->
    %% Sort here so that we know that all data for all vertices
    %% come in the same order; the driver depends on it...
    VtxData = convert_vtxdata(lists:sort(VtxData0), 0, []),
    Data =  [<<Tobj:32/native,
	      X:64/native-float,Y:64/native-float,
	      Z:64/native-float>>|VtxData],
    cast(?gluTessVertex, Data).


"
	      ,[]).


init_c(Fd) ->
    io:format(Fd, 
	      "
/*  
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file \"license.terms\" for information on usage and redistribution
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
#include \"esdl.h\"
#ifdef _OSX_COCOA
#include <OpenGL/glu.h>
#else
#include <GL/glu.h>
#endif
#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

#define GetNativePtr(res, T, s) \\
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

#define ESDL_TESS_VTXDATA_MATERIAL     1
#define ESDL_TESS_VTXDATA_TEXCOORD2    2
#define ESDL_TESS_VTXDATA_NORMAL       4
#define ESDL_TESS_VTXDATA_COLOR        8

void CALLBACK errorCallback(GLenum errorCode)
{
			     const GLubyte *err;
			     err = gluErrorString(errorCode);
			     fprintf(stderr, \"Tesselation error: %d: %s\\r\\n\", errorCode, err);
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

    /* fprintf(stderr, \"Flags: %d\\r\\n\", flags); */
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

    /*    fprintf(stderr, \"New tess: %d -> %d \\r\\n\", (int) eobj, (int) tobj); */

    /*
     * Send back result.
     */
    bp = start = sdl_getbuff(sd, 4, buff);    
/*    putPointer(bp, eobj); */
    * (eglu_tessobj **) bp = eobj;
    bp += sizeof(eglu_tessobj *);

    sendlen = bp - start;
    sdl_send(sd, sendlen, buff);
}

void eglu_deleteTess (sdl_data *sd, int len, char * buff) 
{
    char *bp;
    eglu_tessobj *eobj;
    bp = buff;
    GetNativePtr(eobj, (eglu_tessobj *), bp);
    gluDeleteTess(eobj->tess);
    free(eobj);
    /* fprintf(stderr, \"Deleting tess: %d\\r\\n\", (int) eobj); */
}

void eglu_tessBeginPolygon (sdl_data *sd, int len, char * buff) 
{
    char *bp;
    eglu_tessobj *eobj;

    /* void *polygon_data; */
    bp = buff;
    GetNativePtr(eobj, (eglu_tessobj *), bp);
    /* fprintf(stderr, \"tessBeginPolygon: %d\\r\\n"", (int) eobj); */
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
     /* fprintf(stderr, \"tessVertex: %d %g %g %g\\r\\n\",
       (int) eobj, coords->data[0], coords->data[1], coords->data[2]); */
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
}

void eglu_tessCallback(sdl_data *sd, int len, char * buff) 
{
   char *bp;
   eglu_tessobj *eobj;
   GLenum which;
   int cbId;
   GLvoid (CALLBACK *cbfn)();
   bp = buff;
   GetNativePtr(eobj, (eglu_tessobj *), bp);
   which = get32be(bp);
   cbId = get32be(bp);

   switch(which) {
   case GLU_TESS_COMBINE:
       which = GLU_TESS_COMBINE_DATA;
       break;      
       default:
       break;
      };
      
   switch(cbId) {
   case ESDL_TESSCB_NONE:
      cbfn = NULL;
      break;
   case ESDL_TESSCB_GLBEGIN:
      cbfn = glBegin;
      break;
   case ESDL_TESSCB_GLEND:
      cbfn = glEnd;
      break;
   case ESDL_TESSCB_GLVERTEX:
      cbfn = glVertex3dv;
      break;
   case ESDL_TESSCB_GLEDGEFLAG:
      cbfn = glEdgeFlag;
      break;
   case ESDL_TESSCB_ERROR_PRINT:
      cbfn = errorCallback;
      break;
   case ESDL_TESSCB_COMBINE:
      cbfn = esdl_combine;
      break;
   case ESDL_TESSCB_UDATA_VERTEX:
      cbfn = esdl_udata_vertex;
      break;

   default:
      cbfn = NULL;
      break;
   };
   gluTessCallback(eobj->tess, which, cbfn);
}
", []).
