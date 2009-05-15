/*
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * 
 *     $Id$
 */

#ifdef __cplusplus
extern "C" {
#endif 

enum { gluBeginCurveFunc = OPENGLU_H + 0 };
void eglu_beginCurve (sdl_data *, int, char *); 
enum { gluBeginPolygonFunc = OPENGLU_H + 1 };
void eglu_beginPolygon (sdl_data *, int, char *); 
enum { gluBeginSurfaceFunc = OPENGLU_H + 2 };
void eglu_beginSurface (sdl_data *, int, char *); 
enum { gluBeginTrimFunc = OPENGLU_H + 3 };
void eglu_beginTrim (sdl_data *, int, char *); 
enum { gluBuild1DMipmapsFunc = OPENGLU_H + 4 };
void eglu_build1DMipmaps (sdl_data *, int, char *); 
enum { gluBuild2DMipmapsFunc = OPENGLU_H + 5 };
void eglu_build2DMipmaps (sdl_data *, int, char *); 
enum { gluBuild3DMipmapsFunc = OPENGLU_H + 6 };
/* void eglu_build3DMipmaps (sdl_data *, int, char *);  */
enum { gluCylinderFunc = OPENGLU_H + 7 };
void eglu_cylinder (sdl_data *, int, char *); 
enum { gluDeleteNurbsRendererFunc = OPENGLU_H + 8 };
void eglu_deleteNurbsRenderer (sdl_data *, int, char *); 
enum { gluDeleteQuadricFunc = OPENGLU_H + 9 };
void eglu_deleteQuadric (sdl_data *, int, char *); 
enum { gluDeleteTessFunc = OPENGLU_H + 10 };
void eglu_deleteTess (sdl_data *, int, char *); 
enum { gluDiskFunc = OPENGLU_H + 11 };
void eglu_disk (sdl_data *, int, char *); 
enum { gluEndCurveFunc = OPENGLU_H + 12 };
void eglu_endCurve (sdl_data *, int, char *); 
enum { gluEndPolygonFunc = OPENGLU_H + 13 };
void eglu_endPolygon (sdl_data *, int, char *); 
enum { gluEndSurfaceFunc = OPENGLU_H + 14 };
void eglu_endSurface (sdl_data *, int, char *); 
enum { gluEndTrimFunc = OPENGLU_H + 15 };
void eglu_endTrim (sdl_data *, int, char *); 
enum { gluErrorStringFunc = OPENGLU_H + 16 };
void eglu_errorString (sdl_data *, int, char *); 
enum { gluGetNurbsPropertyFunc = OPENGLU_H + 17 };
void eglu_getNurbsProperty (sdl_data *, int, char *); 
enum { gluGetStringFunc = OPENGLU_H + 18 };
void eglu_getString (sdl_data *, int, char *); 
enum { gluGetTessPropertyFunc = OPENGLU_H + 19 };
void eglu_getTessProperty (sdl_data *, int, char *); 
enum { gluLoadSamplingMatricesFunc = OPENGLU_H + 20 };
void eglu_loadSamplingMatrices (sdl_data *, int, char *); 
enum { gluLookAtFunc = OPENGLU_H + 21 };
void eglu_lookAt (sdl_data *, int, char *); 
enum { gluNewNurbsRendererFunc = OPENGLU_H + 22 };
void eglu_newNurbsRenderer (sdl_data *, int, char *); 
enum { gluNewQuadricFunc = OPENGLU_H + 23 };
void eglu_newQuadric (sdl_data *, int, char *); 
enum { gluNewTessFunc = OPENGLU_H + 24 };
void eglu_newTess (sdl_data *, int, char *); 
enum { gluNextContourFunc = OPENGLU_H + 25 };
void eglu_nextContour (sdl_data *, int, char *); 
enum { gluNurbsCurveFunc = OPENGLU_H + 26 };
void eglu_nurbsCurve (sdl_data *, int, char *); 
enum { gluNurbsPropertyFunc = OPENGLU_H + 27 };
void eglu_nurbsProperty (sdl_data *, int, char *); 
enum { gluNurbsSurfaceFunc = OPENGLU_H + 28 };
void eglu_nurbsSurface (sdl_data *, int, char *); 
enum { gluOrtho2DFunc = OPENGLU_H + 29 };
void eglu_ortho2D (sdl_data *, int, char *); 
enum { gluPartialDiskFunc = OPENGLU_H + 30 };
void eglu_partialDisk (sdl_data *, int, char *); 
enum { gluPerspectiveFunc = OPENGLU_H + 31 };
void eglu_perspective (sdl_data *, int, char *); 
enum { gluPickMatrixFunc = OPENGLU_H + 32 };
void eglu_pickMatrix (sdl_data *, int, char *); 
enum { gluProjectFunc = OPENGLU_H + 33 };
void eglu_project (sdl_data *, int, char *); 
enum { gluPwlCurveFunc = OPENGLU_H + 34 };
void eglu_pwlCurve (sdl_data *, int, char *); 
enum { gluQuadricDrawStyleFunc = OPENGLU_H + 35 };
void eglu_quadricDrawStyle (sdl_data *, int, char *); 
enum { gluQuadricNormalsFunc = OPENGLU_H + 36 };
void eglu_quadricNormals (sdl_data *, int, char *); 
enum { gluQuadricOrientationFunc = OPENGLU_H + 37 };
void eglu_quadricOrientation (sdl_data *, int, char *); 
enum { gluQuadricTextureFunc = OPENGLU_H + 38 };
void eglu_quadricTexture (sdl_data *, int, char *); 
enum { gluScaleImageFunc = OPENGLU_H + 39 };
void eglu_scaleImage (sdl_data *, int, char *); 
enum { gluSphereFunc = OPENGLU_H + 40 };
void eglu_sphere (sdl_data *, int, char *); 
enum { gluTessBeginContourFunc = OPENGLU_H + 41 };
void eglu_tessBeginContour (sdl_data *, int, char *); 
enum { gluTessBeginPolygonFunc = OPENGLU_H + 42 };
void eglu_tessBeginPolygon (sdl_data *, int, char *); 
enum { gluTessCallbackFunc = OPENGLU_H + 43 };
void eglu_tessCallback (sdl_data *, int, char *); 
enum { gluTessEndContourFunc = OPENGLU_H + 44 };
void eglu_tessEndContour (sdl_data *, int, char *); 
enum { gluTessEndPolygonFunc = OPENGLU_H + 45 };
void eglu_tessEndPolygon (sdl_data *, int, char *); 
enum { gluTessNormalFunc = OPENGLU_H + 46 };
void eglu_tessNormal (sdl_data *, int, char *); 
enum { gluTessPropertyFunc = OPENGLU_H + 47 };
void eglu_tessProperty (sdl_data *, int, char *); 
enum { gluTessVertexFunc = OPENGLU_H + 48 };
void eglu_tessVertex (sdl_data *, int, char *); 
enum { gluUnProjectFunc = OPENGLU_H + 49 };
void eglu_unProject (sdl_data *, int, char *); 

enum { esdl_triangulateFunc = OPENGLU_H + 50 };
void esdl_triangulate (sdl_data *, int, char *); 

   // Init 
void esdl_etess_init();

#ifdef __cplusplus
   }
#endif 
