/*
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 * 
 *     $Id$
 */
/*    
   Command Handler
   Dispatch the work on the functions   
*/

#include <stdlib.h>
#include <strings.h>

#include "esdl.h"
#include "esdl_gl.h"
#include "esdl_glu.h"
#include "esdl_util.h"
#ifndef APIENTRY
# define APIENTRY
#endif
#define ESDL_DEFINE_EXTS
#include "esdl_glext.h"

typedef struct sdl_code_fn_def {
   int op;
   char *str;
   sdl_fun fn;
} sdl_code_fn;

static sdl_code_fn code_fns[] = {

#include "esdl_sdl_fp.h"

#include "esdl_gl_fp.h"

#include "esdl_glu_fp.h"

   { 0, "LastFunction", NULL}};

static struct {
  int op;
  char* name;
  sdl_fun fun;
  void* ext_fun;
} ext_fns[] = {

#include "esdl_glext_fp.h"

  { 0, "LastFunction", NULL, NULL}};

static void
undefined_function(sdl_data *sd, int len, char *buff)
{
  sd->len = -2;
}

static void
undefined_extension(sdl_data *sd, int len, char *buff)
{
  sd->len = -3;
}

void init_fps(sdl_data* sd)
{   
  int i;
  int n = MAX_FUNCTIONS_H;  /* Must be greater than then the last function op */ 
  int op;
  sdl_fun* fun_tab;
  char** str_tab;

  fun_tab = sd->fun_tab = malloc((n+1) * sizeof(sdl_fun));
  str_tab = sd->str_tab = malloc((n+1) * sizeof(char*));

  for (i = 0; i < OPENGL_EXTS_H; i++) {
    fun_tab[i] = undefined_function;
    str_tab[i] = "unknown function";
  }

  for ( ; i < n; i++) {
    fun_tab[i] = undefined_extension;
    str_tab[i] = "unknown extension";
  }

  for (i = 0; ((op = code_fns[i].op) != 0); i++) {
    if (fun_tab[op] == undefined_function) {
      fun_tab[op] = code_fns[i].fn;
      str_tab[op] = code_fns[i].str;
    } else {
      fprintf(stderr, 
	      "FParray mismatch in initialization: %d '%s' %d '%s'\r\n",
	      i, str_tab[op], op, code_fns[i].str);
    }
  }
}

/* Must be done after creating a rendering context */
void init_glexts(sdl_data* sd)
{
   int i;
   int op;
   sdl_fun* fun_tab = sd->fun_tab;
   char** str_tab = sd->str_tab;

   for (i = 0; (op = ext_fns[i].op) != 0; i++) {
      if (fun_tab[op] == undefined_extension) {
	 void* ext_ptr = SDL_GL_GetProcAddress(ext_fns[i].name);
	 str_tab[op] = ext_fns[i].name;
	 if (ext_ptr) {
	    /* fprintf(stderr, "Success %s \r\n", ext_fns[i].name);*/ 
	    * (void **) (ext_fns[i].ext_fun) = ext_ptr;
	    fun_tab[op] = ext_fns[i].fun;
	 } else {
	    char arbname[256];	   
	    char* tmp;
	    strcpy(arbname, ext_fns[i].name);
	    tmp = arbname + strlen(arbname);
	    strncpy(tmp, "ARB", 4);
	    ext_ptr = SDL_GL_GetProcAddress(arbname);
	    if (ext_ptr) {
	       /* fprintf(stderr, "Success %s \r\n", ext_fns[i].name); */
	       * (void **) (ext_fns[i].ext_fun) = ext_ptr;
	       fun_tab[op] = ext_fns[i].fun;
	    } else {	    	    
	       /*	       fprintf(stderr, "Failed %s \r\n", ext_fns[i].name); */
	       fun_tab[op] = undefined_extension;
	    }
	 }
      } else {	 
	 fprintf(stderr, "Exiting FP EXTENSION array mismatch in initialization %d %d %s\r\n", 
		 i, ext_fns[i].op, ext_fns[i].name);
      }
   }
}
