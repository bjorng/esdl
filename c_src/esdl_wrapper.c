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
#include <string.h>

#include "esdl.h"
#include "esdl_util.h"
#ifndef APIENTRY
# define APIENTRY
#endif

typedef struct sdl_code_fn_def {
   int op;
   char *str;
   sdl_fun fn;
} sdl_code_fn;

static sdl_code_fn code_fns[] = {

#include "esdl_sdl_fp.h"

   { 0, "LastFunction", NULL}};

static void
undefined_function(sdl_data *sd, int len, char *buff)
{
  sd->len = -2;
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

  for (i = 0; i < MAX_FUNCTIONS_H; i++) {
     fun_tab[i] = undefined_function;
     str_tab[i] = "unknown function";
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
