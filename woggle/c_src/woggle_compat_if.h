#ifndef _WOGGLE_COMPAT_IF_H
#define _WOGGLE_COMPAT_IF_H
#include <esdl_video.h> /* faking some of those */

/* SDL compatibility, implemented in woggle_compat_if.c */
void wog_compat_if_swap_buffers(sdl_data *sd, int len, char *buff); 
void wog_compat_if_list_modes(sdl_data *sd, int len, char *buff);

void wog_compat_if_set_video_mode(sdl_data *sd, int len, char *buff);

/*PLACEHOLDER_FOR_GENERATED_FUNCTIONS*/

#endif
