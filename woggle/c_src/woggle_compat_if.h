#ifndef _WOGGLE_COMPAT_IF_H
#define _WOGGLE_COMPAT_IF_H
#include <esdl_video.h> /* faking some of those */
#include <esdl_events.h> /* and those...*/

/* SDL compatibility, implemented in woggle_compat_if.c */
void wog_compat_if_swap_buffers(sdl_data *sd, int len, char *buff); 
void wog_compat_if_list_modes(sdl_data *sd, int len, char *buff);

void wog_compat_if_set_video_mode(sdl_data *sd, int len, char *buff);

void wog_compat_if_video_driver_name(sdl_data *sd, int len, char *buff);

void wog_compat_if_gl_set_attribute(sdl_data *sd, int len, char *buff);

void wog_compat_if_gl_get_attribute(sdl_data *sd, int len, char *buff);

void wog_compat_if_wm_get_info(sdl_data *sd, int len, char *buff);

void wog_compat_if_wm_is_maximized(sdl_data *sd, int len, char *buff);

void wog_compat_if_video_mode_o_k(sdl_data *sd, int len, char *buff);

void wog_compat_if_event_state(sdl_data *sd, int len, char *buff);

void wog_compat_if_poll_event(sdl_data *sd, int len, char *buff);

/*PLACEHOLDER_FOR_GENERATED_FUNCTIONS*/

#endif
