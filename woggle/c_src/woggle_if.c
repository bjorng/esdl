#include "woggle_driver.h"
#include "woggle_if.h"

void wog_if_swap_buffers(sdl_data *sd, int len, char *buff)
{ 
    wog_swap_buffers(&(sd->wd));
}

void wog_if_list_modes(sdl_data *sd, int len, char *buff)
{
    WogRes *tmp = NULL;
    char *bp;
    int res;
    int i;

    res = wog_list_modes(&tmp); /* tmp will point into static buffer */
    if (res <= 0) {
	bp = sdl_getbuff(sd, 2);
	put16be(bp,res);
    }
    bp = sdl_getbuff(sd, 9 * res);
    for (i = 0; i < res; ++i) {
	put8(bp, tmp[i].cdepth); 
	put32be(bp, tmp[i].freq); 
	put16be(bp, tmp[i].w); 
	put16be(bp, tmp[i].h);
    }
    sdl_send(sd, 9 * res);
}

/*PLACEHOLDER_FOR_GENERATED_FUNCTIONS*/
