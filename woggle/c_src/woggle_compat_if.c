#include "woggle_driver.h"
#include "woggle_if.h"


void wog_compat_if_swap_buffers(sdl_data *sd, int len, char *buff) 
{
   char *bp, *start;
   int sendlen;
   Uint32 ts;
   wog_swap_buffers(&(sd->wd));
   bp = start = sdl_getbuff(sd, 4);
   ts = (Uint32) wog_get_tick();
   put32be(bp, ts);
   sendlen = bp - start;
   sdl_send(sd, sendlen);
}

void wog_compat_if_list_modes(sdl_data *sd, int len, char *buff)
{
    WogRes *tmp = NULL;
    char *bp;
    int res;
    int i,j,w,h;

    res = wog_list_modes(&tmp); /* tmp will point into static buffer */
    if (res <= 0) {
	bp = sdl_getbuff(sd, 2);
	put16be(bp,res);
    }
    bp = sdl_getbuff(sd, 8 * res + 1);
    put8(bp,0);
    w = h = 0;
    for (i = res - 1, j = 0; i > 0; --i) {
	if (tmp[i].cdepth == tmp[0].cdepth && (tmp[i].w != w || tmp[i].h != h)) {
	    put16be(bp, 0); 
	    put16be(bp, 0); 
	    put16be(bp, (w = tmp[i].w)); 
	    put16be(bp, (h = tmp[i].h));
	    ++j;
	}
    }
    sdl_send(sd, 8 * j + 1);
}

void wog_compat_if_set_video_mode(sdl_data *sd, int len, char *buff)
{
    char* start;
    int w,h,depth,sendlen;
    w    = get16be(buff);
    h    = get16be(buff);
    depth  = get16be(buff);
    /* Ignoring flags */
    sd->qh  = wog_create_window((WogWindowToken) 0,"Wings GL",
				w,h,depth,&(sd->wd));
    if (sd->qh && !(sd->extensions_loaded)) {
	init_glexts(sd);
	sd->extensions_loaded = 1;
    }
    buff = start = sdl_getbuff(sd, 4);
    put32be(buff, (sd->qh) ? ((unsigned)sd->qh) : -1); 
    sendlen = buff - start;
    sdl_send(sd, sendlen);
}

/*PLACEHOLDER_FOR_GENERATED_FUNCTIONS*/
