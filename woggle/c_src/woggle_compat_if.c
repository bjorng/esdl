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

void wog_compat_if_video_driver_name(sdl_data *sd, int len, char *buff)
{
    char *bp;
    bp = sdl_getbuff(sd, 6);
    memcpy(bp,"woggle",6);
    sdl_send(sd, 6);
}

void wog_compat_if_gl_set_attribute(sdl_data *sd, int len, char *buff)
{
    /* Dummy cast */
}

#define COMPAT_SDL_GL_RED_SIZE            0
#define COMPAT_SDL_GL_GREEN_SIZE          1
#define COMPAT_SDL_GL_BLUE_SIZE           2
#define COMPAT_SDL_GL_ALPHA_SIZE          3
#define COMPAT_SDL_GL_BUFFER_SIZE         4
#define COMPAT_SDL_GL_DOUBLEBUFFER        5
#define COMPAT_SDL_GL_DEPTH_SIZE          6

void wog_compat_if_gl_get_attribute(sdl_data *sd, int len, char *buff)
{
    int attr;
    int res;
    char *bp;

    attr = get16be(buff);

    switch(attr) {
    case COMPAT_SDL_GL_RED_SIZE:
	res = wog_get_attr(&(sd->wd),WOG_ATTR_RED_SIZE);
	break;
    case COMPAT_SDL_GL_GREEN_SIZE:
	res = wog_get_attr(&(sd->wd),WOG_ATTR_GREEN_SIZE);
	break;
    case COMPAT_SDL_GL_BLUE_SIZE:
	res = wog_get_attr(&(sd->wd),WOG_ATTR_BLUE_SIZE);
	break;
    case COMPAT_SDL_GL_ALPHA_SIZE:
 	res = wog_get_attr(&(sd->wd),WOG_ATTR_ALPHA_SIZE);
	break;
    case COMPAT_SDL_GL_BUFFER_SIZE:
 	res = wog_get_attr(&(sd->wd),WOG_ATTR_BUFFER_SIZE);
	break;
    case COMPAT_SDL_GL_DOUBLEBUFFER:
 	res = wog_get_attr(&(sd->wd),WOG_ATTR_DOUBLEBUFFER);
	break;
    case COMPAT_SDL_GL_DEPTH_SIZE:
 	res = wog_get_attr(&(sd->wd),WOG_ATTR_DEPTH_SIZE);
	break;
    default:
	res = -1;
	break;
    }
    bp = sdl_getbuff(sd, 4);
    put32be(bp, res);
    sdl_send(sd, 4);
}

void wog_compat_if_wm_get_info(sdl_data *sd, int len, char *buff)
{
    char *bp;
    bp = sdl_getbuff(sd, 3);
    put8(bp, WOGGLE_VERSION_MAJOR);
    put8(bp, WOGGLE_VERSION_MINOR);
    put8(bp, WOGGLE_VERSION_PATCH);
    sdl_send(sd, 3);
}

void wog_compat_if_wm_is_maximized(sdl_data *sd, int len, char *buff)
{
    char *bp;
    int res;

    bp = sdl_getbuff(sd, 1);
    res = wog_get_wmattr(&(sd->wd),WOG_WMATTR_MAXIMIZED);
    if (res <= 0)
	res = 0;
    put8(bp, res);
    sdl_send(sd, 1);
}

void do_debug_break(void){DebugBreak();}

void wog_compat_if_video_mode_o_k(sdl_data *sd, int len, char *buff)
{
    char *bp;
    int res;
    WogRes *tmp;

    
    /*do_debug_break();*/
    bp = sdl_getbuff(sd, 1);
    res = wog_list_modes(&tmp); /* tmp will point into static buffer */
    if (res <= 0) {
	put8(bp,0);
    } else {
	int w,h,bpp,i;
	int ok;
	w = get16be(buff);
	h = get16be(buff);
	bpp = get16be(buff);
	ok = 0;
	for (i = 0; i < res && !ok;++i) {
	    ok = ((w == tmp[i].w) && (h == tmp[i].h) && 
		  (bpp == tmp[i].cdepth));
	}
	put8(bp,ok);
    }
    sdl_send(sd, 1);
}

/* Compatibility event types... */
#define SDL_NOEVENT         0
#define SDL_ACTIVEEVENT     1
#define SDL_KEYDOWN	    2
#define SDL_KEYUP	    3
#define SDL_MOUSEMOTION     4
#define SDL_MOUSEBUTTONDOWN 5
#define SDL_MOUSEBUTTONUP   6
#define SDL_JOYAXISMOTION   7
#define SDL_JOYBALLMOTION   8
#define SDL_JOYHATMOTION    9
#define SDL_JOYBUTTONDOWN  10
#define SDL_JOYBUTTONUP    11
#define SDL_QUIT           12
#define SDL_SYSWMEVENT     13
#define SDL_VIDEORESIZE    16
#define SDL_VIDEOEXPOSE    17
#define SDL_ALLEVENTS      0xFF /* yup, thats what it appears as... */

#define SDL_QUERY	-1
#define SDL_IGNORE	 0
#define SDL_ENABLE	 1

#define SDL_ADDEVENT    0
#define SDL_PEEKEVENT   1
#define SDL_GETEVENT    2

#define SDL_PRESSED     1
#define SDL_RELEASED    0

#define SDL_ALL_HOTKEYS	0xFFFFFFFFUL

static struct {
    int sdl_evtype;
    WogCommandTag wog_evtype;
}evTT[] =
{
    {SDL_ACTIVEEVENT,WogListenActivate},
    {SDL_KEYDOWN,WogListenKeyDown},
    {SDL_KEYUP,WogListenKeyUp},
    {SDL_MOUSEMOTION,WogListenMouseMove},
    {SDL_MOUSEBUTTONDOWN,WogListenMouseDown},
    {SDL_MOUSEBUTTONUP,WogListenMouseUp},
    {SDL_QUIT,WogListenClose},
    {SDL_VIDEORESIZE,WogListenResize},
    {SDL_VIDEOEXPOSE,WogListenPaint},
    {SDL_ALLEVENTS,WogListenAll}
};

static int tt_sdl_to_wog(int sdl_evtype)
{
    int i;
    for (i = 0; i < (sizeof(evTT) / sizeof(evTT[0])); ++i) {
	if (evTT[i].sdl_evtype == sdl_evtype) {
	    return (int) evTT[i].wog_evtype;
	}
    }
    return -1;
}

void wog_compat_if_event_state(sdl_data *sd, int len, char *buff)
{
    char *bp;
    int type,state,wetype,onoff = 0;
    type = get8(buff);
    state = get8(buff);
    if ((wetype = tt_sdl_to_wog(type)) < 0) {
	goto error;
    }
    switch (state) {
    case SDL_IGNORE:
	onoff = 0;
	break;
    case SDL_ENABLE:
	onoff = 1;
	break;
    default:
	goto error;
    }
#if 1
    {
	char buff[128];
	sprintf(buff,"type = %d, state = %d",type,state);
	MessageBox(NULL,buff,buff,MB_OK);
    }
#endif
    wog_listen(&(sd->wd),wetype,onoff);
    bp = sdl_getbuff(sd,1);
    put8(bp,1);
    sdl_send(sd, 1);
    return;
 error:
    bp = sdl_getbuff(sd,1);
    put8(bp,0);
    sdl_send(sd, 1);
}

void wog_compat_if_poll_event(sdl_data *sd, int len, char *buff)
{
    WogEventMessage *msg;
    char *bp, *start;
    unsigned mods;

    if (sd->saved_event != NULL) {
	msg = sd->saved_event;
	sd->saved_event = NULL;
    } else if (!(msg = wog_get_event_message(WOG_GLOBAL_EVENT_QUE()))) {
	return;
    }
    switch (msg->any.tag) {
    case WogActivate:
	bp = start = sdl_getbuff(sd,3);
	put8(bp, SDL_ACTIVEEVENT);
	put8(bp,1); /* always gaining */
	put8(bp,0); /* seems always ignored anyway */
	wog_free_event_message(msg);
	break;
    case WogKeyUp:
    case WogKeyDown:
	bp = start = sdl_getbuff(sd,10);
	put8(bp, SDL_KEYDOWN);
	put8(bp,0); /* device index?*/
	if (msg->any.tag == WogKeyUp) {
	    put8(bp,SDL_RELEASED);
	} else {
	    put8(bp,SDL_PRESSED);
	}
	put8(bp,msg->key_up_down.scancode);
	put16be(bp,msg->key_up_down.code);
	/* build the modifiers... */
	mods = 0;
	if (msg->key_up_down.modifiers & WOG_MODIFIER_SHIFT)
	    mods |= 1; /*KMOD_LSHIFT*/
	if (msg->key_up_down.modifiers & WOG_MODIFIER_CTRL)
	    mods |= 0x40; /*KMOD_LCTRL*/
	if (msg->key_up_down.modifiers & WOG_MODIFIER_ALT)
	    mods |= 0x100; /*KMOD_LALT*/
	put16be(bp,mods);
	put16be(bp,0); /* XXX: Unicode not yet implemented! */
	if (msg->key_up_down.repeat > 1) {
	    --(msg->key_up_down.repeat);
	    sd->saved_event = msg;
	} else {
	    wog_free_event_message(msg);
	}
	break;
    case WogMouseMove:
	bp = start = sdl_getbuff(sd,13);
	put8(bp, SDL_MOUSEMOTION);
	put8(bp, 0);
	mods = 0;
	if (msg->mouse_move.modifiers & WOG_MODIFIER_MOUSE_LEFT)
	    mods |= 1;
	if (msg->mouse_move.modifiers & WOG_MODIFIER_MOUSE_MIDDLE)
	    mods |= 2;
	if (msg->mouse_move.modifiers & WOG_MODIFIER_MOUSE_RIGHT)
	    mods |= 4;
	put8(bp,mods);
	put16be(bp,0); /* Hell *will* freeze over before 
			  i'm going to implement this...*/
	put16be(bp,msg->mouse_move.x);
	put16be(bp,msg->mouse_move.y);
	if (sd->save_x < 0) { /* first time */
	    sd->save_x = msg->mouse_move.x;
	    sd->save_y = msg->mouse_move.y;
	}
	put16be(bp,(short) msg->mouse_move.x - sd->save_x);
	put16be(bp,(short) msg->mouse_move.y - sd->save_y);
	sd->save_x = msg->mouse_move.x;
	sd->save_y = msg->mouse_move.y;
	wog_free_event_message(msg);
	break;
    case WogMouseDown:
    case WogMouseUp:
	bp = start = sdl_getbuff(sd,10);
	put8(bp, SDL_MOUSEBUTTONDOWN);
	put8(bp, 0);
	switch (msg->mouse_up_down.button) {
	case WOG_MODIFIER_MOUSE_LEFT:
	    put8(bp,1);
	    break;
	case WOG_MODIFIER_MOUSE_MIDDLE:
	    put8(bp,2);
	    break;
	case WOG_MODIFIER_MOUSE_RIGHT:
	    put8(bp,3);
	    break;
	default:
	    put8(bp,0);
	}
	if (msg->any.tag == WogMouseUp) {
	    put8(bp,SDL_RELEASED);
	} else {
	    put8(bp,SDL_PRESSED);
	}
	put16be(bp,0); /* Not implemented... */
	put16be(bp,msg->mouse_up_down.x);
	put16be(bp,msg->mouse_up_down.y);
	wog_free_event_message(msg);
	break;
    case WogClose:
	bp = start = sdl_getbuff(sd,1);
	put8(bp, SDL_QUIT);
	wog_free_event_message(msg);
	break;
    case WogResize:
	bp = start = sdl_getbuff(sd,5);
	put8(bp, SDL_VIDEORESIZE);
	put16be(bp, msg->resize.width);
	put16be(bp, msg->resize.height);
	wog_free_event_message(msg);
	break;
    case WogPaint:
	bp = start = sdl_getbuff(sd,1);
	put8(bp, SDL_VIDEOEXPOSE);
	wog_free_event_message(msg);
	break;
    default:
	wog_free_event_message(msg);
	return;
    }
    sdl_send(sd, (bp - start));
}

/*PLACEHOLDER_FOR_GENERATED_FUNCTIONS*/
