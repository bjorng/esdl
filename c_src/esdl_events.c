/*
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * 
 *     $Id$
 */

/* 
 * Map erl esdl_events calls to C sdl calls      
 */

#include "esdl.h"

#define MAX_EVENT_SIZE 13

/* Foward decls */
static char* encode_event(const SDL_Event * , char *);

/* API */
void es_pumpEvents(sdl_data *sd, int len, char *buff)
{
    if(sd->use_smp) 
	gl_dispatch(sd, SDL_PumpEventsFunc, len, buff);
    else
	SDL_PumpEvents();
}

void es_peepEvents(sdl_data *sd, int len, char *bp)
{
    if(sd->use_smp) 
	gl_dispatch(sd, SDL_PeepEventsFunc, len, bp);   
    else 
	es_peepEvents2(sd->driver_data, driver_caller(sd->driver_data), bp);
}

void es_peepEvents2(ErlDrvPort port, ErlDrvTermData caller, char *bp)
{  
    SDL_Event events[256];
    int numevents, res, i, sz;
    Uint32 mask;
    char *start;
    ErlDrvBinary * bin;
    ErlDrvTermData rt[8];
    
    mask = * (Uint32 *) bp; bp += sizeof(Uint32);
    numevents = *bp++;
    
    SDL_PumpEvents();
    res = SDL_PeepEvents(events, numevents, SDL_GETEVENT, mask);
    bin = driver_alloc_binary(res*MAX_EVENT_SIZE);
    bp = start = bin->orig_bytes;
    for (i = 0; i < res; i++) {
	bp = encode_event(&(events[i]), bp);
    }
    sz = (int) (bp-start);
    rt[0] = ERL_DRV_ATOM; rt[1]=driver_mk_atom((char *) "_esdl_result_");
    rt[2] = ERL_DRV_BINARY; rt[3] = (ErlDrvTermData) bin; rt[4] = sz; rt[5] = 0;
    rt[6] = ERL_DRV_TUPLE; rt[7] = 2;
    driver_send_term(port,caller,rt,8);
    driver_free_binary(bin);
}

void es_pollEvent(sdl_data *sd, int len, char *buff)
{
    if(sd->use_smp) 
	gl_dispatch(sd, SDL_PollEventFunc, len, buff);
    else 
	es_pollEvent2(sd->driver_data, driver_caller(sd->driver_data));
}

void es_pollEvent2(ErlDrvPort port, ErlDrvTermData caller) 
{
    SDL_Event event;
    ErlDrvTermData rt[8];
    ErlDrvBinary * bin;
    char *bp, *start;
    int sz;

    bin = driver_alloc_binary(MAX_EVENT_SIZE);
    bp = start = bin->orig_bytes;
  
    if (SDL_PollEvent(&event)) {
	bp = encode_event(&event, bp);
    }
  
    sz = (int)(bp-start);
    rt[0] = ERL_DRV_ATOM; rt[1]=driver_mk_atom((char *) "_esdl_result_");  
    rt[2] = ERL_DRV_BINARY; rt[3] = (ErlDrvTermData) bin; rt[4] = sz; rt[5] = 0;
    rt[6] = ERL_DRV_TUPLE; rt[7] = 2;
    driver_send_term(port,caller,rt,8);
    driver_free_binary(bin);
}

void es_waitEvent(sdl_data *sd, int len,char *buff)
{
    if(sd->use_smp) 
	gl_dispatch(sd, SDL_WaitEventFunc, len, buff);
    else 
	es_waitEvent2(sd->driver_data, driver_caller(sd->driver_data));
}

void es_waitEvent2(ErlDrvPort port, ErlDrvTermData caller) 
{
    SDL_Event event;
    ErlDrvBinary * bin;
    ErlDrvTermData rt[8];
    char *bp, *start;
    int sz;

    bin = driver_alloc_binary(MAX_EVENT_SIZE);
    bp = start = bin->orig_bytes;
    
    SDL_WaitEvent(&event);
    bp = encode_event(&event, bp);
    
    sz = (int) (bp-start);
    rt[0] = ERL_DRV_ATOM; rt[1]=driver_mk_atom((char *) "_esdl_result_");  
    rt[2] = ERL_DRV_BINARY; rt[3] = (ErlDrvTermData) bin; rt[4] = sz; rt[5] = 0;
    rt[6] = ERL_DRV_TUPLE; rt[7] = 2;
    driver_send_term(port,caller,rt,8);
    driver_free_binary(bin);
}

void es_eventState(sdl_data *sd, int len, char *bp) 
{
   Uint8 type, res;
   int sendlen, state;
   char *start;
    
   type  = get8(bp);
   state = get8(bp);

   res = SDL_EventState(type, state);
   bp = start = sdl_get_temp_buff(sd, 1);
   put8(bp, res);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}


/* API from the other files e.g. mouse keyboard active */ 

void es_getAppState(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   Uint8 state;

   bp = start = sdl_get_temp_buff(sd, 1);
   state = SDL_GetAppState();
   put8(bp, state);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}
 
void es_enableUNICODE(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   Uint8 enable;

   bp = buff;
   enable = get8(bp);
   bp = start = sdl_get_temp_buff(sd, 1);
   enable = SDL_EnableUNICODE(enable);
   put8(bp, enable);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_enableKeyRepeat(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   Uint16 delay, intv, res;
   bp = buff;
   delay = get16be(bp);
   intv  = get16be(bp);
   bp = start = sdl_get_temp_buff(sd, 1);
   res = SDL_EnableKeyRepeat(delay, intv);
   put8(bp, res);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_getKeyName(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start, *name;
   Uint16 key;
   
   bp = buff;
   key = get16be(bp);
   bp = start = sdl_get_temp_buff(sd, 128);
   name = SDL_GetKeyName(key);
   while(*name != '\0') {
      put8(bp, *name);
      name++;
   }
   
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_getKeyState(sdl_data *sd, int len,char *buff)
{
   Uint8 * keys;
   char *bp, *start;
   int length, sendlen, i;

   keys = SDL_GetKeyState(&length);
   bp = start = sdl_get_temp_buff(sd, length);
   for(i=0; i<length; i++)
      put8(bp, keys[i]);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_getModState(sdl_data *sd, int len,char *buff)
{
   Uint16 state;

   if ((state = SDL_GetModState()) != 0) {
     int sendlen;
     char *bp, *start;
     bp = start = sdl_get_temp_buff(sd, 2);
     put16be(bp, state);
     sendlen = (int) (bp - start);
     sdl_send(sd, sendlen);
   }
}

void es_setModState(sdl_data *sd, int len,char *buff)
{
   char *bp;
   Uint16 state;

   bp = buff;
   state = get16be(bp);
   SDL_SetModState(state);
}

void es_getMouseState(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   Uint8 state;
   int x, y;

   bp = start = sdl_get_temp_buff(sd, 5);
   state = SDL_GetMouseState(&x, &y);
   put8(bp, state);
   put16be(bp, x);
   put16be(bp, y);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_getRelativeMouseState(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   Uint8 state;
   int x, y;

   bp = start = sdl_get_temp_buff(sd, 5);
   state = SDL_GetRelativeMouseState(&x, &y);
   put8(bp, state);
   put16be(bp, x);
   put16be(bp, y);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_warpMouse(sdl_data *sd, int len, char *bp)
{
   int x, y;
   
   x = * (short *) bp; bp += sizeof(short);
   y = * (short *) bp; bp += sizeof(short);
   SDL_WarpMouse(x, y);
}

void es_createCursor(sdl_data *sd, int len, char *bp)
{
  int sendlen;
  char *start;
  Uint8* data;
  Uint8* mask;
  int w,h, hotx, hoty, ds;
  SDL_Cursor* cursor;

  w = * (unsigned short *) bp; bp += sizeof(unsigned short);
  h = * (unsigned short *) bp; bp += sizeof(unsigned short);
  hotx = * (unsigned short *) bp; bp += sizeof(unsigned short);
  hoty = * (unsigned short *) bp; bp += sizeof(unsigned short);
  ds = * (unsigned short *) bp; bp += sizeof(unsigned short);
  data = (Uint8*) bp;
  mask = data + ds;
  cursor = SDL_CreateCursor(data, mask, w, h, hotx, hoty);
  bp = start = sdl_get_temp_buff(sd, 8);
  PUSHGLPTR(cursor, bp);
  sendlen = (int) (bp - start);
  sdl_send(sd, sendlen);
}

void es_setCursor(sdl_data *sd, int len, char *bp)
{
  SDL_Cursor *c;
   
  c = * (SDL_Cursor **) bp;
  SDL_SetCursor(c);
}

void es_getCursor(sdl_data *sd, int len,char *buff)
{
   char *bp, *start;
   SDL_Cursor *c;
   int sendlen;
   
   bp = start = sdl_get_temp_buff(sd, 8);
   c = SDL_GetCursor();
   PUSHGLPTR(c, bp);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_freeCursor(sdl_data *sd, int len, char *bp)
{
  SDL_Cursor *c;
   
  POPGLPTR(c, bp);
  SDL_FreeCursor(c);
}

void es_showCursor(sdl_data *sd, int len, char *bp)
{
    if(!sd->use_smp) {
	es_showCursor2(sd->driver_data, driver_caller(sd->driver_data), bp);
    } else { 
	gl_dispatch(sd, SDL_ShowCursorFunc, len, bp);
    }
}

void es_showCursor2(ErlDrvPort port, ErlDrvTermData caller, char *bp)
{
   Uint8 bool;
   ErlDrvTermData rt[8];

   bool = (Uint8) *bp;
   bool = SDL_ShowCursor(bool);
   rt[0] = ERL_DRV_ATOM; rt[1]=driver_mk_atom((char *) "_esdl_result_");
   rt[2] = ERL_DRV_INT; rt[3] = bool;
   rt[4] = ERL_DRV_TUPLE; rt[5] = 2;
   driver_send_term(port,caller,rt,6);
}

void es_numJoysticks(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   int joys;

   bp = buff;
   bp = start = sdl_get_temp_buff(sd, 1);
   joys = SDL_NumJoysticks();
   put8(bp, joys);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_name(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   int index;
   const char * name;
   bp = buff;
   index = get8(bp);
   bp = start = sdl_get_temp_buff(sd, 256);
   name = SDL_JoystickName(index);
   index = 0;
   while(*name != '\0' && index++ < 256) {
      put8(bp, *name);
      name++;
   }
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_open(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   SDL_Joystick *joy;
   int index;
   
   bp = buff;
   index = get8(bp);
   bp = start = sdl_get_temp_buff(sd, 8);
   if((joy = SDL_JoystickOpen(index)) != NULL) {
      PUSHGLPTR(joy, bp);
   }
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_opened(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   int index, bool;
   
   bp = buff;
   index = get8(bp);
   bp = start = sdl_get_temp_buff(sd, 1);
   bool = SDL_JoystickOpened(index);
   put8(bp, bool);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_index(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   SDL_Joystick *joy;
   int index;

   bp = buff;
   POPGLPTR(joy, bp);
   bp = start = sdl_get_temp_buff(sd, 1);
   index = SDL_JoystickIndex(joy);
   put8(bp,index);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_numAxes(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   SDL_Joystick *joy;
   int axes;

   bp = buff;
   POPGLPTR(joy, bp);
   bp = start = sdl_get_temp_buff(sd, 1);
   axes = SDL_JoystickNumAxes(joy);
   put8(bp,axes);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_numBalls(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   SDL_Joystick *joy;
   int balls;

   bp = buff;
   POPGLPTR(joy, bp);
   bp = start = sdl_get_temp_buff(sd, 1);
   balls = SDL_JoystickNumBalls(joy);
   put8(bp,balls);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_numHats(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   SDL_Joystick *joy;
   int hats;

   bp = buff;
   POPGLPTR(joy, bp);
   bp = start = sdl_get_temp_buff(sd, 1);
   hats = SDL_JoystickNumHats(joy);
   put8(bp,hats);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_numButtons(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   SDL_Joystick *joy;
   int buttons;

   bp = buff;
   POPGLPTR(joy, bp);
   bp = start = sdl_get_temp_buff(sd, 1);
   buttons = SDL_JoystickNumButtons(joy);
   put8(bp,buttons);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_update(sdl_data *sd, int len,char *buff)
{
   SDL_JoystickUpdate();
}
void es_joystick_eventState(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   int state;
   
   bp = buff;
   state = get32be(bp);
   bp = start = sdl_get_temp_buff(sd, 4);
   state = SDL_JoystickEventState(state);
   put32be(bp, state);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_getAxis(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   SDL_Joystick *joy;
   int state, axis;
   bp = buff;
   POPGLPTR(joy, bp);
   axis = get8(bp);
   bp = start = sdl_get_temp_buff(sd, 4);
   state = SDL_JoystickGetAxis(joy, axis);
   put32be(bp, state);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_getHat(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   SDL_Joystick *joy;
   int state;
   Uint8 hat;
   bp = buff;
   POPGLPTR(joy, bp);
   hat = get8(bp);
   bp = start = sdl_get_temp_buff(sd, 1);
   state = SDL_JoystickGetHat(joy, hat);
   put8(bp,state);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_getButton(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   SDL_Joystick *joy;
   int state;
   Uint8 button;
   bp = buff;
   POPGLPTR(joy, bp);
   button = get8(bp);
   bp = start = sdl_get_temp_buff(sd, 1);
   state = SDL_JoystickGetButton(joy, button);
   put8(bp,state);
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_getBall(sdl_data *sd, int len,char *buff)
{
   int sendlen;
   char *bp, *start;
   SDL_Joystick *joy;
   int dx, dy;
   Uint8 ball;
   
   bp = buff;
   POPGLPTR(joy, bp);
   ball = get8(bp);
   bp = start = sdl_get_temp_buff(sd, 8);
   if(0 == SDL_JoystickGetBall(joy, ball, &dx, &dy)) {
      put32be(bp, dx);
      put32be(bp, dy);
   }
   sendlen = (int) (bp - start);
   sdl_send(sd, sendlen);
}

void es_joystick_close(sdl_data *sd, int len,char *buff)
{
   char *bp;
   SDL_Joystick *joy;
   bp = buff;
   POPGLPTR(joy, bp);
   SDL_JoystickClose(joy);
}

/* Internals */ 
static char* encode_event(const SDL_Event * ev, char * bp)
{ 
   switch(ev->type) {
   case SDL_ACTIVEEVENT:	/* Application loses/gains visibility */
      put8(bp, SDL_ACTIVEEVENT);
      put8(bp, ev->active.gain);
      put8(bp, ev->active.state);	    
      break;
   case SDL_KEYDOWN:	/* Keys pressed */
   case SDL_KEYUP:	/* Keys released */ 
      put8(bp, SDL_KEYDOWN);   /* Using same for both */
      put8(bp, ev->key.which);
      put8(bp, ev->key.state);	    
      put8(bp, ev->key.keysym.scancode);	    
      put16be(bp, ev->key.keysym.sym);	    
      put16be(bp, ev->key.keysym.mod);	    
      put16be(bp, ev->key.keysym.unicode);	    
      break;
   case SDL_MOUSEMOTION:	/* Mouse moved */
      put8(bp, SDL_MOUSEMOTION);
      put8(bp, ev->motion.which);
      put8(bp, ev->motion.state);
      put16be(bp, SDL_GetModState());
      put16be(bp, ev->motion.x);
      put16be(bp, ev->motion.y);
      put16be(bp, ev->motion.xrel);
      put16be(bp, ev->motion.yrel);
      break;
   case SDL_MOUSEBUTTONDOWN:/* Mouse button pressed */
   case SDL_MOUSEBUTTONUP:/* Mouse button released */
      put8(bp, SDL_MOUSEBUTTONDOWN);
      put8(bp, ev->button.which);
      put8(bp, ev->button.button);
      put8(bp, ev->button.state);
      put16be(bp, SDL_GetModState());
      put16be(bp, ev->button.x);
      put16be(bp, ev->button.y);	
      break;
   case SDL_JOYAXISMOTION:/* Joystick axis motion */
      put8(bp, SDL_JOYAXISMOTION);
      put8(bp, ev->jaxis.which);
      put8(bp, ev->jaxis.axis);	    
      put16be(bp, ev->jaxis.value);
      break;
   case SDL_JOYBALLMOTION:/* Joystick trackball motion */
      put8(bp, SDL_JOYBALLMOTION);
      put8(bp, ev->jball.which);
      put8(bp, ev->jball.ball);	    
      put16be(bp, ev->jball.xrel);
      put16be(bp, ev->jball.yrel);
      break;
   case SDL_JOYHATMOTION:/* Joystick hat position change */
      put8(bp, SDL_JOYHATMOTION);
      put8(bp, ev->jhat.which);
      put8(bp, ev->jhat.hat);	    
      put8(bp, ev->jhat.value);
      break;
   case SDL_JOYBUTTONDOWN:/* Joystick button pressed */
   case SDL_JOYBUTTONUP:	/* Joystick button released */
      put8(bp, SDL_JOYBUTTONDOWN);
      put8(bp, ev->jbutton.which);
      put8(bp, ev->jbutton.button);
      put8(bp, ev->jbutton.state);	    
      break;
   case SDL_QUIT:	/* User-requested quit */
      put8(bp, SDL_QUIT);
      break;
   case SDL_VIDEORESIZE:	
      put8(bp, SDL_VIDEORESIZE);
      put16be(bp, ev->resize.w);
      put16be(bp, ev->resize.h);
      break;
   case SDL_VIDEOEXPOSE:	
      put8(bp, SDL_VIDEOEXPOSE);
      break;
   case SDL_SYSWMEVENT:	/* System specific event */
      break;	
   default:   /* Forward compatible */
     fprintf(stderr, "ESDL received unsupported event type %x \n",
	     ev->type);
     put8(bp, SDL_NOEVENT);
     break;
   }
   return bp;
}
