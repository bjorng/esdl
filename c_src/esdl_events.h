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

#define SDL_PumpEventsFunc (EVENTS_H + 1) 
void es_pumpEvents(sdl_data *, int , char *);
#define SDL_PeepEventsFunc SDL_PumpEventsFunc+1
void es_peepEvents(sdl_data *, int , char *);
void es_peepEvents2(ErlDrvPort port, ErlDrvTermData caller, char *bp);
#define SDL_PollEventFunc  SDL_PeepEventsFunc+1
void es_pollEvent(sdl_data *, int , char *);
void es_pollEvent2(ErlDrvPort port, ErlDrvTermData caller);
#define SDL_WaitEventFunc  SDL_PollEventFunc +1
void es_waitEvent(sdl_data *, int , char *);
void es_waitEvent2(ErlDrvPort port, ErlDrvTermData caller);
#define SDL_EventStateFunc SDL_WaitEventFunc +1
void es_eventState(sdl_data *, int , char *);

#define SDL_GetMouseStateFunc MOUSE_H +1
void es_getMouseState(sdl_data *, int , char *);
#define SDL_GetRelativeMouseStateFunc SDL_GetMouseStateFunc+1
void es_getRelativeMouseState(sdl_data *, int , char *);
#define SDL_WarpMouseFunc SDL_GetRelativeMouseStateFunc +1
void es_warpMouse(sdl_data *, int len, char *buff);
#define  SDL_CreateCursorFunc SDL_WarpMouseFunc +1
void es_createCursor(sdl_data *, int len, char *buff);
#define SDL_SetCursorFunc SDL_CreateCursorFunc+1
void es_setCursor(sdl_data *, int len, char *buff);
#define SDL_GetCursorFunc SDL_SetCursorFunc+1
void es_getCursor(sdl_data *, int len, char *buff);
#define SDL_FreeCursorFunc SDL_GetCursorFunc+1
void es_freeCursor(sdl_data *, int len, char *buff);
#define SDL_ShowCursorFunc SDL_FreeCursorFunc+1
void es_showCursor(sdl_data *, int len, char *buff);
void es_showCursor2(ErlDrvPort port, ErlDrvTermData caller, char *buff);

#define SDL_GetWMInfoFunc SDL_ShowCursorFunc+1
void es_getWMInfo(sdl_data *, int , char *);


#define SDL_EnableUNICODEFunc KEYBOARD_H +1
void es_enableUNICODE(sdl_data *, int , char *); 
#define SDL_EnableKeyRepeatFunc SDL_EnableUNICODEFunc+1
void es_enableKeyRepeat(sdl_data *, int , char *);
#define SDL_GetKeyNameFunc SDL_EnableKeyRepeatFunc+1
void es_getKeyName(sdl_data *, int , char *);
#define SDL_GetKeyStateFunc SDL_GetKeyNameFunc+1
void es_getKeyState(sdl_data *, int , char *);
#define SDL_GetModStateFunc SDL_GetKeyStateFunc+1
void es_getModState(sdl_data *, int , char *);
#define SDL_SetModStateFunc SDL_GetModStateFunc+1
void es_setModState(sdl_data *, int , char *);

#define SDL_GetAppStateFunc ACTIVE_H +1
void es_getAppState(sdl_data *, int , char *); 

#define SDL_NumJoysticksFunc JOYSTICK_H +1
void es_numJoysticks(sdl_data *, int , char *);       
#define SDL_JoystickNameFunc SDL_NumJoysticksFunc + 1
void es_joystick_name(sdl_data *, int , char *);
#define SDL_JoystickOpenFunc SDL_JoystickNameFunc + 1
void es_joystick_open(sdl_data *, int , char *);
#define SDL_JoystickOpenedFunc SDL_JoystickOpenFunc + 1
void es_joystick_opened(sdl_data *, int , char *);
#define SDL_JoystickIndexFunc SDL_JoystickOpenedFunc + 1
void es_joystick_index(sdl_data *, int , char *);
#define SDL_JoystickNumAxesFunc SDL_JoystickIndexFunc + 1
void es_joystick_numAxes(sdl_data *, int , char *);
#define SDL_JoystickNumBallsFunc SDL_JoystickNumAxesFunc + 1
void es_joystick_numBalls(sdl_data *, int , char *);
#define SDL_JoystickNumHatsFunc SDL_JoystickNumBallsFunc + 1
void es_joystick_numHats(sdl_data *, int , char *);
#define SDL_JoystickNumButtonsFunc SDL_JoystickNumHatsFunc + 1
void es_joystick_numButtons(sdl_data *, int , char *);
#define SDL_JoystickUpdateFunc SDL_JoystickNumButtonsFunc + 1
void es_joystick_update(sdl_data *, int , char *);
#define SDL_JoystickEventStateFunc SDL_JoystickUpdateFunc + 1
void es_joystick_eventState(sdl_data *, int , char *);
#define SDL_JoystickGetAxisFunc SDL_JoystickEventStateFunc + 1
void es_joystick_getAxis(sdl_data *, int , char *);
#define SDL_JoystickGetHatFunc SDL_JoystickGetAxisFunc + 1
void es_joystick_getHat(sdl_data *, int , char *);
#define SDL_JoystickGetButtonFunc SDL_JoystickGetHatFunc + 1
void es_joystick_getButton(sdl_data *, int , char *);
#define SDL_JoystickGetBallFunc SDL_JoystickGetButtonFunc + 1
void es_joystick_getBall(sdl_data *, int , char *);
#define SDL_JoystickCloseFunc SDL_JoystickGetBallFunc + 1
void es_joystick_close(sdl_data *, int , char *);
