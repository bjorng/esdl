#ifndef _WOGGLE_H
#define _WOGGLE_H

#ifdef WIN32
#include <windows.h>
#include <winuser.h>
#define  WOG_WIN_HANDLE   HANDLE
#else
/* X11 */
#include <GL/glx.h>
#define  WOG_WIN_HANDLE   void*
#endif

/* Graphic/OpenGL attributes */
#define WOG_ATTR_RED_SIZE 1
#define WOG_ATTR_GREEN_SIZE 2
#define WOG_ATTR_BLUE_SIZE 3
#define WOG_ATTR_DEPTH_SIZE 4
#define WOG_ATTR_DOUBLEBUFFER 5
#define WOG_ATTR_ALPHA_SIZE 6
#define WOG_ATTR_BUFFER_SIZE 7

/* Window system related attributes */
#define WOG_WMATTR_MAXIMIZED 1

typedef enum {
    WogWindowCreated,
    WogMouseDown,
    WogMouseUp,
    WogMouseWheel,
    WogChar,
    WogKeyDown,
    WogKeyUp,
    WogMouseMove,
    WogMouseDelta,
    WogResize,
    WogActivate,
    WogClose,
    WogGoodbye,
    WogPaint
} WogEventTag;

typedef enum {
    WogListenMouseDown,
    WogListenMouseUp,
    WogListenMouseWheel,
    WogListenMouseMove,
    WogListenKeyUp,
    WogListenKeyDown,
    WogListenChar,
    WogListenResize,
    WogListenPaint,
    WogListenActivate,
    WogListenClose,
    WogListenAll,
    WogDeltaMouse,
    WogCloseWindow,
    WogGrabMouse
} WogCommandTag;

typedef enum {
    WogMouseLeft,
    WogMouseMiddle,
    WogMouseRight
} WogMouseButton;

#define WOG_MODIFIER_SHIFT 1
#define WOG_MODIFIER_CTRL 2
#define WOG_MODIFIER_ALT 4
#define WOG_MODIFIER_MOUSE_LEFT 8
#define WOG_MODIFIER_MOUSE_MIDDLE 16
#define WOG_MODIFIER_MOUSE_RIGHT 32

typedef unsigned int WogModifiers;

typedef void * WogWindowToken;

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
#ifdef WIN32
    HWND window;
#endif
} WogEvWindowCreated;

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
    WogMouseButton button;
    int x;
    int y;
    WogModifiers modifiers;
} WogEvMouseUpDown;

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
    int delta;
    int x;
    int y;
    WogModifiers modifiers; 
} WogEvMouseWheel;

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
    int code;
    int scancode;
    int repeat;
    WogModifiers modifiers; /* Typical Alt */
} WogEvKeyUpDown;

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
    int code;
    int scancode;
    int ch;
    int repeat;
    WogModifiers modifiers; /* Typical Alt */
} WogEvChar;

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
    int x;
    int y;
    WogModifiers modifiers;
} WogEvMouseMove;

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
    int x;
    int y;
    WogModifiers modifiers;
} WogEvMouseDelta;

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
    int width;
    int height;
} WogEvResize;    

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
    int active;
    int iconic;
} WogEvActivate;    

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
} WogEvClose;    

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
} WogEvGoodbye;    

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
    int x1;
    int y1;
    int x2;
    int y2;
} WogEvPaint;

typedef struct {
    WogEventTag tag;
    WogWindowToken token;
} WogEvAny;
    
typedef union {
    WogEvAny any;
    WogEvWindowCreated window_created;
    WogEvMouseUpDown mouse_up_down;
    WogEvMouseWheel mouse_wheel;
    WogEvKeyUpDown key_up_down;
    WogEvChar a_char; 
    WogEvMouseMove mouse_move;
    WogEvMouseDelta mouse_delta;
    WogEvResize resize;
    WogEvPaint paint;
    WogEvActivate activate;
    WogEvClose close;
    WogEvGoodbye goodbye;
} WogEventMessage;

typedef struct {
    WogCommandTag tag;
    int onoff;
} WogCommandMessage;

typedef struct wog_event_item {
    WogEventMessage msg;
    struct wog_event_item *next;
} WogEventItem;

typedef struct wog_command_item {
    WogCommandMessage cmd;
    struct wog_command_item *next;
} WogCommandItem;

typedef struct {
    WogEventItem *event_que_first;
    WogEventItem *event_que_last;
#ifdef WIN32
    CRITICAL_SECTION event_crit;
    HANDLE message_in_event_que; /* Event Handle */
#endif
} WogEventQue;

typedef struct {
    WogCommandItem *command_que_first;
    WogCommandItem *command_que_last;
#ifdef WIN32
    CRITICAL_SECTION command_crit;
    HANDLE message_in_command_que; /* Event Handle */
#endif
} WogCommandQue;

typedef struct {
    WogWindowToken token;
    WogCommandQue *comq;
    WogEventQue *ackq;
    unsigned thrdid;
#ifdef WIN32
    HANDLE hThrd;
    HANDLE hWindow;
    HDC hdc;
    HGLRC hrc;
#else /* X11 */
    Display* display;
    int screen;
    Window window;
    XVisualInfo* vinfo;
    GLXContext ctx;
    Atom deleteAtom;
#endif
} WogWindowData;
       
typedef struct {
    unsigned char cdepth;
    unsigned freq;
    unsigned short w;
    unsigned short h;
} WogRes;

#define WOG_COMMAND_QUE(PWogWindowData) \
    ((PWogWindowData)->comq)
#define WOG_ACK_QUE(PWogWindowData) \
    ((PWogWindowData)->ackq)
#define WOG_ACK_HANDLE(PWogWindowData) \
    ((PWogWindowData)->ackq->message_in_event_que)

/* The real event que is global and slightly opaque */
extern WogEventQue *wog_global_event_que;

#define WOG_GLOBAL_EVENT_QUE() \
    (NULL)
#define WOG_GLOBAL_EVENT_HANDLE() \
    (wog_global_event_que->message_in_event_que)

void wog_init_instance(WOG_WIN_HANDLE instance);
WogCommandMessage *wog_alloc_command_message(void);
void wog_free_event_message(WogEventMessage *msg);
void wog_free_command_message(WogCommandMessage *msg);
WogEventMessage *wog_get_event_message(WogEventQue *eq);
void wog_post_command_message(WogCommandQue *cq, WogCommandMessage *msg);
WOG_WIN_HANDLE wog_create_window(WogWindowToken token, const char *title, 
			 int width, int height, int depth,
			 WogWindowData *wd);
void wog_close_window(WogWindowData *wd);
void wog_swap_buffers(WogWindowData *wd);
unsigned wog_get_tick(void);
int wog_list_modes(WogRes **res);
void wog_set_current_window(WogWindowData *wd);
int wog_get_attr(const WogWindowData *wd, int item);
int wog_get_wmattr(const WogWindowData *wd, int item);
int wog_listen(const WogWindowData *wd, WogCommandTag tag, int onoff);

#endif
