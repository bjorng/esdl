#include "woggle_win.h"
#include <gl/gl.h>
#include <process.h>

#define HWIN_TITLE_MAX 256

typedef struct {
    HINSTANCE instance;
    char title[HWIN_TITLE_MAX];
    int width;
    int height;
    WogWindowToken token;
    WogCommandQue *comq;
    WogEventQue *ackq;
} WTParam;

typedef struct {
    int event_mask[WogListenAll];
    int delta_mouse;
    POINT last_mouse_pos;
    WogWindowToken token;
    WogCommandQue *comq;
    WogEventQue *ackq;
} WogThreadWindowData;

static DWORD thread_data_index = 0; 

static WogEventQue wog_evq;

WogEventQue *wog_global_event_que;

static HANDLE wog_instance_handle = NULL;

static CRITICAL_SECTION message_pool_event_crit;
static CRITICAL_SECTION message_pool_command_crit;

static WogEventItem *event_item_pool;
static WogCommandItem *command_item_pool;


#ifdef HARDDEBUG
static void debugf(char *format, ...)
{
    char buff[1024];
    char buff2[1024];
    DWORD dummy;
    va_list ap;
    va_start(ap, format);

    vsprintf(buff,format,ap);
    CharToOem(buff,buff2);

    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE),
	      buff2,strlen(buff2),&dummy,NULL);

    va_end(ap);
}
#endif

WogEventMessage *wog_alloc_event_message(void)
{
    WogEventMessage *msg;

    EnterCriticalSection(&message_pool_event_crit);
    if (!event_item_pool) {
	msg = malloc(sizeof(WogEventItem)); /* An item,not only a message */
	((WogEventItem *) msg)->next = NULL;
    } else {
	msg = (WogEventMessage *) event_item_pool;
	event_item_pool = event_item_pool->next;
    }
    LeaveCriticalSection(&message_pool_event_crit);
    return msg;
}

WogCommandMessage *wog_alloc_command_message(void)
{
    WogCommandMessage *msg;
    EnterCriticalSection(&message_pool_command_crit);
    if (!command_item_pool) {
	msg = malloc(sizeof(WogCommandItem)); /*An item,not only a message*/
	((WogCommandItem *) msg)->next = NULL;
    } else {
	msg = (WogCommandMessage *) command_item_pool;
	command_item_pool = command_item_pool->next;
    }
    LeaveCriticalSection(&message_pool_command_crit);
    return msg;
}

void wog_free_event_message(WogEventMessage *msg)
{
    WogEventItem *item = (WogEventItem *) msg;
    EnterCriticalSection(&message_pool_event_crit);
    item->next = event_item_pool;
    event_item_pool = item;
    LeaveCriticalSection(&message_pool_event_crit);
}

void wog_free_command_message(WogCommandMessage *msg)
{
    WogCommandItem *item = (WogCommandItem *) msg;
    EnterCriticalSection(&message_pool_command_crit);
    item->next = command_item_pool;
    command_item_pool = item;
    LeaveCriticalSection(&message_pool_command_crit);
}

void wog_post_event_message(WogEventQue *eq, WogEventMessage *msg)
{
    WogEventQue *q;
    WogEventItem *item = (WogEventItem *)msg;
    q = (eq) ? eq : &wog_evq;
    EnterCriticalSection(&(q->event_crit));
    item->next = NULL;
    if (q->event_que_first) {
	q->event_que_last->next = item;
	q->event_que_last = item;
    } else {
	q->event_que_first = q->event_que_last = item;
	SetEvent(q->message_in_event_que);
    }
    LeaveCriticalSection(&(q->event_crit));
}

void wog_merge_key(WogEventQue *eq, WogEventMessage *msg)
{
    WogEventItem *item = (WogEventItem *) msg;
    int do_free = 0;
    WogEventQue *q;

    q = (eq) ? eq : &wog_evq;
    EnterCriticalSection(&(q->event_crit));
    item->next = NULL;
    if (q->event_que_first) {
	WogEventMessage *last = (WogEventMessage *) (q->event_que_last);
	if (last->any.tag == msg->any.tag &&
	    last->any.token == msg->any.token &&
	    last->key_up_down.code == msg->key_up_down.code &&
	    last->key_up_down.scancode == msg->key_up_down.scancode && 
	    last->key_up_down.modifiers == msg->key_up_down.modifiers ) {
	    /* increase repeat...*/
	    ++(last->key_up_down.repeat);
	    do_free = 1;
#ifdef HARDDEBUG
	    debugf("merging keypress\n");
#endif
	} else {
	    q->event_que_last->next = item;
	    q->event_que_last = item;
	}
    } else {
	q->event_que_first = q->event_que_last = item;
	SetEvent(q->message_in_event_que);
    }
    LeaveCriticalSection(&(q->event_crit));
    if (do_free) {
	wog_free_event_message(msg);
    }
}


void wog_merge_char(WogEventQue *eq, WogEventMessage *msg)
{
    WogEventItem *item = (WogEventItem *) msg;
    int do_free = 0;
    WogEventQue *q;

    q = (eq) ? eq : &wog_evq;
    EnterCriticalSection(&(q->event_crit));
    item->next = NULL;
    if (q->event_que_first) {
	WogEventMessage *last = (WogEventMessage *) (q->event_que_last);
	if (last->any.tag == msg->any.tag &&
	    last->any.token == msg->any.token &&
	    last->a_char.code == msg->a_char.code &&
	    last->a_char.ch == msg->a_char.ch &&
	    last->a_char.scancode == msg->a_char.scancode && 
	    last->a_char.modifiers == msg->a_char.modifiers ) {
	    /* increase repeat...*/
	    ++(last->a_char.repeat);
	    do_free = 1;
#ifdef HARDDEBUG
	    debugf("merging char\n");
#endif
	} else {
	    q->event_que_last->next = item;
	    q->event_que_last = item;
	}
    } else {
	q->event_que_first = q->event_que_last = item;
	SetEvent(q->message_in_event_que);
    }
    LeaveCriticalSection(&(q->event_crit));
    if (do_free) {
	wog_free_event_message(msg);
    }
}


void wog_merge_mousemove(WogEventQue *eq, WogEventMessage *msg)
{
    WogEventItem *item = (WogEventItem *) msg;
    int do_free = 0;
    WogEventQue *q;

    q = (eq) ? eq : &wog_evq;
    EnterCriticalSection(&(q->event_crit));
    item->next = NULL;
    if (q->event_que_first) {
	WogEventMessage *last = (WogEventMessage *) (q->event_que_last);
	if (last->any.tag == WogMouseMove &&
	    last->any.token == msg->any.token &&
	    last->mouse_move.modifiers == msg->mouse_move.modifiers) {
	    /* replace x and y values */
	    last->mouse_move.x = msg->mouse_move.x;
	    last->mouse_move.y = msg->mouse_move.y;
	    do_free = 1;
#ifdef HARDDEBUG
	    debugf("merging mousemove\n");
#endif
	} else {
	    q->event_que_last->next = item;
	    q->event_que_last = item;
	}
    } else {
	q->event_que_first = q->event_que_last = item;
	SetEvent(q->message_in_event_que);
    }
    LeaveCriticalSection(&(q->event_crit));
    if (do_free) {
	wog_free_event_message(msg);
    }
}

void wog_merge_mousedelta(WogEventQue *eq, WogEventMessage *msg)
{
    WogEventItem *item = (WogEventItem *) msg;
    int do_free = 0;
    WogEventQue *q;

    q = (eq) ? eq : &wog_evq;
    EnterCriticalSection(&(q->event_crit));
    item->next = NULL;
    if (q->event_que_first) {
	WogEventMessage *last = (WogEventMessage *) (q->event_que_last);
	if (last->any.tag == WogMouseDelta &&
	    last->any.token == msg->any.token &&
	    last->mouse_move.modifiers == msg->mouse_move.modifiers) {
	    /* replace x and y values */
	    last->mouse_delta.x += msg->mouse_delta.x;
	    last->mouse_delta.y += msg->mouse_delta.y;
	    do_free = 1;
#ifdef HARDDEBUG
	    debugf("merging mousedelta\n");
#endif
	} else {
	    q->event_que_last->next = item;
	    q->event_que_last = item;
	}
    } else {
	q->event_que_first = q->event_que_last = item;
	SetEvent(q->message_in_event_que);
    }
    LeaveCriticalSection(&(q->event_crit));
    if (do_free) {
	wog_free_event_message(msg);
    }
}

WogEventMessage *wog_get_event_message(WogEventQue *eq)
{
    WogEventItem *item;
    WogEventQue *q;

    q = (eq) ? eq : &wog_evq;
    EnterCriticalSection(&(q->event_crit));
    if (!(q->event_que_first)) {
	/* Well behaving applications should not come here ? */
	item = NULL;
    } else {
	item = q->event_que_first;
	q->event_que_first = item->next;
	item->next = NULL;
	if (!(q->event_que_first)) {
	    q->event_que_last = NULL; /* Not necessary */
	    ResetEvent(q->message_in_event_que);
	}
    }
    LeaveCriticalSection(&(q->event_crit));
    return (WogEventMessage *) item;
}

void wog_post_command_message(WogCommandQue *cq, WogCommandMessage *msg)
{
    WogCommandItem *item = (WogCommandItem *)msg;
    EnterCriticalSection(&(cq->command_crit));
    item->next = NULL;
    if (cq->command_que_first) {
	cq->command_que_last->next = item;
	cq->command_que_last = item;
    } else {
	cq->command_que_first = cq->command_que_last = item;
	SetEvent(cq->message_in_command_que);
    }
    LeaveCriticalSection(&(cq->command_crit));
}

WogCommandMessage *get_command_message(WogCommandQue *cq)
{
    WogCommandItem *item;
    EnterCriticalSection(&(cq->command_crit));
    if (!(cq->command_que_first)) {
	/* Well behaving applications should not come here ? */
	item = NULL;
    } else {
	item = cq->command_que_first;
	cq->command_que_first = item->next;
	item->next = NULL;
	if (!(cq->command_que_first)) {
	    cq->command_que_last = NULL; /* Not necessary */
	    ResetEvent(cq->message_in_command_que);
	}
    }
    LeaveCriticalSection(&(cq->command_crit));
    return (WogCommandMessage *) item;
}

static void wog_init_event_que(WogEventQue *eq) 
{
    memset(eq,0,sizeof(WogEventQue));
    InitializeCriticalSection(&(eq->event_crit));
    eq->message_in_event_que = CreateEvent(NULL,TRUE,FALSE,NULL);
    eq->event_que_first = eq->event_que_last = NULL;
}

static void wog_init_command_que(WogCommandQue *cq) 
{
    memset(cq,0,sizeof(WogCommandQue));
    InitializeCriticalSection(&(cq->command_crit));
    cq->message_in_command_que = CreateEvent(NULL,TRUE,FALSE,NULL);
    cq->command_que_first = cq->command_que_last = NULL;
}

static void wog_destroy_event_que(WogEventQue *eq)
{
    WogEventItem *ei;
    void *life;

    /* Critical sections cannot be destroyed */

    CloseHandle(eq->message_in_event_que);
    
    for(ei = eq->event_que_first; ei != NULL; ei = life) {
	life = ei->next;
	free(ei);
    }
}

static void wog_destroy_command_que(WogCommandQue *cq)
{
    WogCommandItem *ci;
    void *life;

    /* Critical sections cannot be destroyed */

    CloseHandle(cq->message_in_command_que);
    
    for(ci = cq->command_que_first; ci != NULL; ci = life) {
	life = ci->next;
	free(ci);
    }
}

/* 
 * enable_opengl and disable_opengl taken from GLSAMPLE.CPP
 * by Blaine Hodge
 * Thanks!
 */
static void enable_opengl(HWND hWnd, HDC * hDC, HGLRC * hRC, int depth)
{
	PIXELFORMATDESCRIPTOR pfd;
	int format;
	
	/* get the device context (DC) */
	*hDC = GetDC( hWnd );
	
	/* set the pixel format for the DC */
	ZeroMemory( &pfd, sizeof( pfd ) );
	pfd.nSize = sizeof( pfd );
	pfd.nVersion = 1;
	pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
	pfd.iPixelType = PFD_TYPE_RGBA;
	/*
	 * The parameter depth is the color depth, not the Z-buffer depth,
	 * which has gotten me deeply confused more than once...
	 * I have no interface for setting the Z-buffer depth, 
	 * When I try 32 I get 16, when I try 24 I get 24...
	 */
	pfd.cColorBits = depth;
	pfd.cDepthBits = 24;
	pfd.iLayerType = PFD_MAIN_PLANE;
	format = ChoosePixelFormat( *hDC, &pfd );
	SetPixelFormat( *hDC, format, &pfd );
	
	/* create and enable the render context (RC) */
	*hRC = wglCreateContext( *hDC );
	wglMakeCurrent( *hDC, *hRC );
	
}

static void disable_opengl(HWND hWnd, HDC hDC, HGLRC hRC)
{
	wglMakeCurrent( NULL, NULL );
	wglDeleteContext( hRC );
	ReleaseDC( hWnd, hDC );
}

static WogModifiers check_keystate(WPARAM mousedata) 
{
    WogModifiers m = 0U;

#define PRESSED(KEY) ((((unsigned short) GetKeyState(VK_##KEY)) >> 15) == 1)

#ifdef HARDDEBUG
    debugf("PRESSED(CONTROL) -> 0x%08x\n",(unsigned) PRESSED(CONTROL));
#endif
    
    if (PRESSED(CONTROL)) 
	m |= WOG_MODIFIER_CTRL;
    if (PRESSED(SHIFT))
	m |= WOG_MODIFIER_SHIFT;
    if (PRESSED(MENU))
	m |= WOG_MODIFIER_ALT;

    if (mousedata & MK_LBUTTON)
	m |= WOG_MODIFIER_MOUSE_LEFT;
    if (mousedata & MK_RBUTTON)
	m |= WOG_MODIFIER_MOUSE_RIGHT;
    if (mousedata & MK_MBUTTON)
	m |= WOG_MODIFIER_MOUSE_MIDDLE;

    return m;
#undef PRESSED
}

#define MASK(N) (~(0xFFFFFFFFUL << (N)))

#define GETBITS(From, To, X) ((((unsigned long) (X)) >> (From)) & \
                              MASK((To) - (From) + 1))     

LRESULT CALLBACK wog_process_events(HWND window, UINT message, 
				    WPARAM wp, LPARAM lp)
{
    WogEventMessage *wem;
    LRESULT ret;
    RECT rect;
    WogMouseButton button;
    WogEventTag tag;
    SHORT scanret;
    WogThreadWindowData *twd = TlsGetValue(thread_data_index);
#ifdef HARDDEBUG
    debugf("Window message %d\n",message);
#endif
    switch(message){
    case WM_ACTIVATE:
	if ((twd->event_mask[WogListenActivate])) {
	    wem = wog_alloc_event_message();
	    wem->any.token = twd->token;
	    wem->any.tag = WogActivate;
	    wem->activate.active = (LOWORD(wp) != WA_INACTIVE);
	    wem->activate.iconic = (int) HIWORD(wp);
	    wog_post_event_message(NULL,wem);
	}
	return DefWindowProc(window, message, wp, lp);
    case WM_SIZE:
	if (!(twd->event_mask[WogListenResize]))
	    return DefWindowProc(window, message, wp, lp);
	wem = wog_alloc_event_message();
	wem->any.token = twd->token;
	wem->any.tag = WogResize;
	wem->resize.width = ((unsigned short) LOWORD(lp));
	wem->resize.height = ((unsigned short) HIWORD(lp));
	wog_post_event_message(NULL,wem);
	return 0;
    case WM_PAINT:
	if (!(twd->event_mask[WogListenPaint]))
	    return DefWindowProc(window, message, wp, lp);
	wem = wog_alloc_event_message();
	wem->any.token = twd->token;
	wem->any.tag = WogPaint;
	if (!GetUpdateRect(window,&rect,FALSE)) {
	    wem->paint.x1 = 0;
	    wem->paint.y1 = 0;
	    wem->paint.x2 = 0;
	    wem->paint.y2 = 0;
	} else { 
	    wem->paint.x1 = rect.left;
	    wem->paint.y1 = rect.top;
	    wem->paint.x2 = rect.right;
	    wem->paint.y2 = rect.bottom;
	}
	ret = DefWindowProc(window, message, wp, lp);
	wog_post_event_message(NULL,wem);
	return ret;
	break;        
    case WM_CREATE:
	wem = wog_alloc_event_message();
	wem->any.token = twd->token;
	wem->any.tag = WogWindowCreated;
	wem->window_created.window = window;
	wog_post_event_message(twd->ackq,wem);
	break;        
    case WM_MOUSEMOVE:
	if (!(twd->event_mask[WogListenMouseMove]))
	    return DefWindowProc(window, message, wp, lp);
	if (twd->delta_mouse) {
	    POINT cur_mouse_pos;
	    GetCursorPos(&cur_mouse_pos);
	    if (cur_mouse_pos.x == twd->last_mouse_pos.x && 
		cur_mouse_pos.y == twd->last_mouse_pos.y) {
		return DefWindowProc(window, message, wp, lp);
	    }
	    wem = wog_alloc_event_message();
	    wem->any.token = twd->token;
	    wem->any.tag = WogMouseDelta;
	    wem->mouse_delta.x = cur_mouse_pos.x - twd->last_mouse_pos.x;
	    wem->mouse_delta.y = cur_mouse_pos.y - twd->last_mouse_pos.y;
	    wem->mouse_delta.modifiers = check_keystate(wp);
	    SetCursorPos(twd->last_mouse_pos.x, twd->last_mouse_pos.y);
	    ret = DefWindowProc(window, message, wp, lp);
	    wog_merge_mousedelta(NULL,wem);
	} else {
	    wem = wog_alloc_event_message();
	    wem->any.tag = WogMouseMove;
	    wem->any.token = twd->token;
	    wem->mouse_move.x = ((short) LOWORD(lp));
	    wem->mouse_move.y = ((short) HIWORD(lp));
	    wem->mouse_move.modifiers = check_keystate(wp);
	    ret = DefWindowProc(window, message, wp, lp);
	    wog_merge_mousemove(NULL,wem);
	}
	return ret;
    case WM_LBUTTONDOWN:
	button = WogMouseLeft;
	tag = WogMouseDown;
	goto do_mouse_button;
    case WM_RBUTTONDOWN:
	button = WogMouseRight;
	tag = WogMouseDown;
	goto do_mouse_button;
    case WM_MBUTTONDOWN:
	button = WogMouseMiddle;
	tag = WogMouseDown;
	goto do_mouse_button;
    case WM_LBUTTONUP:
	button = WogMouseLeft;
	tag = WogMouseUp;
	goto do_mouse_button;
    case WM_RBUTTONUP:
	button = WogMouseRight;
	tag = WogMouseUp;
	goto do_mouse_button;
    case WM_MBUTTONUP:
	button = WogMouseMiddle;
	tag = WogMouseUp;
    do_mouse_button:
	if (!(twd->event_mask[WogListenMouseMove]))
	    return DefWindowProc(window, message, wp, lp);
	wem = wog_alloc_event_message();
	wem->any.token = twd->token;
	wem->any.tag = tag;
	wem->mouse_up_down.modifiers = check_keystate(wp);
	wem->mouse_up_down.button = button;
	wem->mouse_up_down.x = ((short) LOWORD(lp));
	wem->mouse_up_down.y = ((short) HIWORD(lp));
	ret = DefWindowProc(window, message, wp, lp);
	if (tag == WogMouseDown){
	    SetCapture(window);
	} else {
	    ReleaseCapture();
	}
	wog_post_event_message(NULL,wem);
	return ret;
    case WM_MOUSEWHEEL:
	if (!(twd->event_mask[WogListenMouseWheel]))
	    return DefWindowProc(window, message, wp, lp);
	wem = wog_alloc_event_message();
	wem->any.token = twd->token;
	wem->any.tag = WogMouseWheel;
	wem->mouse_wheel.delta = (short) HIWORD(wp);
	wem->mouse_wheel.x = (short) LOWORD(lp);    
	wem->mouse_wheel.y = (short) HIWORD(lp);
	wem->mouse_wheel.modifiers = check_keystate(0);
	wog_post_event_message(NULL,wem);
	return 0;
    case WM_KEYDOWN:
	if (!(twd->event_mask[WogListenKeyDown]))
	    return DefWindowProc(window, message, wp, lp);
	wem = wog_alloc_event_message();
	wem->any.token = twd->token;
	wem->any.tag = WogKeyDown;
	wem->key_up_down.repeat = (int) GETBITS(0,15,lp);
	wem->key_up_down.code = (unsigned char) wp;
	wem->key_up_down.scancode = (int) GETBITS(16,23,lp);
	ret = DefWindowProc(window, message, wp, lp);
	wem->key_up_down.modifiers = check_keystate(0);
	wog_merge_key(NULL,wem);
	return ret;
    case WM_KEYUP:
	if (!(twd->event_mask[WogListenKeyUp]))
	    return DefWindowProc(window, message, wp, lp);
	wem = wog_alloc_event_message();
	wem->any.token = twd->token;
	wem->any.tag = WogKeyUp;
	wem->key_up_down.repeat = (int) GETBITS(0,15,lp);
	wem->key_up_down.code = (unsigned char) wp;
	wem->key_up_down.scancode = (int) GETBITS(16,23,lp);
	ret = DefWindowProc(window, message, wp, lp);
	wem->key_up_down.modifiers = check_keystate(0);
	wog_merge_key(NULL,wem);
	return ret;
    case WM_CHAR:
    case WM_SYSCHAR:
	if (GETBITS(31,31,lp)) {
	    /* Released */
	    return DefWindowProc(window, message, wp, lp);
	}

	if (!(twd->event_mask[WogListenChar])) {
	    if (twd->event_mask[WogListenKeyDown] || 
		twd->event_mask[WogListenKeyUp]) {
		return 0; /* Avoid char beeps when listening for keys */
	    } else {
		return DefWindowProc(window, message, wp, lp);
	    }
	}

	wem = wog_alloc_event_message();
	wem->any.token = twd->token;
	wem->any.tag = WogChar;
	wem->a_char.repeat = (int) GETBITS(0,15,lp);
	scanret = VkKeyScan((unsigned char) wp);
	wem->a_char.code = (scanret < 0) ? -1 : scanret & 0xFF;
	wem->a_char.ch = (unsigned char) wp;
	wem->a_char.scancode = (int) GETBITS(16,23,lp);
	/* VkKeyScan could be used for part of the following, 
	   place for improvements */
	wem->a_char.modifiers = check_keystate(0);
	wog_merge_char(NULL,wem);
	return 0;
    case WM_CLOSE:
	if (!(twd->event_mask[WogListenClose])) {
	    PostQuitMessage(0); /* XXX On exit only? */ 
	    return 0;
	}
	wem = wog_alloc_event_message();
	wem->any.token = twd->token;
	wem->any.tag = WogClose;
	wog_post_event_message(NULL,wem);
	return 0;
    case WM_DESTROY:
	PostQuitMessage(0); /* XXX */
	return 0;
    default:
	return DefWindowProc(window, message, wp, lp);
    }
    return 0;
}
  


unsigned WINAPI wog_window_thread(void *params)
{
    WTParam *wtp = (WTParam *) params;
    WogThreadWindowData *twd;
    WogCommandQue *cq = wtp->comq;
    WogWindowToken token = wtp->token;
    WNDCLASS wc;
    HWND window;
    DWORD wres;
    int i;

    twd = malloc(sizeof(WogThreadWindowData));
    memset(twd,0,sizeof(WogThreadWindowData));
    TlsSetValue(thread_data_index, twd);
    for(i = 0; i < WogListenAll; ++i) {
	twd->event_mask[i] = 0; /* all events off */
    }
    twd->delta_mouse = FALSE;
    twd->comq = cq;
    twd->ackq = wtp->ackq;
    twd->token = wtp->token;
    memset(&(twd->last_mouse_pos),0,sizeof(POINT));

    wc.style         = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
    wc.lpfnWndProc   = (WNDPROC)wog_process_events;
    wc.cbClsExtra    = 0;
    wc.cbWndExtra    = 0;
    wc.hInstance     = wtp->instance;
    wc.hIcon         = LoadIcon (NULL, IDI_APPLICATION);
    wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = ((HBRUSH) (COLOR_WINDOW+1));
    wc.lpszMenuName  = NULL /* application_name */;
    wc.lpszClassName = wtp->title;
    
    RegisterClass(&wc);


    window =  CreateWindowEx(0, /*WS_EX_ACCEPTFILES ?*/
			     wtp->title,
			     wtp->title,
			     WS_TILEDWINDOW | WS_VISIBLE,
			     CW_USEDEFAULT,
			     CW_USEDEFAULT,
			     wtp->width,
			     wtp->height,
			     NULL, 
			     NULL, 
			     wtp->instance, 
			     NULL
			     );

    if (window == NULL) {
	goto bummer;
    }


    for (;;) {
	
      wres = MsgWaitForMultipleObjects(1,&(cq->message_in_command_que),
				       FALSE,INFINITE,QS_ALLINPUT);
      if (wres == WAIT_OBJECT_0) {
	WogCommandMessage *msg = get_command_message(cq);
	if (msg->tag < WogListenAll) {
	    twd->event_mask[msg->tag] = msg->onoff;
	} else if (msg->tag == WogListenAll) {
	    for(i = 0; i < WogListenAll; ++i) {
		twd->event_mask[i] = msg->onoff; /* all events off */
	    }
	} else if (msg->tag == WogGrabMouse) {
	    if (msg->onoff) {
		SetCapture(window);
	    } else {
		ReleaseCapture();
	    }
	} else if (msg->tag == WogCloseWindow) {
	    PostQuitMessage(0);
	} else if (msg->tag == WogDeltaMouse) {
	    if (msg->onoff && !(twd->delta_mouse)) {
		ShowCursor(FALSE);
		twd->delta_mouse = TRUE;
		GetCursorPos(&(twd->last_mouse_pos));
	    } else if (!(msg->onoff) && twd->delta_mouse) {
		ShowCursor(TRUE);
		twd->delta_mouse = FALSE;
	    }
	}
	/* ... */
	wog_free_command_message(msg);
      } else if (wres == WAIT_OBJECT_0+1) {
	MSG msg; 
	while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) { 
	  if (msg.message == WM_QUIT) {
	      WogEventMessage *wem = wog_alloc_event_message();
	      wem->any.token = token;
	      wem->any.tag = WogGoodbye;
	      wog_post_event_message(NULL,wem);
	      return 0;
	  } else {
	      TranslateMessage(&msg);
	      DispatchMessage(&msg); 
	  }
	}
      } else {
      bummer:
	  MessageBox(window,"Fatal error in Wait for multiple objects",
		     "Woggle error",MB_OK);
	  exit(1);
      }
    }
    free(twd); /* Message ques freed by caller */
    free(params);
    return 0;
}

void wog_close_window(WogWindowData *wd)
{
    WogCommandMessage *cmd;
    
    if (wd->hWindow == NULL) {
	return;
    }
    disable_opengl(wd->hWindow, wd->hdc, wd->hrc);
    cmd = wog_alloc_command_message();
    cmd->tag = WogCloseWindow;
    wog_post_command_message(wd->comq,cmd);
    if (WaitForSingleObject(wd->hThrd,INFINITE) != WAIT_OBJECT_0) {
	MessageBox(NULL,"Now what? cannot wait for thread!",
		   "Woggle warning",MB_OK);
    }
    wog_destroy_command_que(wd->comq);
    free(wd->comq);
    wog_destroy_event_que(wd->ackq);
    free(wd->ackq);
    CloseHandle(wd->hWindow);
    CloseHandle(wd->hThrd);
}

void wog_init_instance(HANDLE instance)
/*
 * Only to be called when statically linked.
 */
{
    InitializeCriticalSection(&message_pool_event_crit);
    InitializeCriticalSection(&message_pool_command_crit);
    event_item_pool = NULL;  
    command_item_pool = NULL;
    thread_data_index = TlsAlloc();
    wog_init_event_que(&wog_evq); /* Global event que */
    wog_global_event_que = &wog_evq;
    wog_instance_handle = instance;
}

HANDLE wog_create_window(WogWindowToken token, char *title, 
			 int width, int height, int depth,
			 WogWindowData *wd)
{
    WogEventMessage *msg;
    WTParam *params = malloc(sizeof(WTParam));

    if (wog_instance_handle == NULL) {
	MessageBox(NULL, "Instance handle not set when creating window!", 
		   "Woggle error", MB_OK);
	exit(1);
    }
    params->instance = wog_instance_handle; /*GetModuleHandle(NULL); 
					      does not work when we're a DLL!*/
    strncpy(params->title, title, HWIN_TITLE_MAX);
    params->title[HWIN_TITLE_MAX-1] = '\0';
    params->width = width;
    params->height = height;
    params->token = token;
    params->comq = malloc(sizeof(WogCommandQue));
    params->ackq = malloc(sizeof(WogEventQue));
    wog_init_command_que(params->comq);
    wog_init_event_que(params->ackq);
    wd->token = params->token;
    wd->comq = params->comq;
    wd->ackq = params->ackq;
    if (!(wd->hThrd = (HANDLE) _beginthreadex(NULL,0,&wog_window_thread,
					      params, 0, &(wd->thrdid)))){
	MessageBox(NULL,"Thread create failed!","Woggle error",MB_OK);
	exit(1);
    }
    WaitForSingleObject(WOG_ACK_HANDLE(wd),INFINITE);
    msg = wog_get_event_message(WOG_ACK_QUE(wd));
    if (msg->any.tag != WogWindowCreated || msg->any.token != token) {
	MessageBox(NULL,"Window created not first message!",
		   "Woggle error",MB_OK);
	exit(1);
    }
    wd->hWindow = msg->window_created.window;
    enable_opengl(msg->window_created.window, &(wd->hdc), &(wd->hrc), depth);
    return WOG_GLOBAL_EVENT_HANDLE();
}

void wog_swap_buffers(WogWindowData *wd)
{
    SwapBuffers(wd->hdc);
}

unsigned wog_get_tick(void)
{
    return (unsigned) GetTickCount();
}

void wog_set_current_window(WogWindowData *wd)
{
    wglMakeCurrent( wd->hdc, wd->hrc );
}

int wog_list_modes(WogRes **res) {
    static WogRes *buffer = NULL;
    static int buffer_size = 0;
    static DWORD i;
    DEVMODE dm;
    
    if (!buffer_size) {
	buffer = malloc ((buffer_size = 10) * sizeof(WogRes));
	i = 0;
	for (;;) {
	    memset(&dm,0,sizeof(DEVMODE));
	    dm.dmSize = sizeof(DEVMODE);
	    dm.dmDriverExtra = 0;
	    if (!EnumDisplaySettings(NULL,
				     (i) ? (i-1) : ENUM_CURRENT_SETTINGS,&dm))
		break;
	    if (i >= buffer_size) {
		buffer = realloc(buffer,
				 (buffer_size += 10) * sizeof(WogRes)); 
	    }
	    buffer[i].cdepth = dm.dmBitsPerPel;
	    buffer[i].freq = dm.dmDisplayFrequency;
	    buffer[i].w = dm.dmPelsWidth;
	    buffer[i].h = dm.dmPelsHeight;
	    ++i;
	}
    }
    *res = buffer;
    return i;
}
int wog_get_attr(const WogWindowData *wd, int item)
{
    PIXELFORMATDESCRIPTOR  pfd;
    int i;

    if (!(i = GetPixelFormat(wd->hdc))) {
	return -1;
    }
    if (!DescribePixelFormat(wd->hdc, i, sizeof(pfd), &pfd)) {
	return -1;
    }
    switch (item) {
    case WOG_ATTR_RED_SIZE:
	return (int) (pfd.cRedBits);
    case WOG_ATTR_GREEN_SIZE:
	return (int) (pfd.cGreenBits);
    case WOG_ATTR_BLUE_SIZE:
	return (int) (pfd.cBlueBits);
    case WOG_ATTR_ALPHA_SIZE: /* Not supported on windows I think... */
	return (int) (pfd.cAlphaBits);
    case WOG_ATTR_DEPTH_SIZE:
	return (int) (pfd.cDepthBits); /* Z buffer depth */
    case WOG_ATTR_BUFFER_SIZE:
	return (int) (pfd.cColorBits); /* color depth */
    /* I actually provide no interface to change this, 
       it should always be true */
    case WOG_ATTR_DOUBLEBUFFER:
	return (int) (!!(pfd.dwFlags & PFD_DOUBLEBUFFER));
    default:
	return -1;
    }
}
int wog_get_wmattr(const WogWindowData *wd, int item)
{
    switch (item) {
    case WOG_WMATTR_MAXIMIZED:
	return (int) IsZoomed(wd->hWindow);
    default:
	return -1;
    }
}

int wog_listen(const WogWindowData *wd, WogCommandTag tag, int onoff)
{
    WogCommandMessage *cmd;
    cmd = wog_alloc_command_message();
    cmd->tag = tag;
    cmd->onoff = onoff;
    wog_post_command_message(WOG_COMMAND_QUE(wd),cmd);
    return 0;
}

BOOL APIENTRY DllMain(HANDLE hModule, 
                      DWORD  ul_reason_for_call, 
                      LPVOID lpReserved)
{
    switch( ul_reason_for_call ) {
    case DLL_PROCESS_ATTACH:
	InitializeCriticalSection(&message_pool_event_crit);
	InitializeCriticalSection(&message_pool_command_crit);
	event_item_pool = NULL;  
	command_item_pool = NULL;
	wog_instance_handle=hModule;
	thread_data_index = TlsAlloc();
	wog_init_event_que(&wog_evq); /* Global event que */
	wog_global_event_que = &wog_evq;
#ifdef DLL_EVENT_DEBUG
	sprintf(dbuff,"DLL_PROCESS_ATTACH:0x%08X",(unsigned) hModule);
	MessageBox(NULL,dbuff,"DllMain",MB_OK);
#endif
	break;
    case DLL_THREAD_ATTACH:
#ifdef DLL_EVENT_DEBUG
	MessageBox(NULL,"DLL_THREAD_ATTACH","DllMain",MB_OK);
#endif
	break;
    case DLL_THREAD_DETACH:
#ifdef DLL_EVENT_DEBUG
	MessageBox(NULL,"DLL_THREAD_DETACH","DllMain",MB_OK);
#endif
	break;
    case DLL_PROCESS_DETACH:
	TlsFree(thread_data_index);
	wog_destroy_event_que(&wog_evq); /* Global event que */
#ifdef DLL_EVENT_DEBUG
	MessageBox(NULL,"DLL_PROCESS_DETACH","DllMain",MB_OK);
#endif
	break;
    }
    return TRUE;
}



