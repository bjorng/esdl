#include "woggle.h"
#include <GL/gl.h>
#include <stdio.h>


#if 0 && defined(WIN32)
#include <fcntl.h>

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


void inigl2(void)
{
}

static void glpaint2(HDC hDC, float theta) { 	
  glClearColor( 0.0f, 0.0f, 0.0f, 0.0f );
  glClear( GL_COLOR_BUFFER_BIT );
  
  glPushMatrix();
  glRotatef( theta, 0.0f, 0.0f, 1.0f );
  glBegin( GL_TRIANGLES );
  glColor3f( 1.0f, 0.0f, 0.0f ); glVertex2f( 0.0f, 1.0f );
  glColor3f( 0.0f, 1.0f, 0.0f ); glVertex2f( 0.87f, -0.5f );
  glColor3f( 0.0f, 0.0f, 1.0f ); glVertex2f( -0.87f, -0.5f );
  glEnd();
  glPopMatrix();
  
  SwapBuffers( hDC );
}

void inigl(void)
{
    glViewport(0,0,640,480);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(-2.0,2.0,-2.0,2.0,-20.0,20.0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    debugf("%s\n",glGetString(GL_VENDOR));
}


static void glpaint(HDC hDC, float theta) { 	
    float cube[][3] ={{0.5f,0.5f,-0.5f},
		    { 0.5f, -0.5f, -0.5f},
		    {-0.5f, -0.5f, -0.5f},
		    {-0.5f,  0.5f, -0.5f},
		    {-0.5f,  0.5f,  0.5f},
		    { 0.5f,  0.5f,  0.5f},
		    { 0.5f, -0.5f,  0.5f},
		    {-0.5f, -0.5f,  0.5f}};
    float colors[][3] = {{ 1.0f,  1.0f,  0.0f}, 
			{ 1.0f,  0.0f,  0.0f},
			{ 0.0f,  0.0f,  0.0f},
			{ 0.0f,  1.0f,  0.0f},
			{ 0.0f,  1.0f,  1.0f},
			{ 1.0f,  1.0f,  1.0f},
			{ 1.0f,  0.0f,  1.0f},
			{ 0.0f,  0.0f,  1.0f}};

    /*debugf("%f %f %f\n",colors[0][0],colors[0][1],colors[0][2]);*/
    glMatrixMode(GL_MODELVIEW);
    glRotatef(5.0f,1.0f,1.0f,1.0f);
    glClearColor(0.0,0.0,0.0,1.0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glBegin(GL_QUADS);
    glColor3fv(colors[0]);
    glVertex3fv(cube[0]);
    glColor3fv(colors[1]);
    glVertex3fv(cube[1]);
    glColor3fv(colors[2]);
    glVertex3fv(cube[2]);
    glColor3fv(colors[3]);
    glVertex3fv(cube[3]);
    
    glColor3fv(colors[3]);
    glVertex3fv(cube[3]);
    glColor3fv(colors[4]);
    glVertex3fv(cube[4]);
    glColor3fv(colors[7]);
    glVertex3fv(cube[7]);
    glColor3fv(colors[2]);
    glVertex3fv(cube[2]);
    
    glColor3fv(colors[0]);
    glVertex3fv(cube[0]);
    glColor3fv(colors[5]);
    glVertex3fv(cube[5]);
    glColor3fv(colors[6]);
    glVertex3fv(cube[6]);
    glColor3fv(colors[1]);
    glVertex3fv(cube[1]);
#define element(N,B) B[N-1]
    glColor3fv(element(6, colors));
    glVertex3fv(element(6, cube));
    glColor3fv(element(5, colors));
    glVertex3fv(element(5, cube));
    glColor3fv(element(8, colors));
    glVertex3fv(element(8, cube));
    glColor3fv(element(7, colors));
    glVertex3fv(element(7, cube));
    
    glColor3fv(element(6, colors));
    glVertex3fv(element(6, cube));
    glColor3fv(element(1, colors));
    glVertex3fv(element(1, cube));
    glColor3fv(element(4, colors));
    glVertex3fv(element(4, cube));
    glColor3fv(element(5, colors));
    glVertex3fv(element(5, cube));
    
    glColor3fv(element(7, colors));
    glVertex3fv(element(7, cube));
    glColor3fv(element(2, colors));
    glVertex3fv(element(2, cube));
    glColor3fv(element(3, colors));
    glVertex3fv(element(3, cube));
    glColor3fv(element(8, colors));
    glVertex3fv(element(8, cube));
    glEnd();
    SwapBuffers(hDC);
}

#define MSGDEBUG

/* Test */
int APIENTRY WinMain(HINSTANCE instance, HINSTANCE prev_instance, LPSTR command_line,
	int cmd_show)
{
    WogWindowData wd,wd2;
    HANDLE qh,qh2;
    WogEventMessage *msg;
    WogCommandMessage *cmd;

    float theta = 0.0f;
    float theta2 = 0.0f;

    wog_init_instance(instance);
    FreeConsole();
    AllocConsole();
    SetConsoleTitle("Wiggle debug");
    qh  = wog_create_window((WogWindowToken) 0, "Testnisse",
			    640,480, 32, &wd);
    qh2  = wog_create_window((WogWindowToken) 1, "Testnisse2",
			     640,480, 32, &wd2);
    wog_set_current_window(&wd);
    inigl();
    wog_set_current_window(&wd2);
    inigl2();
    
    cmd = wog_alloc_command_message();
    cmd->tag = WogListenAll;
    cmd->onoff = 1;
    wog_post_command_message(WOG_COMMAND_QUE(&wd),cmd);
    cmd = wog_alloc_command_message();
    cmd->tag = WogListenAll;
    cmd->onoff = 1;
    wog_post_command_message(WOG_COMMAND_QUE(&wd2),cmd);
     for(;;) {
	if(WaitForSingleObject(qh,100) == WAIT_TIMEOUT) {
	    theta += 1.0f;
	    theta2 += 1.0f;
	    wog_set_current_window(&wd);
	    glpaint(wd.hdc,theta);
	    wog_set_current_window(&wd2);
	    glpaint2(wd2.hdc,theta2);
	    continue;
	}
	    
	Sleep(100);
	while((msg = wog_get_event_message(WOG_GLOBAL_EVENT_QUE()))) {
#ifdef MSGDEBUG
	    debugf("Message Token: %d\n",msg->any.token);
#endif
	    switch(msg->any.tag) {
	    case WogResize:
#ifdef MSGDEBUG
		debugf("WogResize: %d,%d\n",
		       msg->resize.width,
		       msg->resize.height);
#endif
		glpaint(wd.hdc,theta);
		break;
	    case WogPaint:
#ifdef MSGDEBUG
		debugf("WogPaint: %d,%d,%d,%d\n",
		       msg->paint.x1,
		       msg->paint.y1,
		       msg->paint.x2,
		       msg->paint.y2);
#endif
		glpaint(wd.hdc,theta);
		break;
	    case WogMouseMove:
#ifdef MSGDEBUG
		debugf("WogMouseMove: %d,%d, 0x%08x\n",
		       msg->mouse_move.x,
		       msg->mouse_move.y,
		       msg->mouse_move.modifiers);
#endif

		break;

	    case WogMouseDelta:
#ifdef MSGDEBUG
		debugf("WogMouseDelta: %d,%d, 0x%08x\n",
		       msg->mouse_delta.x,
		       msg->mouse_delta.y,
		       msg->mouse_delta.modifiers);
#endif

		break;

	    case WogMouseDown:
#ifdef MSGDEBUG
		debugf("WogMouseDown: %d,%d, 0x%08x\n",
			msg->mouse_up_down.x,
			msg->mouse_up_down.y,
			msg->mouse_up_down.modifiers);
#endif
		if (msg->mouse_up_down.button == WogMouseMiddle) {
		    cmd = wog_alloc_command_message();
		    cmd->tag = WogDeltaMouse;
		    cmd->onoff = 1;
		    wog_post_command_message(WOG_COMMAND_QUE(&wd),cmd);
		}
		break;

	    case WogMouseUp:
#ifdef MSGDEBUG
		debugf("WogMouseUp: %d,%d, 0x%08x\n",
			msg->mouse_up_down.x,
			msg->mouse_up_down.y,
			msg->mouse_up_down.modifiers);
#endif
		if (msg->mouse_up_down.button == WogMouseMiddle) {
		    cmd = wog_alloc_command_message();
		    cmd->tag = WogDeltaMouse;
		    cmd->onoff = 0;
		    wog_post_command_message(WOG_COMMAND_QUE(&wd),cmd);
		}
		break;

	    case WogKeyUp:
#ifdef MSGDEBUG
		debugf("WogKeyUp: %d,%d,%d, 0x%08x\n",
			msg->key_up_down.code,
			msg->key_up_down.scancode,
			msg->key_up_down.repeat,
			msg->key_up_down.modifiers);
#endif
		break;

	    case WogKeyDown:
#ifdef MSGDEBUG
		debugf("WogKeyDown: %d,%d,%d, 0x%08x\n",
			msg->key_up_down.code,
			msg->key_up_down.scancode,
			msg->key_up_down.repeat,
			msg->key_up_down.modifiers);
#endif
		break;

	    case WogChar:
#ifdef MSGDEBUG
		debugf("WogChar: %d,%d,%d(%c),%d, 0x%08x\n",
		       msg->a_char.code,
		       msg->a_char.scancode,
		       msg->a_char.ch,
		       msg->a_char.ch >= 32 ? msg->a_char.ch : '?',
		       msg->a_char.repeat,
		       msg->a_char.modifiers);
#endif
		break;

	    case WogMouseWheel:
#ifdef MSGDEBUG
		debugf("WogMouseWheel: delta = %d,x = %d,y = %d, 0x%08x\n",
			msg->mouse_wheel.delta,
			msg->mouse_wheel.x,
			msg->mouse_wheel.y,
			msg->mouse_wheel.modifiers);
#endif
		break;

	    case WogClose:
#ifdef MSGDEBUG
		debugf("WogClose\n");
		cmd = wog_alloc_command_message();
		cmd->tag = WogCloseWindow;
		cmd->onoff = 1;
		wog_post_command_message(WOG_COMMAND_QUE(&wd),cmd);
#endif
		break;
	    case WogGoodbye:
#ifdef MSGDEBUG
		debugf("WogGoodbye\n");
		MessageBox(NULL,"Goodbye!",
			   "Bummer!",MB_OK);
		exit(0);
#endif
		break;

#ifdef MSGDEBUG
	    default:
	      debugf("Unknown[%d]\n",
		      msg->any.tag);
#endif
	    }
	    wog_free_event_message(msg);
	}
    }
    wog_close_window(&wd2);
    return 0;
}

#else

#if WIN32
int APIENTRY WinMain(HINSTANCE instance, HINSTANCE prev_instance, LPSTR command_line,
	int cmd_show)
#else
/* UNIX */
#include <unistd.h>
#define Sleep       usleep
#define instance    0
int main( int argc, char** argv )
#endif
{
    WOG_WIN_HANDLE wh;
    /*WogEventMessage *msg;
      WogCommandMessage *cmd;*/
    WogWindowData wd;
    int i;
    float cube[][3] ={{0.5f,0.5f,-0.5f},
		    { 0.5f, -0.5f, -0.5f},
		    {-0.5f, -0.5f, -0.5f},
		    {-0.5f,  0.5f, -0.5f},
		    {-0.5f,  0.5f,  0.5f},
		    { 0.5f,  0.5f,  0.5f},
		    { 0.5f, -0.5f,  0.5f},
		    {-0.5f, -0.5f,  0.5f}};
    float colors[][3] = {{ 1.0f,  1.0f,  0.0f}, 
			{ 1.0f,  0.0f,  0.0f},
			{ 0.0f,  0.0f,  0.0f},
			{ 0.0f,  1.0f,  0.0f},
			{ 0.0f,  1.0f,  1.0f},
			{ 1.0f,  1.0f,  1.0f},
			{ 1.0f,  0.0f,  1.0f},
			{ 0.0f,  0.0f,  1.0f}};

    wog_init_instance(instance);

    wh = wog_create_window((WogWindowToken) 0, "Testnisse", 640,480, 32, &wd);
    if( wh )
    {
        glViewport(0,0,640,480);
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glOrtho(-2.0,2.0,-2.0,2.0,-20.0,20.0);
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        glEnable(GL_DEPTH_TEST);
        glDepthFunc(GL_LESS);
        glClearColor(0.0,0.0,0.0,1.0);
        printf("%s\n",glGetString(GL_VENDOR));

        for(i=0;i<1000;++i)
        {
            glClearColor(0.0,0.0,0.0,1.0);
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

            glBegin(GL_QUADS);
            glColor3fv(colors[0]);
            glVertex3fv(cube[0]);
            glColor3fv(colors[1]);
            glVertex3fv(cube[1]);
            glColor3fv(colors[2]);
            glVertex3fv(cube[2]);
            glColor3fv(colors[3]);
            glVertex3fv(cube[3]);
            
            glColor3fv(colors[3]);
            glVertex3fv(cube[3]);
            glColor3fv(colors[4]);
            glVertex3fv(cube[4]);
            glColor3fv(colors[7]);
            glVertex3fv(cube[7]);
            glColor3fv(colors[2]);
            glVertex3fv(cube[2]);
            
            glColor3fv(colors[0]);
            glVertex3fv(cube[0]);
            glColor3fv(colors[5]);
            glVertex3fv(cube[5]);
            glColor3fv(colors[6]);
            glVertex3fv(cube[6]);
            glColor3fv(colors[1]);
            glVertex3fv(cube[1]);
#define element(N,B) B[N-1]
            glColor3fv(element(6, colors));
            glVertex3fv(element(6, cube));
            glColor3fv(element(5, colors));
            glVertex3fv(element(5, cube));
            glColor3fv(element(8, colors));
            glVertex3fv(element(8, cube));
            glColor3fv(element(7, colors));
            glVertex3fv(element(7, cube));
            
            glColor3fv(element(6, colors));
            glVertex3fv(element(6, cube));
            glColor3fv(element(1, colors));
            glVertex3fv(element(1, cube));
            glColor3fv(element(4, colors));
            glVertex3fv(element(4, cube));
            glColor3fv(element(5, colors));
            glVertex3fv(element(5, cube));
            
            glColor3fv(element(7, colors));
            glVertex3fv(element(7, cube));
            glColor3fv(element(2, colors));
            glVertex3fv(element(2, cube));
            glColor3fv(element(3, colors));
            glVertex3fv(element(3, cube));
            glColor3fv(element(8, colors));
            glVertex3fv(element(8, cube));
            glEnd();

            glMatrixMode(GL_MODELVIEW);
            glRotatef(5.0f,1.0f,1.0f,1.0f);

            wog_swap_buffers(&wd);
            Sleep(10);
        }
        wog_close_window(&wd);
    }

#if 0
    wh = wog_create_window((WogWindowToken) 0, "Testnisse", 100,100, 32, &wd);
    if( wh )
    {
        glViewport(0,0,640,480);
        wog_close_window(&wd);
    }
#endif

    return 0;
}

#endif
