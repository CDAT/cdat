#include "vcs_legacy_events.h"
#include "display.h"
#include "vcs_legacy_events_X11_mapping.h"

extern PyInterpreterState * mainInterpreterState;
extern PyThreadState * mainThreadState;
extern PyThreadState * myThreadState;

extern float BUFFER;

/* Figures out where the right button has been released and launch corresponding action */
/* Thread ??? */


int launch_py_user_action(PyVCScanvas_Object *self, 
#ifdef X11WM
			  Window window, XEvent event, 
#elif defined (QTWM)
			  QEvent event,
#endif
			  struct data_point info, 
			  int ipoint[2])
{
#ifdef X11WM
  Window parent;
#endif
  int x,y,w,h,bw,dpth;
  int line;
  PyObject *canvas, *funcs, *func, *args, *kargs, *kval;
#ifdef X11WM
  XGetGeometry(self->connect_id.display,window,&parent,&x,&y,&w,&h,&bw,&dpth) ;
#elif defined (QTWM)
  vcs_legacy_Qt_get_window_dimensions_by_id(self->connect_id.wkst_id,&x,&y,&w,&h);
#else
  fprintf(stderr,"insert here your WM getgeometry function\n");
#endif
  

  if ((x<BUTTON_X) && (BUTTON_X<x+w) && (y<BUTTON_Y) && (BUTTON_Y<y+h))
    {
      PY_ENTER_THREADS;
      PY_GRAB_THREAD;
      canvas = getPyCanvas( self->canvas_id );
      kargs = PyDict_New();
      if (info.x!=-999.)
	{
	  kval = Py_BuildValue("d",info.x);
	}
      else
	{
	  Py_INCREF(Py_None);
	  kval = Py_None;
	}
      PyDict_SetItemString(kargs,"datawc_x",kval);
      Py_DECREF(kval);

      if (info.y!=-999.)
	{
	  kval = Py_BuildValue("d",info.y);
	}
      else
	{
	  Py_INCREF(Py_None);
	  kval = Py_None;
	}
      PyDict_SetItemString(kargs,"datawc_y",kval);
      Py_DECREF(kval);

      if (info.value!=-999.)
	{
	  kval = Py_BuildValue("d",info.value);
	}
      else
	{
	  Py_INCREF(Py_None);
	  kval = Py_None;
	}
      PyDict_SetItemString(kargs,"value",kval);
      Py_DECREF(kval);
      if (info.value2!=-999.)
	{
	  kval = Py_BuildValue("d",info.value2);
	}
      else
	{
	  Py_INCREF(Py_None);
	  kval = Py_None;
	}
      PyDict_SetItemString(kargs,"value2",kval);
      Py_DECREF(kval);
      if (info.x_index!=-999)
	{
	  kval = Py_BuildValue("i",info.x_index);
	}
      else
	{
	  Py_INCREF(Py_None);
	  kval = Py_None;
	}
      PyDict_SetItemString(kargs,"index_x",kval);
      Py_DECREF(kval);
      if (info.y_index!=-999)
	{
	  kval = Py_BuildValue("i",info.y_index);
	}
      else
	{
	  Py_INCREF(Py_None);
	  kval = Py_None;
	}
      PyDict_SetItemString(kargs,"index_y",kval);
      Py_DECREF(kval);
      if (info.color!=-999.)
	{
	  kval = Py_BuildValue("i",info.color);
	}
      else
	{
	  Py_INCREF(Py_None);
	  kval = Py_None;
	}
      PyDict_SetItemString(kargs,"color",kval);
      Py_DECREF(kval);
      
      kval = Py_BuildValue("i",ipoint[0]);
      PyDict_SetItemString(kargs,"XW_x",kval);
      Py_DECREF(kval);

      kval = Py_BuildValue("i",ipoint[1]);
      PyDict_SetItemString(kargs,"XW_y",kval);
      Py_DECREF(kval);

      PyDict_SetItemString(kargs,"canvas",canvas);
      
      funcs = PyObject_GetAttrString(canvas,"user_actions_names");
      if (PyList_Check(funcs)) 
	{
	  line = (BUTTON_Y-y)*PyList_Size(funcs)/h;
	}
      else line=1;
      Py_DECREF(funcs);      
      /* Set the line number as argument */
      args = Py_BuildValue("()",line);

      /* following is for direct call of func */
      funcs = PyObject_GetAttrString(canvas,"user_actions"); /* decref ? */
      if (PyList_Check(funcs))
	{
	  func = PyList_GetItem(funcs,line);
	  if (PyCallable_Check(func))
	    {
	      PY_RELEASE_THREAD;
	      PY_LEAVE_THREADS;
	      PY_ENTER_THREADS;
	      PY_GRAB_THREAD;
	      kval = PyEval_CallObjectWithKeywords(func,args,kargs);
	      Py_DECREF(kargs);
	      Py_DECREF(args);
	      Py_XDECREF(kval);
	      PY_RELEASE_THREAD;
	      PY_LEAVE_THREADS;
	    }
	  else
	    {
	      PY_RELEASE_THREAD
		PY_LEAVE_THREADS
	      return 1;
	    }
	}
      else {
	      PY_RELEASE_THREAD
		PY_LEAVE_THREADS
	return 1;
      }
    }
  return 0;
}


void draw_text(win, gc, display, screen_num, font_info, win_width, win_height, text)
   Window 	win;
   GC 		gc;
   Display 	*display;
   int 		screen_num;
   XFontStruct 	*font_info;
   unsigned int win_width, win_height;
   struct window_text *text;
{
	int len1, line;
	int font_height;
	int mult=3;
	struct window_text *tmptxt;
/* 	int initial_y_offset, x_offset; */

	line=0;
	/* Need length for both XTextWidth and XDrawString */
	for (tmptxt=text;tmptxt!=NULL;tmptxt=tmptxt->next)
	  {
	    len1 =strlen(tmptxt->text);
	    /* Output text, centered on each line */
	    font_height = font_info->ascent + font_info->descent;

	    /* Output text,  on each line */
	    if (len1!=0) 
	      {
		line+=1;
		XDrawString(display, win, gc, 8, (line*font_height), tmptxt->text, len1);
	      }
	  }
}

Window display_info(PyVCScanvas_Object *self,Gpoint point,struct data_point info)
{
  Display      		*dpy = self->connect_id.display;
  Window      		wparent = self->connect_id.drawable, win;
  XWindowAttributes 	xwa;
  GC 			gc; /* graphics context */
  XGCValues 		values;
  XFontStruct           *font_info;
  int 			screen = DefaultScreen(dpy);
  unsigned int 		win_width=110, win_height=5;
  unsigned int 		xpos, ypos;
  float                 canvas_ratio_l=0.0, canvas_ratio_p=0.0;
  char 			*fontname = "9x15";
  struct window_text    text, *text2;
  int 			len1,i;
  int 			width1;
  extern struct orientation       Page;

  /*                      Get the width and height of the VCS Canvas.     */
  XGetWindowAttributes(self->connect_id.display, self->connect_id.drawable, &xwa);
  canvas_ratio_l = (float) xwa.height / (float) xwa.width;
  canvas_ratio_p = (float) xwa.width / (float) xwa.height;


  text2=&text;
  for (i=0;i<6;i+=1)
    {
      if ((text2->next = (struct window_text *) malloc(sizeof(struct window_text))) ==NULL)
	{
	  PyErr_SetString(PyExc_TypeError, "No memory for the text window string");
	  return (Window)NULL;
	}
      else 
	{
	  text2->next->next=NULL;
	}
      text2=text2->next;
    };
  text2=&text;
  /* First Line of Text */
  if (info.x_index != -999)
    sprintf( text2->text, "X[%d]: %g", info.x_index, info.x);
  else
    {
      if (info.x == -999)
	sprintf( text2->text, "X    : NaN");
      else
	sprintf( text2->text, "X    : %g", info.x);
    }

  /* Second Line of Text */
  text2 = text2->next;
  if (info.y_index != -999)
    sprintf( text2->text, "Y[%d]: %g", info.y_index, info.y);
  else
    {
      if (info.y == -999)
	sprintf( text2->text, "Y    : NaN");
      else
	sprintf( text2->text, "Y    : %g", info.y);
    }

  /* Third Line of Text */
  text2 = text2->next;
  if (((info.x_index == -999) || (info.y_index == -999)) && info.value == -999)
    if (info.value2 == -999)
      {
	sprintf( text2->text, "\0");
      }
    else
      {
	sprintf( text2->text, "Data 1:   N/A");
      }
  else
    if (info.value>9.9E19) 
      {
	if (info.value2 == -999)
	  {
	    sprintf( text2->text, "Data:   Masked");
	  }
	else
	  {
	    sprintf( text2->text, "Data 1:   Masked");
	  }
        info.color=-999;
      }
    else
      {
	if (info.value2 == -999)
	  {
	    sprintf( text2->text, "Data:   %g", info.value);
	  }
	else
	  {
	    sprintf( text2->text, "Data 1:   %g", info.value);
	  }
      }

  /* Fourth and Fith Line of Text */
  text2 = text2->next;
  if (info.value2 != -999)
    if (info.value2>9.9E19) 
      {
	sprintf( text2->text, "Data 2:   Masked");
      }
    else
      {
	sprintf( text2->text, "Data 2:   %g", info.value2);
	if (info.value<9.9E19)
	  {
	    sprintf(text2->next->text, "Vector: %g",sqrt(info.value2*info.value2+info.value*info.value));
	  }
	else
	  {
	    sprintf(text2->next->text, "",sqrt(info.value2*info.value+info.value*info.value));
	  }

      }
  else
    {
      sprintf( text2->text, "");
      sprintf( text2->next->text, "");
    }
  
  /* Sixth Line of Text */
  text2=text2->next->next;
  if (info.color!=-999)
      {
	sprintf( text2->text, "Color:   %d", info.color);
      }
  else
    {
	sprintf( text2->text, "");
    }


  /* Access font */
  if ((font_info = XLoadQueryFont(dpy,fontname)) == NULL) {
    (void) fprintf( stderr, "Basic: Cannot open 9x15 font\n");
    return (Window)NULL;
  }


  for (text2=&text;text2->next!=NULL;text2=text2->next)
    {
      /* Need length for both XTextWidth and XDrawString */
      len1 = strlen(text2->text)-3;
      if (len1!=-3)
	{
	  win_height+=15;
	  width1 = XTextWidth(font_info, text2->text, len1);
	  if (width1>win_width) win_width=width1;
	}
    }

  if (strcmp(Page.page_orient,"landscape") == 0) {
     xpos = (int)( xwa.width * point.x);
     ypos = (int)( xwa.height - (point.y * (xwa.height/canvas_ratio_l)) );
  } else {
     xpos = (int) (point.x * (xwa.width/canvas_ratio_p));
     ypos = (int) (xwa.height - (xwa.height * point.y));
  }

  if ((xpos + win_width) >= xwa.width) xpos = xpos - win_width;
  if ((ypos + win_height) >= xwa.height) ypos = ypos - win_height;
  win = XCreateSimpleWindow(dpy, wparent, xpos, ypos, win_width, win_height, 1,
			    BlackPixel(dpy,screen), WhitePixel(dpy,screen));

  gc = XCreateGC(dpy, win, 0L, &values);

  XMapWindow(dpy, win);

  draw_text(win, gc, dpy, screen, font_info, win_width, win_height, &text);

  for (text2=text.next;text.next!=NULL;text2=text.next)
    {
      text.next=text2->next;
      free((struct window_text *)text2);
    }


  return win;
}

 Window display_menu(PyVCScanvas_Object *self,Gpoint point)
{
  Display      		*dpy = self->connect_id.display;
  Window      		wparent = self->connect_id.drawable, win;
  XWindowAttributes 	xwa;
  GC 			gc; /* graphics context */
  XGCValues 		values;
  XFontStruct           *font_info;
  int 			screen = DefaultScreen(dpy);
  unsigned int 		win_width=110, win_height=5;
  unsigned int 		xpos, ypos;
  float                 canvas_ratio_l=0.0, canvas_ratio_p=0.0;
  struct window_text    text, *text2;
  char 			*fontname = "9x15";
  char                  *astring;
  int 			len1,nactions,i;
  int 			width1;
  extern struct orientation       Page;
  PyObject *canvas, *user_act_nms, *user_action_name;

  /*                      Get the width and height of the VCS Canvas.     */
  XGetWindowAttributes(self->connect_id.display, self->connect_id.drawable, &xwa);
  canvas_ratio_l = (float) xwa.height / (float) xwa.width;
  canvas_ratio_p = (float) xwa.width / (float) xwa.height;

  PY_ENTER_THREADS
    PY_GRAB_THREAD
  canvas  = getPyCanvas( self->canvas_id );
  user_action_name = PyString_FromString("user_actions_names");
  user_act_nms = PyObject_GetAttr(canvas,user_action_name);
  Py_XDECREF(user_action_name);

  /* Access font */
  if ((font_info = XLoadQueryFont(dpy,fontname)) == NULL) {
    (void) fprintf( stderr, "Basic: Cannot open 9x15 font\n");
    return (Window)NULL;
  }

  text.next=NULL;
  text2=&text;
  if PyList_Check(user_act_nms)
    {
      nactions=PyList_Size(user_act_nms);
      for (i=0;i<nactions;i+=1)
	{
	  if (i!=nactions-1)
	  if ((text2->next = (struct window_text *) malloc(sizeof(struct window_text))) == NULL)
	    {
	      PyErr_SetString(PyExc_TypeError, "No memory for the text window string");
	      return (Window)NULL;
	    }
	  else 
	    {
	      text2->next->next=NULL;
	    }

	  user_action_name = PyList_GetItem(user_act_nms,i);
	  if PyString_Check(user_action_name) 
	    {
	      astring = PyString_AsString(user_action_name);
	      sprintf(text2->text,"%s",astring);
	    }
	  else
	    {
	      sprintf(text2->text,"Action %d",i);
	    }
	  len1 = strlen(text2->text)-3;
	  width1 = XTextWidth(font_info, text2->text, len1);
	  win_height+=15;
	  if (width1>win_width) win_width=width1;
	  text2=text2->next;
	}
   }
  else
    {
      PyErr_SetString(PyExc_TypeError, "user_actions_names must be a list");
      return (Window)NULL;
    };
  Py_XDECREF(user_act_nms);

  if (strcmp(Page.page_orient,"landscape") == 0) {
     xpos = (int)( xwa.width * point.x);
     ypos = (int)( xwa.height - (point.y * (xwa.height/canvas_ratio_l)) );
  } else {
     xpos = (int) (point.x * (xwa.width/canvas_ratio_p));
     ypos = (int) (xwa.height - (xwa.height * point.y));
  }

  if ((xpos + win_width) >= xwa.width) xpos = xpos - win_width;
  if ((ypos + win_height) >= xwa.height) ypos = ypos - win_height;
  win = XCreateSimpleWindow(dpy, wparent, xpos, ypos, win_width, win_height, 1,
			    BlackPixel(dpy,screen), WhitePixel(dpy,screen));

  gc = XCreateGC(dpy, win, 0L, &values);

  XMapWindow(dpy, win);

/*   printf("before\n"); */
/*   for (text2=&text;text2!=NULL;text2=text2->next) */
/*     { */
/*       printf("Would draw: %s\n",text2->text); */
/*     } */
/*   printf("after\n"); */
  draw_text(win, gc, dpy, screen, font_info, win_width, win_height, &text);


  for (text2=text.next;text.next!=NULL;text2=text.next)
    {
      text.next=text2->next;
      free((struct window_text *)text2);
    }
  PY_RELEASE_THREAD
    PY_LEAVE_THREADS
  return win;
}


/* obsolete */
/* Window display_info_old(self,point,info) */
/* PyVCScanvas_Object 	*self;        */
/* Gpoint  point; */
/* struct data_point   info; */
/* { */
/* 	Display        		*dpy = self->connect_id.display; */
/* 	Window	       		wparent = self->connect_id.drawable, win; */
/*         XWindowAttributes 	xwa; */
/*         GC 			gc; /\* graphics context *\/ */
/*         XGCValues 		values; */
/* 	XFontStruct             *font_info; */
/* 	int 			screen = DefaultScreen(dpy); */
/* 	unsigned int 		win_width=110, win_height=50; */
/*         unsigned int 		xpos, ypos; */
/*         float                   canvas_ratio_l=0.0, canvas_ratio_p=0.0; */
/*         char 			*fontname = "9x15"; */
/*         char 			xstr[100]; */
/* 	char 			ystr[100]; */
/* 	char 			dstr[100]; */


/* /\*                      Get the width and height of the VCS Canvas.     *\/ */
/*         XGetWindowAttributes(self->connect_id.display, self->connect_id.drawable, &xwa); */
/*         canvas_ratio_l = (float) xwa.height / (float) xwa.width; */
/*         canvas_ratio_p = (float) xwa.width / (float) xwa.height; */

/* 	xpos  = (int)( xwa.width * point.x); */
/* 	ypos = (int)(xwa.height - (point.y *(xwa.height/canvas_ratio_l)) ); */
/* 	if ((xpos + win_width) >= xwa.width) xpos = xpos - win_width; */
/* 	if ((ypos + win_height) >= xwa.height) ypos = ypos - win_height; */
/* 	win = XCreateSimpleWindow(dpy, wparent, xpos, ypos, win_width, win_height, 1, */
/* 	                          BlackPixel(dpy,screen), WhitePixel(dpy,screen)); */

/*         gc = XCreateGC(dpy, win, 0L, &values); */
/* 		      /\* Access font *\/ */
/*         if ((font_info = XLoadQueryFont(dpy,fontname)) == NULL) { */
/*             (void) fprintf( stderr, "Basic: Cannot open 9x15 font\n"); */
/* 	     return (Window)NULL; */
/* 	} */

/* 	XMapWindow(dpy, win); */
/* 	if (info.x_index != -999) */
/* 	   sprintf( xstr, "X[%d]: %g\0", info.x_index, info.x); */
/* 	else */
/* 	   sprintf( xstr, "X[n/a]: NaN\0"); */
/* 	if (info.y_index != -999) */
/* 	   sprintf( ystr, "Y[%d]: %g\0", info.y_index, info.y); */
/* 	else */
/* 	   sprintf( ystr, "Y[n/a]: NaN\0"); */
/* 	if ((info.x_index == -999) && (info.x_index == -999)) */
/* 	   sprintf( dstr, "Data:   N/A\0"); */
/*         else */
/* 	  if (info.value>9.9E19)  */
/* 	    { */
/* 	      sprintf( dstr, "Data:   Missing\0"); */
/* 	    } */
/* 	  else */
/* 	    { */
/* 	      sprintf( dstr, "Data:   %g\0", info.value); */
/* 	    } */
/* 	draw_text(win, gc, dpy, screen, font_info, win_width, win_height, xstr,ystr,dstr); */
/* 	return win; */
/* } */


