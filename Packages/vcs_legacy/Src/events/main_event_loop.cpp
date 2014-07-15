#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xlibint.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include "vcs_legacy_events_X11_mapping.h"
#ifdef CAIRODRAW
#include <cairo-xlib.h>
#endif
#endif

#ifdef QTEM
#include "Qt/mainwindow.h"
#include "Qt/vcs_legacy_events_Qt_mapping.h"
#include <QtCore/QEvent>
#include "mainwindow.h"
#endif

#include "display.h"
#include "workstations.h"
#include "pyvcs_legacy.h"

#include "vcs_legacy_events.h"

extern Gconid_X_drawable connect_id; /* VCS canvas drawable id */
float       BUFFER=.005;

int X_QUE_COUNT = 0;		 /* Checks to see if the X que is free for business */
int BLOCK_X_SERVER = 0;		 /* Stop the X main loop from serving X events */

int     data_selected=0;
int	template_select_all_flg=0;/* select all template objs = 1, unselect = 2, None = 0 */
struct item_list *hold_selected_items=NULL;
enum screen_mode_types SCREEN_MODE = DATA;
enum screen_mode_types CHK_MODE = DATA;

#ifdef X11WM
extern "C" int launch_py_user_action(PyVCScanvas_Object *self, Window window, XEvent event, struct data_point info, int ipoint[2]);
  Window          		data_window_right=(Window)NULL;
  Window          		data_window=(Window)NULL;
#endif

#ifdef QTWM
extern "C" void vcs_legacy_Qt_open_window_by_id(int index);
extern "C" void vcs_legacy_Qt_repaint_window_by_id(int id);
extern "C" void vcs_legacy_Qt_get_window_dimensions_by_id(int id,int *x, int *y,int *w,int *h);
extern "C" void vcs_legacy_Qt_window_set_cursor_by_id(int id, QCursor *cursor);
#endif
extern "C" PyObject *PyVCS_close(PyVCScanvas_Object *self, PyObject *args);
extern "C" PyObject *PyVCS_clear(PyVCScanvas_Object *self, PyObject *args);
extern "C" PyObject *PyVCS_showbg(PyVCScanvas_Object *self, PyObject *args);
extern "C" float cnorm(PyVCScanvas_Object *self, int x_or_y, float value);
extern "C" float plnorm(int x_or_y, float value);
extern "C" int get_data_coords(PyVCScanvas_Object 	 *self,Gpoint point,struct item_list *item,struct data_point *info);
extern "C" int update_template_gui( PyVCScanvas_Object *self);
extern "C" int in_list(struct item_list *item,struct  item_list **selected_items);
extern "C" int draw_selected(struct item_list *selected_items, int shadow);
extern "C" int update_template_toggle_color( PyVCScanvas_Object *self, struct item_list *selected_items);
extern "C" int switch_templates( PyVCScanvas_Object *self, struct item_list *selected_items);
extern "C" void setup_the_magnify_table();
extern "C" int undisplay_resize_plot(PyVCScanvas_Object *self);
extern "C" void display_resize_plot(PyVCScanvas_Object *self, int off);
extern "C" int change_orientation(char *type, Gconid_X_drawable *connect_id_in, int where_from);
// extern "C" void delete_list(struct  item_list  **selected_items) ;
extern "C" struct item_list *select_item(PyVCScanvas_Object *self, Gpoint point,char *gui_template_name,char *attr_name,enum etypes search_only);
// extern  void update_extent(Gpoint pointA, Gpoint pointB, int action, struct  item_list **selected_items);
// extern  void verify_extent(struct item_list **selected_items);
// extern  void zero_priority(struct item_list **selected_items);
// extern  void append_to_list(struct  item_list *item, struct  item_list **selected_items);
// extern  void resize_or_move(PyVCScanvas_Object *self,Gpoint pointA, Gpoint pointB,int action,struct  item_list   **selected_items, int arrow_move);
// extern  void remove_from_list(struct  item_list   *item, struct  item_list **selected_items);
// extern  void draw_selection_box(Gextent extentin, int shadow);
// extern  PyObject *PyVCS_backing_store( PyVCScanvas_Object *self, PyObject *args);
// extern   PyObject *PyVCS_updateVCSsegments( PyVCScanvas_Object *self, PyObject *args);
// extern  int within(Gpoint point, Gextent extent);
//extern "C" void printextent(Gextent extent);
extern "C" Gpoint proj_convert(Gpoint pxy);
extern "C" int rotateextent(Gextent* extent);
extern "C" int cmpncs(char *s1,char *s2);
extern "C"     void set_viewport_and_worldcoordinate ( 
						      float vp[],/* view port.                    */
						      float wc_in[], /* world coordinates.            */
						      char proj[256]
						       );
extern "C" int in_range(Gpoint position,Gextent extent);
extern "C" void vcs_legacy_acquire_update(void);
extern "C" void vcs_legacy_release_update(void);
extern "C" int vcs_legacy_canvas_update ( short use_defer_flg );
extern "C" void setup_canvas_globals(PyVCScanvas_Object *self);
/* Stop the X main loop */
extern "C" PyObject *
PyVCS_stopxmainloop(PyVCScanvas_Object *self, PyObject *args)
{
  self->stopxmainloop = 1;
  self->havexmainloop = 0;
  X_QUE_COUNT = 0;

  /* Return NULL Python Object */
  Py_INCREF(Py_None);
  return Py_None;
}


void set_cursor(PyVCScanvas_Object *self,
		struct Gpoint position,
#ifdef X11WM
		Cursor *cursor,
#elif defined (QTWM)
		QCursor *cursor,
#else
		void *cursor,
#endif
		int  *action, 
		struct item_list *selected_items)
{
    struct item_list *current=selected_items;
    int i, found = 0, priority = 0;
    Gextent extent[10];
    Gextent cextent;
    int j,iprim,counter;
    int corner;
    extern struct table_fill        Tf_tab;
    struct table_fill               *tftab;
    extern struct table_line        Tl_tab;
    struct table_line               *tltab;
    extern struct table_mark        Tm_tab;
    struct table_mark               *tmtab;
    struct points_struct		*xptr=NULL, *yptr=NULL;
    struct array_segments   	*xpts=NULL, *ypts=NULL;
    char                            proj[256];
    Gpoint pxy;
/*     void printextent(); */
    counter=0;

    while (current != NULL)
    {
      cextent.ll.x=current->extent.ll.x;
      cextent.lr.x=current->extent.lr.x;
      cextent.ur.x=current->extent.ur.x;
      cextent.ul.x=current->extent.ul.x;
      cextent.ll.y=current->extent.ll.y;
      cextent.lr.y=current->extent.lr.y;
      cextent.ur.y=current->extent.ur.y;
      cextent.ul.y=current->extent.ul.y;

      corner=rotateextent(&cextent);
       /* Inner region */
        extent[0].ll.x = cextent.ll.x+BUFFER;
        extent[0].lr.x = cextent.lr.x-BUFFER;
        extent[0].ll.y = cextent.ll.y+BUFFER;
        extent[0].ul.y = cextent.ul.y-BUFFER;
        extent[0].ul.x = cextent.ul.x+BUFFER;
        extent[0].ur.x = cextent.ur.x-BUFFER;
        extent[0].lr.y = cextent.lr.y+BUFFER;
        extent[0].ur.y = cextent.ur.y-BUFFER;
        /* Upper left hand corner region */
        extent[1].ll.x = extent[1].ul.x = cextent.ul.x-BUFFER;
        extent[1].lr.x = extent[1].ur.x = cextent.ul.x+BUFFER;
        extent[1].ll.y = extent[1].lr.y = cextent.ul.y-BUFFER;
        extent[1].ul.y = extent[1].ur.y = cextent.ul.y+BUFFER;
        /* Upper right hand corner region */
        extent[2].ll.x = extent[2].ul.x = cextent.ur.x-BUFFER;
        extent[2].lr.x = extent[2].ur.x = cextent.ur.x+BUFFER;
        extent[2].ll.y = extent[2].lr.y = cextent.ur.y-BUFFER;
        extent[2].ul.y = extent[2].ur.y = cextent.ur.y+BUFFER;
        /* Lower right hand corner region */
        extent[3].ll.x = extent[3].ul.x = cextent.lr.x-BUFFER;
        extent[3].lr.x = extent[3].ur.x = cextent.lr.x+BUFFER;
        extent[3].ll.y = extent[3].lr.y = cextent.lr.y-BUFFER;
        extent[3].ul.y = extent[3].ur.y = cextent.lr.y+BUFFER;
        /* Lower left hand corner region */
        extent[4].ll.x = extent[4].ul.x = cextent.ll.x-BUFFER;
        extent[4].lr.x = extent[4].ur.x = cextent.ll.x+BUFFER;
        extent[4].ll.y = extent[4].lr.y = cextent.ll.y-BUFFER;
        extent[4].ul.y = extent[4].ur.y = cextent.ll.y+BUFFER;
        /* Top line region */
        extent[5].ll.x = cextent.ul.x+BUFFER;
        extent[5].lr.x = cextent.ur.x-BUFFER;
        extent[5].ll.y = cextent.ul.y-BUFFER;
        extent[5].ul.y = cextent.ul.y+BUFFER;
        extent[5].ul.x = cextent.ul.x+BUFFER;
        extent[5].ur.x = cextent.ur.x-BUFFER;
        extent[5].lr.y = cextent.ur.y-BUFFER;
        extent[5].ur.y = cextent.ur.y+BUFFER;

        /* Right side line region */
        extent[6].ll.x = cextent.lr.x-BUFFER;
        extent[6].ll.y = cextent.lr.y-BUFFER;
        extent[6].lr.x = cextent.lr.x+BUFFER;
        extent[6].lr.y = cextent.lr.y-BUFFER;
        extent[6].ul.x = cextent.ur.x-BUFFER;
        extent[6].ul.y = cextent.ur.y-BUFFER;
        extent[6].ur.x = cextent.ur.x+BUFFER;
        extent[6].ur.y = cextent.ur.y-BUFFER;

        /* Bottom line region */

        extent[7].ll.x = cextent.ll.x+BUFFER;
        extent[7].lr.x = cextent.lr.x-BUFFER;
        extent[7].ll.y = cextent.ll.y-BUFFER;
        extent[7].ul.y = cextent.ll.y+BUFFER;
        extent[7].ul.x = cextent.ll.x+BUFFER;
        extent[7].ur.x = cextent.lr.x-BUFFER;
        extent[7].lr.y = cextent.lr.y-BUFFER;
        extent[7].ur.y = cextent.lr.y+BUFFER;

        /* Left side line region */
        extent[8].ll.x = cextent.ll.x-BUFFER;
        extent[8].ll.y = cextent.ll.y-BUFFER;
        extent[8].lr.x = cextent.ll.x+BUFFER;
        extent[8].lr.y = cextent.ll.y-BUFFER;
        extent[8].ul.x = cextent.ul.x-BUFFER;
        extent[8].ul.y = cextent.ul.y-BUFFER;
        extent[8].ur.x = cextent.ul.x+BUFFER;
        extent[8].ur.y = cextent.ul.y-BUFFER;

        for (i = 1; i < 9; i++)
            if (within(position,extent[i]))
            {
                found = priority = 1;
                *action=i;
#ifdef X11WM
                XDefineCursor(self->connect_id.display,self->connect_id.drawable,cursor[i]);
#elif defined (QTWM)
		vcs_legacy_Qt_window_set_cursor_by_id(self->wkst_id,&cursor[i]);
#else
		fprintf(stderr,"insert here your WM set cursor function\n");
#endif
                break;
            }

	if (current->type == display_tab)
	  {
	    if (strcmp(current->data.pd->type,"fillarea")==0)
	      {
		tftab = &Tf_tab;
		while (tftab != NULL) {
		  if (cmpncs(current->data.pd->g_name, tftab->name) == 0) break;
		  tftab = tftab->next;
		}
		if (tftab->priority == 0) break; /* do nothing */
		strcpy(proj,tftab->proj);
		set_viewport_and_worldcoordinate ( tftab->fvp, tftab->fwc,proj );
		xptr = tftab->fx; yptr = tftab->fy;
	      }
	    if (strcmp(current->data.pd->type,"line")==0)
	      {
		tltab = &Tl_tab;
		while (tltab != NULL) {
		  if (cmpncs(current->data.pd->g_name, tltab->name) == 0) break;
		  tltab = tltab->next;
		}
		if (tltab->priority == 0) break; /* do nothing */
		strcpy(proj,tltab->proj);
		set_viewport_and_worldcoordinate ( tltab->lvp, tltab->lwc,proj );
		xptr = tltab->lx; yptr = tltab->ly;
	      }
	    if (strcmp(current->data.pd->type,"marker")==0)
	      {
		tmtab = &Tm_tab;
		while (tmtab != NULL) {
		  if (cmpncs(current->data.pd->g_name, tmtab->name) == 0) break;
		  tmtab = tmtab->next;
		}
		if (tmtab->priority == 0) break; /* do nothing */
		strcpy(proj,tmtab->proj);
		set_viewport_and_worldcoordinate ( tmtab->mvp, tmtab->mwc,proj );
		xptr = tmtab->mx; yptr = tmtab->my;
	      }
	    xpts = xptr->ps;  ypts = yptr->ps;
	    counter=0;
	    for (iprim=0; iprim<xptr->nsegs; iprim++) {
	      for (j=0;j<xpts->npts;j++) {
		counter+=1;
		pxy.x=xpts->pts[j];
		pxy.y=ypts->pts[j];
		pxy=proj_convert(pxy);
		/* 		    printf("comparing %f, %f with point %i, %f,%f\n",position.x,position.y,counter,pxy.x,pxy.y); */
/* 		    printextent(extent[4]); */
		extent[9].ll.x = extent[9].ul.x = pxy.x-BUFFER;
		extent[9].lr.x = extent[9].ur.x = pxy.x+BUFFER;
		extent[9].ll.y = extent[9].lr.y = pxy.y-BUFFER;
		extent[9].ul.y = extent[9].ur.y = pxy.y+BUFFER;
		if (in_range(position,extent[9]))
		  {
		    found = priority = 1;
		    *action=100+counter;
#ifdef X11WM
		    XDefineCursor(self->connect_id.display,self->connect_id.drawable,cursor[10]);
#elif defined (QTWM)
		    vcs_legacy_Qt_window_set_cursor_by_id(self->wkst_id,&cursor[10]);
#else
		    fprintf(stderr,"insert here your WM set cursor function\n");
#endif
		    break;
		  }
	      }
	      if (found==1) break;
		  xpts = xpts->next;
		  ypts = ypts->next;
	    }
	    
	    if (found==1) break;
	  }
	if (!priority && within(position,extent[0])) 
	  {
	    found = 1;
	    *action=0;
#ifdef X11WM
	    XDefineCursor(self->connect_id.display,self->connect_id.drawable,cursor[0]);
#elif defined (QTWM)
	    vcs_legacy_Qt_window_set_cursor_by_id(self->wkst_id,&cursor[0]);
#else
	    fprintf(stderr,"insert here your WM set cursor function\n");
#endif
	    break;
	  }
	
        current = current->next;
    }
    if (!found)
    {
#ifdef X11WM
      XUndefineCursor(self->connect_id.display,self->connect_id.drawable);
#elif defined (QTWM)
      vcs_legacy_Qt_window_set_cursor_by_id(self->wkst_id,NULL);
#else
      fprintf(stderr,"insert here your WM unset cursor function\n");
#endif
        *action = -1;
    }
    /* Uncomment the following code if you only want the user to only be able 
     * to move the template objects.
    else
    {
        *action = 0;
        XDefineCursor(self->connect_id.display,self->connect_id.drawable,cursor[0]);
    }
    */

}




#ifdef X11EM
extern "C" Window display_info(PyVCScanvas_Object *self,Gpoint point,struct data_point info);
extern "C" Window display_menu(PyVCScanvas_Object *self,Gpoint point);
void event_handler(PyVCScanvas_Object *self,  XEvent event)
#elif defined (QTEM)
  extern "C" void vcs_legacy_Qt_display_info(int id, Gpoint pointA, struct data_point info);
  extern "C" void vcs_legacy_Qt_hide_info(int id);
extern "C" void vcs_legacy_Qt_display_menu(int id, float x, float y);
void event_handler(PyVCScanvas_Object *self,  QEvent *event)
#else
  void event_handler(PyVCScanvas_Object *self,  void *event)
#endif
{

#ifdef X11EM
  KeySym 				keysym;
  XComposeStatus 			compose;
#endif
#ifdef QTEM
  int keysym;
#endif
#ifdef X11WM
  static GC         		gc; /* graphics context */
  Window  			rroot_win;
  XWindowAttributes 		xwa;
  Cursor                  	cursor[20];
#elif defined (QTWM)
  QCursor                  	cursor[20];
  QDialog *data_window;
#endif
  int               		action=-1,just_moved = 0;
  PyObject 			*update_args, *args;
  int				rxpos,rypos;
  int             		x,y, hold_continents;
  int 				screen_num;
  int 				border_width = 4;
  extern struct p_tab     	*getP();
  int 				bufsize = 10;
  char 				buffer[10];
  struct data_point   		info;
  struct  item_list   		*item=NULL;
  int               		waiting=0,control = 0, resize_flg=0;
  time_t 	          		orgtime;
  struct p_tab          		*ptab;
  extern struct p_tab   		Pic_tab;
  struct display_tab     		*dtab;
  extern struct display_tab 	D_tab;
  struct a_tab            	*atab;
  extern struct a_tab     	A_tab;
  extern int              	update_ind;
  int               		orientation_flg=0;/* if orientation, stop resize */
  int               		*segment[4];
  int               		i, ipointA[2];
  char              		type[10];
  extern struct orientation 	Page;
  Gpoint		 		pxy, selection[5], pointA, pointB, temppoint;
  Gextent 			extent; 
#ifdef X11WM
  unsigned int 			width, height;
  unsigned int 			rwidth,rheight,rborder,rdepth;
#else
  int 	          		width, height;
  int 			        rwidth,rheight,rborder,rdepth;
#endif
  extern struct default_continents 	Dc;
  /*         PyVCScanvas_Object	        *resize_self; /\* store resized canvas id *\/ */
  int did_switch_templates;
  //   printf("we are in event handler\n");

#ifdef X11WM
  XGetGeometry(self->connect_id.display,self->connect_id.drawable,
               &rroot_win,
               &rxpos, &rypos,
               &rwidth, &rheight,
               &rborder, &rdepth);
#elif defined (QTEM)
  /* I don't think it is used for anything so i don't do it yet */
#else
  printf("insert your WM get geom here\n");
#endif


  /*  arguments for PyVCS_backingstore */
  args = PyTuple_New(0);
  /* Update segemate arguments for PyVCS_updateVCSsegments. Sets the mode argument to 1 */
//   update_args = PyTuple_New(1);
//   PyTuple_SetItem(update_args, 0, Py_BuildValue("i", 1)); /* Set the 1st and only argv to 1 */  
  update_args = Py_BuildValue("(i)",1);

    /* Clean up the SCREEN_MODE selection mode before moving onto the DATA MODE */
    if (CHK_MODE != SCREEN_MODE) {
      CHK_MODE = SCREEN_MODE;
      if (SCREEN_MODE == DATA) {
        delete_list(&hold_selected_items);
        PyVCS_updateVCSsegments(self, update_args);
        PyVCS_backing_store(self, args);
      }
    }
  if (BLOCK_X_SERVER == 0) {
    /*if ((XPending(self->connect_id.display)>0) && (BLOCK_X_SERVER == 0)) {
      XNextEvent(self->connect_id.display, &event);*/
    /*       if (BLOCK_X_SERVER == 0) { */
    /* Handle the Template events */
    if (template_select_all_flg == 1) {
      template_select_all_flg = 0;
      action=1;
    } else if (template_select_all_flg == 2) {
      hold_selected_items = NULL;
      delete_list(&hold_selected_items);
      template_select_all_flg = 0;
      action=-1;
    }
    /* Handle the events */
    switch (EVENT_TYPE) {
    case VCS_ButtonPress:
      /* Set up the VCS Canvas id and XGKS workstation */
      connect_id = self->connect_id;
      
      /* Record initial mouse click position */
      pointA.x = cnorm(self, 0,(float)BUTTON_X);
      pointA.y = cnorm(self, 1,(float)BUTTON_Y);
      temppoint.x = cnorm(self, 0,(float)BUTTON_X);
      temppoint.y = cnorm(self, 1,(float)BUTTON_Y);
      ipointA[0] = BUTTON_X;
      ipointA[1] = BUTTON_Y;
      
      /* If just_moved is still equal to 0 when we release the
       * mouse then we did not move */
      just_moved = 0;
      
      /* Since we can't seem to detect when the shift key is 
       * being pressed on ButtonRelease, set variable here */
      if (SHIFT_PRESSED) {
        control = 1;
        action = -1;
      } else
        control = 0;
      /* Show the coordinate and data display window */
      /* button 1 is left click */
      /* button 3 is right click */
      /* button 2 is left and right click */
      /* button 4 is scroll up button*/
      /* button 5 is scroll down button */
      if ((BUTTON == LEFT_BUTTON) || ((BUTTON==RIGHT_BUTTON) && (SCREEN_MODE == DATA))) {
        item = select_item(self, pointA, NULL, NULL, pe_none);
        /* Initialize info struct */
        info.x=-999.;
        info.y=-999.;
        info.x_index=-999;
        info.y_index=-999;
        info.value=-999.;
        info.value2=-999.;
        info.color=-999;
        if ((item != NULL) && (item->type == pe_dsp)) {
          if (item != NULL) {
            extent.ll.x = plnorm(0,item->data.pedsp->x1);
            extent.ll.y = plnorm(1,item->data.pedsp->y1);
            extent.lr.x = plnorm(0,item->data.pedsp->x2);
            extent.lr.y = plnorm(1,item->data.pedsp->y1);
            extent.ur.x = plnorm(0,item->data.pedsp->x2);
            extent.ur.y = plnorm(1,item->data.pedsp->y2);
            extent.ul.x = plnorm(0,item->data.pedsp->x1);
            extent.ul.y = plnorm(1,item->data.pedsp->y2);
          }
//           printextent(extent);
          if (within(pointA,extent)) {
            if ((item != NULL) && (SCREEN_MODE == DATA)) { 
              waiting = 1; /* Tell the expose event to wait until data window is done */
              get_data_coords(self, pointA, item, &info);
              if (BUTTON == LEFT_BUTTON)
#ifdef X11WM
                data_window = display_info(self, pointA, info);
#elif defined (QTWM)
              vcs_legacy_Qt_display_info(connect_id.wkst_id,pointA,info);
#else
              printf("insert your WM... data_window generator here, you clicked at: (%f,%f)\n",pointA.x,pointA.y);
#endif
            }
          }
        }
      }
     /* Seems to cause threads issues in Vistrails GUI removing for now */
      /*
      if ((BUTTON == RIGHT_BUTTON ) && ( SCREEN_MODE== DATA)) {
#ifdef X11WM
        data_window_right = display_menu(self, pointA);
#elif defined (QTWM)
	vcs_legacy_Qt_display_menu(connect_id.wkst_id,(float)BUTTON_X,(float)BUTTON_Y);
#else
        printf("insert your WM display menu here you cliked at (%f,%f)\n",pointA.x,pointA.y);
#endif
      }*/

      /* 		      else if (BUTTON == 1) { * Initial ZOOM values (x1, y1) * */
      /*                         item = select_item(self, pointA, NULL, NULL, pe_none); */
      /*                         get_data_coords(self, pointA, item, &info); */
      /* 		     } */
      break;
    case VCS_ButtonRelease:
      /* Record new mouse click position */
      pointB.x = cnorm(self, 0,(float)BUTTON_X);
      pointB.y = cnorm(self, 1,(float)BUTTON_Y);
      
      /* Did the user click on a template object? */
      item = select_item(self, pointA, NULL, NULL, pe_none);
      if (SCREEN_MODE==GMEDITOR){
        if (item!=NULL) {
          delete_list(&hold_selected_items);
          append_to_list(item,&hold_selected_items);
          PyVCS_updateVCSsegments(self, update_args);
          draw_selection_box(item->extent,248);
          PyVCS_backing_store(self, args);
        }
      }
      if (SCREEN_MODE == TEDITOR)
        {
          verify_extent(&hold_selected_items);
          /* 		        printf("ButtonRelease 3 : just_moved = %d; action = %d \n", just_moved, action); */
          /* Check to see if we are supposed to update the template
           *  and canvas because we did a move or resize */
          if (action >= 0)
            {
              if ((control == 0) && (just_moved == 0)) {
                delete_list(&hold_selected_items);
                append_to_list(item,&hold_selected_items);
                update_template_toggle_color(self, hold_selected_items);
              }
              resize_or_move(self, pointA, pointB, action, &hold_selected_items, 0);
              PyVCS_updateVCSsegments(self, update_args);
              draw_selected(hold_selected_items,0);
              PyVCS_backing_store(self, args);
              update_template_gui(self);
              /*action = -1;   DNW - Don't change the action  to -1 on a release */
            }
          else 
            {
              set_cursor(self, pointB, cursor, &action, item);
              if (pointA.x != pointB.x || pointA.y != pointB.y)
                {
                  /* DNW - For Zooming later
                     delete_list(&hold_selected_items);
                     select_all_in_range(self,pointA,pointB,&hold_selected_items);
                     printf("Got here 1\n");
                     *                                PyVCS_updateVCSsegments(self, update_args);*
                     printf("Got here 2\n");
                     draw_selected(hold_selected_items,0);
                     printf("Got here 3\n");
                     PyVCS_backing_store(self, args);
                     printf("Supposedly updated canvas\n");
                  */
                }
              else
                {
                  if (item != NULL)
                    {
                      /* Multiple selection mode */
                      if (control)
                        {
                          /* If we clicked on the data, delete all
                           * other selected objects since we only 
                           * want to select or unselect the data */
                          if (item->next != NULL)
                            delete_list(&item->next->next);
			  
                          /* If item is already in list, delete
                           * the item and unselect it on the VCS
                           * canvas */
                          if (in_list(item,&hold_selected_items))
                            {
                              remove_from_list(item,&hold_selected_items);
                              PyVCS_updateVCSsegments(self, update_args);
                              draw_selected(hold_selected_items,0);
                              PyVCS_backing_store(self, args);
                              update_template_toggle_color(self, hold_selected_items);
                              delete_list(&item);
                            }
                          /* If the item is not already in the list,
                           * add it and draw a box around it */
                          else
                            {
                              append_to_list(item,&hold_selected_items);
                              draw_selected(hold_selected_items,0);
                              PyVCS_backing_store(self, args);
                              update_template_toggle_color(self, hold_selected_items);
                            }
                        }
                      /* Single selection mode 
                       * In single selection mode, delete all 
                       * previously selected objects and redraw
                       * just the one that was selected */
                      else
                        {
                          delete_list(&hold_selected_items);
                          append_to_list(item,&hold_selected_items);
                          PyVCS_updateVCSsegments(self, update_args);
                          draw_selected(hold_selected_items,0);
                          PyVCS_backing_store(self, args);
                          did_switch_templates = switch_templates(self, hold_selected_items);
                          if (did_switch_templates) {
                            PyVCS_updateVCSsegments(self, update_args);
                            draw_selected(hold_selected_items,0);
                            PyVCS_backing_store(self, args);
                          }
                          update_template_toggle_color(self, hold_selected_items);
                        }
                    }
                  /* We did not select an object */
                  else
                    {
                      if (hold_selected_items != NULL)
                        {
                          delete_list(&hold_selected_items);
                          PyVCS_updateVCSsegments(self, update_args);
                          PyVCS_backing_store(self, args);
                          update_template_toggle_color(self, hold_selected_items);
                        }
                    }
                }   
            }
        }
      /* We are in data selection mode so we need to figure
       * out what the x and y coordinates are in addition
       * to the value at that point and the color used in
       * the colormap */
      else 
        {
          if (SCREEN_MODE == DATA) {
            if (BUTTON==LEFT_BUTTON) 
              {
                /* Destroy the data and coordinate window */ 
#ifdef X11WM
                if (data_window != (Window)NULL) {
                  XDestroyWindow(self->connect_id.display, data_window);
                  data_window = (Window)NULL;
                  waiting = 0; /* Tell expose event to do its updating when necessary */
                  if (self->connect_id.canvas_pixmap != (Pixmap)NULL) {
                    gc = DefaultGC(self->connect_id.display,
                                   DefaultScreen(self->connect_id.display));
                    XGetWindowAttributes(self->connect_id.display, 
                                         self->connect_id.drawable, &xwa);
                    XCopyArea(self->connect_id.display,
                              self->connect_id.canvas_pixmap,
                              self->connect_id.drawable,
                              gc, 0,0, xwa.width, xwa.height, 0, 0);
                  }
                }
#elif defined QTWM
                vcs_legacy_Qt_hide_info(self->connect_id.wkst_id);
#else
                printf("insert here your WM Calls to remove data/coord window\n");
#endif
              }
	    
          }
          /* We are in data zoom mode so we need to figure
           * out what the x and y coordinates are, then redraw
           * the plot.
           */
          else 
            if (SCREEN_MODE == ZOOM)
              SCREEN_MODE = DATA;
        }
      break;
    case VCS_CirculateNotify:
//       fprintf(stderr,"ok got a circulate event\n");
      break;
    case VCS_ConfigureNotify:
#ifdef QTWM
    case QEvent::Type(QEvent::Resize):
      // if (QApplication::mouseButtons()&Qt::LeftButton) {
      // 	printf("Ok we are resizing and the left button is on\n");
      // 	break;
      // }
#endif
      setup_canvas_globals(self);
      /* Set up the VCS Canvas id and XGKS workstation */
      //printf("THe workstation id is: %i\n",self->wkst_id);
      connect_id = self->connect_id;
//       fprintf(stderr,"Received a configure notify\n");
      /* This is needed for animation to grab the correct window screen. If raise window is not here, then */
      /* animations will flash if the canvas was resized prior to animating. */
#ifdef X11WM
      if ( (self->gui == 1) && (self->connect_id.display != NULL) && (self->connect_id.drawable != 0) ) 
        XRaiseWindow(self->connect_id.display, self->connect_id.drawable);
#elif defined(QTWM)
      if ((self->gui == 1) && (self->connect_id.cr!=NULL)) {
        vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
      }
#else
      fprintf(stderr,"insert your WM raise func here \n");
#endif
      
      /*                   To get the "Display content in resizing windows" to work, I had
                           to remove the below two lines and add the next four. With this 	
                           addition the other way will not work. The way I have it now is
                           the default since Linux RedHat 8.x won't allow you to change the
                           settings.
      */
//       if (VCS_RESIZING) /* Only do resize events */
//         break;
      /* Doing the portrait/landscape check */
#ifdef X11WM
      XGetWindowAttributes(connect_id.display, connect_id.drawable, &xwa);
      width =xwa.width;
      height = xwa.height;
      /* ok here we try to destroy the cairo shit and all */
#ifdef CAIRODRAW
      if (self->connect_id.cr != NULL) {
// 	fprintf(stderr,"need to create a new win\n");
	vcs_legacy_acquire_update();
	cairo_destroy(self->connect_id.cr);
	cairo_surface_destroy(self->connect_id.surface);
// 	fprintf(stderr,"ok created a new cairo surface with the good dims %ix%i and visual: %p\n",width,height,self->connect_id.visual);
        self->connect_id.surface = cairo_xlib_surface_create(self->connect_id.display,
							self->connect_id.drawable,
							self->connect_id.visual,
							width,
							height);
// 	fprintf(stderr,"in event loop created image surface to cairo surface, status: %s\n",cairo_status_to_string(cairo_surface_status(self->connect_id.surface)));
	cairo_surface_mark_dirty(self->connect_id.surface);
        self->connect_id.cr = cairo_create(self->connect_id.surface);
	vcs_legacy_release_update();
      }
#endif
#elif defined(QTWM)
      vcs_legacy_Qt_get_window_dimensions_by_id(self->connect_id.wkst_id,&rxpos,&rypos,&width,&height);
#else
      printf("insert your WM getwin geom hre\n");
#endif
      if (width>height) {
        strcpy(Page.page_orient,"landscape");
        self->orientation=0;
      }
      else {
        strcpy(Page.page_orient,"portrait");
        self->orientation=1;
      }
      
      /* Use the continents type of the original plot*/
      hold_continents = Dc.selected;
      Dc.selected = self->savecontinents; /* Set to saved Continent's flag */
      /*                      resize_flg = 1; */
      /*PyVCS_clear(self,NULL);*/
      
      /* Discard all events on the input queue. Including those that
         were on the queue before XSync() was called. Without this 
         call, the VCS Canvas window will flash for a considerably
         long time after the initial resize event. This is because 
         the user has the "Display content in resizing windows" 
         turned on in the windowing enviornment. This works whether
         the "Display content" is toggled on or off.*/
#ifdef X11WM
      XSync(self->connect_id.display,TRUE);
#elif defined (QTWM)
      // nothing to do really
#else
      printf("insert your WM sync call here\n");
#endif
      
      /* Debug printf("Resize the window! send_event = %d\n", event.xany.send_event);
       * if the orientation command was called, then don't do a resize *
       if (orientation_flg==1) {
       orientation_flg=0;
       return;
       }*/
      
      /* Reset the canvas to landscape or portrait */
      if (strcmp(Page.page_orient,"landscape") == 0)
        {
          strcpy(type,"portrait");
          change_orientation(type,&self->connect_id, 2);
        }
      else if (strcmp(Page.page_orient,"portrait") == 0)
        {
          strcpy(type,"landscape");
          change_orientation(type,&self->connect_id, 1);
        }
      
      /* Reset the canvas flag settings back */
      if (strcmp(Page.page_orient,"landscape") == 0)
        strcpy(Page.page_orient, "portrait");
      else if (strcmp(Page.page_orient,"portrait") == 0)
        strcpy(Page.page_orient, "landscape");
      
      /* DNW-9/29/04                     draw_selected(hold_selected_items,0);
         PyVCS_backing_store(self, args);
         DNW-9/29/04 */
      
      /* Set up the magnification table, used for animation */
      //setup_the_magnify_table();
      
      
      /*                      resize_self = self; /\* Store resize connection id *\/ */
      display_resize_plot( self, undisplay_resize_plot( self ) );

      Dc.selected = hold_continents; /* Restore continent's flag */
      
      /* 		     resize_flg=0; */
      
      /* Display background graphics segments */
      /* DNW-9/29/04                     if (self->background != NULL)
         PyVCS_showbg(self, args);
         DNW-9/29/04 */
      break;
    case VCS_CreateNotify:
//       fprintf(stderr,"createnotify\n");
      break;
    case VCS_ClientMessage:
//       fprintf(stderr,"clientmsg\n");
      break;
    case VCS_DestroyNotify:
//       fprintf(stderr,"destroyed\n");
      /* Debug printf("I have Destroyed the VCS Canvas!\n");*/
      /*
       * Close the VCS Canvas, and leave the X main loop thread.
       * We are done!
       */
      PyVCS_close(self, args);
      self->stopxmainloop = 1;
      self->havexmainloop = 0;
      
      break;
    case VCS_EnterNotify:
//       fprintf(stderr,"enter\n");
      break;
    case VCS_LeaveNotify:
//       fprintf(stderr,"leave\n");
      break;
    case VCS_FocusIn:
//       fprintf(stderr,"I have just focus in canvas number %d\n", self->canvas_id);
      break;
    case VCS_FocusOut:
//       fprintf(stderr,"I have just focus out canvas number %d\n", self->canvas_id);
      break;
    case VCS_Expose:
//       fprintf(stderr,"I have Exposed the VCS Canvas!\n");
      /* Get the graphics contents. */
      if (waiting == 0) { /* wait only if displaying the data and coordinate window */
#ifdef X11WM
        if (self->connect_id.canvas_pixmap != (Pixmap)NULL) {
          gc = DefaultGC(self->connect_id.display,
                         DefaultScreen(self->connect_id.display));
          XGetWindowAttributes(self->connect_id.display,
                               self->connect_id.drawable, &xwa);
          XCopyArea(self->connect_id.display,
                    self->connect_id.canvas_pixmap,
                    self->connect_id.drawable,
                    gc, 0,0, xwa.width, xwa.height, 0, 0);
        }
#elif defined(QTEM)
        vcs_legacy_Qt_repaint_window_by_id(self->wkst_id);
#else
        fprintf(stderr,"insert your WM call here\n");
#endif
      }
      break;
    case VCS_GraphicsExpose:
//       fprintf(stderr,"Graph expose\n");
      break;
    case VCS_GravityNotify:
//       fprintf(stderr,"gravity\n");
      break;
    case VCS_KeyPress:
//       fprintf(stderr,"got a key press\n");
      /* If Delete or Backspace was pressed, change the
       * priority of all selected objects to 0 and replot.
       * This simulates getting rid of the object on the 
       * plot. */
#ifdef X11EM
      XLookupString((XKeyEvent *)&event,buffer,bufsize,&keysym,&compose);
#elif defined (QTEM)
      keysym = ((QKeyEvent *)event)->key();
#endif
      if (keysym == BACKSPACE_KEY || keysym == DELETE_KEY) {
        zero_priority(&hold_selected_items);
        delete_list(&hold_selected_items);
        PyVCS_updateVCSsegments(self, update_args);
        PyVCS_backing_store(self, args);
        update_template_gui( self );
      } else if (keysym == RIGHT_KEY) {
        just_moved = 1;
        action = 0;
        pointB=pointA;
        pointB.x = cnorm(self, 0,(float)(ipointA[0]+3));
        resize_or_move(self, pointA, pointB, action, &hold_selected_items, 1);
        PyVCS_updateVCSsegments(self, update_args);
        update_extent(pointA, pointB, action, &hold_selected_items);
        draw_selected(hold_selected_items,0);
        PyVCS_backing_store(self, args);
        update_template_gui(self);
      } else if (keysym == LEFT_KEY) {
        just_moved = 1;
        action = 0;
        pointB=pointA;
        pointB.x = cnorm(self, 0,(float)(ipointA[0]-3));
        resize_or_move(self, pointA, pointB, action, &hold_selected_items, 2);
        PyVCS_updateVCSsegments(self, update_args);
        update_extent(pointA, pointB, action, &hold_selected_items);
        draw_selected(hold_selected_items,0);
        PyVCS_backing_store(self, args);
        update_template_gui(self);
      } else if (keysym == UP_KEY) {
        just_moved = 1;
        action = 0;
        pointB=pointA;
        pointB.y = cnorm(self, 1,(float)(ipointA[1]-3));
        resize_or_move(self, pointA, pointB, action, &hold_selected_items, 3);
        PyVCS_updateVCSsegments(self, update_args);
        update_extent(pointA, pointB, action, &hold_selected_items);
        draw_selected(hold_selected_items,0);
        PyVCS_backing_store(self, args);
        update_template_gui(self);
      } else if (keysym == DOWN_KEY) {
        just_moved = 1;
        action = 0;
        pointB=pointA;
        pointB.y = cnorm(self, 1,(float)(ipointA[1]+3));
        resize_or_move(self, pointA, pointB, action, &hold_selected_items, 4);
        PyVCS_updateVCSsegments(self, update_args);
        update_extent(pointA, pointB, action, &hold_selected_items);
        draw_selected(hold_selected_items,0);
        PyVCS_backing_store(self, args);
        update_template_gui(self);
      }
      break;
    case VCS_KeyRelease:
//       fprintf(stderr,"k release\n");
      break;
    case VCS_MapNotify:
//       fprintf(stderr,"mapnotif\n");
      break;
    case VCS_MotionNotify:
//       fprintf(stderr,"got a motion notify\n");
      if (SCREEN_MODE == TEDITOR) 
        {
          /* Record the current location of the mouse */
          pxy.x = cnorm(self, 0,(float)BUTTON_X);
          pxy.y = cnorm(self, 1,(float)BUTTON_Y);
	  
          /* If the left mouse button is being pressed, 
           * change the just_moved flag to 1 and we're not
           * moving an object, do a backing_store of the image */
          if (LEFT_BUTTON_PRESSED_WHILE_MOVING)
            {
              just_moved = 1;
#ifdef X11WM
              if (self->connect_id.canvas_pixmap != (Pixmap)NULL) {
                gc = DefaultGC(self->connect_id.display,
                               DefaultScreen(self->connect_id.display));
                XGetWindowAttributes(self->connect_id.display, 
                                     self->connect_id.drawable, &xwa);
                XCopyArea(self->connect_id.display,
                          self->connect_id.canvas_pixmap,
                          self->connect_id.drawable,
                          gc, 0,0, xwa.width, xwa.height, 0, 0);
              }
#elif defined (QTWM)
	      vcs_legacy_Qt_repaint_window_by_id(self->wkst_id);
#else
              fprintf(stderr,"insert your WM backing store thingy here\n");
#endif
              /*                             Selecting a range with lasso */
              if (action == -1)
                {
                  /* DNW - Don't lasso at this time. Do later! This will be used for selecting multiple objects
                     selection[0].x = selection[3].x = selection[4].x = temppoint.x;
                     selection[1].x = selection[2].x = pxy.x;
                     selection[0].y = selection[1].y = selection[4].y = temppoint.y;
                     selection[2].y = selection[3].y = pxy.y;
		     
                     gsplci(241);
                     gsln(2);
                     gpl(5,selection);     */
                }
              else
                {  /* Moving or resizing objects, draw shadow */
                  update_extent(temppoint, pxy, action, &hold_selected_items);
                  temppoint.x = pxy.x; 
                  temppoint.y = pxy.y; 
                  if (action>100) draw_selected(hold_selected_items,1);
                  else draw_selected(hold_selected_items,2);
                }
            }
          else {
//             fprintf(stderr,"MotionNotify: just_moved = %d; action = %d; pxy.x = %f, pxy.y = %f \n", just_moved, action, pxy.x, pxy.y);
            set_cursor(self, pxy, cursor, &action, hold_selected_items);
          }
       } 
//       else {
//          fprintf(stderr,"nothing to do\n");
//       }
      break;
    case VCS_NoExpose:
//       fprintf(stderr,"no expose\n");
      break;
    case VCS_ReparentNotify:
//       fprintf(stderr,"repar\n");
      break;
    case VCS_UnmapNotify:
//       fprintf(stderr,"unmap\n");
      /*                     if (resize_flg == 1) { */
      /*                        printf("Do the REPLOT! canvas id = %d\n", resize_self->canvas_id); */
      /*                        display_resize_plot( resize_self ); */
      /*                        resize_flg = 0; */
      /*                     } */
      break;
    default: 
      //printf("ok got something else: %i\n",event->type());
      /*Debug printf(stderr,"VCS got unexpected event type %d.\n", event.type);*/
      break;               /* ignore all other X events */
    }  /* switch */
    /*     } /\* BLOCK *\/ */
    
  } /* if */
  //Py_XDECREF(update_args);
}
 
extern "C" PyObject *
PyVCS_startxmainloop(PyVCScanvas_Object *self, PyObject *args)
{

#ifdef X11EM
  XEvent event;
  //  extern void event2_handler(PyVCScanvas_Object *self, XEvent event);
#elif defined (QTEM)
  QEvent *event;
  //  extern void event3_handler(PyVCScanvas_Object *self, QEvent *event);
#endif
#ifdef USEX11
  if (self->connect_id.display == NULL) {
#else
    if (self->connect_id.cr == NULL ) {
#endif
      /* Return NULL Python Object, Program is in background mode! */
      Py_INCREF(Py_None);
      return Py_None;
    }
    
    /* If the VCS Canvas is not open, then return. */
#ifdef USEX11
    if (self->connect_id.drawable == 0) {
#else
      if (self->connect_id.cr == NULL) {
#endif
        PyErr_SetString(PyExc_TypeError, "Must first open VCS (i.e., x.open()).");
        return NULL;
      }
      
      if (self->havexmainloop == 1) {
        /* Return NULL Python Object, X main loop is already running! */
        Py_INCREF(Py_None);
        return Py_None;
      } else {
        self->stopxmainloop = 0;
        self->havexmainloop = 1;
      }
      
      /*         XSelectInput(self->connect_id.display,self->connect_id.drawable, ButtonPressMask | ButtonReleaseMask | EnterWindowMask | LeaveWindowMask | FocusChangeMask | ExposureMask | PointerMotionMask ); */
      /* Catch all X calls, start the X main loop.*/
      /*        Py_BEGIN_ALLOW_THREADS
                { PyThreadState *_save_x_main;
                _save_x_main = PyEval_SaveThread();*/
      
      /* Begin Python Threads to allow for the calling of Python from C */
      Py_BEGIN_ALLOW_THREADS
        /*         PY_ENTER_THREADS */

#ifdef X11WM
        Cursor cursor[11]; 
      cursor[0] = XCreateFontCursor(self->connect_id.display,XC_fleur);
      cursor[1] = XCreateFontCursor(self->connect_id.display,XC_top_left_corner);
      cursor[2] = XCreateFontCursor(self->connect_id.display,XC_top_right_corner);
      cursor[3] = XCreateFontCursor(self->connect_id.display,XC_bottom_right_corner);
      cursor[4] = XCreateFontCursor(self->connect_id.display,XC_bottom_left_corner);
      cursor[5] = XCreateFontCursor(self->connect_id.display,XC_top_side);
      cursor[6] = XCreateFontCursor(self->connect_id.display,XC_right_side);
      cursor[7] = XCreateFontCursor(self->connect_id.display,XC_bottom_side);
      cursor[8] = XCreateFontCursor(self->connect_id.display,XC_left_side);
      cursor[9] = XCreateFontCursor(self->connect_id.display,XC_sizing);
      cursor[10] = XCreateFontCursor(self->connect_id.display,XC_pencil);
#elif defined (QTWM)
#include <QtGui/QCursor>
      QCursor cursor[11];
      cursor[0]= QCursor( Qt::SizeAllCursor);
      cursor[1]= QCursor( Qt::SizeFDiagCursor );
      cursor[2]= QCursor( Qt::SizeBDiagCursor);
      cursor[3]= QCursor( Qt::SizeFDiagCursor);
      cursor[4]= QCursor( Qt::SizeBDiagCursor);
      cursor[5]= QCursor( Qt::SizeVerCursor);
      cursor[6]= QCursor( Qt::SizeHorCursor);
      cursor[7]= QCursor( Qt::SizeVerCursor);
      cursor[8]= QCursor( Qt::SizeHorCursor);
      cursor[9]= QCursor( Qt::SizeAllCursor);
      cursor[10]= QCursor( Qt::CrossCursor);
#else
      printf("insert your WM cursor defining here\n");
#endif

#ifdef X11EM
      while (!self->stopxmainloop) {
        X_QUE_COUNT = XQLength(self->connect_id.display);
        XNextEvent(self->connect_id.display, &event);
        event_handler(self,event);
      }
#elif defined (QTEM)
      /* nothing to do here already handled by QT itself */
      /* in the Qt event funct we will call event_handler */
#else
      printf("insert here your event manger queing event counter\n");
      printf("insert here your event manger queing call\n");
#endif

      /*Py_END_ALLOW_THREADS
        PyEval_RestoreThread(_save_x_main); }*/
      Py_END_ALLOW_THREADS
        /* 	PY_LEAVE_THREADS */

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
      return Py_None;
    }
