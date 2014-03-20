/************************************************************************
 *             PCMDI's Climate Data Analysis Tool - (CDAT)              *
 *              Command Line User Interface Popup Window                *
 *                         Developed at LLNL                            *
 *                                                                      *
 *                                                                      *
 *      Copyright (C) 1997. The Regents of the University of California.*
 *      All rights reserved.                                            *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *      Author: Dean N. Williams                 			*
 *                                                                      *
 *      Date: 04/05/97                                                  *
 *                                                                      *
 *      File name: vcs_legacy_canvas.c                                         *
 *                                                                      *
 *                                                                      *
 *      Langague: ANSI C                                                *
 *                                                                      *
 *                                                                      *
 *      Comments:                                                       *
 *              This function calls the function that creates the       *
 *              output window for the VCS Canvas user interface.        *
 *                                                                      *
 *                                                                      *
 *      Modifications:                                                  *
 *                                                                      *
 *                                                                      *
 *      Contact:                                                        *
 *                                                                      *
 *              Dean N. Williams                                        *
 *                                                                      *
 *              LLNL                                                    *
 *              PO Box 808, L-264                                       *
 *              Livermore, CA                                           *
 *              94550                                                   *
 *                                                                      *
 *              (510) 423-0145                                          *
 *                                                                      *
 *                                                                      *
 ************************************************************************
 */
/****************************************************************************
*** Includes ****************************************************************
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "workstations.h"
#include "vcs_legacy_canvas.h"
#include "color_editor.h"
#include "animation.h"
#include "gks.h"

/****************************************************************************
*** Prototypes **************************************************************
*****************************************************************************/

/****************************************************************************
*** Global Variables ********************************************************
*****************************************************************************/
CANVASINFO_LINK		head_canvas_info=NULL;/* store connection ID info */
#ifdef X11WM
Pixmap                 	canvas_pixmap=(Pixmap)NULL;/*used as the backing store*/
#endif
int			a_hori=0, a_vert=0;  /* magnification routines*/
int  			orientation_flg=0;/* if orientation, stop resize */
MAG_INFO magnify_table[Magnify_Table_Length];/* magnification values */

/****************************************************************************
*** Extern Global Variables *************************************************
*****************************************************************************/
extern struct workstations Wkst[];
extern Gconid_X_drawable connect_id;
extern struct orientation Page;

/****************************************************************************
*** VCS Canvas DrawingArea Translations *************************************
*** ManagerGadget functions are necessary for DrawingArea widgets ***********
*** that steal away button events from the normal translation tables. *******
*****************************************************************************
"<Enter>:   change_canvas_cursor(in) grabbed()  \n\
<Leave>:    change_canvas_cursor(out) ungrabbed()";
"<FocusOut>:   DrawingAreaInput() ManagerFocusOut()\n\
<FocusIn>:   DrawingAreaInput()  ManagerFocusIn()\n\
<EnterWindow>:   DrawingAreaInput()  ManagerEnter()\n\
<LeaveWindow>:    DrawingAreaInput() ManagerLeave()";

"<FocusIn>:   change_canvas_cursor(in) ManagerFocusIn() \n\
<FocusOut>:   change_canvas_cursor(out) ManagerFocusOut() \n\
 */ 

/*
XtActionsRec    canvas_actions [] = { {"change_canvas_cursor", (XtActionProc)change_canvas_cursor}, };
*/

char canvas_translations[] =
"<EnterWindow>:   change_canvas_cursor(in) ManagerEnter() \n\
<LeaveWindow>:    change_canvas_cursor(out) ManagerLeave()";

/****************************************************************************
*** Global Variables Functions **********************************************
*****************************************************************************/


void store_vcs_legacy_connection_information(Gconid_X_drawable connect_id,int wkst_id) /* XGKS workstation ID number,  VCS canvas info ID */
{
        CANVASINFO_LINK		cptr, tcptr; /* connection ID pointers */

	/* Setup pointer */
        cptr = (CANVASINFO_LINK) malloc(sizeof(CANVASINFO));
	cptr->connect_id = connect_id;
        cptr->wkst_id = wkst_id;
        cptr->dlist = NULL;
        cptr->head_animation_memory = NULL;
        cptr->tail_animation_memory = NULL;
        cptr->head_animation_list = NULL;
        cptr->next = NULL;

	/* Put pointer in link list */
	if ( head_canvas_info == NULL) {
            head_canvas_info = cptr;
	} else {
 	   tcptr = head_canvas_info;
           while (tcptr->next != NULL)
               tcptr = tcptr->next;
           tcptr->next = cptr;

	}
}

void remove_vcs_legacy_connection_information(connect_id, wkst_id)
Gconid_X_drawable connect_id;           /* VCS canvas info ID */
int wkst_id;                            /* XGKS workstation ID number */
{
        CANVASINFO_LINK         cptr, tcptr; /* connection ID pointers */
	display_name_list 	*dptr, *tdptr;
	extern ANIMATIONWINDOWLIST_LINK head_animation_window_list; 
	extern void		free_animation_file_list();
	extern void		free_animation_memory_list();
	extern void		free_animation_window_list();

        tcptr = cptr = head_canvas_info;
        while ((cptr != NULL) &&
#ifdef X11WM
               (cptr->connect_id.drawable != connect_id.drawable)) {
#else
               (cptr->connect_id.cr != connect_id.cr)) {
#endif
	    tcptr = cptr;
            cptr = cptr->next;
	}

	if (cptr == NULL)
	   return ;

	/* Free the display list and names */
	dptr = cptr->dlist;
	while (dptr != NULL) {
	   if (dptr->display_name != NULL)
	      free((char *) dptr->display_name);
	   tdptr = dptr;
	   dptr=dptr->next;
	   free((char *) tdptr);
	}

	/* Free the animation file list */
	if (cptr->head_animation_list != NULL)
	   free_animation_file_list(cptr);

	/* Free the animation frames */
        if (cptr->head_animation_memory != NULL)
	   free_animation_memory_list(cptr);

	/* Free the animation window list */
	if (head_animation_window_list != NULL)
	   free_animation_window_list(cptr);

	/* Reorder the link and free the canvas information link. */
	if (cptr == head_canvas_info){
	    head_canvas_info = cptr->next;}
	else
	    tcptr->next = cptr->next;
        free((char *) cptr);
}

void update_vcs_legacy_connection_information(connect_id, animate_popup)
Gconid_X_drawable connect_id;           /* VCS canvas info ID */
int animate_popup;                   /* animation popup */
{
        CANVASINFO_LINK         cptr=head_canvas_info;
	display_name_list *dptr, *tdptr;

	/* Point to the correct VCS Canvas information */
	if (cptr == NULL) {fprintf(stderr,"crap1!\n");};
#ifdef X11WM
//	while ((cptr != NULL) && (cptr->connect_id.drawable != connect_id.drawable))
#else
	while ((cptr != NULL) && (cptr->connect_id.cr != connect_id.cr))
#endif
//	{
//fprintf(stderr,"the connectid pointers are: %p, %p\n",&cptr->connect_id,&connect_id);
//fprintf(stderr,"the drawable pointers are: %p, %p\n",cptr->connect_id.drawable,connect_id.drawable);
//             cptr = cptr->next;
//};
cptr->connect_id=connect_id;
	if (cptr == NULL) {fprintf(stderr,"crap2!\n");};
	cptr->connect_id.animate_popup = animate_popup;

}

void store_display_name(connect_id, display_name)
Gconid_X_drawable connect_id;           /* VCS canvas info ID */
char *display_name;
{
        CANVASINFO_LINK cptr=head_canvas_info;
	display_name_list *dptr, *tdptr;

	/* Point to the correct VCS Canvas information */
#ifdef X11WM
	while ((cptr != NULL) && (cptr->connect_id.drawable != connect_id.drawable))
#else
	while ((cptr != NULL) && (cptr->connect_id.cr != connect_id.cr))
#endif
             cptr = cptr->next;

        /* Create a display name structure for linked list */
        dptr = (display_name_list *)malloc(sizeof(display_name_list));
        dptr->display_name = (char *) malloc((strlen(display_name)+1)*
                             sizeof(char)+1);
        strcpy(dptr->display_name, display_name);
        dptr->next = NULL;

        if (cptr != NULL) {
           if (cptr->dlist == NULL)
              cptr->dlist = dptr;
           else {
              tdptr = cptr->dlist;
              while (tdptr->next != NULL)
                  tdptr = tdptr->next;
              tdptr->next = dptr;
           }
        }
}

void remove_display_name(connect_id, display_name)
Gconid_X_drawable connect_id;           /* VCS canvas info ID */
char *display_name;
{
        CANVASINFO_LINK cptr=head_canvas_info;
	display_name_list *dptr, *tdptr;

	/* Point to the correct VCS Canvas information */
#ifdef X11WM
	while ((cptr != NULL) && (cptr->connect_id.drawable != connect_id.drawable))
#else
	while ((cptr != NULL) && (cptr->connect_id.cr != connect_id.cr))
#endif
             cptr = cptr->next;

        if (cptr == NULL) return;

        tdptr = dptr = cptr->dlist;
        if (dptr != NULL) {
           while ((dptr != NULL) &&
                 (strcmp(dptr->display_name, display_name) != 0)) {
	         tdptr = dptr;
                 dptr = dptr->next;
	   }

	   if (dptr != NULL) {
	      if (dptr == cptr->dlist)
                 cptr->dlist = dptr->next;
	      else
	         tdptr->next = dptr->next;
	   } else
             cptr->dlist = NULL;

           if (dptr != NULL) {
              if (dptr->display_name != NULL)
		 free((char *) dptr->display_name);
	      free((char *) dptr);
	   }
	}
}

int set_the_connect_id(w)
#ifdef X11WM
Window w;
#else
cairo_t *w;
#endif
{
        CANVASINFO_LINK		cptr=head_canvas_info;

	/* Find the correct canvas information */
#ifdef X11WM
	while ((cptr != NULL) && (cptr->connect_id.drawable != w))
#else
	while ((cptr != NULL) && (cptr->connect_id.cr != w))
#endif
	   cptr = cptr->next;

	/* Set the connection */
	if (cptr != NULL) {
           connect_id = cptr->connect_id;
	   Wkst[0].id = cptr->wkst_id;
	   return 1;
	} else
	   return 0;
}

char * return_display_name( connect_id )
Gconid_X_drawable connect_id;           /* VCS canvas info ID */
{
	CANVASINFO_LINK 	cptr=head_canvas_info;
	display_name_list 	*dptr, *tdptr;
	int			i=1;
	static int 		start=1;

	/* Point to the correct VCS Canvas information */
#ifdef X11WM
	while ((cptr != NULL) && (cptr->connect_id.drawable != connect_id.drawable))
#else
	while ((cptr != NULL) && (cptr->connect_id.cr != connect_id.cr))
#endif
	  {
	    cptr = cptr->next;
	  }

	if (cptr != NULL) {
	   if (start == 1) {
              ++start;
	      if (cptr->dlist != NULL)
	         return (cptr->dlist->display_name);
              else
                 return NULL;
	   } else {
              tdptr = cptr->dlist;
              while ((tdptr != NULL) && (i < start)) {
	          ++i;
                  tdptr = tdptr->next;
              }
	      if (tdptr != NULL) {
	         ++start;
	         return (tdptr->display_name);
	      } else {
                 start = 1;
                 return NULL;
	      }
	   }
	}

        start = 1;
        return NULL;
}

void vcs_legacy_canvas_open_cb( Gconid_X_drawable connect_id ) /* VCS canvas drawable id */
{
        /*
         * Manage VCS Canvas window, this function will map the 
         * canvas on top of all other sibling windows.
         */
#ifdef X11WM
        XMapRaised( connect_id.display, connect_id.drawable );
#endif
#ifdef QTWM
	fprintf(stderr,"opening from vcs_legacy_canvas: %i\n",connect_id.wkst_id);
	vcs_legacy_Qt_open_window_by_id(connect_id.wkst_id);
#endif
}

void vcs_legacy_canvas_quit_cb( Gconid_X_drawable connect_id) /* VCS canvas drawable id */
{

        /*
         * Unmanage the VCS Canvas window. This function removes 
         * the VCS Canavas fromt he screen.
         */
#ifdef X11WM
        XUnmapWindow( connect_id.display, connect_id.drawable );
#elif defined QTWM
	extern void vcs_legacy_Qt_destroy_window(int index);
	vcs_legacy_Qt_destroy_window(connect_id.wkst_id);
#else
	fprintf(stderr,"insert your WM unmap func here\n");
#endif

}

void reset_canvas_geometry(Gconid_X_drawable connect_id, int width, int height, int xpos, int ypos)
{
	/* Change the VCS Canvas geometry */
#ifdef X11WM
        XMoveResizeWindow(connect_id.display, connect_id.drawable,
                          xpos, ypos,
                          width, height);

	XMapRaised( connect_id.display, connect_id.drawable );
#elif defined QTWM
	extern  void vcs_legacy_Qt_resize_window(int index,int x,int y,int w, int h);
	vcs_legacy_Qt_resize_window(connect_id.wkst_id,xpos,ypos,width,height);
#else
	fprintf(stderr,"insert your WM resize here\n");
#endif
}

void return_canvas_geometry(Gconid_X_drawable connect_id, int *width, int *height, int *xpos, int *ypos)
{
#ifdef X11WM
        Window		rroot_win;
#elif defined QTWM
	int w,h;
#endif
	unsigned int   	rwidth, rheight, rborder, rdepth;
        int		rxpos, rypos;
        

	/* Change the VCS Canvas geometry */
#ifdef X11WM
        XGetGeometry(connect_id.display, connect_id.drawable,
                     &rroot_win,
                     &rxpos, &rypos,
                     &rwidth, &rheight,
                     &rborder, &rdepth );
	*width  = (int) rwidth;
	*height = (int) rheight;
#elif defined QTWM
	vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&rxpos,&rypos,&w,&h);
	*width  = (int) w;
	*height = (int) h;
#else
	fprintf(stderr,"insert your WM getgeometry func here\n");
	*width  = (int) rwidth;
	*height = (int) rheight;
#endif
	*xpos   = (int) rxpos;
	*ypos   = (int) rypos;
}

/* #ifdef X11OUT */
/* void draw_canvas_box_cdat(widget, event, args, num_args) */
/* Widget widget; */
/* XButtonEvent *event; */
/* char **args; */
/* int *num_args; */
/* { */
/* 	if (event->button == 1) */
/* 		printf("button 1 was pressed!\n"); */
/* } */
/* #endif */

/* #ifdef X11OUT */
/* void change_canvas_cursor(widget, event, args, num_args) */
/* Widget widget; */
/* XCrossingEvent *event; */
/* char **args; */
/* int *num_args; */
/* { */
/* 	if (*num_args != 1) */
/* 	    return ; */

/*         printf("I'm IN!\n"); */

/* 	if (strcmp(args[0], "in")==0) { */
/* 	  printf("Entering VCS Canvas Window!\n"); */
/* 	} else if (strcmp(args[0], "out")==0) { */
/* 	  printf("Leaving VCS Canvas Window!\n"); */
/* 	} */
/* } */
/* #endif */


/* 
 * Create the VCS Canvas pixmap named canvas_pixmap.
 * canvas_pixmap is used to restore the VCS Canvas.
 */
#ifdef X11WM
Pixmap create_pixmap(Gconid_X_drawable connect_id)
{
        XWindowAttributes       xwin_attr;
        int                     xwidth, yheight;
        int                     depth;
        Screen *                scrn;
 
        /* Get the window attributes of the VCS Canvas. */
        XGetWindowAttributes(connect_id.display,connect_id.drawable,&xwin_attr);
        xwidth  = xwin_attr.width;
        yheight = xwin_attr.height;
        scrn    = xwin_attr.screen;
        depth   = xwin_attr.depth;

        /* Return a pixmap the same size as the drawing area. */
/* 	fprintf(stderr,"creating a Pixmap of size: %i,%i, depth: %i\n",xwidth, yheight, depth); */
        return (XCreatePixmap(connect_id.display,
                          connect_id.drawable, xwidth, yheight, depth));
/*         return (XCreatePixmap(connect_id.display, */
/*                           RootWindowOfScreen(scrn), xwidth, yheight, depth)); */

}
#endif
/* 
 * Copy the VCS Canvas to a pixmap image. This pixmap, called 
 * canvas_pixmap, is used to restore the VCS Canvas if the
 * canvas needs restoring.
 */
#ifdef X11WM
Pixmap copy_pixmap(Gconid_X_drawable connect_id,int canvas_id)
{
        XWindowAttributes       xwin_attr;
        XSetWindowAttributes    attrib;
        Pixmap			canvas_pixmap = (Pixmap) NULL;
        unsigned long valuemask = 0; /* ignore XGCvalues and use defaults */
        Widget                  canvas;
        GC                      gc; /* graphics context */
        int                     xwidth, yheight;
        int                     depth, screen_num;
        Screen *                scrn;
        Pixmap 			create_pixmap();

	/* Check to make sure the canvas exist. */
	if ((connect_id.display == NULL) || (connect_id.drawable == 0))
	   return canvas_pixmap;

        /* Get the window attributes of the VCS Canvas. */
        XGetWindowAttributes(connect_id.display,connect_id.drawable,&xwin_attr);
        xwidth  = xwin_attr.width;
        yheight = xwin_attr.height;
/*
        scrn    = xwin_attr.screen;
        depth   =xwin_attr.depth;

	* Set and get the VCS Canvas attributes *
        attrib.backing_store = NotUseful;
        attrib.save_under = False;
        attrib.backing_pixel = True;
        attrib.backing_planes = True;
        attrib.colormap = connect_id.n_cmap;
        valuemask = CWBackingStore|CWBackingPlanes|CWBackingPixel|CWSaveUnder|CWColormap;
        XChangeWindowAttributes(connect_id.display,connect_id.drawable,
                               valuemask, &attrib);
*/

        /* Get the graphics contents. */
        screen_num = DefaultScreen(connect_id.display);
        gc = DefaultGC(connect_id.display,screen_num);

        /* Copy the canvas to the pixmap. *
        if (connect_id.canvas_pixmap != (Pixmap) NULL)
           XFreePixmap(connect_id.display, connect_id.canvas_pixmap);*/
        //printf("PIXMAP 1: canvas_pixmap %d = %p\n", canvas_id, connect_id.canvas_pixmap);
        canvas_pixmap = create_pixmap(connect_id); /*create Canvas pixmap*/
	//fprintf(stderr,"xcopy5\n");
	  XCopyArea(connect_id.display, connect_id.drawable,
		    canvas_pixmap, gc, 0, 0, xwin_attr.width, xwin_attr.height, 0, 0);
	  //printf("PIXMAP 2: canvas_pixmap %d = %p\n", canvas_id, connect_id.canvas_pixmap);
	  
	  /* 
	   * This is needed to expose the pixmap when called for the first time
	   * in the routine drawing_expose_resize_CB. Without it the XCopyArea 
	   * does not get called in time.
	   */
	  //fprintf(stderr,"ok we are actually syncing\n");
	  XSync(connect_id.display,FALSE);
	  //fprintf(stderr,"ok we are actually flusihne\n");
	  XFlush(connect_id.display);
	  //fprintf(stderr,"ok we are actually out of here\n");
	return canvas_pixmap;
}
#endif

/* 
 * X callback that redraws the plot(s) on the VCS Canvas if the 
 * user resizes the screen, or if the VCS Canvas is exposed.
 */
void drawing_expose_resize_CB( event )
int event;
{
#ifdef X11WM
        XWindowAttributes xwa;
        Dimension       xwidth, yheight;
        extern Boolean  quit_animation;
        GC              gc; /* graphics context */
#endif
        int             screen_num;
        char            type[10];
        extern int      change_orientation();
        extern void     change_to_full_screen();
        extern void     grid_on_off();
	void		setup_the_magnify_table();
	int 		set_the_connect_id();
	extern void 	deactivate_all_other_wksts();

#ifdef X11WM
        if (canvas_pixmap == (Pixmap) NULL)
           return;
#else
        fprintf(stderr,"insert your WM pixmap cache here\n");
#endif

	/* Set the connection ID, which points to the correct VCS Canvas */
#ifdef X11WM
	if (!set_the_connect_id(connect_id.drawable))
#else
	if (!set_the_connect_id(connect_id.cr))
#endif
           return;

	deactivate_all_other_wksts(Wkst[0].id);

#ifdef X11WM
        /* Get the graphics contents. */
        screen_num = DefaultScreen(connect_id.display);

        gc = DefaultGC(connect_id.display,screen_num);

/*        if (cb->reason == XmCR_EXPOSE) { * Redraw the VCS Canvas */
        if (event == 1) { /* Redraw the VCS Canvas, My version of Backing_Store */
           XGetWindowAttributes(connect_id.display, connect_id.drawable, &xwa);
  
        /*   XSync(connect_id.display,FALSE);
           XFlush(connect_id.display);*/
	   //fprintf(stderr,"yeppppp 6\n");
           XCopyArea(connect_id.display, connect_id.canvas_pixmap,
                     connect_id.drawable, gc, 0,0, xwa.width, xwa.height, 0, 0);
           return ;
        } else if (event == 2) {
	   /* if the orientation command was called, then don't do a resize */
	   if (orientation_flg==1) {
	      orientation_flg=0;
	      return;
	   }

           /* Temporary reset of the canvas flag to landscape or portrait */
           if (strcmp(Page.page_orient,"landscape") == 0)
               strcpy(type,"portrait");
           else if (strcmp(Page.page_orient,"portrait") == 0)
               strcpy(type,"landscape");

	   /* Redraw the canvas with the new orientation */
           change_orientation(type,2);

	   /* Reset the canvas flag settings back */
           if (strcmp(Page.page_orient,"landscape") == 0)
               strcpy(Page.page_orient, "portrait");
           else if (strcmp(Page.page_orient,"portrait") == 0)
               strcpy(Page.page_orient, "landscape");

	   /* Sync and flush all X calls */
           XSync(connect_id.display,FALSE);
           XFlush(connect_id.display);
        }
#else
	fprintf(stderr,"insert your WM copy to pixmap code here\n");
#endif

	/* Set up the magnification table, used for animation */
        setup_the_magnify_table();
}

/* 
 * Set up the magnification table. This is used in animation. This
 * function get the size of the VCS Canvas, then generates the 
 * appropriate magnification values to be used in the anaimation
 * zoom, horizontal pan, and vertical pan.
 */
void setup_the_magnify_table()
{
        int       xwidth, yheight; /*width and height of the VCS canvas*/
#ifdef X11WM
        XWindowAttributes xwa;
#else
	Grectangle xwa;
#endif
        int             i;

        /* Get the VCS canvas width and height */
#ifdef X11WM
        XGetWindowAttributes(connect_id.display, connect_id.drawable, &xwa);
#elif defined QTWM
	vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&xwa.x, &xwa.y, &xwa.width, &xwa.height);
#else
	fprintf(stderr,"insert your WM get geom call here\n");
#endif
        xwidth  = xwa.width;
        yheight = xwa.height;
        a_vert  = 0;
        a_hori  = 0;

        /* Create the magnification table */
        for (i = 0; i < Magnify_Table_Length; ++i) {
           magnify_table[i].mag_x = i + 1;
           magnify_table[i].mag_y = i + 1;
           magnify_table[i].width = (int)xwidth / (i + 1);
           magnify_table[i].height = (int)yheight / (i + 1);
           magnify_table[i].dx = ((int)xwidth * 0.5) -
                                  (magnify_table[i].width * 0.5);
           magnify_table[i].dy = ((int)yheight * 0.5) -
                                  (magnify_table[i].height * 0.5);
        }
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/
