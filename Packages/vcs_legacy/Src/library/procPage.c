#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#else
#include <stdbool.h>
#define Boolean bool
#define TRUE 1
#define FALSE 0
#endif
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "display.h"
#include "workstations.h"
#ifdef PYTHON
#include "vcs_legacy_canvas.h"
#endif

    extern FILE *fpin,*fpout,*fperr;

    extern struct workstations Wkst[];
    extern struct orientation Page;
    extern Gconid_X_drawable connect_id;

/*		Flag to indicate interaction off (=1) or on (=0).	*/

    extern int Inactive;
    extern int Batch_mode;

/*	Process a PAGE command.						*/

    int procPage(str,tok)

      char str[];
      int *tok;

      {
	int tokm;
	int c;

	char strm[256];

	if (*tok != '(')
	  {
	   err_warn(1,fperr,
		"Error - (PAGE) not a proper token (%c).\n",*tok);
	   return 0;
	  }
	c=getsttk(strm,&tokm);
	if (tokm != ')')
	  {
	   err_warn(1,fperr,
		"Error - not a proper token (%s%c%s%c).\n",str,*tok,strm,tokm);
	   return 0;
	  }
	if (c == 0)
	  {
	   err_warn(0,fperr,"Warning - no PAGE orientation.\n");
	   return 1;
	  }
	else
	  {
	   /*c=change_orientation(strm,1);*/
	  }
	return c;
      }

/*		Change "landscape" to "portrait" or vice versa.		*/
/*		Close down GKS and resize the display surface.		*/

    int change_orientation(char *type, Gconid_X_drawable **connect_id_in, int where_from)
      {
       int i,j;
       Gintlist wsid;
       int *pid;
       float dx,dy,dmax;

#ifdef PYTHON
       SEG_ORDER_LINK   tptr=NULL, hptr=NULL, aptr=NULL, bptr=NULL;
       Gconid_X_drawable *connect_id;
#ifdef X11WM
       Position		xvalue,yvalue;
       XWindowAttributes xwa;
       XEvent           report;
#elif defined (QTWM)
       int qw,qh,qx,qy;
#endif
       int		event_loop, *pi;
       float 		canvas_ratio_l, canvas_ratio_p;
       char	 	conec[256], *display_name;
       Boolean 		landscape_flg = TRUE;
       int 		width,height;
       Glimit 		window_shape;
       Gintlist 	gksseg;
       struct display_tab *pd;
       extern struct display_tab D_tab;
       extern char     	*formid;
       extern int       update_ind;
       extern void   	vcs_legacy_canvas_update();
#ifdef X11WM
       extern Colormap	n_cmap;
#endif
#ifdef X11DRAW
       extern Pixmap 	create_pixmap();
       extern Pixmap	copy_pixmap();
#endif
       extern char *    return_display_name();
#endif

       connect_id = (Gconid_X_drawable *)connect_id_in;
       if (cmpncs(type,"landscape") != 0 && cmpncs(type,"portrait") != 0)
	 {
	  err_warn(1,fperr,
		"Error - Page orientation type (%s) doesn't exist.\n",type);
	  return 0;
	 }
       if (cmpncs(type,Page.page_orient) == 0) return 1;
       else
	 {
	  strcpy(Page.page_orient,type);
	  Page.sw=1;
	 }

/*       if (Batch_mode)
	 {*/
          wsid.number = 0;
          wsid.integers = NULL;
          gqopwk(&wsid);
	  for (i=0,pid=wsid.integers;i<wsid.number;pid++,i++)
	    {
/*			Shutdown the screen if it's open and re-config
			and re-open it.					*/

	      if (*pid == Wkst[0].id)
		{
#ifndef PYTHON
		 shutdown(connect_id, Wkst[0].id);
		 connect_id->display=XOpenDisplay(NULL);
		 config_canvas(Wkst[0].id,Page.page_orient);
#else
		 if (strcmp(Page.page_orient, "landscape") != 0)
                   landscape_flg = FALSE;
                 if (where_from == 3) {
                    landscape_flg = TRUE;
                    where_from = 2;
                 }
#endif
		 /* Translate the display and window into a widget ID */

#ifdef X11WM
		 XSync(connect_id->display,FALSE);
		 XFlush(connect_id->display);
#elif defined (QTWM)
/*          vcs_legacy_Qt_repaint_window(connect_id); */
#else
		 printf("insert your WM flush here\n");
#endif

                /* Run through the loop to flush out all events *
                event_loop = XPending(connect_id->display);
                while (event_loop != 0) {
                      XNextEvent(connect_id->display, &report);
                      XFilterEvent(&report, connect_id->drawable);
                      event_loop = XPending(connect_id->display);
                }
*/

                /* Clear the canvas, and deactivate and close the workstation */
#ifdef X11WM
		 XClearWindow(connect_id->display,connect_id->drawable);
#elif defined (QTWM)
/* 		 cairo_set_source_rgb(connect_id->cr,0,0,100); */
/* 		 cairo_paint(connect_id->cr); */
#else
		 printf("insert your WM clear func here\n");
#endif
		 gdacwk(Wkst[0].id); /* deactivate the workstation */
		 gclwk(Wkst[0].id);  /* close the workstation */

                /* Open and activate the workstation */
#ifdef X11WM
		 sprintf(conec,formid,connect_id->drawable,
			 connect_id->display,n_cmap);
#elif defined QTWM
		 sprintf(conec,"%i",connect_id->wkst_id);
#endif
		
		 gopwk(Wkst[0].id, conec, "CANVAS");
		 gacwk(Wkst[0].id);
		 gsclip(GNOCLIP);

#ifdef X11WM
                /* Reset the colormap to active colormap. */
		 if (DefaultDepth(connect_id->display,
				  DefaultScreen(connect_id->display)) != 8)
                   set_active_colors();
#elif defined (QTWM)
		 /* not necessary here */
#else 
		 printf("inert your WM test for active colormap here\n");
#endif

#ifdef X11WM
		 /* Set the aspect ratio of the window. */
		 XGetWindowAttributes(connect_id->display, connect_id->drawable, &xwa);

	 	/* If where_from is 1, then the call originated from
	 	 * a page orientation. Thus, we want to reset the canvas
		 * to its original canvas dimensions.
		 * If where_from is 2, then the call originated from
		 * resizing the canvas. Thus, compute new canvas dimensions.
		 */
                dx=(float) xwa.width;
                dy=(float) xwa.height;
#elif defined (QTWM)
		vcs_legacy_Qt_get_window_dimensions_by_id(connect_id->wkst_id,&qx,&qy,&qw,&qh);
		dx = (float) qw;
		dy = (float) qh;
#else
		printf("insert your WM getgeom call here\n");
#endif

                dmax=(dx>dy)?dx:dy;
                window_shape.xmin=0.0;
                window_shape.xmax=dx/dmax;
                window_shape.ymin=0.0;
		window_shape.ymax=dy/dmax;

		canvas_ratio_l = dy/dx;
		canvas_ratio_p = dx/dy;
                window_shape.xmin = 0.0;
                if (strcmp(Page.page_orient, "portrait") == 0) {
                   if (where_from==2)
                     window_shape.xmax=(landscape_flg==FALSE) ? 1.0:canvas_ratio_p;
                   else
                     window_shape.xmax=(landscape_flg==TRUE) ? 1.0:canvas_ratio_p;
                   window_shape.ymin = 0.0;
                   if (where_from==2)
                     window_shape.ymax=(landscape_flg==TRUE) ? 1.0:canvas_ratio_l;
                   else
                     window_shape.ymax=(landscape_flg==FALSE) ? 1.0:canvas_ratio_l;
                } else {
                   if (where_from==2)
                     window_shape.xmax=(landscape_flg==TRUE) ? 1.0:canvas_ratio_p;
                   else
                     window_shape.xmax=(landscape_flg==FALSE) ? 1.0:canvas_ratio_p;
                   window_shape.ymin = 0.0;
                   if (where_from==2)
                     window_shape.ymax=(landscape_flg==FALSE) ? 1.0:canvas_ratio_l;
                   else
                     window_shape.ymax=(landscape_flg==TRUE) ? 1.0:canvas_ratio_l;
		}
                gswkwn(Wkst[0].id, &window_shape);

                /* set the deferral state to be "at some time" */
                gsds(Wkst[0].id,GASTI,GSUPPRESSED);

               if (where_from!=2) {
		 display_name=return_display_name(*connect_id);
		 while (display_name!=NULL) {
	           for (pd=&D_tab;pd != NULL;pd=pd->next) {
		     if (strcmp(display_name,pd->name)==0) {
		       for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4) {
			 if (*pi > 0 && *(pi+3) == 0) {
			   if((tptr=(SEG_ORDER_LINK)malloc(sizeof(SEG_ORDER))) == NULL) {
			     err_warn(1,fperr,"Error - No memory for segments\n");
			     return 0;
			   }
			   tptr->num = *pi;
			   tptr->next = NULL;
			   if (hptr == NULL)
			     hptr = tptr;
			   else {
			     aptr = bptr = hptr;
			     while (aptr != NULL) {
			       if ((*pi < aptr->num) && (aptr == hptr)) {
				 tptr->next = hptr;
				 hptr = tptr;
				 break;
			       } else if ((*pi<aptr->num) && (aptr!=hptr)) {
				 bptr->next = tptr;
				 tptr->next = aptr;
				 break;
			       }
			       bptr = aptr;
			       aptr = aptr->next;
			     }
			     if (aptr == NULL) 
			       bptr->next = tptr;
			   }
			   
			 }
		       }
		     }
                   }
                   aptr = hptr;
                   while (aptr != NULL) {
		     gasgwk(Wkst[0].id, aptr->num);
		     aptr = aptr->next;
                   }
                   aptr = hptr;
                   while (aptr != NULL) {
		     bptr = aptr;
		     aptr = aptr->next;
		     free((char *) bptr);
                   }
		   
                   tptr=NULL; hptr=NULL; aptr=NULL; bptr=NULL;
	           display_name=return_display_name(*connect_id);
		 }
               }

                /* Copy the segments to the workstation *
                gksseg.number = 0;
                gksseg.integers = NULL;
                gqsgus(&gksseg);
                for (j=0,pid=gksseg.integers; j<gksseg.number; pid++,j++)
                   gasgwk(Wkst[0].id,*pid);
                if (gksseg.number > 0 && gksseg.integers != NULL)
                    free((char *)gksseg.integers);
		 */
		 
               /* update the VCS Canvas display */
/*                 update_ind = 1; */
/*                 vcs_legacy_canvas_update(0); */
#ifdef X11WM
		 XSync(connect_id->display,FALSE);
		 XFlush(connect_id->display);
#elif defined (QTWM)
/* 		 vcs_legacy_acquire_update(); */
/* 		 cairo_set_source_rgb(connect_id->cr,1.,1.,1.); */
/* 		 cairo_paint(connect_id->cr); */
/* 		 vcs_legacy_release_update(); */
/* 		 vcs_legacy_Qt_repaint_window_by_id(connect_id->wkst_id); */
#else
		 printf("insert your WM sync/flush here\n");
#endif
		
	        /* Create the new VCS Canvas Pixmap and save image to Pixmap */
                /* This is the backing store *
                if (*canvas_pixmap != (Pixmap)NULL)
	           XFreePixmap(connect_id->display, *canvas_pixmap);
                *canvas_pixmap = (Pixmap) NULL;
		*canvas_pixmap = create_pixmap();*/
#ifdef USEX11
                if (connect_id->canvas_pixmap != (Pixmap) NULL) {
                    XFreePixmap(connect_id->display, connect_id->canvas_pixmap);
                    connect_id->canvas_pixmap = (Pixmap) NULL;
                }
	  	connect_id->canvas_pixmap = copy_pixmap(*connect_id);
		XSync(connect_id->display,FALSE);
                XFlush(connect_id->display);
#elif defined (QTWM)
		/* no flushing need here */
#else
		fprintf(stderr,"ok in procPAge we may need some code here\n");
/* 		  cairo_surface_flush(connect_id->surface); */
#endif
	       }
		}
          if (wsid.number > 0 && wsid.integers != NULL)
	      free((char *)wsid.integers);
/*	 }*/
       wsid.number = 0;
       wsid.integers = NULL;
       gqopwk(&wsid);
       for (i=0,pid=wsid.integers;i<wsid.number;pid++,i++)
	 {
/*			Shutdown cgm if it's open.		*/
	  if (*pid == Wkst[1].id)
	    {
	      shutdown(connect_id, Wkst[1].id);
	    }
	 }
       if (wsid.number > 0 && wsid.integers != NULL) {
           free((char *)wsid.integers);
       }

       if (!Inactive && fpout != NULL) {
		prtPage(fpout);
       }

       return 1;
      }



/* 
 * Change the VCS Canvas size and position to portrait or lanscape.
 * If the orientation command is called then set the orientation 
 * flag to prevent the resizing callback.
 */
void set_up_canvas(connect_id,type, WIDTH, HEIGHT, XPOS, YPOS)
Gconid_X_drawable connect_id;
int WIDTH, HEIGHT, XPOS, YPOS;
char *type;
{

#ifdef X11WM
        XWindowAttributes xwa;
#endif
       	int 		canvas_height, canvas_width;
        int		x_pos, y_pos;
	int             width,height;
	extern int	orientation_flg;
	Boolean         landscape_flg = TRUE;

	/* Set the landscape flag used in the change_orientation routine. */
        if (strcmp(type, "landscape") != 0)
           landscape_flg = FALSE;

	/* This flag will stops the resize routine from redrawing the plot. */
	orientation_flg = 1;

	/* Get the VCS Canvas height and width */
#ifdef X11WM
        XGetWindowAttributes(connect_id.display, connect_id.drawable, &xwa);
        canvas_width  = xwa.width;
        canvas_height = xwa.height;
#elif defined (QTWM)
	vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&x_pos,&y_pos,&canvas_width,&canvas_height);
#else
	printf("insert your WM getgeom func here\n");
#endif

#ifdef X11WM
	/* Get the screen height and width */
        height=DisplayHeight(connect_id.display,
                             DefaultScreen(connect_id.display));
        width=DisplayWidth(connect_id.display,
                             DefaultScreen(connect_id.display));
#elif defined (QTWM)
	vcs_legacy_Qt_get_desktop_dimensions(connect_id.wkst_id,&x_pos,&y_pos,&width,&height);
#else
	printf("insert your WM get screen dims func here\n");
#endif
	/* Compute the VCS canvas portrait or landscape height and width */
        if (landscape_flg)
          {
           /* For dual windows this will not work, since display width is now the
           length of both screens. The display height is fixed, so the height
           is used to calculate the x (width) and y (height) of the canvas.
           canvas_width=0.60*width;
           canvas_height=0.76*canvas_width;*/
           canvas_height=0.568359375 * height;
           canvas_width=1.319587628866 * canvas_height;
          }
        else
          {
           /* For dual windows this will not work, since display width is now the
           length of both screens. The display height is fixed, so the height
           is used to calculate the x (width) and y (height) of the canvas.
           canvas_width=0.48*width;
           canvas_height=canvas_width/0.76;*/
           canvas_height=0.7880859375 * height;
           canvas_width=0.76084262701363 * canvas_height;
          }

        if ( (WIDTH != -99) && (HEIGHT != -99) ) {
           canvas_width = WIDTH;
           canvas_height = HEIGHT;
        }

        x_pos = 8;
        y_pos = ( height-canvas_height-30 );

        if ( (XPOS != -99) && (YPOS != -99) ) {
           x_pos = XPOS;
           y_pos = YPOS;
        }

	/* Resize the VCS Canvas to  the portrait or landscape settings */
#ifdef X11WM
        XMoveResizeWindow(connect_id.display, connect_id.drawable,
                          x_pos, y_pos, canvas_width, canvas_height);
        XFlush( connect_id.display );
        XSync( connect_id.display, TRUE );
#elif defined QTWM
	extern void vcs_legacy_Qt_resize_window(int index,int x,int y,int w, int h);
	vcs_legacy_Qt_resize_window(connect_id.wkst_id,x_pos,y_pos,canvas_width,canvas_height);
	/* vcs_legacy_Qt_repaint_window_by_id(connect_id.wkst_id); */
#else
	printf("insert your WM move func here\n");
	printf("insert your WM sync/flush here\n");
#endif
}

/*			Print the page orientation command.		*/

    int prtPage(FILE *fp)
      {
	fprintf (fp,"Page(%s)\n",Page.page_orient);
	return 1;
      }
