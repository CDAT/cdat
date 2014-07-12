#include "gks.h"
#include "gksshort.h"
#ifdef X11WM
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xlibint.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "color.h"
#include "display.h"
#include "workstations.h"

    extern FILE *fpin,*fpout,*fperr;
    extern FILE *fpcgm;

    extern char cgm_file[1024];

    extern int Inactive;
    extern int user_defer_update;

    extern struct color_table C_tab;
    extern struct workstations Wkst[];
    extern struct orientation Page;
    extern struct c_val std_color[];
    extern char active_colors[17];
/*    extern Gconid_X_drawable connect_id;*/
    /*extern XtAppContext app_context;             * gotta have it */
#ifdef X11WM
    extern Display *display;                     /* display reference */
    extern Visual *visual;                       /* the display's visual */
    extern Colormap n_cmap;
#endif
    void normal_cmap_emulate_default();

/*      Clear the VCS Canvas.                                           */
int clearCanvas(Gconid_X_drawable connect_id)
       {
#ifdef X11WM
          /* Display window and clear */
	  if ((connect_id.display != NULL) &&
	     (connect_id.drawable != 0))
	    XClearWindow(connect_id.display,connect_id.drawable);
#elif defined QTWM
	  vcs_legacy_Qt_clear_window_by_id(connect_id.wkst_id);
#else
	  fprintf(stderr,"insert your WM clear function here\n");
#endif

	  return 0;
       }

/*	Process a CANVAS command.					*/

int procCanvas(char *str, Gconid_X_drawable **connect_id_in, int canvas_id, double ratio, char *gui, int *tok)
      {
       int i, found=0;
       Gintlist wsid;
       Gconid_X_drawable *connect_id;
       int *pid;
	struct workstations *w;
	int tokm;
	int c=1;
	char strm[256];
#ifdef QTWM
	extern  void vcs_legacy_Qt_open_window(Gconid_X_drawable *connect_id);
#endif
      connect_id = (Gconid_X_drawable *)connect_id_in;
      if (*tok != -99) {
	if (*tok != '(')
	  {
	   err_warn(1,fperr,
		"Error - (CANVAS) not a proper token (%c).\n",*tok);
	   return 0;
	  }
	c=getsttk(strm,&tokm);
	if (tokm != ')')
	  {
	   err_warn(1,fperr,
		"Error - not a proper token (%s%c%s%c).\n",str,*tok,strm,tokm);
	   return 0;
	  }
      }

	/* Copy the command from str to strm */
	strcpy(strm, str);

	/* DNW and CD not checking against w->id anylonger it was set incorrectly when opening multile ws */
	for (w=&Wkst[0]; cmpncs(w->type,"X_WIN") != 0;w++);
	//	for (w=&Wkst[0];w->id != 0 && cmpncs(w->type,"X_WIN") != 0;w++);
	if (w->id == 0)
	  {
	   err_warn(0,fperr,"Warning - X_WIN not available.\n");
	   return 1;
	  }

	if (c > 0 && cmpncs(strm,"close") == 0)
	  {
	    shutdown(connect_id, w->id);
	   return 1;
	  }


/*		Check whether it's open already.			*/

        wsid.number = 0;
        wsid.integers = NULL;
        gqopwk(&wsid);
        for (i=0,pid=wsid.integers;i<wsid.number;pid++,i++)
	  /* DNW and CD use to check against w->id but it is wrong when multiple ws open */
           if (*pid == connect_id->wkst_id) {
	      found = 1;
	      break;
	   }

#ifdef PYTHON
#ifdef X11WM
/* 			CDAT can open as many window as it needs. 	*/
	if (connect_id->display == NULL)
	       connect_id->display=XOpenDisplay(NULL);
#elif defined(QTWM)
/* 	fprintf(stderr,"proc open first, id: %i, %i\n",connect_id,connect_id->wkst_id); */
	vcs_legacy_Qt_open_window_by_id(connect_id->wkst_id);
#else
	fprintf(stderr,"insert your WM open win function here\n");
#endif
	if (!found) {
	  config_canvas(w->id,connect_id,canvas_id,Page.page_orient,gui,ratio);}

#else
/*			If it isn't open, open it.			*/
	if (i == wsid.number)
	  {
	   if (connect_id->display == NULL)
	       connect_id->display=XOpenDisplay(NULL);
           fprintf(stderr,"YAHOOO! %i, %f\n",connect_id,ratio);
	   config_canvas(w->id, connect_id, canvas_id,Page.page_orient,gui,ratio);
	  }
#endif

	if (wsid.number > 0 && wsid.integers != NULL)
	    free((char *)wsid.integers);
	return 1;
      }

/*			Configure the canvas.				*/

    int config_canvas(id,connect_id_in,canvas_id,orient,gui,ratio)
      int id;
      Gconid_X_drawable **connect_id_in;
      int canvas_id;
      char *orient;
      char *gui;
      double ratio;
      {
       int i;
       int x,y;
       int width,height;
#ifdef PYTHON
       Gconid_X_drawable *connect_id;
#ifdef X11WM
       Atom       proto=0, delwin=0;
#endif
       char       buf[1024];
#endif

       char *base_dir, dirbase[1024], icon_file[1024];
       char *formid="-w %d -dp %ld -cmp %d";
       float dx,dy,dmax;
       char conec[256];
	
       Gwsti wst_updst;

       Glimit pw;
       Gintlist pseg;
       int *pi, bg=0, fg=1;

       unsigned long int valuemask=0;
       unsigned long int event_mask=0;
#ifdef X11WM
       XSetWindowAttributes ws_attr;
       XSizeHints hints;
       XWMHints wmhints;
       XVisualInfo vTemp;
       XVisualInfo *visualList;
       Pixmap iconpixmap;
#endif
       int xhr,yhr,itmp;
       unsigned int wr,hr;
       int screen_num;

       int visualsMatched;
       int parent_win;

       extern void vcs_legacy_canvas_module();  /* setup the VCS Canvas */
       char geom[100];
#ifdef QTWM
       extern  void vcs_legacy_Qt_open_window(Gconid_X_drawable *connect_id);
       extern void vcs_legacy_Qt_get_desktop_dimensions(int index,int *x, int *y, int *w,int *h);
       extern void vcs_legacy_Qt_set_window_properties(int index, int minWidth, int minHeight, int maxWidth,int maxWeight, char *icon, char *title);
       extern void vcs_legacy_Qt_resize_window(int index,int x,int y,int w, int h);
#endif
      connect_id = (Gconid_X_drawable *)connect_id_in;
#ifdef X11WM
      if (visual->class != PseudoColor) {
    	   bg = WhitePixel(connect_id->display, DefaultScreen(connect_id->display));
           fg = 0;
       }
       /* Set the window-event mask. */
       event_mask = StructureNotifyMask |
            ExposureMask |
            EnterWindowMask |
            LeaveWindowMask |
            FocusChangeMask |
            KeyPressMask |
            ButtonPressMask | ButtonReleaseMask | ButtonMotionMask |
            ResizeRedirectMask;

       /* Set the window-attributes structure  */
       /* Turning backing_store off, not supported on all platforms */
       ws_attr.backing_store=NotUseful;
       ws_attr.background_pixel=bg;
       /*ws_attr.background_pixmap=bg;*/
       ws_attr.border_pixel = 1;
       ws_attr.win_gravity = UnmapGravity;
       ws_attr.event_mask = ExposureMask | ResizeRedirectMask | StructureNotifyMask | EnterWindowMask | LeaveWindowMask | FocusChangeMask;
       ws_attr.do_not_propagate_mask = 0;
       ws_attr.do_not_propagate_mask = event_mask &
            (KeyPressMask | KeyReleaseMask | ButtonPressMask |
             ButtonReleaseMask | PointerMotionMask | Button1MotionMask |
             Button2MotionMask | Button3MotionMask | Button4MotionMask |
             Button5MotionMask | ButtonMotionMask);

/*       conct=(unsigned char *)&conec[0];
*/
       screen_num=DefaultScreen(connect_id->display);
       vTemp.screen=screen_num;
       vTemp.depth=DefaultDepth(connect_id->display, screen_num);
       vTemp.colormap_size=visual->map_entries;
       vTemp.class=visual->class;

       /* Get the visual information */
       visualList=XGetVisualInfo(connect_id->display,
	(XID) VisualScreenMask|VisualDepthMask|VisualClassMask|VisualColormapSizeMask,
			&vTemp,&visualsMatched);
       if (visualsMatched == 0)
	 {
	  err_warn(1,fperr,"No Matching Visuals.\n");
	  return 0;
	 }

       /* Create the window's colormap */
       if (connect_id->app_context == 0) {
/* 	 for (x=0;x<visualsMatched;x++) { */
/* 	   printf("ok visual %i out of %i is: %p vs %p\n",x,visualsMatched,visualList[x].visual,visual); */
/* 	   if (visualsMatched,visualList[x].visual == visual ) { */
/* 	     fprintf(stderr,"YEAAAHHHHHHHHHHHHHHH : %i\n",x); */
/* 	   } */
/* 	 } */
           if (visual->class != PseudoColor) {
              connect_id->n_cmap = n_cmap=XCreateColormap(connect_id->display,
	         RootWindow(connect_id->display,screen_num),
		   visualList[0].visual,AllocNone);
	      connect_id->visual=visualList[0].visual;
	      visual = visualList[0].visual;
           }
           ws_attr.colormap=connect_id->n_cmap;
           valuemask=CWDontPropagate|CWBackPixel|CWBorderPixel|CWEventMask|CWBackingStore|CWColormap;
       } else {
           connect_id->display = display; /* store display for VCS Canvas */
           connect_id->app_context = 0;/* store application context */
           connect_id->app_shell = 0; /* store first widget */
           connect_id->n_cmap = n_cmap; /* store the color map */
           connect_id->visual = visual; /* store the visual */

       }
       height=DisplayHeight(connect_id->display,screen_num);
       width=DisplayWidth(connect_id->display,screen_num);	
#elif defined (QTWM)
      vcs_legacy_Qt_get_desktop_dimensions(connect_id->wkst_id,&x,&y,&width,&height);
/*       printf("back in C dims are: %ix%i, at (%i,%i)\n",width,height,x,y); */
      fg = 0;
      bg=1; /* ??? Need to put code to figure out bg color here */
#else
      fprintf(stderr,"insert your WM get screen dims and fg/bg color setting func here\n");
#endif

       if (cmpncs(orient,"landscape") == 0)
	 {
	/* For dual windows this will not work, since display width is now the
           length of both screens. The display height is fixed, so the height
           is used to calculate the x (width) and y (height) of the canvas.
          x=0.60*width;
	  y=0.76*x;*/
          y=0.568359375 * height;
          x=1.319587628866 * y;
	  x= ratio * y;
	 }
       else if (cmpncs(orient,"portrait") == 0)
	 {
	  /* For dual windows this will not work, since display width is now the
           length of both screens. The display height is fixed, so the height
           is used to calculate the x (width) and y (height) of the canvas.
          x=0.48*width;
	  y=x/0.76;*/
          y=0.7880859375 * height;
          x=0.76084262701363 * y;
	  x= y / ratio ;
	 }
#ifdef X11WM
       /* Create the window */
       if (connect_id->app_context == 0) {
          parent_win = RootWindow(connect_id->display,screen_num);
          if (connect_id->drawable != 0) parent_win = connect_id->drawable;
          connect_id->drawable=(Window) XCreateWindow(connect_id->display,
            parent_win,2,height-y-30,x,y,0,
            vTemp.depth,InputOutput,visualList[0].visual,valuemask,&ws_attr);
	  //fprintf(stderr,"window created with visual: %p\n",visualList[0].visual);
          /* Trap for the "Close" or "Quit" from the menu */
          proto = XInternAtom(connect_id->display,
                                        "WM_PROTOCOLS",False);
          delwin = XInternAtom(connect_id->display, "WM_DELETE_WINDOW", FALSE);
          XChangeProperty(connect_id->display, connect_id->drawable,
                  proto, XA_ATOM, 32, PropModeReplace, (unsigned char *) &delwin, 1);
       } else {
	  /* Bring up the vcs_legacy canvas via a popup window */
	  sprintf(geom, "%dx%d+%d+%d", x, y, 2,height-y-30);
	  /*vcs_legacy_canvas_module(connect_id->app_shell, canvas_id, geom);*/
       }

       XSync( connect_id->display, FALSE );
       XFlush(connect_id->display);
#elif defined(QTWM)
       vcs_legacy_Qt_resize_window(connect_id->wkst_id,2,height-y-30,x,y);
       vcs_legacy_Qt_open_window_by_id(connect_id->wkst_id);
#else
       fprintf(stderr,"insert your WM window open/resize here\n");
#endif


          /* Set properties for window manager (always before mapping!) */
	  sprintf(buf, "%d. - Visualization and Control System (VCS)",
                  canvas_id);

	  /*
	   * Find a base directory.
	   */
	  if ((base_dir=getenv(DOT_DIRECTORY_ENV)) == NULL) {
              /* HVO getenv is not thread-safe and somehow this getenv does
                 not return a valid pointer */
              /* 	    if ((base_dir=getenv("HOME")) == NULL || strlen(base_dir) ==0) */
              if (1) {
                  strcpy(dirbase,"./");
                  strcat(dirbase,DOT_DIRECTORY);
              }
              else {
                  strcpy(dirbase,base_dir);
                  strcat(dirbase,"/");
                  strcat(dirbase,DOT_DIRECTORY);
              }
          } else 
              strcpy(dirbase,base_dir);
          base_dir=dirbase;
          strcpy(icon_file, base_dir);
	  strcat(icon_file, "/vcs_legacy_icon.xbm");

#ifdef X11WM
       /* Initialize size hints for window manager */
       if (connect_id->app_context == 0) {
/*          hints.flags=USPosition|USSize|PMinSize|PMaxSize|PResizeInc|PAspect;
            hints.flags=USPosition|USSize|PResizeInc|PAspect;
            hints.flags=USSize|PResizeInc; */
          hints.flags=USPosition|USSize|PMinSize|PMaxSize|PResizeInc;
          hints.flags=USPosition|USSize|PResizeInc;
          hints.y=height-y-30;
          hints.x=10;
          hints.width=x;
          hints.height=y;
          hints.min_width=x;
          hints.min_height=y;
          hints.max_width=x;
          hints.max_height=y;
          hints.width_inc=0;
          hints.height_inc=0;
/*          hints.min_aspect.x=x;
            hints.min_aspect.y=y;
            hints.max_aspect.x=x;
            hints.max_aspect.y=y;*/

          XSetStandardProperties(connect_id->display,connect_id->drawable,
	                         buf, "vcs_legacy", None,0,0,&hints);

	  wmhints.flags=IconPixmapHint;
	  itmp=XReadBitmapFile(connect_id->display,connect_id->drawable,icon_file,&wr,&hr,&iconpixmap,&xhr,&yhr);
	  wmhints.icon_pixmap=iconpixmap;
	  XSetWMHints(connect_id->display,connect_id->drawable,&wmhints);
          /* Select event types wanted */
          XSelectInput(connect_id->display, connect_id->drawable, ExposureMask );
       }

       /* Display window and clear */
       XMapWindow(connect_id->display,connect_id->drawable);
       XClearWindow(connect_id->display,connect_id->drawable);

       XFlush(connect_id->display);
       //fprintf(stderr,"ok cmap maybe: %p\n",connect_id->n_cmap);
       sprintf (conec,formid,connect_id->drawable,connect_id->display,connect_id->n_cmap);
#elif defined (QTWM)
       sprintf(conec,"%i",connect_id->wkst_id);
       vcs_legacy_Qt_set_window_properties(connect_id->wkst_id,-1,-1,width,height,icon_file,buf);
#else
       fprintf(stderr,"insert here your WM properties manager here\n");
#endif


       /* Open the XGKS workstation and associate it to the created window */
       gopwk(id,conec,"CANVAS");
       gsds(id,GASTI,GSUPPRESSED);

       gqwkt(id,&wst_updst);
       dx=wst_updst.current.v.xmax-wst_updst.current.v.xmin;
       dy=wst_updst.current.v.ymax-wst_updst.current.v.ymin;

       dx=x;
       dy=y;
       dmax=(dx>dy)?dx:dy;
       pw.xmin=0.0;
       pw.xmax=dx/dmax;
       pw.ymin=0.0;
       pw.ymax=dy/dmax;
       gswkwn(id,&pw);
       gacwk(id);
       gsclip(GNOCLIP);

#ifdef USEX11
       /* Set the window's colormap */
       XSetWindowColormap(connect_id->display,connect_id->drawable,connect_id->n_cmap);
       //fprintf(stderr,"colormap\n");

#elif defined (QTWM)
#else
       fprintf(stderr,"insert here your window colormap setting if necessary\n");
#endif
       //fprintf(stderr,"b4 cairodraw\n");

#ifdef CAIRODRAW
#ifdef X11WM
       if (connect_id->surface!=NULL) {
	 /* fprintf(stderr,"crap it is not NULL\n"); */
	 /* fprintf(stderr,"b4 creating X11 surface to cairo surface, status: %s\n",cairo_status_to_string(cairo_surface_status(connect_id->surface))); */
       }

       //fprintf(stderr,"in cairodraw: depth: %i\n",DefaultDepth(connect_id->display, DefaultScreen(connect_id->display)));

/*        if (connect_id->draw_pixmap != NULL) { */
/* 	 XFreePixmap(connect_id->display,connect_id->draw_pixmap); */
/* 	 connect_id->draw_pixmap = NULL; */
/*        } */
/*        connect_id->draw_pixmap = XCreatePixmap(connect_id->display,connect_id->drawable,x,y,DefaultDepth(connect_id->display, DefaultScreen(connect_id->display))); */
/*        fprintf(stderr,"creating X11 with visual: %p %p %p\n",visualList[0].visual,visual, connect_id->visual); */
/*         connect_id->surface = cairo_xlib_surface_create(connect_id->display, */
/* 							connect_id->draw_pixmap, */
/* 							connect_id->visual, */
/* 							x, */
/* 							y); */
        connect_id->surface = cairo_xlib_surface_create(connect_id->display,
							connect_id->drawable,
							connect_id->visual,
							x,
							y);
       	cairo_surface_mark_dirty(connect_id->surface);
       XFree(visualList);
/* 	fprintf(stderr,"created image surface to cairo surface, status: %s\n",cairo_status_to_string(cairo_surface_status(connect_id->surface))); */

/* 	visual_find(); */
/*         connect_id->surface = cairo_xlib_surface_create(connect_id->display, */
/* 								   connect_id->canvas_pixmap, */
/* 								   DefaultScreen(connect_id->display), */
/* 								   x, */
/* 								   y); */
       //fprintf(stderr,"created image surface to cairo surface, status: %s\n",cairo_status_to_string(cairo_surface_status(connect_id->surface)));

	//connect_id->surface = cairo_image_surface_create(CAIRO_FORMAT_RGB24,x,y);
/* 	fprintf(stderr,"widrth and height are: %ix%i, visual : %p, class: %p, %i.%i.%i\n",width,height,visual,visual->class,CAIRO_VERSION_MAJOR,CAIRO_VERSION_MINOR,CAIRO_VERSION_MICRO); */
/* 	fprintf(stderr,"created image surface to cairo surface, status: %s\n",cairo_status_to_string(cairo_surface_status(connect_id->surface))); */
	connect_id->cr = cairo_create(connect_id->surface);
	/* need to paint it white, actually colormap color 0 */
	VCS2CAIRO_setrgb(connect_id->cr,0);
	cairo_paint(connect_id->cr);
/* 	printf("tied X11 surface to cairo surface, status: %s\n",cairo_status_to_string(cairo_surface_status(connect_id->surface))); */
/* 	cairo_surface_mark_dirty(connect_id->surface); */
/* 	printf("marked surface dirty, status: %s\n",cairo_status_to_string(cairo_surface_status(connect_id->surface))); */
#elif defined (QTWM)
	/* connector done at init in X_initialize no need to do it here for us */
#else
	fprintf(stderr,"insert your WM connector here if not done already\n");
#endif
#endif

/*			Set patterns.					*/
       set_patterns();

/*			Set the colormap to active colormap.		*/
       set_active_colors();

#ifndef PYTHON
       pseg.number = 0;
       pseg.integers = NULL;
       gqsgus(&pseg);
       for (i=0,pi=pseg.integers;i<pseg.number;pi++,i++)
	 {
          gasgwk(id,*pi);
	 }
       if (pseg.number > 0 && pseg.integers != NULL)
           free((char *)pseg.integers);
#endif
       
       return 1;
      }

/*		Deactivate all workstations				*/
    void deactivate_all_other_wksts (act_ws)
      int act_ws;
      {
       int i;
       Gintlist wsid;
       Gwsstate wsstate;

       /* Deactivate all workstation except for workstation 7 (i.e., WISS) */
       wsid.number = 0;
       wsid.integers = NULL;
       gqacwk(&wsid);
       for (i=0; i < wsid.number; i++)
	 {
	  if (wsid.integers[i] != 7) {
             gqwks(wsid.integers[i], &wsstate);
             if (wsstate == GACTIVE)
	        gdacwk(wsid.integers[i]);
          }
	 }

       if (wsid.number > 0 && wsid.integers != NULL)
           free((char *)wsid.integers);

       /* Activate the one workstation */
       gacwk(act_ws);
       Wkst[0].id = act_ws;
      }

/*              Deactivate all workstations                             */
    Gint active_xgks_wks (void)
      {
       int i;
       Gint save_wks;
       Gintlist wsid;
       Gwsstate wsstate;

       /* Find which workstation is active except 
        * for workstation 7 (i.e., WISS)
        */
       wsid.number = 0;
       wsid.integers = NULL;
       gqacwk(&wsid);
       for (i=0; i < wsid.number; i++)
         {
          if (wsid.integers[i] != 7) {
             gqwks(wsid.integers[i], &wsstate);
             if (wsstate == GACTIVE)
                save_wks = wsid.integers[i];
          }
         }

       if (wsid.number > 0 && wsid.integers != NULL)
           free((char *)wsid.integers);

       /* Return activate workstation */
       return save_wks;
      }


/*		Shut down a workstation.				*/

int shutdown (Gconid_X_drawable connect_id, int wks)
      {
       int i;
       Gintlist wsid;
       int *pid;
       extern void vcs_legacy_canvas_quit_cb();

       wsid.number = 0;
       wsid.integers = NULL;
       gqacwk(&wsid);
       for (i=0,pid=wsid.integers;i < wsid.number;pid++,i++)
	 {
          gqacwk(&wsid);
          if (*pid == wks)
	    {
	     gdacwk(wks);
	     break;
	    }
	 }
       if (wsid.number > 0 && wsid.integers != NULL)
           free((char *)wsid.integers);

       wsid.number = 0;
       wsid.integers = NULL;
       gqopwk(&wsid);
       for (i=0,pid=wsid.integers;i < wsid.number;pid++,i++)
	 {
          if (*pid == wks)
	    {
	     if (wks != 7 && wks != 2) 
	       {
		gclrwk(wks,GALWAYS);
                if ((Inactive==1) && (user_defer_update==0))
		   guwk(wks,GPERFORM);
                else if ((Inactive==0) && (user_defer_update==0))
		   guwk(wks,GPERFORM);
	       }
	     gclwk(wks);
	     for (i=0;i < 3; i++)
	       {
		if (strcmp(Wkst[i].type,"X_WIN")==0 && wks==Wkst[i].id)
		  {
		    
#ifdef X11WM
		    if ((connect_id.display != NULL) && (connect_id.drawable != 0)) {
#ifdef CAIRODRAW
		      if (connect_id.cr!=NULL) {
			cairo_destroy(connect_id.cr);
			cairo_surface_destroy(connect_id.surface);
			connect_id.cr=NULL;
			connect_id.surface=NULL;
		      }
#endif
		      XDestroyWindow(connect_id.display,connect_id.drawable);
		    }
		    XFlush(connect_id.display);
fprintf(stderr,"shutiing down a wkst\n");
		    connect_id.drawable = (XID) NULL;
		    /*XFreeColormap(connect_id.display,connect_id.n_cmap);		*
		      XCloseDisplay(XtDisplay(connect_id.drawable));*/
		    /*vcs_legacy_canvas_quit_cb(NULL, NULL, NULL);*/
#elif defined (QTWM)
		    vcs_legacy_Qt_destroy_window(connect_id.wkst_id);
#else
		    fprintf(stderr,"insert your WM destroy win here\n");
#endif
		    break;
		  }
	       }
	     if (wks == 2 && fpcgm != NULL)
			 {fclose (fpcgm); fpcgm=NULL; cgm_file[0]='\0';}
	     
	     break;
	    }
	 }
       if (wsid.number > 0 && wsid.integers != NULL)
           free((char *)wsid.integers);
       return 1;
      }

/*
 * normal_cmap_emulate_default()
 *      Copy default colormap entries over to new
 *      custom normal colormap.
 *
 *      This does not gaurantee a fix for colormap flashing but may help to
 *      reduce it.
 *
 */
void normal_cmap_emulate_default(connect_id,Num_colors)
Gconid_X_drawable * connect_id;
int	Num_colors;
{
#ifdef X11WM
        Status stat;            /* return value */
        Colormap n_cmap_def;    /* normal default colormap */
        unsigned long pmask[1]; /* array for plane masks */
        unsigned long *index;   /* malloc'ed array for color indices */
        XColor *color;          /* malloc'ed array for color replication */
        int ncolors;            /* number of colors to copy */
        int i;                  /* index */

        /* get default colormap and determine how many colors to copy */
        n_cmap_def = DefaultColormap(connect_id->display, 
                                     DefaultScreen(connect_id->display));
        ncolors = DisplayCells(connect_id->display, 
				     DefaultScreen(connect_id->display));
        if (ncolors > Num_colors) {
           ncolors = Num_colors; /*limit how many default colors we copy*/
        }

        /*
         * allocate a bunch of read/write colors cells.  since this colormap
         * was just created these colors should end up being the lower colors.
         */
        index = (unsigned long *) Xmalloc(ncolors*sizeof(index[0]));
        color = (XColor *) Xmalloc(ncolors*sizeof(XColor));
        stat = XAllocColorCells(connect_id->display,n_cmap,True,
                                pmask,0,index,ncolors);
        if (! stat) {
	   err_warn(1,fperr,
                "Failure: Default colormap allocation failed. \n");
           exit((int) 1);
        }

        /* map out the color entries we will query for */
        for (i=0; i<ncolors; i++)
        color[i].pixel = index[i];

        /* transfer colors using query/store */
        XQueryColors(connect_id->display, n_cmap_def, color, ncolors);
        XStoreColors(connect_id->display, n_cmap    , color, ncolors);

        /* cleanup */
        XFree((char *) index);
        XFree((char *) color);
#endif
}
