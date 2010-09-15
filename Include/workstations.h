#ifndef WORKSTATIONS_H
#define WORKSTATIONS_H 1

#ifdef USEX11
/*
 ************************************************************************
 *                      X-Library include files.                        *
 ************************************************************************/
#include <X11/Intrinsic.h>
#endif
#include <cairo/cairo.h>

/*		A structure for Workstation descriptions is defined.	*/
/*		Values are initialized in main.				*/

  struct workstations
    {
     int id;			/* The value to use when referring to
				   a workstation.			*/
     char type[12];		/* The string defining the wkstn type.	*/
    };

/*		The page orientation is specified here.			*/

  struct orientation
    {
     int sw;			/* Switch determines whether cgm
				   needs a page orientation change.	*/
     char page_orient[12];	/* Defines the orientation, either
				   "landscape" or "portrait".		*/
    };
/*		Define the connection identification structure.		*/
  typedef struct
    {
#ifdef USEX11
     Display  *display;		 /* display */
     XID      drawable;		 /* X window drawable */
     int   canvas_popup;	 /* VCS Canvas popup handle */
     int   canvas_drawable;	 /* VCS Canvas drawable handle */
     int app_context;   /* gotta have it, application context */
     int app_shell;           /* first widget */
     int cf_io_text;          /* text input window */
     Colormap n_cmap;		 /* virtual normal colormap */
     Visual *visual;             /* hopefully we get one we want, depth 8, 16, 24, 32 */
     Pixmap canvas_pixmap;       /* used as the backing store*/
     Pixmap draw_pixmap;       /* used as the backing store*/
#endif
     int      animate_popup; 	 /* animation popup handle */
      cairo_surface_t *surface;  /* cairo surface */
      cairo_t *cr;               /* cairo  drawable */
      int wkst_id;
    } Gconid_X_drawable;

/* 		Keep track of the segment order for later redrawing.	*/
  struct segment_order {
        int    num;
        struct segment_order *next;
  };
  typedef struct segment_order    SEG_ORDER;
  typedef SEG_ORDER               *SEG_ORDER_LINK;

#endif
