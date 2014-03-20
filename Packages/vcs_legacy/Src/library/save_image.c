#include "gks.h"
#include "gksshort.h"
#include <stdio.h>
#include <stdlib.h>
#ifdef X11WM
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif

#include "workstations.h"
#include "vcs_legacy_canvas.h"
#include "color.h"

#ifdef INTERFACE
#include "base_frame.h"
extern base_frame_objects       *bf;
#endif

extern FILE *fperr;
extern struct workstations Wkst[];
extern Gconid_X_drawable connect_id;
extern struct orientation Page;


/*		Active colormap name.					*/

extern char active_colors[];
extern struct c_val std_color[16];
extern struct color_table C_tab;

int save_image (FILE *file)
{
	int 		x_loc,y_loc;
#ifdef X11WM
	Window    	canvas_root_win, child_win;
        XImage		*ximage;
#else
	void *ximage = NULL;
#endif
	char		r[MAX_COLORS],g[MAX_COLORS],b[MAX_COLORS];
	unsigned int	x_width,y_height,border_width,depth;
	int		width, height;
#ifdef X11WM
	int		screen = DefaultScreen(connect_id.display);
	int		scrn_width=DisplayWidth(connect_id.display,screen);
        int		scrn_height=DisplayHeight(connect_id.display,screen);
#else
	int		screen;
	int		scrn_width;
        int		scrn_height;
#endif
	int		x1,y1;

	/* 
 	 * Get allocated memory for the XImage structure and copy the
 	 * canvas window to it.
	 */
	/* Get the application window's position */

#ifdef X11WM
	XRaiseWindow(connect_id.display,connect_id.drawable);
	if (	XGetGeometry(
		connect_id.display,connect_id.drawable, &canvas_root_win,
	   	&x_loc, &y_loc, &x_width, &y_height, &border_width, &depth) == 0
				||
		XTranslateCoordinates(connect_id.display,connect_id.drawable,
		canvas_root_win,0,0,&x1,&y1,&child_win) == False)
	  {
	   err_warn(1,fperr,"Error - getting raster image.\n");
	   return 0;
	  }

	width = x_width;
	height = y_height;

	/* Find the new x location and the width, if necessary. */

	if ((-x1 > width) || (x1 > scrn_width)) {
	   err_warn(1,fperr,"Error - X for getting raster image.\n");
	   return 0;
	  }
	if (x1 < 0)
	  {
	   width=width+x1;
	   x1=0;
	  }
	else if (x1+width > scrn_width)
	  {
	   width=scrn_width-x1;
	  }

	if ((-y1 > height) || (y1 > scrn_height)) {
	   err_warn(1,fperr,"Error - Y for getting raster image.\n");
	   return 0;
	  }
	if (y1 < 0)
	  {
	   height=height+y1;
	   y1=0;
	  }
	else if (y1+height > scrn_height)
	  {
	   height=scrn_height-y1;
	  }

        /* This way is good because the root window will create partially obscure windows.
	 * But this method doesn't work for the Mac which has a rootless window.
         * ximage = XGetImage(cptr->connect_id.display,
                RootWindowOfScreen(DefaultScreenOfDisplay(cptr->connect_id.display)),
                   x1, y1, width, height, XAllPlanes(), ZPixmap);
         */
	ximage = XGetImage(connect_id.display,
		           connect_id.drawable,
                           0, 0, width, height, XAllPlanes(), ZPixmap);
#elif defined QTWM
	vcs_legacy_Qt_window_get_image_by_id(connect_id.wkst_id,&ximage);
#else
	fprintf(stderr,"insert here your WM to get image\n");
#endif
        if (ximage == NULL)
	  {
	   err_warn(1,fperr,"Error - getting raster image.\n");
	   return 0;
	  }
	/* If display has 8 bit color */
#ifdef X11WM
        if (ximage->depth == 8) {
           /* Convert X colors to u_char arrays */
           convert_gks_color(r, g, b);

           /* Save the image and its colormap in a Sun Raster file */
           ras_write_ximage(file,  ximage, r, g, b, MAX_COLORS);
        } else {
           /* Save the image with no colormap in a Sun Raster file */
           ras_write_ximage(file, ximage, NULL, NULL, NULL, 0);
	}
#elif defined QTWM
	ras_write_ximage(file, ximage, NULL, NULL, NULL, 0);
#else
	fprintf(stderr,"insert here your write to ras func\n");
#endif

	/* Print out image information
        print_image_info(x_loc, y_loc, ximage, MAX_COLORS);
	*/

	/* Remove the image from memory */
#ifdef X11WM
        XDestroyImage(ximage);
#elif defined QTWM
	free(ximage);
#else
	fprintf(stderr,"insert here your destroy ximage func\n");
#endif
	return 1;
}

int store_image_in_memory(cptr, image_ct)
CANVASINFO_LINK    cptr; /* connection ID info */
int	image_ct;
{
        ANIMATIONMEMORYLIST_LINK        iptr, tiptr;
#ifdef X11WM
        Window  	canvas_root_win, child_win;
        XImage          *ximage;
	int		screen = DefaultScreen(cptr->connect_id.display);
        int     	scrn_width=DisplayWidth(cptr->connect_id.display,screen);
        int     	scrn_height=DisplayHeight(cptr->connect_id.display,screen);
#else
	int		screen;
        int     	scrn_width;
        int     	scrn_height;
	void *ximage = NULL;
#endif
        int     	width, height;
        int     	i, x1, y1;
        int             x_loc, y_loc;
	unsigned int    x_width, y_height, border_width, depth;
        char    	r[MAX_COLORS],g[MAX_COLORS],b[MAX_COLORS];

	/*
         * Get snapshot (image) of canvas.
         * First, get allocated memory for the XImage 
         * structure and copy the canvas window to it.
         */

        /* Bring the canvas window to the front, then
         * get the application's canvas position.
         */
#ifdef X11WM
        XRaiseWindow(cptr->connect_id.display,cptr->connect_id.drawable);

        if (    XGetGeometry(
                cptr->connect_id.display,cptr->connect_id.drawable, &canvas_root_win,
                &x_loc, &y_loc, &x_width, &y_height, &border_width, &depth) == 0
                                ||
                XTranslateCoordinates(cptr->connect_id.display,cptr->connect_id.drawable,
                canvas_root_win,0,0,&x1,&y1,&child_win) == False)
          {
           err_warn(1,fperr,
                    "Error - getting canvas geometry for raster image.\n");
           return 0;
          }
        width = x_width;
        height = y_height;

        /* Find the new x location and the width, if necessary. */
        if ((-x1 > width) || (x1 > scrn_width)) {
           err_warn(1,fperr,"Error - getting X location for raster image.\n");
           return 0;
          }
        if (x1 < 0)
          {
           width=width+x1;
           x1=0;
          }
        else if (x1+width > scrn_width)
          {
           width=scrn_width-x1;
          }

        /* Find the new y location and the height, if necessary. */
        if ((-y1 > height) || (y1 > scrn_height)) {
           err_warn(1,fperr,"Error - getting Y location for raster image.\n");
           return 0;
          }
        if (y1 < 0)
          {
           height=height+y1;
           y1=0;
          }
        else if (y1+height > scrn_height)
          {
           height=scrn_height-y1;
          }

	/* Create the image of the canvas */
        ximage = XGetImage(cptr->connect_id.display,
                           cptr->connect_id.drawable,
                           0, 0, width, height, XAllPlanes(), ZPixmap);
        /* This way is good because the root window will create partially obscure windows.
	 * But this method doesn't work for the Mac which has a rootless window.
         * ximage = XGetImage(cptr->connect_id.display,
                RootWindowOfScreen(DefaultScreenOfDisplay(cptr->connect_id.display)),
                   x1, y1, width, height, XAllPlanes(), ZPixmap);
         */
        if (ximage == NULL)
          {
           err_warn(1,fperr,
                    "Error - No more memory for creating raster image.\n");
           return 0;
          }

        /* If display has 8 bit color then get the RGB values */
        if (ximage->depth == 8)
           convert_gks_color(r, g, b); /* Convert X colors to u_char arrays */
#elif defined QTWM
	vcs_legacy_Qt_window_get_image_by_id(connect_id.wkst_id,&ximage);
	vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&x_loc,&y_loc,&width,&height);
#else
	fprintf(stderr,"insert here your get image func\n");
#endif

        /* Create the raster link list */
        if ((iptr=(ANIMATIONMEMORYLIST_LINK)
             malloc(sizeof(ANIMATIONMEMORYLIST))) == NULL) {
            err_warn(1,fperr,
                     "Error - Cannot Store any more images in memory!\n");
            return 0;
        }

	/* Place the image in the linked list structure */
	iptr->ximage = ximage;
        iptr->position = image_ct;
        iptr->ras_width = width;
        iptr->ras_height = height;
#ifdef X11WM
        if (ximage->depth == 8) { /* Save the raster's color map */
           iptr->map_length = MAX_COLORS;
           iptr->wm_offset = 0;
           for (i=0; i < MAX_COLORS; ++i) {
               iptr->red[i] = r[i];
               iptr->green[i] = g[i];
               iptr->blue[i] = b[i];
           }
	}
#elif defined QTWM
	/* nothing to do */
#else
	fprintf(stderr,"insert here your func for color saving\n");
#endif
        iptr->prev = NULL;
        iptr->next = NULL;

	/* Link the newly created image structure */
        if (cptr->head_animation_memory == NULL)
           cptr->head_animation_memory = cptr->tail_animation_memory = iptr;
        else {
	   tiptr = cptr->tail_animation_memory;
           tiptr->next = iptr;
           iptr->prev = tiptr;
           cptr->tail_animation_memory = iptr;
        }
#ifdef X11WM
#ifdef INTERFACE
	XFlush(XtDisplay(bf->base_formshell));
#endif
#elif defined QTWM
	/*nothing to */
#else
	fprintf(stderr,"insert here your flush fun\n");
#endif
	return 1;
}
