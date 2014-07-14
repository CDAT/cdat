#include "workstations.h"
#include "vcs_legacy_canvas.h"
#include "gks.h"
#include <stdio.h>
#include <stdlib.h>
#include "gksshort.h"
#ifdef X11OUT
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif

#include "color.h"

ANIMATIONMEMORYLIST_LINK        head_animation_memory=NULL;
ANIMATIONMEMORYLIST_LINK        tail_animation_memory=NULL;
extern FILE *fperr;
extern Gconid_X_drawable connect_id;

/*		Active colormap name.					*/

extern char active_colors[];
extern struct c_val std_color[16];
extern struct color_table C_tab;

int save_image_vcs_legacy (FILE *file)
{
	int 		x_loc,y_loc;
#ifdef USEX11
	Window    	canvas_root_win, child_win;
        XImage		*ximage;
	int		screen = DefaultScreen(connect_id.display);
	int		scrn_width=DisplayWidth(connect_id.display,screen);
        int		scrn_height=DisplayHeight(connect_id.display,screen);
#else
	int		screen;
	int		scrn_width;
        int		scrn_height;
#endif
	char		r[MAX_COLORS],g[MAX_COLORS],b[MAX_COLORS];
	unsigned int	x_width,y_height,border_width,depth;
	int		width, height;
	int		x1,y1;

	/* 
 	 * Get allocated memory for the XImage structure and copy the
 	 * canvas window to it.
	 */
	/* Get the application window's position */
#ifdef USEX11
	XRaiseWindow(connect_id.display,connect_id.drawable);
#else
	fprintf(stderr,"in save_image_vcs_legacy need a raise window\n");
#endif

#ifdef USEX11
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
#else
	fprintf(stderr,"in save_image_vcs_legacy need a get geom window\n");
#endif
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

#ifdef USEX11
	ximage = XGetImage(connect_id.display,
		RootWindowOfScreen(DefaultScreenOfDisplay(connect_id.display)),
                   x1, y1, width, height, XAllPlanes(), ZPixmap);
        if (ximage == NULL)
	  {
	   err_warn(1,fperr,"Error - getting raster image.\n");
	   return 0;
	  }
#else
	fprintf(stderr,"need a getimage in save_vcs_legacy\n");
#endif

	/* If display has 8 bit color */
#ifdef USEX11
        if (ximage->depth == 8) {
           /* Convert X colors to u_char arrays */
           convert_gks_color(r, g, b);
           /* Save the image and its colormap in a Sun Raster file */
           ras_write_ximage(file,  ximage, r, g, b, MAX_COLORS);
        } else {
           /* Save the image with no colormap in a Sun Raster file */
           ras_write_ximage(file, ximage, NULL, NULL, NULL, 0);
	}
#else
	fprintf(stderr,"need a raw_write here\n");
#endif
	/* Print out image information
        print_image_info(x_loc, y_loc, ximage, MAX_COLORS);
	*/

	/* Remove the image from memory */
#ifdef USEX11
        XDestroyImage(ximage);
#else
	fprintf(stderr,"need a ximg destroy here\n");
#endif
	return 1;
}

int store_image_in_memory_vcs_legacy(image_ct)
int	image_ct;
{
        ANIMATIONMEMORYLIST_LINK        iptr, tiptr;
#ifdef USEX11
        Window  	canvas_root_win, child_win;
        XImage          *ximage;
	int		screen = DefaultScreen(connect_id.display);
        int     	scrn_width=DisplayWidth(connect_id.display,screen);
        int     	scrn_height=DisplayHeight(connect_id.display,screen);
#else
	int		screen;
        int     	scrn_width;
        int     	scrn_height;
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
#ifdef USEX11
        XRaiseWindow(connect_id.display,connect_id.drawable);
#else
	fprintf(stderr,"need a raise win here\n");
#endif

#ifdef USEX11
        if (    XGetGeometry(
                connect_id.display,connect_id.drawable, &canvas_root_win,
                &x_loc, &y_loc, &x_width, &y_height, &border_width, &depth) == 0
                                ||
                XTranslateCoordinates(connect_id.display,connect_id.drawable,
                canvas_root_win,0,0,&x1,&y1,&child_win) == False)
          {
           err_warn(1,fperr,
                    "Error - getting canvas geometry for raster image.\n");
           return 0;
          }
#else
	fprintf(stderr,"need a getgeon in sv_img_vcs_legacy\n");
#endif
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

#ifdef USEX11
	/* Create the image of the canvas */
        ximage = XGetImage(connect_id.display,
                RootWindowOfScreen(DefaultScreenOfDisplay(connect_id.display)),
                   x1, y1, width, height, XAllPlanes(), ZPixmap);
        if (ximage == NULL)
          {
           err_warn(1,fperr,
                    "Error - No more memory for creating raster image.\n");
           return 0;
          }

        /* If display has 8 bit color then get the RGB values */
        if (ximage->depth == 8)
           convert_gks_color(r, g, b); /* Convert X colors to u_char arrays */
#else
	fprintf(stderr,"need a getimg in sv_img_vcs_legacy\n");
#endif
        /* Create the raster link list */
        if ((iptr=(ANIMATIONMEMORYLIST_LINK)
             malloc(sizeof(ANIMATIONMEMORYLIST))) == NULL) {
            err_warn(1,fperr,
                     "Error - Cannot Store any more images in memory!\n");
            return 0;
        }

#ifdef USEX11
	/* Place the image in the linked list structure */
	iptr->ximage = ximage;
#else
	fprintf(stderr,"need to sv ing\n");
#endif
        iptr->position = image_ct;
        iptr->ras_width = width;
        iptr->ras_height = height;
#ifdef USEX11
        if (ximage->depth == 8) { /* Save the raster's color map */
           iptr->map_length = MAX_COLORS;
           iptr->wm_offset = 0;
           for (i=0; i < MAX_COLORS; ++i) {
               iptr->red[i] = r[i];
               iptr->green[i] = g[i];
               iptr->blue[i] = b[i];
           }
	}
#else
	fprintf(stderr,"raster colormap saving stuff\n");
#endif
        iptr->prev = NULL;
        iptr->next = NULL;

	/* Link the newly created image structure */
        if (head_animation_memory == NULL)
           head_animation_memory = tail_animation_memory = iptr;
        else {
	   tiptr = tail_animation_memory;
           tiptr->next = iptr;
           iptr->prev = tiptr;
           tail_animation_memory = iptr;
        }

	return 1;
}
