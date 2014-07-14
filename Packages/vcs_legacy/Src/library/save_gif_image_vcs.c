#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif
#include "rasterfile.h"
#include "color.h"
#include "workstations.h"

/*#include "xv.h"*/
#include "gd.h"

extern FILE *fperr;
extern Gconid_X_drawable connect_id;

/*		Active colormap name.					*/

extern char active_colors[];
extern struct c_val std_color[16];
extern struct color_table C_tab;

#ifdef USEX11
int write_gd_gif(file, ximage, r, g, b, map_length)
FILE    *file;
XImage  *ximage;
u_char  *r, *g, *b;
int     map_length;
{
	gdImagePtr	im_out;
	int i,j;
	unsigned char ch;

	im_out = gdImageCreate(ximage->width,ximage->height);
	im_out->colorsTotal = map_length;
	for (i=0; i<ximage->height; i++) {
	   for (j=0; j<ximage->width; j++) {
              /*ch=((unsigned char *)ximage->data)[i*ximage->bytes_per_line + j];
	      im_out->pixels[i][j] = ch;*/
	      im_out->pixels[i][j] = ((unsigned char *)ximage->data)[i*ximage->bytes_per_line + j];
	   }
	}
	for (i = 0; i < map_length; i++) {
	   im_out->red[i] = r[i];
	   im_out->green[i] = g[i];
	   im_out->blue[i] = b[i];
	   im_out->open[i] = 0;
	   im_out->brushColorMap[i] = 0;
	   im_out->tileColorMap[i] = 0;
	}
	im_out->transparent = -1;
	im_out->polyInts = NULL;
	im_out->brush = NULL;
	im_out->tile = NULL;
	im_out->polyAllocated = 0;
	im_out->styleLength = 0;
	im_out->stylePos = 0;
	im_out->style = NULL;
	im_out->interlace = 0;

	gdImageGif(im_out, file);

	return 0;
}
#endif

int save_gif_image_vcs_legacy (FILE *file)
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
	fprintf(stderr,"we need a window raise here in save_gif\n");
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

	/* If display has 8 bit color */

        if (ximage->depth == 8) {
           /* Convert X colors to u_char arrays */
           convert_gks_color(r, g, b);

           /* Save the image and its colormap in a Sun Raster file */
           /*ras_write_ximage(file,  ximage, r, g, b, MAX_COLORS);*/
	   write_gd_gif(file, ximage, r,g,b, MAX_COLORS);
        } else {
           /* Save the image with no colormap in a Sun Raster file */
           ras_write_ximage(file, ximage, NULL, NULL, NULL, 0);
	}
	/* Print out image information
        print_image_info(x_loc, y_loc, ximage, MAX_COLORS);
	*/

	/* Remove the image from memory */
        XDestroyImage(ximage);
#else
	fprintf(stderr,"ok huge chunk of code missing in save_gif\n");
#endif
	return 1;
}
