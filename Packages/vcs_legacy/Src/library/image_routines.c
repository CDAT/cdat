/*
 ************************************************************************
 *                      PCMDI GRAPHICS PACKAGE                          *
 *                      Developed for LLNL-PCMDI use                    *
 *                                                                      *
 *                                                                      *
 *      Copyright (C) 1992. The Regents of the University of California.*
 *      All rights reserved.                                            *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *      Author: Dean N. Williams                                        *
 *                                                                      *
 *      Date: 01/3/93                                                   *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *      File name: image_routines.c                                     *
 *                                                                      *
 *                                                                      *
 *      Langague: C                                                     *
 *                                                                      *
 *                                                                      *
 *      Comments:                                                       *
 *		  Some of the files below where taken from the 		*
 *		  xim_ras_utils.c file, which was created at Sun	*
 *		  Microsystems, Inc.  Permission to use, copy,    	*
 *		  modify, and even distribute was given as long as 	*
 * 		  acknowledgment of Sun Microsystems appears.		*
 *                                                                      *
 *	          Below is the acknowledgment:				*
 *									*
 * Copyright (c) 1990-1991 Sun Microsystems, Inc & David Berry		*
 *                                              david.berry@sun.com	*
 * Permission to use, copy, modify, and distribute this software and	*
 * its documentation for any purpose and without fee is hereby granted,	*
 * provided that the above copyright notice appear in all copies	*
 * and that both that copyright notice and this permission notice appear*
 * in supporting documentation.						*
 *                                                                      *
 *                                                                      *
 *      Modifications:                                                  *
 *                                                                      *
 *                                                                      *
 *      Contact:                                                        *
 *                                                                      *
 *              Dean N. Williams    Robert L. Mobley                    *
 *                                                                      *
 *              LLNL                LLNL                                *
 *              PO Box 808, L-264   PO Box 808, L-264                   *
 *              Livermore, CA       Livermore, CA                       *
 *              94550               94550                               *
 *                                                                      *
 *              (510) 423-0145      (510) 422-7649                      *
 *                                                                      *
 *                                                                      *
 ************************************************************************
 */
/*
 ************************************************************************
 *                      Include Files                                   *
 ************************************************************************
 */
#include <stdio.h>
#include <stdlib.h>
#ifdef X11WM
#include <X11/Xatom.h>
#endif
#include "xgks.h" /* include gks routines */
#include "gksshort.h"
#include "color_editor.h"
#include "rasterfile.h"
#include "workstations.h"

/* Number of bytes in an XImage */
#define bytes_in_ximage(x)      ((x)->bytes_per_line*(x)->height)
#define get_ras_bytes(x)        ((x)/16 + ((x)%16 ? 1 : 0))*2;
#define check_depth(x)          ((x) == 24 ? 32 : (x))

/****************************************************************************
*** Global Variables ********************************************************
*****************************************************************************/
#ifdef X11WM
extern GC 			gc;                     /* graphics context */
extern Display 			*display;              /* display reference */
#endif
extern int 	    		screen;    /* screen index we're running on */
extern int			app_x, app_y, border_height, border_width;

/*
 * Captures an image from the drawable window (canvas)
 * and stores the image and its colormap in a raster
 * file. That is, it store the rasters in the 
 * Sun raster format.
 */
void store_raster_file(file, canvas_display, xwin, app_x_pos, app_y_pos)
FILE    	*file;
#ifdef X11WM
Display         *canvas_display;
Window          xwin;
#endif
int		app_x_pos;
int		app_y_pos;
{
#ifdef X11WM
        XImage              *ximage;
	extern Visual       *visual; /* hopefully we get one we want */
#else
	void *ximage;
#endif
        u_char              r[NUM_COLORS];
        u_char              g[NUM_COLORS];
        u_char              b[NUM_COLORS];
#ifdef X11WM
	Window		    canvas_root_win;
	int		    screen = DefaultScreen(canvas_display);
	int	            scrn_width=DisplayWidth(canvas_display,screen);
        int                 scrn_height=DisplayHeight(canvas_display,screen);
#else
	int		    screen;
	int	            scrn_width;
        int                 scrn_height;
#endif
	int		    x1, y1, x_loc=0, y_loc=0;
	int		    combined_width, combined_height;
	unsigned int	    width, height;
	unsigned int	    border_width;
	unsigned int	    depth;
	/*int 		    convert_xcolor_to_uchar();*/
	extern FILE 	    *fperr;
        extern void         print_image_info();
	extern int          Inactive;
	extern Gconid_X_drawable connect_id;/*VCS canvas display and drawable id */

	/* 
	 * Need canvas information. So,
	 * get the x and y location of the display canvas,
	 * get the width and height of the display canvas, and
	 * get the depth of the canvas window. Also, need the 
	 * geometry of the application window.
	 */
	x_loc = app_x_pos; y_loc = app_y_pos;
	if (!Inactive) {
#ifdef X11WM
	   XGetGeometry(connect_id.display, 
                     connect_id.drawable, &canvas_root_win,
	   	     &x1, &y1, &width, &height, &border_width, &depth);
#elif defined QTWM
	   vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&x1,&y1,&width,&height);
#else
	   fprintf(stderr,"insert here you WM getgeom\n");
#endif
	   x_loc += x1; y_loc += y1;
	}
#ifdef X11WM
	XGetGeometry(canvas_display, xwin, &canvas_root_win,
	   	    &x1, &y1, &width, &height, &border_width, &depth);
#elif defined QTWM
	   vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&x1,&y1,&width,&height);
#else
	   fprintf(stderr,"insert here your wm get dims\n");
#endif
	x_loc += x1;
	y_loc += y1;

	/* Find the new x location and the width, if necessary. */
	combined_width = width + x_loc;
	if (x_loc < 0) {
	   width = width + x_loc;
	   x_loc = 0;
	} else if (combined_width > scrn_width)
	   width = width - (combined_width - scrn_width) + x1;

	/* Find the new y location and the height, if necessary. */
	combined_height = height +  y_loc + y1;
	if (y_loc < 0) {
	   height = height + y_loc;
	   y_loc = 0;
	} else if (combined_height > scrn_height)
	   height = height - (combined_height-scrn_height) + y1;

	/* 
 	 * Get allocated memory for the XImage structure and copy the
 	 * canvas window to it.
	 */
#ifdef X11WM
	ximage = XGetImage(canvas_display, 
		   RootWindowOfScreen(DefaultScreenOfDisplay(canvas_display)),
                   x_loc, y_loc, width, height, XAllPlanes(), ZPixmap);
#elif defined QTWM
	vcs_legacy_Qt_window_get_image_by_id(connect_id.wkst_id,&ximage);
#else
	fprintf(stderr,"insert your getimg here\n");
#endif
	if (ximage == NULL) {
           err_warn(1,fperr,"Error - creating raster image.\n");
           return ;
        }
#ifdef X11WM
	/* If display is has 8 bit color */
        if (ximage->depth == 8) {
           /* Convert X colors to u_char arrays *
           convert_xcolor_to_uchar(set_xcolors, NUM_COLORS, r, g, b);
*/

           /* Save the image and its colormap in a Sun Raster file */
           ras_write_ximage(file,  ximage, r, g, b, NUM_COLORS);
        } else 
#endif
	  {
	    /* Save the image with no colormap in a Sun Raster file */
	    ras_write_ximage(file, ximage, NULL, NULL, NULL, 0);
	  }
	
	/* Print out image information
        print_image_info(x_loc, y_loc, ximage, NUM_COLORS);
	*/

	/* Remove the image from memory */
#ifdef X11WM
        XDestroyImage(ximage);
#elif defined QTWM
	free(ximage);
#endif
}

#ifdef USEX11
/*
 * Convert the 3 xcolor r,g,b arrays to 3 u_char r,g,b arrays
 */
int convert_xcolor_to_uchar(xcolor_val, length, r, g, b)
XColor  *xcolor_val;
int     length;
u_char  *r, *g, *b;
{
    register int i;

    if(!xcolor_val || !r || !g || !b)
        return(-1);

    for(i=0; i<length; i++) {
        r[i] = xcolor_val[i].red>>8;
        g[i] = xcolor_val[i].green>>8;
        b[i] = xcolor_val[i].blue>>8;
    }

    return(0);
}

/*
 * image copy
 */
int
xim_copy(src_ximage, dst_ximage)
XImage *src_ximage, *dst_ximage;
{
    register int        i, lbytes_in, lbytes_out;
    register u_char     *src_data;
    register u_char     *dst_data;
 
    /* Validate that we can do the copy */
    if( (!src_ximage || !dst_ximage) ||
       (src_ximage->depth != dst_ximage->depth) ||
       (src_ximage->width != dst_ximage->width) ||
       (src_ximage->height != dst_ximage->height) )
        return(-1);
 
    /* Place offset image in the destination image */
    src_data = (u_char *)src_ximage->data;
    dst_data = (u_char *)dst_ximage->data;
    lbytes_in = src_ximage->bytes_per_line;
    lbytes_out = dst_ximage->bytes_per_line;
 
    for(i=0; i<src_ximage->height; i++) {
	memcpy((void *)dst_data, (const void *)src_data, lbytes_in);
        src_data += lbytes_in;
        dst_data += lbytes_out;
    }
    return(0);
}
#endif
/*
 * Dump the specified ximage and colour map to a file
int
ras_write_ximage(file, ximage, r, g, b, map_length)
FILE    *file;
XImage  *ximage;
u_char  *r, *g, *b;
int     map_length;
{
    struct rasterfile   rh;
    register int        i, ras_bytes, bytes_to_copy;
    register u_char     *data_ptr, *out_data;
    int                 rc;
                        * Lint complains about this useful for
                         * debugging so I'll leave it in
                         *

    if(!ximage)
        return(-1);

    rh.ras_magic = RAS_MAGIC;
    rh.ras_width = ximage->width;
    rh.ras_height = ximage->height;
    if (ximage->depth == 24)
        rh.ras_depth = 32;
    else
        rh.ras_depth = ximage->depth;
    rh.ras_type = RT_STANDARD;
    if(map_length == 0) {
        rh.ras_maptype = RMT_NONE;
        rh.ras_maplength = 0;
    } else {
        rh.ras_maptype = RMT_EQUAL_RGB;
        rh.ras_maplength = 3*map_length;
    }

    *
     * Dump everything to the file
     * 1. Calculate the amount of image data to be dumped
     * 2. The rasterfile header
     * 3. the colour map if there is one
     * 4. the image data
     *
    ras_bytes = ximage->width*rh.ras_depth;
    ras_bytes = get_ras_bytes(ras_bytes);
    if ((out_data = (u_char *)malloc(ras_bytes)) == NULL)
        return(-1);
    memset(out_data, '\0', ras_bytes);

    rh.ras_length = ras_bytes * ximage->height;
    rc = fwrite(&rh, sizeof(struct rasterfile), 1, file);

    * Keep the value of rc around for debugging purposes *
    if(rh.ras_maptype == RMT_EQUAL_RGB) {
        rc = fwrite(r, sizeof(u_char), map_length, file);
        rc = fwrite(g, sizeof(u_char), map_length, file);
        rc = fwrite(b, sizeof(u_char), map_length, file);
    }

    * Write the image data line by line ensures a good raster file *
    data_ptr = (u_char *)ximage->data;
    if (rh.ras_depth == 1)
        bytes_to_copy = ximage->width/8 + (ximage->width%8 ? 1 : 0);
    else
        bytes_to_copy = ximage->width * rh.ras_depth/8;

    for (i=0; i<ximage->height; i++) {
	memcpy(out_data, data_ptr, bytes_to_copy);
        rc = fwrite(out_data, sizeof(u_char), ras_bytes, file);
        memset(out_data, '\0', ras_bytes);
        data_ptr += ximage->bytes_per_line;
    }
    free(out_data);
    return(0);
}
 */

/* Print information on the file */
/*void
print_image_info(x, y, ximage, map_length)
int     x, y;
XImage  *ximage;
int     map_length;
{
    fprintf(stderr, "Frame buffer contents at %d %d,", x, y);
    fprintf(stderr, " width = %d, height = %d\n",
            ximage->width, ximage->height);
    fprintf(stderr, "dumped to a Sun rasterfile.\n");
    fprintf(stderr, "Colormap length compressed to = %d.\n",map_length);
    return;
}
*/

/*
 * Reads a 4-byte int in Sun byteorder returns 0 for success, EOF for failure.
 */
static int read_sun_long (int *l, FILE *fp)
{
  int c0, c1, c2, c3;

  c0 = fgetc(fp);
  c1 = fgetc(fp);
  c2 = fgetc(fp);
  c3 = fgetc(fp);

  *l = (((u_long) c0 & 0xff) << 24) |
       (((u_long) c1 & 0xff) << 16) |
       (((u_long) c2 & 0xff) <<  8) |
       (((u_long) c3 & 0xff));

  if (ferror(fp) || feof(fp)) return EOF;

  return 0;
}

/* Get Sun rasterfile header from stream. */
int
ras_load_header(FILE *file, struct rasterfile *rh)
{
    if (rh == 0)
        return(-1);

    read_sun_long (&rh->ras_magic    , file);
    read_sun_long (&rh->ras_width    , file);
    read_sun_long (&rh->ras_height   , file);
    read_sun_long (&rh->ras_depth    , file);
    read_sun_long (&rh->ras_length   , file);
    read_sun_long (&rh->ras_type     , file);
    read_sun_long (&rh->ras_maptype  , file);
    read_sun_long (&rh->ras_maplength, file);

    if (rh->ras_magic == RAS_MAGIC)
        return(0);
    else
        return(-1);
}

/* Get the images colormap, if there is one */
int
ras_load_colormap(FILE *file,struct rasterfile *rh, int *map_length, u_char *red, u_char *green,u_char *blue)
{
    if (!file || !rh || !red || !green || !blue || !map_length)
        return(-1);

    /* Read colormap data, if any */
    switch (rh->ras_maptype) {
    case RMT_NONE :
    case RMT_RAW :
        if (rh->ras_maplength != 0) {
            fseek(file, rh->ras_maplength, 1);
        }
        *map_length = 0;
        return(0);
    case RMT_EQUAL_RGB :
        /* Allocate space for the colormap data */
        *map_length = rh->ras_maplength/3;

        if (fread(red, 1, *map_length, file) != *map_length)
            return(-1);
        if (fread(green, 1, *map_length, file) != *map_length)
            return(-1);
        if (fread(blue, 1, *map_length, file) != *map_length)
            return(-1);
        return(0);
    default :
        return(-1);
    }
}

/*
 * Load the image into an X11 image data structure
 * Assumes that the header and colormap have
 * already been allowed.
 */
#ifdef USEX11
XImage *
ras_load_ximage(display, visual, file, rh)
Display                 *display;
Visual                  *visual;
FILE                    *file;
struct rasterfile       *rh;
{
    	register int        	i, j, ras_bytes, bytes_left;
    	XImage              	*r_ximage;
    	register u_char     	*tmp_ptr;
    	u_char     		tmp_buf[4];
	XImage   		*xim_create_ximage();

    	if (!rh)
           return(0);

    	r_ximage = (XImage *)xim_create_ximage(display, &visual,
                        rh->ras_width, rh->ras_height,
                        check_depth(rh->ras_depth), 1);
    	if (!r_ximage)
           return(0);

    switch (rh->ras_type) {
    case RT_OLD :
    case RT_STANDARD :
        if (rh->ras_depth == 24) {
            /*
             * Assume that data is in the order BGR, but 24-bits
             * deep. Needs expanding to 32-bits.
             */
            ras_bytes = rh->ras_width * rh->ras_depth;
            ras_bytes = get_ras_bytes(ras_bytes);
            bytes_left = ras_bytes - (3 * rh->ras_width);
            tmp_ptr = (u_char *)(r_ximage->data + 1);
            for (i=0; i<rh->ras_height; i++) {
                for (j=0; j<rh->ras_width; j++) {
                    if (fread(tmp_ptr, 1, 3, file) != 3)
                        return(NULL);
                    tmp_ptr += 4;
                }

                /* Skip over the padding pixels */
                for (j=0; j<bytes_left; j++)
                    fgetc(file);
            }
        } else {
            /* Standard image just read the whole thing */
            if ((fread(r_ximage->data, 1, bytes_in_ximage(r_ximage), file)) !=
                    bytes_in_ximage(r_ximage))
                return(NULL);
        }
        break;
    case RT_FORMAT_RGB :
        if (rh->ras_depth == 24) {
            /* Expand and reverse to XBGR */
            ras_bytes = rh->ras_width * rh->ras_depth;
            ras_bytes = get_ras_bytes(ras_bytes);
            bytes_left = ras_bytes - (3 * rh->ras_width);
            tmp_ptr = (u_char *)(r_ximage->data + 1);
            for (i=0; i<rh->ras_height; i++) {
                for (j=0; j<rh->ras_width; j++) {
                    if (fread(tmp_buf, 1, 3, file) != 3)
                        return(NULL);
                    *tmp_ptr = tmp_buf[2];
                    *(tmp_ptr+1) = tmp_buf[1];
                    *(tmp_ptr+2) = tmp_buf[0];
                    tmp_ptr += 4;
                }

                /* Skip over the padding pixels */
                for (j=0; j<bytes_left; j++)
                    fgetc(file);
            }
        } else {
            /* Assume it is 32-bit data in XRGB format */
            tmp_ptr = (u_char *)(r_ximage->data + 1);
            for (i=0; i<rh->ras_height; i++) {
                for (j=0; j<rh->ras_width; j++) {
                    if (fread(tmp_buf, 1, 4, file) != 4)
                        return(NULL);
                    *tmp_ptr = tmp_buf[3];
                    *(tmp_ptr+1) = tmp_buf[2];
                    *(tmp_ptr+2) = tmp_buf[1];
                    tmp_ptr += 4;
                }
            }
        }
        break;
    case RT_BYTE_ENCODED :
        /* Image is byte encoded
        if (ras_read_rle(file, r_ximage->data, rh->ras_length) == -1)
            return(0);
	*/
        break;
    }
    return(r_ximage);
}

/*
 * Create an X11 image given a desired
 * width, height and depth.
 */
XImage *
xim_create_ximage(display, visual, w, h, depth, create_mem)
Display *display;
Visual  *visual;
int     w, h, depth, create_mem;
{
    	register long       howmuch;
    	XImage              *r_ximage;
        extern FILE         *fperr;

/*	The visual class is not detected on the SuSE or Mac platform, so just check for depth greater than 24.
	This should be good enough.
    	if((depth == 32) && (visual->class == TrueColor)) {
        	depth = 24;
    	}*/
    	if (depth > 24) depth = 24;

    	if(depth == 1)
           r_ximage = XCreateImage(display,visual,1,XYBitmap,0,0,w,h,16,0);
    	else
           r_ximage = XCreateImage(display,visual,depth,ZPixmap,0,0,w,h,16,0);

    	if(r_ximage == NULL) {
           err_warn(1,fperr, "Error - Cannot create image.\n");
           return((XImage *)NULL);
    	}

    	if(create_mem) {
           howmuch = bytes_in_ximage(r_ximage);
           r_ximage->data = (char *)malloc(howmuch);
           if(r_ximage->data == NULL) {
               err_warn(1,fperr, "Error - Cannot malloc image data.\n");
               return((XImage *)NULL);
           }

           /* Clear the memory out */
           memset(r_ximage->data, '\0', howmuch);
       	}

    	return(r_ximage);
}
/*
 * Create a shared memory X11 image
 *
XImage *
xim_create_shmximage(display, visual, w, h, depth, shminfo)
Display         *display;
Visual          *visual;
int             w, h, depth;
XShmSegmentInfo *shminfo;
{
    XImage              *ximage;
    int                 howmuch;
 
    *if(!XShmQueryVersion(display,&majorv,&minorv,&sharedPixmaps)) {
        fprintf(stderr,"X11 Shared memory images not available");
        return((XImage *)NULL);
    }*
 
    if((depth == 32) && (visual->class == TrueColor)) {
        depth = 24;
    }
 
    * Create the Shared Memory XImage *
    if(depth == 1)
        ximage = XShmCreateImage(display, visual, 1,
                                 XYBitmap, 0, shminfo, w, h);
    else
        ximage = XShmCreateImage(display, visual, depth,
                                     ZPixmap, 0, shminfo, w, h);
 
    * Create the Shared Memory Segment to store the image data *
    howmuch = h*ximage->bytes_per_line+1024;
    shminfo->shmid = shmget(IPC_PRIVATE, howmuch, IPC_CREAT|0777);
    if(shminfo->shmid < 0) {
        perror("shmget");
        return((XImage *)NULL);
    }

    * Attach this Shared Memory Segment to the process *
    shminfo->shmaddr = (char *)shmat(shminfo->shmid, 0, 0);
    if(shminfo->shmaddr == ((char *)-1)) {
        perror("shmat");
        return((XImage *)NULL);
    }
    ximage->data = shminfo->shmaddr;
 
    * Clear the memory out *
    *bzero(ximage->data, howmuch);*
 
    shminfo->readOnly = False;

    * Tell the server to attach to the Shared Memory Segment *
    XShmAttach(display, shminfo);

    XSync(display,0);

    shmctl(shminfo->shmid, IPC_RMID, 0);

    * Return the successfully created Shared Memory XImage *
    return(ximage);
}
*/

/*
 * Destroy the X11 shared memory image specified
 *
int
xim_destroy_shmximage(display, ximage, shminfo)
Display         *display;
XImage          *ximage;
XShmSegmentInfo *shminfo;
{
    int rc;
 
    if (ximage) {
*        XShmDetach(display, shminfo);*
        XDestroyImage(ximage);
        rc = shmdt(shminfo->shmaddr);
        if(rc < 0) {
            perror("shmdt");
            return(-1);
        }
        rc = shmctl(shminfo->shmid, IPC_RMID, 0);
        if (rc < 0) {
            perror("shmctl");
            return(-1);
        }
    } else {
        return(-1);
    }
    return(0);
}
*/

/*
 * Try to allocate read/write cells from the default color map.
 * This is the preferred choice as the window manager will install
 * the map for us. Need to return the value of the offset so
 * that we can compensate for it elsewhere.
 */
int
xim_load_default_cmap(display, cmap, r, g, b, map_length, offset)
Display         *display;
Colormap        cmap;
u_char          *r, *g, *b;
int             map_length, *offset;
{
    Status              rc;
    register int        i;
    u_long              pixels[256], plane_masks[256];
    XColor              colors[256];

    rc = XAllocColorCells(display,
                          cmap, True, plane_masks, 0, pixels, map_length);

    if (!rc) {
        /* XAllocColorCells failed, return a failure */
        return(-1);
    }

    if ((pixels[0]+map_length-1) != pixels[map_length-1]) {
        /*
         * The map returned wasn't contigous.
         * Free the colors returned in pixels.
         * Return a failure.
         */
        XFreeColors(display, cmap, pixels, map_length, 0);
        return(-1);
    }

    /* Now set the values of the colour array */
    for (i=0; i<map_length; i++) {
        colors[i].pixel = i + pixels[0];
        colors[i].red = r[i]<<8;
        colors[i].green = g[i]<<8;
        colors[i].blue = b[i]<<8;
        colors[i].flags = DoRed | DoGreen | DoBlue;
    }

    /* Return the value of offset to the user */
    *offset = pixels[0];

    /* Store it in the colour map */
    XStoreColors(display, cmap, colors, map_length);
    return(0);
}

/*
 * Use the private color map and install it in
 * the canvas
 */
void
xim_load_private_cmap(display, cmap, r, g, b, map_length, offset,
        base_xid, pw_xid)
Display         *display;
Colormap        cmap;
u_char          *r, *g, *b;
int             map_length, offset;
XID             base_xid, pw_xid;
{
    register int        i;
    u_long              pixels[256], plane_masks[256];
    XColor              colors[256];
    Atom                cmap_atom;

    /* Allocate 256 cells again */
    XAllocColorCells(display,
                     cmap, True, plane_masks, 0, pixels, 256);

    /* Now set the values of the colour array */
    for (i=0; i<map_length; i++) {
        colors[i].pixel = i + offset;
        colors[i].red = r[i]<<8;
        colors[i].green = g[i]<<8;
        colors[i].blue = b[i]<<8;
        colors[i].flags = DoRed | DoGreen | DoBlue;
    }

    /* Free the ones we don't need *
    if (offset) {
        * Free window manager colors *
        for(i=0; i<offset; i++)
            pixels[i] = i;
        XFreeColors(display, cmap, pixels, offset, 0);

        * Free colors at the top of the map *
        for(i=(offset+map_length); i<256; i++)
            pixels[i-(offset+map_length)] = i;
        XFreeColors(display, cmap, pixels,
                    256-(offset+map_length), 0);
    }
    */

    /* Store it in the colour map */
    /*XStoreColors(display, cmap, colors, map_length);*/

#ifdef NON_ICCCM
    /* Now install it in the h/ware */
    /* This is non ICCCM compliant */
    XInstallColormap(display, cmap);
#else
    /*
     * This gobbledigook (sp ?) tells the window manager that
     * we are using a private colormap. It must be installed
     * against the frame window as that is the only window the
     * ICCCM says the window manager must know about.
     */
    cmap_atom = XInternAtom(display, "WM_COLORMAP_WINDOWS", False);
    XChangeProperty(display, base_xid, cmap_atom, XA_WINDOW,
                    32, PropModeAppend, (unsigned char*)&pw_xid, 1);
#endif /*NON_ICCCM*/
    return;
}

/*
 * Add, subtract an offset
 */
int
xim_add_offset(src_ximage, dst_ximage, offset)
XImage          *src_ximage, *dst_ximage;
register int    offset;
{
    register int        i, j, lbytes_in, lbytes_out;
    register u_char     *src_data;
    register u_char     *dst_data;

    /* Check for NULL  pointers */
    if(!src_ximage || !src_ximage->data)
        return(-1);

    /* Only works for 8-bit images */
    if(src_ximage->depth != 8)
        return(-1);

    if(!dst_ximage) {
        /* Just add the offset to the source data */
        src_data = (u_char *)src_ximage->data;
        lbytes_in = src_ximage->bytes_per_line;

        for(i=0; i<src_ximage->height; i++) {
            for(j=0; j<src_ximage->width; j++)
                src_data[j] += offset;
            src_data += lbytes_in;
        }
    } else {
        /* Check for NULL  pointers */
        if(!dst_ximage->data)
            return(-1);

        /* Only works for 8-bit images */
        if(dst_ximage->depth != 8)
            return(-1);

        /* Place offset image in the destination image */
        src_data = (u_char *)src_ximage->data;
        dst_data = (u_char *)dst_ximage->data;
        lbytes_in = src_ximage->bytes_per_line;
        lbytes_out = dst_ximage->bytes_per_line;

        for(i=0; i<src_ximage->height; i++) {
            for(j=0; j<src_ximage->width; j++)
                dst_data[j] = src_data[j] + offset;
            src_data += lbytes_in;

            dst_data += lbytes_out;
        }
    }
    return(0);
}
#endif

/*******************************************************************************
        END OF FILE
*******************************************************************************/
