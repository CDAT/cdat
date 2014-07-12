#include <stdlib.h>
#include <string.h>
#include "gks.h"
#include "gksshort.h"
#include <memory.h>
#ifdef USEX11
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif
#include "rasterfile.h"
#include "color.h"

/*		Active colormap name.					*/

    extern char active_colors[];
    extern struct c_val std_color[16];
    extern struct color_table C_tab;

/* 
 * Write a long word in sun byte-order returns 0 for success, EOF for failure.
 * This works for big- and little-indian machines.
 */
static int write_sun_long (l, fp)
int l;
FILE *fp;
{
    char c;

    c = ((l >> 24) & 0xff);
    if (putc (c, fp) == EOF) return (EOF);
    c = ((l >> 16) & 0xff);
    if (putc (c, fp) == EOF) return (EOF);
    c = ((l >> 8) & 0xff);
    if (putc (c, fp) == EOF) return (EOF);
    c = (l & 0xff);
    if (putc (c, fp) == EOF) return (EOF);
    return (0);
}

/*
 * Convert the 3 xcolor r,g,b arrays to 3 u_char r,g,b arrays
 */

int convert_gks_color(r, g, b)
unsigned char  *r, *g, *b;
{
    struct color_table *ctab;
    register int i;

    for (ctab=&C_tab; strcmp(active_colors,ctab->name)!=0;ctab=ctab->next);
    if (ctab == NULL)
      {
      }
    
    for(i=0; i<240; i++) {
        r[i] = ctab->cval[i].red*2.550+0.5;
        g[i] = ctab->cval[i].green*2.550+0.5;
        b[i] = ctab->cval[i].blue*2.550+0.5;
    }
    for(i=240; i<256; i++) {
        r[i] = std_color[i-240].red*2.550+0.5;
        g[i] = std_color[i-240].green*2.550+0.5;
        b[i] = std_color[i-240].blue*2.550+0.5;
    }

    return 1;
}

/*
 * Dump the specified ximage and colour map to a file
 */
#ifdef USEX11
int ras_write_ximage(FILE *file, XImage *ximage, u_char *r, u_char *g, u_char *b, int map_length)
#else
#include <sys/types.h>
#include "workstations.h"
  extern Gconid_X_drawable connect_id;/*VCS canvas display and drawable id */
int ras_write_ximage(FILE *file, void *ximage, u_char *r, u_char *g, u_char *b, int map_length)
#endif
{
    struct rasterfile   rh;
    register int        i, ras_bytes, bytes_to_copy;
    register u_char     *data_ptr, *out_data;
    int                 rc;
                        /* Lint complains about this useful for
                         * debugging so I'll leave it in
                         */
#define get_ras_bytes(x)        ((x)/16 + ((x)%16 ? 1 : 0))*2;

    if(!ximage)
        return(-1);

    rh.ras_magic = RAS_MAGIC;
#ifdef X11WM
    rh.ras_width = ximage->width;
    rh.ras_height = ximage->height;
#elif defined QTWM
    rh.ras_width = cairo_image_surface_get_width(connect_id.surface);
    rh.ras_height = cairo_image_surface_get_height(connect_id.surface);
#else
    fprintf(stderr,"insert here your get image dims func");
#endif
#ifdef X11WM
    if (ximage->depth == 24)
        rh.ras_depth = 32;
    else
        rh.ras_depth = ximage->depth;
#elif defined QTWM
        rh.ras_depth = 32;
#else
	fprintf(stderr,"insert here your image depth func\n");
#endif

    rh.ras_type = RT_STANDARD;
    if(map_length == 0) {
        rh.ras_maptype = RMT_NONE;
        rh.ras_maplength = 0;
    } else {
        rh.ras_maptype = RMT_EQUAL_RGB;
        rh.ras_maplength = 3*map_length;
    }

    /*
     * Dump everything to the file
     * 1. Calculate the amount of image data to be dumped
     * 2. The rasterfile header
     * 3. the colour map if there is one
     * 4. the image data
     */
    ras_bytes = rh.ras_width*rh.ras_depth;
    ras_bytes = get_ras_bytes(ras_bytes);
    if ((out_data = (u_char *)malloc(ras_bytes)) == NULL)
        return(-1);
    memset((void *)out_data, '\0', ras_bytes);

    rh.ras_length = ras_bytes * rh.ras_height;
    write_sun_long (rh.ras_magic    , file);
    write_sun_long (rh.ras_width    , file);
    write_sun_long (rh.ras_height   , file);
    write_sun_long (rh.ras_depth    , file);
    write_sun_long (rh.ras_length   , file);
    write_sun_long (rh.ras_type     , file);
    write_sun_long (rh.ras_maptype  , file);
    write_sun_long (rh.ras_maplength, file);

    /* Keep the value of rc around for debugging purposes */
    if(rh.ras_maptype == RMT_EQUAL_RGB) {
        rc = fwrite(b, sizeof(u_char), map_length, file);
        rc = fwrite(g, sizeof(u_char), map_length, file);
        rc = fwrite(r, sizeof(u_char), map_length, file);
    }

    /* Write the image data line by line ensures a good raster file */
#ifdef X11WM
    data_ptr = (u_char *)ximage->data;
#elif defined QTWM
    data_ptr = (u_char *)ximage;
#else
    fprintf(stderr,"insert here your image data pointer\n");
#endif
    if (rh.ras_depth == 1)
        bytes_to_copy = rh.ras_width/8 + (rh.ras_width%8 ? 1 : 0);
    else
        bytes_to_copy = rh.ras_width * rh.ras_depth/8;

    for (i=0; i<rh.ras_height; i++) {
        /*bcopy(data_ptr, out_data, bytes_to_copy); Only for BSD */
        memcpy((void *)out_data, (const void *)data_ptr, bytes_to_copy);
        rc = fwrite(out_data, sizeof(u_char), ras_bytes, file);
        memset((void *)out_data, '\0', ras_bytes);
#ifdef X11WM
        data_ptr += ximage->bytes_per_line;
#elif defined QTWM
	//fprintf(stderr,"need to double check this stride deal\n");
	data_ptr += cairo_image_surface_get_stride(connect_id.surface);
#else
	fprintf(stderr,"insert here yuor move one row datptr func\n");
#endif
    }
    free(out_data);
    return(0);
}

#ifdef USEX11
/* Print information on the file */
void
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
#endif
