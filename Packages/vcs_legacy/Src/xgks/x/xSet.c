/*
 *		Copyright IBM Corporation 1989
 *
 *                      All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of IBM not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 *
 * University of Illinois at Urbana-Champaign
 * Department of Computer Science
 * 1304 W. Springfield Ave.
 * Urbana, IL	61801
 *
 * (C) Copyright 1987, 1988 by The University of Illinois Board of Trustees.
 *	All rights reserved.
 *
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include "gks_implem.h"

#include "cairo/cairo.h"
#include "workstations.h"
extern Gconid_X_drawable connect_id;/*VCS canvas display and drawable id */


#ifndef lint
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif

#ifdef X11DRAW
xXgksSetForeground(dpy, gc, fg)
    Display        *dpy;
    GC              gc;
    unsigned long   fg;
{
    XSetForeground(dpy, gc, fg);
}
#endif
#ifdef CAIRODRAW
cairoXgksSetForeground(cairo_t *cr, unsigned long fg)
{
  VCS2CAIRO_setrgb(cr,fg);
}
#ifndef USEX11
#include "Qt/Qt_X_emul.h"
#endif

#endif

#ifdef X11DRAW
xXgksSetLineAttributes(dpy, gc, line_width, line_style, cap_style, join_style)
    Display        *dpy;
    GC              gc;
    unsigned int    line_width;
    int             line_style, cap_style, join_style;
{

    XSetLineAttributes(dpy, gc, line_width, line_style, cap_style,
		       join_style);
}
#endif
#ifdef CAIRODRAW
cairoXgksSetLineAttributes(cr, line_width,line_style,cap_style, join_style)
    cairo_t *cr; 
    unsigned int    line_width;
    int             line_style, cap_style, join_style;
{
/*   printf("setting attributes to: %i, %i, %i, %i\n",line_width,line_style,cap_style, join_style); */
  if (line_width==0) {
    cairo_set_line_width(cr,.5);
  }
  else {
    cairo_set_line_width(cr,(double)line_width);
  }
  if (join_style == JoinMiter) {
    cairo_set_line_join(cr,CAIRO_LINE_JOIN_MITER);
  }
  else {
    printf("unknown join type: %i\n",join_style);
  }
  if (cap_style == CapButt ) {
    //printf("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5\n");
    cairo_set_line_cap(cr,CAIRO_LINE_CAP_BUTT);
  }
  else {
    printf("unknown cap type: %i\n",cap_style);
  }
/*   if (line_style == LineSolid) { */
/*     cairo_set_dash(cr,NULL,0,0.); /\* no dash *\/ */
/*   } */
}
#endif

#ifdef X11DRAW
xXgksSetStipple(dpy, gc, stipple)
    Display        *dpy;
    GC              gc;
    Pixmap          stipple;
{
    XSetStipple(dpy, gc, stipple);
}
#endif

#ifdef X11DRAW
 xXgksSetDashes(dpy, gc, ws, i)
    Display        *dpy;
    GC              gc;
    WS_STATE_PTR    ws;
    Gint            i;
 {
     if (i != ws->last_dash_index)
	 XSetDashes(dpy, gc, 0, xgksDASHES[i].dashl, xgksDASHES[i].dn);
 }
#endif
#ifdef CAIRODRAW
  cairoXgksSetDashes(cairo_t *cr,Gint i)
  {
    double dashes[17];
    int j;
    for (j=0;j<xgksDASHES[i].dn;j++) {
      dashes[j] = (double) xgksDASHES[i].dashl[j];
    }
    cairo_set_dash(cr,&dashes[0],xgksDASHES[i].dn,0.);
  }
#endif


#ifdef X11DRAW
xXgksSetTile(dpy, gc, tile)
    Display        *dpy;
    GC              gc;
    Pixmap          tile;
{
    XSetTile(dpy, gc, tile);
}
#endif

#ifdef X11DRAW
xXgksSetClipMask(dpy, gc, pixmap)
    Display        *dpy;
    GC              gc;
    Pixmap          pixmap;
{
    XSetClipMask(dpy, gc, pixmap);
}
#endif

#ifdef X11DRAW
xXgksSetPlineClipRectangles(dpy, gc, ws, rectangle)
    Display        *dpy;
    GC              gc;
    WS_STATE_PTR    ws;
    XRectangle     *rectangle;
{
    if ((rectangle->x != ws->last_pline_rectangle.x)
	    || (rectangle->y != ws->last_pline_rectangle.y)
	    || (rectangle->width != ws->last_pline_rectangle.width)
	    || (rectangle->height != ws->last_pline_rectangle.height)) {
	XSetClipRectangles(dpy, gc, 0, 0, rectangle, 1, Unsorted);
	ws->last_pline_rectangle = *rectangle;
    }
}
#endif
#ifdef CAIRODRAW
cairoXgksSetPlineClipRectangles(cr, ws, rectangle)
    cairo_t *cr;
    WS_STATE_PTR    ws;
#ifdef USEX11
    XRectangle     *rectangle;
#else
    Grectangle     *rectangle;
#endif
{
/*   double x1,x2,y1,y2; */
    if ((rectangle->x != ws->last_pline_rectangle.x)
	    || (rectangle->y != ws->last_pline_rectangle.y)
	    || (rectangle->width != ws->last_pline_rectangle.width)
	    || (rectangle->height != ws->last_pline_rectangle.height)) {
      cairo_reset_clip(cr);
      cairo_rectangle(cr,rectangle->x,rectangle->y,rectangle->width,rectangle->height);
      cairo_clip(cr);
      ws->last_pline_rectangle = *rectangle;
    }

}
#endif

#ifdef X11DRAW
xXgksSetPmarkerClipRectangles(dpy, gc, ws, rectangle)
    Display        *dpy;
    GC              gc;
    WS_STATE_PTR    ws;
    XRectangle     *rectangle;
{
    if ((rectangle->x != ws->last_pmarker_rectangle.x)
	    || (rectangle->y != ws->last_pmarker_rectangle.y)
	    || (rectangle->width != ws->last_pmarker_rectangle.width)
	    || (rectangle->height != ws->last_pmarker_rectangle.height)) {
	XSetClipRectangles(dpy, gc, 0, 0, rectangle, 1, Unsorted);
	ws->last_pmarker_rectangle = *rectangle;
    }
}
#endif
#ifdef CAIRODRAW
cairoXgksSetPmarkerClipRectangles(cr, ws, rectangle)
    cairo_t *cr;
    WS_STATE_PTR    ws;
#ifdef USEX11
    XRectangle     *rectangle;
#else
    Grectangle     *rectangle;
#endif
{
    if ((rectangle->x != ws->last_pmarker_rectangle.x)
	    || (rectangle->y != ws->last_pmarker_rectangle.y)
	    || (rectangle->width != ws->last_pmarker_rectangle.width)
	    || (rectangle->height != ws->last_pmarker_rectangle.height)) {
      cairo_reset_clip(cr);
      cairo_rectangle(cr,rectangle->x,rectangle->y,rectangle->width,rectangle->height);
      cairo_clip(cr);
      ws->last_pmarker_rectangle = *rectangle;
    }

}
#endif

#ifdef X11DRAW
xXgksSetFillAreaClipRectangles(dpy, gc, ws, rectangle)
    Display        *dpy;
    GC              gc;
    WS_STATE_PTR    ws;
    XRectangle     *rectangle;
{
    if ((rectangle->x != ws->last_farea_rectangle.x)
	    || (rectangle->y != ws->last_farea_rectangle.y)
	    || (rectangle->width != ws->last_farea_rectangle.width)
	    || (rectangle->height != ws->last_farea_rectangle.height)) {
	XSetClipRectangles(dpy, gc, 0, 0, rectangle, 1, Unsorted);
	ws->last_farea_rectangle = *rectangle;
    }
}
#endif
#ifdef CAIRODRAW
cairoXgksSetFillAreaClipRectangles(cr, ws, rectangle)
    cairo_t *cr;
    WS_STATE_PTR    ws;
#ifdef USEX11
    XRectangle     *rectangle;
#else
    Grectangle     *rectangle;
#endif
{
  if ((rectangle->x != ws->last_farea_rectangle.x)
      || (rectangle->y != ws->last_farea_rectangle.y)
      || (rectangle->width != ws->last_farea_rectangle.width)
      || (rectangle->height != ws->last_farea_rectangle.height)) {
    cairo_reset_clip(cr);
    cairo_rectangle(cr,rectangle->x,rectangle->y,rectangle->width,rectangle->height);
    cairo_clip(cr);
    ws->last_farea_rectangle = *rectangle;
  }
}
#endif

#ifdef X11DRAW
xXgksSetTextClipRectangles(dpy, gc, ws, rectangle)
    Display        *dpy;
    GC              gc;
    WS_STATE_PTR    ws;
    XRectangle     *rectangle;
{
    if ((rectangle->x != ws->last_text_rectangle.x)
	    || (rectangle->y != ws->last_text_rectangle.y)
	    || (rectangle->width != ws->last_text_rectangle.width)
	    || (rectangle->height != ws->last_text_rectangle.height)) {
	XSetClipRectangles(dpy, gc, 0, 0, rectangle, 1, Unsorted);
	ws->last_text_rectangle = *rectangle;
    }
}
#endif

#ifdef X11DRAW
xXgksSetFillStyle(dpy, gc, fill_style)
    Display        *dpy;
    GC              gc;
    int             fill_style;
{
    XSetFillStyle(dpy, gc, fill_style);
}
#endif
#ifdef CAIRODRAW
cairoXgksSetFillStyle(cairo_t *cr, int fill_style)
{
  /* X11 Allows for: FillSolid, FillTiled, FillStippled, or FillOpaqueStippled. */
  /* not sure cairo needs this...*/
}
#endif
