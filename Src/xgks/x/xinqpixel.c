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

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif


/*
 * xXgksInqPixelarrayDim(ws, rect, dim)	INQUIRE PIXEL ARRAY DIMENSIONS
 *
 * WS_STATE_PTR ws;		workstation state list pointer.
 * Grect *rect;			rectangele pointer.
 * Gipoint *dim;		OUT dimensions of the pixel array.
 */
xXgksInqPixelarrayDim(ws, rect, dim)
    WS_STATE_PTR    ws;			/* workstation */
    Grect          *rect;		/* rectangle of region of interest */
    Gipoint        *dim;		/* returned dimensional sizes */
{
    Gpoint          p1, p2;
#ifdef X11OUT
    XPoint          xp1, xp2;
#else
    Gpoint          xp1,xp2;
#endif

#ifdef X11OUT
    (void) XgksSIGIO_OFF(ws->dpy);
#endif

    /* Transform the rectangle from WC to X */
    WcToNdc(&(rect->ll), &p1);
    WcToNdc(&(rect->ur), &p2);

    NdcToX(ws, &p1, &xp1);
    NdcToX(ws, &p2, &xp2);

    /* Set the sizes of the dimensions */
    dim->x = (unsigned) (xp2.x - xp1.x + 1);
    dim->y = (unsigned) (xp1.y - xp2.y + 1);

#ifdef X11OUT
    (void) XgksSIGIO_ON(ws->dpy);
#endif

    return OK;
}


/*
 * xXgksInqPixelarray(ws, point, dim, pxarr)	INQUIRE PIXEL ARRAY
 *
 * WS_STATE_PTR ws;		workstation state list pointer.
 * Gpoint *point;		pixel array location pointer.
 * Gipoint *dim;		dimensions of the pixel array.
 * Gpxarray *pxarr;		OUT pixel array.
 *
 */
xXgksInqPixelarray(ws, point, dim, pxarr)
    WS_STATE_PTR    ws;			/* workstation */
    Gpoint         *point;		/* WC origin of desired pixel-array */
    Gipoint        *dim;		/* size of desired pixel-array in
					 * pixels */
    Gpxarray       *pxarr;		/* returned colour-index array
					 * structure */
{
    Gpoint          p1;			/* DC origin of desired pixel-array */
#ifdef X11OUT
    Display        *dpy = ws->dpy;	/* convenient display pointer */
    Window          win = ws->win;	/* convenient window ID */
    Window          dummywin;		/* dummy window for XTranslate...() */
    XWindowAttributes rootattr;		/* root-window attributes */
    XWindowAttributes wsattr;		/* XGKS-window attributes */
    XPoint          xp1;		/* array-origin in XGKS-window
					 * co-ordinates */
    XImage         *image;		/* X pixel-array */
#else
    Gpoint          xp1;		
#endif
    int             x0, y0;		/* XY-coordinates of the origin of
					 * the XGKS-window -- given in
					 * X-pixels relative to the origin of
					 * the root- window */
    int             maxx, maxy;		/* XY-coordinates of the most lower-
					 * right, visible pixel in the XGKS-
					 * window -- given in X-pixels
					 * relative to the origin of the
					 * XGKS-window */
    int             x1, y1;		/* XY-coordinates of upper-left pixel
					 * of returned pixel-array -- given
					 * in X-pixels relative to the origin
					 * of the XKGS-window */
    int             x2, y2;		/* XY-coordinates of lower-right
					 * pixel of returned pixel-array --
					 * given in X-pixels relative to the
					 * origin of the XKGS-window */
    int             x, y;		/* general XY-coordinates of a pixel
					 * in the returned pixel-array --
					 * given in returned-array indices */
    Gint           *intp;		/* pointer into returned pixel-array */

#ifdef X11OUT
    (void) XgksSIGIO_OFF(ws->dpy);
#endif

    /* Transform the rectangle from WC to X */
    WcToNdc(point, &p1);
    NdcToX(ws, &p1, &xp1);

    /*
     * In case the window is not completely on the screen, find the maximum
     * co-ordinates of the visible portion.  NB: we translate the position of
     * the XGKS window to that of the root window to account for possible
     * reparenting of the XGKS window by a window-manager.
     */
#ifdef X11OUT
    XGetWindowAttributes(dpy, win, &wsattr);
    XGetWindowAttributes(dpy, wsattr.root, &rootattr);
    XTranslateCoordinates(dpy, win, wsattr.root, 0, 0, &x0, &y0,
			  &dummywin);
    maxx = ((x0 + ws->wbound.x <= rootattr.width)
	    ? ws->wbound.x : rootattr.width - x0) - 1;
    maxy = ((y0 + ws->wbound.y <= rootattr.height)
	    ? ws->wbound.y : rootattr.height - y0) - 1;
#endif
    pxarr->covalid = GABSENT;

    /* Set the upper and lower limits on the valid-pixel indices */
    if (xp1.x < 0) {
	x1 = 0;
	pxarr->covalid = GPRESENT;
    } else if (xp1.x > maxx) {
	x1 = maxx;
	pxarr->covalid = GPRESENT;
    } else {
	x1 = xp1.x;
    }

    if (xp1.y < 0) {
	y1 = 0;
	pxarr->covalid = GPRESENT;
    } else if (xp1.y > maxy) {
	y1 = maxy;
	pxarr->covalid = GPRESENT;
    } else {
	y1 = xp1.y;
    }

    if ((xp1.x + dim->x - 1) < 0) {
	x2 = 0;
	pxarr->covalid = GPRESENT;
    } else if ((xp1.x + dim->x - 1) > maxx) {
	x2 = maxx;
	pxarr->covalid = GPRESENT;
    } else {
	x2 = xp1.x + dim->x - 1;
    }

    if ((xp1.y + dim->y - 1) < 0) {
	y2 = 0;
	pxarr->covalid = GPRESENT;
    } else if ((xp1.y + dim->y - 1) > maxy) {
	y2 = maxy;
	pxarr->covalid = GPRESENT;
    } else {
	y2 = xp1.y + dim->y - 1;
    }
#ifdef X11OUT
    /* Get the X pixel-array */
    image = XGetImage(dpy, win, x1, y1, x2 - x1 + 1, y2 - y1 + 1, AllPlanes,
		      XYPixmap);
#endif
    /* Allocate memory for the returned array */
    pxarr->array = (Gint *) malloc((size_t) (dim->x * dim->y * sizeof(Gint)));
    GKSERROR((pxarr->array == NULL), 300, errxInqPixelarray);
    intp = pxarr->array;

    /*
     * Translate the X pixel-values into either GKS colour-indices or the
     * "no-data" value, -1.
     */
    if (pxarr->covalid == GPRESENT) {
	for (y = 0; y < dim->y; y++) {
	    int             yy = xp1.y + y;

	    for (x = 0; x < dim->x; x++) {
		int             xx = xp1.x + x;
#ifdef X11OUT
		*intp++ = (xx >= x1 && xx <= x2 && yy >= y1 && yy <= y2)
		    ? XcColourIndex(ws, XGetPixel(image, xx - x1, yy - y1))
		    : -1;
#endif
	    }
	}
    } else
	for (y = 0; y < dim->y; y++) {
	  for (x = 0; x < dim->x; x++) {
#ifdef X11OUT
		*intp++ = XcColourIndex(ws, XGetPixel(image, x, y));
#endif
	  }
	}
#ifdef X11OUT
    /* Release the no-longer-needed X-image */
    (void) XDestroyImage(image);

    (void) XgksSIGIO_ON(ws->dpy);
#endif
    return OK;
}


/*
 * xXgksInqPixel(ws, ppoint, pix)	INQUIRE PIXEL
 *
 * WS_STATE_PTR ws;		workstation state list pointer.
 * Gpoint *ppoint;		pixel location pointer.
 * Gint *pix;  			OUT pixel colour.
 *
 */
xXgksInqPixel(ws, ppoint, pix)
    WS_STATE_PTR    ws;			/* workstation */
    Gpoint         *ppoint;		/* desired-pixel position in WC */
    Gint           *pix;		/* returned colour-index */
{
    Gpoint          p1;			/* DC origin of desired pixel-array */
#ifdef X11OUT
    Display        *dpy = ws->dpy;	/* convenient display pointer */
    Window          win = ws->win;	/* convenient window ID */
    Window          dummywin;		/* dummy window for XTranslate...() */
    XWindowAttributes rootattr;		/* root-window attributes */
    XWindowAttributes wsattr;		/* XGKS-window attributes */
    XPoint          xp1;		/* array-origin in XGKS-window */
#else
    Gpoint xp1;
#endif
					 /* co-ordinates */
    int             x0, y0;		/* XY-coordinates of the origin of
					 * the XGKS-window -- given in
					 * X-pixels relative to the origin of
					 * the root- window */
    int             maxx, maxy;		/* XY-coordinates of the most lower-
					 * right, visible pixel in the XGKS-
					 * window -- given in X-pixels
					 * relative to the origin of the
					 * XGKS-window */

#ifdef X11OUT
    (void) XgksSIGIO_OFF(ws->dpy);
#endif
    /* Transform the rectangle from WC to X */
    WcToNdc(ppoint, &p1);
    NdcToX(ws, &p1, &xp1);

    /*
     * In case the window is not completely on the screen, find the maximum
     * co-ordinates of the visible portion.  NB: we translate the position of
     * the XGKS window to that of the root window to account for possible
     * reparenting of the XGKS window by a window-manager.
     */
#ifdef X11OUT
    XGetWindowAttributes(dpy, win, &wsattr);
    XGetWindowAttributes(dpy, wsattr.root, &rootattr);
    XTranslateCoordinates(dpy, win, wsattr.root, 0, 0, &x0, &y0,
			  &dummywin);
    maxx = ((x0 + ws->wbound.x <= rootattr.width)
	    ? ws->wbound.x : rootattr.width - x0) - 1;
    maxy = ((y0 + ws->wbound.y <= rootattr.height)
	    ? ws->wbound.y : rootattr.height - y0) - 1;
#endif
    /* Get the X pixel-value and convert it to a GKS colour-index */
    if (xp1.x >= 0 && xp1.x <= maxx && xp1.y >= 0 && xp1.y <= maxy) {
#ifdef X11OUT
	XImage         *image = XGetImage(dpy, win, xp1.x, xp1.y, 1, 1,
					  AllPlanes, XYPixmap);

	*pix = XcColourIndex(ws, XGetPixel(image, 0, 0));
	(void) XDestroyImage(image);
#endif
    } else {
	*pix = -1;
    }

#ifdef X11OUT
    (void) XgksSIGIO_ON(dpy);
#endif
    return OK;
}

