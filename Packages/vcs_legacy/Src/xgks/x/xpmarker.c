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
 * All rights reserved.
 *
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 *
 * XGKS polymarker primitive output ws : the pointer to current workstation list
 * pmk_ptr: the pointer to the output polymarker primitive
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include "gks_implem.h"

#ifndef USEX11
#include "Qt/Qt_X_emul.h"
#endif

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif

#ifdef CAIRODRAW
#include <cairo/cairo.h>
#include "workstations.h"
extern Gconid_X_drawable connect_id;/*VCS canvas display and drawable id */
#endif

xXgksPolyMarker(ws, pmk_ptr)
    WS_STATE_PTR    ws;
    PMARK_ST       *pmk_ptr;

{
#ifdef USEX11
    Display        *dpy;
    Window          win;
    GC              gc;
    XPoint         *xpe, *ype;
#else
    Gpoint         *xpe, *ype;
#endif
#ifdef CAIRODRAW
    cairo_t        *cr;
#endif
    int             mark_type;

    Gmkbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;
    Gpoint         *pe;
    Gint            gi;
    Gmkattr        *ptr;

    int             i, npnt;
    float           mark_size;

    if (ws->ewstype != X_WIN)
	return 0;

    /* Initialization  */

#ifdef X11WM
    (void) XgksSIGIO_OFF(ws->dpy);

    dpy = ws->dpy;
    win = ws->win;
    gc = ws->pmarkgc;

    xpe = (XPoint *) malloc((size_t) (sizeof(pmk_ptr->location[0]) * 
				      pmk_ptr->num_pts));
#else
    xpe = (Gpoint *) malloc((size_t) (sizeof(pmk_ptr->location[0]) * 
			    pmk_ptr->num_pts));
#endif
#ifdef CAIRODRAW
    cr = connect_id.cr;
#endif
    ype = xpe;

    GKSERROR((xpe == NULL), 300, errxPolyMarker);

    /* Set current GC values for the primitive  */

    ptr = &(pmk_ptr->mkattr);
    gi = ptr->mark;
    if (gi < 1 || gi >= MAX_BUNDL_TBL)
	gi = 1;
    idv_ptr = &(ptr->bundl);
    bdl_ptr = &(ws->mkbundl_table[gi]);

    if (ptr->colour == GBUNDLED)		/* marker type  */
	bundl_ptr = bdl_ptr;
    else
	bundl_ptr = idv_ptr;

    i = bundl_ptr->colour;
    if (!WS_AVAIL_COLOUR(ws, i))
	i = 1;
    if (ws->wscolour == 2) {			/* monochrome ? */
	if (i == 0)
	    i = ws->wsbg;
	else if (i == 1)
	    i = ws->wsfg;
    }
#ifdef X11DRAW
    xXgksSetForeground(dpy, gc, XcPixelValue(ws, i));
#endif
#ifdef CAIRODRAW
    cairoXgksSetForeground(cr,i);
#endif
    /* the fill style attribute */

    if (ptr->size == GBUNDLED)			/* marker size */
	mark_size = (float) bdl_ptr->size;
    else
	mark_size = (float) idv_ptr->size;


    if (ptr->type == GBUNDLED)			/* marker type  */
	bundl_ptr = bdl_ptr;
    else
	bundl_ptr = idv_ptr;

    if (WS_MARKER_TYPE(bundl_ptr->type))
	mark_type = bundl_ptr->type;
    else
	mark_type = GMK_STAR;

#ifdef X11DRAW
    xXgksSetLineAttributes(dpy, gc, 0, LineSolid, CapButt, JoinMiter);
    xXgksSetFillStyle(dpy, gc, FillSolid);

    xXgksSetPmarkerClipRectangles(dpy, gc, ws, &(ws->xclip));
#endif
#ifdef CAIRODRAW
    cairoXgksSetLineAttributes(cr, 0, LineSolid, CapButt, JoinMiter);
    cairoXgksSetFillStyle(cr, FillSolid);

    cairoXgksSetPmarkerClipRectangles(cr, ws, &(ws->xclip));
#endif
    /* Display Workstation Transformation */

    pe = pmk_ptr->location;
    npnt = pmk_ptr->num_pts;

    for (i = 0; i < npnt; i++) {
	NdcToX(ws, pe, xpe);
	++xpe;
	++pe;
    }

    /* Output the primitive  */
#ifdef X11DRAW
    XgksDrawMarkers(dpy, win, gc, ype, npnt, mark_type, mark_size);
#endif
#ifdef CAIRODRAW
    CAIRODrawMarkers(cr,ype,npnt,mark_type,mark_size);
#endif


#ifdef X11WM
    XFlush(dpy);
#endif
#ifdef CAIRODRAW
    cairo_surface_flush((cairo_surface_t *)connect_id.surface);
#endif
    ufree((voidp)ype);
    
#ifdef X11WM
    (void) XgksSIGIO_ON(ws->dpy);
#endif
    return OK;
}


/*
 * Output polymarkers
 */
#ifdef X11DRAW
XgksDrawMarkers(dpy, win, gc, pe, n, type, s)
    Display        *dpy;
    Window          win;
    GC              gc;
    XPoint         *pe;
    int             n, type;
    float           s;
{
    XSegment        plus[2], star[3], cros[2];
    XSegment        xseg[5];
    int             i, j, x0, y0, s1;
    float           x, y;

    switch (type) {

    case GMK_POINT:
	s1 = (int) (2 * s);
	for (i = 0; i < n; i++) {
	    x0 = (int) (pe->x - s);
	    y0 = (int) (pe->y - s);
	    XFillArc(dpy, win, gc, x0, y0, s1, s1, 0, 23040);
	    XFlush(dpy);
	    pe++;
	}
	break;

    case GMK_PLUS:
	plus[0].x1 = -s;
	plus[0].y1 = 0;
	plus[0].x2 = s;
	plus[0].y2 = 0;
	plus[1].x1 = 0;
	plus[1].y1 = s;
	plus[1].x2 = 0;
	plus[1].y2 = -s;
	for (i = 0; i < n; i++) {
	    x = pe->x;
	    y = pe->y;
	    for (j = 0; j < 2; j++) {
		xseg[j].x1 = x + plus[j].x1;
		xseg[j].y1 = y + plus[j].y1;
		xseg[j].x2 = x + plus[j].x2;
		xseg[j].y2 = y + plus[j].y2;
	    }
	    XDrawSegments(dpy, win, gc, xseg, 2);
	    XFlush(dpy);
	    pe++;
	}
	break;

    case GMK_STAR:
	star[0].x1 = -s;
	star[0].y1 = 0;
	star[0].x2 = s;
	star[0].y2 = 0;
	star[1].x1 = s * 0.5;
	star[1].y1 = s * 0.866;
	star[1].x2 = -s * 0.5;
	star[1].y2 = -s * 0.866;
	star[2].x1 = s * 0.5;
	star[2].y1 = -s * 0.866;
	star[2].x2 = -s * 0.5;
	star[2].y2 = s * 0.866;
	for (i = 0; i < n; i++) {
	    x = pe->x;
	    y = pe->y;
	    for (j = 0; j < 3; j++) {
		xseg[j].x1 = x + star[j].x1;
		xseg[j].y1 = y + star[j].y1;
		xseg[j].x2 = x + star[j].x2;
		xseg[j].y2 = y + star[j].y2;
	    }
	    XDrawSegments(dpy, win, gc, xseg, 3);
	    XFlush(dpy);
	    pe++;
	}
	break;

    case GMK_O:
	s1 = (int) (2 * s);
	for (i = 0; i < n; i++) {
	    x0 = (int) (pe->x - s);
	    y0 = (int) (pe->y - s);
	    XDrawArc(dpy, win, gc, x0, y0, s1, s1, 0, 23040);
	    XFlush(dpy);
	    pe++;
	}
	break;

    case GMK_X:
	cros[0].x1 = 0.5 * s;
	cros[0].y1 = 0.866 * s;
	cros[0].x2 = -0.5 * s;
	cros[0].y2 = -0.866 * s;
	cros[1].x1 = 0.5 * s;
	cros[1].y1 = -0.866 * s;
	cros[1].x2 = -0.5 * s;
	cros[1].y2 = 0.866 * s;
	for (i = 0; i < n; i++) {
	    x = pe->x;
	    y = pe->y;
	    for (j = 0; j < 2; j++) {
		xseg[j].x1 = x + cros[j].x1;
		xseg[j].y1 = y + cros[j].y1;
		xseg[j].x2 = x + cros[j].x2;
		xseg[j].y2 = y + cros[j].y2;
	    }
	    XDrawSegments(dpy, win, gc, xseg, 2);
	    XFlush(dpy);
	    pe++;
	}
	break;

    default:
	break;
    }

    (void) XgksSIGIO_ON(dpy);
}
#endif
