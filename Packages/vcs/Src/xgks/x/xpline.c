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
 * Urbana, IL    61801
 *
 * (C) Copyright 1987, 1988 by The University of Illinois Board of Trustees.
 * All rights reserved.
 *
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 *
 * XGKS polyline primitive output
 * ws : the pointer to current workstation list
 * plin_ptr: the pointer to the output polyline primitive
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include "gks_implem.h"

#ifdef CAIRODRAW
#include "cairo/cairo.h"
#include "workstations.h"
extern Gconid_X_drawable connect_id;/*VCS canvas display and drawable id */
#ifndef USEX11
#include "Qt/Qt_X_emul.h"
#endif
#endif

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif

extern DashList xgksDASHES[];

#define	OUTCODE(pt, xmin, ymin, xmax, ymax) { \
	(pt).code.mask	= 0; \
	(pt).code.bits.is_above	= (ymax) - (pt).pos.y < 0; \
	(pt).code.bits.is_below	= (ymin) - (pt).pos.y > 0; \
	(pt).code.bits.is_right	= (xmax) - (pt).pos.x < 0; \
	(pt).code.bits.is_left	= (xmin) - (pt).pos.x > 0; \
    }

#define	IS_INSIDE(c)			((c).mask == 0)
#define	COMPLETELY_OUTSIDE(c1, c2)	((c1).mask & (c2).mask)
#define	COMPLETELY_INSIDE(c1, c2)	(IS_INSIDE(c1) & IS_INSIDE(c2))

typedef union {
    int             mask;
    struct {
	int             is_above:1;
	int             is_below:1;
	int             is_right:1;
	int             is_left:1;
    }               bits;
}               Outcode;

typedef struct {
    Gpoint          pos;
    Outcode         code;
}               CodedPoint;

typedef struct {
    CodedPoint      old;
    CodedPoint      new;
    int             clipped;
}               point;


/*
 *	Clip against the NDC display space.  Output one or more polylines.
 *
 *	Reference: "Fundamentals of Interactive Computer Graphics" (1983) by
 *	J.D. Foley and A. Van Dam; section 4.2.1, "Clipping Output Primitives".
 */
    static void
pclip(ws, gpoints, npts, xpoints)
    WS_STATE_PTR    ws;
    Gpoint         *gpoints;
    int             npts;
#ifdef X11OUT
    XPoint         *xpoints;
#else
    Gpoint         *xpoints;
#endif
{
    Gfloat          xmin = ws->clip.xmin, xmax = ws->clip.xmax;
    Gfloat          ymin = ws->clip.ymin, ymax = ws->clip.ymax;
    point           pt_a;
    point          *p1 = &pt_a;
    Gpoint         *gp;
    Gpoint         *lastpt = gpoints + npts - 1;
#ifdef X11OUT
    XPoint         *xp = xpoints;
#else
    Gpoint         *xp = xpoints;
#endif

    pt_a.old.pos = *gpoints;
    OUTCODE(pt_a.old, xmin, ymin, xmax, ymax);
    pt_a.new = pt_a.old;
    pt_a.clipped = 0;

    for (gp = gpoints + 1; gp < gpoints + npts; ++gp) {
	point           pt_b;
	point          *p2 = &pt_b;

	pt_b.old.pos = *gp;
	OUTCODE(pt_b.old, xmin, ymin, xmax, ymax);
	pt_b.new = pt_b.old;
	pt_b.clipped = 0;

	for (;;) {
	    if (COMPLETELY_OUTSIDE(p1->new.code, p2->new.code)) {
		pt_a = pt_b;
		break;
	    }
	    if (COMPLETELY_INSIDE(p1->new.code, p2->new.code)) {

		/* Add the first point to the output polyline.  */
		NdcToX(ws, &pt_a.new.pos, xp);	/* WARNING: macro */
		++xp;

		/*
		 * If the second point lies outside the clip box, or if it's
		 * the last point, then add it to the output polyline and
		 * then draw the polyline.  Setup for the next output
		 * polyline.
		 */
		if (pt_b.clipped || gp == lastpt) {
		    NdcToX(ws, &pt_b.new.pos, xp);	/* WARNING: macro */
#ifdef X11OUT
		    XDrawLines(ws->dpy, ws->win, ws->plinegc, xpoints,
			       xp - xpoints + 1, CoordModeOrigin);
#endif
		    xp = xpoints;
		    pt_a.old = pt_b.old;
		    pt_a.new = pt_b.old;
		    pt_a.clipped = 0;
		} else {
		    pt_a = pt_b;
		}

		break;
	    }
	    /*
	     * One of the points lies outside the window.  Insure that *p1 is
	     * that point.
	     */
	    if (IS_INSIDE(p1->new.code)) {
		point          *pt = p1;

		p1 = p2;
		p2 = pt;
	    }
	    /*
	     * Compute the co-ordinates of the intersection of the window
	     * with the line segment.
	     */
	    if (p1->new.code.bits.is_above) {
		p1->new.pos.x += (p2->new.pos.x - p1->new.pos.x) *
		    (ymax - p1->new.pos.y) /
		    (p2->new.pos.y - p1->new.pos.y);
		p1->new.pos.y = ymax;
	    } else if (p1->new.code.bits.is_below) {
		p1->new.pos.x += (p2->new.pos.x - p1->new.pos.x) *
		    (ymin - p1->new.pos.y) /
		    (p2->new.pos.y - p1->new.pos.y);
		p1->new.pos.y = ymin;
	    } else if (p1->new.code.bits.is_right) {
		p1->new.pos.y += (p2->new.pos.y - p1->new.pos.y) *
		    (xmax - p1->new.pos.x) /
		    (p2->new.pos.x - p1->new.pos.x);
		p1->new.pos.x = xmax;
	    } else if (p1->new.code.bits.is_left) {
		p1->new.pos.y += (p2->new.pos.y - p1->new.pos.y) *
		    (xmin - p1->new.pos.x) /
		    (p2->new.pos.x - p1->new.pos.x);
		p1->new.pos.x = xmin;
	    }
	    /*
	     * Indicate that *p1 has been clipped and set it's new out-code
	     */
	    p1->clipped = 1;
	    OUTCODE(p1->new, xmin, ymin, xmax, ymax);
	}					/* accept/reject loop */
    }						/* input points loop */
}


xXgksPolyLine(ws, plin_ptr)
    WS_STATE_PTR    ws;
    PLINE_ST       *plin_ptr;
{
    int             line_style;
    int             i, npnt;
    Gint            gi;
#ifdef X11DRAW
    GC              gc;
    XPoint         *xpe, *ype;
    Window          win;
    Display        *dpy;
#else
    Gpoint         *xpe, *ype;
    extern void VCS2CAIRO_setrgb(cairo_t *cr,int color);
#endif
    Gpoint         *pe;
    Glnattr        *ptr;
    Glnbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;
    unsigned int    line_width;

    if (ws->ewstype != X_WIN)
	return OK;

    /*
     * Initialization:
     */
#ifdef X11WM
    (void) XgksSIGIO_OFF(ws->dpy);
#endif
#ifdef X11DRAW
    dpy = ws->dpy;
    win = ws->win;
    gc = ws->plinegc;
    xpe = (XPoint *) malloc((size_t) (sizeof(plin_ptr->pts[0]) *
			    plin_ptr->num_pts));
#else
    xpe = (Gpoint *) malloc((size_t) (sizeof(plin_ptr->pts[0]) *
			    plin_ptr->num_pts));
#endif
    ype = xpe;
    /*
     * Set current GC values for the primitive.
     */

    ptr = &(plin_ptr->plnattr);
    gi = ptr->line;
    if (gi < 1 || gi >= MAX_BUNDL_TBL)
	gi = 1;
    idv_ptr = &(ptr->bundl);
    bdl_ptr = &(ws->lnbundl_table[gi]);

    if (ptr->colour == GBUNDLED) {		/* gc.foreground */
	bundl_ptr = bdl_ptr;
    } else {
	bundl_ptr = idv_ptr;
    }

    i = bundl_ptr->colour;
/*     fprintf(stderr,"ok we picked up color: %i and avail colours: %i\n",i,ws->wscolour); */
    if (!WS_AVAIL_COLOUR(ws, i))
	i = 1;
    if (ws->wscolour == 2) {			/* monochrome ? */
	if (i == 0) {
	    i = ws->wsbg;
	} else if (i == 1) {
	    i = ws->wsfg;
	}
    }
#ifdef X11DRAW
    xXgksSetForeground(dpy, gc, XcPixelValue(ws, i));
#endif
#ifdef CAIRODRAW
    /* ok let's make sure there is actually a cairto waiting.... */
    cairoXgksSetForeground(connect_id.cr, i);
#endif
    /*
     * Set the fill style attribute.
     */

    if (ptr->width == GBUNDLED) {		/* gc.line_width */
	line_width = bdl_ptr->width;		/* line width    */
    } else {
	line_width = idv_ptr->width;
    }

    if (ptr->type == GBUNDLED) {		/* gc.line_style */
	bundl_ptr = bdl_ptr;
    } else {					/* line type     */
	bundl_ptr = idv_ptr;
    }

    i = bundl_ptr->type;
    if (i == GLN_SOLID || !WS_LINE_TYPE(i)) {
#ifdef X11DRAW
	line_style = LineSolid;
#else
	line_style = GLN_SOLID;
#endif
#ifdef CAIRODRAW
	cairo_set_dash(connect_id.cr,0,0,0.);
#endif
	if (line_width == 1)
	    line_width = 0;
    } else {					/* set dashed line values */
	if (i < 0) {
	    i += 3;
	} else {
	    i += 1;
	}
#ifdef X11DRAW
	line_style = LineOnOffDash;
#endif

#ifdef X11DRAW
	xXgksSetDashes(dpy, gc, ws, i);
#endif
#ifdef CAIRODRAW
	cairoXgksSetDashes(connect_id.cr,i);
#endif
    }

#ifdef X11DRAW
    xXgksSetLineAttributes(dpy, gc, line_width, line_style, CapButt,
			   JoinMiter);
    xXgksSetFillStyle(dpy, gc, FillSolid);
    xXgksSetPlineClipRectangles(dpy, gc, ws, &(ws->xclip));
#endif
#ifdef CAIRODRAW
    cairoXgksSetLineAttributes(connect_id.cr, line_width, line_style, CapButt,
			   JoinMiter);
    cairoXgksSetFillStyle(connect_id.cr, FillSolid);
    cairoXgksSetPlineClipRectangles(connect_id.cr, ws, &(ws->xclip));
#endif    /*
     * Display Workstation Transformation:
     */

    pe = plin_ptr->pts;
    npnt = plin_ptr->num_pts;

    /* Convert the polyline to X co-ordinates and then display it. */
    if (ws->soft_clipping_on) {
	/*
	 * Soft clipping is enabled.  Clip against the workstation window
	 * while displaying.
	 */
	pclip(ws, pe, npnt, ype);

    } else {
	/* Convert to X co-ordinates. */
	for (i = 0; i < npnt; i++) {
	    NdcToX(ws, pe, xpe);		/* WARNING: macro */
	    ++xpe;
	    ++pe;
	}

	/* Display the primitive.  */
#ifdef X11DRAW
	XDrawLines(dpy, win, gc, ype, npnt, CoordModeOrigin);
#endif
#ifdef CAIRODRAW
	extern void CAIRODrawLines(cairo_t *cr,Gpoint *ype,int npt,int mode);
        CAIRODrawLines(connect_id.cr,ype,npnt,CoordModeOrigin);
#endif
    }

#ifdef X11WM
    XFlush(ws->dpy);
#endif
#ifdef CAIRODRAW
    cairo_surface_flush(connect_id.surface);
#endif
    ufree((voidp)ype);
#ifdef X11WM
    (void) XgksSIGIO_ON(ws->dpy);
#endif
    return OK;
}
