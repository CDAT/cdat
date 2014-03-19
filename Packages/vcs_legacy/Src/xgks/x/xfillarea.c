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
 * XGKS fillarea primitive output ws : the pointer to current workstation list
 * fill_ptr: the pointer to the output fillarea primitive
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
#ifdef CAIRODRAW
#include "cairo/cairo.h"
#include "workstations.h"
extern Gconid_X_drawable connect_id;/*VCS canvas display and drawable id */
#endif
#ifdef QTWM
#include "Qt/Qt_X_emul.h"
#endif


xXgksFillArea(WS_STATE_PTR    ws, FILL_AREA_ST   *fill_ptr)
{
#ifdef X11DRAW
    Display        *dpy;
    Window          win;
    GC              gc, stipgc;
    XGCValues       gcv;
    XPoint         *xpe, *ype, *ptt, *ptt1;
    Pixmap          pixm;
#else
    Gpoint         *xpe, *ype, *ptt, *ptt1;
#endif
#ifdef CAIRODRAW
    cairo_t *cr;
    cairo_pattern_t *pattern=NULL;
    cairo_surface_t *asurface=NULL;
#endif

    Gflinter        fill_inter;
    int             fill_style;
    int             fill_colour;

    Gflbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;
    Gpoint         *pe;
    Gint            gi, *array;
    Gflattr        *ptr;

    int             i, w, h, npnt, n, nn;
    int             px, py, prev, cur;

    if (ws->ewstype != X_WIN)
	return OK;

    /* Initialization  */

#ifdef X11WM
    (void) XgksSIGIO_OFF(ws->dpy);
#endif
#ifdef X11DRAW
    dpy = ws->dpy;
    win = ws->win;
    gc = ws->fillareagc;
    xpe = (XPoint *) malloc((size_t) (sizeof(fill_ptr->pts[0]) * 
			    (fill_ptr->num_pts + 1)));
#else
    xpe = (Gpoint *) malloc((size_t) (sizeof(fill_ptr->pts[0]) * 
			    (fill_ptr->num_pts + 1)));
#endif
#ifdef CAIRODRAW
    cr = connect_id.cr;
#endif
    ype = xpe;

    GKSERROR((xpe == NULL), 300, errxFillArea);

    /* Set current GC values for the primitive  */

    ptr = &(fill_ptr->flattr);
    gi = ptr->fill;
    if (gi < 1 || gi >= MAX_BUNDL_TBL)
	gi = 1;
    idv_ptr = &(ptr->bundl);
    bdl_ptr = &(ws->flbundl_table[gi]);

    /* the colour attribute */

    if (ptr->colour == GBUNDLED)
	bundl_ptr = bdl_ptr;
    else
	bundl_ptr = idv_ptr;

    fill_colour = bundl_ptr->colour;
    if (!WS_AVAIL_COLOUR(ws, fill_colour))
	fill_colour = 1;
    if (ws->wscolour == 2) {			/* monochrome ? */
	if (fill_colour == 0)
	    fill_colour = ws->wsbg;
	else if (fill_colour == 1)
	    fill_colour = ws->wsfg;
    }
#ifdef X11DRAW
    xXgksSetForeground(dpy, gc, XcPixelValue(ws, fill_colour));
#endif
#ifdef CAIRODRAW
    cairoXgksSetForeground(cr, fill_colour);
#endif
    /* the fill interior attribute */

    if (ptr->inter == GBUNDLED)
	bundl_ptr = bdl_ptr;
    else
	bundl_ptr = idv_ptr;

    fill_inter = bundl_ptr->inter;

    /* the fill style attribute */

    if (ptr->style == GBUNDLED)
	fill_style = bdl_ptr->style;
    else
	fill_style = idv_ptr->style;

    if (WS_FILL_TYPE(fill_inter, fill_style))
	if (fill_inter == GHATCH)
	    fill_style += 1;
	else
	    fill_style -= 1;
    else if ((fill_inter == GHATCH) && (fill_style > 0)) /* DNW 7/19/01 */
	fill_style = -fill_style + 1;
    else
	fill_style = 0;

    /* set GC values */

#ifdef X11DRAW
    xXgksSetLineAttributes(dpy, gc, 0, LineSolid, CapButt, JoinMiter);
    xXgksSetFillStyle(dpy, gc, FillSolid);
#endif
#ifdef CAIRODRAW
    cairoXgksSetLineAttributes(cr, 0, LineSolid, CapButt, JoinMiter);
    cairoXgksSetFillStyle(cr, FillSolid);
#endif
    /* Display Workstation Transformation */

    pe = fill_ptr->pts;
    npnt = fill_ptr->num_pts;

    for (i = 0; i < npnt; i++) {
	NdcToX(ws, pe, xpe);
	++xpe;
	++pe;
    }

    if (fill_ptr->pts[0].x != fill_ptr->pts[npnt - 1].x ||
	    fill_ptr->pts[0].y != fill_ptr->pts[npnt - 1].y) {
	xpe->x = ype->x;
	xpe->y = ype->y;
	npnt += 1;
    }
    /* Output the primitive  */

    if (fill_inter == GHOLLOW) {
#ifdef X11DRAW
	xXgksSetFillAreaClipRectangles(dpy, gc, ws, &(ws->xclip));
	XDrawLines(dpy, win, gc, ype, npnt, CoordModeOrigin);
#endif
#ifdef CAIRODRAW
	cairoXgksSetFillAreaClipRectangles(cr, ws, &(ws->xclip));
	CAIRODrawLines(cr, ype, npnt, CoordModeOrigin);
#endif
    } else if (fill_inter == GSOLID) {
#ifdef X11DRAW
	xXgksSetFillAreaClipRectangles(dpy, gc, ws, &(ws->xclip));
	XFillPolygon(dpy, win, gc, ype, npnt, Complex, CoordModeOrigin);
#endif
#ifdef CAIRODRAW
	cairoXgksSetFillAreaClipRectangles(cr, ws, &(ws->xclip));
	CAIROFillPolygon(cr, ype, npnt, Complex, CoordModeOrigin);
#endif
    } else if (fill_inter == GPATTERN) {
#ifdef X11DRAW
	XSetClipMask(dpy, gc, None);
	pixm = XCreatePixmap(dpy, win,
			     ws->ptbundl_table[fill_style].size.x,
			     ws->ptbundl_table[fill_style].size.y,
			     DefaultDepth(dpy, DefaultScreen(dpy)));
#endif

	w = ws->ptbundl_table[fill_style].size.x;
	h = ws->ptbundl_table[fill_style].size.y;
	nn = w * h;
	array = ws->ptbundl_table[fill_style].array;

	/* build the pixmap to tile with */
#ifdef X11DRAW
	ptt1 = (XPoint *) malloc((size_t) (nn * sizeof(XPoint)));
	GKSERROR((ptt1 == NULL), 300, errxFillArea);

	n = 0;
	ptt = ptt1;				/* count & array of same
						 * colour cells */
	prev = *array;				/* previous colour */
	if (!WS_AVAIL_COLOUR(ws, prev))
	    prev = 1;
	if (ws->wscolour == 2) {		/* monochrome ? */
	    if (prev == 0)
		prev = ws->wsfg;
	    else if (prev == 1)
		prev = ws->wsbg;
	}
	prev=1-prev;
	for (px = 0; px < w; px++) {
	  for (py = 0; py < h; py++) {
		cur = 1-*array;
		if (!WS_AVAIL_COLOUR(ws, cur))
		    cur = 1;
		if (ws->wscolour == 2) {	/* monochrome ? */
		    if (cur == 0)
			cur = ws->wsfg;
		    else if (cur == 1)
			cur = ws->wsbg;
		}
		if (cur != prev) {		/* colour change */

		    xXgksSetForeground(dpy, gc,
				       XcPixelValue(ws, prev));
		    XDrawPoints(dpy, pixm, gc, ptt1, n, CoordModeOrigin);
		    n = 0;
		    ptt = ptt1;			/* reset */
		    prev = cur;
		}
		ptt->x = px;
		ptt->y = py;
		ptt++;
		n++;
		array++;
	    }
	}
	xXgksSetForeground(dpy, gc, XcPixelValue(ws, prev));
	XDrawPoints(dpy, pixm, gc, ptt1, n, CoordModeOrigin);

	/* load the tile and draw the fill area */
	xXgksSetFillStyle(dpy, gc, FillTiled);
	xXgksSetTile(dpy, gc, pixm);

	xXgksSetFillAreaClipRectangles(dpy, gc, ws, &(ws->xclip));

	XFillPolygon(dpy, win, gc, ype, npnt, Complex, CoordModeOrigin);

	XFreePixmap(dpy, pixm);
	ufree((voidp) ptt1);
#endif
#ifdef CAIRODRAW
	cairo_set_source_rgb(cr,0,0,0);
	pattern =  CAIROcreatePattern(cr,fill_style+1,0);
	cairo_set_source(cr,pattern);
	CAIROFillPolygon(cr, ype, npnt, Complex, CoordModeOrigin);
#endif
    } else if (fill_inter == GHATCH) {
	/*
	 * Hatched fill areas have the fill area colour used for every 
	 * non-zero cell of the pattern array.  The zero cells are
	 * transparent. X maps the ones in the stipple bitmap to the fill
	 * colour (Foreground) and ignores the zeros.
	 */
#ifdef X11DRAW
	pixm = XCreatePixmap(dpy, win,
			     ws->ptbundl_table[-fill_style].size.x,
			     ws->ptbundl_table[-fill_style].size.y,
			     1);
	gcv.foreground = 1;
	gcv.background = 0;
	stipgc = XCreateGC(dpy, pixm,
			 (unsigned long) GCForeground | GCBackground, &gcv);
	XSetClipMask(dpy, stipgc, None);
	w = ws->ptbundl_table[-fill_style].size.x;
	h = ws->ptbundl_table[-fill_style].size.y;
	nn = w * h;
	array = ws->ptbundl_table[-fill_style].array;

	/* build the pixmap to stipple with */
	ptt1 = (XPoint *) malloc((size_t) (nn * sizeof(XPoint)));
	GKSERROR((ptt1 == NULL), 300, errxFillArea);

	n = 0;
	ptt = ptt1;				/* count & array of same
						 * colour cells */
	prev = *array;				/* previous colour */
	if (!WS_AVAIL_COLOUR(ws, prev))
	    prev = 1;
	if (prev != 0)
	    prev = 1;
	for (px = 0; px < w; px++) {
	    for (py = 0; py < h; py++) {
		cur = *array;
		if (!WS_AVAIL_COLOUR(ws, cur))
		    cur = 1;
		if (cur != 0)
		    cur = 1;
		if (cur != prev) {		/* colour change */
		    xXgksSetForeground(dpy, stipgc,
				       XcPixelValue(ws, prev));
		    XDrawPoints(dpy, pixm, stipgc, ptt1, n, CoordModeOrigin);
		    n = 0;
		    ptt = ptt1;			/* reset */
		    prev = cur;
		}
		ptt->x = px;
		ptt->y = py;
		ptt++;
		n++;
		array++;
	    }
	}
	xXgksSetForeground(dpy, stipgc, XcPixelValue(ws, prev));
	XDrawPoints(dpy, pixm, stipgc, ptt1, n, CoordModeOrigin);

	/* load the tile and draw the fill area */
	xXgksSetForeground(dpy, gc, XcPixelValue(ws, fill_colour));
	xXgksSetFillStyle(dpy, gc, FillStippled);
	xXgksSetStipple(dpy, gc, pixm);

	xXgksSetFillAreaClipRectangles(dpy, gc, ws, &(ws->xclip));

	XFillPolygon(dpy, win, gc, ype, npnt, Complex, CoordModeOrigin);
	XFreeGC(dpy, stipgc);
	XFreePixmap(dpy, pixm);

	ufree((voidp) ptt1);
#endif
#ifdef CAIRODRAW
	cairoXgksSetForeground(cr,fill_colour);
	fprintf(stderr,"Ok first of all pattenr is: %p, and cr is: %p, index:%i\n",pattern,cr,-fill_style+1);
	pattern =  CAIROcreatePattern(cr,-fill_style+1,1);
	fprintf(stderr,"CAIRO STATUS: %s",cairo_status_to_string(cairo_status(cr)));
	cairo_set_source(cr,pattern);
	CAIROFillPolygon(cr, ype, npnt, Complex, CoordModeOrigin);
	cairo_pattern_get_surface(pattern,&asurface);
	cairo_surface_destroy(asurface);
#endif	
    }
#ifdef X11WM
    XFlush(ws->dpy);
#endif
#ifdef CAIRODRAW
    cairo_surface_flush(connect_id.surface);
#endif
    ufree((voidp)ype);
#ifdef X11DRAW
    (void) XgksSIGIO_ON(dpy);
#endif

    return OK;
}
