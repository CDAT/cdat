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
 * XGKS cellarray primitive output
 * ws : the pointer to current workstation list
 * cell_ptr: the pointer to the output cellarray
 * primitive
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


xXgksCellarray(ws, cell_ptr)
    WS_STATE_PTR    ws;
    CELL_ARRAY_ST  *cell_ptr;
{
#ifdef X11OUT
    Display        *dpy;
    Window          win;
    GC              gc;
    XPoint          ll, lr, ur, ul, pol2[4];
#else
    Gpoint          ll, lr, ur, ul, pol2[4];
#endif
    Gpoint          pol[4];
    Gfloat          dx0, dy0, dx1, dy1;
    Gint            i, j, k, nx, clr, *cll, *clp, row;
    Gfloat          DX, DY;



    if (ws->ewstype != X_WIN)
	return OK;

    /* Initialization  */

#ifdef X11OUT
    (void) XgksSIGIO_OFF(ws->dpy);

    dpy = ws->dpy;
    win = ws->win;
    gc = ws->gc;
#endif

    /* display workstation transformation NDC to X_WIN */

    NdcToX(ws, &(cell_ptr->ll), &ll);
    NdcToX(ws, &(cell_ptr->lr), &lr);
    NdcToX(ws, &(cell_ptr->ur), &ur);
    NdcToX(ws, &(cell_ptr->ul), &ul);

    dx0 = (((Gfloat) (lr.x - ll.x)) / cell_ptr->dim.x);
    dy0 = (((Gfloat) (lr.y - ll.y)) / cell_ptr->dim.x);
    dx1 = (((Gfloat) (ul.x - ll.x)) / cell_ptr->dim.y);
    dy1 = (((Gfloat) (ul.y - ll.y)) / cell_ptr->dim.y);

    nx = cell_ptr->dim.x;

    /* get the memory for subset of the colour index array */

    cll = (Gint *) malloc((size_t) (nx * cell_ptr->dim.y * sizeof(int)));
    GKSERROR((cll == NULL), 300, errgcellarray);

    /* copy the values of the subset of colour index array */

    clp = cll;
    row = cell_ptr->rowsize;

    for (i = 0; i < cell_ptr->dim.y; i++)
	for (j = 0; j < row; j++)
	    if (j < nx) {
		*clp = cell_ptr->colour[i * row + j];
		clp++;
	    }
    clp = cll;

    /* set the clip area and fill area style */
#ifdef X11OUT
    XSetClipRectangles(dpy, gc, 0, 0, &(ws->xclip), 1, Unsorted);

    XSetFillStyle(dpy, gc, FillSolid);
#endif
    /* draw the cell array */

    DX = cell_ptr->dim.x * dx0;
    DY = cell_ptr->dim.x * dy0;

    pol[0].x = ll.x + DX - dx0 - dx1;
    pol[0].y = ll.y + DY - dy0 - dy1;

    for (i = 0; i < cell_ptr->dim.y; i++) {

	pol[0].x -= DX;
	pol[0].y -= DY;

	pol[0].x += dx1;
	pol[0].y += dy1;

	for (j = 0; j < cell_ptr->dim.x; j++) {

	    pol[0].x += dx0;
	    pol[0].y += dy0;

	    pol[1].x = pol[0].x + dx0;
	    pol[1].y = pol[0].y + dy0;

	    pol[2].x = pol[1].x + dx1;
	    pol[2].y = pol[1].y + dy1;

	    pol[3].x = pol[0].x + dx1;
	    pol[3].y = pol[0].y + dy1;

	    clr = *clp;
	    clp++;

	    if (!WS_AVAIL_COLOUR(ws, clr))
		clr = 1;
	    if (ws->wscolour == 2) {		/* monochrome ? */
		if (clr == 0)
		    clr = ws->wsbg;
		else if (clr == 1)
		    clr = ws->wsfg;
	    }
#ifdef X11OUT
	    XSetForeground(dpy, gc, XcPixelValue(ws, clr));
#endif
	    for (k = 0; k < 4; k++) {
		pol2[k].x = pol[k].x;
		pol2[k].y = pol[k].y;
	    }
#ifdef X11OUT
	    XFillPolygon(dpy, win, gc, &pol2[0], 4, Complex, CoordModeOrigin);
#endif
	}
    }

#ifdef X11OUT
    XFlush(dpy);
#endif
    ufree((voidp)cll);

#ifdef X11OUT
    (void) XgksSIGIO_ON(ws->dpy);
#endif

    return OK;
}
