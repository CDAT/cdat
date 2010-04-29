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
 * The following routines are just valid for Display window workstations
 *
 * xUpdateTrans(ws): update the constants for transformations from NDC to DC
 * and then from DC to WIN.  The DC space is assumed to be 1280(X) by 1024(Y).
 * Every time the user changes the workstation window, the worstation viewport,
 * or the window space, this routine will be called.
 *
 * xUpdateClip(): update the clipping area constants in the X window.
 * It is calculated from NDC clipping area to X window clipping area.
 * Every time the user selects a new NDC transformation this routine will be
 * called for each opened display workstation.
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include "gks_implem.h"

#ifndef lint
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif


xXgksUpdateTrans(ws)
    WS_STATE_PTR    ws;
{

    Gfloat          D_WWX, D_WWY, D_WVX, D_WVY,	/* size of worstation window
						 * and viewport */
                    D_DCX, D_DCY, D_XWX, D_XWY;	/* size of DC space and X
						 * window space    */

    Gfloat          scale_WT,		/* scale factor for WT
					 * transformation(NDC->DC) */
                    scale_DT;		/* scale factor for DT
					 * transformation(DC->WIN) */

    Gfloat          flt1, flt2;

    D_DCX = ws->size.x;
    D_DCY = ws->size.y;

    D_WWX = (Gfloat) (ws->wsti.current.w.xmax - ws->wsti.current.w.xmin);
    D_WWY = (Gfloat) (ws->wsti.current.w.ymax - ws->wsti.current.w.ymin);

    D_WVX = (Gfloat) (ws->wsti.current.v.xmax - ws->wsti.current.v.xmin);
    D_WVY = (Gfloat) (ws->wsti.current.v.ymax - ws->wsti.current.v.ymin);

    if ((flt1 = D_WVX / D_WWX) < (flt2 = D_WVY / D_WWY))
	scale_WT = flt1;
    else
	scale_WT = flt2;

    D_XWX = (Gfloat) (ws->wbound.x);
    D_XWY = (Gfloat) (ws->wbound.y);

    /* -1 is a fudge to fill window 
    if ((flt1 = D_XWX / (D_DCX)) < (flt2 = (D_XWY - 1) / (D_DCY)))
	scale_DT = flt1;
    else
	scale_DT = flt2;*/
    /* Scale according to the height only 9/30/94 */
    scale_DT = (D_XWY-1) / (D_DCY);

    /* NDC to DC transformation */
    ws->ndctodctrans.xScale = scale_WT;
    ws->ndctodctrans.yScale = scale_WT;
    ws->ndctodctrans.xTrans = ws->wsti.current.v.xmin -
			      ws->wsti.current.w.xmin * scale_WT;
    ws->ndctodctrans.yTrans = ws->wsti.current.v.ymin -
			      ws->wsti.current.w.ymin * scale_WT;

    /* DC to X transformation */
    ws->dctoxtrans.xScale = scale_DT;
    ws->dctoxtrans.yScale = scale_DT;
    ws->dctoxtrans.xTrans = 0.0;

    /* fudge to fill window */
    ws->dctoxtrans.yTrans = 1.0;

    /* NDC to X transformation */
    ws->ndctoxtrans.xScale = ws->ndctodctrans.xScale * ws->dctoxtrans.xScale;
    ws->ndctoxtrans.yScale = ws->ndctodctrans.yScale * ws->dctoxtrans.yScale;
    ws->ndctoxtrans.xTrans = ws->ndctodctrans.xTrans * ws->dctoxtrans.xScale +
			     ws->dctoxtrans.xTrans;
    ws->ndctoxtrans.yTrans = ws->ndctodctrans.yTrans * ws->dctoxtrans.yScale +
			     ws->dctoxtrans.yTrans;
}



/*
 * Note the rounding that occurs in the following due to the 0.5 additive
 * term.  This obtains the closest X pixel co-ordinate.  Also note that
 * the horizontal and vertical extents of the X clip window are computed
 * in X pixel-space from the transformed boundaries of the GKS clip window.
 * space.
 */
xXgksUpdateClip(ws)
    WS_STATE_PTR    ws;
{
    ws->xclip.x =
	(short) (ws->clip.xmin * ws->ndctoxtrans.xScale 
		 + ws->ndctoxtrans.xTrans + 0.5);
    ws->xclip.y =
	(short) (ws->wbound.y - (ws->clip.ymax * ws->ndctoxtrans.yScale 
				 + ws->ndctoxtrans.yTrans) + 0.5);

    ws->xclip.width = 
	(unsigned short) ((short) (ws->clip.xmax * ws->ndctoxtrans.xScale 
				   + ws->ndctoxtrans.xTrans + 0.5) 
			  - ws->xclip.x + 1);
    ws->xclip.height =
	(unsigned short) ((short) (ws->wbound.y - (ws->clip.ymin 
						   * ws->ndctoxtrans.yScale 
						   + ws->ndctoxtrans.yTrans)
				   + 0.5)
			  - ws->xclip.y + 1);
}

