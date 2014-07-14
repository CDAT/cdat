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
 * transforms.c - functions and data for gks transformations
 *			gsetwindow()
 *			gsetviewport()
 *			gsetviewportinputpri()
 *			gselntran()
 *			gsetclip()
 *			gsetwswindow()
 *			gsetwsviewport()
 *			gevaltran()
 *			gaccumtran()
 *
 * utility functions: 	XgksComputeTrans()
 *			XgksFindNTrans()
 *			XgksFindNTransNpts()
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include <math.h>
#include "gks_implem.h"

#ifndef lint
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif


/*
 * XgksComputeTrans(trans) - computes the transformation matrix values from
 *	the wc window and wc viewport.
 *
 * int trans;			transformation number to compute.
 */
    static void
XgksComputeTrans(trans)
    Gint            trans;
{
    NORM_TRANS     *nt;

    nt = &(xgks_state.ntrans_list[trans]);

    /* compute the wc to ndc matrix values (sx,tx,sy,ty) */
    nt->wc_to_ndc.xmin = (nt->ntrans.v.xmax - nt->ntrans.v.xmin)
	/ (nt->ntrans.w.xmax - nt->ntrans.w.xmin);
    nt->wc_to_ndc.xmax = nt->ntrans.v.xmin - nt->wc_to_ndc.xmin
	* nt->ntrans.w.xmin;
    nt->wc_to_ndc.ymin = (nt->ntrans.v.ymax - nt->ntrans.v.ymin)
	/ (nt->ntrans.w.ymax - nt->ntrans.w.ymin);
    nt->wc_to_ndc.ymax = nt->ntrans.v.ymin - nt->wc_to_ndc.ymin
	* nt->ntrans.w.ymin;
}


    static void
XgksSetClip(ind)
    Gclip           ind;
{
    if (ind == GCLIP) {
	xgks_state.cliprec.rec.xmin =
	    xgks_state.ntrans_list[xgks_state.cur_ntrans].ntrans.v.xmin;
	xgks_state.cliprec.rec.xmax =
	    xgks_state.ntrans_list[xgks_state.cur_ntrans].ntrans.v.xmax;
	xgks_state.cliprec.rec.ymin =
	    xgks_state.ntrans_list[xgks_state.cur_ntrans].ntrans.v.ymin;
	xgks_state.cliprec.rec.ymax =
	    xgks_state.ntrans_list[xgks_state.cur_ntrans].ntrans.v.ymax;
    } else {
	xgks_state.cliprec.rec.xmin = xgks_state.cliprec.rec.ymin = 0.0;
	xgks_state.cliprec.rec.xmax = xgks_state.cliprec.rec.ymax = 1.0;
    }

    XgksProcessClip(&(xgks_state.cliprec.rec));
}


    static void
XgksSetWsWindow(ws, window)
    WS_STATE_PTR    ws;
    Glimit         *window;
{
    ws->wsti.request.w.xmin = window->xmin;
    ws->wsti.request.w.xmax = window->xmax;
    ws->wsti.request.w.ymin = window->ymin;
    ws->wsti.request.w.ymax = window->ymax;
    ws->wsti.wstus = GPENDING;
    if (!(ws->wsdus.irgmode == GSUPPRESSED && ws->wsdus.dspsurf == GNOTEMPTY))
	XgksUnpendPendingTrans(ws);

    /* if irgmode is suppressed, REDRAWWS() will set appropreate flags */
    REDRAWWS(ws);
}


    static void
XgksSetWsViewport(ws, viewport)
    WS_STATE_PTR    ws;
    Glimit         *viewport;
{
    ws->wsti.request.v.xmax = viewport->xmax;
    ws->wsti.request.v.xmin = viewport->xmin;
    ws->wsti.request.v.ymax = viewport->ymax;
    ws->wsti.request.v.ymin = viewport->ymin;
    ws->wsti.wstus = GPENDING;
    if (!(ws->wsdus.irgmode == GSUPPRESSED && ws->wsdus.dspsurf == GNOTEMPTY))
	XgksUnpendPendingTrans(ws);

    /* if irgmode is suppressed, REDRAWWS() will set appropreate flags */
    REDRAWWS(ws);
}


/*
 * XgksInitGksTrans -- initialise gks state list transformation stuff
 */
XgksInitGksTrans()
{
    Gint            i;			/* Loop counter */
    Glimit          tmp;		/* temporary limit variable */

    xgks_state.cur_ntrans = 0;			/* current normalization
						 * transformation */
    xgks_state.cliprec.ind = GCLIP;		/* clip at window and
						 * viewport */
    xgks_state.cliprec.rec.xmin = 0.0;
    xgks_state.cliprec.rec.xmax = 1.0;
    xgks_state.cliprec.rec.ymin = 0.0;
    xgks_state.cliprec.rec.ymax = 1.0;

    for (i = 0; i <= MAX_TRANS; i++)
	xgks_state.ntrans_priority[i] = i;

    xgks_state.ntrans_list[0].ntrans.w.xmin = 0.0;	/* window */
    xgks_state.ntrans_list[0].ntrans.w.xmax = 1.0;
    xgks_state.ntrans_list[0].ntrans.w.ymin = 0.0;
    xgks_state.ntrans_list[0].ntrans.w.ymax = 1.0;
    xgks_state.ntrans_list[0].ntrans.v.xmin = 0.0;	/* viewport */
    xgks_state.ntrans_list[0].ntrans.v.xmax = 1.0;
    xgks_state.ntrans_list[0].ntrans.v.ymin = 0.0;
    xgks_state.ntrans_list[0].ntrans.v.ymax = 1.0;
    xgks_state.ntrans_list[0].wc_to_ndc.xmax = 0.0;	/* sx */
    xgks_state.ntrans_list[0].wc_to_ndc.xmin = 1.0;	/* tx */
    xgks_state.ntrans_list[0].wc_to_ndc.ymax = 0.0;	/* sx */
    xgks_state.ntrans_list[0].wc_to_ndc.ymin = 1.0;	/* tx */

    /* now use normal methods to initialise others */
    tmp.xmin = 0.0;
    tmp.xmax = 1.0;
    tmp.ymin = 0.0;
    tmp.ymax = 1.0;
    for (i = 1; i <= MAX_TRANS; i++) {
	(void) gsetwindow(i, &(tmp));
	(void) gsetviewport(i, &(tmp));

	/* lower than privious */
	(void) gsetviewportinputpri(i, i - 1, GLOWER);
    }
}


/*
 * XgksInitWssTrans(ws) -- initialize workstation transformation stuff
 *  WS_STATE_PTR ws;
 */
XgksInitWssTrans(ws)
    WS_STATE_PTR    ws;
{
    ws->wsdus.defmode = GASAP;
    ws->wsdus.dspsurf = GEMPTY;
    ws->wsdus.irgmode = GALLOWED;
    ws->wsdus.nframe = GNO;
    ws->wsti.wstus = GNOTPENDING;
    ws->wsti.current.w.xmin = 0.0;
    ws->wsti.current.w.xmax = 1.0;
    ws->wsti.current.w.ymin = 0.0;
    ws->wsti.current.w.ymax = 1.0;
    ws->wsti.request.w.xmin = 0.0;
    ws->wsti.request.w.xmax = 1.0;
    ws->wsti.request.w.ymin = 0.0;
    ws->wsti.request.w.ymax = 1.0;

    ws->wsti.current.v.xmin = 0.0;		/* Vieport defalut should be
						 * set to DC MAX !! ?? */
    ws->wsti.current.v.xmax = ws->size.x;
    ws->wsti.current.v.ymin = 0.0;
    ws->wsti.current.v.ymax = ws->size.y;
    ws->wsti.request.v.xmin = 0.0;
    ws->wsti.request.v.xmax = ws->size.x;
    ws->wsti.request.v.ymin = 0.0;
    ws->wsti.request.v.ymax = ws->size.y;

    if (ws->ewstype != MO) {
	XgksSetWsWindow(ws, &(ws->wsti.current.w));
	XgksSetWsViewport(ws, &(ws->wsti.current.v));
    }
}


/*
 * gsetwindow(trans, window); - SET WINDOW
 *
 * Gint    trans;		normalization transformation to change
 * Glimit *window;		new window bounds in WC
 *
 * returns: 0=OK, or one of 8, 50, 51
 *
 * See also: ANSI Standard p.107
 */
gsetwindow(trans, window)
    Gint            trans;
    Glimit         *window;
{
    /* check gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgsetwindow);

    /* check for valid transformation number */
    GKSERROR((trans < 1 || trans > MAX_TRANS), 50, errgsetwindow);

    /* check valid mins and maxs */
    /* DNW - This causes and error. It returns garbage. */
/*    GKSERROR((window->xmin >= window->xmax || window->ymin >= window->ymax),
	     51, errgsetwindow);*/

    /* OK to change the window! */
    xgks_state.ntrans_list[trans].ntrans.w.xmax = window->xmax;
    xgks_state.ntrans_list[trans].ntrans.w.xmin = window->xmin;
    xgks_state.ntrans_list[trans].ntrans.w.ymax = window->ymax;
    xgks_state.ntrans_list[trans].ntrans.w.ymin = window->ymin;

    /*
     * now compute the new values for the wc to ndc transform and clip
     * rectangle
     */
    XgksComputeTrans(trans);

    if (MO_OPENED == TRUE) {
	XgksMoSetPatRef();
	XgksMoSetPatSize();
	XgksMoSetCharUp();
    }
    if (trans == xgks_state.cur_ntrans)
	XgksSetClip(xgks_state.cliprec.ind);

    return OK;
}


/*
 * gsetviewport(trans,viewport) - SET VIEWPORT
 *
 * Gint trans;			which normalization transformation
 * Glimit *viewport; 		viewport bounds in NDC space.
 *
 * returns 0 = OK, or one of 8, 50, 51, 52
 *
 * See also: ANSI Standard p.107
 */
gsetviewport(trans, viewport)
    Gint            trans;
    Glimit         *viewport;
{
    /* check gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgsetviewport);

    /* check for valid transformation number */
    GKSERROR((trans < 1 || trans > MAX_TRANS), 50, errgsetviewport);

    /* check valid mins and maxs */
    GKSERROR((viewport->xmin >= viewport->xmax
	      || viewport->ymin >= viewport->ymax), 51, errgsetviewport);

    /* check all within 0..1 */
    GKSERROR((viewport->xmin < 0.0 || viewport->xmin > 1.0
	      || viewport->xmax < 0.0 || viewport->xmax > 1.0
	      || viewport->ymin < 0.0 || viewport->ymin > 1.0
	      || viewport->ymax < 0.0 || viewport->ymax > 1.0),
	     52, errgsetviewport);

    /* ok to change the viewport */
    xgks_state.ntrans_list[trans].ntrans.v.xmax = viewport->xmax;
    xgks_state.ntrans_list[trans].ntrans.v.xmin = viewport->xmin;
    xgks_state.ntrans_list[trans].ntrans.v.ymax = viewport->ymax;
    xgks_state.ntrans_list[trans].ntrans.v.ymin = viewport->ymin;

    /* now compute the new values for the wc to ndc transform and clip */
    XgksComputeTrans(trans);

    if (trans == xgks_state.cur_ntrans)
	XgksSetClip(xgks_state.cliprec.ind);

    if (MO_OPENED == TRUE) {
	XgksMoSetPatRef();
	XgksMoSetPatSize();
	XgksMoSetCharUp();
/*			RLM replaced Dec 16, 1994.			*/
/*	XgksMoSetClip(&xgks_state.cliprec.rec);				*/
	XgksMoSetClip(&xgks_state.cliprec);
    }
    return OK;
}


/*
 * gsetviewportinputpri(trans, ref_trans, priority) - 
 *			SET VIEWPORT INPUT PRIORITY
 *
 * Gint	 trans,			transformation to set the priority on.
 *	 ref_trans;		set it relative to ref_trans.
 * Gvpri priority;		set it GHIGHER | GLOWER
 *
 * returns 0=OK, or one of 8, 50
 *
 * See also: ANSI Standard p.107
 */
gsetviewportinputpri(trans, ref_trans, priority)
    Gint            trans, ref_trans;
    Gvpri           priority;
{
    Gint            i, j;

    /* check gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgsetviewportinputpri);

    /* check for valid transformation number */
    GKSERROR((trans > MAX_TRANS || trans < 0),
	     50, errgsetviewportinputpri);

    /* check for invalid priority value */
    GKSERROR((priority != GHIGHER && priority != GLOWER), 2000, 
	     errgsetviewportinputpri);

    if (trans != ref_trans) {
	/* first find & remove trans from the priority list */
	for (i = 0; xgks_state.ntrans_priority[i] != trans; i++)
	    continue;
	while (i < MAX_TRANS) {
	    xgks_state.ntrans_priority[i] = xgks_state.ntrans_priority[i + 1];
	    i++;
	}

	/* just to be safe in the next step */
	xgks_state.ntrans_priority[MAX_TRANS] = ref_trans;

	/* now find the reference transformation in the priority list */
	for (i = 0; xgks_state.ntrans_priority[i] != ref_trans; i++)
	    continue;
	/* insert trans back into the list */
	if (priority == GLOWER)
	    i++;
	for (j = MAX_TRANS; i < j; j--)
	    xgks_state.ntrans_priority[j] = xgks_state.ntrans_priority[j - 1];
	xgks_state.ntrans_priority[i] = trans;
    }
    return OK;
}


/*
 * gselntran(trans) - SELECT NORMALIZATION TRANSFORMATION
 *
 * Gint trans;		new current normalization transformation
 *
 * returns: 0=OK, 8, 50
 *
 * See also: ANSI Standard p.108
 */
gselntran(trans)
    Gint            trans;
{
    /* check gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgselntran);

    /* check for valid transformation number */
    GKSERROR((trans > MAX_TRANS || trans < 0), 50, errgselntran);

    /* change the current normalization */
    xgks_state.cur_ntrans = trans;
    XgksSetClip(xgks_state.cliprec.ind);

    if (MO_OPENED == TRUE) {
	XgksMoSetPatRef();
	XgksMoSetPatSize();
	XgksMoSetCharUp();
/*			RLM replaced Dec 16, 1994.			*/
/*	XgksMoSetClip(&xgks_state.cliprec.rec);				*/
	XgksMoSetClip(&xgks_state.cliprec);
    }
    return OK;
}


/*
 * gsetclip(ind)	- SET CLIPPING INDICATOR
 *
 * Gclip ind;		set viewport clipping to GCLIP | GNOCLIP
 *
 * returns: 0=OK, or 8, 2000
 *
 * See also: ANSI Standard p.108
 */
gsetclip(ind)
    Gclip           ind;
{
    /* check gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgsetclip);

    /* Check for valid enumeration type */
    GKSERROR((ind != GCLIP && ind != GNOCLIP), 2000, errgsetclip);

    /* set it and change the clipping rectangle */
    xgks_state.cliprec.ind = ind;

    XgksSetClip(ind);

    if (MO_OPENED == TRUE)
/*			RLM replaced Dec 16, 1994.			*/
/*	XgksMoSetClip(&(xgks_state.cliprec.rec));			*/
	XgksMoSetClip(&(xgks_state.cliprec));

    return OK;
}


/*
 * gsetwswindow(ws_id, window) - SET WORKSTATION WINDOW
 *
 * Gint ws_id;			workstation identifier.
 * Glimit *window; 		new window bounds.
 *
 * returns: 0=OK, or one of 7, 20, 25, 33, 36, 51, 53
 *
 * See also: ANSI Standard p.109
 */
gsetwswindow(ws_id, window)
    Gint            ws_id;
    Glimit         *window;
{
    WS_STATE_PTR    ws;

    /* step 1: check for errors */
    /* check gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP),
	     7, errgsetwswindow);

    /* check for invalid workstation id */
    /* DWO 7/28/88  added check to differentiate between */
    /* error 20 and error 25 for PTR c1012  */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgsetwswindow);

    /* check for open ws_id */
    /* DWO 7/26/88  changed macro name from VALID_WSID */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgsetwswindow);

    GKSERROR((WS_CAT(ws) == GMI), 33, errgsetwswindow);

    GKSERROR((WS_CAT(ws) == GWISS), 36, errgsetwswindow);

    /* check valid mins and maxs */
    GKSERROR((window->xmin >= window->xmax || window->ymin >= window->ymax),
	     51, errgsetwswindow);

    /* check all within 0..1 */
    GKSERROR((window->xmin < 0.0 || window->xmin > 1.0
	      || window->xmax < 0.0 || window->xmax > 1.0
	      || window->ymin < 0.0 || window->ymin > 1.0
	      || window->ymax < 0.0 || window->ymax > 1.0),
	     53, errgsetwswindow);

    if (ws->ewstype != MO)
	XgksSetWsWindow(ws, window);

    if (ws->ewstype == MO)
	XgksMoSetLimit(ws, 71, window);

    return OK;
}


/*
 * gsetwsviewport(ws_id, viewport) - SET WORKSTATION VIEWPORT
 *
 * Gint ws_id;				workstation identifier.
 * Glimit *viewport;		 	new viewport bounds.
 *
 * returns: 0=OK, or one of 7, 20, 25, 33, 36, 51, 54
 *
 * See also: ANSI Standard p.109
 *
 * Note : in XGKS workstation viewport is mapped onto X-window, so this
 * reoutine is always ignored in the reality. Although all error checking are
 * done. Because the fact that this routine is always ignored, <request.v> and
 * <current.v> are always the same.
 */

gsetwsviewport(ws_id, viewport)
    Gint            ws_id;
    Glimit         *viewport;
{
    WS_STATE_PTR    ws;

    /* STEP 1: check for errors */
    /* check gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP),
	     7, errgsetwsviewport);

    /* check for invalid workstation id */
    /* DWO 7/28/88  added check to differentiate between */
    /* error 20 and error 25 for PTR c1012  */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgsetwsviewport);

    /* check for open ws_id */
    /* DWO 7/26/88  changed macro name from VALID_WSID */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgsetwsviewport);

    /* check for proper ws category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgsetwsviewport);

    GKSERROR((WS_CAT(ws) == GWISS), 36, errgsetwsviewport);

    /* check valid mins and maxs */
    GKSERROR((viewport->xmin >= viewport->xmax
	      || viewport->ymin >= viewport->ymax), 51, errgsetwsviewport);

    /* check all within display space */
    GKSERROR((viewport->xmin < 0.0 || viewport->xmin > ws->size.x
	      || viewport->xmax < 0.0 || viewport->xmax > ws->size.x
	      || viewport->ymin < 0.0 || viewport->ymin > ws->size.y
	      || viewport->ymax < 0.0 || viewport->ymax > ws->size.y),
	     54, errgsetwsviewport);

    if (ws->ewstype != MO)
	XgksSetWsViewport(ws, viewport);

    if (ws->ewstype == MO)
	XgksMoSetLimit(ws, 72, viewport);

    return OK;
}


/*
 * XgksUnpendPendingTrans(ws) - make any pending transformation changes
 *	current.
 *  WS_STATE_PTR ws;
 */
XgksUnpendPendingTrans(ws)
    WS_STATE_PTR    ws;
{
    /* step 1: check for nothing to do, return if so */
    if (ws->wsti.wstus == GNOTPENDING)
	return 0;

    /* step 2: move the pending transformation to the current transformation. */
    ws->wsti.current.w = ws->wsti.request.w;
    ws->wsti.current.v = ws->wsti.request.v;

    ws->wsti.wstus = GNOTPENDING;		/* reset the PENDING status
						 * and ... we're done */

    /*
     * step 3: compute the ndc_to_dc transformation.
     * this is part is pushed down to xlib.
     */
    xXgksUpdateTrans(ws);
    XgksUpdateWsClip(ws, &(xgks_state.cliprec.rec));

    return 0;
}


/*
 * find_ntrans - utility function to find the normalization transformation
 *	and world coordinate point associated with the viewport that
 *      ndcpt appears in, taking into account the viewport priority.
 *
 * Gpoint *ndcpt;
 * Gloc *gloc;
 *
 * algorithm: for each ntrans in the priority list apply the reverse 
 *	      transformation if the wcpt is within the window then FOUND IT!!
 *
 * See Also: ANSI Standard p.58
 */
#ifndef Bool
#include <stdbool.h>
#define Bool bool
#endif
    Bool
XgksFindNTrans(ndcpt, gloc)
    Gpoint         *ndcpt;
    Gloc           *gloc;
{
    int             pl;
    Glimit         *win;

#define trans	gloc->transform
#define wcpt	gloc->position

    /* run down the priority list */
    for (pl = 0; pl <= MAX_TRANS; pl++) {
	trans = xgks_state.ntrans_priority[pl];	/* get trans. of prio. pl */
	NtNdcToWc(trans, ndcpt, &wcpt);		/* convert point with trans */

	/* get limits of trans.  */
	win = &xgks_state.ntrans_list[trans].ntrans.w;

	/* if within limits of transform window... */
	if (wcpt.x >= win->xmin && wcpt.y >= win->ymin
		&& wcpt.x <= win->xmax && wcpt.y <= win->ymax)
	    break;				/* FOUND IT!!! */
    }
    if (pl > MAX_TRANS) {
	/* this should not happen because trans 0 covers full NDC      */
	/* space - so, if it does happen, there are bogus points in    */
	/* the list.  (from bogus initial points, altered data, or     */
	/* transforms were changed since point data was created. (DWO) */
	trans = 0;
	NtNdcToWc(trans, ndcpt, &wcpt);
	return -1;
    } else {
	return 0;
    }
#undef trans
#undef wcpt
}


/*
 * find_ntrans_npts - utility function to find the normalization transfomation
 *	and world coordinates associated with the viewport that ALL ndcpts
 *	appears in, taking account for the veiwport priority.
 */
XgksFindNTransNpts(num, ndcpts, ntrans, wcpts)
    int             num;		/* Number of points */
    Gpoint         *ndcpts;		/* those points in NDC space */
    Gint           *ntrans;		/* OUTPUT the normalization
					 * transformation */
    Gpoint         *wcpts;		/* OUTPUT the points in WC space */
{
    int             pl, cnt;		/* index into pri. list, and point
					 * counter */
    Glimit         *win;		/* limits of transform windows */
    Gpoint         *ndcpt, *wcpt;	/* point to pts before & after
					 * tansformation */

    /* run down the priority list */
    for (pl = 0; pl <= MAX_TRANS; pl++) {

	/* transform of pri pl */
	*ntrans = xgks_state.ntrans_priority[pl];

	/* limits of trans */
	win = &xgks_state.ntrans_list[*ntrans].ntrans.w;
	ndcpt = ndcpts;				/* head of list */
	wcpt = wcpts - 1;			/* backup one */
	cnt = 0;				/* count points */
	do {					/* for all the points */
	    wcpt++;
	    NtNdcToWc(*ntrans, ndcpt, wcpt);	/* convert ndcpt to wcpt */
	    ndcpt++;
	} while (++cnt < num			/* while still more points to
						 * check */
	/* and current point within limits of current trans. */
		 && wcpt->x >= win->xmin && wcpt->y >= win->ymin
		 && wcpt->x <= win->xmax && wcpt->y <= win->ymax);
	if (cnt == num
		&& wcpt->x >= win->xmin && wcpt->y >= win->ymin
		&& wcpt->x <= win->xmax && wcpt->y <= win->ymax
	    )					/* all the points transformed
						 * ok */
	    /* we found a trans. that contains all the points */
	    return 0;
    }
    if (pl > MAX_TRANS) {			/* if we used up all the
						 * trans. */
	/* this should not happen because trans 0 covers full NDC      */
	/* space - so, if it does happen, there are bogus points in    */
	/* the list.  (from bogus initial points, altered data, or     */
	/* transforms were changed since point data was created. (DWO) */
	/* convert remaining points... */
	do {
	    wcpt++;
	    NtNdcToWc(*ntrans, ndcpt, wcpt);	/* convert ndcpt to wcpt */
	    ndcpt++;
	}
	while (++cnt < num);			/* go thru rest of points */
	*ntrans = 0;				/* return trans 0 as a
						 * default */
	return -1;				/* indicate that something is
						 * wrong... */
    }
    /* ...in case we care someday */
    else
	return 0;
}


/*
 * XgksUpdateWsClip () 	- according to gks clip.ind
 *			  set all ws->clip rectangle to
 *
 *		GCLIP --> intersection of NDC-viewport and ws-window
 *	      GNOCLIP --> ws-window bounds
 *
 *			  this is done on ALL openedws[]
 */
XgksUpdateWsClip(ws, bound)
    WS_STATE_PTR    ws;
    Glimit         *bound;
{
    if (ws->ewstype != X_WIN)
	return 0;
    XgksWsWinInterset(ws, bound, &(ws->clip));
    xXgksUpdateClip(ws);
}


/*
 * XgksWsWinInterset (ws, v, clip)	find the intersection between
 *	NDC-viewport and ws-window
 * WS_STATE_PTR ws;
 * Glimit *v, *clip
 */
XgksWsWinInterset(ws, v, clip)
    WS_STATE_PTR    ws;
    Glimit         *v, *clip;
{
    Glimit         *w = &(ws->wsti.current.w);

    if ((v->xmax <= w->xmin) || (v->ymax <= v->ymin) ||
	    (v->xmin >= w->xmax) || (v->ymin >= w->ymax)) {

	/* This probably means two are disjoint */
	clip->xmin = clip->xmax = clip->ymin = clip->ymax = 0.0;
	return 0;
    }
    if ((w->xmax >= v->xmax) && (w->xmin <= v->xmin) &&
	    (w->ymax >= v->ymax) && (w->ymin <= v->ymin)) {

	/* This means ws-window encloses NDC-viewport */
	*(clip) = *v;
	return 0;
    }
    if ((v->xmax >= w->xmax) && (v->xmin <= w->xmin) &&
	    (v->ymax >= w->ymax) && (v->ymin <= w->ymin)) {

	/* This means NDC-viewport encloses ws-window */
	*(clip) = *w;
	return 0;
    }

    /* Bigger of the two */
    clip->xmin = (w->xmin > v->xmin) ? w->xmin : v->xmin;

    /* Bigger of the two */
    clip->ymin = (w->ymin > v->ymin) ? w->ymin : v->ymin;

    /* Smaller of the two */
    clip->xmax = (w->xmax < v->xmax) ? w->xmax : v->xmax;

    /* Smaller of the two */
    clip->ymax = (w->ymax < v->ymax) ? w->ymax : v->ymax;

    return 0;
}


/*
 * gevaltran (ppoint, pshift, angle, pscale, coord, result) -- EVALUATE
 *	TRANSFORMATION MATRIX
 *
 * Gpoint *ppoint;		 pointer to the reference point
 * Gpoint *pshift;		 Shift vector, w.r.t reference point
 * Gfloat angle;		 Rotation angle in radians
 * Gpoint	*pscale;	 Scaling factor
 * Gcsw	coord;			 coordinate switch GWC | GNDC
 * Gfloat  segtran[2][3];	 result transformation matrix
 *
 * returns : 0 = OK, or one of 8, 2000
 *
 * see also : ANSI Standard p.193
 */
gevaltran(ppoint, pshift, angle, pscale, coord, result)
    Gpoint         *ppoint;		/* pointer to the reference point */
    Gpoint         *pshift;		/* Shift vector, w.r.t reference
					 * point */
    Gfloat          angle;		/* Rotation angle in radians */
    Gpoint         *pscale;		/* Scaling factor */
    Gcsw            coord;		/* coordinate switch GWC | GNDC */
    Gfloat          result[2][3];	/* result transformation matrix */
{
    Gpoint          ndc_ref, ndc_shift, wc_org, ndc_org;
    double          c, s;

#ifdef TRANDEBUG
    (void) fprintf(stderr,
"gevaltran:transforms.c: ppoint(%f %f) pshift(%f %f) angle(%f) pacale(%f %f)\n",
		   ppoint->x, ppoint->y, pshift->x, pshift->y, angle,
		   pscale->x, pscale->y);
    (void) fprintf(stderr,
	       "gevaltran:transforms.c: input result = %f %f %f %f %f %f\n",
		   result[0][0], result[0][1], result[0][2],
		   result[1][0], result[1][1], result[1][2]);
#endif

    /* check for proper gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgevaltran);

    /* check for proper coordinate type */
    GKSERROR((coord != GWC && coord != GNDC), 2000, errgevaltran);

    /* translation must be calculated in NDC */
    if (coord == GWC) {
	wc_org.x = wc_org.y = 0.0;
	WcToNdc(&wc_org, &ndc_org);
	WcToNdc(ppoint, &ndc_ref);
	WcToNdc(pshift, &ndc_shift);
	ndc_shift.x -= ndc_org.x;
	ndc_shift.y -= ndc_org.y;
    } else {
	ndc_ref = *ppoint;
	ndc_shift = *pshift;
    }
    c = (cos((double) angle));
    s = (sin((double) angle));

#ifdef TRANDEBUG
    (void) fprintf(
		   stderr,
	   "gevaltran:transforms.c: (cosine angle)c=%f, (sine angle)s=%f\n",
		   c,
		   s);
#endif

    result[0][0] = (Gfloat) (pscale->x * (double) c);
    result[0][1] = (Gfloat) (pscale->x * (double) s);
    result[1][0] = (Gfloat) (-(pscale->y * (double) s));
    result[1][1] = (Gfloat) (pscale->y * (double) c);
    result[0][2] = (ndc_ref.x + ndc_shift.x) - (ndc_ref.x * result[0][0])
	- (ndc_ref.y * result[1][0]);
    result[1][2] = (ndc_ref.y + ndc_shift.y) - (ndc_ref.x * result[0][1])
	- (ndc_ref.y * result[1][1]);

#ifdef TRANDEBUG
    (void) fprintf(stderr,
		   "gevaltran:transforms.c: result = %f %f %f %f %f %f\n",
		   result[0][0], result[0][1], result[0][2],
		   result[1][0], result[1][1], result[1][2]);
#endif

    return OK;
}


/*
 *
 * gaccumtran (segtran, ppoint, pshift, angle, pscale, coord, result)
 *	-- ACCUMULATE TRANSFORMATION MATRIX
 * Gfloat segtran[2][3]		 input segment transformation
 * Gpoint *ppoint;		 pointer to the reference point
 * Gpoint *pshift;		 Shift vector, w.r.t reference point
 * Gfloat angle;		 Rotation angle in radians
 * Gpoint	*pscale;	 Scaling factor
 * Gcsw	coord;		         coordinate switch GWC | GNDC
 * Gfloat  result[2][3];	 result transformation matrix
 *
 * returns : 0 = OK, or one of 8, 2000
 *
 * see also : ANSI Standard p.193
 */
gaccumtran(segtran, ppoint, pshift, angle, pscale, coord, result)
    Gfloat          segtran[2][3];	/* input segment transformation */
    Gpoint         *ppoint;		/* pointer to the reference point */
    Gpoint         *pshift;		/* Shift vector, w.r.t reference
					 * point */
    Gfloat          angle;		/* Rotation angle in radians */
    Gpoint         *pscale;		/* Scaling factor */
    Gcsw            coord;		/* coordinate switch GWC | GNDC */
    Gfloat          result[2][3];	/* result transformation matrix */
{
    Gpoint          ndc_ref, ndc_shift, wc_org, ndc_org;
    double          c, s;		/* cosine, sine vars */

    /* check for proper gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgaccumtran);

    /* check for proper coordinate type */
    GKSERROR((coord != GWC && coord != GNDC), 2000, errgaccumtran);

    /* translation must be calculated in NDC */
    if (coord == GWC) {
	wc_org.x = wc_org.y = 0.0;
	WcToNdc(&wc_org, &ndc_org);
	WcToNdc(ppoint, &(ndc_ref));
	WcToNdc(pshift, &(ndc_shift));
	ndc_shift.x -= ndc_org.x;
	ndc_shift.y -= ndc_org.y;
    } else {
	ndc_ref = *ppoint;
	ndc_shift = *pshift;
    }
    /* calculate cosine and sine ONCE */
    c = cos((double) angle);
    s = sin((double) angle);

    result[0][0] = (segtran[0][0] * pscale->x * c) + 
		   (segtran[0][1] * -pscale->y * s);
    result[1][0] = (segtran[1][0] * pscale->x * c) +
		   (segtran[1][1] * -pscale->y * s);
    result[0][1] = (segtran[0][0] * pscale->x * s) +
		   (segtran[0][1] * pscale->y * c);
    result[1][1] = (segtran[1][0] * pscale->x * s) +
		   (segtran[1][1] * pscale->y * c);

    result[0][2] = (ndc_ref.x + ndc_shift.x)
	+ segtran[0][2] * pscale->x * c - segtran[1][2] * pscale->y * s
	+ ndc_ref.y * pscale->y * s - ndc_ref.x * pscale->x * c;
    result[1][2] = (ndc_ref.y + ndc_shift.y)
	+ segtran[0][2] * pscale->x * s + segtran[1][2] * pscale->y * c
	- ndc_ref.x * pscale->x * s - ndc_ref.y * pscale->y * c;

    return OK;
}
