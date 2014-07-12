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
 * stroke.c - functions and data for GKS Stroke
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include <signal.h>
#include <math.h>
#include "gks_implem.h"

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif

#ifndef Bool
#include <stdbool.h>
#endif
#ifndef True
#define True 1
#define False 0
#endif

/*
 * create a default string device. returns True if cannot creat.
 */
    static Bool
XgksCreateDefStroke(ws, dev, idevp)
    WS_STATE_ENTRY *ws;
    Gint            dev;
    INPUT_DEV     **idevp;
{
#ifdef X11OUT
    XGCValues       gcvalues;
#endif
    INPUT_DEV      *idev;

    idev = XgksIDevNew();
    if (idev == NULL)
	return True;
    idev->class = GISTROKE;
    idev->dev = dev;
    idev->active = False;
#ifdef X11OUT
    gcvalues.function = GXinvert;
    gcvalues.foreground = ws->wsfg;
    gcvalues.background = ws->wsbg;
    gcvalues.line_width = 0;
    gcvalues.line_style = LineSolid;
    gcvalues.fill_style = FillSolid;
    idev->gc = XCreateGC(ws->dpy, ws->win,
			 GCFunction | GCForeground | GCBackground |
			 GCLineWidth | GCLineStyle | GCFillStyle,
			 &gcvalues);
#endif
    idev->data.stk.initst.mode = GREQUEST;
    idev->data.stk.initst.esw = GECHO;
    idev->data.stk.initst.stroke.transform = 0;
    idev->data.stk.initst.stroke.n_points = 0;
    idev->data.stk.initst.stroke.points = NULL;
    idev->data.stk.initst.pet = 1;
    idev->data.stk.initst.e_area.xmin = 0.0;
    idev->data.stk.initst.e_area.xmax = ws->size.x;
    idev->data.stk.initst.e_area.ymin = 0.0;
    idev->data.stk.initst.e_area.ymax = ws->size.y;
    idev->data.stk.initst.record.pet1.bufsiz = 64;
    idev->data.stk.initst.record.pet1.editpos = 1;
    idev->data.stk.initst.record.pet1.interval.x = 0.001;
    idev->data.stk.initst.record.pet1.interval.y = 0.001;
    idev->data.stk.initst.record.pet1.time = 0.0;
    idev->data.stk.initst.record.pet1.data = NULL;

    idev->data.stk.stkbuf = (Gpoint *) malloc((size_t) (sizeof(Gpoint) * 64));
    if (idev->data.stk.stkbuf == NULL)
	return True;
    idev->data.stk.stkbuf[0].x = 0.5;
    idev->data.stk.stkbuf[0].y = 0.5;
    idev->data.stk.interval.x = 0.001;
    idev->data.stk.interval.y = 0.001;
    idev->data.stk.editpos = 0;

    /* link the new device into the list */
    idev->next = ws->in_dev_list;
    ws->in_dev_list = idev;

    *idevp = idev;
    return False;
}


/*
 * INITIALISE STROKE
 *
 * Returns: 0, 7, 20, 25, 38, 50, 51, 60, 63, 65, 66, 67, 69, 71, 92, 140, 
 *	    141, 144, 145, 146, 152, 153
 *
 * See Also: ANSI Standard p.121
 */
ginitstroke(ws_id, dev, init, pet, area, record)
    Gint            ws_id;
    Gint            dev;
    Gstroke        *init;
    Gint            pet;
    Glimit         *area;
    Gstrokerec     *record;
{
    WS_STATE_ENTRY *ws;
    INPUT_DEV      *idev;
#ifdef X11OUT
    XGCValues       gcvalues;
#endif
    Gpoint         *spt, *dpt;
    int             i, didx;
    static Gpoint  *ptr;
    static int      cnt = 1;
    static Glimit  *win;

    /* STEP 1: check for errors. */
    /* proper gks state? */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginitstroke);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginitstroke);

    /* open wsid? */
    GKSERROR(!(ws = OPEN_WSID(ws_id)), 25, errginitstroke);

    /* valid workstation category */
    GKSERROR((WS_CAT(ws) != GOUTIN && WS_CAT(ws) != GINPUT), 38, 
	     errginitstroke);

    /* valid echo area */
    GKSERROR((area->xmin > area->xmax || area->ymin > area->ymax), 51, 
	     errginitstroke);

    /* valid stroke device number */
    GKSERROR((dev < 1), 140, errginitstroke);

    /* valid and supported prompt mode? */
    GKSERROR(pet != 1 && pet != 3 && pet != 4, 144, errginitstroke);

    /* Echo inside display space? */
    GKSERROR((area->xmin < 0 || area->xmax > ws->size.x
	      || area->ymin < 0 || area->ymax > ws->size.y),
	     145, errginitstroke);

    /* valid data record */
    GKSERROR(record->pet1.bufsiz < 1, 146, errginitstroke);
    GKSERROR(record->pet1.editpos < 1, 146, errginitstroke);
    GKSERROR(record->pet1.editpos > record->pet1.bufsiz, 146, errginitstroke);
    GKSERROR(record->pet1.time < 0.0, 146, errginitstroke);
    GKSERROR(init->n_points < 0, 152, errginitstroke);
    GKSERROR(init->n_points > record->pet1.bufsiz, 153, errginitstroke);
    GKSERROR(init->transform < 0, 50, errginitstroke);
    GKSERROR(init->transform > MAX_TRANS, 50, errginitstroke);

    /*
     * Check that editpos is within the initial points (so that bogus data
     * won't be mistaken for valid points).
     * The standard doesn't really mention this restriction, but it seems
     * needed.
     */
    GKSERROR(record->pet1.editpos > init->n_points + 1, 152, errginitstroke);

    /* make sure initial points are within the transform in initstroke */
    win = &xgks_state.ntrans_list[init->transform].ntrans.w;
    for (ptr = init->points, cnt = 1;
	    cnt <= init->n_points;
	    ptr++, cnt++)
	GKSERROR(((ptr->x < win->xmin) || (win->xmax < ptr->x)
		  || (ptr->y < win->ymin) || (win->ymax < ptr->y)),
		 146, errginitstroke);

    didx = 0;

    switch (pet) {
    default:
    case 1:
	break;
    case 3:					/* polymarker */
	if (record->pet3.acf == GSPECIFIED) {
	    GKSERROR(record->pet3.mk.mark < 1 ||
		     record->pet3.mk.mark >= MAX_BUNDL_TBL, 66, errginitstroke);
	    GKSERROR(!WS_MARKER_TYPE(record->pet3.mk.bundl.type), 69,
		     errginitstroke);
	    GKSERROR(record->pet3.mk.bundl.size < 0.0, 71, errginitstroke);
	    GKSERROR(!WS_AVAIL_COLOUR(ws, record->pet3.mk.bundl.colour), 92,
		     errginitstroke);
	} else {
	    record->pet3.mk.type = xgks_state.gks_mkattr.type;
	    record->pet3.mk.size = xgks_state.gks_mkattr.size;
	    record->pet3.mk.colour = xgks_state.gks_mkattr.colour;
	    record->pet3.mk.mark = xgks_state.gks_mkattr.mark;
	    record->pet3.mk.bundl = xgks_state.gks_mkattr.bundl;
	}
	/*
	 * Bind attributes to device by storing in pet3.mk.bundl the
	 * attributes based on the state of the ASF flags.
	 */
#define MKBUND    ws->mkbundl_table[record->pet3.mk.mark]
	if (record->pet3.mk.type == GBUNDLED)
	    record->pet3.mk.bundl.type = MKBUND.type;
	if (record->pet3.mk.size == GBUNDLED)
	    record->pet3.mk.bundl.size = MKBUND.size;
	if (record->pet3.mk.colour == GBUNDLED)
	    record->pet3.mk.bundl.colour = MKBUND.colour;

#ifdef X11OUT
	/* Bind values into the GC */
	gcvalues.function = GXinvert;
	gcvalues.foreground = record->pet3.mk.bundl.colour;
	if (!WS_AVAIL_COLOUR(ws, gcvalues.foreground))
	    gcvalues.foreground = ws->wsfg;
	gcvalues.line_width = 0;
	gcvalues.line_style = LineSolid;
#endif
#undef MKBUND
	break;
    case 4:					/* polyline */
	if (record->pet4.acf == GSPECIFIED) {
	    GKSERROR(record->pet4.ln.line < 1 ||
		     record->pet4.ln.line >= MAX_BUNDL_TBL, 60, errginitstroke);
	    GKSERROR(record->pet4.ln.bundl.type == 0, 63, errginitstroke);
	    GKSERROR(!WS_LINE_TYPE(record->pet4.ln.bundl.type), 64,
		     errginitstroke);
	    GKSERROR(record->pet4.ln.bundl.width < 0.0, 65, errginitstroke);
	    GKSERROR(!WS_AVAIL_COLOUR(ws, record->pet4.ln.bundl.colour), 92,
		     errginitstroke);
	} else {
	    record->pet4.ln.type = xgks_state.gks_lnattr.type;
	    record->pet4.ln.width = xgks_state.gks_lnattr.width;
	    record->pet4.ln.colour = xgks_state.gks_lnattr.colour;
	    record->pet4.ln.line = xgks_state.gks_lnattr.line;
	    record->pet4.ln.bundl = xgks_state.gks_lnattr.bundl;
	}
	/*
	 * Bind attributes to device by storing in pet3.mk.bundl the
	 * attributes based on the state of the ASF flags.
	 */
#define LNBUND    ws->lnbundl_table[record->pet4.ln.line]
	if (record->pet4.ln.type == GBUNDLED)
	    record->pet4.ln.bundl.type = LNBUND.type;
	if (record->pet4.ln.width == GBUNDLED)
	    record->pet4.ln.bundl.width = LNBUND.width;
	if (record->pet4.ln.colour == GBUNDLED)
	    record->pet4.ln.bundl.colour = LNBUND.colour;

	/* Bind values into the GC */
#ifdef X11OUT
	gcvalues.function = GXinvert;
	gcvalues.line_style = (int) record->pet4.ln.bundl.type;
	didx = gcvalues.line_style;
	if (didx < 0)
	    didx += 3;
	else if (didx > 0)
	    didx += 1;
	/* if (didx == 0) doesn't matter */

	gcvalues.line_style = (gcvalues.line_style == GLN_SOLID) ?
	    LineSolid : LineOnOffDash;
	gcvalues.line_width = (int) record->pet4.ln.bundl.width;
	if (gcvalues.line_style == LineSolid && gcvalues.line_width == 1)
	    gcvalues.line_width = 0;
	gcvalues.foreground = record->pet4.ln.bundl.colour;
	if (!WS_AVAIL_COLOUR(ws, gcvalues.foreground))
	    gcvalues.foreground = ws->wsfg;
#endif
#undef LNBUNDLE
	break;
    }
    if ((idev = XgksIDevLookup(ws, dev, GISTROKE)) == NULL) {
	GKSERROR(XgksCreateDefStroke(ws, dev, &idev), 300, errginitstroke);
    } else {
	GKSERROR(idev->data.stk.initst.mode != GREQUEST, 141, errginitstroke);
    }
#ifdef X11OUT
    gcvalues.background = ws->wsbg;
    XChangeGC(ws->dpy, idev->gc,
       GCFunction | GCForeground | GCBackground | GCLineWidth | GCLineStyle,
	      &gcvalues);
#endif
    /* copy points to initial stroke */
    if (idev->data.stk.initst.stroke.points != NULL)
	ufree((voidp) idev->data.stk.initst.stroke.points);
    idev->data.stk.initst.stroke = *init;
    dpt = idev->data.stk.initst.stroke.points =
	(Gpoint *) malloc((size_t) (sizeof(Gpoint) * init->n_points));
    GKSERROR(dpt == NULL, 300, errginitstroke);
    for (i = 0, spt = init->points; i < init->n_points; i++, spt++, dpt++)
	*dpt = *spt;

    /* copy other initialization data */
    idev->data.stk.initst.pet = pet;
    idev->data.stk.initst.e_area = *area;
    idev->data.stk.initst.record = *record;

    /*
     * moved this if from after ChangeGC to avoid core dump problem. don't
     * know why, but this fixes it - bg
     */
#ifdef X11OUT
    if (gcvalues.line_style == LineOnOffDash)
	XSetDashes(ws->dpy, idev->gc, 0, xgksDASHES[didx].dashl,
		   xgksDASHES[didx].dn);
#endif
    return 0;
}


/*
 * SET STROKE MODE
 *
 * Returns: 0, 7, 20, 25, 38, 140, 143, 2000
 *
 * See Also: ANSI Standard p.129
 */
gsetstrokemode(ws_id, dev, mode, echo)
    Gint            ws_id;
    Gint            dev;
    Gimode          mode;
    Gesw            echo;
{
    WS_STATE_ENTRY *ws;
    INPUT_DEV      *idev;
    Gpoint         *spt, *dpt, ndcpt, wcpt;
    int             i;

    /* STEP 1: check for errors. */
    /* proper gks state? */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgsetstrokemode);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgsetstrokemode);

    /* open wsid? */
    GKSERROR(!(ws = OPEN_WSID(ws_id)), 25, errgsetstrokemode);

    /* valid workstation type */
    GKSERROR((WS_CAT(ws) != GOUTIN && WS_CAT(ws) != GINPUT), 38, 
	     errgsetstrokemode);

    /* valid stroke device number */
    GKSERROR((dev < 1), 140, errgsetstrokemode);

    /* check enumerations */
    GKSERROR(((mode != GREQUEST && mode != GSAMPLE && mode != GEVENT)
	      || (echo != GECHO && echo != GNOECHO)),
	     2000, errgsetstrokemode);

    if ((idev = XgksIDevLookup(ws, dev, GISTROKE)) == NULL) {
	/* Create the Input Device structure */
	GKSERROR(XgksCreateDefStroke(ws, dev, &idev), 300, errgsetstrokemode);
    } else {
#ifdef X11OUT
	if ((idev->active == True) && (idev->data.stk.initst.esw == GECHO))
	    (void) XgksStkUpdatePrompt(ws, idev, PROMPTOFF, (Gpoint *) NULL,
				       (XMotionEvent *) NULL, -1);
#endif
    }
    idev->data.stk.initst.mode = mode;
    idev->data.stk.initst.esw = echo;

    if (mode == GSAMPLE || mode == GEVENT) {
	/* copy & transform initial points to current stroke */
	if (idev->data.stk.stkbuf != NULL)
	    ufree((voidp) idev->data.stk.stkbuf);
	dpt = idev->data.stk.stkbuf = 
	    (Gpoint *) malloc((size_t) (sizeof(Gpoint) *
			      idev->data.stk.initst.record.pet1.bufsiz));
	GKSERROR(dpt == NULL, 300, errgsetstrokemode);
	for (i = 0, spt = idev->data.stk.initst.stroke.points;
	     i < idev->data.stk.initst.stroke.n_points; i++, spt++, dpt++) {
	    NtWcToNdc(idev->data.stk.initst.stroke.transform, spt, &ndcpt);
	    NdcToDc(ws, &ndcpt, dpt);
	}
	NtWcToNdc(idev->data.stk.initst.stroke.transform,
		  &(idev->data.stk.initst.record.pet1.interval),
		  &(idev->data.stk.interval));
	wcpt.x = 0.0;
	wcpt.y = 0.0;
	NtWcToNdc(idev->data.stk.initst.stroke.transform, &wcpt, &ndcpt);
	idev->data.stk.interval.x = fabs((double) (idev->data.stk.interval.x
						   - ndcpt.x));
	idev->data.stk.interval.y = fabs((double) (idev->data.stk.interval.y
						   - ndcpt.y));

	idev->data.stk.editpos =
	    idev->data.stk.initst.record.pet1.editpos - 1;
	idev->active = True;
#ifdef X11OUT
	if (echo == GECHO)
	    (void) XgksStkUpdatePrompt(ws, idev, PROMPTON,
		&(idev->data.stk.stkbuf[idev->data.stk.editpos]),
		(XMotionEvent *) NULL, -1);
#endif
    } else					/* GREQUEST */
	idev->active = False;

    return 0;
}


/*
 * REQUEST STROKE
 *
 * Returns: 0, 7, 20, 25, 38, 140, 141
 *
 * See Also: ANSI Standard p.132
 */
greqstroke(ws_id, dev, response)
    Gint            ws_id;
    Gint            dev;
    Gqstroke       *response;
{
    WS_STATE_ENTRY *ws;
    INPUT_DEV      *idev;
    Gpoint         *spt, *dpt, ndcpt, *ndcpts, wcpt;
    int             i;

    /* STEP 1: check for errors. */
    /* proper gks state? */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgreqstroke);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgreqstroke);

    /* open wsid? */
    GKSERROR(!(ws = OPEN_WSID(ws_id)), 25, errgreqstroke);

    /* valid workstation type */
    GKSERROR((WS_CAT(ws) != GOUTIN && WS_CAT(ws) != GINPUT), 38, errgreqstroke);

    /* valid locator device number */
    GKSERROR((dev < 1), 140, errgreqstroke);

    if ((idev = XgksIDevLookup(ws, dev, GISTROKE)) == NULL) {
	/* Create the Input Device structure */
	GKSERROR(XgksCreateDefStroke(ws, dev, &idev), 300, errgreqstroke);
    } else {
	GKSERROR((idev->data.stk.initst.mode != GREQUEST), 141, errgreqstroke);
    }

    /* Make sure the workstation is up to date */
    (void) gupdatews(ws_id, GPERFORM);

    /* copy & transform initial points to current stroke */
    if (idev->data.stk.stkbuf != NULL)
	ufree((voidp) idev->data.stk.stkbuf);
    dpt = idev->data.stk.stkbuf = (Gpoint *) 
	malloc((size_t) (sizeof(Gpoint) *
	       idev->data.stk.initst.record.pet1.bufsiz));
    GKSERROR(dpt == NULL, 300, errgreqstroke);
    for (i = 0, spt = idev->data.stk.initst.stroke.points;
	    i < idev->data.stk.initst.stroke.n_points; i++, spt++, dpt++) {
	NtWcToNdc(idev->data.stk.initst.stroke.transform, spt, &ndcpt);
	NdcToDc(ws, &ndcpt, dpt);
    }
    NtWcToNdc(idev->data.stk.initst.stroke.transform,
	      &(idev->data.stk.initst.record.pet1.interval),
	      &(idev->data.stk.interval));
    wcpt.x = 0.0;
    wcpt.y = 0.0;
    NtWcToNdc(idev->data.stk.initst.stroke.transform, &wcpt, &ndcpt);
    idev->data.stk.interval.x = fabs((double) (idev->data.stk.interval.x -
					       ndcpt.x));
    idev->data.stk.interval.y = fabs((double) (idev->data.stk.interval.y -
					       ndcpt.y));

    /* set editpos to the editpos specified in initstroke             */
    /* NOTE: this will be incremented by incoming data!  From here on */
    /* do not confuse it with the initstroke version of editpos */
    idev->data.stk.editpos =
	idev->data.stk.initst.record.pet1.editpos - 1;

    idev->active = True;			/* activate the stroke device */
    /* if echo is set to on... */
#ifdef X11OUT
    if (idev->data.stk.initst.esw == GECHO)
	/* update prompt (display initial points etc?) */
	(void) XgksStkUpdatePrompt(ws, idev, PROMPTON,
	    &(idev->data.stk.stkbuf[idev->data.stk.editpos]),
	    (XMotionEvent *) NULL, -1);
#endif

    /* wait for trigger or break */
#ifdef X11OUT
    (void) XgksSIGIO_OFF(ws->dpy);
#endif
    idev->touched = False;
    idev->breakhit = False;
#ifdef X11OUT
    while (idev->touched == False && idev->breakhit == False)
	 (void) XgksAwaitEvent(&ws, 1, -1.0);
    (void) XgksSIGIO_ON(ws->dpy);
#endif
    /* deactivate stroke device */
    idev->active = False;
#ifdef X11OUT
    if (idev->data.stk.initst.esw == GECHO)
	(void) XgksStkUpdatePrompt(ws, idev, PROMPTOFF, (Gpoint *) NULL,
				   (XMotionEvent *) NULL, -1);
#endif

    /* if user hit <break>, return GNONE, else return GOK */
    if ((idev->breakhit == True)) {
	response->status = GNONE;
    } else {
	response->status = GOK;
	/* get a buffer to store the points in */
	spt = ndcpts = (Gpoint *) malloc((size_t) (sizeof(Gpoint) *
					 idev->data.stk.editpos));
	GKSERROR(spt == NULL, 300, errgreqstroke);

	/* convert all points from DC to NDC (and copy into ndcpts) */
	for (i = 0, dpt = idev->data.stk.stkbuf;
		i < idev->data.stk.editpos; i++, spt++, dpt++) {
	    DcToNdc(ws, dpt, spt);
	}
	/* find the ntrans and WC points */
	if (idev->data.stk.editpos > 0)
	    (void) XgksFindNTransNpts(idev->data.stk.editpos, ndcpts,
				      &(response->stroke.transform),
				      response->stroke.points);
	response->stroke.n_points = idev->data.stk.editpos;
	ufree((voidp) ndcpts);
	/*
	 * NOTE: editpos at this point is not the editpos set in initstroke.
	 * editpos is incremented as new points are added to the
	 * buffer.  It is reset at the beginning of request stroke to
	 * the initialized value.  (It is also one less - the user
	 * refers to the edit position starting at 1, while internally
	 * it starts at 0)
	 */
    }

    return OK;
}


/*
 * SAMPLE STROKE
 *
 * Returns: 0, 7, 20, 25, 38, 140, 142
 *
 * See Also: ANSI Standard p.135
 */
gsamplestroke(ws_id, dev, response)
    Gint            ws_id;
    Gint            dev;
    Gstroke        *response;
{
    WS_STATE_ENTRY *ws;
    INPUT_DEV      *idev;
    Gpoint         *spt, *dpt, *ndcpts;
    int             i;

    /* STEP 1: check for errors. */
    /* proper gks state? */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgsamplestroke);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgsamplestroke);

    /* open wsid? */
    GKSERROR(!(ws = OPEN_WSID(ws_id)), 25, errgsamplestroke);

    /* valid workstation type */
    GKSERROR((WS_CAT(ws) != GOUTIN && WS_CAT(ws) != GINPUT), 38, 
	     errgsamplestroke);

    /* valid locator device number */
    GKSERROR((dev < 1), 140, errgsamplestroke);

    idev = XgksIDevLookup(ws, dev, GISTROKE);
    GKSERROR((idev == NULL) || (idev->data.stk.initst.mode != GSAMPLE), 142, 
	     errgsamplestroke);

    /* Make sure the workstation is up to date */
    (void) gupdatews(ws_id, GPERFORM);

    /* Convert current measure to WC space */
    spt = ndcpts = (Gpoint *) malloc((size_t) (sizeof(Gpoint) *
				     idev->data.stk.editpos));
    GKSERROR(spt == NULL, 300, errgsamplestroke);
    for (i = 0, dpt = idev->data.stk.stkbuf;
	    i < idev->data.stk.editpos; i++, spt++, dpt++) {
	DcToNdc(ws, dpt, spt);
    }
    /* find the ntrans and WC points */
    if (idev->data.stk.editpos > 0)
	(void) XgksFindNTransNpts(idev->data.stk.editpos, ndcpts,
				  &(response->transform), response->points);
    response->n_points = idev->data.stk.editpos;
    ufree((voidp)ndcpts);

    return OK;
}


/*
 * INQUIRE STROKE DEVICE STATE
 *
 * GKS mallocs space for the list of initial stroke points, this memory
 * should be freeed by the user when no longer needed.
 *
 * Returns: 0, 7, 20, 25, 38, 140, 2000
 *
 * See Also: ANSI Standard p.167
 */
ginqstrokest(ws_id, dev, type, state)
    Gint            ws_id;
    Gint            dev;
    Gqtype          type;
    Gstrokest      *state;
{
    WS_STATE_ENTRY *ws;
    INPUT_DEV      *idev;
    Gpoint         *spt, *dpt;
    int             i;

    /* STEP 1: check for errors. */
    /* proper gks state? */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqstrokest);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqstrokest);

    /* open wsid? */
    GKSERROR(!(ws = OPEN_WSID(ws_id)), 25, errginqstrokest);

    /* valid workstation type */
    GKSERROR((WS_CAT(ws) != GOUTIN && WS_CAT(ws) != GINPUT), 38, 
	     errginqstrokest);

    /* valid locator device number */
    GKSERROR((dev < 1), 140, errginqstrokest);

    /* valid enumeration */
    GKSERROR((type != GSET) && (type != GREALIZED), 2000, errginqstrokest);

    if ((idev = XgksIDevLookup(ws, dev, GISTROKE)) == NULL) {
	/* Create the Input Device structure */
	GKSERROR(XgksCreateDefStroke(ws, dev, &idev), 300, errginqstrokest);
    }
    *state = idev->data.stk.initst;
    dpt = state->stroke.points = (Gpoint *) malloc((size_t) (sizeof(Gpoint) *
						   state->stroke.n_points));
    GKSERROR(state->stroke.points == NULL, 300, errginqstrokest);
    for (i = 0, spt = idev->data.stk.initst.stroke.points;
	    i < state->stroke.n_points; i++, spt++, dpt++)
	*dpt = *spt;

    /*
     * if idev->data.stk.initst.record.pet?.data pointed to anything it would
     * be copied here.
     */

    return OK;
}


/*
 * INQUIRE DEFAULT STROKE DEVICE DATA
 *
 * GKS mallocs space for the list of supported prompt types, this memory should
 * be freeed by the user when no longer needed.
 *
 * Returns: 0, 8, 22, 23, 38, 140, 300
 *
 * See Also: ANSI Standard p.186
 */
ginqdefstroke(type, dev, data)
    Gchar          *type;
    Gint            dev;
    Gdefstroke     *data;
{
    EWSTYPE         ewstype;

    /* STEP 1: check for errors */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqdefstroke);

    /* valid wsid? */
    ewstype = XgksWsTypeToEnum(type);
    GKSERROR(ewstype == WST_INVALID, 22, errginqdefstroke);
    GKSERROR(ewstype != X_WIN, 38, errginqdefstroke);

    /* valid stroke dev number */
    GKSERROR(dev < 1, 140, errginqdefstroke);

    /* STEP 2: set up the return values */
    data->bufsiz = 64;

    data->pets.number = 3;
    data->pets.integers = (Gint *) malloc((size_t) (sizeof(Gint) * 3));
    GKSERROR(data->pets.integers == NULL, 300, errginqdefstroke);
    data->pets.integers[0] = 1;
    data->pets.integers[1] = 3;
    data->pets.integers[2] = 4;

    data->e_area.xmin = 0.0;
    data->e_area.xmax = WS_MAX_DCX;
    data->e_area.ymin = 0.0;
    data->e_area.ymax = WS_MAX_DCY;

    data->record.pet1.bufsiz = 64;
    data->record.pet1.editpos = 1;
    data->record.pet1.interval.x = 0.001;
    data->record.pet1.interval.y = 0.001;
    data->record.pet1.time = 0.0;
    data->record.pet1.data = NULL;

    return OK;
}


/*
 * XgksStkUpdatePrompt - update the stroke prompt
 */
#ifdef X11OUT
XgksStkUpdatePrompt(ws, idev, pstate, newdcpt, xmev, event_id)
    WS_STATE_ENTRY *ws;
    INPUT_DEV      *idev;
    PromptStatus    pstate;
    Gpoint         *newdcpt;
    XMotionEvent   *xmev;
    int             event_id;
{
    Gpoint          prev, ndcpt, *ndcpts, *spt, *dpt;
    Gstroke        *data;
    XRectangle      rect;
    XPoint         *xpts, *xpoints;
    int             i;

#define STKBUFSIZ(P)    (idev->data.stk.initst.record.P.bufsiz)
#define STKEDITPOS    (idev->data.stk.editpos)
#define MTYPE    (idev->data.stk.initst.record.pet3.mk.bundl.type)
#define MSIZE    (idev->data.stk.initst.record.pet3.mk.bundl.size)

    rect.x = 0;
    rect.y = 0;
    rect.width = ws->wbound.x;
    rect.height = ws->wbound.y;
    XSetClipRectangles(ws->dpy, idev->gc, 0, 0, &rect, 1, Unsorted);

    switch (pstate) {
    case PROMPTON:
	/* transform the points */
	xpoints = (XPoint *) malloc((size_t) (sizeof(XPoint) * STKEDITPOS));
	for (i = 0, ndcpts = idev->data.stk.stkbuf, xpts = xpoints;
		i < STKEDITPOS; i++, ndcpts++, xpts++) {
	    DcToX(ws, ndcpts, xpts);
	}
	switch (idev->data.stk.initst.pet) {
	default:
	case 1:				/* tracking cross uses X cursor */
	    break;
	case 3:				/* polymarkers */
	    XgksDrawMarkers(ws->dpy, ws->win, idev->gc, xpoints, STKEDITPOS,
			    MTYPE, MSIZE);
	    break;
	case 4:				/* polyline */
	    if (STKEDITPOS > 1)
		XgksXDrawLines(ws->dpy, ws->win, idev->gc, xpoints,
			       STKEDITPOS, CoordModeOrigin);
	    break;
	}
	ufree((voidp)xpoints);
	break;
    case PROMPTOFF:
	/* transform the points */
	xpoints = (XPoint *) malloc((size_t) (sizeof(XPoint) * STKEDITPOS));
	for (i = 0, ndcpts = idev->data.stk.stkbuf, xpts = xpoints;
		i < STKEDITPOS; i++, ndcpts++, xpts++) {
	    DcToX(ws, ndcpts, xpts);
	}
	switch (idev->data.stk.initst.pet) {
	default:
	case 1:				/* tracking cross uses X
						 * cursor */
	    break;
	case 3:				/* polymarker */
	    XgksDrawMarkers(ws->dpy, ws->win, idev->gc, xpoints, STKEDITPOS,
			    MTYPE, MSIZE);
	    break;
	case 4:				/* polyline */
	    if (STKEDITPOS > 1)
		XgksXDrawLines(ws->dpy, ws->win, idev->gc, xpoints,
			       STKEDITPOS, CoordModeOrigin);
	    break;
	}
	ufree((voidp)xpoints);
	break;
    case PROMPTMOVE:
	/* stroke point must lie within the workstation window */
	DcToNdc(ws, newdcpt, &ndcpt);

#ifdef STKDEBUG
	(void) fprintf(stderr, "XgksStkUpdatePrompt( DC= %f %f NDC %f %f )\n",
		       newdcpt->x, newdcpt->y, ndcpt.x, ndcpt.y);
	(void) fprintf(stderr, "    editpos %d bufsiz %d mtype %d msize %f\n",
		       STKEDITPOS, STKBUFSIZ(pet1), MTYPE, MSIZE);
#endif

	if (ndcpt.x < ws->wsti.current.w.xmin
		|| ndcpt.x > ws->wsti.current.w.xmax
		|| ndcpt.y < ws->wsti.current.w.ymin
		|| ndcpt.y > ws->wsti.current.w.ymax)
	    return 0;

	if (STKEDITPOS > 0) {
	    DcToNdc(ws, &(idev->data.stk.stkbuf[STKEDITPOS - 1]), &prev);

#ifdef STKDEBUG
	    (void) fprintf(stderr, "XgksStkUpdatePrompt: dx %f dy %f\n",
			   fabs((double) (ndcpt.x - prev.x)),
			   fabs((double) (ndcpt.y - prev.y)));
#endif
	}
	if ((STKEDITPOS == 0) ||
	    (fabs((double) (ndcpt.x - prev.x)) >= idev->data.stk.interval.x) ||
	    (fabs((double) (ndcpt.y - prev.y)) >= idev->data.stk.interval.y)) {

	    /* transform the points */
	    xpoints = (XPoint *) malloc((size_t) (sizeof(XPoint) *
					(STKEDITPOS + 1)));
	    for (i = 0, ndcpts = idev->data.stk.stkbuf, xpts = xpoints;
		    i < STKEDITPOS; i++, ndcpts++, xpts++) {
		DcToX(ws, ndcpts, xpts);
	    }
	    switch (idev->data.stk.initst.pet) {
	    default:
	    case 1:				/* tracking cross uses X
						 * cursor */
		if (STKEDITPOS < STKBUFSIZ(pet1)) {
		    idev->data.stk.stkbuf[STKEDITPOS] = *newdcpt;
		    STKEDITPOS++;
		}
		break;
	    case 3:				/* polymarker */
		if (STKEDITPOS < STKBUFSIZ(pet3)) {
		    idev->data.stk.stkbuf[STKEDITPOS] = *newdcpt;
		    DcToX(ws, &(idev->data.stk.stkbuf[STKEDITPOS]),
			  &(xpoints[STKEDITPOS]));
		    STKEDITPOS++;
		    if (idev->data.stk.initst.esw == GECHO)
			XgksDrawMarkers(ws->dpy, ws->win, idev->gc,
			       &(xpoints[STKEDITPOS - 1]), 1, MTYPE, MSIZE);
		}
		break;
	    case 4:				/* polyline */
		if (STKEDITPOS < STKBUFSIZ(pet4)) {
		    idev->data.stk.stkbuf[STKEDITPOS] = *newdcpt;
		    DcToX(ws, &(idev->data.stk.stkbuf[STKEDITPOS]),
			  &(xpoints[STKEDITPOS]));
		    STKEDITPOS++;
		    if (idev->data.stk.initst.esw == GECHO
			    && STKEDITPOS > 1)
			XgksXDrawLines(ws->dpy, ws->win, idev->gc,
			    &(xpoints[STKEDITPOS - 2]), 2, CoordModeOrigin);
		}
		break;
	    }
	    ufree((voidp)xpoints);
	}
	switch (idev->data.stk.initst.mode) {
	default:
	case GREQUEST:
	    if (xmev->type != ButtonRelease)
		break;
	    idev->touched = True;
	    XBell(ws->dpy, 0);
	    break;
	case GSAMPLE:
	    break;
	case GEVENT:
	    if (xmev->type != ButtonRelease)
		break;
	    data = (Gstroke *) malloc(sizeof(Gstroke));
	    if (data == NULL) {
		(void) gerrorhand(300, errXgksStkUpdatePrompt,
				  xgks_state.gks_err_file);
		return 300;
	    } else {
		XBell(ws->dpy, 0);
		/* Convert current measure to WC space */
		spt = ndcpts = (Gpoint *) malloc((size_t) (sizeof(Gpoint) *
						 STKEDITPOS));
		data->points = (Gpoint *) malloc((size_t) (sizeof(Gpoint) *
						 STKEDITPOS));
		GKSERROR(spt == NULL || data->points == NULL, 300,
			 errXgksStkUpdatePrompt);
		for (i = 0, dpt = idev->data.stk.stkbuf;
			i < STKEDITPOS; i++, spt++, dpt++) {
		    DcToNdc(ws, dpt, spt);
		}
		/* find the ntrans and WC points */
		if (STKEDITPOS > 0)
		    (void) XgksFindNTransNpts(STKEDITPOS, ndcpts,
					      &(data->transform), data->points);
		data->n_points = STKEDITPOS;
		ufree((voidp)ndcpts);
		(void) XgksEnqueueEvent(ws->ws_id, idev->dev, GISTROKE,
					(char *) data, event_id);
	    }
	    break;
	}
	break;
    default:
	break;
    }

    XFlush(ws->dpy);
    return 0;
}

#endif
/*
 * free all memory associate with a stroke logical input device
 */
XgksStkDelete(ws, idev)
    WS_STATE_ENTRY *ws;
    INPUT_DEV      *idev;
{
#ifdef X11OUT
    XFreeGC(ws->dpy, idev->gc);
#endif
    if (idev->data.stk.initst.stroke.points != NULL)
	ufree((voidp)idev->data.stk.initst.stroke.points);

    /* this used to say:
     * if ( idev->data.stk.initst.record.pet1.data != NULL )
     * GKS_FREE( idev->data.stk.initst.record.pet1.data );
     * ...but... record.pet1, .pet2, .pet3, and .pet4 are a union.
     * In many cases in GKS some part of the union is refered to
     * by .pet1.??? regardless of which pet is currently appropriate.
     * This works sometimes (when the .??? part is the same in both
     * pets and in the same position, and with the same amount of
     * memory defined above it).  This is not the case here.
     * (It's probably not a good practice to assume the union will
     * always remain the way it is today anyway!)     (DWO) 
     */
    switch (idev->data.stk.initst.pet) {
    case 1:
	if (idev->data.stk.initst.record.pet1.data != NULL)
	    ufree((voidp)idev->data.stk.initst.record.pet1.data);
	break;
    case 2:
	if (idev->data.stk.initst.record.pet2.data != NULL)
	    ufree((voidp)idev->data.stk.initst.record.pet2.data);
	break;
    case 3:
	if (idev->data.stk.initst.record.pet3.data != NULL)
	    ufree((voidp)idev->data.stk.initst.record.pet3.data);
	break;
    case 4:
	if (idev->data.stk.initst.record.pet4.data != NULL)
	    ufree((voidp)idev->data.stk.initst.record.pet4.data);
	break;
    }	

    if (idev->data.stk.stkbuf != NULL)
	ufree((voidp)idev->data.stk.stkbuf);
}


/*
 * This function replaces calls to XDrawLines for stroke input devices
 * because...  XDrawLines makes nice corners and intelligently handles
 * lines that cross over themselves.  This is great if your line is
 * defined before it is drawn.  But, as in the case of our stroke, our
 * polyline is changing.  We need to be able to extend an existing line
 * and be able to erase it without leaving trash behind.  So... this
 * function will make multiple calls to XDrawLine. 
 */
#ifdef X11OUT
XgksXDrawLines(dpy, win, gc, xpts, npts, mode)
    Display        *dpy;
    Drawable        win;
    GC              gc;
#ifdef X11OUT
    XPoint         *xpts;
#else
    Gpoint         *xpts;
#endif
    int             npts;
    int             mode;
{
    int             i;

    if (npts > 1) {
	if (mode != CoordModeOrigin)
	    /* convert to CoordModeOrigin */
	    for (i = 1; i < npts; i++) {
		xpts[i].x += xpts[i - 1].x;
		xpts[i].y += xpts[i - 1].y;
	    }

	for (i = 1; i < npts; i++)
	    XDrawLine(dpy, win, gc, xpts[i - 1].x, xpts[i - 1].y,
		      xpts[i].x, xpts[i].y);

	if (mode != CoordModeOrigin)
	    /* convert back to CoordModePrevious */
	    for (i = npts - 1; i > 0; i--) {
		xpts[i].x -= xpts[i - 1].x;
		xpts[i].y -= xpts[i - 1].y;
	    }
    }
}
#endif
