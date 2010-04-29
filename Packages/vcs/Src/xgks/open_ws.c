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
 * gopenws - functions for opening a new workstation
 *            gopenws()
 *            gclosews()
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <string.h>
#include <stdlib.h>
#include "gks_implem.h"

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif

extern void		XgksDelAllMoSeg();


/*
 * XgksOneOpenWs ()
 *          return TRUE if there is at least one open ws
 *          else return FALSE
 */
    static
XgksOneOpenWs()
{
    Gint            i;

    for (i = 0; i < MAX_OPEN_WS; i++) {
	if (xgks_state.openedws[i].ws_id != INVALID)
	    return TRUE;
    }
    return FALSE;
}


/*
 * XgksDeleteOpenWs (ws_id)
 *   Gint ws_id;
 *
 * delete <ws_id> from openedws[] in gks state list by setting corresponding 
 * entry to INVALID
 */
    static void 
XgksDeleteOpenWs(ws_id)
    Gint            ws_id;
{
    Gint            i;

    for (i = 0; i < MAX_OPEN_WS; i++) {
	if (xgks_state.openedws[i].ws_id == ws_id) {
	    xgks_state.openedws[i].ws_id = INVALID;
#ifdef USEX11
	    xgks_state.openedws[i].win = INVALID;
#endif
	    xgks_state.openedws[i].ws = NULL;
	    return;
	}
    }
}


/*
 * XgksSetWsPtr (ws_id, ws)
 *  Gint    ws_id;
 *  WS_STATE_PTR  ws;
 *
 * set the entry in openedws[].ws_id = ws_id  of gkslist to corresponding ptr
 */
    static 
XgksSetWsPtr(ws_id, ws)
    Gint            ws_id;
    WS_STATE_PTR    ws;
{
    Gint            i;

    for (i = 0; i < MAX_OPEN_WS; i++)
	if (xgks_state.openedws[i].ws_id == ws_id) {
	    xgks_state.openedws[i].ws = ws;
	    return OK;
	}
    return INVALID;
}


/*
 * XgksAllocNewWs (ws_id)
 *  Gint  ws_id;
 *
 * tries to allocate a slot in openedws[] in gks state list and assign ws_id 
 * into the slot
 *
 * return INVALID if there's no empty slot
 */
    static 
XgksAllocNewWs(ws_id)
    Gint            ws_id;
{
    Gint            i;

    for (i = 0; i < MAX_OPEN_WS; i++)
	if (xgks_state.openedws[i].ws_id == INVALID) {
	    xgks_state.openedws[i].ws_id = ws_id;
	    return OK;
	}
    return INVALID;
}


/* WS_STATE_PTR
 * XgksNewWs ()
 *
 *   Allocate a new ws entry
 */
    static WS_STATE_PTR 
XgksNewWs()
{

    WS_STATE_PTR    new;

    if ((new = (WS_STATE_PTR) malloc(sizeof(WS_STATE_ENTRY))) 
	    != NULL) {
	new->ws_id = 0;
#ifdef USEX11
	new->dpy = NULL;
#endif
	new->wsstate = GINACTIVE;
	new->ws_is_closing = FALSE;
	new->conn = NULL;
	new->mf.any = NULL;
	new->clip = xgks_state.cliprec.rec;
	new->primi_list.pid = CLIP_REC;
	new->primi_list.seg_cnt = 0;
	new->primi_list.primi.clip.rec = xgks_state.cliprec.rec;
	new->primi_list.primi.clip.segment = FALSE;
	new->primi_list.next = NULL;
	new->primi_insert_pt = &(new->primi_list);
	new->message_pt = NULL;			/* Initialize message ptr
						 * PTR# c1001 */
	new->wbound.x = 1.0; /* default settings */
	new->wbound.y = 0.75; /* default settings */
	new->clear_regen_segs = FALSE; /* nothing to clear */

	new->seglist = NULL;
	new->seg_insertpt = NULL;
	new->seg_list_dirty = FALSE;
	new->size.x = WS_MAX_DCX;
	new->size.y = WS_MAX_DCY;
	new->primi_store = GYES;
	new->redrawfuncp = NULL;
#ifdef X11WM
	(void) XcNew(new);			/* birth color-index
						 * mapping-thingy */
#endif
    }
    return new;
}

 
/*
 * gopenws(ws_id, connection, ws_type) - OPEN WORKSTATION
 *
 * Gint  ws_id;           User selected workstation identifier.
 * Gchar *connection      Not implemented YET !!
 * Gchar *ws_type;        Which workstation type.
 *
 * Returns: 0, 8, 20, 21, 22, 23, 24, 26, 28, 42
 *
 * See also: ANSI standard p.74
 * Modified by Dean N. Williams (DNW) to use drawable widgets (or canvas) as
 * the XGKS workstation.
 */
gopenws(ws_id, connection, ws_type)
    Gint            ws_id;
    Gchar          *connection;
    Gchar          *ws_type;
{
    WS_STATE_PTR    ws;			/* workstation state list */
    EWSTYPE         ewstype;		/* corresponding enum-valued ws_type */
    char           *getenv();
    int             status;

    /* check proper state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgopenws);

    /* check for valid ws_id  ..  ws_id must be positive */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgopenws);

    /* Check for valid connection */
/* DNW - 7/15/02 - Relax this test for the correct DISPLAY. If no display is
	given then continue on with the creation of the WISS output.
    if (connection == (char *) NULL)
	connection = getenv("DISPLAY");

    GKSERROR((STRLEN(connection) == 0), 21, errgopenws);
*/

    /*
     * Check valid ws_type.  If type==NULL type = connection.
     */
    if (ws_type == (char *) NULL)
	ws_type = connection;

    GKSERROR((STRLEN(ws_type) == 0), 23, errgopenws);

    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 23, errgopenws);

    /* check workstation already open */
    GKSERROR((OPEN_WSID(ws_id) != NULL), 24, errgopenws);

    /* update <openedws> array in gks_state list */
    GKSERROR((XgksAllocNewWs(ws_id) != 0), 42, errgopenws);

    /* create the new workstation list structure */
    GKSERROR((!(ws = XgksNewWs())), 300, errgopenws);

    ws->ws_id = ws_id;
    ws->wstype = (Gchar *) malloc((size_t) (STRLEN(ws_type) + 1));
    GKSERROR(ws->wstype == NULL, 300, errgopenws);
    STRCPY(ws->wstype, ws_type);
    ws->ewstype = ewstype;

    GKSERROR(((ws->conn = (Gchar *) malloc((size_t) (STRLEN(connection) 
		+ 1))) == NULL),
	     300, errgopenws);
    STRCPY((ws->conn), connection);

    /* add this ws ptr into openedws[] array */
    (void) XgksSetWsPtr(ws_id, ws);
    switch (ewstype) {
    case X_WIN:
	/* open the workstation as X-11 window */
        if (STRCMP(ws_type, "CANVAS") == 0) { /* 9/19/94 - DNW */
           if ((status = xXgksOpenCanvasWs(ws)) != 0) {
               ufree((voidp)ws->conn);
               ufree((voidp)ws);
               XgksDeleteOpenWs(ws_id);         /* update openedws[] array */
               (void) gerrorhand(status, errgopenws, xgks_state.gks_err_file);
               return status;
           }
	} else {
	   if ((status = xXgksOpenWs(ws)) != 0) {
	       ufree((voidp)ws->conn);
	       ufree((voidp)ws);
	       XgksDeleteOpenWs(ws_id);		/* update openedws[] array */
	       (void) gerrorhand(status, errgopenws, xgks_state.gks_err_file);
	       return status;
	   }
	}
	break;
    case MI:
	if ((status = XgksMiOpenWs(ws)) != 0) {
	    ufree((voidp)ws->conn);
	    ufree((voidp)ws);
	    XgksDeleteOpenWs(ws_id);		/* update openedws[] array */
	    (void) gerrorhand(status, errgopenws, xgks_state.gks_err_file);
	    return status;
	}
	break;
    case MO:
	if ((status = XgksMoOpenWs(ws)) != 0) {
	    ufree((voidp)ws->conn);
	    ufree((voidp)ws);
	    XgksDeleteOpenWs(ws_id);		/* update openedws[] array */
	    (void) gerrorhand(26, errgopenws, xgks_state.gks_err_file);
	    return status;
	}
	xgks_state.open_mo++;
	break;
    case WISS:
	/*
	 * If opening WISS, make sure there's no other WISS being opened.
	 */
	GKSERROR((xgks_state.wiss_id != INVALID), 28, errgopenws);
	xgks_state.wiss_id = ws_id;
	break;
    default:
	break;
    }

    if (xgks_state.gks_state == GGKOP)
	xgks_state.gks_state = GWSOP;		/* change state to 1 ws open */
    XgksInitWssPlines(ws);			/* init POLYLINE BUNDLE TABLE */
    XgksInitWssPmarkers(ws);			/* POLYMARKER BUNDLE TABLE */
    (void) XgksInitWssFillArea(ws);		/* FILL AREA BUNDLE */
    XgksInitWssText(ws);			/* TEXT BUNDLE TABLE */

    /* Only meaningful for X_WIN */
    if (ewstype == X_WIN)
	XgksInitIDev(ws);			/* initialize input devices */

    /* Must follow XgksInitIDev() */
    XgksInitWssTrans(ws);			/* workstation transformation */

    return OK;
}


/*
 * gclosews(ws_id) - CLOSE WORKSTATION
 *
 * Gint ws_id;            Workstation to close.
 *
 * Returns: 0, 7, 20, 25, 29, 147
 *
 *         Error 147 (EventQueue OverFlow) is not checked
 *
 * See also: ANSI standard p.75
 */
gclosews(ws_id)
    Gint            ws_id;
{
    Gint            i;
    WS_STATE_PTR    ws;

    /* check gks in proper state */
    GKSERROR((xgks_state.gks_state == GGKOP || xgks_state.gks_state == GGKCL), 
	     7, errgclosews);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgclosews);

    /* check for workstation not open yet */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgclosews);

    /* check for workstation still active */
    GKSERROR((ws->wsstate == GACTIVE), 29, errgclosews);

    /*
     * delete all input devices associated with this ws
     * NOTE: This is what caused ptr c1120 (key presses as ws closes cause
     *       X error).  New events entered after the gflushevents but 
     *       before the ws was completely gone.  This left an event in the
     *       queue pointing to a non-existant ws.  Also - the devices on a
     *       workstation were never being handled when the workstation
     *       closed.  All the memory dynamically allocated for devices was
     *       being lost.  So... we call XgksIDevDelete to destroy the
     *       devices so no new events will enter and also free the memory
     *       used by the devices.  (XgksIDevDelete was written, but never
     *       called.  This seems to be the place it was meant for.) (DWO)
     */
    if (ws->ewstype != MO && ws->ewstype != WISS)
	XgksIDevDelete(ws);

    /* XgksDeleteAllSeg was moved after XgksIDevDelete to stop the devices
     * from flashing as the ws closes.  XgksDeleteAllSeg calls XgksIDevDisable
     * which erases all the devices, then deletes the segments, then calls
     * XgksIDevEnable to redraw all the devices.  This caused them to flash.
     * Now, the devices are deleted before the segments.
     * Delete all segments from this ws, and at the same time
     * delete this ws from all segments associated with it
     * (XgksDelAllWsSeg would be a better name)
     */
    if (ws->ewstype != MO)
	XgksDeleteAllSeg(ws);
    else
	XgksDelAllMoSeg(ws);

    /*
     * mark the ws as closing to prevent any interrupts to jump
     * in while the ws is in an unstable state.  (if a change
     * focus came in after the xXgksCloseWs, but before the
     * completion of gclosews, GKS used to try to inquire the
     * color map of an X window that was already gone)  DWO
     */
    ws->ws_is_closing = TRUE;

    /* House keeping for workstation consistency */
    switch (ws->ewstype) {
    case X_WIN:
	/* tell x-window system to destroy the window */
	if (strcmp(ws->wstype,"CANVAS") != 0)
	   (void) xXgksCloseWs(ws);

	/* Flush the event queue for this workstation */
	(void) gflushevents(ws_id, GNCLASS, 0);
	break;
    case MI:
	XgksMiCloseWs(ws);
	break;
    case MO:
	XgksMoCloseWs(ws);
	xgks_state.open_mo--;
	break;
    case WISS:
	xgks_state.wiss_id = INVALID;
	break;
    default:
	break;
    }

    /* take workstation out of list and free it up */
    XgksDeleteOpenWs(ws_id);			/* remove ws from openedws
						 * array in gkslist */
    ufree((voidp)ws->conn);
    ufree((voidp)ws->wstype);
    /* HVO: commented since always giving seg fault on wrong set_colour_rep */
/*     ufree((voidp)ws->set_colour_rep); */

    for (i = 0; i < MAX_BUNDL_TBL; i++) {
        if (ws->ptbundl_table[i].array != NULL)
            ufree((voidp) ws->ptbundl_table[i].array );
            ws->ptbundl_table[i].array = NULL;
    }
    if (ws->ewstype == X_WIN) (void) XcEnd(ws);    /* free color-index */
    ufree((voidp)ws);

    /* clean up stuff */
    if (XgksOneOpenWs() == FALSE)		/* that was the last
						 * workstation open */
	xgks_state.gks_state = GGKOP;		/* set state to gks open no
						 * ws open */

    /* everything went ok */
    return OK;
}


/*
 * gclearws(ws_id, control_flag) - CLEAR WORKSTATION
 *
 * Gint ws_id;            workstation to clear
 * Gclrflag control_flag;    clear display GCONDITIONALLY | GALWAYS
 *
 * returns: 0 = OK, or one of 6, 20, 25, 33, 35
 *
 * See also: ANSI standard p.77
 */
gclearws(ws_id, control_flag)
    Gint            ws_id;
    Gclrflag        control_flag;
{
    WS_STATE_PTR    ws;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state != GWSOP && xgks_state.gks_state != GWSAC), 
	     6, errgclearws);

    /* Check for valid ws_id  ..  ws_id must be positive */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgclearws);

    /* check for valid ws_id */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgclearws);

    /* check for proper category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgclearws);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgclearws);

    if (ws->ewstype == MO)
	XgksMoClearWs(ws, control_flag);

    /* STEP 2: tell the workstation */
    if (control_flag == GCONDITIONALLY && ws->wsdus.dspsurf == GEMPTY)
	return OK;

    if (ws->ewstype != MO) {
	XgksIDevDisable(ws);
	(void) xXgksClearWs(ws);
    }
    if (ws->wsti.wstus == GPENDING)
	XgksUnpendPendingTrans(ws);

    if (ws->ewstype != MO) {
	XgksDeletePrimi(&(ws->primi_list), &(ws->primi_insert_pt));
	ws->message_pt = NULL;			/* Clear any messages   PTR
						 * c1001 */
	XgksDeleteAllSeg(ws);
	XgksIDevEnable(ws);
    }
    ws->wsdus.nframe = GNO;
    ws->wsdus.dspsurf = GEMPTY;

    return OK;
}


/*
 * WS_STATE_PTR
 * XgksValidWsId (ws_id)
 *   Gint  ws_id;
 *
 * See if <ws_id> is already opened in the openedws[] array, if yes return
 * pointer to the ws_state_list, else return NULL
 */
    WS_STATE_PTR
XgksValidWsId(ws_id)
    Gint            ws_id;
{
    Gint            i;

    for (i = 0; i < MAX_OPEN_WS; i++)
	if (xgks_state.openedws[i].ws_id == ws_id)
	    return xgks_state.openedws[i].ws;

    return NULL;
}


/*
 * Convert a workstation type string to the enumerated version
 */
    EWSTYPE
XgksWsTypeToEnum(ws_type)
    Gchar          *ws_type;
{
    if (STRCMP(ws_type, "MI") == 0) {
	return MI;
    } else if (STRCMP(ws_type, "MO") == 0) {
	return MO;
    } else if (STRCMP(ws_type, "WISS") == 0) {
	return WISS;
    } else if (STRCMP(ws_type, "CANVAS") == 0) { /* 9/19/94 - DNW */
        return X_WIN;
    } else if (ws_type == NULL) {
	return X_WIN;
    } else if (strchr(ws_type, ':') != 0) {
	return X_WIN;
    } else {
	return WST_INVALID;
    }
}


/*
 * XgksWsCategory (ws)
 * WS_STATE_PTR ws;  - return the categorey of the workstation pointed
 *               by the state pointer
 */
    Gwscat
XgksWsCategory(ws)
    WS_STATE_PTR    ws;
{
    if (ws->ewstype == X_WIN)
	return GOUTIN;
    else if (ws->ewstype == WISS)
	return GWISS;
    else if (ws->ewstype == MI)
	return GMI;
    else if (ws->ewstype == MO)
	return GMO;
    else
	return (Gwscat) INVALID;
}


/*
 * XgksXReDrawWs (ws)
 *  WS_STATE_PTR ws;        this is an X initiated redraw.
 */
XgksXReDrawWs(ws)
    WS_STATE_PTR    ws;
{
    OUT_PRIMI      *primi = &(ws->primi_list);
    WS_SEG_LIST    *seg_ptr = ws->seglist;
    Gint            cnt;

    Glimit          tmp_wsclip;		/* Have to save current ws clip value */
    Glimit          tmp_nonsegclip;	/* Have to save current non_segment
					 * clip value */

    tmp_wsclip = ws->clip;			/* Save the current ws->clip
						 * value */
    while (primi != NULL) {
	XgksReDrawWs(ws, primi);
	tmp_nonsegclip = ws->clip;		/* save clip region for
						 * non-segment primitives */
	cnt = primi->seg_cnt;
	while (cnt-- > 0) {
	    XgksReDrawSeg(ws, seg_ptr->seg);
	    seg_ptr = seg_ptr->next;
	}
	ws->clip = tmp_nonsegclip;		/* restore non-segment
						 * primitive clip regions */
	xXgksUpdateClip(ws);
	primi = primi->next;
    }
    ws->clip = tmp_wsclip;			/* restore the old ws->clip
						 * value */
    xXgksUpdateClip(ws);
}


/*
 * Redraw the display surface of a workstation.  
 *
 * This is a GKS-initiated redraw, as opposed to an X-initiated redraw.
 * The difference is that invalid segments might have to be deleted, 
 * transformations updated, the display cleared, etc.
 *
 * This routine redraws segmented primitives *and* non-segmented primitives
 * (in redrawing non-segmented primitives, this routine differs from 
 * XgksReDrawSegWs(), which doesn't).  This action is useful when, for
 * example, the display needs to be redrawn just prior to a REQUEST PICK
 * action as any prompting message will not be deleted.
 *
 * This routine is a merging of XgksReDrawSegWs() and XgksXReDrawWs().
 *
 *	--Steve Emmerson 11/20/91
 */
XgksGReDrawWs(ws)
    WS_STATE_PTR    ws;
{
    OUT_PRIMI      *primi	= &(ws->primi_list);
    WS_SEG_LIST    *seg_ptr	= ws->seglist;
    Glimit          tmp_wsclip;

    XgksIDevDisable(ws);			/* disable input devices */

    /* Delete all invalid segments. */
    XgksCleanUpWsSegList(ws);

    /*
     * If the display surface is not empty and a new frame is needed, then
     * clear it.
     */
    if (ws->wsdus.dspsurf == GNOTEMPTY && ws->wsdus.nframe == GYES)
	(void) xXgksClearWs(ws);

    /* The display surface is definitely empty, and no new frame is needed. */
    ws->wsdus.dspsurf	= GEMPTY;
    ws->wsdus.nframe	= GNO;

    /* Unpend any pending transformations. */
    if (ws->wsti.wstus == GPENDING)
	XgksUnpendPendingTrans(ws);

    /* Update the segment list according to priority. */
    if (ws->seg_list_dirty == TRUE)
	XgksUpdateWsSegList(ws);

    tmp_wsclip = ws->clip;			/* Save the current ws->clip
						 * value */
    while (primi != NULL) {
	Glimit          tmp_nonsegclip;

	XgksReDrawWs(ws, primi);
	tmp_nonsegclip = ws->clip;		/* save clip region for
						 * non-segment primitives */
	while (seg_ptr != NULL) {
	    XgksReDrawSeg(ws, seg_ptr->seg);
	    seg_ptr = seg_ptr->next;
	}
	ws->clip = tmp_nonsegclip;		/* restore non-segment
						 * primitive clip regions */
	xXgksUpdateClip(ws);
	primi = primi->next;
    }
    ws->clip = tmp_wsclip;			/* restore the old ws->clip
						 * value */
    xXgksUpdateClip(ws);

    XgksIDevEnable(ws);				/* enable input devices */
}


/*
 * XgksOutputToWs (primi)
 * OUT_PRIMI *primi    - output primitive to all, ALL active outputable 
 *			 workstations
 *
 */
XgksOutputToWs(primi)
    OUT_PRIMI      *primi;
{
    Gint            cnt;

    for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {	/* send to all active
						 * workstations */
	if (xgks_state.activews[cnt].ws_id != INVALID) {
	    switch (xgks_state.activews[cnt].ws->ewstype) {
	    case X_WIN:
		XgksDrawToWs(xgks_state.activews[cnt].ws, primi);
		break;
	    default:
		break;
	    }
	}
    }
}


/* XgksReDrawWs (ws, primi)
 * WS_STATE_PTR ws;        -- decode primi->pid and call the corresponding 
 *			      X-routine
 * OUT_PRIMI *primi;           to draw out the primitive to window
 *
 * Will not disable any input devices
 */
XgksReDrawWs(ws, primi)
    WS_STATE_PTR    ws;
    OUT_PRIMI      *primi;
{
    switch (primi->pid) {
    case PLINE:
	(void) xXgksPolyLine(ws, &(primi->primi.pline));
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case PMARK:
	(void) xXgksPolyMarker(ws, &(primi->primi.pmark));
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case FILL_AREA:
	(void) xXgksFillArea(ws, &(primi->primi.fill_area));
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case CLIP_REC:
	if (primi->primi.clip.segment == TRUE) {
	    XgksUpdateWsClip(ws, &(primi->primi.clip.rec));
	} else {
	    ws->clip = primi->primi.clip.rec;
	    xXgksUpdateClip(ws);
	}
	break;
    case TEXT:
	(void) xXgksText(ws, &(primi->primi.text));
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case XGKS_MESG:
#ifdef X11OUT
	(void) xXgksMesg(ws, &(primi->primi.mesg));
#endif
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case CELL_ARRAY:
	(void) xXgksCellarray(ws, &(primi->primi.cell_array));
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case GDP:
	break;
    }
}


/* XgksDrawToWs (ws, primi)
 * WS_STATE_PTR ws;        -- decode primi->pid and call the corresponding 
 *			      X-routine
 * OUT_PRIMI *primi;          to draw out the primitive to window
 *
 * Well disable all input devices before output and Enable them AFTER output
 */
XgksDrawToWs(ws, primi)
    WS_STATE_PTR    ws;
    OUT_PRIMI      *primi;
{
    XgksIDevDisable(ws);			/* Disable all input devices */
    switch (primi->pid) {
    case PLINE:
	(void) xXgksPolyLine(ws, &(primi->primi.pline));
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case PMARK:
	(void) xXgksPolyMarker(ws, &(primi->primi.pmark));
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case FILL_AREA:
	(void) xXgksFillArea(ws, &(primi->primi.fill_area));
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case CLIP_REC:
	if (primi->primi.clip.segment == TRUE) {
	    XgksUpdateWsClip(ws, &(primi->primi.clip.rec));
	} else {
	    ws->clip = primi->primi.clip.rec;
	    xXgksUpdateClip(ws);
	}
	break;
    case TEXT:
	(void) xXgksText(ws, &(primi->primi.text));
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case XGKS_MESG:
#ifdef X11OUT
	(void) xXgksMesg(ws, &(primi->primi.mesg));
#endif
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case CELL_ARRAY:
	(void) xXgksCellarray(ws, &(primi->primi.cell_array));
	ws->wsdus.dspsurf = GNOTEMPTY;
	break;
    case GDP:
	break;
    }
    XgksIDevEnable(ws);				/* Disable all input devices */
}


/*
 * XgksAppendWsPrimi (primi)
 * OUT_PRIMI *primi;     - append the primitive entry to workstation non 
 *			   segment list
 *
 */
XgksAppendWsPrimi(primi)
    OUT_PRIMI      *primi;
{
    Gint            cnt;

    for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {	/* send to all active
						 * workstations */
	if ((xgks_state.activews[cnt].ws_id != INVALID)
		&& (xgks_state.activews[cnt].ws->primi_store == GYES)
		&& (xgks_state.activews[cnt].ws->ewstype != MO))
	    if (primi->pid == XGKS_MESG)	/* Check if prim. is MESG */

		(void) XgksInsertMesgPrimi(xgks_state.activews[cnt].ws, primi);
	    else				/* message primitives have to
						 * be inserted differently */
		XgksInsertPrimi(&(xgks_state.activews[cnt].ws->primi_insert_pt),
				primi);
    }
}


/*
 * XgksAppendWsClip () - check if opened_ws->primi_insert_pt->pid == CLIP_REC
 *               if yes, update clip.rec to cliprec.rec, else
 *               append a new CLIP_REC to the list
 */
XgksAppendWsClip(rec)
    Glimit         *rec;
{
    Gint            cnt;
    OUT_PRIMI      *clip;

    for (cnt = 0; cnt < MAX_OPEN_WS; cnt++) {
	if (xgks_state.openedws[cnt].ws_id != INVALID) {
	    if (xgks_state.openedws[cnt].ws->primi_insert_pt->pid
		    == CLIP_REC) {

		XgksWsWinInterset(xgks_state.openedws[cnt].ws, rec,
		  &(xgks_state.openedws[cnt].ws->primi_insert_pt->primi.clip
		    .rec));
	    } else {
		clip = XgksNewPrimi();
		clip->pid = CLIP_REC;
		clip->primi.clip.segment = FALSE;
		XgksWsWinInterset(xgks_state.openedws[cnt].ws, rec,
				  &(clip->primi.clip.rec));
		XgksInsertPrimi(&(xgks_state.openedws[cnt].ws
				  ->primi_insert_pt), clip);
	    }

	    XgksUpdateWsClip(xgks_state.openedws[cnt].ws, rec);
	}
    }
}
