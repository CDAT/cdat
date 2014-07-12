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
 * act_ws.c - functions for workstation activation/deactivation.
 *
 *		gactivatews()
 *		gdeactivatews()
 */

/* LINTLIBRARY */

#ifndef lint
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif
                     
#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include "gks_implem.h"


/*
 * XgksNoActiveWs() - return 1 if no open workstations are active
 *	else return 0;
 */
    static
XgksNoActiveWs()
{
    Gint            i;

    for (i = 0; i < MAX_ACTIVE_WS; i++)
	if (xgks_state.activews[i].ws_id != INVALID)
	    return FALSE;
    return TRUE;
}


/* 
 * XgksAllocActiveWs (ws_id)
 *  Gint  ws_id;
 *
 * tries to allocate a slot in activews[] in gks state list and assign ws_id 
 * into the slot
 *
 * return INVALID if there's no empty slot
 *
 */
    static
XgksAllocActiveWs (ws_id,ws)
    Gint ws_id;
    WS_STATE_PTR ws;
{
   Gint i;

   for (i=0; i<MAX_ACTIVE_WS; i++) 
     	if (xgks_state.activews[i].ws_id == INVALID) {
		xgks_state.activews[i].ws_id = ws_id;
		xgks_state.activews[i].ws = ws;
		return 0;
	}
   return INVALID;
}


/*
 * XgksDeleteActiveWs (ws_id)
 *   Gint ws_id;
 *
 * delete <ws_id> from activews[] in gks state list by setting corresponding 
 * entry to INVALID
 *
 */
    static void
XgksDeleteActiveWs (ws_id)
    Gint  ws_id;
{
	Gint i;

	for (i=0; i<MAX_ACTIVE_WS; i++) {
		if (xgks_state.activews[i].ws_id == ws_id) {
			xgks_state.activews[i].ws_id = INVALID;
#ifdef X11OUT
			xgks_state.activews[i].win = INVALID;
#endif
			xgks_state.activews[i].ws = NULL;
			return;
		}
	}
}


/*
 * gactivatews(ws_id) - ACTIVATE WORKSTATION 
 *
 * Gint ws_id;		workstation to activate, must already be open.
 *
 * output primitives are sent to and segments are stored on active wss.
 *
 * returns: 0 = OK, or one of 6, 20, 25, 29, 33, 35, 43
 *
 * See also: ANSI standard p.76
 */

Gint gactivatews(ws_id)
    Gint ws_id;
{
    WS_STATE_PTR ws;

    /* check for valid workstation open and active (must be at least one) */
    GKSERROR(((xgks_state.gks_state != GWSOP) && 
	     (xgks_state.gks_state != GWSAC)), 6, errgactivatews);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgactivatews);   

    /* check for workstation opened */
    GKSERROR(((ws = OPEN_WSID(ws_id))==NULL), 25, errgactivatews);  

    /* check for workstation active */
    GKSERROR((ws->wsstate == GACTIVE), 29, errgactivatews);

    /* check for ws category */
    GKSERROR ((WS_CAT(ws) == GMI), 33, errgactivatews);

    GKSERROR ((WS_CAT(ws) == GINPUT), 35, errgactivatews);

    ws->wsstate = GACTIVE;

    /* check if max number of allowable active ws would be exceeded */
    GKSERROR ((XgksAllocActiveWs(ws_id,ws)!=0), 43, errgactivatews);

    if (xgks_state.gks_state == GWSOP)
	xgks_state.gks_state = GWSAC;		/* change operating state */

    /* Tell the Metafile facility of this change. */
    if (ws->ewstype == MO)
	XgksMoActivateWs(ws);

    return OK;
}


/*
 * gdeactivatews(ws_id) - DEACTIVATE WORKSTATION
 *
 * Gint ws_id;		workstation to deactivate, must already be active.
 *
 *	While a workstation is inactive, primitives are not sent to it nor
 *	does it store new segments.  Segments already stored on this ws are
 *	retained.
 *
 * returns: 0 = OK, or one of 3, 20, 30, 33, 35
 *
 * See also: ANSI standard p.76
 */
Gint gdeactivatews(ws_id)
    Gint            ws_id;
{
    WS_STATE_PTR    ws;

    /* first check for proper state */
    GKSERROR((xgks_state.gks_state != GWSAC), 3, errgdeactivatews);

    /* check for ws invalid */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgdeactivatews);

    /* check for  ws active (open) */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 30, errgdeactivatews);

    /* check for workstation active */
    GKSERROR((ws->wsstate == GINACTIVE), 30, errgdeactivatews);

    /* check for ws category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgdeactivatews);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgdeactivatews);

    /* Tell the Metafile facility of this change. */
    if (ws->ewstype == MO)
	XgksMoDeactivateWs(ws);

    ws->wsstate = GINACTIVE;
    XgksDeleteActiveWs(ws_id);
    if (XgksNoActiveWs() == TRUE)
	xgks_state.gks_state = GWSOP;		/* change operating state */

    return OK;
}
