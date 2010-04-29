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
 * deferral_ws.c - function to set the ramtek workstation deferral mode.
 *		gsetdeferst()
 */

#ifndef lint
    static char afsid[]="$__Header$";
    static char rcsid[]="$Id$";
#endif

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include "gks_implem.h"

/* LINTLIBRARY */

/*
 * gsetdeferst(ws_id, deferral_mode, regen_mode) - SET DEFERRAL STATE 
 *
 * Gint ws_id;			workstation identifier
 * Gdefmode deferral_mode;	how outputs can be deferred ASAP | BNIG | 
 *				BNIL | ASTI
 * Girgmode regen_mode;	implicit regeneration mode SUPPRESSED | ALLOWED
 *
 * returns: 0 = OK, or one of 7, 20, 25, 33, 35, 36, 2000
 *
 * See also: ANSI standard p.79
 */
gsetdeferst(ws_id, deferral_mode, regen_mode)
    Gint            ws_id;
    Gdefmode        deferral_mode;
    Girgmode        regen_mode;
{
    WS_STATE_PTR    ws;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKOP || xgks_state.gks_state == GGKCL), 
	     7, errgsetdeferst);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgsetdeferst);

    /* check for open ws_id */
    /* DWO 7/26/88  changed macro name from VALID_WSID */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgsetdeferst);

    /* check for valid category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgsetdeferst);
    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgsetdeferst);
    GKSERROR((WS_CAT(ws) == GWISS), 36, errgsetdeferst);

    /* check for valid deferral_mode & regen_mode */
    GKSERROR((deferral_mode != GASAP && deferral_mode != GBNIG &&
	      deferral_mode != GBNIL && deferral_mode != GASTI),
	     2000, errgsetdeferst);

    if (ws->ewstype == MO)
	XgksMoDeferWs(ws, deferral_mode, regen_mode);

    /* see if redraw is necessary */

    ws->wsdus.defmode = deferral_mode;
    ws->wsdus.irgmode = regen_mode;

    if (ws->wsdus.irgmode == GALLOWED && ws->wsdus.nframe == GYES && 
	    ws->ewstype != MO)
	XgksReDrawSegWs(ws);

    return OK;
}
