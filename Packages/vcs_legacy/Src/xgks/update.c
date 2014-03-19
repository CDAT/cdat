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


/*
 * gupdatews(ws_id, regenflag) - UPDATE WORKSTATION
 *
 * Gint  ws_id;			User selected workstation identifier.
 * Gregen regenflag;		regneration flag GPERFORM | GPOSTPONE
 *
 * Returns: 0, 20, 25, 33, 35, 36
 *
 * See also: ANSI standard p.78
 */
gupdatews(ws_id, regenflag)
    Gint            ws_id;
    Gregen          regenflag;
{
    WS_STATE_PTR    ws;			/* workstation state list ptr */
    Gint    	    No_Need_To_Update;
    Gint            priority_in_order();

    /* check proper state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgupdatews);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgupdatews);

    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgupdatews);

    /* check for valid ws category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgupdatews);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgupdatews);

    GKSERROR((WS_CAT(ws) == GWISS), 36, errgupdatews);

    /* check for valid enumeration type */
    GKSERROR((regenflag != GPERFORM && regenflag != GPOSTPONE), 2000, 
	     errgupdatews);

    if (ws->ewstype == MO)
	XgksMoUpdateWs(ws, regenflag);

    if (regenflag == GPOSTPONE || ws->wsdus.nframe == GNO)
	return OK;

    /* check for priority of the segments - DNW 10/6/94 */
    /*if (No_Need_To_Update = priority_in_order(ws))
	return OK;*/

    if (ws->ewstype != MO)
	XgksReDrawSegWs(ws);
        
    /* if necessary call user defined redraw notifying function */
    if (ws->redrawfuncp != NULL)
	(*(ws->redrawfuncp)) (ws_id, GRD_GKS);

    return OK;
}
