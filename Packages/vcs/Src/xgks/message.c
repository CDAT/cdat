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
 * message.c - workstation message function
 *              gmessage()
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

/*
 * Make sure message will only be output to the specified workstaiton, this is
 * done by setting saving current activews[] and unset rest of the ws_id beside
 * the one we want.
 */
static WS_ENTRY	 tmp_act[MAX_ACTIVE_WS];


/*
 * gmessage(ws_id, string) - MESSAGE -
 *	display a message on the workstation in an
 *      implementation dependent way.  In this implementation it will display on
 *      the stdout device.
 *
 * Gint ws_id;          the workstation id, not used since output to stdout
 * Gchar *string;	the message to print
 *
 * returns: 0 = OK, or one of 7, 20, 25, 33, 36
 *
 * See also: ANSI standard p.80
 */
gmessage(ws_id, string)
    Gint            ws_id;
    Gchar          *string;
{
    WS_STATE_PTR    ws;
    OUT_PRIMI      *mesg;
    Gos             tmp_os;
    Gint            i;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgmessage);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgmessage);

    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgmessage);

    /* check for valid workstation category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgmessage);

    GKSERROR((WS_CAT(ws) == GWISS), 36, errgmessage);

    if (ws->ewstype == MO)
	XgksMoMessage(ws, string);

    /* open an primitive structure */
    GKSERROR(((mesg = XgksNewPrimi()) == NULL), 300, errgmessage);

    mesg->pid = XGKS_MESG;

    /* get momory for mesg string */
    GKSERROR(((mesg->primi.mesg.string =
		(Gchar *) malloc((size_t) (STRLEN(string) + 1))) == NULL),
	     300,
	     errgmessage);

    STRCPY((mesg->primi.mesg.string), string);

    /*
     * Have to avoid message being saved as part of segments so save current
     * gks_state, and if it's GSGOP, then set it to GWSAC and restore
     * gks_state, after output is done
     * 
     * Also we have to changed the xgks_state.activews[] array to only one
     * worstation active
     */
    tmp_os = xgks_state.gks_state;
    for (i = 0; i < MAX_ACTIVE_WS; i++)
	tmp_act[i] = xgks_state.activews[i];

    if (xgks_state.gks_state == GSGOP)
	xgks_state.gks_state = GWSAC;

    for (i = 0; i < MAX_ACTIVE_WS; i++) {
	if (xgks_state.openedws[i].ws_id == ws_id)
	    xgks_state.activews[0] = xgks_state.openedws[i];
	if (i != 0)
	    xgks_state.activews[i].ws_id = INVALID;
    }

    XgksProcessPrimi(mesg);

    for (i = 0; i < MAX_ACTIVE_WS; i++)
	xgks_state.activews[i] = tmp_act[i];
    xgks_state.gks_state = tmp_os;

    ufree((voidp)mesg->primi.mesg.string);
    ufree((voidp)mesg);

    return OK;
}
