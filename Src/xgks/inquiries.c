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
 *	All rights reserved.
 *
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 * 
 * inquiries.c - this file contains inquiry functions for the GKS Description table
 * and GKS state list.
 *		ginqopst
 *		ginqlevelgks
 *		ginqwsmaxnum
 *		ginqopenws
 *		ginqactivews
 *		ginqprimattr
 *		ginqindivattr
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


/*
 * INQUIRE OPERATING STATE VALUE
 */
ginqopst(state)
    Gos            *state;
{
    *state = xgks_state.gks_state;
    return OK;
}


/*
 * Inquiry Functions for GKS Description Table
 *--------------------------------------
 */

/*
 * INQUIRE LEVEL OF GKS
 */
ginqlevelgks(lev)
    Glevel         *lev;
{
    /* check for proper gks state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqlevelgks);

    /* set up the return values */
    *lev = xgks_state.level;

    return OK;
}


/*
 * INQUIRE WORKSTATION MAXIMUM NUMBERS
 *
 * errors 0, 8
 */
ginqwsmaxnum(maxws)
    Gwsmax         *maxws;
{
    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqwsmaxnum);

    /* set up the return values */
    *maxws = xgks_state.wsmax;

    return OK;
}


/*
 * Inquiry Functions for GKS State List
 *--------------------------------------
 */


/*
 * INQUIRE SET OF OPEN WORKSTATIONS
 *
 * errors 0, 8
 */
ginqopenws(wsids)
    Gintlist       *wsids;
{
    int             i, j;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqopenws);

    /* set up the return values */
    if (xgks_state.gks_state == GGKOP) {
	wsids->number = 0;
	wsids->integers = 0;
    } else {
	j = 0;
	wsids->integers = (Gint *) malloc((size_t) (MAX_OPEN_WS * sizeof(int)));
	GKSERROR((wsids->integers == NULL), 300, errginqopenws);
	for (i = 0; i < MAX_OPEN_WS; i++) {
	    if (xgks_state.openedws[i].ws_id != INVALID) {
		wsids->integers[j] = xgks_state.openedws[i].ws_id;
		j++;
	    }
	}
	wsids->number = j;
    }
    return OK;
}


/*
 * INQUIRE SET OF ACTIVE WORKSTATIONS
 *
 * errors 0, 8
 */
ginqactivews(wsids)
    Gintlist       *wsids;
{
    int             i, j;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqactivews);

    /* set up the return values */
    if (xgks_state.gks_state == GGKOP) {
	wsids->number = 0;
	wsids->integers = 0;
    } else {
	j = 0;
	wsids->integers =
	    (Gint *) malloc((size_t) (MAX_ACTIVE_WS * sizeof(int)));
	GKSERROR((wsids->integers == NULL), 300, errginqactivews);
	for (i = 0; i < MAX_ACTIVE_WS; i++) {
	    if (xgks_state.activews[i].ws_id != INVALID) {
		wsids->integers[j] = xgks_state.activews[i].ws_id;
		j++;
	    }
	}
	wsids->number = j;
    }

    return OK;
}


/*
 * INQUIRE CURRENT PRIMITIVE ATTRIBUTE VALUES
 *
 * errors 0, 8
 */
ginqprimattr(primattr)
    Gpriattr       *primattr;
{
    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqprimattr);

    /* set up the return values */
    primattr->plnindex = xgks_state.gks_lnattr.line;
    primattr->pmkindex = xgks_state.gks_mkattr.mark;
    primattr->txindex = xgks_state.gks_txattr.text;
    primattr->height = xgks_state.gks_chattr.height;
    primattr->up = xgks_state.gks_chattr.up;
    primattr->chwidth = xgks_state.gks_chattr.chwidth;
    primattr->base = xgks_state.gks_chattr.base;
    primattr->path = xgks_state.gks_chattr.path;
    primattr->align = xgks_state.gks_chattr.align;
    primattr->flindex = xgks_state.gks_flattr.fill;
    primattr->widthvec = xgks_state.gks_ptattr.widthvec;
    primattr->heightvec = xgks_state.gks_ptattr.heightvec;
    primattr->prp = xgks_state.gks_ptattr.ptp;

    return OK;
}


/*
 * INQUIRE CURRENT INDIVIDUAL ATTRIBUTE VALUES
 *
 * errors 0, 8.
 */
ginqindivattr(indivattr)
    Gindattr       *indivattr;
{
    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqindivattr);

    /* set up the return values */
    indivattr->lntype = xgks_state.gks_lnattr.bundl.type;
    indivattr->lnwidth = xgks_state.gks_lnattr.bundl.width;
    indivattr->lncolour = xgks_state.gks_lnattr.bundl.colour;
    indivattr->mktype = xgks_state.gks_mkattr.bundl.type;
    indivattr->mksize = xgks_state.gks_mkattr.bundl.size;
    indivattr->mkcolour = xgks_state.gks_mkattr.bundl.colour;
    indivattr->fp = xgks_state.gks_txattr.bundl.fp;
    indivattr->chexp = xgks_state.gks_txattr.bundl.ch_exp;
    indivattr->chspace = xgks_state.gks_txattr.bundl.space;
    indivattr->txcolour = xgks_state.gks_txattr.bundl.colour;
    indivattr->flstyle = xgks_state.gks_flattr.bundl.inter;
    indivattr->flindex = xgks_state.gks_flattr.bundl.style;
    indivattr->flcolour = xgks_state.gks_flattr.bundl.colour;
    indivattr->asflist.ln_type = xgks_state.gks_lnattr.type;
    indivattr->asflist.ln_width = xgks_state.gks_lnattr.width;
    indivattr->asflist.ln_colour = xgks_state.gks_lnattr.colour;
    indivattr->asflist.mk_type = xgks_state.gks_mkattr.type;
    indivattr->asflist.mk_size = xgks_state.gks_mkattr.size;
    indivattr->asflist.mk_colour = xgks_state.gks_mkattr.colour;
    indivattr->asflist.tx_fp = xgks_state.gks_txattr.fp;
    indivattr->asflist.tx_exp = xgks_state.gks_txattr.tx_exp;
    indivattr->asflist.tx_space = xgks_state.gks_txattr.space;
    indivattr->asflist.tx_colour = xgks_state.gks_txattr.colour;
    indivattr->asflist.fl_inter = xgks_state.gks_flattr.inter;
    indivattr->asflist.fl_style = xgks_state.gks_flattr.style;
    indivattr->asflist.fl_colour = xgks_state.gks_flattr.colour;

    return OK;
}
