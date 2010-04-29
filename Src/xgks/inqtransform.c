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
    static char afsid[] = "$__Header$";
    static char rcsid[] = "$Id$";
#endif


/*
 * ginqcurntrannum(tran) - INQUIRE CURRENT NORMALIZATION TRANSFORMATION NUMBER
 *
 * Gint	*tran;			the currently selected transformation.
 *
 * returns all information in the parameters.
 * errors 0,
 *
 * See also: ANSI Standard p.147
 */
ginqcurntrannum(tran)
    Gint           *tran;
{
    /* check proper gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqcurntrannum);

    /* set up return values */
    *tran = xgks_state.cur_ntrans;
    return OK;
}


/*
 * ginqntrannum(tranlist) - INQUIRE LIST OF NORMALIZATION TRANSFORMATION NUMBERS
 *
 * Gintlist *tranlist;       OUT list of normalization transformation numbers
 *
 *	On input, ntrans contains the maximum size for the returned list.
 *
 * returns all information in the parameters
 * errors 0, 8
 *
 * See also: ANSI Standard p.147
 */
ginqntrannum(tranlist)
    Gintlist       *tranlist;
{
    int             i;

    /* check proper gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqntrannum);

    /* allocate the memory for the list */
    tranlist->integers = 
	(Gint *) malloc((size_t) ((MAX_TRANS + 1) * sizeof(int)));

    /* set up return values */
    for (i = 0; i <= MAX_TRANS; i++)
	tranlist->integers[i] = xgks_state.ntrans_priority[i];
    tranlist->number = MAX_TRANS + 1;

    return OK;
}


/*
 * ginqntran(num, tran) - INQUIRE NORMALIZATION TRANSFORMATION
 *
 * Gint num;                    normalization transformation number
 * Gwstran *tran,		transformation structure
 *
 * returns all information in the parameters
 * errors 0, 8, 50
 *
 * See also: ANSI Standard p.148
 */
ginqntran(num, tran)
    Gint            num;
    Gwstran        *tran;
{
    /* check proper gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqntran);

    /* check for valid trans number */
    GKSERROR((num < 0 || num > MAX_TRANS), 50, errginqntran);

    /* set up the return values */
    *tran = xgks_state.ntrans_list[num].ntrans;

    return OK;
}


/*
 * ginqclip(clipping) - INQUIRE CLIPPING INDICATOR
 *
 * Gcliprec *clipping,        OUT current clipping indicator and rectangle
 *
 * all information is returned in the parameters
 * errors 0, 8
 *
 * See also: ANSI Standard p.148
 */
ginqclip(clipping)
    Gcliprec       *clipping;
{

    /* check proper gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqclip);

    /* set up return values */
    if (xgks_state.cliprec.ind == GCLIP)
	*clipping = xgks_state.cliprec;		/* c1147 */
    else {					/* ...ind == GNOCLIP */
	clipping->ind = GNOCLIP;
	clipping->rec.xmin =
	    xgks_state.ntrans_list[xgks_state.cur_ntrans].ntrans.v.xmin;
	clipping->rec.xmax =
	    xgks_state.ntrans_list[xgks_state.cur_ntrans].ntrans.v.xmax;
	clipping->rec.ymin =
	    xgks_state.ntrans_list[xgks_state.cur_ntrans].ntrans.v.ymin;
	clipping->rec.ymax =
	    xgks_state.ntrans_list[xgks_state.cur_ntrans].ntrans.v.ymax;
    }

    return OK;
}


/*
 * ginqmaxntrannum(maxtran) -
 *		INQUIRE MAXIMUM NORMALIZATION TRANSFORMATION NUMBER
 *
 * Gint	*maxtran;			maximum transformation number available
 *
 * returns all information in the parameters
 * errors 0, 8
 *
 * See also: ANSI Standard p.144
 */
ginqmaxntrannum(maxtran)
    Gint           *maxtran;
{
    /* check proper gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqmaxntrannum);

    /* set up return values */
    *maxtran = MAX_TRANS;

    return OK;
}


/*
 * ginqwstran(ws_id, wstran)
 *		INQUIRE WORKSTATION TRANSFORMATION
 *
 * Gint ws_id;			workstation identifier
 * Gwsti *wstran;               OUT requested and current transformations
 *
 * NOTE: Each of the four pointers above must point to an array of size 2 of
 *	the appropriate type.  The first element will get the lower left corner
 *	and the second element will get the upper right corner.
 *
 * returns all information in the parameters
 * errors 0, 7, 20, 25, 33, 36
 *
 * See also: ANSI Standard p.162
 */
ginqwstran(ws_id, wstran)
    Gint            ws_id;
    Gwsti          *wstran;
{
    WS_STATE_PTR    ws;

    /* check proper gks operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqwstran);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqwstran);

    /* check for open ws_id */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqwstran);

    /* proper category */
    GKSERROR((ws->ewstype == MI), 33, errginqwstran);
    GKSERROR((ws->ewstype == WISS), 36, errginqwstran);

    /* set up the return values */
    *wstran = ws->wsti;

    return OK;
}
