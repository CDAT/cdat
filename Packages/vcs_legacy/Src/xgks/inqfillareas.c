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
#include "fillarea.h"

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[] = "$__Header$";
    static char rcsid[] = "$Id$";
#endif

extern Gptbundl xgks_xpttn[];


/*
 * INQUIRE FILLAREA FACILITIES
 *
 * errors 0, 8, 22, 23, 39 can occur.
 *
 * See also: ANSI standard p.177
 */
ginqfillfacil(ws_type, fac)
    Gchar          *ws_type;
    Gflfac         *fac;
{
    int             i;
    EWSTYPE         ewstype;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqfillfacil);

    /* check for valid ws_type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqfillfacil);
    GKSERROR(ewstype != X_WIN, 39, errginqfillfacil);

    /* set the return values */
    fac->predefined = PDF_FILL_BNDLS;
    fac->interiors.number = 4;
    fac->hatches.number = 20;

    /* get space for list */
    fac->interiors.integers = 
	(Gint *) malloc((size_t) (fac->interiors.number * sizeof(int)));
    GKSERROR((fac->interiors.integers == NULL), 300, errginqfillfacil);

    fac->hatches.integers = 
	(Gint *) malloc((size_t) (fac->hatches.number * sizeof(int)));
    GKSERROR((fac->hatches.integers == NULL), 300, errginqfillfacil);

    /* set returned index values */
    fac->interiors.integers[0] = (Gint) GHOLLOW;
    fac->interiors.integers[1] = (Gint) GSOLID;
    fac->interiors.integers[2] = (Gint) GPATTERN;
    fac->interiors.integers[3] = (Gint) GHATCH;

    for (i = 0; i < fac->hatches.number; i++)
	fac->hatches.integers[i] = -(i + 1);

    return OK;
}


/*
 * INQUIRE PREDEFINED FILLAREA REPRESENTATION
 *
 * errors 0, 8, 22, 23, 39, 80, 82 can occur.
 *
 * See also: ANSI standard p.178
 */
ginqpredfillrep(ws_type, idx, rep)
    Gchar          *ws_type;
    Gint            idx;
    Gflbundl       *rep;
{
    EWSTYPE         ewstype;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqpredfillrep);

    /* check for valid ws_type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqpredfillrep);
    GKSERROR(ewstype != X_WIN, 39, errginqpredfillrep);

    /* check for valid idx */
    GKSERROR((idx < 1 || idx > PDF_FILL_BNDLS), 80, errginqpredfillrep);

    /* set the return values */
    rep->inter = def_flbundl[idx - 1].inter;
    rep->style = def_flbundl[idx - 1].style;
    rep->colour = def_flbundl[idx - 1].colour;

    return OK;
}


/*
 * INQUIRE LIST OF FILLAREA INDICES
 *
 * errors 0, 7, 20, 25, 33, 35, 36 can occur.
 *
 * See also: ANSI standard p.161
 */
ginqfillindices(ws_id, idxlist)
    Gint            ws_id;
    Gintlist       *idxlist;
{
    WS_STATE_PTR    ws;
    int             i;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqfillindices);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqfillindices);

    /* check if this ws_id is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqfillindices);

    /* check workstation type */
    GKSERROR((ws->ewstype == MI), 33, errginqfillindices);
    GKSERROR((ws->ewstype == WISS), 36, errginqfillindices);

    /* get space for list */
    idxlist->number = 20;
    idxlist->integers = 
	(Gint *) malloc((size_t) (idxlist->number * sizeof(int)));

    /* set the indexes values */
    for (i = 0; i < idxlist->number; i++)
	idxlist->integers[i] = i + 1;

    return OK;
}


/*
 * INQUIRE FILLAREA REPRESENTATION
 *
 * errors 0, 7, 20, 25, 33, 35, 36, 80, 81 can occur.
 *
 * See also: ANSI standard p.162
 */
/*ARGSUSED*/
ginqfillrep(ws_id, idx, type, rep)
    Gint            ws_id;
    Gint            idx;
    Gqtype          type;
    Gflbundl       *rep;
{
    WS_STATE_PTR    ws;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqfillrep);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqfillrep);

    /* check if this ws_id is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqfillrep);

    /* check workstation type */
    GKSERROR((ws->ewstype == MI), 33, errginqfillrep);
    GKSERROR((ws->ewstype == WISS), 36, errginqfillrep);

    /* check for valid idx */
    GKSERROR((idx < 1 || idx > 20), 80, errginqfillrep);

    /* set the returned values */
    rep->inter = ws->flbundl_table[idx].inter;
    rep->style = ws->flbundl_table[idx].style;
    rep->colour = ws->flbundl_table[idx].colour;

    return OK;
}


/*
 * INQUIRE PATTERN FACILITIES
 *
 * errors 0, 8, 22, 23, 39 can occur.
 *
 * See also: ANSI standard p.179
 */
ginqpatfacil(ws_type, fac)
    Gchar          *ws_type;
    Gint           *fac;
{
    EWSTYPE         ewstype;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqpatfacil);

    /* check for valid ws_type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqpatfacil);
    GKSERROR(ewstype != X_WIN, 39, errginqpatfacil);

    /* set the return values */
    (*fac) = 20;

    return OK;
}


/*
 * INQUIRE PREDEFINED PATTERN REPRESENTATION
 *
 * errors 0, 8, 22, 23, 39, 85, 89 can occur.
 *
 * See also: ANSI standard p.179
 */
ginqpredpatrep(ws_type, idx, rep)
    Gchar          *ws_type;
    Gint            idx;
    Gptbundl       *rep;
{
    EWSTYPE         ewstype;
    int             i, j;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqpredpatrep);

    /* check for valid ws_type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqpredpatrep);
    GKSERROR(ewstype != X_WIN, 39, errginqpredpatrep);

    /* check for valid idx */
    GKSERROR((idx < 1 || idx > 20), 85, errginqpredpatrep);

    /* set the return values */
    *rep = xgks_xpttn[idx - 1];
    j = rep->size.x * rep->size.y;
    rep->array = (Gint *) malloc((size_t) (j * sizeof(int)));
    GKSERROR((rep->array == NULL), 300, errginqpredpatrep);

    for (i = 0; i < j; i++)
	rep->array[i] = xgks_xpttn[idx - 1].array[i];

    return OK;
}


/*
 * INQUIRE LIST OF PATTERN INDICES
 *
 * errors 0, 7, 20, 25, 33, 35, 36 can occur.
 *
 * See also: ANSI standard p.162
 */
ginqpatindices(ws_id, idxlist)
    Gint            ws_id;
    Gintlist       *idxlist;
{
    WS_STATE_PTR    ws;
    int             i;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqpatindices);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqpatindices);

    /* check if this ws_id is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqpatindices);

    /* check workstation type */
    GKSERROR((ws->ewstype == MI), 33, errginqpatindices);
    GKSERROR((ws->ewstype == WISS), 36, errginqpatindices);

    /* get space for list */
    idxlist->number = 20;
    idxlist->integers = 
	(Gint *) malloc((size_t) (idxlist->number * sizeof(int)));

    /* set the indecies values */
    for (i = 0; i < idxlist->number; i++)
	idxlist->integers[i] = i + 1;

    return OK;
}


/*
 * INQUIRE PATTERN REPRESENTATION
 *
 * errors 0, 7, 20, 25, 33, 35, 36, 85, 88 can occur.
 *
 * See also: ANSI standard p.152
 */
/*ARGSUSED*/
ginqpatrep(ws_id, idx, type, rep)
    Gint            ws_id;
    Gint            idx;
    Gqtype          type;
    Gptbundl       *rep;
{
    WS_STATE_PTR    ws;
    int             i, j;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqpatrep);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqpatrep);

    /* check if this ws_id is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqpatrep);

    /* check workstation type */
    GKSERROR((ws->ewstype == MI), 33, errginqpatrep);
    GKSERROR((ws->ewstype == WISS), 36, errginqpatrep);

    /* check for valid idx */
    GKSERROR((idx < 1 || idx > 20), 85, errginqpatrep);

    /* set the returned values */
    *rep = ws->ptbundl_table[idx - 1];
    j = rep->size.x * rep->size.y;
    rep->array = (Gint *) malloc((size_t) (j * sizeof(int)));
    GKSERROR((rep->array == NULL), 300, errginqpatrep);

    for (i = 0; i < j; i++)
	rep->array[i] = ws->ptbundl_table[idx - 1].array[i];

    return OK;
}
