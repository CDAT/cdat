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
#include "text.h"

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[] = "$__Header$";
    static char rcsid[] = "$Id$";
#endif


/*
 * ginqtextfacil(ws_type, fac)
 *	INQUIRE TEXT FACILITIES
 *
 * Gchar *ws_type;		type of workstation the inquiry is about.
 * Gtxfac fac;                  returned text facility values.
 *
 * returns all information in the parameters.
 * errors 0, 8, 22, 23, 39 can occur.
 *
 * See also: ANSI standard p.171
 */
ginqtextfacil(ws_type, fac)
    Gchar          *ws_type;
    Gtxfac         *fac;
{
    EWSTYPE         ewstype;
    int             i;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqtextfacil);

    /* check for valid ws_type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqtextfacil);
    GKSERROR(ewstype != X_WIN, 39, errginqtextfacil);

    /* set the return values */
    fac->fps = DEFINED_FONT_TYPE;

    /* get space for list */
    fac->fp_list = (Gtxfp *) malloc((size_t) (fac->fps * sizeof(Gtxfp)));

    /* set the return values */
    for (i = 0; i < fac->fps; i++) {
	fac->fp_list[i].font = i + 1;
	/* |----- is this OK???? */
	fac->fp_list[i].prec = GSTROKE /* GSTRING */ ;
    }
    fac->heights = 0.0;
    fac->min_ht = 0.01;
    fac->max_ht = 1024.0;
    fac->expansions = 0.0;
    fac->min_ex = 0.001;
    fac->max_ex = 1024.0;
    fac->predefined = PDF_TEXT_BNDLS;

    return OK;
}


/*
 * ginqpredtextrep(ws_type, idx, rep) -
 *	INQUIRE PREDEFINED TEXT REPRESENTATION
 *
 * Gchar *ws_type;		workstation type this inquiry is about.
 * Gint idx;                    text index.
 * Gtxbundl *rep;               predefined text bundle values.
 *
 * returns all information in the parameters.
 * errors 0, 8, 22, 23, 39, 72, 74 can occur.
 *
 * See also: ANSI standard p.171
 */

ginqpredtextrep(ws_type, idx, rep)
    Gchar          *ws_type;
    Gint            idx;
    Gtxbundl       *rep;
{
    EWSTYPE         ewstype;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqpredtextrep);

    /* check for valid ws_type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqpredtextrep);
    GKSERROR(ewstype != X_WIN, 39, errginqpredtextrep);

    /* check for valid idx */
    GKSERROR((idx < 1 || idx > PDF_TEXT_BNDLS), 72, errginqpredtextrep);

    /* set the return values */
    rep->fp.font = def_txbundl[idx - 1].fp.font;
    rep->fp.prec = def_txbundl[idx - 1].fp.prec;
    rep->ch_exp = def_txbundl[idx - 1].ch_exp;
    rep->space = def_txbundl[idx - 1].space;
    rep->colour = def_txbundl[idx - 1].colour;

    return OK;
}


/*
 * ginqtextindices(ws_id, idxlist) - INQUIRE LIST OF TEXT INDICES
 *
 * Gint ws_id;			workstation inquiry is about.
 * Gintlist *idxlist;           list of defined text indices
 *
 * returns all information in the parameters.
 * errors 0, 7, 20, 25, 33, 35, 36 can occur.
 *
 * See also: ANSI standard p.152
 */
ginqtextindices(ws_id, idxlist)
    Gint            ws_id;
    Gintlist       *idxlist;
{
    WS_STATE_PTR    ws;
    int             i;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqtextindices);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqtextindices);

    /* check if this workstation is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqtextindices);

    /* check workstation type */
    GKSERROR((ws->ewstype == MI), 33, errginqtextindices);
    GKSERROR((ws->ewstype == WISS), 36, errginqtextindices);

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
 * ginqtextrep(ws_id, idx, type, rep) - INQUIRE LIST OF TEXT INDICES
 *
 * Gint ws_id;			workstation inquiry is about.
 * Gint idx;
 * Gqtype type;
 * Gtxbundl *rep;
 *
 * returns all information in the parameters.
 * errors 0, 7, 20, 25, 33, 35, 36, 72, 73 can occur.
 *
 * See also: ANSI standard p.152
 */
/*ARGSUSED*/
ginqtextrep(ws_id, idx, type, rep)
    Gint            ws_id;
    Gint            idx;
    Gqtype          type;
    Gtxbundl       *rep;
{
    WS_STATE_PTR    ws;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	7, errginqtextrep);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqtextrep);

    /* check if this workstation is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqtextrep);

    /* check workstation type */
    GKSERROR((ws->ewstype == MI), 33, errginqtextrep);
    GKSERROR((ws->ewstype == WISS), 36, errginqtextrep);

    /* check for valid idx */
    GKSERROR((idx < 1 || idx > 20), 72, errginqtextrep);

    /* set the returned values */
    rep->fp.font = ws->txbundl_table[idx].fp.font;
    rep->fp.prec = ws->txbundl_table[idx].fp.prec;
    rep->ch_exp = ws->txbundl_table[idx].ch_exp;
    rep->space = ws->txbundl_table[idx].space;
    rep->colour = ws->txbundl_table[idx].colour;

    return OK;
}
