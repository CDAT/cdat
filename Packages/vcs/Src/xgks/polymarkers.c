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
 * polymarkers.c - functions and data for polymarkers
 *              gpolymarker();
 *              gsetmarkersize();
 *              gsetmarkertype();
 *              gsetmarkercolourind();
 *              gsetmarkerind();
 *              gsetmarkerrep();
 *              s_pmark_asf();
 *
 *              XgksInitGksPmarkers();
 *		XgksInitWssPmarkers();
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include "gks_implem.h"
#include "polymarkers.h"

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif


/* 
 * XgksInitGksPmarkers-- initialise gks state list polymarker stuff
 */
XgksInitGksPmarkers()
{
    xgks_state.gks_mkattr.mark = 1;		/* mark indx */
    xgks_state.gks_mkattr.type = GINDIVIDUAL;	/* type ASF */
    xgks_state.gks_mkattr.size = GINDIVIDUAL;	/* size ASF */
    xgks_state.gks_mkattr.colour = GINDIVIDUAL;	/* colour ASF */

    /* marker bundle */
    xgks_state.gks_mkattr.bundl.type = def_mkbundl[0].type;
    xgks_state.gks_mkattr.bundl.size = DEFMRKRSIZE;
    xgks_state.gks_mkattr.bundl.colour = def_mkbundl[0].colour;
}


/*
 * XgksInitWssPmarkers(ws) - send the current INDIVIDUAL attributes and BUNDLE
 *	index to the newly opened workstation ws.
 */
XgksInitWssPmarkers(ws)
    WS_STATE_PTR    ws;
{
    Gint            i;			/* Loop counter */

    /* c1143: mkbundl_table[0] never used but initialized anyway */
    ws->mkbundl_table[0] = def_mkbundl[0];

    /* c1143: initialize predefined representation bundles */
    for (i = 1; i <= PDF_MARK_BNDLS; i++)
	/* c1075: use implementation defaults, not current attributes */
	ws->mkbundl_table[i] = def_mkbundl[i - 1];

    /* c1143: initialize rest of representation bundle array */
    for (i = PDF_MARK_BNDLS + 1; i < MAX_BUNDL_TBL; i++)
	ws->mkbundl_table[i] = def_mkbundl[0];
}


/*
 * gpolymarker(num_pts, pts) - POLYMARKER
 *
 * Gint num_pts;         number of points to plot markers at.
 * Gpoint *pts;	         array of world coordinate points.
 *
 * returns 0=OK, 5, 100.
 *
 * See Also: ANSI Standard p.82
 */
gpolymarker(num_pts, pts)
    Gint            num_pts;
    Gpoint         *pts;
{
    int             cnt;
    Gpoint         *ndc_pt;
    OUT_PRIMI      *pmark;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state != GWSAC && xgks_state.gks_state != GSGOP), 
	     5, errgpolymarker);

    /* check for valid number of points */
    GKSERROR((num_pts < 1), 100, errgpolymarker);

    /* open an primitive structure */
    GKSERROR(((pmark = XgksNewPrimi()) == NULL), 300, errgpolymarker);

    pmark->pid = PMARK;
    pmark->primi.pmark.num_pts = num_pts;

    /* get memory for transformed locations */
    GKSERROR((((pmark->primi.pmark.location) =
	   (Gpoint *) malloc((size_t) (num_pts * sizeof(Gpoint)))) == NULL),
	     300, errgpolymarker);

    /* transform the locations to NDC space */
    ndc_pt = pmark->primi.pmark.location;
    for (cnt = 0; cnt < num_pts; cnt++, pts++, ndc_pt++)
	WcToNdc(pts, ndc_pt);

    /* set up attributes for pmarker from gks state list */
    pmark->primi.pmark.mkattr = xgks_state.gks_mkattr;

    XgksProcessPrimi(pmark);

    if (MO_OPENED == TRUE)
	XgksMoGraphicOutput(12, num_pts, pmark->primi.pmark.location);

    ufree((voidp)pmark->primi.pmark.location);
    ufree((voidp)pmark);

    return 0;
}


/*
 * gsetmarkersize(size) - SET MARKER SIZE SCALE FACTOR
 *
 * Gfloat size;		new size for INDIVIDUALly specified polymarkers.
 *
 * returns 0=OK or 8, 71
 *
 * See Also: ANSI Standard p.92
 */
gsetmarkersize(size)
    Gfloat          size;
{
    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgsetmarkersize);

    /* check for proper scale factor size */
    GKSERROR((size < 0.0), 71, errgsetmarkersize);
    /* ok to change scale factor */
    xgks_state.gks_mkattr.bundl.size = size;

    if (MO_OPENED == TRUE)
	XgksMoSetGraphicSize(27, size);

    return 0;
}


/*
 * gsetmarkertype(type) - SET MARKER TYPE
 *
 * Gint type;		new type for INDIVIDUALly specified markers.
 *
 * returns 0=OK or one of 8, 69
 *
 * See Also: ANSI Standard p.91
 */
gsetmarkertype(type)
    Gint            type;
{
    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgsetmarkertype);

    /* check for valid type */
    GKSERROR((type == 0), 69, errgsetmarkertype);

    /* ok to change the type */
    xgks_state.gks_mkattr.bundl.type = type;

    if (MO_OPENED == TRUE)
	XgksMoSetGraphicAttr(26, type);

    return 0;
}


/*
 * gsetmarkercolourind(colour) - SET POLYMARKER COLOUR INDEX
 *
 * Gint colour;		new colour for INDIVIDULALlY specified markers.
 *
 * returns 0=OK, or one of 8, 92
 *
 * See Also: ANSI Standard p.92
 */
gsetmarkercolourind(colour)
    Gint            colour;
{
    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgsetmarkercolourind);

    /* check for proper colour index */
    GKSERROR((colour < 0), 92, errgsetmarkercolourind);

    /* ok to change the colour */
    xgks_state.gks_mkattr.bundl.colour = colour;

    if (MO_OPENED == TRUE)
	XgksMoSetGraphicAttr(28, colour);

    return 0;
}


/*
 * gsetmarkerind(idx) - SET POLYMARKER INDEX
 *
 * Gint idx;	             new bundle table index for BUNDLEd attributes.
 *
 * returns 0=OK or one of 8, 66
 *
 * See Also: ANSI Standard p.91
 */
gsetmarkerind(idx)
    Gint            idx;
{
    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgsetmarkerind);

    /* check for valid polymarker index */
    GKSERROR((idx < 1), 66, errgsetmarkerind);

    /* change the bundle table index in the gks state table */
    xgks_state.gks_mkattr.mark = idx;

    if (MO_OPENED == TRUE)
	XgksMoSetGraphicAttr(25, idx);

    return 0;
}


/*
 * gsetmarkerrep(ws_id, idx, rep) - SET POLYMARKER REPRESENTATION
 *
 * Gint ws_id;		workstation identifier.
 * Gint idx,		polymarker bundle entry to set.
 * Gmkbundl *rep;	pmark bundle representation
 *
 * returns 0=OK or one of 7, 20, 25, 33, 35, 36, 66, 69, 70, 71, 93
 *
 * See Also: ANSI Standard p.101
 */
gsetmarkerrep(ws_id, idx, rep)
    Gint            ws_id;
    Gint            idx;
    Gmkbundl       *rep;
{
    WS_STATE_PTR    ws;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgsetmarkerrep);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgsetmarkerrep);

    /* check for open ws_id */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgsetmarkerrep);

    /* check for valid workstation category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgsetmarkerrep);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgsetmarkerrep);

    GKSERROR((WS_CAT(ws) == GWISS), 36, errgsetmarkerrep);

    /* check for valid polymarker index */
    GKSERROR((idx < 1 || idx >= MAX_BUNDL_TBL), 66, errgsetmarkerrep);

    /* check for valid marker type */
    GKSERROR((rep->type == 0), 69, errgsetmarkerrep);

    GKSERROR((!WS_MARKER_TYPE(rep->type)), 70, errgsetmarkerrep);

    /* check for valid factor size */
    GKSERROR((rep->size < 0.0), 71, errgsetmarkerrep);

    /* check for proper colour index */
    GKSERROR((!WS_AVAIL_COLOUR(ws, rep->colour)), 93, errgsetmarkerrep);

    if (ws->ewstype == MO)
	XgksMoSetLineMarkRep(ws, 52, idx, rep->type, rep->size, rep->colour);

    /* tell the workstation */
    ws->mkbundl_table[idx] = (*rep);
    return 0;
}
