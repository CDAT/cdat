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

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include "gks_implem.h"

/* LINTLIBRARY */

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char	afsid[]	= "$__Header$";
    static char	rcsid[]	= "$Id$";
#endif


/*
 * gcellarray(rect, dim, row, colour) - CELLARRAY
 * 
 * Grect *rect;		cell array rectangle pointer.
 * Gipoint *dim;	colour index array dimensions pointer.
 * Gint row;		colour index array row length.
 * Gint *colour;	colour index array.
 *
 * returns: 0=OK, or one of 5, 91
 *
 * See also: ANSI standard p.85
 */
gcellarray(rect, dim, row, colour)
    Grect          *rect;
    Gipoint        *dim;
    Gint            row;
    Gint           *colour;
{
    Gpoint          pnt;		/* temp space for tranformed points */
    Gint            i, cnt, *p1, *p2;
    OUT_PRIMI      *cell;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state != GWSAC && 
	      xgks_state.gks_state != GSGOP), 5, errgcellarray);

    /* check array size */
    GKSERROR((dim->x < 1 || dim->y < 1), 91, errgcellarray);

    /* open an primitive structure */
    GKSERROR(((cell = XgksNewPrimi()) == NULL), 300, errgcellarray);

    cell->pid = CELL_ARRAY;
    cell->primi.cell_array.dim = *dim;
    cell->primi.cell_array.rowsize = row;

    cnt = row * dim->y;

    /* now get memory for colour index array */
    GKSERROR(((cell->primi.cell_array.colour = 
	      (Gint*)malloc((size_t)(cnt*sizeof(Gint)))) == NULL),
	     300, errgcellarray);

    /* transform the WC to NDC */
    pnt = rect->ll;
    WcToNdc(&pnt, &(cell->primi.cell_array.ll));
    pnt = rect->ur;
    WcToNdc(&pnt, &(cell->primi.cell_array.ur));
    pnt.x = rect->ll.x;
    pnt.y = rect->ur.y;
    WcToNdc(&pnt, &(cell->primi.cell_array.ul));
    pnt.x = rect->ur.x;
    pnt.y = rect->ll.y;
    WcToNdc(&pnt, &(cell->primi.cell_array.lr));

    p1 = colour;
    p2 = cell->primi.cell_array.colour;
    for (i = 0; i < cnt; i++, p1++, p2++)
	*p2 = *p1;

    if (MO_OPENED == TRUE)
	XgksMoCellArray(&(cell->primi.cell_array.ll), 
			&(cell->primi.cell_array.ur),
		        &(cell->primi.cell_array.lr), 
			row, 
			cell->primi.cell_array.colour,
			&(cell->primi.cell_array.dim));

    /* process this primitive */
    XgksProcessPrimi(cell);
    ufree((voidp)cell->primi.cell_array.colour);
    ufree((voidp)cell);

    return 0;
}
