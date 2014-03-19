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

#ifndef lint
    static char afsid[] = "$__Header$";
    static char rcsid[] = "$Id$";
#endif

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include "gks_implem.h"


ginqpixelarraydim(ws_id, rect, dim)
    Gint            ws_id;
    Grect          *rect;
    Gipoint        *dim;
{
    WS_STATE_PTR    ws;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqpixelarraydim);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqpixelarraydim);

    /* check if this workstation is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqpixelarraydim);

    /* check workstation type */
    GKSERROR((ws->ewstype != X_WIN), 39, errginqpixelarraydim);

    /* get the returned values from the X-server */
    (void) xXgksInqPixelarrayDim(ws, rect, dim);

    return OK;
}


/*
 * ginqpixelarray(ws_id, point, dimen, pxarr)	INQUIRE PIXEL ARRAY
 *
 * Gint ws_id;		        workstation identifier.
 * Gpoint *point;		pixel array location pointer.
 * Gipoint *dimen;		pixel array dimension pointer.
 * Gpxarray *pxarr;		OUT pixel array.
 *
 * returns all information in the parameters.
 * errors 7, 20, 25, 39 40 91 can occur.
 *
 * See also: ANSI standard p.191
 */
ginqpixelarray(ws_id, point, dimen, pxarr)
    Gint            ws_id;
    Gpoint         *point;
    Gipoint        *dimen;
    Gpxarray       *pxarr;
{
    WS_STATE_PTR    ws;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqpixelarray);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqpixelarray);

    /* check if this workstation is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqpixelarray);

    /* check workstation type */
    GKSERROR((ws->ewstype != X_WIN), 39, errginqpixelarray);

    /* check for valid dimensions of colour array */
    GKSERROR((dimen->x < 1 || dimen->y < 1), 91, errginqpixelarray);

    /* get the returned values from the X-server */
    (void) xXgksInqPixelarray(ws, point, dimen, pxarr);

    return OK;
}


/*
 * ginqpixel(ws_id, ppoint, pix)	INQUIRE PIXEL
 *
 * Gint ws_id;		        workstation identifier.
 * Gpoint *ppoint;		pixel location pointer.
 * Gint *pix;			OUT pixel colour.
 *
 * returns all information in the parameters.
 * errors 7, 20, 25, 39 40 can occur.
 *
 * See also: ANSI standard p.191
 */
ginqpixel(ws_id, ppoint, pix)
    Gint            ws_id;
    Gpoint         *ppoint;
    Gint           *pix;
{
    WS_STATE_PTR    ws;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP),
	     7, errginqpixel);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqpixel);

    /* check if this workstation is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqpixel);

    /* check workstation type */
    GKSERROR((ws->ewstype != X_WIN), 39, errginqpixel);

    /* get the returned values from the X-server */
    (void) xXgksInqPixel(ws, ppoint, pix);

    return OK;
}
