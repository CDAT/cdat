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
#include <string.h>
#include "gks_implem.h"

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]="$__Header$";
    static char rcsid[]="$Id$";
#endif


/*
 * INQUIRE LIST OF AVAILABLE WORKSTATION TYPES
 *
 * Note: The item wstypes->strings is allocated by GKS, and must be freed
 *	by the application after use.  However, the strings pointed to by
 *	wstypes->strings are static and should not be modified or freed.
 *
 * returns: 0, 8
 *
 * See Also: ANSI Standard p.145
 */
ginqavailwstypes(wstypes)
    Gstrlist       *wstypes;
{
    static char    *WSTList[] = {"MI", "MO", "WISS", NULL};

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqavailwstypes);

    wstypes->number = 4;
    wstypes->strings = (Gchar **) malloc((size_t) (sizeof(Gchar *) * 4));
    GKSERROR((wstypes->strings == NULL), 300, errginqavailwstypes);
    wstypes->strings[0] = WSTList[0];
    wstypes->strings[1] = WSTList[1];
    wstypes->strings[2] = WSTList[2];
    wstypes->strings[3] = WSTList[3];

    return OK;
}


/*
 * ginqdisplayspacesize(ws_type, dspsz) - INQUIRE MAXIMUM DISPLAY SURFACE SIZE
 *
 * Gchar *ws_type;		asking about this type of workstation.
 * Gdspsize *dspsz;		Out display space size structure
 *
 * errors 0, 8, 22, 23, 31, 33, 36
 *
 * See Also: ANSI Standard p.171
 */
ginqdisplayspacesize(ws_type, dspsz)
    Gchar          *ws_type;
    Gdspsize       *dspsz;
{
    EWSTYPE         ewstype;

/* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqdisplayspacesize);

/* check for valid workstation type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqdisplayspacesize);
    GKSERROR((ewstype == MO), 31, errginqdisplayspacesize);
    GKSERROR((ewstype == MI), 33, errginqdisplayspacesize);
    GKSERROR((ewstype == WISS), 36, errginqdisplayspacesize);

/* set up the return values, all X workstation look the same. */
    dspsz->units = GDC_OTHER;
    dspsz->device.x = 1280.0;
    dspsz->device.y = 1024.0;
    dspsz->raster.x = 1280;
    dspsz->raster.y = 1024;

    return OK;
}


/*
 * ginqwscategory(ws_type, cat) - INQUIRE WORKSTATION CATEGORY
 *
 * Gchar *ws_type;		asking about this type of workstation.
 * Gwscat *cat;			workstation category GOUTPUT | GINPUT | 
 *				GOUTIN | GWISS | GMO | GMI.
 *
 * errors 0, 8, 22, 23
 *
 * See Also: ANSI Standard p.170
 */
ginqwscategory(ws_type, cat)
    Gchar          *ws_type;
    Gwscat         *cat;
{
    EWSTYPE         ewstype;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqwscategory);

    /* check for valid workstation type */
    ewstype = XgksWsTypeToEnum(ws_type);

    /* set up the return values */
    switch (ewstype) {
    case WISS:
	*cat = GWISS;
	break;
    case MI:
	*cat = GMI;
	break;
    case MO:
	*cat = GMO;
	break;
    case X_WIN:
	*cat = GOUTIN;
	break;
    default:
	GKSERROR((ewstype == WST_INVALID), 22, errginqwscategory);
	break;
    }
    return OK;
}


/*
 * ginqwsclass(ws_type, class) - INQUIRE WORKSTATION CLASSIFICATION
 *
 * Gchar *ws_type;		asking about this type of workstation.
 * Gwsclass *class;		workstation class, VECTOR | RASTER | OTHER.
 *
 * errors 0, 22, 23, 31, 33, 35, 36
 *
 * See Also: ANSI Standard p.171
 */
ginqwsclass(ws_type, class)
    Gchar          *ws_type;
    Gwsclass       *class;
{
    EWSTYPE         ewstype;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqwsclass);

    /* check for valid workstation type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqwsclass);
    GKSERROR((ewstype == WISS || ewstype == MO || ewstype == MI), 39,
	     errginqwsclass);

/* set up the return values */
    if (ewstype == X_WIN)
	*class = GRASTER;
    return OK;
}


/*
 * ginqmodwsattr(ws_type, dyn) -
 *		INQUIRE DYNAMIC MODIFICATION OF WORKSTATION ATTRIBUTES
 *
 * Gchar *ws_type;		asking about this type of workstation.
 * Gmodws *dyn;                 OUT structure of dynamic modification 
 *				indications
 *
 * errors 0, 8, 22, 23, 39
 *
 * See Also: ANSI Standard p.172
 */
ginqmodwsattr(ws_type, dyn)
    Gchar          *ws_type;
    Gmodws         *dyn;
{
    EWSTYPE         ewstype;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqmodwsattr);

    /* check for valid workstation type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqmodwsattr);
    GKSERROR((ewstype != X_WIN), 39, errginqmodwsattr);

    /* set up the return values */
    dyn->line = GIMM;
    dyn->mark = GIMM;
    dyn->text = GIMM;
    dyn->fill = GIMM;
    dyn->pat = GIMM;
    dyn->colour = GIMM;
    dyn->wstran = GIRG;
    return OK;
}


/*
 * ginqdefdeferst(ws_type, def) - INQUIRE DEFUALT DEFERRAL STATE VALUES
 *
 * Gchar *ws_type;		asking about this type of workstation.
 * Gdefer *def;                 OUT default deferral state values structure
 *
 * errors 0, 8, 22, 23, 39
 *
 * See Also: ANSI Standard p.173
 */
ginqdefdeferst(ws_type, def)
    Gchar          *ws_type;
    Gdefer         *def;
{
    EWSTYPE         ewstype;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqdefdeferst);

    /* check for valid workstation type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqdefdeferst);
    GKSERROR((ewstype != X_WIN), 39, errginqdefdeferst);

    /* set up the return values */
    def->defmode = GASAP;
    def->irgmode = GALLOWED;

    return OK;
}


/*
 * ginqmaxwssttables(ws_type, tables) -
 *		INQUIRE MAXIMUM LENGTH OF WORKSTATION STATE TABLES
 *
 * Gchar *ws_type;		asking about this type of workstation.
 * Gwstables *tables;           OUT maximum length of workstation state tables
 *
 * errors 0, 8, 22, 23, 39
 *
 * See Also: ANSI Standard p.182
 */
ginqmaxwssttables(ws_type, tables)
    Gchar          *ws_type;
    Gwstables      *tables;
{
    EWSTYPE         ewstype;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqmaxwssttables);

    /* check for valid workstation type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqmaxwssttables);
    GKSERROR((ewstype != X_WIN), 39, errginqmaxwssttables);

    /* set up the return values */
    tables->line = 20;
    tables->mark = 20;
    tables->text = 20;
    tables->fill = 20;
    tables->pat = 20;
    tables->colour = XgksMaxColours(ws_type);

    GKSERROR(tables->colour < 0, 22, errginqmaxwssttables);

    return OK;
}


/*
 * ginqnumavailinput(ws_type, num) -
 *		INQUIRE NUMBER OF AVAILABLE LOGICAL INPUT DEVICES
 *
 * Gchar *ws_type;		Workstation type.
 * Gnumdev *num;                OUT number of input devices
 *
 * Errors: 0, 8, 22, 23, 38
 *
 * See Also: ANSI Standard p.184
 */
ginqnumavailinput(ws_type, num)
    Gchar          *ws_type;
    Gnumdev        *num;
{
    EWSTYPE         ewstype;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqnumavailinput);

    /* check for valid workstation type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqnumavailinput);
    GKSERROR((ewstype != X_WIN), 38, errginqnumavailinput);

    /* set up the return values */
    num->locator = 10;
    num->stroke = 10;
    num->valuator = 10;
    num->choice = 10;
    num->pick = 10;
    num->string = 10;

    return OK;
}


/*
 * Inquiry Functions for Workstation State List
 */


/*
 * ginqwsconntype(ws_id, ct) - INQUIRE WORKSTATION CONNECTION AND TYPE
 *
 * Gint ws_id;			workstation identifier.
 * Gwsct *ct;                   OUT connection identifier and workstation
 *				type ct->conn & ct->type are malloc'ed by GKS
 *				and should be free'ed by the user.
 *
 * errors 0, 7, 20, 25
 *
 * See Also: ANSI Standard p.153
 */
ginqwsconntype(ws_id, ct)
    Gint            ws_id;
    Gwsct          *ct;
{
    WS_STATE_PTR    ws;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqwsconntype);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqwsconntype);

    /* check for open workstation id */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqwsconntype);

    /* set up the return values */
    ct->conn = (Gchar *) malloc((size_t) (STRLEN(ws->conn) + 1));
    GKSERROR((ct->conn == NULL), 300, errginqwsconntype);
    STRCPY(ct->conn, ws->conn);

    ct->type = (Gchar *) malloc((size_t) (STRLEN(ws->wstype) + 1));
    GKSERROR((ct->type == NULL), 300, errginqwsconntype);
    STRCPY(ct->type, ws->wstype);

    return OK;
}


/*
 * ginqwsst(ws_id, state) - INQUIRE WORKSTATION STATE
 *
 * Gint ws_id;			workstation identifier.
 * Gwsstate *state;		current workstation state INACTIVE|ACTIVE
 *
 * errors 0, 7, 20, 25, 33, 35
 *
 * See Also: ANSI Standard p.153
 */
ginqwsst(ws_id, state)
    Gint            ws_id;
    Gwsstate       *state;
{
    WS_STATE_PTR    ws;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqwsst);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqwsst);

    /* check for open workstation id */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqwsst);

    /* check for valid workstation category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errginqwsst);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errginqwsst);

    /* set up the return values */
    *state = ws->wsstate;

    return OK;
}


/*
 * ginqwsdeferupdatest(ws_id, du) -
 *		INQUIRE WORKSTATION DEFERRAL AND UPDATE STATES
 *
 * Gint ws_id;			workstation identifier.
 * Gwsdus *du;			deferral mode.
 *
 * errors 0, 7, 20, 25, 33, 35, 36
 *
 * See Also: ANSI Standard p.154
 */
ginqwsdeferupdatest(ws_id, du)
    Gint            ws_id;
    Gwsdus         *du;
{
    WS_STATE_PTR    ws;

    /* check for proper operating state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqwsdeferupdatest);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqwsdeferupdatest);

    /* check for open workstation id */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqwsdeferupdatest);

    /* check for valid workstation category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errginqwsdeferupdatest);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errginqwsdeferupdatest);

    GKSERROR((WS_CAT(ws) == GWISS), 36, errginqwsdeferupdatest);

    /* set up the return values */
    *du = ws->wsdus;

    return OK;
}
