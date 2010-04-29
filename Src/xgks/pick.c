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
 * University of Illinois at Urbana-Champaign
 * Department of Computer Science
 * 1304 W. Springfield Ave.
 * Urbana, IL    61801
 *
 * (C) Copyright 1987, 1988 by The University of Illinois Board of Trustees.
 * All rights reserved.
 *
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 *
 * pick.c - routines for XGKS PICK input device
 *    ginitpick()
 *    gsetpickmode()
 *    greqpick()
 *    gsamplepick()
 *
 *    ginqdefpick()
 *    ginqpickst()
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include <signal.h>
#include "gks_implem.h"

#ifdef lint
    static void lint_malloc(n) size_t n; { n++; }
#define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char     afsid[] = "$__Header$";
    static char     rcsid[] = "$Id$";
#endif

/*
 * As under current implementation only pet #1 and #3 are being supported.
 */
#define SUPPORTED_PICK_PROMPT(pet)    (pet==1 || pet==3)


/*
 * ginitpick(ws_id, dev, init, pet, area, record) - INITIALISE PICK
 *
 * Gint     ws_id;        workstation identifier
 * int        dev;        pick device number
 * Gpick    *init;        initial pick pointer
 * Gint        pet;       Prompt and echo type
 * Glimit   *area;        Echo area pointer in DC
 * Gpickrec *record;      pick data record pointer
 *
 * returns: 0, 7, 20, 25, 37, 51, 140, 141, 144, 145, 146, 152, 2000
 *
 * NOTE : As there's no implementation dependent data record being used,
 *        error 146 is not checked and will never occur.
 *
 * See Also: ANSI Standard p.127
 */
ginitpick(ws_id, dev, init, pet, area, record)
    Gint            ws_id, dev, pet;
    Gpick          *init;
    Glimit         *area;
    Gpickrec       *record;
{
    WS_STATE_PTR    ws;
    INPUT_DEV      *idev;
#ifdef X11OUT
    XGCValues       gcvalues;
#endif

    /* STEP 1: check for errors */
    /*    gks in proper state? */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginitpick);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginitpick);

    /* open workstation ? */
    GKSERROR((!(ws = OPEN_WSID(ws_id))), 25, errginitpick);

    /* check category */
    GKSERROR((WS_CAT(ws) != GOUTIN), 37, errginitpick);

    /* rectangle defintion ok */
    GKSERROR((area->xmin >= area->xmax || area->ymin >= area->ymax), 51, 
	     errginitpick);

    /* valid pick device number */
    GKSERROR((dev < 1), 140, errginitpick);

    /* prompt and echo type supported */
    GKSERROR((!SUPPORTED_PICK_PROMPT(pet)), 144, errginitpick);

    /* echo area within display space */
    GKSERROR((area->xmin < 0 || area->xmax > ws->size.x || area->ymin < 0 || 
		area->ymax > ws->size.y),
	     145, errginitpick);

    /* initial values ok */
    /* pick id can take on any value */
    GKSERROR((init->seg < 1), 152, errginitpick);

    GKSERROR((init->status != GP_OK && init->status != GP_NOPICK &&
	      init->status != GP_NONE), 2000, errginitpick);

    /* Check if the device already exist */
    if ((idev = XgksIDevLookup(ws, dev, GPICK)) == NULL) {
	/* Build a new input device structure */
#ifdef X11OUT
	gcvalues.function = GXxor;
#endif
	idev = XgksIDevNew();
#ifdef X11OUT
	idev->gc = XCreateGC(ws->dpy, ws->win, GCFunction, &gcvalues);
#endif
	idev->class = GPICK;
	idev->dev = dev;
	idev->data.pic.initst.mode = GREQUEST;	/* initialize to GREQUEST */
	idev->data.pic.initst.esw = GECHO;
	/* Add into workstation input device queue */
	XgksIDevAdd(ws, idev);
    } else {
	/* pick device must be in REQUEST mode */
	GKSERROR((idev->data.pic.initst.mode != GREQUEST), 141, errginitpick);
    }
    idev->data.pic.initst.pick = *init;
    idev->data.pic.initst.pet = pet;
    idev->data.pic.initst.e_area = *area;
    idev->data.pic.initst.record = *record;
    return 0;
}


/*
 * gsetpickmode(ws_id, dev, mode, echo) - SET PICK MODE
 *
 * Gint ws_id;            workstation identifier
 * Gint    dev;           pick device number
 * Gimode mode,           operating mode (GREQUEST | GSAMPLE | GEVENT)
 * Gesw  echo;            echo switch (GECHO | GNOECHO)
 *
 * Returns: 0, 7, 20, 25, 37, 140, 143, 2000
 *
 * See Also: ANSI Standard p.130
 */
gsetpickmode(ws_id, dev, mode, echo)
    Gint            ws_id, dev;
    Gimode          mode;
    Gesw            echo;
{
    WS_STATE_PTR    ws;
    INPUT_DEV      *idev;
#ifdef X11OUT
    XGCValues       gcvalues;
#endif

    /* STEP 1: check for errors */
    /*     gks in proper state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgsetpickmode);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgsetpickmode);

    /* workstation id valid and open */
    GKSERROR((!(ws = OPEN_WSID(ws_id))), 25, errgsetpickmode);

    /* check category */
    GKSERROR((WS_CAT(ws) != GOUTIN), 37, errgsetpickmode);

    /* valid pick device */
    GKSERROR((dev < 1), 140, errgsetpickmode);

    /* check enumerated type values */
    GKSERROR(((mode != GREQUEST && mode != GSAMPLE && mode != GEVENT) ||
	      (echo != GECHO && echo != GNOECHO)), 2000, errgsetpickmode);

    /* STEP 2: tell the workstation */
    if ((idev = XgksIDevLookup(ws, dev, GPICK)) == NULL) {
	/* We have to create one with default values */

#ifdef IDEVPICKDEBUG
	(void) fprintf(stderr, 
		   "gsetpickmode: device does not exist ..ws_id=%d, dev=%d\n",
		       ws_id, dev);
#endif

	idev = XgksIDevNew();
	idev->class = GPICK;
	idev->dev = dev;
#ifdef X11OUT
	gcvalues.function = GXxor;
	idev->gc = XCreateGC(ws->dpy, ws->win, GCFunction, &gcvalues);
#endif
	idev->data.pic.initst.mode = GREQUEST;
	idev->data.pic.initst.esw = GECHO;
	idev->data.pic.initst.pet = 1;
	idev->data.pic.initst.pick.status = GP_NOPICK;
	idev->data.pic.initst.pick.seg = INVALID;
	idev->data.pic.initst.pick.pickid = INVALID;
	idev->data.pic.initst.e_area.xmin = 0.0;
	idev->data.pic.initst.e_area.xmax = ws->size.x;
	idev->data.pic.initst.e_area.ymin = 0.0;
	idev->data.pic.initst.e_area.ymax = ws->size.y;
	idev->data.pic.initst.record.pet1.data = NULL;
	XgksIDevAdd(ws, idev);
    }
    idev->data.pic.initst.mode = mode;
    idev->data.pic.initst.esw = echo;

    if (mode == GSAMPLE || mode == GEVENT)
	idev->active = TRUE;

    return 0;
}


/*
 * greqpick(ws_id, dev, respons) - REQUEST PICK
 *
 * Gint ws_id;           workstation identifier
 * Gint    dev;          pick device number
 * Gpick *respons        Response of the pick
 *      status -- GP_OK, GP_NOPICK, GP_NONE
 *      seg    -- picked seg id
 *      pickid -- pickid
 *
 * returns: 0, 7, 20, 25, 37, 140, 141
 *
 * See Also: ANSI Standard p.134
 */
greqpick(ws_id, dev, response)
    Gint            ws_id, dev;
    Gpick          *response;
{
    WS_STATE_PTR    ws;
    INPUT_DEV      *idev;
#ifdef X11OUT
    XGCValues       gcvalues;
#endif

    /* STEP 1: check for errors */
    /*     gks in proper state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgreqpick);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgreqpick);

    /* workstation id open */
    GKSERROR((!(ws = OPEN_WSID(ws_id))), 25, errgreqpick);

    /* check category */
    GKSERROR((WS_CAT(ws) != GOUTIN), 37, errgreqpick);

    /* valid pick device */
    GKSERROR((dev < 1), 140, errgreqpick);

    /* ask the workstation for the device */
    if ((idev = XgksIDevLookup(ws, dev, GPICK)) == NULL) {
	/* We have to create one with default values */

#ifdef IDEVPICKDEBUG
	(void) fprintf(stderr, 
		       "greqpick: device does not exist: ws_id=%d, dev=%d\n",
		       ws_id, dev);
#endif

	idev = XgksIDevNew();
	idev->class = GPICK;
	idev->dev = dev;
#ifdef X11OUT
	gcvalues.function = GXxor;
	idev->gc = XCreateGC(ws->dpy, ws->win, GCFunction, &gcvalues);
#endif
	idev->data.pic.initst.mode = GREQUEST;
	idev->data.pic.initst.esw = GECHO;
	idev->data.pic.initst.pet = 1;
	idev->data.pic.initst.pick.status = GP_NOPICK;
	idev->data.pic.initst.pick.seg = INVALID;
	idev->data.pic.initst.pick.pickid = INVALID;
	idev->data.pic.initst.e_area.xmin = 0.0;
	idev->data.pic.initst.e_area.xmax = ws->size.x;
	idev->data.pic.initst.e_area.ymin = 0.0;
	idev->data.pic.initst.e_area.ymax = ws->size.y;
	idev->data.pic.initst.record.pet1.data = NULL;
	XgksIDevAdd(ws, idev);
    } else {
	GKSERROR((idev->data.pic.initst.mode != GREQUEST), 141, errgreqpick);
    }

    /* Make sure the workstation is up to date */
    /*
     * Calling the following function doesn't quite do what is needed as
     * only primitives contained in segments are redrawn: primitives which
     * lie outside segments (e.g. a gmessage() primitive) are not redrawn.
     * Unfortunately, since the gupdatews() function might clear the screen,
     * such primitives will disappear.
     *		--Steve Emmerson 11/19/91
     */
#if 0
    (void) gupdatews(ws_id, GPERFORM);
#endif
    /*
     * Insure an up-to-date display surface -- including the display of 
     * any non-segmented primitives (e.g. prompting messages).
     */
    XgksGReDrawWs(ws);

    idev->active = TRUE;

    /* wait for trigger or break */
#ifdef X11OUT
    (void) XgksSIGIO_OFF(ws->dpy);
#endif
    idev->touched	= 0;
    idev->breakhit	= 0;
#ifdef X11OUT
    while (idev->touched == False && idev->breakhit == False)
	 (void) XgksAwaitEvent(&ws, 1, -1.0);
    (void) XgksSIGIO_ON(ws->dpy);
#endif

    idev->active = FALSE;

    *response = idev->data.pic.initst.pick;

    if (idev->breakhit == TRUE)
	response->status = GP_NONE;

    return 0;
}


/*
 * gsamplepick(ws_id, dev, response) - SAMPLE PICK
 *
 * Gint ws_id;            workstation identifier
 * Gint    dev;           pick device number
 * Gpick *respons         Response of the pick
 *      status -- GP_OK, GP_NOPICK, GP_NONE
 *      seg    -- picked seg id
 *      pickid -- pickid
 *
 * Returns: 0, 7, 20, 25, 37, 140, 142
 *
 * See Also: ANSI Standard p.137
 */
gsamplepick(ws_id, dev, response)
    Gint            ws_id, dev;
    Gpick          *response;
{
    WS_STATE_PTR    ws;
    INPUT_DEV      *idev;
    Gpoint          ndcpt;

    /* STEP 1: check for errors */
    /* gks in proper state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP),
	     7, errgsamplepick);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgsamplepick);

    /* workstation id open */
    GKSERROR((!(ws = OPEN_WSID(ws_id))), 25, errgsamplepick);

    /* check category */
    GKSERROR((WS_CAT(ws) != GOUTIN), 37, errgsamplepick);

    /* valid pick device */
    GKSERROR((dev < 1), 140, errgsamplepick);
    idev = XgksIDevLookup(ws, dev, GPICK);

    /* is current mode SAMPLE ? */
    GKSERROR((idev == NULL) || (idev->data.pic.initst.mode != GSAMPLE), 142,
	     errgsamplepick);

    /* Make sure the workstation is up to date */
    (void) gupdatews(ws_id, GPERFORM);

    /*
     * Grep the current position in pick data record and figure out the
     * segment name
     */
    if (idev->breakhit == TRUE)
	return OK;

    DcToNdc(ws, &(idev->data.pic.curpos), &ndcpt);
    (void) XgksFindPickSeg(ws, &ndcpt, response, idev, 2);

    return 0;
}

/*    Pick inquiries */

/*
 * ginqpickst(ws_id, dev, type, state) - INQUIRE PICK DEVICE STATE
 * Gint  ws_id       workstation identifier
 * Gint  dev;        pick device number
 * Gqtype type;      type of return value
 * Gpickst *state    Output of current pick state
 *
 * Errors: 7, 20, 25, 37, 140, 2000
 *
 * See Also: ANSI Standard p.169
 */
ginqpickst(ws_id, dev, type, state)
    Gint            ws_id, dev;
    Gqtype          type;
    Gpickst        *state;
{
    WS_STATE_ENTRY *ws;
    INPUT_DEV      *idev;

    /* STEP 1: check for errors. */
    /* proper gks state? */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqpickst);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqpickst);

    /* check for ws_id, if correspond to opened ws */
    GKSERROR(!(ws = OPEN_WSID(ws_id)), 25, errginqpickst);

    /* valid workstation type */
    GKSERROR((WS_CAT(ws) != GOUTIN), 37, errginqpickst);

    /* check enumeration */
    GKSERROR((type != GSET && type != GREALIZED), 2000, errginqpickst);

    /* valid locator device number */
    GKSERROR((dev < 1), 140, errginqpickst);

    /* Copy the data to the user's structure */
    if ((idev = XgksIDevLookup(ws, dev, GPICK)) == NULL) {
	state->mode = GREQUEST;
	state->esw = GECHO;
	state->pet = 1;
	state->pick.status = GP_NOPICK;
	state->pick.seg = INVALID;
	state->pick.pickid = INVALID;
	state->e_area.xmin = 0.0;
	state->e_area.xmax = ws->size.x;
	state->e_area.ymin = 0.0;
	state->e_area.ymax = ws->size.y;
	state->record.pet1.data = NULL;
    } else {
	*state = idev->data.pic.initst;
	/*
	 * if idev->data.loc.initst.record pointed anywhere, it would be
	 * copied here.
	 */
    }

    return OK;
}


/*
 * ginqdefpick(type, dev, data) - INQUIRE DEFAULT DEVICE DATA
 * Gchar *type;       workstation type string
 * Gint  dev;         pick device number
 * Gdefpick *data;    output default pick device data structure
 *
 * Returns all information in the parameters.
 * Errors: 0, 8, 22, 23, 38, 140
 *
 * See Also: ANSI Standard p.188
 */
ginqdefpick(type, dev, data)
    Gchar          *type;
    Gint            dev;
    Gdefpick       *data;
{
    EWSTYPE         ewstype;
    int             i;

    /* STEP 1: check for errors. */
    /* proper gks state? */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqdefpick);

    /* valid wsid? */
    ewstype = XgksWsTypeToEnum(type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqdefpick);

    /*
     * valid workstation type (assumes all INPUT and OUTIN workstations are
     * X_WIN
     */
    GKSERROR(ewstype != X_WIN, 38, errginqdefpick);

    /* valid locator device? */
    GKSERROR((dev < 1), 140, errginqdefpick);

    /* STEP 2: set up the return values */
    data->pets.number = 2;
    data->pets.integers = (Gint *) malloc((size_t) (sizeof(Gint)
					  * data->pets.number));
    if (data->pets.integers == NULL) {
	ufree((voidp)data);
	(void) gerrorhand(300, errginqdefpick, xgks_state.gks_err_file);
	return 300;
    }
    for (i = 0; i < 2; i++)
	data->pets.integers[i] = i + 1;

    data->e_area.xmin = 0.0;
    data->e_area.xmax = WS_MAX_DCX;
    data->e_area.ymin = 0.0;
    data->e_area.ymax = WS_MAX_DCY;
    data->record.pet1.data = NULL;

    return OK;
}


#ifdef X11OUT
/*
 * XgksPicUpdatePrompt - update the locator prompt
 */
XgksPicUpdatePrompt(ws, idev, newdcpt, xmev, event_id)
    WS_STATE_ENTRY *ws;
    INPUT_DEV      *idev;
    Gpoint         *newdcpt;
    XMotionEvent   *xmev;
 /* ARGSUSED */
    int             event_id;
{
    Gpick          *data;
    Gpoint          ndcpt;
    XRectangle      rect;

    rect.x = 0;
    rect.y = 0;
    rect.width = ws->wbound.x;
    rect.height = ws->wbound.y;

    XSetClipRectangles(ws->dpy, idev->gc, 0, 0, &rect, 1, Unsorted);

    /* pick point must lie within the workstation window */
    DcToNdc(ws, newdcpt, &ndcpt);

#define IPICK idev->data.pic.initst.pick
    switch (idev->data.pic.initst.mode) {
    case GREQUEST:
	if (xmev == NULL || xmev->type != ButtonRelease)
	    break;
	idev->touched = TRUE;
	if (idev->breakhit == TRUE)
	    IPICK.status = GP_NONE;
	else
	    (void) XgksFindPickSeg(ws, &ndcpt, &(IPICK), idev, 1);
	break;
    case GSAMPLE:
	idev->data.pic.curpos = *newdcpt;
	if (xmev == NULL || xmev->type != ButtonRelease)
	    break;
	(void) XgksFindPickSeg(ws, &ndcpt, &(IPICK), idev, 0);
#undef IPICK
	break;
    case GEVENT:
	if (xmev == NULL || xmev->type != ButtonRelease)
	    break;
	data = (Gpick *) malloc(sizeof(Gpick));
	if (data == NULL) {
	    (void) gerrorhand(300, errXgksPicUpdatePrompt,
			      xgks_state.gks_err_file);
	    return 300;
	} else {
	    if (idev->breakhit == TRUE)
		data->status = GP_NONE;
	    else {
		if (XgksFindPickSeg(ws, &ndcpt, data, idev, 1) == GP_OK) {
		    data->status = GP_OK;
		} else
		    data->status = GP_NOPICK;
	    }
	    (void) XgksEnqueueEvent(ws->ws_id, idev->dev, GPICK,
				    (char *) data, -1);
	}
	break;
    default:
	break;
    }

#ifdef notdef
    /*
     * XgksFindPickSeg () will always return pickid of the first non-clip
     * primitive in the segment. This should be undefed... after prompt #2 is
     * implemented.
     */

    if (idev->data.pic.initst.pet == 2) {
	/*
	 * Call a function to figure out exact pickid from the <ndcpt> and
	 * <segment> (returned from  XgksFindPickSeg).
	 */
    }
#endif

    XFlush(ws->dpy);
    return 0;
}
#endif

/*
 * XgksPicDelete -- Free Everything in the pick-device structure ...
 *             BUT, BUT not the pointer to the structure itself
 *            calling program still needs that
 */
XgksPicDelete(ws, idev)
    WS_STATE_ENTRY *ws;
    INPUT_DEV      *idev;
{
#ifdef X11OUT
    XFreeGC(ws->dpy, idev->gc);
#endif
}
