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
#include <string.h>
#include <stdlib.h>
#include "gks_implem.h"

/* LINTLIBRARY */

#ifdef lint
    static void			lint_malloc(n) size_t n; { n++; }
#   define malloc(n)		(lint_malloc(n), 0)
#else
    static char	afsid[] = "$__Header$";
    static char	rcsid[] = "$Id$";
#endif

extern char	*progname;


#ifdef X11OUT
/*
 * INQUIRE X ATTRIBUTES
 */
gescinqxattr(ws_id, dpy, win, gc)
    Gint            ws_id;
    Display       **dpy;
    Window         *win;
    GC             *gc;
{
    WS_STATE_ENTRY *ws;

    /* check for proper gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP),
	     7, errgescinqxattr);

    /* check for valid ws_id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgescinqxattr);

    /* check for workstation opened */
    /* ALP 8/16/88  changed macro name from VALID_WSID */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgescinqxattr);

    /* check for valid workstation category */
    GKSERROR((WS_CAT(ws) == GMO), 31, errgescinqxattr);

    GKSERROR((WS_CAT(ws) == GMI), 33, errgescinqxattr);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgescinqxattr);

    GKSERROR((WS_CAT(ws) == GWISS), 36, errgescinqxattr);

    *dpy = ws->dpy;
    *win = ws->win;
    *gc = ws->gc;

    return OK;
}


/* 
 * gescsetcolourmask( ws_id, mask ) ---- to change the colour plane mask
 * Gint ws_id;			workstation identifier.
 * unsigned long mask;		the desired colour plane mask.
 */
gescsetcolourmask(ws_id, mask)
    Gint            ws_id;
    unsigned long   mask;
{
    WS_STATE_PTR    ws;

    /* check for proper gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgescsetcolourmask);

    /* check for valid ws_id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgescsetcolourmask);

    /* check for workstation opened */
    /* ALP 8/8/88  changed macro name from VALID_WSID */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgescsetcolourmask);

    /* check for valid workstation category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgescsetcolourmask);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgescsetcolourmask);

    GKSERROR((WS_CAT(ws) == GWISS), 36, errgescsetcolourmask);

    /* set the colour plane mask value in workstation GC */
    XSetPlaneMask(ws->dpy, ws->gc, mask);

    return OK;
}


/* 
 * gescsetdcsize( ws_id, size ) ---- to change the workstation DC space size.
 * Gint ws_id;			workstation identifier.
 * Gpoint size;			the size of workstation DC space.
 */
gescsetdcsize(ws_id, size)
    Gint            ws_id;
    Gpoint          size;
{
    WS_STATE_PTR    ws;

    /* check for proper gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgescsetdcsize);

    /* check for valid ws_id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgescsetdcsize);

    /* check for workstation opened */
    /* ALP 8/8/88  changed macro name from VALID_WSID */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgescsetdcsize);

    /* check for valid workstation category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgescsetdcsize);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgescsetdcsize);

    GKSERROR((WS_CAT(ws) == GWISS), 36, errgescsetdcsize);

    /* set the colour plane mask value in workstation GC */
    ws->size = size;

    /* move the pending transformation to the current transformation */
    if (ws->wsti.wstus != GNOTPENDING) {
	ws->wsti.current.w = ws->wsti.request.w;
	ws->wsti.current.v = ws->wsti.request.v;
	ws->wsti.wstus = GNOTPENDING;
    }

    /* check and modify the workstation viewport values */
    if (ws->wsti.current.v.xmin > size.x)
	ws->wsti.current.v.xmin = size.x;
    if (ws->wsti.current.v.ymin > size.y)
	ws->wsti.current.v.ymin = size.y;
    if (ws->wsti.current.v.xmax > size.x)
	ws->wsti.current.v.xmax = size.x;
    if (ws->wsti.current.v.ymax > size.y)
	ws->wsti.current.v.ymax = size.y;

    /* update the workstation transformation matrix and clip region */
    xXgksUpdateTrans(ws);
    XgksUpdateWsClip(ws, &(xgks_state.cliprec.rec));

    /* redraw the workstation content */
    XgksXReDrawWs(ws);

    return OK;
}
#endif

/* 
 * gescstoreprimi( ws_id, store ) ---- to set store non-segment output 
 *				       primitives
 * Gint ws_id;			workstation identifier.
 * Gstore store;		
 */
gescstoreprimi(ws_id, store)
    Gint            ws_id;
    Gstore          store;
{
    WS_STATE_PTR    ws;

    /* check for proper gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgescstoreprimi);

    /* check for valid ws_id */
    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgescstoreprimi);

    /* check for workstation opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgescstoreprimi);

    /* check for valid workstation category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgescstoreprimi);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgescstoreprimi);

    GKSERROR((WS_CAT(ws) == GWISS), 36, errgescstoreprimi);

    /* set the non-segment output primitive storage value */
    ws->primi_store = store;

    return OK;
}


/* 
 * gescredrawnotify( ws_id, funcp ) ---- to set the user defined redraw 
 *					 notifying function pointer
 * Gint ws_id;			workstation identifier.
 * Gint (*funcp)();		the pointer of redraw notifying function.
 */
gescredrawnotify(ws_id, funcp)
    Gint            ws_id;
    Gint          (*funcp) ();
{
    WS_STATE_PTR    ws;

    /* check for proper gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgescredrawnotify);

    /* check for valid ws_id */
    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgescredrawnotify);

    /* check for workstation opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgescredrawnotify);

    /* check for valid workstation category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgescredrawnotify);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgescredrawnotify);

    GKSERROR((WS_CAT(ws) == GWISS), 36, errgescredrawnotify);

    /* set the colour plane mask value in workstation GC */
#ifdef ESCAPEDEBUG
    (void) fprintf(stderr, "redrawnotify=%d\n", ws->redrawfuncp);
#endif

    ws->redrawfuncp = funcp;

#ifdef ESCAPEDEBUG
    (void) fprintf(stderr, "redrawnotify=%d\n", ws->redrawfuncp);
#endif

    return OK;
}


#ifdef X11OUT
/*
 * WHAT:   Set (or unset) the backing-store feature of the X-server.
 *
 * HOW:	   Set the appropriate variable in a window-attribute structure
 *         to an appropriate value and call the X window-attribute-setting
 *	   routine.
 *
 * INPUT:  Pointer to a GKS workstation-structure.  Flag for setting or
 *	   unsetting feature.
 *
 * OUTPUT: None.
 *
 * SIDE EFFECTS:
 *	   Modifies backing-store attribute of given window.
 */
    void
gescsetbackingstore(ws_id, i)
    Gint            ws_id;		/* Workstation number */
    Gint            i;			/* true or false */
{
    Display        *dpy;
    Window          win;
    GC              gc;
    XSetWindowAttributes xswa;

    (void) gescinqxattr(ws_id, &dpy, &win, &gc);
    xswa.backing_store = i ? Always : NotUseful;
    XChangeWindowAttributes(dpy, win, (unsigned long) CWBackingStore, &xswa);
}

#endif
/*
 * WHAT:   Set the name of the application program for subsequent use in
 *	   obtaining X-resources.
 *
 * HOW:	   Copy the name to allocated storage and set the program-name
 *	   pointer to that storage.
 *
 * INPUT:  Name of the program.
 *
 * OUTPUT: None.
 *
 * SIDE EFFECTS:
 *	   Changes value of program-name pointer.
 *
 */
    void
gescsetprogname(name)
    char	*name;
{
    if (progname != NULL)
	ufree((voidp)progname);

    if ((progname = (char*)malloc((size_t) (strlen(name)+1))) != NULL)
	(void)strcpy(progname, name);
}
