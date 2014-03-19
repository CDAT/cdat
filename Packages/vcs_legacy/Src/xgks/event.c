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
 * Event Queue maintenance routines.
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <sys/types.h>		/* for pid_t */
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <math.h>		/* for ceil() */
#include <assert.h>		/* for assert() */
#include "gks_implem.h"

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char	afsid[] = "$__Header$";
    static char	rcsid[] = "$Id$";
#endif

#ifndef Boolean
#include <stdbool.h>
#define Boolean bool
#define TRUE 1
#define FALSE 0
#endif

static Boolean	Waiting;	/* True if await event is waiting */


/*
 * enqueue an event.  Memory pointed to by data must be malloc'ed by the
 * caller but NOT freeed.
 */
XgksEnqueueEvent(ws, dev, class, data, event_id)
    Gint            ws;
    Gint            dev;
    Giclass         class;
    char           *data;
    int             event_id;
{
    EQEntry        *e;

    if ((e = (EQEntry *) malloc(sizeof(EQEntry))) == NULL) {
	(void) gerrorhand(300, errXgksEnqueueEvent,
			  xgks_state.gks_err_file);
	return 300;
    }
    e->id = event_id;
    e->event.ws = ws;
    e->event.dev = dev;
    e->event.gclass = class;
    e->data = data;
    e->next = NULL;

    if (xgks_state.event_queue_head == NULL) {
	xgks_state.event_queue_head = e;
	xgks_state.event_queue_tail = e;
    } else {
	xgks_state.event_queue_tail->next = e;
	xgks_state.event_queue_tail = e;
    }

    /* Insure that an AWAIT EVENT will stop waiting */
    Waiting	= FALSE;

    return 0;
}


/*
 *  AWAIT EVENT 
 *
 * Await any event on all applicable workstations.
 *
 * Returns: 0, 7, 147, 151
 */
    int
gawaitevent(timeout, event)
    Gfloat          timeout;
    Gevent         *event;
{
    int		    iws;		/* workstation index */
    int		    nws;		/* number of workstations in list */
    WS_STATE_ENTRY
		   *wsbuf[MAX_OPEN_WS];	/* buffer for workstation-list */
    WS_STATE_ENTRY
		  **wslist	= wsbuf;/* position in workstation-list */

    /* Check for proper state */
    GKSERROR((xgks_state.gks_state == GGKCL
	      || xgks_state.gks_state == GGKOP), 7, errgawaitevent);

    /* Check for valid timeout value */
    GKSERROR((timeout < 0.0), 151, errgawaitevent);

    /* If an event is in the queue, return it. */
    if (xgks_state.event_queue_head != NULL) {
	xgks_state.CurEvent = *xgks_state.event_queue_head;
	*event = xgks_state.CurEvent.event;
	/*
	 * WARNING: I think xgks_state.event_queue_head should be free()ed 
	 * here.  SRE 1993-3-13
	ufree((voidp)xgks_state.event_queue_head);
	 */
	xgks_state.event_queue_head = xgks_state.event_queue_head->next;
	return 0;
    }

    /* If the timeout is zero, return "no class" since the queue is empty. */
    if (timeout == 0.0) {
	xgks_state.CurEvent.event.gclass = GNCLASS;
	event->gclass = GNCLASS;
	return 0;
    }

    /*
     * Establish the list of workstations to wait upon.
     */
    for (iws = 0; iws < MAX_OPEN_WS; ++iws) {
	if (xgks_state.openedws[iws].ws_id != INVALID) {
	    /*
	     * The workstation is active.
	     */
	    int			active_device;	/* active device? */
	    INPUT_DEV		*id;
	    WS_STATE_ENTRY	*ws	= xgks_state.openedws[iws].ws;

	    /*
	     * Determine whether or not the workstation has an active
	     * input device (i.e. one in SAMPLE or EVENT mode).
	     */
	    for (id = ws->in_dev_list; id != NULL; id = id->next)
		if (id->active != FALSE)
		    break;
	    active_device	= id != NULL;

	    if (active_device) {
		/*
		 * Active workstation with active input device.  Disable
		 * SIGIO for the workstation and add the workstation to the
		 * workstation-list.
		 */
		assert(wslist < wsbuf + MAX_OPEN_WS);
#ifdef X11OUT
		(void) XgksSIGIO_OFF(ws->dpy);
#endif
		*wslist++	= ws;
	    }
	}					/* active workstation */
    }						/* workstation loop */
    nws	= wslist - wsbuf;

    /* 
     * Await and process an event from any workstation in the workstation-
     * list.  The boolean flag "Waiting" will be cleared when 
     * XgksEnqueueEvent(), which is in this file, is called by the event-
     * processing machinery of XgksAwaitEvent().  XgksAwaitEvent() returns 0
     * if a timeout occured.
     */
    Waiting = TRUE;
#ifdef X11OUT
    while (Waiting == TRUE && XgksAwaitEvent(wsbuf, nws, (double)timeout) != 0)
	;					/* EMPTY */
#endif
    /*
     * Re-enable SIGIO on the workstations.
     */
#ifdef X11OUT
    for (wslist = wsbuf; wslist < wsbuf + nws; ++wslist)
	(void) XgksSIGIO_ON((*wslist)->dpy);
#endif
    /*
     * Set up the return values.  The data portion of the event is freed
     * in the individual GET...EVENT functions.
     */
    if (xgks_state.event_queue_head != NULL) {
	xgks_state.CurEvent = *xgks_state.event_queue_head;
	*event = xgks_state.CurEvent.event;
	ufree((voidp)xgks_state.event_queue_head);
	xgks_state.event_queue_head = xgks_state.CurEvent.next;
    } else {
	xgks_state.CurEvent.event.gclass = GNCLASS;
	event->gclass = GNCLASS;
    }

    return 0;
}


/*
 *  FLUSH DEVICE EVENTS
 *
 * NOTE: if called with class == GNCLASS then all events for the 
 *	specified workstation will be flushed.
 *
 * Returns: 0, 7, 20, 25, 38, 140, 147
 */
gflushevents(ws_id, class, dev)
    Gint            ws_id;
    Giclass         class;
    Gint            dev;
{
    WS_STATE_ENTRY *ws;
    EQEntry        *e, *prev;

    /* check gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgflushevents);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgflushevents);

    /* open workstation */
    GKSERROR(!(ws = OPEN_WSID(ws_id)), 25, errgflushevents);

    /* valid workstation category */
    GKSERROR((WS_CAT(ws) != GOUTIN && WS_CAT(ws) != GINPUT), 38, 
	     errgflushevents);

    /* valid device number */
    GKSERROR((class != GNCLASS) && (dev < 1), 140, errgflushevents);

    /* search the event queue for events to delete */
    for (e = xgks_state.event_queue_head, prev = (EQEntry *) NULL;
	 e != (EQEntry *) NULL;)

	if (((class == GNCLASS) && (e->event.ws == ws_id)) || 
	    ((e->event.ws == ws_id) && (e->event.gclass == class) && 
	    (e->event.dev == dev))) {

	    if (e == xgks_state.event_queue_head) {
		xgks_state.event_queue_head = e->next;
		if (e->data != NULL)
		    ufree((voidp)e->data);
		ufree((voidp)e);
		e = xgks_state.event_queue_head;
	    } else {
		prev->next = e->next;
		if (e->data != NULL)
		    ufree((voidp)e->data);
		ufree((voidp)e);
		e = prev->next;
	    }
	} else {
	    prev = e;
	    e = e->next;
	}

    return 0;
}


/*
 * INQUIRE INPUT QUEUE OVERFLOW
 *
 * Returns: 0, 7, 148, 149
 */
ginqinputoverflow(overflow)
    Gqueue         *overflow;
{
    /* check gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqinputoverflow);

    overflow->gclass = GNCLASS;
    overflow->devno = 0;

    return 148;
}


/*
 * INQUIRE MORE SIMULTANEOUS EVENTS
 *
 * Errors: 0, 7
 */
ginqmoreevents(events)
    Gsimultev      *events;
{
    /* check gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqmoreevents);
    /*
     * Each event has an ID that is set when  the event is generated.  If two
     * events have the same ID, then they were produced simultaneously by the 
     * same trigger!
     */
    if ((xgks_state.event_queue_head != NULL) &&
	    (xgks_state.CurEvent.id == xgks_state.event_queue_head->id)) {
	*events = GMORE;
    } else {
	*events = GNOMORE;
    }

    return 0;
}


/*
 * GET LOCATOR
 *
 * returns 0, 7, 150
 */
ggetloc(response)
    Gloc           *response;
{
    /* check gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errggetloc);

    /* Is the Current Event the proper a Locator event? */
    GKSERROR((xgks_state.CurEvent.event.gclass != GLOCATOR), 150, errggetloc);

    /* return the event data */
    *response = *(Gloc *)xgks_state.CurEvent.data;

    ufree((voidp)xgks_state.CurEvent.data);

    return 0;
}


/*
 * GET STROKE
 *
 * returns 0, 7, 150
 */
ggetstroke(response)
    Gstroke        *response;
{
    /* check gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errggetstroke);

    /* Is the Current Event the proper a Stroke event? */
    GKSERROR((xgks_state.CurEvent.event.gclass != GISTROKE), 150, 
	     errggetstroke);

    /* return the event data */
    *response = *(Gstroke *)xgks_state.CurEvent.data;

    ufree((voidp)xgks_state.CurEvent.data);

    return 0;
}


/*
 * GET CHOICE
 *
 * returns 0, 7, 150
 */
ggetchoice(response)
    Gchoice        *response;
{
    /* check gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errggetchoice);

    /* Is the Current Event the proper a Choice event? */
    GKSERROR((xgks_state.CurEvent.event.gclass != GCHOICE), 150, errggetchoice);

    /* return the event data */
    *response = *(Gchoice *) xgks_state.CurEvent.data;

    ufree((voidp)xgks_state.CurEvent.data);

    return 0;
}


/*
 * GET PICK
 * 
 * returns 0, 7, 150
 */
ggetpick(response)
    Gpick          *response;
{
    /* check gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errggetpick);

    /* Is the Current Event the proper a Pick event? */
    GKSERROR((xgks_state.CurEvent.event.gclass != GPICK), 150, errggetpick);

    /* return the event data */
    *response = *(Gpick *) xgks_state.CurEvent.data;

    ufree((voidp)xgks_state.CurEvent.data);

    return 0;
}


/*
 * GET VALUATOR
 * 
 * returns 0, 7, 150
 */
ggetval(response)
    Gfloat         *response;
{
    /* check gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errggetval);

    /* Is the Current Event the proper a valuator event? */
    GKSERROR((xgks_state.CurEvent.event.gclass != GVALUATOR), 150, errggetval);

    /* return the event data */
    *response = *(Gfloat *) xgks_state.CurEvent.data;

    ufree((voidp)xgks_state.CurEvent.data);

    return 0;
}


/*
 * GET STRING
 * 
 * returns 0, 7, 150
 */
ggetstring(response)
    Gchar          *response;
{
    /* check gks state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errggetstring);

    /* Is the Current Event the proper a valuator event? */
    GKSERROR((xgks_state.CurEvent.event.gclass != GISTRING), 150, errggetstring);

    /* return the event data */
    STRCPY(response, (Gchar *) xgks_state.CurEvent.data);

    ufree((voidp)xgks_state.CurEvent.data);

    return 0;
}
