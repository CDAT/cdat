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
 *  XGKS -- Xevent interrupt handeling and process routines
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <assert.h>
#include "gks_implem.h"

#ifndef lint
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif

/*
 * SIGIO response status: 
 *     0 => handling
 *    >0 => ignoring
 */
static int      SigCount = 0;


/*
 * Redraw an XGKS window.
 */
    static int
redraw(ws)
    WS_STATE_PTR    ws;
{
#ifdef X11OUT
    Display		*dpy	= ws->dpy;
    Window		win	= ws->win;
    XWindowAttributes	win_att;

    /*
     * Disable all input devices.
     */
    XgksIDevDisable(ws);

    /*
     * Get current window width and height values and update the 
     * transformation.
     */
    XGetWindowAttributes(dpy, win, &win_att);
    ws->wbound.x = win_att.width;
    ws->wbound.y = win_att.height;
    xXgksUpdateTrans(ws);

    /*
     * Redraw the window contents.
     */
    XClearArea(dpy, win, 0, 0, 0, 0, False);
#ifdef DEBUG
	(void) fprintf(stderr, "redraw: calling XgksXReDrawWs()\n");
#endif
    XgksXReDrawWs(ws);

    /* if necessary call user defined redraw notifying function */
    if (ws->redrawfuncp != NULL)
	(*(ws->redrawfuncp)) (ws->ws_id, GRD_X);

    XFlush(dpy);

    /*
     * Enable the input devices.
     */
    XgksIDevEnable(ws);
#endif

    return 0;
}


/*
 * The synchronous and asynchronous XEvent processing routine.
 *
 * This routine is called by both xProcessEvents() and xProcessAsyncEvents().
 * This is done so that those routines can contain assertions.
 */
    static void
ProcessEvents()
{
    Gint            i;
    int		    need_redraw[MAX_OPEN_WS];
#ifdef X11OUT

    for (i = 0; i < MAX_OPEN_WS; i++) {
	need_redraw[i]	= 0;

	if (xgks_state.openedws[i].ws_id != INVALID && 
	    xgks_state.openedws[i].ws != NULL &&
	    xgks_state.openedws[i].ws->dpy != NULL &&
	    !xgks_state.openedws[i].ws->ws_is_closing) {

	    XEvent		xev;
	    Window		win	= xgks_state.openedws[i].win;
	    WS_STATE_ENTRY	*ws	= xgks_state.openedws[i].ws;

	    (void) XgksSIGIO_OFF(ws->dpy);

	    /*
	     * Insure that the event-queue is as full as possible.
	     */
	    /* XSync(ws->dpy, False); */

#	    ifdef EVENTDEBUG
		(void) fputs("ProcessEvents(): calling XCheckWindowEvent()\n",
			     stderr);
#	    endif

	    while (XCheckWindowEvent(ws->dpy, win, ~NoEventMask, &xev) 
		   == True) {

#		ifdef EVENTDEBUG
		    print_event(&xev);
#		endif

		switch (xev.type) {

		/*
		 * Because a window exposure can result in multiple expose
		 * events (each indicating a sub-region to be redrawn) and
		 * XGKS can only redraw the entire window, every expose 
		 * event only sets the "redraw" flag for the window.
		 *
		 * NOTE: When a window is only moved, some servers 
		 * (Ultrix 4.2) do not send expose-events while other servers
		 * (AIX, MIT X11R4 under SunOS) do.  The result is unnecessary
		 * redraws on the latter servers.
		 */
		case Expose:
		    need_redraw[i]	= 1;
		    break;

		case KeyPress:
		case MotionNotify:
		case ButtonPress:
		case ButtonRelease:
		    XgksIProcessXEvent(&xev, ws);
		    break;

		default:
		    break;
		}
	    }					/* event loop */

	    (void) XgksSIGIO_ON(ws->dpy);
	}					/* workstation is X11 window */
    }						/* GKS workstation loop */

    /*
     * Redraw those windows needing it.
     */
    for (i = 0; i < MAX_OPEN_WS; i++)
	if (need_redraw[i])
	    (void) redraw(xgks_state.openedws[i].ws);
#endif
}


/*
 * The synchronous XEvent processing routine.
 */
    void
xProcessEvents()
{
    assert(SigCount > 0);
    ProcessEvents();
}


/*
 * The asynchronous XEvent processing routine.
 */
    static void
xProcessAsyncEvents()
{
    assert(SigCount == 0);
    ProcessEvents();
}


/*
 * Start the SIGIO interrupt system.
 */
#ifdef X11OUT
    int
xXgksSIGIOStart(ws)
    WS_STATE_PTR    ws;
{
    Display        *dpy	= ws->dpy;
    pid_t           pid = getpid();

    if (dpy == NULL)				/* not opened yet */
	return INVALID;

    /*
     * Ignore SIGIO signals.
     */
    (void) sio_off();

    /*
     * Set the process-group ID of processes that will receive SIGIO and 
     * SIGURG signals associated with the display file-descriptor to the 
     * PID of this process.
     */
    (void) sockspgrp(ConnectionNumber(dpy), pid);

    /*
     * Make I/O on the display file-descriptor asynchronous.
     */
    (void) sockasync(ConnectionNumber(dpy), 1);

    return 0;
}
#endif
#ifdef X11WM
    int
XgksSIGIO_OFF(dpy)
    Display        *dpy;
{
    SigCount++;

    assert(SigCount > 0);

#ifdef SIGDEBUG
    (void) fprintf(stderr, "XgksSIGIO_OFF SigCount == %d\n", SigCount);
#endif

    if (SigCount > 1)				/* already off */
	return 0;

    /* If socket does not exist, I/O is not possible. */
    if (dpy == NULL)
	return 0;

    /*
     * Ignore SIGIO signals.
     */
    (void) sio_off();

    /*
     * Make I/O on the display file-descriptor synchronous.
     */
    (void) sockasync(ConnectionNumber(dpy), 0);

    return 0;
}


    int
XgksSIGIO_ON(dpy)
    Display        *dpy;
{
    pid_t           pid = getpid();

    assert(SigCount > 0);

    SigCount--;

#ifdef SIGDEBUG
    (void) fprintf(stderr, "XgksSIGIO_ON SigCount == %d\n", SigCount);
#endif

    if (SigCount > 0)				/* only on last request */
	return 0;

    /* If socket does not exist, I/O is not possible. */
    if (dpy == NULL)
	return 0;

#if 0
    /* I don't know why this is here.  -- SRE 5/2/91 */
    ProcessEvents();
#endif

    /*
     * Register the SIGIO signal-handler.
     */
    (void) sio_on(xProcessAsyncEvents);

    /*
     * Set the process-group ID of processes that will receive SIGIO and 
     * SIGURG signals associated with  the display file-descriptor to the 
     * PID of this process.
     */
    (void) sockspgrp(ConnectionNumber(dpy), pid);

    /*
     * Make I/O on the display file-descriptor asynchronous.
     */
    (void) sockasync(ConnectionNumber(dpy), 1);

    return 0;
}

#endif
#ifdef EVENTDEBUG
    static int
print_event(evnt)
    XEvent         *evnt;
{
    switch (evnt->type) {
    case 0:
	(void) fprintf(stderr, "some kind of error");
	break;
    case 1:
	(void) fprintf(stderr, "some kind of reply");
	break;
    case KeyPress:
	(void) fprintf(stderr, "KeyPress");
	break;
    case KeyRelease:
	(void) fprintf(stderr, "KeyRelease");
	break;
    case ButtonPress:
	(void) fprintf(stderr, "ButtonPress");
	break;
    case ButtonRelease:
	(void) fprintf(stderr, "ButtonRelease");
	break;
    case MotionNotify:
	(void) fprintf(stderr, "MotionNotify");
	break;
    case EnterNotify:
	(void) fprintf(stderr, "EnterNotify");
	break;
    case LeaveNotify:
	(void) fprintf(stderr, "LeaveNotify");
	break;
    case FocusIn:
	(void) fprintf(stderr, "FocusIn");
	break;
    case FocusOut:
	(void) fprintf(stderr, "FocusOut");
	break;
    case KeymapNotify:
	(void) fprintf(stderr, "KeymapNotify");
	break;
    case Expose:
	(void) fprintf(stderr, "Expose");
	break;
    case GraphicsExpose:
	(void) fprintf(stderr, "GraphicsExpose");
	break;
    case NoExpose:
	(void) fprintf(stderr, "NoExpose");
	break;
    case VisibilityNotify:
	(void) fprintf(stderr, "VisibilityNotify");
	break;
    case CreateNotify:
	(void) fprintf(stderr, "CreateNotify");
	break;
    case DestroyNotify:
	(void) fprintf(stderr, "DestroyNotify");
	break;
    case UnmapNotify:
	(void) fprintf(stderr, "UnmapNotify");
	break;
    case MapNotify:
	(void) fprintf(stderr, "MapNotify");
	break;
    case MapRequest:
	(void) fprintf(stderr, "MapRequest");
	break;
    case ReparentNotify:
	(void) fprintf(stderr, "ReparentNotify");
	break;
    case ConfigureNotify:
	(void) fprintf(stderr, "ConfigureNotify");
	break;
    case ConfigureRequest:
	(void) fprintf(stderr, "ConfigureRequest");
	break;
    case GravityNotify:
	(void) fprintf(stderr, "GravityNotify");
	break;
    case ResizeRequest:
	(void) fprintf(stderr, "ResizeRequest");
	break;
    case CirculateNotify:
	(void) fprintf(stderr, "CirculateNotify");
	break;
    case CirculateRequest:
	(void) fprintf(stderr, "CirculateRequest");
	break;
    case PropertyNotify:
	(void) fprintf(stderr, "PropertyNotify");
	break;
    case SelectionClear:
	(void) fprintf(stderr, "SelectionClear");
	break;
    case SelectionRequest:
	(void) fprintf(stderr, "SelectionRequest");
	break;
    case SelectionNotify:
	(void) fprintf(stderr, "SelectionNotify");
	break;
    case ColormapNotify:
	(void) fprintf(stderr, "ColormapNotify");
	break;
    case ClientMessage:
	(void) fprintf(stderr, "ClientMessage");
	break;
    case MappingNotify:
	(void) fprintf(stderr, "MappingNotify");
	break;
    default:
	if (evnt->type >= LASTEvent) {
	    (void) fprintf(stderr, "extension event #%d", evnt->type);
	} else {
	    (void) fprintf(stderr, "\nInternal Error in XUnhandledWireEvent!");
	}
	break;
    }
    (void) fputc('\n', stderr);
    return 0;
}
#endif	/* EVENTDEBUG defined */

