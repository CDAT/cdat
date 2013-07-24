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
 *
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 * 
 * functions to manipulate the list of logical input devices
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
#include <stdlib.h>
#include "gks_implem.h"
#ifdef X11OUT
#include <X11/cursorfont.h>
#define XK_MISCELLANY
#define XK_LATIN1
#include <X11/keysymdef.h>
#endif

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#endif


/*
 * Initialise the input device list
 */
XgksInitIDev(ws)
    WS_STATE_ENTRY *ws;
{
#ifdef X11WM
/*     XSetWindowAttributes xswa; */
#endif
    ws->in_dev_list = NULL;
    if (ws->ewstype == X_WIN) {
#ifdef X11WM
/* 	XgksSIGIO_OFF(ws->dpy); */
	/*xswa.cursor = XCreateFontCursor(ws->dpy, XC_pencil); Don't need to change the cursor to pencil
	XChangeWindowAttributes(ws->dpy, ws->win, CWCursor, &xswa);*/
/* 	XgksSIGIO_ON(ws->dpy); */
#endif
    }
}


/*
 * Add a logical device to the list
 */
    void
XgksIDevAdd(ws, idev)
    WS_STATE_ENTRY *ws;
    INPUT_DEV      *idev;
{
    idev->next = ws->in_dev_list;
    ws->in_dev_list = idev;
}


/*
 * Cleaning up on input device queue.
 * This routine will free up all memory assoicated with input device structure
 *
 */
XgksIDevDelete(ws)
    WS_STATE_ENTRY *ws;
{
    INPUT_DEV      *idev = ws->in_dev_list;

    while (idev != NULL) {
	switch (idev->class) {
	case GLOCATOR:
	    XgksLocDelete(ws, idev);
	    break;
	case GISTROKE:
	    XgksStkDelete(ws, idev);
	    break;
	case GVALUATOR:
	    XgksValDelete(ws, idev);
	    break;
	case GCHOICE:
	    XgksChoDelete(ws, idev);
	    break;
	case GPICK:
	    XgksPicDelete(ws, idev);
	    break;
	case GISTRING:
	    XgksStrDelete(ws, idev);
	    break;
	default:
	    break;
	}
	idev = ws->in_dev_list->next;
	ufree((voidp)ws->in_dev_list);
	ws->in_dev_list = idev;
    }
}


/*
 * Allocate memory for a new logical input device, returns pointer
 * to the new device structure, or NULL if malloc fails.
 *   Some Default initilation are done ...
 *
 *     idev->class    = GNCLASS;
 *     idev->dev      = INVALID;
 *     idev->active   = FALSE;
 *     idev->touched  = FALSE;
 *     idev->breakhit = FALSE;
 *     idev->next = NULL;
 */
    INPUT_DEV*
XgksIDevNew()
{
    INPUT_DEV      *new;

    if ((new = (INPUT_DEV *) malloc(sizeof(INPUT_DEV))) != NULL) {
	new->class = GNCLASS;
	new->dev = INVALID;
	new->active = FALSE;
	new->touched = FALSE;
	new->breakhit = FALSE;
	new->next = NULL;
    }
    return new;
}


/*
 * Search the input device list for a specific device.  Return the device or
 * a NULL pointer.
 */
    INPUT_DEV*
XgksIDevLookup(ws, dev, class)
    WS_STATE_ENTRY *ws;
    Gint            dev;
    Giclass         class;
{
    INPUT_DEV      *id;

    for (id = ws->in_dev_list; id != NULL; id = id->next)
	if ((id->class == class) && (id->dev == dev))
	    return id;
    return (INPUT_DEV *) NULL;
}


/*
 * Process X events and see if they trigger any GKS logical input devices
 *
 * XEvent can be one of: MotionNotify
 */
#ifdef X11OUT
XgksIProcessXEvent(xev, ws)
    XEvent         *xev;
    WS_STATE_ENTRY *ws;
{
    INPUT_DEV      *idev;
    Bool            breakhit;
    KeySym          ksym;
    Gpoint          dcpt;
    XPoint          xpt;
    Glimit         *ea;

#define InEchoArea	( dcpt.x >= ea->xmin && dcpt.x <= ea->xmax && \
			  dcpt.y >= ea->ymin && dcpt.y <= ea->ymax )

    XMotionEvent   *xmev;

    /* simultaneous events have same ID! */
    static int      current_event_id = 0;

#define MAX_EVENT_ID 32767

    xmev = (XMotionEvent *) xev;

    /*
     * If the closing of this ws has already been initiated,
     * don't try to do anything.  (by the time we get to the
     * inquire color map the actual X window may be gone -
     * producing an X error!)
     */
    if (ws->ws_is_closing)
	return;

    switch (xev->type) {
    case ButtonPress:
    case ButtonRelease:
    case MotionNotify:
    case KeyPress:
	/* transform point from X space to DC space */
	xpt.x = xmev->x;
	xpt.y = xmev->y;
	XToDc(ws, &xpt, &dcpt);

	if (xev->type == KeyPress) {
	    XLookupString((XKeyEvent *) xev, (char *) NULL, 0, &ksym, NULL);
	    breakhit = (ksym == XK_Pause) || (ksym == XK_Break) ? True : False;
	} else {
	    breakhit = False;
	}

#ifdef IDEVDEBUG
	(void) fprintf(stderr, 
    "XgksIProcessXEvent: xmev=(%d,%d) xpt=(%d,%d) dcpt=(%f,%f) breakhit=%d\n",
		       xmev->x, xmev->y,
		       xpt.x, xpt.y, dcpt.x, dcpt.y, 
		       breakhit);
#endif

	/*
	 * Increment current event ID.  Used to determine if two events are
	 * simultaneous events - they are if they have the same ID.
	 */
	if (++current_event_id > MAX_EVENT_ID)
	    current_event_id = 0;

	/* check each active input device for trigger */
	for (idev = ws->in_dev_list; idev != NULL; idev = idev->next) {
	    if (idev->active == False)
		continue;

	    /* check echo area */
	    switch (idev->class) {
	    case GLOCATOR:
		ea = &idev->data.loc.initst.e_area;
		if (InEchoArea)
		    if (breakhit)
			idev->breakhit = True;
		    else if (xev->type != KeyPress)
			(void) XgksLocUpdatePrompt(ws, idev, PROMPTMOVE, 
						   &dcpt, xmev, 
						   current_event_id);
		break;
	    case GCHOICE:
		ea = &idev->data.cho.initst.e_area;

#ifdef IDEVDEBUG
		(void) fprintf(stderr,
	    "XgksIProcessXEvent: dcpt=(%f,%f) Cho earea=([%f-%f],[%f-%f])\n",
			       dcpt.x, dcpt.y, 
			       ea->xmin, ea->xmax, ea->ymin, ea->ymax);
#endif

		if (InEchoArea) {
		    if (breakhit) {
			idev->breakhit = True;
		    } else {
			if (((idev->data.cho.initst.pet == 3) && 
				(xev->type != KeyPress)) || 
				((idev->data.cho.initst.pet == 1) && 
				(xev->type == KeyPress)) || 
				((idev->data.cho.initst.pet == 2) && 
				(xev->type == KeyPress))) {
#ifdef IDEVDEBUG
			    (void) fprintf(stderr,
			"XgksIProcessXEvent: calling XgksChoUpdatePrompt\n");
#endif
			    (void) XgksChoUpdatePrompt(ws, idev, PROMPTMOVE, 
						       xmev, current_event_id);
			}
		    }
		}
		break;
	    case GVALUATOR:
		ea = &idev->data.val.initst.e_area;
		if (InEchoArea)
		    if (breakhit)
			idev->breakhit = True;
		    else if (xev->type != KeyPress)
			(void) XgksValUpdatePrompt(ws, idev, PROMPTMOVE, 
			    &dcpt, xmev, current_event_id);
		break;
	    case GPICK:
		ea = &idev->data.pic.initst.e_area;

#ifdef IDEVDEBUG
		if (InEchoArea)
		    (void) fprintf(stderr, 
	"XgksIProcessXEvent: device GPICK --> InEchoArea .. %f %f %f %f ?\n",
			ea->xmin, ea->xmax, ea->ymin, ea->ymax);
		else
		    (void) fprintf(stderr, 
	"XgksIProcessXEvent: device GPICK --> InEchoArea .. %f %f %f %f ?\n",
			ea->xmin, ea->xmax, ea->ymin, ea->ymax);
#endif

		if (InEchoArea)
		    if (breakhit)
			idev->breakhit = True;
		    else if (xev->type != KeyPress)
			(void) XgksPicUpdatePrompt(ws, idev, &dcpt, xmev, 
						   current_event_id);
		break;
	    case GISTRING:
		ea = &idev->data.str.initst.e_area;
		if (InEchoArea)
		    if (breakhit)
			idev->breakhit = True;
		    else {
			if (xev->type == KeyPress)
			    (void) XgksStrUpdatePrompt(ws, idev, PROMPTMOVE, 
				(XKeyPressedEvent *) xmev, current_event_id);
		    }
		break;
	    case GISTROKE:
		ea = &idev->data.stk.initst.e_area;
		if (InEchoArea)
		    if (breakhit)
			idev->breakhit = True;
		    else if (xev->type != KeyPress)
			(void) XgksStkUpdatePrompt(ws, idev, PROMPTMOVE,
						   &dcpt, xmev,
						   current_event_id);
		break;
	    default:
		break;
	    }
	}
	break;
    default:
	break;
    }
}
#endif

#ifndef False
#define False 0
#endif
/*
 * Disable all input devices for a given workstation.
 *	Basically, just get their prompts off the screen so that GKS can
 *	produce some output.
 */
static          DisCount = 0;

XgksIDevDisable(ws)
    WS_STATE_ENTRY *ws;
{
    INPUT_DEV      *idev;

    DisCount++;

    if (ws->ewstype != X_WIN)
	return 0;

#ifdef IDEVDEBUG
    (void) fprintf(stderr, "XgksIDevDisable() DisCount = %d\n", DisCount);
#endif

    if (DisCount > 1)				/* already done */
	return 0;

#ifdef X11WM
    (void) XgksSIGIO_OFF(ws->dpy);
#endif

/* check each active input device for trigger */
    for (idev = ws->in_dev_list; idev != NULL; idev = idev->next) {
	if (idev->active == False)
	    continue;

	/* check echo area */
	switch (idev->class) {
	case GLOCATOR:
	    /* this call is not commented out because  */
	    /* loclines are draw in XOR mode. So if we */
	    /* didn't erase it now, it would disappear */
	    /* when we tried to redraw it later!       */
#ifdef X11OUT
	    if (idev->data.loc.initst.esw == GECHO)
		(void) XgksLocUpdatePrompt(ws, idev, PROMPTOFF,
					   (Gpoint *) NULL,
					   (XMotionEvent *) NULL, -1);
#endif
	    break;
	case GCHOICE:
	    /* This call has been commented out because it caused */
	    /* all the devices to flash for each primitive drawn  */
	    /* (This was the cause of PTR c1023  -  DWO)          */
	    /* if(idev->data.cho.initst.esw==GECHO) */
	    /*
	     * (void)XgksChoUpdatePrompt( ws, idev, PROMPTOFF, (XMotionEvent
	     * *)NULL, -1);
	     */
	    break;
	case GPICK:
	    /* This call has been commented out because it caused */
	    /* all the devices to flash for each primitive drawn  */
	    /* (This was the cause of PTR c1023  -  DWO)          */
	    /* if(idev->data.pic.initst.esw==GECHO) */
	    /*
	     * (void)XgksPicUpdatePrompt( ws, idev, (Gpoint *)NULL,
	     * (XMotionEvent *)NULL, -1);
	     */
	    break;
	case GVALUATOR:
	    /* This call has been commented out because it caused */
	    /* all the devices to flash for each primitive drawn  */
	    /* (This was the cause of PTR c1023  -  DWO)          */
	    /* if(idev->data.val.initst.esw==GECHO) */
	    /*
	     * (void)XgksValUpdatePrompt( ws, idev, PROMPTOFF, (Gpoint
	     * *)NULL, (XMotionEvent *)NULL, -1);
	     */
	    break;
	case GISTRING:
	    /* This call has been commented out because it caused */
	    /* all the devices to flash for each primitive drawn  */
	    /* (This was the cause of PTR c1023  -  DWO)          */
	    /* if(idev->data.str.initst.esw==GECHO) */
	    /*
	     * (void)XgksStrUpdatePrompt( ws, idev, PROMPTOFF,
	     * (XKeyPressedEvent *)NULL, -1);
	     */
	    break;
	case GISTROKE:
	    /* this call is not commented out because  */
	    /* strokes are drawn in XOR mode.  So if we */
	    /* didn't erase it now, it would disappear */
	    /* when we tried to redraw it later!       */
#ifdef X11OUT
	    if (idev->data.stk.initst.esw == GECHO)
		(void) XgksStkUpdatePrompt(ws, idev, PROMPTOFF, 
					   (Gpoint *) NULL, 
					   (XMotionEvent *) NULL, -1);
#endif
	    break;
	default:
	    break;
	}
    }
}


/*
 * Enable all input devices for a given workstation.
 */
XgksIDevEnable(ws)
    WS_STATE_ENTRY *ws;
{
    INPUT_DEV      *idev;

    DisCount--;

    if (ws->ewstype != X_WIN)
	return 0;

#ifdef IDEVDEBUG
    (void) fprintf(stderr, "XgksIDevEnable() DisCount = %d\n", DisCount);
#endif

    /* only disable on last request */
    if (DisCount > 0)
	return 0;

    /* check each active input device for trigger */
    for (idev = ws->in_dev_list; idev != NULL; idev = idev->next) {
	if (idev->active == False)
	    continue;

	/* check echo area */
#ifdef X11OUT
	switch (idev->class) {
	case GLOCATOR:
	    if (idev->data.loc.initst.esw == GECHO)
		(void) XgksLocUpdatePrompt(ws, idev, PROMPTON, 
					   (Gpoint *) NULL,
					   (XMotionEvent *) NULL, -1);
	    break;
	case GCHOICE:
	    if (idev->data.cho.initst.esw == GECHO)
		(void) XgksChoUpdatePrompt(ws, idev, PROMPTON,
					   (XMotionEvent *) NULL, -1);
	    break;
	case GPICK:
	    if (idev->data.pic.initst.esw == GECHO)
		(void) XgksPicUpdatePrompt(ws, idev, &(idev->data.pic.curpos),
					   (XMotionEvent *) NULL, -1);
	    break;
	case GVALUATOR:
	    if (idev->data.val.initst.esw == GECHO) {
		Gpoint          gp;

		gp.x = gp.y = idev->data.val.CurPos;
		(void) XgksValUpdatePrompt(ws, idev, PROMPTON, &gp,
					   (XMotionEvent *) NULL, -1);
	    }
	    break;
	case GISTRING:
	    if (idev->data.str.initst.esw == GECHO)
		(void) XgksStrUpdatePrompt(ws, idev, PROMPTON,
					   (XKeyPressedEvent *) NULL, -1);
	    break;
	case GISTROKE:
	    if (idev->data.stk.initst.esw == GECHO)
		(void) XgksStkUpdatePrompt(ws, idev, PROMPTON,
		    &(idev->data.stk.stkbuf[idev->data.stk.editpos]),
		    (XMotionEvent *) NULL, -1);
	    break;
	default:
	    break;
	}
#endif
    }
#ifdef X11WM
    (void) XgksSIGIO_ON(ws->dpy);
#endif
}
