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
 * GKS Input processing structures
 *
 * This header-file depends upon header-file "xgks.h".
 * 
 * $Id$
 * $__Header$
 */

#ifndef INPUT_H
#define INPUT_H

/*
 * Each workstation structure contains a list of input devices that have been
 * accessed by the application.  These devices may be active or inactive.
 * Each logical input device is represented by a structure that completely
 * defines the initial state, current state of that device, and implementation
 * details of that device.  When a physical device is triggered, this list
 * is search for active logical devices that must be triggered.  The echo area
 * and current location of the physical device determine if the logical device
 * is triggered.  Each triggered input device updates its value and prompt.
 * When a logical device that is operating in EVENT mode is triggered, it
 * creates an event record and places that record in the GKS state event queue.
 */

typedef struct {
    Gchoicest       initst;		/* initial state */
    /* implementation state */
    Gint            curcho;		/* current choice */
    /*
     * X space coordinates are computed each time PROMPTON is envoked
     */
#ifdef USEX11
    XPoint          origin;		/* top left corner of menu in X space */
#else
    Gpoint          origin;		/* top left corner of menu in X space */
#endif
    int             iheight;		/* height of each item in X space */
    int             width;		/* width of each item in X space */
    int             height;		/* total height of menu in X space */
}               WSCHOICE;

typedef struct {
    Glocst          initst;		/* initial state */
    /* implementation state */
    Gpoint          initpos;		/* initial position in DC */
    Gpoint          curpos;		/* current position in DC */
}               WSLOCATOR;

typedef struct {
    Gpoint          curpos;		/* current position in DC */
    Gpickst         initst;		/* initial state */
    /* implementation state */
}               WSPICK;

typedef struct {
    Gstringst       initst;		/* initial state */
    /* implementation state */
    Gchar          *strbuf;		/* input string buffer */
    Gint            editpos;		/* edit position */
#ifdef USEX11
  XPoint          curpos;		/* current position of the cursor in */
#else
  Gpoint          curpos;		/* current position of the cursor in */
#endif				        /* X space */
}               WSSTRING;

typedef struct {
    Gstrokest       initst;		/* initial state */
    /* implementation state */
    Gpoint         *stkbuf;		/* input stroke buffer */
    Gpoint          interval;		/* minimum interval between points DC */
    Gint            editpos;		/* insertion point */
}               WSSTROKE;

typedef struct {
    Gvalst          initst;		/* initial state */
    VAL_AXIS        axis;		/* Orientation of echo area */
    Gfloat          convert[2];		/* convertion from DC to valuator
					 * output val == (DC*convert[0] -
					 * convert[1]) */
    Gfloat          BarWidth;		/* Width of the sliding bar */
    Gfloat          BarHeight;		/* Height of Sliding Bar */
    /*
     * Note the sliding bar is always constructed with input DC at center, so
     * what is being saved here are actually OFFSETS FROM CENTER to construct
     * the sliging bar
     */
    Gfloat          CurPos;		/* Current position (in DC) w.r.t.
					 * the echo min This is either X or Y
					 * value */
    Gpoint          SlidRule[2];	/* Location of the Slide Ruler in DC */
    Gqval           val;		/* Response record in valuator-ratio  */
    /* implementation state */
}               WSVALUATOR;

#ifndef USEX11
#include <stdbool.h>
#define Bool bool
#endif
typedef struct INPUT_DEV {
    Giclass         class;		/* device class */
    Gint            dev;		/* device number */
    Bool            active;		/* is this device active? */
    Bool            touched;		/* has the device been used lately? */
    Bool            breakhit;		/* was break hit? */
#ifdef USEX11
    GC              gc;			/* graphics context */
#endif
    struct INPUT_DEV *next;		/* linked list */
    union {
	WSCHOICE        cho;
	WSLOCATOR       loc;
	WSPICK          pic;
	WSSTRING        str;
	WSSTROKE        stk;
	WSVALUATOR      val;
    }               data;
}               INPUT_DEV;

typedef struct EQEntry {
    struct EQEntry *next;		/* Event queue is a linked list */
    Gevent          event;		/* Data returned by Await Event */
    char           *data;		/* Data returned by Get<class>
					 * functions */
    int             id;			/* event id (simultaneous */
    /* events have same id)   */
} EQEntry;

typedef enum {
    PROMPTON,
    PROMPTOFF,
    PROMPTMOVE
}               PromptStatus;

extern void            XgksIDevAdd();
extern INPUT_DEV      *XgksIDevLookup();
extern INPUT_DEV      *XgksIDevNew();
extern Gpstat          XgksFindPickSeg();

#endif
