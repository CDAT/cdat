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
 * open_gks.c - functions for opening and closing gks.
 *		open_gks()
 *		close_gks()
 */

/* LINTLIBRARY */

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include "gks_implem.h"
#include "fillarea.h"
#include "text.h"
#include "polylines.h"
#include "polymarkers.h"

/*
 * FORTRAN error file:
 */
extern FILE	*errfp;

#ifndef lint
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif

/* gks state list */
GKS_STATE_LIST	xgks_state;

static void XgksInitGksStateList();
static void XgksInitGksErrorStateList();


/*
 * gopengks(err_file, memory) - OPEN GKS - initialises gks data structures.
 *
 * Gfile *err_file;       file where gks is to print error messages.
 * int memory;		maximum buffer space, ignored in this implementation.
 *
 * returns 0 - opened ok
 *	   1 - gks not in proper state
 *       200 - specified error file is invalid
 *
 * See also: Ansi standard p. 74
 */
gopengks(err_file, memory)
    Gfile          *err_file;
    /*ARGSUSED*/
    Glong           memory;
{
    /* specified error file is not opened yet */
    GKSERROR((err_file == NULL), 200, errgopengks);

    /* can't open if not closed */
    GKSERROR((xgks_state.gks_state != GGKCL), 1, errgopengks);

    XgksInitGksErrorStateList(err_file);
    XgksInitGksStateList();

    /*
     * Set FORTRAN error file.
     */
    errfp	= err_file;

    return 0;
}


/*
 * gclosegks() - CLOSE GKS - sets the operating state to GGKCL if current 
 *			     state is GGKOP
 *
 * returns: 0 - closed ok
 *	    2 - gks not in proper state
 *
 * See also: ANSI standard p.74
 */
gclosegks()
{
    /* check for proper state */
    GKSERROR((xgks_state.gks_state != GGKOP), 2, errgclosegks);

    xgks_state.gks_state = GGKCL;
    return 0;
}


/*
 * XgksInitGksErrorStateList(err_file) - initialise data for error processing.
 *
 * Gfile *err_file;
 */
    static void
XgksInitGksErrorStateList(err_file)
    Gfile          *err_file;
{
    xgks_state.gks_err_file = err_file;		/* save the error file  */
    xgks_state.gks_err_state = ERR_OFF;		/* no error has occured */
    /* rest of list is undefined until input queue overflow */
}


/*
* XgksInitGksStateList() - called by open_gks() to initialise the gks state 
*			    list.
*
* returns nothing
*/
    static void
XgksInitGksStateList()
{
    Gint            i;

    /*
     * additional initialization made necessary by bundling of
     * gks state list into a single structure (this initialization
     * was formerly done globally at the declaration)
     */
    xgks_state.level = GL2C;			/* Level of GKS */

    /* Workstation maximum numbers (open, active, assoc) */
    xgks_state.wsmax.open = MAX_OPEN_WS;
    xgks_state.wsmax.active = MAX_ACTIVE_WS;
    xgks_state.wsmax.assoc = MAX_ASSOC_WS;

    xgks_state.wiss_id = INVALID;

    /*
     * Switch to keep track of Wiss's open and close.  Number of open meta
     * output files.
     */
    xgks_state.open_mo = 0;

    /* Set all workstations to close and inactive */
    for (i = 0; i < MAX_OPEN_WS; i++) {
	xgks_state.openedws[i].ws_id = INVALID;
#ifdef USEX11
	xgks_state.openedws[i].win = INVALID;
#endif
	xgks_state.openedws[i].ws = NULL;
    }
    for (i = 0; i < MAX_ACTIVE_WS; i++) {
	xgks_state.activews[i].ws_id = INVALID;
#ifdef USEX11
	xgks_state.activews[i].win = INVALID;
#endif
	xgks_state.activews[i].ws = NULL;
    }

    xgks_state.gks_state = GGKOP;		/* set state to GKs OPen */
    XgksInitGksAsf();				/* initialize asfs */
    XgksInitGksPlines();			/* initialise polyline stuff */
    XgksInitGksPmarkers();			/* initialise polymarker
						 * stuff */
    XgksInitGksFillArea();			/* initialise fill area stuff */
    XgksInitGksSegments();			/* init segment menagement
						 * and state list */
    XgksInitGksText();				/* initialise text stuff */
    XgksInitGksTrans();				/* initialise transformation
						 * stuff */

    XgksInitGksM();				/* Initialise metafile system */

    xgks_state.event_queue_head = NULL;
    xgks_state.event_queue_tail = NULL;

    xgks_state.CurEvent.next = (EQEntry *) NULL;
    xgks_state.CurEvent.event.ws = 0;
    xgks_state.CurEvent.event.dev = 0;
    xgks_state.CurEvent.event.gclass = GNCLASS;
    xgks_state.CurEvent.data = (char *) NULL;
    xgks_state.CurEvent.id = -999;
    xgks_state.focus_ws = (WS_STATE_PTR) NULL;
}
