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
 * gks_error.c - GKS functions	gerrorhand()
 * 				gerrorlog()
 *				gemergencyclosegks()
 */

 /* LINTLIBRARY */

#ifndef lint
    static char	afsid[]	= "$__Header$";
    static char	rcsid[]	= "$Id$";
#endif

#define GKS_ERROR_C

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include "gks_implem.h"
#include "gkserrmsg.h"		/* Defines static error-messages */

/*FILE    *errfp                  = stderr;       * error file */
FILE    *errfp;       		/* error file */
int     error_lun               = -1;   	/* error file LUN */

#define errfp stderr            /* define the error file to standard error */

extern char	*procname[];
extern char	*procname1000[];
extern int      error_lun;

/*
 * gerrorhand -  Error Handling
 *
 *      gerrorhand() is called internally to the FORTRAN and C bindings (when
 *      a binding function determines that there's an error)  When the user
 *      wants to call the error handler, he can only call gerhnd() (FORTRAN)
 *      or gerrorhand() (C).
 *
 *      gerrorhand() needs to have a file pointer as the 3rd argument so C
 *      binding functions can call it correctly, but it only uses that
 *      argument if the user isn't using the FORTRAN binding.
 *
 *  Gint errnr     - error number
 *  Gchar *fctid   - function that detected the error
 *  Gfile *efp     - file for error output.
 *
 * See also: ANSI standard p. 195
 */
gerrorhand(errnr, fctid, efp)
    Gint            errnr;
    Gerrmap         fctid;
    Gfile          *efp;
{
/*    debug(("FORTRAN Errorhand:  errnr = %d, fctid = %d\n", errnr, fctid));*/

    gerrorlog(errnr, fctid, efp);

    /*debug(("Exit errorhand\n"));*/

    return 0;
}


/*
 * gemergencyclosegks() - EMERGENCY CLOSE GKS
 *
 * returns: 0 (always)
 *
 * See also: Ansi standard p. 193
 */
gemergencyclosegks()
{
    int             i;

    if (xgks_state.gks_state == GGKCL)
	/* already closed, nothing to do */
	return OK;

    if (xgks_state.gks_state == GSGOP)		/* close any open segment */
	(void) gcloseseg();

    /* any workstations open? */
    if (xgks_state.gks_state != GGKCL && xgks_state.gks_state != GGKOP) {
	for (i = 0; i < MAX_OPEN_WS; i++)
	    if (xgks_state.openedws[i].ws_id != INVALID) {
		if (xgks_state.openedws[i].ws->wsstate == GACTIVE)
		    (Gint) gdeactivatews(xgks_state.openedws[i].ws_id);
		(void) gclosews(xgks_state.openedws[i].ws_id);
	    }
    }
    (void) gclosegks();

    return OK;
}


/*
 *  ERROR LOGGING
 */
gerrorlog(errnum, funcname, perrfile)
    Gint            errnum;		/* number for the error that was
					 * detected. */
    Gerrmap         funcname;		/* name of function that detected the
					 * error. */
    Gfile          *perrfile;		/* file where error message is to be
					 * printed. */
{
    Gchar          *fname;

    /*
     * look up function name - this is printed now instead of the function
     * number
     */
    /* all fprintf statements reflect this change */
    if (((int)funcname < 197) && ((int)funcname > 0))
	fname = procname[(int)funcname];
    else if (((int)funcname > 999) && ((int)funcname < 1017))
	fname = procname1000[(int)funcname - 1000];
    else
	fname = "Invalid function name/number";

    if (perrfile == NULL)
	perrfile = stderr;

    if (errnum < 0)
	errnum = 0;

    if (errnum < 201)
	if (GKSErrorMessages0to200[errnum] == (char *) NULL)
	    (void) fprintf(perrfile, "%s %d <Undefined Error>\n", 
			   fname, errnum);
	else
	    (void) fprintf(perrfile, "%s %4d %s\n", fname, errnum, 
			   GKSErrorMessages0to200[errnum]);
    else if ((errnum > 299) && (errnum < 310))
	if (GKSErrorMessages300to309[errnum - 300] == (char *) NULL)
	    (void) fprintf(perrfile, "%s %d <Undefined Error>\n", 
			   fname, errnum);
	else
	    (void) fprintf(perrfile, "%s %4d %s\n", fname, errnum, 
			   GKSErrorMessages300to309[errnum - 300]);
    else if ((errnum > 1999) && (errnum < 2004))
	if (GKSErrorMessages2000to2000[errnum - 2000] == (char *) NULL)
	    (void) fprintf(perrfile, "%s %d <Undefined Error>\n", 
			   fname, errnum);
	else
	    (void) fprintf(perrfile, "%s %4d %s\n", fname, errnum, 
			   GKSErrorMessages2000to2000[errnum - 2000]);
    else
	(void) fprintf(perrfile, "%s %d <Undefined Error>\n", fname, errnum);

    return OK;
}
