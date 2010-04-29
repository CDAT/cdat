/*
 * This file contains inherently non-portable and non-standard support
 * functions for the X component of XGKS.
 */

#ifndef lint
    static char	rcsid[]	= "$Id$";
#endif

/*
 * The conditionally-compiled code that follows defines items related
 * to the use of the BSD system(2) call.  These items are inherently 
 * non-portable and system-dependent.  If you port this file to another 
 * system, you'll have to ensure that the following are appropriately 
 * defined:
 *
 *	FD_SETSIZE	Macro setting the size of a file-descriptor set
 *	FD_ZERO		Macro for clearing a file-descriptor set
 *	FD_SET		Macro for setting a file descriptor in a file-
 *			    descriptor set
 *	fd_set		Typedef for a file-descriptor set
 */
#ifdef _AIX
#   include	<sys/types.h>		/* for <sys/select.h>. Must be before */
#   include	<sys/select.h>
#else
#   ifdef _hpux
#       define	_HPUX_SOURCE
#   endif
#   include	<sys/types.h>		/* for `fd_set' & the FD_... macros */
#endif
#include <sys/time.h>		/* for `struct timeval' */

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif		/* for as much portability as possible */
#include <stdio.h>		/* for NULL and perror() */
#include <signal.h>
#include <errno.h>		/* for EINTR */
#include <assert.h>		/* for assert() */
#include <sys/ioctl.h>		/* for ioctl(), SIOCSPGRP, and FIOASYNC */
#include "gks_implem.h"		/* for WS_STATE_ENTRY */


/*
 * Ignore SIGIO signals.  This routine is non-portable because
 * not every platform has SIGIO.
 */
sio_off()
{
#ifdef SIGIO
    struct sigaction	act;

    act.sa_flags	= 0;
    act.sa_handler	= SIG_IGN;
    (void) sigemptyset(&act.sa_mask);

    return sigaction(SIGIO, &act, (struct sigaction*)NULL);
#else
    return 0;
#endif
}


/*
 * Register the SIGIO signal-handler.  This routine is non-portable because
 * not every platform has SIGIO.
 */
sio_on(handler)
    void	(*handler)();
{
#ifdef SIGIO
    struct sigaction	act;

    act.sa_flags	= 0;
    act.sa_handler	= handler;
    (void) sigemptyset(&act.sa_mask);

    return sigaction(SIGIO, &act, (struct sigaction*)NULL);
#else
    return 0;
#endif
}


/*
 * Set the process-group ID of a socket.  This routine is non-portable because
 * not every platform has sockets.
 */
sockspgrp(fd, pid)
    int		fd;
    pid_t	pid;
{
#ifdef SIOCSPGRP
    return ioctl(fd, SIOCSPGRP, (char*)&pid);
#else
    return 0;
#endif
}


/*
 * Make I/O on a socket asynchronous or not.  This routine is non-portable 
 * because not every platform has sockets.
 */
sockasync(fd, yes)
    int		fd;
    int		yes;
{
    int		zero	= 0;
    int		one	= 1;

#ifdef FIOASYNC
#ifdef DEBUG_ASYNC
    (void) fprintf(stderr, "sockasync: file-descriptor %d %s\n",
		   fd, yes ? "on" : "off");
#endif
    return ioctl(fd, FIOASYNC, yes ? (char*)&one : (char*)&zero);
#else
    return 0;
#endif
}


/*
 * Await and process an event from any workstation in a given list.
 * This routine is non-portable because not every platform has select().  
 * Note, however, that on platforms without select(), Xlib implements 
 * its own in terms of poll(); thus, select() should always be available.
 * (I think I just contradicted myself ;-)
 *
 * This routine has been verified under AIX 3.1, SunOS 4.1.1, and ULTRIX
 * 4.0.  Regretably, all of these have a select() system-call.
 *
 * Returns:
 *	-1	failure occurred (perror() called)
 *	 0	timeout occurred
 *	 1	event occurred and was processed
 */
#ifdef X11OUT
    int
XgksAwaitEvent(wslist, nws, timeout)
    WS_STATE_ENTRY
		**wslist;	/* list of workstations upon which to wait */
    int		nws;		/* number of workstations in list */
    double	timeout;	/* Timeout in seconds. <0 => indefinite wait. */
{
    int		status;			/* this routine's status */

    int		width	= FD_SETSIZE;	/* num fd_set bits to check */
    fd_set	readfds;		/* descriptors to wait upon */

    struct timeval
		timeval;		/* timeout interval */
    struct timeval
		*time_out;		/* pointer to timeout */

    /*
     * Set the timeout interval.
     */
    if (timeout < 0) {
	time_out		= NULL;
    } else {
	time_out		= &timeval;
	time_out->tv_sec	= timeout;
	time_out->tv_usec	= (timeout - time_out->tv_sec) * 1000000;
    }

    /*
     * Establish the file descriptors to wait upon.
     */
    FD_ZERO(&readfds);
    if (nws > 0) {
	WS_STATE_ENTRY	**wsstop;

	for (wsstop = wslist + nws; wslist < wsstop; ++wslist) {
	    WS_STATE_ENTRY	*ws	= *wslist;

	    assert(ConnectionNumber(ws->dpy) > 0);
	    assert(ConnectionNumber(ws->dpy) < FD_SETSIZE);
	    FD_SET(ConnectionNumber(ws->dpy), &readfds);
	}
    }

    /*
     * Await an event on the given file descriptors.
     */
    status	= select(width, &readfds, (fd_set*)NULL, 
			 (fd_set*)NULL, time_out);
    if (status == -1) {				/* error */
	if (errno == EINTR) {
	    /*
	     * select() was interrupted by a signal.  Assume it was
	     * SIGIO and that it was caught & processed.
	     */
	    status	= 1;
	} else {
	    perror("select");
	}
    } else if (status > 0) {			/* event occurred */
	xProcessEvents();
    }

    return status;
}
#endif
