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
 * XGKS open workstation function
 * Open display wk->conn if necessary, then create and map the workstation 
 * window wk: workstation list pointer return: (0)---- open succeeds (21)-- 
 * open fails .
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include <sys/types.h>			/* for uid_t */
#ifdef SYSINFO
#include <sys/systeminfo.h>
#else
#include <unistd.h>			/* NeXTOS requires that this be */
#endif
					/* after <sys/types.h> */
#include <string.h>
#include <pwd.h>
#include <ctype.h>			/* for isupper() & tolower() */
#include "gks_implem.h"
#ifdef USEX11
#include <X11/Xresource.h>
#endif

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif

static int      InitDefaults();
static int      GetUsersDatabase();
static int      InsureConn();
static int      CreateWindow();
static int      SetWindow(); /* 9/19/94 - DNW */
static char    *GetHomeDir();
static void     GetWindowGeometry();
static void     AllocGroundColors();
static void     GetWMHints();
static void     CreateGC();
static void     UpdateOpenWSTable();

/*
 * The following may be changed by applications programs to the name of
 * the application.
 */
extern char    *progname;


    Gint
xXgksOpenWs(wk)
    WS_STATE_PTR    wk;
{
    int             status;		/* success */

    if (wk->ewstype != X_WIN) {
	status = INVALID;
    } else {
#ifdef X11OUT
	/* Insure connection to display. */
	if (!(status = InsureConn(wk))) {
	    char           *basename;	/* UPC: base program name */
	    XrmDatabase     rDB;	/* resource database */

	    (void) xXgksSIGIOStart(wk);
	    (void) XgksSIGIO_OFF(wk->dpy);

	    /* Extract program name. */
	    if (progname == NULL)
		progname = "XGKS";
	    if ((basename = strrchr(progname, '/')) == (char *) NULL)
		basename = progname;
	    else
		basename += 1;

	    /* Initialize resource database (i.e. X-defaults). */
	    (void) InitDefaults(basename, wk->dpy, &rDB);

	    /* Create window -- using resource database for defaults. */
	    if (!(status = CreateWindow(basename, rDB, wk))) {
		XEvent          xev;	/* X event structure */
		XWindowAttributes WinAtt;	/* window attributes */

		/* Map window.  Wait for exposure-event. */
		XMapWindow(wk->dpy, wk->win);
		XWindowEvent(wk->dpy, wk->win, ExposureMask, &xev);
		XSync(wk->dpy, 0);

		/* Get size of actual window obtained. */
		XGetWindowAttributes(wk->dpy, wk->win, &WinAtt);
		wk->wbound.x = wk->size.x = WinAtt.width;
		wk->wbound.y = wk->size.y = WinAtt.height;

		/* Update open-workstation table. */
		(void) UpdateOpenWSTable(wk);

		/* Select Input Events */
		XSelectInput(wk->dpy, wk->win, wk->event_mask);
		XSync(wk->dpy, 0);
	    }
	}
	/*
	 * We depend on XgksSIGIO_ON() checking for a NULL wk->dpy -- caused
	 * by failure to establish a connection.
	 */
	(void) XgksSIGIO_ON(wk->dpy);
#endif
	fprintf(stderr,"x11out baddy!\n");
    }

    return status ? status : OK;
}

/*
 * 9/19/94 - DNW  
 * Modified to open an XGKS workstation with a drawable widget (or canvas).
 */
#define SEPCHARS " -\t\n" /* separaters for the connection string */
    Gint
xXgksOpenCanvasWs(wk)
    WS_STATE_PTR    wk;
{
    int             status=0;             /* success */
    Gchar           *conn_str;
    char            *hold_text, win[30], dpy[30];
#ifdef QTWM
    int qx,qy,qw,qh,iwkstid;
#endif
    if (wk->ewstype != X_WIN) {
        status = INVALID;
    } else {

        GKSERROR(((conn_str = (Gchar *)malloc((size_t) (STRLEN(wk->conn) + 1)))
== NULL), 300, errgopenws);
        STRCPY((conn_str), wk->conn);

#ifdef USEX11
        /* 
         * Get the number of display pointer and X window from the 
         * connection string
         */
        hold_text = strtok(conn_str, SEPCHARS);
        wk->wclmp = (int) NULL;
        while ( hold_text != NULL ) {
               if (strcmp(hold_text,"w") == 0) {
                  hold_text = strtok(NULL, SEPCHARS);
                  wk->win = (Window ) atoi(hold_text); /* set the window */
               } else if (strcmp(hold_text,"dp") == 0) {
                  hold_text = strtok(NULL, SEPCHARS);
                  wk->dpy = (Display *) atol(hold_text); /* set the display */
               } else if (strcmp(hold_text,"cmp") == 0) {
                  hold_text = strtok(NULL, SEPCHARS);
                  wk->wclmp = atoi(hold_text); /* set the colormap */
               } else
                  hold_text = strtok(NULL, SEPCHARS);
        }

        /* Create graphics-context for window. *
        (void) CreateGC(dpy, win, wk);*
        wk->gc = DefaultGC(dpy, DefaultScreen(dpy));
        wk->plinegc = DefaultGC(dpy, DefaultScreen(dpy));
        wk->pmarkgc = DefaultGC(dpy, DefaultScreen(dpy));
        wk->fillareagc = DefaultGC(dpy, DefaultScreen(dpy));
        wk->textgc = DefaultGC(dpy, DefaultScreen(dpy));*/


        /* Insure connection to display. */
        if (wk->dpy != NULL) {
            char           *basename;   /* UPC: base program name */
            XrmDatabase     rDB;        /* resource database */

            (void) XgksSIGIO_OFF(wk->dpy); /* turn GKS input off */

            /* Extract program name. */
            if (progname == NULL)
                progname = "";
            if ((basename = strrchr(progname, '/')) == (char *) NULL)
                basename = progname;
            else
                basename += 1;

            /* Initialize resource database (i.e. X-defaults). */
            (void) InitDefaults(basename, wk->dpy, &rDB);

            /* Set window -- using resource database for defaults. */
            if (!(status = SetWindow(basename, rDB, wk))) {
                XEvent          xev;    /* X event structure */
                XWindowAttributes WinAtt;       /* window attributes */

                /* Map window.  Wait for exposure-event. */
                XMapWindow(wk->dpy, wk->win);
                /*XWindowEvent(wk->dpy, wk->win, ExposureMask, &xev);*/
                XSync(wk->dpy, 0);

                /* Get size of actual window obtained. */
                XGetWindowAttributes(wk->dpy, wk->win, &WinAtt);
		wk->wbound.x = wk->size.x = WinAtt.width;
		wk->wbound.y = wk->size.y = WinAtt.height;

                /* Update open-workstation table. */
                (void) UpdateOpenWSTable(wk);

                /* Select Input Events */
                XSelectInput(wk->dpy, wk->win, wk->event_mask);
                XSync(wk->dpy, 0);
            }
        } else
            status = INVALID;
#elif defined (QTWM)
	sscanf(conn_str,"%i",&iwkstid);
	//fprintf(stderr,"in xgks opeing id: %i, new: %i\n",wk->ws_id -7,iwkstid);
	vcs_legacy_Qt_open_window_by_id(iwkstid); /* make sure the window is shown */
	vcs_legacy_Qt_get_window_dimensions_by_id(iwkstid,&qx,&qy,&qw,&qh); /*ok grabs geom */
	wk->wbound.x = wk->size.x = qw;
	wk->wbound.y = wk->size.y = qh;
	wk->soft_clipping_on=0;
	wk->wscolour=256;
	(void) UpdateOpenWSTable(wk);

#else
	printf("ok insert your WM bounds and stuff here\n");
#endif
    }

    return status ? status : OK;
}

/*
 * Insure a connection to the display.
 */
    static
InsureConn(wk)
    WS_STATE_PTR    wk;
{
    int             status = 0;		/* success */
    int             i;
    WS_STATE_PTR    wk_p = NULL;

    /* Check if Display wk->conn has been opened  */
    for (i = 0; i < MAX_OPEN_WS; i++) {
	if (xgks_state.openedws[i].ws_id >= 0 &&
		xgks_state.openedws[i].ws->ewstype == X_WIN) {

	    WS_STATE_PTR    ws = xgks_state.openedws[i].ws;

	    if ((STRCMP(wk->conn, ws->conn) == 0) && (ws != wk)) {
		wk_p = ws;
		break;
	    }
	}
    }
#ifdef USEX11
    if (wk_p != NULL) {				/* Has been opened */
	wk->dpy = wk_p->dpy;
	wk->dclmp = wk_p->dclmp;
	wk->wclmp = wk_p->dclmp;
    } else {					/* Open a new display */
	if ((wk->dpy = XOpenDisplay(wk->conn)) == NULL) {
	    status = 26;
	} else {
	    char           *ptr = DisplayString(wk->dpy);

	    ufree((voidp)wk->conn);
	    if ((wk->conn = (char *) malloc((size_t) (STRLEN(ptr) + 1))) == 
		    NULL) {
		status = 300;
	    } else {
		STRCPY(wk->conn, ptr);

		/* Set the screen default colour map ID. */
		wk->dclmp = DefaultColormap(wk->dpy,
					    DefaultScreen(wk->dpy));
		wk->wclmp = wk->dclmp;

		XSelectInput(wk->dpy, DefaultRootWindow(wk->dpy),
			     0);
	    }
	}
    }
#endif
    return status;
}


/*
 *	Indicate whether a boolean resource is set.  Returns -1 if the
 *	resource doesn't exist, 0 if it's false, and 1 if it's true.
 */
#ifdef USEX11
    static int
BoolResource(prog, name, class, rDB)
    char           *prog;		/* program name */
    char           *name;		/* resource name */
    char           *class;		/* resource class */
    XrmDatabase     rDB;		/* resource database */
{
    int             ison = -1;		/* return status = doesn't exist */

    char            name_key[1024];
    char            class_key[1024];
    char           *str_type[20];
    XrmValue        value;

    (void) strcat(strcat(strcpy(name_key, prog), "."), name);
    (void) strcat(strcpy(class_key, "Xgks."), class);

    if (XrmGetResource(rDB, name_key, class_key, str_type, &value) == True) {
	char           *cp;

	(void) strncpy(name_key, value.addr, (int) value.size);
	name_key[value.size] = 0;

	for (cp = name_key; *cp != 0; ++cp) {
	    if (isupper(*cp))
		*cp = tolower(*cp);
	}

	ison =
	    strcmp(name_key, "on") == 0 ||
	    strcmp(name_key, "1") == 0 ||
	    strcmp(name_key, "yes") == 0 ||
	    strcmp(name_key, "set") == 0 ||
	    strcmp(name_key, "true") == 0;
    }
    return ison;
}
#endif

/*
 * Create an XGKS window -- with all its associated attributes.
 */
#ifdef X11OUT
    static
CreateWindow(name, rDB, wk)
    char           *name;		/* program name */
    XrmDatabase     rDB;		/* resource database */
    WS_STATE_PTR    wk;			/* workstation structure */
{
    int             status = 0;		/* success */
    int             init_status=0;      /* init_success */
    int             NumMatched;		/* number of visuals found */
#ifdef X11OUT
    Display        *dpy = wk->dpy;	/* for convenience */
    XSizeHints      SizeHints;		/* window size hints */
    XSetWindowAttributes xswa;		/* window attributes */
    XVisualInfo     VisualTemplate;
    XVisualInfo    *VisualList;

    /* Initialize color-mapping. */
    /* DNW - I always want 256 colors. wk->wscolour = DisplayCells(dpy, DefaultScreen(dpy));*/
    wk->wscolour = 256;
    /* size of colour map */
    VisualTemplate.screen = DefaultScreen(dpy);
    VisualTemplate.visualid =
	DefaultVisual(dpy, DefaultScreen(dpy))->visualid;
    if ((VisualList = XGetVisualInfo(dpy, VisualScreenMask | VisualIDMask,
				     &VisualTemplate, &NumMatched)) == NULL)
	status = 26;
    if (status != 26) init_status = XcInit(wk, VisualList);
    if ((status == 26) || (init_status == 0)) {
        status = 26;
    } else {
	/*
	 * Get foreground and background colors (and set them in the
	 * colormap).
	 */
	(void) AllocGroundColors(wk, name, rDB, &wk->wsfg, &wk->wsbg);

	/*
	 * Init pointer to table of set values to NULL.  The table will be
	 * alloc'ed and init'ed in gsetcolourrep on the 1st call (DWO).
	 * 
	 * This may no longer be meaningful since I've changed the way the
	 * mapping from GKS color-index to X-colorcell-index is handled.
	 * --SRE 2/1/90
	 */
	wk->set_colour_rep = (Gcobundl *) NULL;

	/* Get window geometry hints. */
	(void) GetWindowGeometry(name, dpy, rDB, &SizeHints);

	/* Set the window-event mask. */
	wk->event_mask = StructureNotifyMask | 
	    ExposureMask | 
            EnterWindowMask |
            LeaveWindowMask |
            FocusChangeMask |
	    KeyPressMask |
	    ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask;

	/*
	 * Set the window-attributes structure.
	 */
	xswa.event_mask = StructureNotifyMask | ExposureMask | EnterWindowMask | LeaveWindowMask | FocusChangeMask;
	xswa.background_pixel = XcPixelValue(wk, wk->wsbg);
	xswa.border_pixel = XcPixelValue(wk, wk->wsfg);
	xswa.do_not_propagate_mask = wk->event_mask &
	    (KeyPressMask | KeyReleaseMask | ButtonPressMask |
	     ButtonReleaseMask | PointerMotionMask | Button1MotionMask |
	     Button2MotionMask | Button3MotionMask | Button4MotionMask |
	     Button5MotionMask | ButtonMotionMask);
	xswa.colormap = wk->dclmp;		/* default colormap --SRE */
	xswa.backing_store	= 
	    BoolResource(name, "backingstore", "Backingstore", rDB) == 0
		? NotUseful
		: DoesBackingStore(DefaultScreenOfDisplay(dpy)) == NotUseful
		    ? NotUseful
		    : Always;

	/* Create the window. */
	if ((wk->win = XCreateWindow(dpy, DefaultRootWindow(dpy),
				     (int) SizeHints.x, (int) SizeHints.y,
			      (int) SizeHints.width, (int) SizeHints.height,
				     5,		/* border width */
				     DefaultDepth(dpy, DefaultScreen(dpy)),
			InputOutput, DefaultVisual(dpy, DefaultScreen(dpy)),
	    (unsigned long) (CWDontPropagate | CWBackPixel | CWBorderPixel |
	     CWEventMask | CWColormap | CWBackingStore), &xswa)) == False) {

	    status = 26;
	} else {
	    Window          win = wk->win;	/* for convenience */
	    XWMHints        WMHints;	/* window-manager hints */
	    XClassHint      ClassHints;	/* class hints */

	    /* Set standard window properties. */
	    XSetStandardProperties(dpy, win, name, name, None,
				   (char **) NULL, 0, &SizeHints);

	    /* Set window-manager hints. */
	    (void) GetWMHints(dpy, name, rDB, &WMHints);
	    XSetWMHints(dpy, win, &WMHints);

	    /* Set class hints. */
	    if ((ClassHints.res_name = getenv("RESOURCE_NAME")) == NULL)
		ClassHints.res_name = name;
	    ClassHints.res_class = name;
	    (void) XSetClassHint(dpy, win, &ClassHints);

	    /* Create graphics-context for window. */
	    (void) CreateGC(dpy, win, wk);

	    /* Set foreground and background colors in graphics-context. */
	    XSetForeground(dpy, wk->gc, XcPixelValue(wk, wk->wsfg));
	    XSetBackground(dpy, wk->gc, XcPixelValue(wk, wk->wsbg));

	    /*
	     * Initialize last-clipping rectangles to absurd values.  This
	     * will cause actual clipping window to be set.
	     */
	    wk->last_pline_rectangle.x = 0;
	    wk->last_pline_rectangle.y = 0;
	    wk->last_pline_rectangle.width = 0;
	    wk->last_pline_rectangle.height = 0;
	    wk->last_pmarker_rectangle = wk->last_pline_rectangle;
	    wk->last_farea_rectangle = wk->last_pline_rectangle;
	    wk->last_text_rectangle = wk->last_pline_rectangle;

	    wk->last_dash_index = 1;

	    /* Set soft-clipping if appropriate.  It's off by default. */
	    wk->soft_clipping_on = BoolResource(name, "softclipping",
						"Softclipping", rDB) == 1;

	    /* Save the setting of backing-store. */
	    wk->backing_store_on	= xswa.backing_store == Always;
	}					/* window created */
    }						/* color-mapping initialized */

    if (VisualList != NULL)
	XFree((char *) VisualList);
#endif
    return status;
}
#endif
/*
 * 9/19/94 - DNW
 * Set the given XGKS window -- with all its associated attributes.
 */
#ifdef USEX11
    static
SetWindow(name, rDB, wk)
    char           *name;               /* program name */
    XrmDatabase     rDB;                /* resource database */
    WS_STATE_PTR    wk;                 /* workstation structure */
{
    Display        *dpy = wk->dpy;      /* for convenience */
    Window          win = wk->win;      /* for convenience */
    XSetWindowAttributes xswa;             /* window attributes */
    XSizeHints      SizeHints;          /* window size hints */
    XVisualInfo     VisualTemplate;
    XVisualInfo    *VisualList;
    int 	    bg=0,fg=1;
    int             status = 0;         /* success */
    int             init_status=0;      /* init_success */
    int             NumMatched;         /* number of visuals found */

    /* Obtain the current attributes of the window 
    XGetWindowAttributes(dpy, win, &xswa);*/

    /* Initialize color-mapping. */
    /* DNW - I always want 256 colors. wk->wscolour = DisplayCells(dpy, DefaultScreen(dpy));*/
    wk->wscolour = 256;
    /* size of colour map */
    VisualTemplate.screen = DefaultScreen(dpy);
    VisualTemplate.visualid =
        DefaultVisual(dpy, DefaultScreen(dpy))->visualid;
    if ((VisualList = XGetVisualInfo(dpy, VisualScreenMask | VisualIDMask,
                                     &VisualTemplate, &NumMatched)) == NULL)
        status = 26;
    if (status != 26) init_status = XcInit(wk, VisualList);
    if ((status == 26) || (init_status == 0)) {
        status = 26;
    } else {
	/* Set the screen default colour map ID. */
	wk->dclmp = DefaultColormap(wk->dpy, DefaultScreen(wk->dpy));
	if (wk->wclmp == (int) NULL)
	   wk->wclmp = wk->dclmp;
	else
           wk->dclmp = wk->wclmp;
	wk->dclmp = wk->wclmp;

        /*
         * Get foreground and background colors (and set them in the
         * colormap).
         */
        (void) AllocGroundColors(wk, name, rDB, &wk->wsfg, &wk->wsbg);

	/*
	 * Init pointer to table of set values to NULL.  The table will be
	 * alloc'ed and init'ed in gsetcolourrep on the 1st call (DWO).
	 * 
	 * This may no longer be meaningful since I've changed the way the
	 * mapping from GKS color-index to X-colorcell-index is handled.
	 * --SRE 2/1/90
	 */
	wk->set_colour_rep = (Gcobundl *) NULL;

        /* Set the window-event mask. */
        wk->event_mask = StructureNotifyMask |
            ExposureMask |
            EnterWindowMask |
            LeaveWindowMask |
            FocusChangeMask |
            KeyPressMask |
            ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask;

        /*
         * Set the window-attributes structure.
        xswa.background_pixel = wk->wsbg;
        xswa.border_pixel = wk->wsfg;
         */
        xswa.event_mask = StructureNotifyMask | ExposureMask | EnterWindowMask |LeaveWindowMask | FocusChangeMask;
        /* DNW - must set the backgound color for TrueColor visuals */
	if (DefaultDepth(wk->dpy, DefaultScreen(wk->dpy)) != 8) {
           bg = WhitePixel(wk->dpy, DefaultScreen(wk->dpy));
           fg = BlackPixel(wk->dpy, DefaultScreen(wk->dpy));
        } else {
           bg = wk->wsbg;
	   fg = wk->wsfg;
	}
        xswa.background_pixel = bg;
        xswa.border_pixel = fg;
        /*xswa.background_pixel = XcPixelValue(wk, wk->wsbg);
        xswa.border_pixel = XcPixelValue(wk, wk->wsfg);*/
        xswa.do_not_propagate_mask = wk->event_mask &
            (KeyPressMask | KeyReleaseMask | ButtonPressMask |
             ButtonReleaseMask | PointerMotionMask | Button1MotionMask |
             Button2MotionMask | Button3MotionMask | Button4MotionMask |
             Button5MotionMask | ButtonMotionMask);
	xswa.colormap = wk->wclmp;          /* default colormap --SRE */
        xswa.backing_store      =
            BoolResource(name, "backingstore", "Backingstore", rDB) == 0
                ? NotUseful
                : DoesBackingStore(DefaultScreenOfDisplay(dpy)) == NotUseful
                    ? NotUseful
                    : Always;

        /* Set the window attributes */
        XChangeWindowAttributes(dpy, win, (unsigned long)
                         (CWDontPropagate | CWBackPixel | CWBorderPixel |
                          CWEventMask | CWColormap | CWBackingStore), &xswa);

        /* Create graphics-context for window. */
        (void) CreateGC(dpy, win, wk);

        /* Set foreground and background colors in graphics-context. */
	if (DefaultDepth(wk->dpy, DefaultScreen(wk->dpy)) == 8) {
	   XSetForeground(dpy, wk->gc, XcPixelValue(wk, wk->wsfg));
	   XSetBackground(dpy, wk->gc, XcPixelValue(wk, wk->wsbg));
	}
        /*XSetForeground(dpy, wk->gc, wk->wsfg);
        XSetBackground(dpy, wk->gc, wk->wsbg);*/

        /*
         * Initialize last-clipping rectangles to absurd values.  This
         * will cause actual clipping window to be set.
         */
        wk->last_pline_rectangle.x = 0;
        wk->last_pline_rectangle.y = 0;
        wk->last_pline_rectangle.width = 0;
        wk->last_pline_rectangle.height = 0;
        wk->last_pmarker_rectangle = wk->last_pline_rectangle;
        wk->last_farea_rectangle = wk->last_pline_rectangle;
        wk->last_text_rectangle = wk->last_pline_rectangle;

        wk->last_dash_index = 1;

        /* Set soft-clipping if appropriate.  It's off by default. */
        wk->soft_clipping_on = BoolResource(name, "softclipping",
                                            "Softclipping", rDB) == 1;

        /* Save the setting of backing-store. */
        wk->backing_store_on        = xswa.backing_store == Always;
                                           /* window created */
                                           /* color-mapping initialized */
    }

    if (VisualList != NULL)
        XFree((char *) VisualList);
    return status;

}
#endif

/*
 * Create a graphics-context for a window.
 */
#ifdef USEX11
    static void
CreateGC(dpy, win, wk)
    Display        *dpy;
    Window          win;
    WS_STATE_PTR    wk;
{
    wk->gc = XCreateGC(dpy, win, (unsigned long) 0,
		       (XGCValues *) NULL);
    wk->plinegc = XCreateGC(dpy, win, (unsigned long) 0,
			    (XGCValues *) NULL);
    wk->pmarkgc = XCreateGC(dpy, win, (unsigned long) 0,
			    (XGCValues *) NULL);
    wk->fillareagc = XCreateGC(dpy, win, (unsigned long) 0,
			       (XGCValues *) NULL);
    wk->textgc = XCreateGC(dpy, win, (unsigned long) 0,
			   (XGCValues *) NULL);
}

#endif
/*
 * Update the open-workstation table by saving the window-identifier.
 *
 * I don't like the fact that this routine assumes that the appropriate
 * slot exists -- SRE.
 */
    static void
UpdateOpenWSTable(wk)
    WS_STATE_PTR    wk;
{
    int             i;

    for (i = 0; i < MAX_OPEN_WS; i++)
	if (wk->ws_id == xgks_state.openedws[i].ws_id)
	    break;
#ifdef USEX11
    xgks_state.openedws[i].win = wk->win;
#endif
}



/*
 * Get window geometry defaults.
 */
#ifdef X11OUT
    static void
GetWindowGeometry(name, dpy, rDB, SizeHints)
    char           *name;		/* program name */
    Display        *dpy;
    XrmDatabase     rDB;		/* resource database */
    XSizeHints     *SizeHints;		/* window-size hints */
{
    char            buf[1024];
    char           *str_type[20];
    XrmValue        value;

    SizeHints->flags = 0;

    /*
     * Set to program-specified values and then override with any
     * user-specified values.
     */
    SizeHints->width = 640;
    SizeHints->height = 512;
    SizeHints->x = (DisplayWidth(dpy, DefaultScreen(dpy)) -
		    SizeHints->width) >> 1;
    SizeHints->y = (DisplayHeight(dpy, DefaultScreen(dpy)) -
		    SizeHints->height) >> 1;
    SizeHints->flags |= PSize | PPosition;

    if (XrmGetResource(rDB, strcat(strcpy(buf, name), ".geometry"),
		       "Xgks.Geometry", str_type, &value) == True) {

	int             x, y;
	long            flags;
	unsigned        width, height;

	(void) strncpy(buf, value.addr, (int) value.size);

	flags = XParseGeometry(buf, &x, &y, &width, &height);

	if (WidthValue & flags && HeightValue & flags &&
	     width >= 1 && width <= DisplayWidth(dpy, DefaultScreen(dpy)) &&
	  height >= 1 && height <= DisplayHeight(dpy, DefaultScreen(dpy))) {

	    SizeHints->width = width;
	    SizeHints->height = height;
	    SizeHints->flags |= USSize;
	}
	if (XValue & flags && YValue & flags) {
	    if (XNegative & flags)
		x += DisplayWidth(dpy, DefaultScreen(dpy)) -
		    SizeHints->width;
	    if (YNegative & flags)
		y += DisplayHeight(dpy, DefaultScreen(dpy)) -
		    SizeHints->height;
	    SizeHints->x = x;
	    SizeHints->y = y;
	    SizeHints->flags |= USPosition;
	}
    }
}

#endif

/*
 * Get foreground and background color defaults.
 */
#ifdef USEX11
    static void
AllocGroundColors(wk, name, rDB, fg, bg)
    WS_STATE_PTR    wk;			/* workstation structure */
    char           *name;		/* program name */
    XrmDatabase     rDB;		/* resource database */
    Gint           *fg, *bg;		/* fore/back-ground indexes */
{
    char            buf[1024];
    char           *str_type[20];
    XrmValue        value;

    if (BoolResource(name, "invertmono", "Invertmono", rDB) == 1 ||
	    BoolResource(name, "reverse", "Reverse", rDB) == 1) {
	*fg = 0;
	*bg = 1;
    } else {
	*fg = 1;
	*bg = 0;
    }

    /*
     * Set XGKS background color.
     */
    if (XrmGetResource(rDB, strcat(strcpy(buf, name), ".background"),
		       "Xgks.Background", str_type, &value) == True) {

	XColor          Xrep;

	(void) strncpy(buf, value.addr, (int) value.size);

	if (XParseColor(wk->dpy, DefaultColormap(wk->dpy,
			DefaultScreen(wk->dpy)), buf, &Xrep)) {

	    Gcobundl        GKSrep;

	    GKSrep.red   = (double)Xrep.red   / 65535.0;
	    GKSrep.green = (double)Xrep.green / 65535.0;
	    GKSrep.blue  = (double)Xrep.blue  / 65535.0;

	    (void) XcSetColour(wk, (Gint) 0, &GKSrep);
	} else {
	    (void) fprintf(stderr, "%s\"%s\"%s\n",
			   "AllocGroundColors: Background color ", buf,
			   " not known.  Using default.");
	}
    }
    /*
     * Set XGKS foreground color.
     */
    if (XrmGetResource(rDB, strcat(strcpy(buf, name), ".foreground"),
		       "Xgks.Foreground", str_type, &value) == True) {

	XColor          Xrep;

	(void) strncpy(buf, value.addr, (int) value.size);

	if (XParseColor(wk->dpy, DefaultColormap(wk->dpy,
				     DefaultScreen(wk->dpy)), buf, &Xrep)) {

	    Gcobundl        GKSrep;

	    GKSrep.red   = (double)Xrep.red   / 65535.0;
	    GKSrep.green = (double)Xrep.green / 65535.0;
	    GKSrep.blue  = (double)Xrep.blue  / 65535.0;

	    (void) XcSetColour(wk, (Gint) 1, &GKSrep);
	} else {
	    (void) fprintf(stderr, "%s\"%s\"%s\n",
			   "AllocGroundColors: Foreground color ", buf,
			   " not known.  Using default.");
	}
    }
}
#endif

/*
 * Get window-manager hints.
 */
#ifdef X11OUT
    static void
GetWMHints(dpy, name, rDB, WMHints)
    Display        *dpy;		/* display */
    char           *name;		/* program name */
    XrmDatabase     rDB;		/* resource database */
    XWMHints       *WMHints;		/* window-manager hints */
{
    char            buf[1024];
    char           *str_type[20];
    XrmValue        value;

    WMHints->flags = 0;

    WMHints->input = True;
    WMHints->flags |= InputHint;

    WMHints->initial_state =
	BoolResource(name, "iconic", "Iconic", rDB) == 1 ? IconicState
	: NormalState;
    WMHints->flags |= StateHint;

    if (XrmGetResource(rDB, strcat(strcpy(buf, name), ".icon.geometry"),
		       "Xgks.Icon.Geometry", str_type, &value) == True) {

	int             x, y;
	long            flags;
	unsigned        width, height;

	(void) strncpy(buf, value.addr, (int) value.size);

	flags = XParseGeometry(buf, &x, &y, &width, &height);

	if (XValue & flags && YValue & flags) {
	    if (XNegative & flags && !((WidthValue & flags)) ||
		    YNegative & flags && !((HeightValue & flags))) {

		(void) fprintf(stderr, "%s%s\n",
			       "GetWMHints: Negative X (Y) icon position ",
			       "requires height (width) spec.");
	    } else {
		if (XNegative & flags)
		    x += DisplayWidth(dpy, DefaultScreen(dpy)) - width;
		if (YNegative & flags)
		    y += DisplayHeight(dpy, DefaultScreen(dpy)) - height;

		WMHints->icon_x = x;
		WMHints->icon_y = y;
		WMHints->flags |= IconPositionHint;
	    }
	}
    }
}
#endif


/*
 * Initialize our local resource manager.  Taken from "X11R4/contrib/
 * examples/OReilly/Xlib/basecalc/basecalc.c".
 */
#ifdef USEX11
    static
InitDefaults(name, dpy, rDB)
    char           *name;		/* name of application */
    Display        *dpy;
    XrmDatabase    *rDB;		/* resource database */
{
    int             status = 1;		/* routine status = success */

    /* So we can use the resource manager data-merging functions */
    XrmInitialize();

    /* Clear resource database */
    *rDB = XrmGetStringDatabase("");

    /* Get server defaults, program defaults, and .Xdefaults and merge them */
    (void) GetUsersDatabase(name, rDB, dpy);

    return status;
}
#endif

/*
 * Get program's and user's defaults
 */
#ifdef USEX11
    static
GetUsersDatabase(prog, rDB, dpy)
    char           *prog;
    XrmDatabase    *rDB;		/* resource database */
    Display        *dpy;
{
    int             status = 1;		/* routine status = success */
    XrmDatabase     homeDB, serverDB, applicationDB;

    char            filename[1024];
    char           *environment;
    char            name[255];
    char           *appresdir = getenv("XAPPLRESDIR");

    if (appresdir == NULL)
	appresdir = "/usr/lib/X11/app-defaults";

    (void) strcpy(name, appresdir);
    (void) strcat(name, "/");
    (void) strcat(name, prog);

    /*
     * Get application defaults file, if any.
     */
    applicationDB = XrmGetFileDatabase(name);
    (void) XrmMergeDatabases(applicationDB, rDB);

    /*
     * MERGE server defaults, these are canonically created by xrdb, loaded 
     * as a property of the root window when the server initializes.  If not 
     * defined, then use the resources specified in ~/.Xdefaults.
     */
    {
	int		actual_format;
	Atom		actual_type;
	unsigned long	nitems;
	unsigned long	bytesafter;
	unsigned char	*prop;

	if (XGetWindowProperty(dpy, DefaultRootWindow(dpy), 
			       XA_RESOURCE_MANAGER, 
			       (long)0, (long)0, False,
			       XA_STRING, &actual_type, &actual_format, 
			       &nitems, &bytesafter, &prop) == Success
		&& actual_type == XA_STRING) {
	    (void) XGetWindowProperty(dpy, DefaultRootWindow(dpy), 
				      XA_RESOURCE_MANAGER, 
				      (long)0, (long)bytesafter, False,
				      XA_STRING, &actual_type, &actual_format, 
				      &nitems, &bytesafter, &prop);
	    serverDB = XrmGetStringDatabase((char*)prop);

	} else {
	    /*
	     * Read ~/.Xdefaults file.
	     */
	    (void) GetHomeDir(filename);
	    (void) strcat(filename, "/.Xdefaults");
	    serverDB = XrmGetFileDatabase(filename);
	}
    }
    XrmMergeDatabases(serverDB, rDB);

    /*
     * Open XENVIRONMENT file, or if not defined, the
     * ~/.Xdefaults-<hostname>, and merge into existing data base
     */
    if ((environment = getenv("XENVIRONMENT")) == NULL) {
	int             len;

	environment = GetHomeDir(filename);
	(void) strcat(environment, "/.Xdefaults-");
	len = strlen(environment);
#ifdef SYSINFO
	sysinfo(SI_HOSTNAME, environment + len, sizeof(filename) - len);
#else
	(void) gethostname(environment + len, sizeof(filename) - len);
#endif
    }
    homeDB = XrmGetFileDatabase(environment);
    XrmMergeDatabases(homeDB, rDB);

    return status;
}
#endif


/*
 * Get the path of the user's home directory.
 */
    static char*
GetHomeDir(dest)
    char           *dest;
{
    uid_t           uid;
    struct passwd  *pw;
    register char  *ptr;

    if ((ptr = getenv("HOME")) != NULL) {
	(void) strcpy(dest, ptr);

    } else {
	if ((ptr = getenv("USER")) != NULL) {
	    pw = getpwnam(ptr);
	} else {
	    uid = getuid();
	    pw = getpwuid(uid);
	}
	if (pw) {
	    (void) strcpy(dest, pw->pw_dir);
	} else {
	    *dest = '\0';
	}
    }
    return dest;
}


/*
 *  xXgksClearWs(wk) --- clear the corresponding x-window
 */
xXgksClearWs(wk)
    WS_STATE_PTR    wk;
{
    if (wk->ewstype != X_WIN)
	return OK;

 #ifdef X11WM
   (void) XgksSIGIO_OFF(wk->dpy);
    XClearArea(wk->dpy, wk->win, 0, 0, 0, 0, False);
    XSync(wk->dpy, 0);
    (void) XgksSIGIO_ON(wk->dpy);
#endif
    return OK;
}


/*
 * xXgksCloseWs(ws) --- close the corresponding x-window
 */
xXgksCloseWs(ws)
    WS_STATE_PTR    ws;
{
    if (ws->ewstype != X_WIN)
	return OK;
#ifdef X11WM
    (void) XgksSIGIO_OFF(ws->dpy);
    XUnmapWindow(ws->dpy, ws->win);
    XDestroyWindow(ws->dpy, ws->win);
    XFreeGC(ws->dpy, ws->gc);
    XSync(ws->dpy, 0);
#endif
    (void) XcEnd(ws);				/* free color-index
	

					 * mapping-thingy */
#ifdef X11WM
    (void) XgksSIGIO_ON(ws->dpy);
#endif

    return OK;
}


/*
 * xXgksHighLight(ws, bd) --- highlight a primitive
 */
xXgksHighLight(ws, bd)
    Gpoint         *bd;
    WS_STATE_PTR    ws;
{
#ifdef X11OUT
    Display        *dpy;
    Window          win;
    GC              gc;

    XPoint          box[5];
#else
    Gpoint          box[5];
#endif
    int             i;

#ifdef X11OUT
    dpy = ws->dpy;
    win = ws->win;
    gc = ws->gc;
#endif

    if (ws->ewstype != X_WIN)
	return OK;

#ifdef X11OUT
    (void) XgksSIGIO_OFF(ws->dpy);
    XSetFunction(dpy, gc, GXinvert);
    XSetLineAttributes(dpy, gc, 0, LineSolid, CapButt, JoinMiter);

    XSetFillStyle(dpy, gc, FillSolid);
#endif

    for (i = 0; i < 5; i++)
	NdcToX(ws, bd, &box[i]);		/* compound-statement macro */

#ifdef X11OUT
    XDrawLines(dpy, win, gc, box, 5, CoordModeOrigin);

    XFlush(ws->dpy);

    XSetFunction(dpy, gc, GXcopy);
    (void) XgksSIGIO_ON(ws->dpy);
#endif
    return OK;
}

