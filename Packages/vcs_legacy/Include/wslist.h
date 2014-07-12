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
 * This header-file depends upon header-files "gks_defines.h", "primitive.h",
 * and "wdt.h".
 *
 * $Id$
 */


#ifndef  WSLIST_H
#define  WSLIST_H
#ifndef USEX11
#include "gks.h"
#endif
#ifdef CAIRODRAW
#include <cairo/cairo.h>
#endif
/*
 * NDC to DC, DC to X, and NDC to X transformations are stored in the
 * workstation structure using the WS_TRANS structure.
 */
typedef struct {
    Gfloat          xScale, xTrans;
    Gfloat          yScale, yTrans;
}               WS_TRANS;


/* Workstation type */
typedef enum {
    WST_INVALID,			/* Invalid workstation */
    X_WIN,				/* X Window System */
    WISS,				/* Workstation-independent segment-
					 * storage */
    MI,					/* Metafile input */
    MO					/* Metafile output */
} EWSTYPE;


/* Dash-list type */
typedef struct {
    int             dn;			/* dash list length */
    char            dashl[17];		/* dash list */
} DashList;


extern DashList xgksDASHES[10];		/* defined in xpline.c */


/*
 * Workstation information for an X window (this structure is unused as yet):
 */
typedef struct Xwindow {
    struct ws_struct
		   *ws;			/* (Enclosing) workstation entry */
#ifdef USEX11
    Display        *dpy;		/* the display ID */
    Window          win;		/* the window ID */
#endif
    unsigned long   event_mask;		/* the initial window event mask */
#ifdef USEX11
    GC              gc;			/* the window graphics context */
    GC              plinegc;		/* graphics contexts for specific */
    GC              pmarkgc;		/* primitives */
    GC              fillareagc;
    GC              textgc;
    Colormap        dclmp;		/* the screen default colour map ID */
    Colormap        wclmp;		/* the window colourmap ID */
#endif
    Gpoint          wbound;		/* Current x window bound */
    Gint            wscolour;		/* Number of available colours on the
					 * ws */
    Gcobundl       *set_colour_rep;	/* colours set by user */
    Gint            wsfg, wsbg;		/* foreground and background pixel
					 * values */
    Gint	    clear_regen_segs;	/*DNW 10/6/94 - use to decide if to 
                                         * regenerate segments */
    WS_TRANS        dctoxtrans;		/* transformation constants from DC
					 * space to X_WIN space */
    WS_TRANS        ndctoxtrans;	/* composite of ndctodc and dctox */
#ifdef USEX11
    XRectangle      xclip;		/* the clip area in the X window */
    XRectangle      last_pline_rectangle;
    XRectangle      last_pmarker_rectangle;
    XRectangle      last_farea_rectangle;
    XRectangle      last_text_rectangle;
#else
    Grectangle      xclip;		/* the clip area in the X window */
    Grectangle      last_pline_rectangle;
    Grectangle      last_pmarker_rectangle;
    Grectangle      last_farea_rectangle;
    Grectangle      last_text_rectangle;
#endif
    Gint            last_dash_index;
    Gint            last_message_width;
#ifdef USEX11
    XcMap           XcMap;		/* GKS <-> X color-mapping */
#endif
    int             soft_clipping_on;   /* soft-clipping is enabled? */
    int		    backing_store_on;	/* backing-store is enabled? */
}               Xwindow;


/*
 * Type of Metafile:
 */
typedef enum mf_type {
    MF_GKSM	= 0,
    MF_CGM,
    MF_CAIRO
}		mf_type;


/*
 * Workstation information common to all Metafile implementations:
 */
#define MF_COMMON \
    mf_type	type;			/* Type of Metafile */ \
    Gfile	*fp;			/* File structure */ \
    Gint	filestat;		/* File status */ \
    Ggksmit	CurItem;		/* Current item (type, length) */ \
    Gint	GksmEmpty;		/* Metafile is empty? */


/*
 * Common Metafile data-structure:
 */
typedef struct mf_any {
    MF_COMMON
}		mf_any;

/*
 * Workstation information specific to Metafiles:
 *
 * The implementation details are hidden in the individual modules.
 */
typedef union Metafile {
    mf_any		*any;
    struct mf_gksm	*gksm;
    struct mf_cgmi	*cgmi;
    struct mf_cgmo	*cgmo;
}		Metafile;


/*
 * Workstation information:
 */
typedef struct ws_struct {
    Gint            ws_id;		/* Workstation identifier */
    Gchar          *conn;		/* Workstation Connection and type */
    Gchar          *wstype;		/* workstation type */
    EWSTYPE         ewstype;		/* enum ws type */

    Gint            ws_is_closing;	/* true = closing */

    Metafile        mf;			/* Metafile information: */

    /*
     * Entries in this group do not exist for workstation of categories INPUT
     * and MI
     */
    Gwsstate        wsstate;		/* Workstation state
					 * [ACTIVE/INACTIVE] */

    /*
     * Entries in this group do not exist for workstation of categoeies
     * INPUT, WISS, MI
     */
    Gstore          primi_store;	/* state of storage of non-segment
					 * primitives */
    Gpoint          size;		/* Workstation DC space size */
    Gwsti           wsti;		/* Workstation transformation
					 * information */
    Gwsdus          wsdus;		/* Workstation defferal & update
					 * state */
    /*
     * User-defined function called each time after workstation redraw
     */
    Gint            (*redrawfuncp) ();

    /*
     * Entries in this group do not exist for workstation of categoeies
     * INPUT, WISS, MI, MO
     */
    Glnbundl        lnbundl_table[MAX_BUNDL_TBL];	/* Polyline bundle
							 * table */
    Gmkbundl        mkbundl_table[MAX_BUNDL_TBL];	/* Polymarker bundle
							 * table */
    Gtxbundl        txbundl_table[MAX_BUNDL_TBL];	/* Text bundle table */
    Gflbundl        flbundl_table[MAX_BUNDL_TBL];	/* Fill area bundle
							 * table */
    Gptbundl        ptbundl_table[MAX_BUNDL_TBL];	/* Pattern bundle
							 * table */

    /*
     * Segments associated with this workstation -- this structure is very
     * important when we want to clear a ws
     */
    WS_SEG_LIST    *seglist;
    WS_SEG_LIST    *seg_insertpt;
    Gint            seg_list_dirty;	/* Flag indicating that ws->seglist
					 * needs to be re-arrange before next
					 * gks-redraw */

    /*
     * A note on implementation, this list will be mantain by routines in
     * segment.c and should only be changed by routines in it !
     */

    /*
     * logical input devices are implemented as a linked list of all the
     * devices that have been used
     */
    INPUT_DEV      *in_dev_list;

    Glimit          clip;		/* Intersection between NDC-viewport
					 * and WS_window */

    /*
     * Following output primitive list are for non-segment primitives
     * associated with this workstation
     */
    OUT_PRIMI       primi_list;		/* First primitve is always a
					 * CLIP_REC */
    OUT_PRIMI      *primi_insert_pt;
    /*
     * Primitive manager needs this for efficient insertion
     */
    OUT_PRIMI      *message_pt;		/* Points to mesg. prim. in list;
					 * NULL if none */
    OUT_PRIMI      *bef_message;	/* Points to node before mesg in list */

    WS_TRANS        ndctodctrans;	/* transformation constants from NDC
					 * space to DC space */

    /*
     * X-specific stuff.  NB: this should be unioned (and, hopefully, will
     * eventually be) with the Metafile union defined above.
     */
#ifdef USEX11
    Display        *dpy;		/* the display ID */
    Window          win;		/* the window ID */
    unsigned long
                    event_mask;		/* the initial window event mask */
    GC              gc;			/* the window graphics context */
    GC              plinegc;		/* graphics contexts for specific */
    GC              pmarkgc;		/* primitives */
    GC              fillareagc;
    GC              textgc;
    Colormap        dclmp;		/* the screen default colour map ID */
    Colormap        wclmp;		/* the window colourmap ID */
#endif
#ifdef CAIRODRAW
    cairo_t        *cr;                 /* the cairo context */
#endif
    Gpoint          wbound;		/* Current x window bound */
    Gint            wscolour;		/* Number of available colours on the
					 * ws */
    Gcobundl       *set_colour_rep;	/* colours set by user */
    Gint            wsfg, wsbg;		/* foreground and background pixel
					 * values */
    Gint	    clear_regen_segs;	/* DNW 10/6/94 - use to decide if to 
                                         * regenerate segments */
    WS_TRANS        dctoxtrans;		/* transformation constants from DC
					 * space to X_WIN space */
    WS_TRANS        ndctoxtrans;	/* composite of ndctodc and dctox */
#ifdef USEX11
    XRectangle      xclip;		/* the clip area in the X window */
    XRectangle      last_pline_rectangle;
    XRectangle      last_pmarker_rectangle;
    XRectangle      last_farea_rectangle;
    XRectangle      last_text_rectangle;
#else
    Grectangle      xclip;		/* the clip area in the X window */
    Grectangle      last_pline_rectangle;
    Grectangle      last_pmarker_rectangle;
    Grectangle      last_farea_rectangle;
    Grectangle      last_text_rectangle;
#endif
    Gint            last_dash_index;
    Gint            last_message_width;
#ifdef USEX11
    XcMap           XcMap;		/* GKS <-> X color-mapping */
#endif
    int             soft_clipping_on;	/* soft-clipping is enabled? */
    int		    backing_store_on;	/* backing-store is enabled? */
}              WS_STATE_ENTRY;

typedef WS_STATE_ENTRY *WS_STATE_PTR;


#define	NOT_SET	(-9.99)


extern Gwscat          XgksWsCategory		PROTO((WS_STATE_PTR ws));
extern EWSTYPE         XgksWsTypeToEnum		PROTO((Gchar *wstype));
extern WS_STATE_PTR    XgksValidWsId		PROTO((Gint ws_id));


/*
 * VALID_WSID(i) used to check to see if there was a workstation open with
 * the name i.  I renamed this function OPEN_WSID and created a new VALID_WSID
 * that checks if the ws id is valid (non-negative).  This was all done in the
 * name of PTR c1012: many functions returned error 25 (ws not open) in cases
 * where error 20 (ws invalid) would be more appropriate.
 */
#define VALID_WSID(i)	((i) >= 0)
#define OPEN_WSID(i)	(XgksValidWsId(i))
#define WS_CAT(t)	(XgksWsCategory(t))


#define NdcToDc(ws, ndc, dc) { \
    /* WS_STATE_ENTRY *ws; Gpoint *ndc, *dc; */  \
    (dc)->x = (ndc)->x * (ws)->ndctodctrans.xScale \
	    + (ws)->ndctodctrans.xTrans; \
    (dc)->y = (ndc)->y * (ws)->ndctodctrans.yScale \
	    + (ws)->ndctodctrans.yTrans; \
}


#define DcToNdc(ws, dc, ndc) { \
    /* WS_STATE_ENTRY *ws; Gpoint *dc, *ndc; */  \
    (ndc)->x = ((dc)->x - (ws)->ndctodctrans.xTrans) \
	    / (ws)->ndctodctrans.xScale; \
    (ndc)->y = ((dc)->y - (ws)->ndctodctrans.yTrans) \
	    / (ws)->ndctodctrans.yScale; \
}


/*
 * Note the rounding performed in the following by the addition of the 0.5
 * term.  We do this to obtain the X pixel closest to the DC point.  We use
 * a positive 0.5 for rounding because only non-negative X window co-ordinates
 * are useful.
 */
#define DcToX(ws, dc, xpt) { \
    /* WS_STATE_ENTRY *ws; Gpoint *dc; XPoint *xpt; */  \
    (xpt)->x = (short)((dc)->x * (ws)->dctoxtrans.xScale \
	    + (ws)->dctoxtrans.xTrans + 0.5); \
    (xpt)->y = (short)(ws->wbound.y - ((dc)->y * (ws)->dctoxtrans.yScale \
	    + (ws)->dctoxtrans.yTrans) + 0.5); \
}


#define XToDc(ws, xpt, dc) { \
    /* WS_STATE_ENTRY *ws; Gpoint *dc; XPoint *xpt; */  \
    (dc)->x = ((xpt)->x - (ws)->dctoxtrans.xTrans) \
	    / (ws)->dctoxtrans.xScale; \
    (dc)->y = ((ws->wbound.y - (xpt)->y) - (ws)->dctoxtrans.yTrans) \
	    / (ws)->dctoxtrans.yScale; \
}


/*
 * Note the rounding performed in the following by the addition of the 0.5
 * term.  We do this to obtain the X pixel closest to the NDC point.  We use
 * a positive 0.5 for rounding because only non-negative X window co-ordinates
 * are useful.  (Aside: Instead of the 0.5 term in the following, Harry Edmon,
 * in his fix, had 0.001).
 */
#define NdcToX(ws, ndc, xpt) { \
    /* WS_STATE_ENTRY *ws; Gpoint *ndc; XPoint *xpt; */  \
    (xpt)->x = (short)((ndc)->x * (ws)->ndctoxtrans.xScale \
	    + (ws)->ndctoxtrans.xTrans + 0.5); \
    (xpt)->y = (short)(ws->wbound.y - ((ndc)->y * (ws)->ndctoxtrans.yScale \
	    + (ws)->ndctoxtrans.yTrans) + 0.5); \
}


#define XToNdc(ws, xpt, ndc) { \
    /* WS_STATE_ENTRY *ws; Gpoint *ndc; XPoint *xpt; */  \
    (ndc)->x = ((xpt)->x - (ws)->ndctoxtrans.xTrans) \
	    / (ws)->ndctoxtrans.xScale; \
    (ndc)->y = ((ws->wbound.y - (xpt)->y) - (ws)->ndctoxtrans.yTrans) \
	    / (ws)->ndctoxtrans.yScale; \
}


/*
 * The following lines are for the GKS-color-index-to-X-color-cell mapping
 * abstraction.
 *
 * The "Xc" prefix refers to "X-color".
 */

#ifndef PROTO
#   define	PROTO(x)	()
#endif


/*
 * Procedural interface to the GKS <-> X color-mapping abstraction:
 */
extern int	XcNew		PROTO((WS_STATE_PTR XcWs));
extern int	XcInit		PROTO((WS_STATE_PTR XcWs, XVisualInfo *vinfo));
extern int	XcSetColour	PROTO((WS_STATE_PTR XcWs, Gint ColourIndex,
				       Gcobundl *XcRep));
extern unsigned long	
	    	XcPixelValue    PROTO((WS_STATE_PTR XcWs, Gint ColourIndex));
extern Gint	XcColourIndex	PROTO((WS_STATE_PTR XcWs,
				       unsigned long PixelValue));
extern int	XcEnd		PROTO((WS_STATE_PTR XcWs));

#endif					/* WSLIST_H not defined */
