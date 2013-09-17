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
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 *
 * xtext.c - functions and data for gks text.
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <math.h>
#include "gks_implem.h"
#ifdef X11DRAW
  #include "font.h"
  static struct {
    Gint            f_loaded;
    FONT           *f_font;
  }               FontTable[DEFINED_FONT_TYPE + 1];

#else
  #include "cairo/cairo.h"
  /*#include <cairo/cairo-xlib.h>*/
  /*#include <cairo/cairo-xlib-xrender.h>*/
  #include "workstations.h"
  extern Gconid_X_drawable connect_id;/*VCS canvas display and drawable id */
#endif

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif

/* PCMDI - redefined in /usr/include/math.h
#define PI	3.1415926535897932384626433
*/

/* Hope this is enough, it should be */
#define MAX_CHAR_STROKE	 100

static Gpoint          sndc_ht;
static Gpoint          sndc_wt;

static Gfloat          stroke_trans[6];

static Gfloat          wsr_spacing;		/* additional spacing */
static Gint            wsr_font;
static unsigned long   wsr_colour;

static unsigned int    DCYMAX;

static Gint     FirstTime = TRUE;

//static Display        *dpy;
//static Window          win;
//static GC              gc;

static int      xPlotStrokeChar();
static Gfloat   xFxMax();
static Gfloat   xFyMin();
static Gfloat   xFyMax();
static int      xReadFont();
static int      xIdentityMatrix();
static int      xTransMatrix();
static int      xCopyMatrix();
static int      xMultMatrix();

#ifdef CAIRODRAW
#include "ttffonts.h"
extern FT_Library ft_library;
extern cairo_font_face_t *CAIRO_FONT_FACES[MAX_FONTS];
#endif

#ifdef X11DRAW
    static 
xComputeText(ws, tx)
    WS_STATE_PTR    ws;
    TEXT_ST        *tx;
{
    Gint            idx;

    DCYMAX = ws->wbound.y;			/* Need this info later */

    idx = tx->txattr.text;
    if (tx->txattr.text < 1 || tx->txattr.text >= MAX_BUNDL_TBL)
	idx = 1;

    /* get the font */
    if (tx->txattr.fp == GINDIVIDUAL)
	wsr_font = tx->txattr.bundl.fp.font;
    else
	wsr_font = ws->txbundl_table[idx].fp.font;

    if (FontTable[wsr_font].f_loaded != TRUE)
	if (xReadFont(wsr_font) != 0) {
	    wsr_font = 1;
	    if (xReadFont(wsr_font) != 0) {
		(void) fprintf(stderr,
		     "XGKS: can't load font %d from path '%s' - aborting\n",
			       wsr_font, xgks_state.fontdbdir);
		exit(0);
	    }
	}

    /* font colour */
    if (tx->txattr.colour == GINDIVIDUAL)
	wsr_colour = tx->txattr.bundl.colour;
    else
	wsr_colour = ws->txbundl_table[idx].colour;

    if (!WS_AVAIL_COLOUR(ws, wsr_colour))
	wsr_colour = 1;
    if (ws->wscolour == 2) {			/* monochrome ? */
	if (wsr_colour == 0)
	    wsr_colour = ws->wsbg;
	else if (wsr_colour == 1)
	    wsr_colour = ws->wsfg;
    }

    /* New Text Transformation code */
    /* up_vector (with height) in X coordinate */
    sndc_ht.x = (tx->up_vec.x * ws->ndctoxtrans.xScale);
    sndc_ht.y = (tx->up_vec.y * ws->ndctoxtrans.yScale);

    /* Base vector (with size) in X coordinate */
    sndc_wt.x = (tx->base_vec.x * ws->ndctoxtrans.xScale);
    sndc_wt.y = (tx->base_vec.y * ws->ndctoxtrans.yScale);

    /* apply the char_expansion factor */
    if (tx->txattr.tx_exp == GINDIVIDUAL) {
	sndc_wt.x *= tx->txattr.bundl.ch_exp;
	sndc_wt.y *= tx->txattr.bundl.ch_exp;
    } else {
	sndc_wt.x *= ws->txbundl_table[idx].ch_exp;
	sndc_wt.y *= ws->txbundl_table[idx].ch_exp;
    }
    (void) xIdentityMatrix(stroke_trans);
    stroke_trans[0] =
	(sndc_wt.x) / (FontTable[wsr_font].f_font->fnominalx);
    stroke_trans[3] =
	(sndc_wt.y) / (FontTable[wsr_font].f_font->fnominalx);
    stroke_trans[1] =
	(sndc_ht.x) / (FontTable[wsr_font].f_font->fnominaly);
    stroke_trans[4] =
	(sndc_ht.y) / (FontTable[wsr_font].f_font->fnominaly);

    /* additional spacing scale factor */
    if (tx->txattr.space == GINDIVIDUAL)
	wsr_spacing = tx->txattr.bundl.space;
    else
	wsr_spacing = ws->txbundl_table[idx].space;
    /*
     * At this point the Scale and Rotate matrix is set up.  The rotation 
     * angle is in global variable 'alpha'; the colour is in wsr_colour the 
     * font number is in wsr_font.
     */
}

#endif

/*
 * xXgksText(ws, text)	-- TEXT output
 *
 * Note: this function will output tabs, form feeds, line feeds, and like 
 * control characters as a special symbol.
 */


xXgksText(ws, tx)
    WS_STATE_PTR    ws;
    TEXT_ST        *tx;
{
#ifdef X11DRAW
  xXgksTextOld(ws, tx);
#endif
#ifdef CAIRODRAW
  xXgksTextCAIRO(ws, tx);
#endif
}

#ifdef CAIRODRAW
xXgksTextCAIRO(ws, tx)
    WS_STATE_PTR    ws;
    TEXT_ST        *tx;
{

  
  Gpoint at,apoint;
  Gcliprec       clipping;
  cairo_t        *cr;
  cairo_surface_t *surface;
  double dx,dy;
  Gpoint          xp1,xp2;
#ifdef X11WM
  XPoint          xp1,xp2;
  XWindowAttributes 	xwa;
#endif
    extern char text_hpath,text_vpath,text_path;
    extern float text_height,text_angle;
    extern int text_color,text_font;

  if (ws->ewstype != X_WIN)
    return OK;

  at.x=tx->location->x;
  at.y=tx->location->y;

  if ((tx->up_vec.y<0.0000001) && ((-.0000001<tx->up_vec.y))) text_angle = 90.*sign(tx->up_vec.x);
  else text_angle = atan(tx->up_vec.x/tx->up_vec.y)/3.1415926536*180.;
  if (tx->up_vec.y<-0.00001) text_angle+=180.;
  text_angle = - text_angle;
  text_height = sqrt((tx->up_vec.x)*(tx->up_vec.x)+(tx->up_vec.y)*(tx->up_vec.y));

  text_font = tx->txattr.bundl.fp.font;
  text_color = tx->txattr.bundl.colour;
  switch (tx->chattr.path) {
  case GTP_LEFT:
    text_path = 'l';
    break;
  case GTP_RIGHT:
    text_path = 'r';
    break;
  case GTP_UP:
    text_path = 'u';
    break;
  case GTP_DOWN:
    text_path = 'd';
    break;
  }
  switch (tx->chattr.align.hor) {
  case GTH_NORMAL:
    text_hpath = 'l';
    break;
  case GTH_LEFT:
    text_hpath = 'l';
    break;
  case GTH_CENTRE:
    text_hpath = 'c';
    break;
  case GTH_RIGHT:
    text_hpath = 'r';
    break;
  }
  switch (tx->chattr.align.ver) {
  case GTV_TOP:
    text_vpath = 't';
    break;
  case GTV_CAP:
    text_vpath = 'c';
    break;
  case GTV_HALF:
    text_vpath = 'h';
    break;
  case GTV_NORMAL:
    text_vpath = 'h';
    break;
  case GTV_BASE:
    text_vpath = 'b';
    break;
  case GTV_BOTTOM:
    text_vpath = 's';
    break;
  }
  


  /* Ok we need to set the existing clip onto the new screen */
  ginqclip(&clipping);


  apoint.x = clipping.rec.xmin;
  apoint.y = clipping.rec.ymin;
  NdcToX(ws, &apoint, &xp1);

  apoint.x = clipping.rec.xmax;
  apoint.y = clipping.rec.ymax;
  NdcToX(ws, &apoint, &xp2);

      
/* #ifdef X11WM */
/*   XGetWindowAttributes(connect_id.display, connect_id.drawable, &xwa); */
/* #else */
/*   printf("insert your WM getgeom here\n"); */
/* #endif       */

  cr = connect_id.cr;
  //  printf("in text cairo statu is : %s\n",cairo_status_to_string(cairo_status(cr)));
  //cairo_rectangle(cr,xp1.x,xp1.y,xp2.x-xp1.x,xp2.y-xp1.y);
  //cairo_clip(cr);
 

  NdcToX(ws, &at, &xp1);
  dx = (double) xp1.x;
  dy = (double) xp1.y;
  cairo_move_to( cr, dx, dy );

  /* set up string defs for cairo drawing */
  //printf("trying to write: %s\n",tx->string);
  VCS2CAIRO_drawString(&at,tx->string,cr);


/*   cairo_destroy (cr); */
/*   cairo_surface_destroy (surface); */
}
#endif

#ifdef X11DRAW
xXgksTextOld(ws, tx)
    WS_STATE_PTR    ws;
    TEXT_ST        *tx;
{
    Gchar          *string;
    Gchar          *text_ptr;
    Gpoint          atsndc;
    FONT           *cfont;
    Gfloat          fsize, fwidth, xtrans, ytrans;
    struct vcharst *cdef;
    Gint            i;

    if (ws->ewstype != X_WIN)
	return OK;

    (void) XgksSIGIO_OFF(ws->dpy);

#ifdef TDEBUG
    (void) fprintf(stderr, "\n\nEntering ......---->  XTEXT  <-----......\n");
#endif

    if (FirstTime == TRUE) {			/* First Time initializing */
	for (i = 0; i < DEFINED_FONT_TYPE; i++)
	    FontTable[i].f_loaded = FALSE;
	FirstTime = FALSE;
    }

    /* X-window attributes init */
    dpy = ws->dpy;
    win = ws->win;
    gc = ws->textgc;

    xComputeText(ws, tx);

    cfont = FontTable[wsr_font].f_font;

    /*
     * Compute transformation to move the whole string to the alignment point
     * transform the NDC start point to Scaled NDC.
     */
    atsndc.x = (tx->location->x * ws->ndctoxtrans.xScale) +
	       ws->ndctoxtrans.xTrans;
    atsndc.y = (tx->location->y * ws->ndctoxtrans.yScale) +
	       ws->ndctoxtrans.yTrans;

/*     printf("tx loc: %f,%f\n",atsndc.x,atsndc.y); */
#ifdef TDEBUG
    (void) fprintf(stderr, " from xtext ... ATSNDC.x=%f  atsndc.y=%f\n",
		   atsndc.x, atsndc.y);
#endif

    stroke_trans[2] = atsndc.x;
    stroke_trans[5] = atsndc.y;

#ifdef TDEBUG
    (void) fprintf(stderr, "after matrix ops of ATSNDc ..\n");
    xDumpMatrix("stroke_trans", stroke_trans);
#endif

    /* find total length or height of string + spacing */
    for ((string = tx->string), fsize = 0.0, fwidth = -9999;
		*string; string++) {
	if (cfont->fcharoffset[*string] >= 0) {	/* defined ? */
	    cdef = &(cfont->fchars[cfont->fcharoffset[*string]]);
	    switch (tx->chattr.path) {
	    case GTP_LEFT:
	    case GTP_RIGHT:
		fsize += xFxMax(cdef);
		fsize += cfont->fnominalx * wsr_spacing;
		break;
	    case GTP_UP:
	    case GTP_DOWN:
		fsize += xFyMax(cdef) - xFyMin(cdef);
		fsize += cfont->fnominaly * wsr_spacing;
		if ((i = xFxMax(cdef)) > fwidth)
		    fwidth = i;
		break;
	    }
	}
    }


#ifdef TDEBUG
    (void) fprintf(stderr, "xtext 	fsize = %f fwidth=%f\n", fsize,
		   fwidth);
#endif

    /*
     * translate alignment point to origin which is currently at the LEFT
     * BASE of the first character in the string.
     */
    switch (tx->chattr.path) {
    default:
    case GTP_RIGHT:
	switch (tx->chattr.align.hor) {
	default:
	case GTH_NORMAL:
	case GTH_LEFT:
	    xtrans = 0;
	    break;
	case GTH_CENTRE:
	    xtrans = -fsize / 2.0;
	    break;
	case GTH_RIGHT:
	    xtrans = -fsize;
	    break;
	}
	switch (tx->chattr.align.ver) {
	case GTV_TOP:
	    ytrans = -cfont->ftop;
	    break;
	case GTV_CAP:
	    ytrans = -cfont->fcap;
	    break;
	case GTV_HALF:
	    ytrans = -cfont->fhalf;
	    break;
	default:
	case GTV_NORMAL:
	case GTV_BASE:
	    ytrans = -cfont->fbase;
	    break;
	case GTV_BOTTOM:
	    ytrans = -cfont->fbottom;
	    break;
	}
	break;

    case GTP_LEFT:
	switch (tx->chattr.align.hor) {
	case GTH_LEFT:
	    xtrans = fsize -
		xFxMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]]));
	    break;
	case GTH_CENTRE:
	    xtrans = fsize / 2.0 -
		xFxMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]]));
	    break;
	default:
	case GTH_NORMAL:
	case GTH_RIGHT:
	    xtrans =
		-xFxMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]]));
	    break;
	}
	switch (tx->chattr.align.ver) {
	case GTV_TOP:
	    ytrans = -cfont->ftop;
	    break;
	case GTV_CAP:
	    ytrans = -cfont->fcap;
	    break;
	case GTV_HALF:
	    ytrans = -cfont->fhalf;
	    break;
	default:
	case GTV_NORMAL:
	case GTV_BASE:
	    ytrans = -cfont->fbase;
	    break;
	case GTV_BOTTOM:
	    ytrans = -cfont->fbottom;
	    break;
	}
	break;
    case GTP_UP:
	switch (tx->chattr.align.hor) {
	case GTH_LEFT:
	    xtrans = 0;
	    break;
	default:
	case GTH_NORMAL:
	case GTH_CENTRE:
	    xtrans = -fwidth / 2.0;
	    break;
	case GTH_RIGHT:
	    xtrans = -fwidth;
	    break;
	}
	switch (tx->chattr.align.ver) {
	case GTV_TOP:
	    ytrans = -fsize;
	    break;
	case GTV_CAP:
	    ytrans = -fsize - cfont->fnominaly * wsr_spacing;

	    break;
	case GTV_HALF:
	    ytrans = -fsize / 2.0;
	    break;
	default:
	case GTV_NORMAL:
	case GTV_BASE:
	    ytrans = 0;
	    break;
	case GTV_BOTTOM:
	    ytrans = -cfont->fbottom;
	    break;
	    /* Changed fsize to -fsize   PTR# c1083  */
	}
	break;

    case GTP_DOWN:
	switch (tx->chattr.align.hor) {
	default:
	case GTH_LEFT:
	    xtrans = 0;
	    break;
	case GTH_NORMAL:
	case GTH_CENTRE:
	    xtrans = -fwidth / 2.0;
	    break;
	case GTH_RIGHT:
	    xtrans = -fwidth;
	    break;
	}
	switch (tx->chattr.align.ver) {
	default:
	case GTV_NORMAL:
	case GTV_TOP:
	    ytrans = -cfont->ftop;
	    break;
	case GTV_CAP:
	    ytrans = -cfont->fcap;
	    break;
	case GTV_HALF:
	    ytrans = fsize / 2.0 -
		xFyMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]]));
	    break;
	case GTV_BASE:
	    ytrans = fsize -
		xFyMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]]));
	    break;
	case GTV_BOTTOM:
	    ytrans = fsize -
		xFyMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]])) -
		cfont->fbottom;
	    break;
	}
	break;
    }

#ifdef TDEBUG
    (void) fprintf(stderr, "ibmrt_ddgks:text: xtrans=%f ytrans=%f\n", xtrans,
		   ytrans);
#endif

    (void) xTransMatrix(stroke_trans, xtrans, ytrans);

#ifdef TDEBUG
    (void) fprintf(stderr, "ibmrt_ddgks:text: xtrans=%f ytrans=%f\n", xtrans,
		   ytrans);
    xDumpMatrix("stroke_trans", stroke_trans);
#endif

    /* set up x-system output attribute */
    xXgksSetTextClipRectangles(dpy, gc, ws, &(ws->xclip));
    xXgksSetForeground(dpy, gc, XcPixelValue(ws, (Gint) wsr_colour));
    xXgksSetLineAttributes(dpy, gc, 0, LineSolid, CapButt, JoinMiter);
    xXgksSetFillStyle(dpy, gc, FillSolid);

    for (text_ptr = tx->string; *text_ptr; text_ptr++) {
	xPlotStrokeChar(&(cfont->fchars[cfont->fcharoffset[*text_ptr]]));

	/* modify trans to move to LEFT BASE of next char */
	switch (tx->chattr.path) {
	default:
	case GTP_RIGHT:
	    xtrans = xFxMax(&(cfont->fchars[cfont->fcharoffset[*text_ptr]]))
		+ cfont->fnominalx * wsr_spacing;
	    ytrans = 0;
	    break;
	case GTP_LEFT:
	    xtrans =
		-xFxMax(&(cfont->fchars[cfont->fcharoffset[*(text_ptr + 1)]]))
		- cfont->fnominalx * wsr_spacing;
	    ytrans = 0;
	    break;
	case GTP_UP:
	    ytrans = xFyMax(&(cfont->fchars[cfont->fcharoffset[*text_ptr]])) -
		xFyMin(&(cfont->fchars[cfont->fcharoffset[*(text_ptr + 1)]]))

	    /* Corrects y pos of char */
		+ cfont->fnominaly * wsr_spacing;
	    xtrans = 0;
	    break;
	case GTP_DOWN:
	    ytrans =
		-xFyMax(&(cfont->fchars[cfont->fcharoffset[*(text_ptr + 1)]]))
		+ xFyMin(&(cfont->fchars[cfont->fcharoffset[*(text_ptr)]]))
		- cfont->fnominaly * wsr_spacing;

	    /* Corrects y pos of char */
	    xtrans = 0;
	    break;
	}
	(void) xTransMatrix(stroke_trans, xtrans, ytrans);
    }

    (void) XgksSIGIO_ON(ws->dpy);
    return OK;
}


/*
 * Vector Font Routines
 */

/*
 * xPlotStrokeChar(c) - utility function to plot a single stroke precision 
 *			font character.
 *
 * char c;			the character to plot.
 *
 * returns nothing.
 */
    static
xPlotStrokeChar(cvp)
    struct vcharst *cvp;
{
    char            LastType = 'm';
    int             cnt;
    Gpoint          tmp;
    XPoint         *pt, pts[MAX_CHAR_STROKE];

    cnt = 0;
    pt = pts;
    tmp.x = (int) (stroke_trans[0] * 0 + stroke_trans[1] * 0 +
		   stroke_trans[2] + 0.5);
    tmp.y = DCYMAX - (int) (stroke_trans[3] * 0 + stroke_trans[4] * 0 +
			    stroke_trans[5] + 0.5);
    pt++;
    cnt++;

    while ((cvp->vc_type != 'e')) {		/* get the next stroke */
	if (cnt >= MAX_CHAR_STROKE) {
	    XDrawLines(dpy, win, gc, pts, cnt, CoordModeOrigin);
	    pts[0] = pts[--cnt];
	    cnt = 1;
	}
	switch (cvp->vc_type) {
	case 'm':
	    if (LastType == 'd')
		XDrawLines(dpy, win, gc, pts, cnt, CoordModeOrigin);
	    LastType = 'm';
	    cnt = 1;
	    pt = pts;
	    tmp.x = cvp->vc_x;
	    tmp.y = cvp->vc_y;
	    pt->x = (int) (stroke_trans[0] * tmp.x +
			   stroke_trans[1] * tmp.y + stroke_trans[2] + 0.5);
	    pt->y = (DCYMAX) - (int) (stroke_trans[3] * tmp.x
		     + stroke_trans[4] * cvp->vc_y + stroke_trans[5] + 0.5);
	    pt++;
	    break;
	case 'd':
	    LastType = 'd';
	    tmp.x = cvp->vc_x;
	    tmp.y = cvp->vc_y;
	    pt->x = (int) (stroke_trans[0] * tmp.x +
			   stroke_trans[1] * tmp.y + stroke_trans[2] + 0.5);
	    pt->y = (DCYMAX) - (int) (stroke_trans[3] * tmp.x
		     + stroke_trans[4] * cvp->vc_y + stroke_trans[5] + 0.5);
	    cnt++;
	    pt++;
	    break;
	default:
	    break;
	}
	cvp++;
    }
    if (cnt > 1)
	XDrawLines(dpy, win, gc, pts, cnt, CoordModeOrigin);
    XFlush(dpy);
}


/*
 * xFxMax(struct vcharst *cvp)	returns width of char starting at vcp
 */
    static Gfloat
xFxMax(cvp)
    struct vcharst *cvp;
{
    while (cvp->vc_type != 'e') {
	switch (cvp->vc_type) {
	case 'S':
	    return (Gfloat) cvp->vc_x;
	}
	cvp++;
    }
    return 0.0;					/* oops, should have found a
						 * 'S' record */
}


/*
 * xFyMin(struct vcharst *cvp)	returns depth from base of char starting at vcp
 *     added to correct y positioning of characters with descenders.
 */
    static Gfloat
xFyMin(cvp)
    struct vcharst *cvp;
{
    while (cvp->vc_type != 'e') {
	switch (cvp->vc_type) {
	case 's':
	    return (Gfloat) cvp->vc_y;
	}
	cvp++;
    }
    return 0.0;					/* oops, should have found a
						 * 's' record */
}


/*
 * xFyMax(struct vcharst *cvp)	returns height from base of char starting at 
 *				vcp
 */
    static Gfloat
xFyMax(cvp)
    struct vcharst *cvp;
{
    while (cvp->vc_type != 'e') {
	switch (cvp->vc_type) {
	case 'S':
	    return (Gfloat) cvp->vc_y;
	}
	cvp++;
    }
    return 0.0;					/* oops, should have found a
						 * 'S' record */
}


/*
 * xReadFont(f)	Read font number f from the font database.
 */
    static int
xReadFont(f)
    int             f;
{
    char            fontpath[1000];
    int             fd;
    int             size;

    if (f < 0 || f > DEFINED_FONT_TYPE + 1)
	return -1;

    if (FontTable[f].f_loaded == TRUE)		/* already loaded */
	return 0;

    /* xgks_state.fontdbdir is set in text.c to either */
    /* FONTDBDIR set by Makefile or the environment */
    /* variable XGKSFontDir      (DWO)              */
    (void) sprintf(fontpath, "%s/%d", xgks_state.fontdbdir, f);
    if ((fd = open(fontpath, O_RDONLY, 0644)) < 0) {
	(void) close(fd);
	return -1;
    }
    if (read(fd, (char *) &size, sizeof(int)) != sizeof(int)) {
	(void) close(fd);
	return -1;
    }
    if ((FontTable[f].f_font = (FONT *) malloc((size_t) size)) == NULL) {
	(void) close(fd);
	return -1;
    }
    if (read(fd, (char *) FontTable[f].f_font, (unsigned) size) != size) {
	ufree((voidp)FontTable[f].f_font);
	(void) close(fd);
	return -1;
    }
    (void) close(fd);
    FontTable[f].f_loaded = TRUE;
    return 0;
}


/*
 * Matrix operations:
 *
 * a matrix is an array of six REALs which specify the first two columns
 * of a 3x3 2D transformation matrix.
 */
    static
xIdentityMatrix(m)
    Gfloat          m[];
{
    m[0] = 1;
    m[3] = 0;					/* 0 */
    m[1] = 0;
    m[4] = 1;					/* 0 */
    m[2] = 0;
    m[5] = 0;					/* 1 */
    return OK;
}


#ifdef TDEBUG
    static int
xScaleMatrix(m, sx, sy)
    Gfloat          m[];
    double          sx, sy;
{
    Gfloat          sm[6], newm[6];

    (void) xIdentityMatrix(sm);
    sm[0] = sx;
    sm[4] = sy;
    (void) xMultMatrix(m, sm, newm);
    (void) xCopyMatrix(m, newm);
}


    static int
xRotateMatrix(m, axis, rad)
    Gfloat          m[];
    int             axis;
    double          rad;
{
    Gfloat          rm[6], newm[6];
    double          c, s;

    c = cos(rad);
    s = sin(rad);

    (void) xIdentityMatrix(rm);
    switch (axis) {
    case 'Z':
	rm[0] = c;
	rm[1] = -s;
	rm[3] = s;
	rm[4] = c;
	break;
    default:
	return;
    }
    (void) xMultMatrix(m, rm, newm);
    (void) xCopyMatrix(m, newm);
}
#endif	/* TDEBUG defined */


    static
xTransMatrix(m, tx, ty)
    Gfloat          m[];
    Gfloat          tx, ty;
{
    Gfloat          tm[6], newm[6];

    (void) xIdentityMatrix(tm);
    tm[2] = tx;
    tm[5] = ty;
    (void) xMultMatrix(tm, m, newm);
    (void) xCopyMatrix(m, newm);
    return OK;
}


    static
xCopyMatrix(dest, src)
    Gfloat          dest[], src[];
{
    int             i;

    for (i = 0; i < 6; i++)
	dest[i] = src[i];
    return 0;
}


/*
 * matrix multiply: a * b = c
 */
    static
xMultMatrix(a, b, c)
    Gfloat          a[], b[], c[];
{
    c[0] = a[0] * b[0] + a[3] * b[1] + 0 * b[2];
    c[1] = a[1] * b[0] + a[4] * b[1] + 0 * b[2];
    c[2] = a[2] * b[0] + a[5] * b[1] + 1 * b[2];

    c[3] = a[0] * b[3] + a[3] * b[4] + 0 * b[5];
    c[4] = a[1] * b[3] + a[4] * b[4] + 0 * b[5];
    c[5] = a[2] * b[3] + a[5] * b[4] + 1 * b[5];
    return 0;
}


#ifdef TDEBUG
    static int
xDumpMatrix(s, m)
    char           *s;
    Gfloat          m[];
{
    int             i;

    (void) fprintf(stderr, "xtext:%s", s);
    for (i = 0; i < 6; i++)
	(void) fprintf(stderr, "	%10.5f", m[i]);
    (void) fprintf(stderr, "\n");
}
#endif	/* TDEBUG defined */


    static
ck_ranges(cdef, Xmin, Xmax, Ymin, Ymax)
    struct vcharst *cdef;
    Gfloat         *Xmin, *Xmax, *Ymin, *Ymax;
{
    Gint            DoneBoth = 0;

    while (DoneBoth < 2) {
	switch (cdef->vc_type) {
	case 's':
	    if (cdef->vc_x < *Xmin)
		*Xmin = cdef->vc_x;
	    if (cdef->vc_y < *Ymin)
		*Ymin = cdef->vc_y;
	    DoneBoth++;
	    break;
	case 'S':
	    if (cdef->vc_x > *Xmax)
		*Xmax = cdef->vc_x;
	    if (cdef->vc_y > *Ymax)
		*Ymax = cdef->vc_y;
	    DoneBoth++;
	    break;
	case 'e':
	    DoneBoth += 2;
	    break;
	}
	cdef++;
    }
    return 0;
}


    static
ApplyMatrix(m, in, out)
    Gfloat          m[];
    Gpoint         *in, *out;
{
    out->x = m[0] * in->x + m[1] * in->y + m[2] * 1.0;
    out->y = m[3] * in->x + m[4] * in->y + m[5] * 1.0;
    return 0;
}


/*
 * xXgksInqTextExtent () -- X's text inquire extent routine
 *
 */
  xXgksInqTextExtent(ws, tx, ndc_points)
    WS_STATE_PTR    ws;
    TEXT_ST        *tx;
    Gpoint          ndc_points[5];
 {
    Gpoint          start;
    Gchar          *string;
    Gpoint          atsndc;
    FONT           *cfont;
    Gfloat          fsize, fwidth, xtrans, ytrans;

    /* new xFyMin() funct added */
    Gfloat          Xmin, Xmax, Ymin, Ymax;
    Gfloat          XminFirst, XmaxFirst, YminFirst, YmaxFirst;
    struct vcharst *cdef;
    Gint            i;

    (void) XgksSIGIO_OFF(ws->dpy);

    if (FirstTime == TRUE) {			/* First Time initializing */
	for (i = 0; i < DEFINED_FONT_TYPE; i++)
	    FontTable[i].f_loaded = FALSE;
	FirstTime = FALSE;
    }
    xComputeText(ws, tx);

    cfont = FontTable[wsr_font].f_font;

    /*
     * Compute transformation to move the whole string to the alignment point.
     * Transform the NDC start point to scaled NDC.
     */
    atsndc.x = (tx->location->x * ws->ndctoxtrans.xScale) +
	       ws->ndctoxtrans.xTrans;
    atsndc.y = (tx->location->y * ws->ndctoxtrans.yScale) +
	       ws->ndctoxtrans.yTrans;

    stroke_trans[2] = atsndc.x;
    stroke_trans[5] = atsndc.y;

    /*
     * Find total length or height of string + spacing.
     * Find minimum and maximum X & Y.
     */
    Xmin = Ymin = 1500.0;
    Xmax = Ymax = -1500.0;
    for (string = tx->string, fsize = 0.0, fwidth = -9999; *string; string++) {
	if (cfont->fcharoffset[*string] >= 0) {	/* defined ? */
	    cdef = &(cfont->fchars[cfont->fcharoffset[*string]]);
	    (void) ck_ranges(cdef, &Xmin, &Xmax, &Ymin, &Ymax);
	    switch (tx->chattr.path) {
	    case GTP_LEFT:
	    case GTP_RIGHT:
		fsize += xFxMax(cdef);
		fsize += cfont->fnominalx * wsr_spacing;
		break;
	    case GTP_UP:
	    case GTP_DOWN:
		fsize += xFyMax(cdef) - xFyMin(cdef);
		fsize += cfont->fnominaly * wsr_spacing;
		if ((i = xFxMax(cdef)) > fwidth)
		    fwidth = i;
		break;
	    }
	}
    }

    /*
     * translate alignment point to origin which is currently at the LEFT
     * BASE of the first character in the string.
     */
    switch (tx->chattr.path) {
    default:
    case GTP_RIGHT:
	switch (tx->chattr.align.hor) {
	default:
	case GTH_NORMAL:
	case GTH_LEFT:
	    xtrans = 0;
	    break;
	case GTH_CENTRE:
	    xtrans = -fsize / 2.0;
	    break;
	case GTH_RIGHT:
	    xtrans = -fsize;
	    break;
	}
	switch (tx->chattr.align.ver) {
	case GTV_TOP:
	    ytrans = -cfont->ftop;
	    break;
	case GTV_CAP:
	    ytrans = -cfont->fcap;
	    break;
	case GTV_HALF:
	    ytrans = -cfont->fhalf;
	    break;
	default:
	case GTV_NORMAL:
	case GTV_BASE:
	    ytrans = -cfont->fbase;
	    break;
	case GTV_BOTTOM:
	    ytrans = -cfont->fbottom;
	    break;
	}
	break;

    case GTP_LEFT:
	switch (tx->chattr.align.hor) {
	case GTH_LEFT:
	    xtrans = fsize -
		xFxMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]]));
	    break;
	case GTH_CENTRE:
	    xtrans = fsize / 2.0 -
		xFxMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]]));
	    break;
	default:
	case GTH_NORMAL:
	case GTH_RIGHT:
	    xtrans = 
		-xFxMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]]));
	    break;
	}
	switch (tx->chattr.align.ver) {
	case GTV_TOP:
	    ytrans = -cfont->ftop;
	    break;
	case GTV_CAP:
	    ytrans = -cfont->fcap;
	    break;
	case GTV_HALF:
	    ytrans = -cfont->fhalf;
	    break;
	default:
	case GTV_NORMAL:
	case GTV_BASE:
	    ytrans = -cfont->fbase;
	    break;
	case GTV_BOTTOM:
	    ytrans = -cfont->fbottom;
	    break;
	}
	break;
    case GTP_UP:
	switch (tx->chattr.align.hor) {
	case GTH_LEFT:
	    xtrans = 0;
	    break;
	default:
	case GTH_NORMAL:
	case GTH_CENTRE:
	    xtrans = -fwidth / 2.0;
	    break;
	case GTH_RIGHT:
	    xtrans = -fwidth;
	    break;
	}
	switch (tx->chattr.align.ver) {
	case GTV_TOP:
	    ytrans = -fsize;
	    break;
	case GTV_CAP:
	    ytrans = -fsize - cfont->fnominaly * wsr_spacing;
	    break;
	case GTV_HALF:
	    ytrans = -fsize / 2.0;
	    break;
	default:
	case GTV_NORMAL:
	case GTV_BASE:
	    ytrans = 0;
	    break;
	case GTV_BOTTOM:
	    ytrans = -cfont->fbottom;
	    break;
	    /* Changed fsize to -fsize   PTR# c1083  */
	}
	break;

    case GTP_DOWN:
	switch (tx->chattr.align.hor) {
	case GTH_LEFT:
	    xtrans = 0;
	    break;
	default:
	case GTH_NORMAL:
	case GTH_CENTRE:
	    xtrans = -fwidth / 2.0;
	    break;
	case GTH_RIGHT:
	    xtrans = -fwidth;
	    break;
	}
	switch (tx->chattr.align.ver) {
	default:
	case GTV_NORMAL:
	case GTV_TOP:
	    ytrans = -cfont->ftop;
	    break;
	case GTV_CAP:
	    ytrans = -cfont->fcap;
	    break;
	case GTV_HALF:
	    ytrans = fsize / 2.0 -
		xFyMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]]));
	    break;
	case GTV_BASE:
	    ytrans = fsize -
		xFyMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]]));
	    break;
	case GTV_BOTTOM:
	    ytrans = fsize -
		xFyMax(&(cfont->fchars[cfont->fcharoffset[*(tx->string)]])) -
		cfont->fbottom;
	    break;
	}
	break;
    }

    (void) xTransMatrix(stroke_trans, xtrans, ytrans);

#ifdef TDEBUG
    (void) fprintf(stderr, "ibmrt_ddgks:text: xtrans=%f ytrans=%f\n", xtrans,
		   ytrans);
    xDumpMatrix("stroke_trans", stroke_trans);
#endif

    /* find minimum and maximum X & Y of First Character */
    XminFirst = YminFirst = 9999.0;
    XmaxFirst = YmaxFirst = -9999.0;
    cdef = &(cfont->fchars[cfont->fcharoffset[*(tx->string)]]);
    (void) ck_ranges(cdef, &XminFirst, &XmaxFirst, &YminFirst, &YmaxFirst);

#ifdef TDEBUG
    (void) fprintf(stderr,
		   "ibmrt_ddgks:i_text_extent:First min %f %f max %f %f\n",
		   XminFirst, YminFirst, XmaxFirst, YmaxFirst);
#endif

    /*
     * We know all we need to know so send the bounding box through the trans.
     */

    switch (tx->chattr.path) {
    default:
    case GTP_RIGHT:
	ndc_points[1].x = XminFirst;
	ndc_points[1].y = Ymin;
	ndc_points[2].x = fsize;
	ndc_points[2].y = Ymin;
	ndc_points[3].x = fsize;
	ndc_points[3].y = Ymax;
	ndc_points[4].x = XminFirst;
	ndc_points[4].y = Ymax;
	switch (tx->chattr.align.hor) {
	default:
	case GTH_NORMAL:
	case GTH_LEFT:
	    ndc_points[0].x = fsize;
	    break;
	case GTH_CENTRE:
	    ndc_points[0].x = (fsize - XminFirst) / 2;
	    break;

	    /* when align is CENTRE define concat pt as start pt */
	case GTH_RIGHT:
	    ndc_points[0].x = XminFirst;
	    break;
	}
	switch (tx->chattr.align.ver) {
	case GTV_TOP:
	    ndc_points[0].y = cfont->ftop;
	    break;
	case GTV_CAP:
	    ndc_points[0].y = cfont->fcap;
	    break;
	case GTV_HALF:
	    ndc_points[0].y = cfont->fhalf;
	    break;
	default:
	case GTV_NORMAL:
	case GTV_BASE:
	    ndc_points[0].y = cfont->fbase;
	    break;
	case GTV_BOTTOM:
	    ndc_points[0].y = cfont->fbottom;
	    break;
	}
	break;
    case GTP_LEFT:
	ndc_points[1].x = -fsize + XmaxFirst;
	ndc_points[1].y = Ymin;
	ndc_points[2].x = XmaxFirst;
	ndc_points[2].y = Ymin;
	ndc_points[3].x = XmaxFirst;
	ndc_points[3].y = Ymax;
	ndc_points[4].x = -fsize + XmaxFirst;
	ndc_points[4].y = Ymax;
	switch (tx->chattr.align.hor) {
	case GTH_LEFT:
	    ndc_points[0].x = XmaxFirst;
	    break;
	case GTH_CENTRE:
	    ndc_points[0].x = XmaxFirst - fsize / 2;
	    break;

	    /* when align is CENTRE define concat pt as start pt */
	default:
	case GTH_NORMAL:
	case GTH_RIGHT:
	    ndc_points[0].x = -fsize + XmaxFirst;
	    break;
	}
	switch (tx->chattr.align.ver) {
	case GTV_TOP:
	    ndc_points[0].y = cfont->ftop;
	    break;
	case GTV_CAP:
	    ndc_points[0].y = cfont->fcap;
	    break;
	case GTV_HALF:
	    ndc_points[0].y = cfont->fhalf;
	    break;
	default:
	case GTV_NORMAL:
	case GTV_BASE:
	    ndc_points[0].y = cfont->fbase;
	    break;
	case GTV_BOTTOM:
	    ndc_points[0].y = cfont->fbottom;
	    break;
	}
	break;
    case GTP_UP:
	ndc_points[1].x = Xmin;
	ndc_points[1].y = YminFirst;
	ndc_points[2].x = Xmax;
	ndc_points[2].y = YminFirst;
	ndc_points[3].x = Xmax;
	ndc_points[3].y = fsize;
	ndc_points[4].x = Xmin;
	ndc_points[4].y = fsize;
	switch (tx->chattr.align.hor) {
	case GTH_LEFT:
	    ndc_points[0].x = Xmin;
	    break;
	default:
	case GTH_NORMAL:
	case GTH_CENTRE:
	    ndc_points[0].x = (Xmax - Xmin) / 2.0;
	    break;
	case GTH_RIGHT:
	    ndc_points[0].x = Xmax;
	    break;
	}
	switch (tx->chattr.align.ver) {
	case GTV_TOP:
	    ndc_points[0].y = YminFirst;
	    break;
	case GTV_CAP:
	    ndc_points[0].y = YminFirst;
	    break;
	case GTV_HALF:
	    ndc_points[0].y = fsize / 2;
	    break;

	    /* when align is HALF define concat pt as start pt */
	default:
	case GTV_NORMAL:
	case GTV_BASE:
	    ndc_points[0].y = fsize;
	    break;
	case GTV_BOTTOM:
	    ndc_points[0].y = fsize;
	    break;
	}
	break;
    case GTP_DOWN:
	ndc_points[1].x = Xmin;
	ndc_points[1].y = -fsize + YmaxFirst;
	ndc_points[2].x = Xmax;
	ndc_points[2].y = -fsize + YmaxFirst;
	ndc_points[3].x = Xmax;
	ndc_points[3].y = YmaxFirst;
	ndc_points[4].x = Xmin;
	ndc_points[4].y = YmaxFirst;
	switch (tx->chattr.align.hor) {
	case GTH_LEFT:
	    ndc_points[0].x = Xmin;
	    break;
	default:
	case GTH_NORMAL:
	case GTH_CENTRE:
	    ndc_points[0].x = (Xmax - Xmin) / 2.0;
	    break;
	case GTH_RIGHT:
	    ndc_points[0].x = Xmax;
	    break;
	}
	switch (tx->chattr.align.ver) {
	default:
	case GTV_NORMAL:
	case GTV_TOP:
	    ndc_points[0].y = -fsize + YmaxFirst;
	    break;
	case GTV_CAP:
	    ndc_points[0].y = -fsize + YmaxFirst;
	    break;
	case GTV_HALF:
	    ndc_points[0].y = YmaxFirst - fsize / 2;
	    break;

	    /* when align is HALF define concat pt as start pt */
	case GTV_BASE:
	    ndc_points[0].y = YmaxFirst;
	    break;
	case GTV_BOTTOM:
	    ndc_points[0].y = YmaxFirst;
	    break;
	}
	break;
    }

    /*
     * Send the points through the text alignment matrix to get device
     * coordinates and the inverse of the workstation transformation to get
     * NDC coordinates.
     */
    for (i = 0; i < 5; i++) {
	(void) ApplyMatrix(stroke_trans, &(ndc_points[i]), &start);
	ndc_points[i].x = (start.x - ws->ndctoxtrans.xTrans) /
			  ws->ndctoxtrans.xScale;
	ndc_points[i].y = (start.y - ws->ndctoxtrans.yTrans) /
			  ws->ndctoxtrans.yScale;
    }

    (void) XgksSIGIO_ON(ws->dpy);

    return OK;
  }

#define MAX(a, b) (((a)>(b))?(a):(b))


xXgksMesg(ws, mesg)
    WS_STATE_PTR    ws;
    MESG_ST        *mesg;
{

    Display        *loc_dpy;
    Window          loc_win;
    GC              loc_gc;
    Gpoint          dc_pos;
    int             height, width, tempwidth, topheight;
    XPoint          x_pos;
    XRectangle      x_clip;
    XFontStruct    *MFontInfo;

    if (ws->ewstype != X_WIN)
	return OK;

    (void) XgksSIGIO_OFF(ws->dpy);

    loc_dpy = ws->dpy;
    loc_win = ws->win;
    loc_gc = ws->gc;

    dc_pos.x = 150.0;
    dc_pos.y = 200.0;
    DcToX(ws, &dc_pos, &x_pos);

    x_clip.x = 0;
    x_clip.y = 0;
    x_clip.width = ws->wbound.x;
    x_clip.height = ws->wbound.y;

    /* must clear a rectangle for the message */
    MFontInfo = XLoadQueryFont(loc_dpy, "6x10");
    if (MFontInfo == NULL) {
	static int	loaded	= 0;
	if (!loaded) {
	    (void) fputs("xXgksMesg(): Can't load font \"6x10\".", stderr);
	    (void) fputs("  Trying \"fixed\"...\n", stderr);
	    loaded	= 1;
	}
	MFontInfo	= XLoadQueryFont(ws->dpy, "fixed");
    }
    XSetFont(loc_dpy, loc_gc, MFontInfo->fid);
    tempwidth = XTextWidth(MFontInfo, mesg->string, STRLEN(mesg->string));
    width = MAX(tempwidth, ws->last_message_width);
    ws->last_message_width = tempwidth;
    height = MFontInfo->ascent + MFontInfo->descent;
    topheight = MFontInfo->ascent;
    XSetForeground(loc_dpy, loc_gc, XcPixelValue(ws, (Gint) ws->wsbg));
    XFillRectangle(loc_dpy, loc_win, loc_gc, x_pos.x, x_pos.y - topheight,
		   (unsigned) width, (unsigned) height);

    /*
     * XFillRectangle(loc_dpy, loc_win, loc_gc, x_pos.x, x_pos.y - height +
     * MFontInfo->descent, (unsigned)width, (unsigned)height);
     */

    if (ws->wscolour == 2)			/* monochrome ? */
	XSetForeground(loc_dpy, loc_gc,
		       XcPixelValue(ws, (Gint) ws->wsfg));
    else
	XSetForeground(loc_dpy, loc_gc, XcPixelValue(ws, (Gint) 1));

    /* the fill style attribute */
    XSetClipRectangles(loc_dpy, loc_gc, 0, 0, &x_clip, 1, Unsorted);
    XDrawString(loc_dpy, loc_win, loc_gc, x_pos.x, x_pos.y, mesg->string,
		STRLEN(mesg->string));

    (void) XgksSIGIO_ON(ws->dpy);

    return OK;
}
#endif
