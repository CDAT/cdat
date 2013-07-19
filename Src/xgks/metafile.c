/*
 * This file contains the interface to the metafile implementations of XGKS.
 * It is mostly a "switching" interface in that it does little work itself, 
 * but rather calls a specific metafile implementation that does the actual 
 * work.  Currently, two types of metafile implementations are available: 
 * GKSM and CGM.  For historical reasons, the default is GKSM.
 *
 * Copyright (C) 1991 UCAR/Unidata
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of UCAR/Unidata not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  UCAR makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.  It is
 * provided with no support and without obligation on the part of UCAR or
 * Unidata, to assist in its use, correction, modification, or enhancement.
 */

/*LINTLIBRARY*/

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include <time.h>		/* for time(), localtime(), and strftime() */
#include <sys/types.h>		/* for uid_t */
#include <unistd.h>		/* for getuid() & getlogin() */
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>
#include "gks_implem.h"
#include "gksm.h"
#include "cgm.h"
#include "cgm_implem.h"
#include "ps.h"
#include "meta.h"
#include "gif.h"

#ifndef lint
    static char rcsid[]	= "$Id$";
    static char afsid[]	= "$__Header$";
#endif

#define NOTSUPPORTED(type)	(type==6 || type==16)

/*
 * List of active output Metafiles:
 */
static int	num_cgmo;
static int	num_gksmo;
static int      num_cairomo;
static Metafile	active_cgmo[MAX_ACTIVE_WS];
static Metafile	active_gksmo[MAX_ACTIVE_WS];
static Metafile	active_cairomo[MAX_ACTIVE_WS];

typedef int (*INT_PROC)();

/*
 * Keep in old K&R C to avoid painfu, picky function prototyping requirements
 * of MIPS c compiler
 */
static INT_PROC SEL_FUNC(mf, gksm, cgm, cairo, gif)
     Metafile *mf; INT_PROC gksm; INT_PROC cgm; INT_PROC cairo; INT_PROC gif;
{
    switch(mf->any->type){
      case MF_GKSM:
	return gksm;
#ifdef CAIRODRAW
      case MF_CAIRO:
	return cairo;
#endif
      case MF_CGM:
	return cgm;
      default:
	return 0;
    }
}

static INT_PROC emptyProc(void)
{
    return (INT_PROC)0;
}

/*
 * The following are macros that expand to the appropriate type of metafile
 * function (i.e. either GKSM or CGM).
 *
 * This allows the metafile functions to be chosen at the time of metafile 
 * creation
 */

/* #define SEL_FUNC(mf, gksm, cgm)	\ */
/* 	    (*((mf)->any->type == MF_CGM ? (cgm) : (gksm))) */


#define MO_CELL_ARRAY(mf, num, ll, ur, lr, row, colour, dim) \
	    SEL_FUNC(mf, (INT_PROC)GMcellArray, (INT_PROC)CGMcellArray, \
		     (INT_PROC)CAIROcellArray)\
	    (mf, num, ll, ur, lr, row, colour, dim)
#define MO_CLEAR(mf, num, flag)	\
	    SEL_FUNC(mf, (INT_PROC)GMclear, (INT_PROC)CGMclear, (INT_PROC)CAIROclear)(mf, num, flag)
#define MO_CLOSE(mf)	\
	    SEL_FUNC(mf, (INT_PROC)GMmoClose, (INT_PROC)CGMmoClose, (INT_PROC)CAIROmoClose)(mf)
#define MO_CLOSE_SEG(mf, num)	\
	    SEL_FUNC(mf, (INT_PROC)GMcloseSeg, (INT_PROC)CGMcloseSeg, (INT_PROC)CAIROcloseSeg)(mf, num)
#define MO_DEFER(mf, num, defer_mode, regen_mode)	\
	    SEL_FUNC(mf, (INT_PROC)GMdefer, (INT_PROC)CGMdefer, (INT_PROC)CAIROdefer)(mf, num, defer_mode, regen_mode)
#define MI_GET_NEXT_ITEM(mf)	\
	    SEL_FUNC(mf, (INT_PROC)GMnextItem, (INT_PROC)CGMnextItem, emptyProc)(mf)
#define MO_MESSAGE(mf, num, string)	\
	    SEL_FUNC(mf, (INT_PROC)GMmessage, (INT_PROC)CGMmessage, (INT_PROC)CAIROmessage)(mf, num, string)
#define MI_OPEN(mf)	\
	    SEL_FUNC(mf, (INT_PROC)GMmiOpen, (INT_PROC)CGMmiOpen, emptyProc)
#define MO_OPEN(mf)	\
	    SEL_FUNC(mf, (INT_PROC)GMmoOpen, (INT_PROC)CGMmoOpen, (INT_PROC)CAIROmoOpen)(mf)
#define MI_CLOSE(mf)	\
	    SEL_FUNC(mf, GMmiClose, CGMmiClose(mf), emptyProc)(mf)
#define MI_READ_ITEM(mf, record)	\
	    SEL_FUNC(mf, (INT_PROC)GMreadItem, (INT_PROC)CGMreadItem, emptyProc)(mf, record)
#define MO_REDRAW_ALL_SEG(mf, num)	\
	    SEL_FUNC(mf, (INT_PROC)GMredrawAllSeg, (INT_PROC)CGMredrawAllSeg, (INT_PROC)CAIROredrawAllSeg)(mf, num)
#define MO_RENAME_SEG(mf, num, old, new)	\
	    SEL_FUNC(mf, (INT_PROC)GMrenameSeg, (INT_PROC)CGMrenameSeg, (INT_PROC)CAIROrenameSeg)(mf, num, old, new)
#define MO_SET_ASF(mf, num)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetAsf, (INT_PROC)CGMsetAsf, (INT_PROC)CAIROsetAsf)(mf, num)
#define MO_SET_CHAR_UP(mf, num, up, base)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetCharUp, (INT_PROC)CGMsetCharUp, (INT_PROC)CAIROsetCharUp)(mf, num, up, base)
#define MO_SET_CLIPPING(mf, num, rect)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetClip, (INT_PROC)CGMsetClip, (INT_PROC)CAIROsetClip)(mf, num, rect)
#define MO_SET_COLOUR_REP(mf, num, idx, rep)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetColRep, (INT_PROC)CGMsetColRep, (INT_PROC)CAIROsetColRep)(mf, num, idx, rep)
#define MO_SET_FILL_REP(mf, num, idx, rep)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetFillRep, (INT_PROC)CGMsetFillRep, (INT_PROC)CAIROsetFillRep)(mf, num, idx, rep)
#define MO_SET_FILL_STYLE(mf, num, style)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetFillStyle, (INT_PROC)CGMsetFillStyle, (INT_PROC)CAIROsetFillStyle)(mf, num, style)
#define MO_SET_GRAPH_SIZE(mf, num, code, size)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetGraphSize, (INT_PROC)CGMsetGraphSize, (INT_PROC)CAIROsetGraphSize)(mf, num, code, size)
#define MO_SET_GRAPH_ATTR(mf, num, code, attr)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetGraphAttr, (INT_PROC)CGMsetGraphAttr, (INT_PROC)CAIROsetGraphAttr)(mf, num, code, attr)
#define MO_SET_LIMIT(mf, num, code, rect)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetLimit, (INT_PROC)CGMsetLimit, (INT_PROC)CAIROsetLimit)(mf, num, code, rect)
#define MO_SET_LINE_MARKER_REP(mf, num, code, idx, type, size, colour)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetLineMarkRep, (INT_PROC)CGMsetLineMarkRep, (INT_PROC)CAIROsetLineMarkRep)\
		(mf, num, code, idx, type, size, colour)
#define MO_SET_PATTERN_REFPT(mf, num)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetPatRefpt, (INT_PROC)CGMsetPatRefpt, (INT_PROC)CAIROsetPatRefpt)(mf, num)
#define MO_SET_PATTERN_REP(mf, num, idx, rep)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetPatRep, (INT_PROC)CGMsetPatRep, (INT_PROC)CAIROsetPatRep)(mf, num, idx, rep)
#define MO_SET_PATTERN_SIZE(mf, num)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetPatSize, (INT_PROC)CGMsetPatSize, (INT_PROC)CAIROsetPatSize)(mf, num)
#define MO_SET_SEG_ATTR(mf, num, name, code, attr)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetSegAttr, (INT_PROC)CGMsetSegAttr, (INT_PROC)CAIROsetSegAttr)(mf, num, name, code, attr)
#define MO_SET_SEG_DETECT(mf, num, name, det)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetSegDetect, (INT_PROC)CGMsetSegDetect, (INT_PROC)CAIROsetSegDetect)(mf, num, name, det)
#define MO_SET_SEG_HILIGHT(mf, num, name, hilight)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetSegHilight, (INT_PROC)CGMsetSegHilight, (INT_PROC)CAIROsetSegHilight)\
		(mf, num, name, hilight)
#define MO_SET_SEG_PRI(mf, num, name, pri)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetSegPri, (INT_PROC)CGMsetSegPri, (INT_PROC)CAIROsetSegPri)(mf, num, name, pri)
#define MO_SET_SEG_TRANS(mf, num, name, matrix)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetSegTran, (INT_PROC)CGMsetSegTran, (INT_PROC)CAIROsetSegTran)(mf, num, name, matrix)
#define MO_SET_SEG_VIS(mf, num, name, vis)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetSegVis, (INT_PROC)CGMsetSegVis, (INT_PROC)CAIROsetSegVis)(mf, num, name, vis)
#define MO_SET_TEXT_ALIGN(mf, num, align)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetTextAlign, (INT_PROC)CGMsetTextAlign, (INT_PROC)CAIROsetTextAlign)(mf, num, align)
#define MO_SET_TEXT_FP(mf, num, txfp)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetTextFP, (INT_PROC)CGMsetTextFP, (INT_PROC)CAIROsetTextFP)(mf, num, txfp)
#define MO_SET_TEXT_PATH(mf, num, path)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetTextPath, (INT_PROC)CGMsetTextPath, (INT_PROC)CAIROsetTextPath)(mf, num, path)
#define MO_SET_TEXT_REP(mf, num, idx, rep)	\
	    SEL_FUNC(mf, (INT_PROC)GMsetTextRep, (INT_PROC)CGMsetTextRep, (INT_PROC)CAIROsetTextRep)(mf, num, idx, rep)
#define MO_UPDATE(mf, num, regenflag)	\
	    SEL_FUNC(mf, (INT_PROC)GMupdate, (INT_PROC)CGMupdate, (INT_PROC)CAIROupdate)(mf, num, regenflag)
#define MO_GRAPHIC(mf, num, code, num_pt, pos)	\
	    SEL_FUNC(mf, (INT_PROC)GMoutputGraphic, (INT_PROC)CGMoutputGraphic, (INT_PROC)CAIROoutputGraphic)\
		(mf, num, code, num_pt, pos)
#define MO_WRITE_ITEM(mf, num, type, length, data)	\
	    SEL_FUNC(mf, (INT_PROC)GMwriteItem, (INT_PROC)CGMwriteItem, emptyProc)(mf, num, type, length, data)
#define MO_TEXT(mf, num, at, string)	\
	    SEL_FUNC(mf, (INT_PROC)GMtext, (INT_PROC)CGMtext, (INT_PROC)CAIROtext)(mf, num, at, string)


/* #define MO_CELL_ARRAY(mf, num, ll, ur, lr, row, colour, dim) \ */
/* 	    SEL_FUNC(mf, GMcellArray, CGMcellArray) \ */
/* 		(mf, num, ll, ur, lr, row, colour, dim) */
/* #define MO_CLEAR(mf, num, flag)	\ */
/* 	    SEL_FUNC(mf, GMclear, CGMclear)(mf, num, flag) */
/* #define MO_CLOSE(mf)	\ */
/* 	    SEL_FUNC(mf, GMmoClose, CGMmoClose)(mf) */
/* #define MO_CLOSE_SEG(mf, num)	\ */
/* 	    SEL_FUNC(mf, GMcloseSeg, CGMcloseSeg)(mf, num) */
/* #define MO_DEFER(mf, num, defer_mode, regen_mode)	\ */
/* 	    SEL_FUNC(mf, GMdefer, CGMdefer)(mf, num, defer_mode, regen_mode) */
/* #define MI_GET_NEXT_ITEM(mf)	\ */
/* 	    SEL_FUNC(mf, GMnextItem, CGMnextItem)(mf) */
/* #define MO_MESSAGE(mf, num, string)	\ */
/* 	    SEL_FUNC(mf, GMmessage, CGMmessage)(mf, num, string) */
/* #define MI_OPEN(mf)	\ */
/* 	    SEL_FUNC(mf, GMmiOpen, CGMmiOpen)(mf) */
/* #define MI_CLOSE(mf)	\ */
/* 	    SEL_FUNC(mf, GMmiClose, CGMmiClose)(mf) */
/* #define MO_OPEN(mf)	\ */
/* 	    SEL_FUNC(mf, GMmoOpen, CGMmoOpen)(mf) */
/* #define MI_READ_ITEM(mf, record)	\ */
/* 	    SEL_FUNC(mf, GMreadItem, CGMreadItem)(mf, record) */
/* #define MO_REDRAW_ALL_SEG(mf, num)	\ */
/* 	    SEL_FUNC(mf, GMredrawAllSeg, CGMredrawAllSeg)(mf, num) */
/* #define MO_RENAME_SEG(mf, num, old, new)	\ */
/* 	    SEL_FUNC(mf, GMrenameSeg, CGMrenameSeg)(mf, num, old, new) */
/* #define MO_SET_ASF(mf, num)	\ */
/* 	    SEL_FUNC(mf, GMsetAsf, CGMsetAsf)(mf, num) */
/* #define MO_SET_CHAR_UP(mf, num, up, base)	\ */
/* 	    SEL_FUNC(mf, GMsetCharUp, CGMsetCharUp)(mf, num, up, base) */
/* #define MO_SET_CLIPPING(mf, num, rect)	\ */
/* 	    SEL_FUNC(mf, GMsetClip, CGMsetClip)(mf, num, rect) */
/* #define MO_SET_COLOUR_REP(mf, num, idx, rep)	\ */
/* 	    SEL_FUNC(mf, GMsetColRep, CGMsetColRep)(mf, num, idx, rep) */
/* #define MO_SET_FILL_REP(mf, num, idx, rep)	\ */
/* 	    SEL_FUNC(mf, GMsetFillRep, CGMsetFillRep)(mf, num, idx, rep) */
/* #define MO_SET_FILL_STYLE(mf, num, style)	\ */
/* 	    SEL_FUNC(mf, GMsetFillStyle, CGMsetFillStyle)(mf, num, style) */
/* #define MO_SET_GRAPH_SIZE(mf, num, code, size)	\ */
/* 	    SEL_FUNC(mf, GMsetGraphSize, CGMsetGraphSize)(mf, num, code, \ */
/* 							     size) */
/* #define MO_SET_GRAPH_ATTR(mf, num, code, attr)	\ */
/* 	    SEL_FUNC(mf, GMsetGraphAttr, CGMsetGraphAttr)(mf, num, code, \ */
/* 							     attr) */
/* #define MO_SET_LIMIT(mf, num, code, rect)	\ */
/* 	    SEL_FUNC(mf, GMsetLimit, CGMsetLimit)(mf, num, code, rect) */
/* #define MO_SET_LINE_MARKER_REP(mf, num, code, idx, type, size, colour)	\ */
/* 	    SEL_FUNC(mf, GMsetLineMarkRep, CGMsetLineMarkRep)\ */
/* 		(mf, num, code, idx, type, size, colour) */
/* #define MO_SET_PATTERN_REFPT(mf, num)	\ */
/* 	    SEL_FUNC(mf, GMsetPatRefpt, CGMsetPatRefpt)(mf, num) */
/* #define MO_SET_PATTERN_REP(mf, num, idx, rep)	\ */
/* 	    SEL_FUNC(mf, GMsetPatRep, CGMsetPatRep)(mf, num, idx, rep) */
/* #define MO_SET_PATTERN_SIZE(mf, num)	\ */
/* 	    SEL_FUNC(mf, GMsetPatSize, CGMsetPatSize)(mf, num) */
/* #define MO_SET_SEG_ATTR(mf, num, name, code, attr)	\ */
/* 	    SEL_FUNC(mf, GMsetSegAttr, CGMsetSegAttr)(mf, num, name, code, \ */
/* 							 attr) */
/* #define MO_SET_SEG_DETECT(mf, num, name, det)	\ */
/* 	    SEL_FUNC(mf, GMsetSegDetect, CGMsetSegDetect)(mf, num, name, det) */
/* #define MO_SET_SEG_HILIGHT(mf, num, name, hilight)	\ */
/* 	    SEL_FUNC(mf, GMsetSegHilight, CGMsetSegHilight)\ */
/* 		(mf, num, name, hilight) */
/* #define MO_SET_SEG_PRI(mf, num, name, pri)	\ */
/* 	    SEL_FUNC(mf, GMsetSegPri, CGMsetSegPri)(mf, num, name, pri) */
/* #define MO_SET_SEG_TRANS(mf, num, name, matrix)	\ */
/* 	    SEL_FUNC(mf, GMsetSegTran, CGMsetSegTran)(mf, num, name, matrix) */
/* #define MO_SET_SEG_VIS(mf, num, name, vis)	\ */
/* 	    SEL_FUNC(mf, GMsetSegVis, CGMsetSegVis)(mf, num, name, vis) */
/* #define MO_SET_TEXT_ALIGN(mf, num, align)	\ */
/* 	    SEL_FUNC(mf, GMsetTextAlign, CGMsetTextAlign)(mf, num, align) */
/* #define MO_SET_TEXT_FP(mf, num, txfp)	\ */
/* 	    SEL_FUNC(mf, GMsetTextFP, CGMsetTextFP)(mf, num, txfp) */
/* #define MO_SET_TEXT_PATH(mf, num, path)	\ */
/* 	    SEL_FUNC(mf, GMsetTextPath, CGMsetTextPath)(mf, num, path) */
/* #define MO_SET_TEXT_REP(mf, num, idx, rep)	\ */
/* 	    SEL_FUNC(mf, GMsetTextRep, CGMsetTextRep)(mf, num, idx, rep) */
/* #define MO_UPDATE(mf, num, regenflag)	\ */
/* 	    SEL_FUNC(mf, GMupdate, CGMupdate)(mf, num, regenflag) */
/* #define MO_GRAPHIC(mf, num, code, num_pt, pos)	\ */
/* 	    SEL_FUNC(mf, GMoutputGraphic, CGMoutputGraphic)\ */
/* 		(mf, num, code, num_pt, pos) */
/* #define MO_WRITE_ITEM(mf, num, type, length, data)	\ */
/* 	    SEL_FUNC(mf, GMwriteItem, CGMwriteItem)(mf, num, type, length, \ */
/* 						       data) */
/* #define MO_TEXT(mf, num, at, string)	\ */
/* 	    SEL_FUNC(mf, GMtext, CGMtext)(mf, num, at, string) */


/*
 * Execute the data contained in a Metafile item.
 */
    static Gint
XgksExecData(type, record)
    Gint            type;
    char           *record;
{
    XGKSMONE       *ptr1;
    XGKSMTWO       *ptr2;
    XGKSMMESG      *msg;
    XGKSMGRAPH     *graph;
    XGKSMTEXT      *text;
    XGKSMSIZE      *size;
    XGKSMCHARVEC   *vec;
    XGKSMASF       *asf;
    XGKSMLMREP     *lmrep;
    XGKSMTEXTREP   *txrep;
    XGKSMFILLREP   *flrep;
    XGKSMPATREP    *patrep;
    XGKSMCOLOURREP *corep;
    XGKSMLIMIT     *limit;
    XGKSMSEGTRAN   *tran;
    XGKSMSEGPRI    *pri;
    XGKSMCELLARRAY *cell;
    XGKSMPATREF    *patref;
    XGKSMPATSIZ    *patsiz;
    OUT_PRIMI      *primi;
    Gtxpath         path;
    Gflinter        inter;
    Gdefmode        defmode;
    Gtxfp           txfp;
    Gsegattr        segattr;
    Gtxalign        txalign;
    Glnbundl        lnrep;
    Gmkbundl        mkrep;
    Gtxbundl        textrep;
    Gflbundl        fillrep;
    Gptbundl        ptrep;
    Gcobundl        colourrep;
    Gasfs           asfs;
    Gint            cnt, i, j;
    Gpoint         *pts;
    Gpoint          siz;
    Gfloat          height;

    switch (type) {

    case 0:
	break;

    case 2:
	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {
		/* c1138:  don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gredrawsegws(xgks_state.activews[cnt].ws_id);
		}
	    }
	}
	break;
    case 82:
	/*
	 * only need to call gcloseseg() once, not for each workstation
	 */
	(void) gcloseseg();
	break;

    case 1:
	ptr1 = (XGKSMONE *) record;

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {
		(void) gclearws(xgks_state.activews[cnt].ws_id, 
				(ptr1->flag == 0 ? GCONDITIONALLY : GALWAYS));
	    }
	}
	break;
    case 3:
	ptr1 = (XGKSMONE *) record;

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {
		/* c1138:  don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gupdatews(xgks_state.activews[cnt].ws_id,
				     (ptr1->flag == 0 ? GPERFORM : GPOSTPONE));
		}
	    }
	}
	break;
    case 21:
	ptr1 = (XGKSMONE *) record;
	(void) gsetlineind(ptr1->flag);
	break;
    case 22:
	ptr1 = (XGKSMONE *) record;
	(void) gsetlinetype(ptr1->flag);
	break;
    case 24:
	ptr1 = (XGKSMONE *) record;
	(void) gsetlinecolourind(ptr1->flag);
	break;
    case 25:
	ptr1 = (XGKSMONE *) record;
	(void) gsetmarkerind(ptr1->flag);
	break;
    case 26:
	ptr1 = (XGKSMONE *) record;
	(void) gsetmarkertype(ptr1->flag);
	break;
    case 28:
	ptr1 = (XGKSMONE *) record;
	(void) gsetmarkercolourind(ptr1->flag);
	break;
    case 29:
	ptr1 = (XGKSMONE *) record;
	(void) gsettextind(ptr1->flag);
	break;
    case 33:
	ptr1 = (XGKSMONE *) record;
	(void) gsettextcolourind(ptr1->flag);
	break;
    case 35:
	ptr1 = (XGKSMONE *) record;
	if (ptr1->flag == 0)
	    path = GTP_RIGHT;
	else if (ptr1->flag == 1)
	    path = GTP_LEFT;
	else if (ptr1->flag == 2)
	    path = GTP_UP;
	else
	    path = GTP_DOWN;
	(void) gsettextpath(path);
	break;
    case 37:
	ptr1 = (XGKSMONE *) record;
	(void) gsetfillind(ptr1->flag);
	break;
    case 38:
	ptr1 = (XGKSMONE *) record;
	if (ptr1->flag == 0)
	    inter = GHOLLOW;
	else if (ptr1->flag == 1)
	    inter = GSOLID;
	else if (ptr1->flag == 2)
	    inter = GPATTERN;
	else
	    inter = GHATCH;
	(void) gsetfillintstyle(inter);
	break;
    case 39:
	ptr1 = (XGKSMONE *) record;
	(void) gsetfillstyleind(ptr1->flag);
	break;
    case 40:
	ptr1 = (XGKSMONE *) record;
	(void) gsetfillcolourind(ptr1->flag);
	break;
    case 41:
	patsiz = (XGKSMPATSIZ *) record;
	siz.x = patsiz->wid.x;
	siz.y = patsiz->hgt.y;
	(void) gsetpatsize(&siz);
	break;
    case 42:
	patref = (XGKSMPATREF *) record;
	(void) gsetpatrefpt(&patref->ref);
	break;
    case 44:
	ptr1 = (XGKSMONE *) record;
	(void) gsetpickid(ptr1->flag);
	break;
    case 81:
	ptr1 = (XGKSMONE *) record;
	(void) gcreateseg(ptr1->flag);
	break;
    case 84:
	ptr1 = (XGKSMONE *) record;
	(void) gdelseg(ptr1->flag);
	break;

    case 4:
	ptr2 = (XGKSMTWO *) record;
	if (ptr2->item1 == 0)
	    defmode = GASAP;
	else if (ptr2->item1 == 1)
	    defmode = GBNIG;
	else if (ptr2->item1 == 2)
	    defmode = GBNIL;
	else
	    defmode = GASTI;

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {
		/* c1138:  don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gsetdeferst(xgks_state.activews[cnt].ws_id,
				       defmode,
				       (ptr2->item2 == 0 
					   ? GALLOWED 
					   : GSUPPRESSED));
		}
	    }
	}
	break;
    case 30:
	ptr2 = (XGKSMTWO *) record;
	txfp.font = ptr2->item1;
	if (ptr2->item2 == 0)
	    txfp.prec = GSTRING;
	else if (ptr2->item2 == 1)
	    txfp.prec = GCHAR;
	else
	    txfp.prec = GSTROKE;
	(void) gsettextfontprec(&txfp);
	break;
    case 36:
	ptr2 = (XGKSMTWO *) record;
	if (ptr2->item1 == 0)
	    txalign.hor = GTH_NORMAL;
	else if (ptr2->item1 == 1)
	    txalign.hor = GTH_LEFT;
	else if (ptr2->item1 == 2)
	    txalign.hor = GTH_CENTRE;
	else
	    txalign.hor = GTH_RIGHT;
	if (ptr2->item2 == 0)
	    txalign.ver = GTV_NORMAL;
	else if (ptr2->item2 == 1)
	    txalign.ver = GTV_TOP;
	else if (ptr2->item2 == 2)
	    txalign.ver = GTV_CAP;
	else if (ptr2->item2 == 3)
	    txalign.ver = GTV_HALF;
	else if (ptr2->item2 == 4)
	    txalign.ver = GTV_BASE;
	else
	    txalign.ver = GTV_BOTTOM;
	(void) gsettextalign(&txalign);
	break;
    case 83:
	ptr2 = (XGKSMTWO *) record;
	(void) grenameseg(ptr2->item1, ptr2->item2);
	break;
    case 92:
	ptr2 = (XGKSMTWO *) record;
	segattr.seg = ptr2->item1;
	(void) ginqsegattr(&segattr);
	segattr.vis = (ptr2->item2 == 0 ? GVISIBLE : GINVISIBLE);
	(void) gsetsegattr(ptr2->item1, &segattr);
	break;
    case 93:
	ptr2 = (XGKSMTWO *) record;
	segattr.seg = ptr2->item1;
	(void) ginqsegattr(&segattr);
	segattr.hilight = (ptr2->item2 == 0 ? GNORMAL : GHIGHLIGHTED);
	(void) gsetsegattr(ptr2->item1, &segattr);
	break;
    case 95:
	ptr2 = (XGKSMTWO *) record;
	segattr.seg = ptr2->item1;
	(void) ginqsegattr(&segattr);
	segattr.det = (ptr2->item2 == 0 ? GUNDETECTABLE : GDETECTABLE);
	(void) gsetsegattr(ptr2->item1, &segattr);
	break;

    case 5:
	msg = (XGKSMMESG *) record;

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {
		/* c1138:  don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gmessage(xgks_state.activews[cnt].ws_id,
				    msg->string);
		}
	    }
	}
	break;

    case 11:
	graph = (XGKSMGRAPH *) record;
	GKSERROR(((pts = (Gpoint*)malloc((size_t) (graph->num_pts*
			sizeof(Gpoint)))) == NULL),
		 300,
		 errXgksExecData);
	for (i = 0; i < graph->num_pts; i++)
	    NdcToWc(&(graph->pts[i]), &(pts[i]));
	(void) gpolyline(graph->num_pts, pts);
	ufree((voidp)pts);
	break;
    case 12:
	graph = (XGKSMGRAPH *) record;
	GKSERROR(((pts = (Gpoint *) malloc((size_t) (graph->num_pts *
			sizeof(Gpoint)))) == NULL),
		 300,
		 errXgksExecData);
	for (i = 0; i < graph->num_pts; i++)
	    NdcToWc(&(graph->pts[i]), &(pts[i]));
	(void) gpolymarker(graph->num_pts, pts);
	ufree((voidp)pts);
	break;
    case 14:
	graph = (XGKSMGRAPH *) record;
	GKSERROR(((pts = (Gpoint *) malloc((size_t) (graph->num_pts *
			sizeof(Gpoint)))) == NULL),
		 300,
		 errXgksExecData);
	for (i = 0; i < graph->num_pts; i++)
	    NdcToWc(&(graph->pts[i]), &(pts[i]));
	(void) gfillarea(graph->num_pts, pts);
	ufree((voidp)pts);
	break;

    case 13:
	text = (XGKSMTEXT *) record;
	GKSERROR(((pts = (Gpoint *) malloc(sizeof(Gpoint))) == NULL),
		 300, errXgksExecData);
	NdcToWc(&(text->location), pts);
	(void) gtext(pts, text->string);
	ufree((voidp)pts);
	break;

    case 15:
	cell = (XGKSMCELLARRAY *) record;
	GKSERROR(((primi = XgksNewPrimi()) == NULL), 300, errXgksExecData);
	primi->pid = CELL_ARRAY;
	primi->primi.cell_array.dim = cell->dim;
	/* rowsize is equal to cell->dim.x */
	primi->primi.cell_array.rowsize = cell->dim.x;
	j = cell->dim.x * cell->dim.y;
	GKSERROR(((primi->primi.cell_array.colour = (Gint *) 
			malloc((size_t) (j * sizeof(Gint)))) == NULL),
		 300,
		 errXgksExecData);
	primi->primi.cell_array.ll = cell->ll;
	primi->primi.cell_array.ur = cell->ur;
	primi->primi.cell_array.lr = cell->lr;
	primi->primi.cell_array.ul.x = cell->ll.x + (cell->ur.x - cell->lr.x);
	primi->primi.cell_array.ul.y = cell->ll.y + (cell->ur.y - cell->lr.y);
	for (i = 0; i < j; i++)
	    primi->primi.cell_array.colour[i] = cell->colour[i];
	XgksProcessPrimi(primi);
	if (MO_OPENED == TRUE)
	    XgksMoCellArray(&(primi->primi.cell_array.ll),
			    &(primi->primi.cell_array.ur),
			    &(primi->primi.cell_array.lr),
			    primi->primi.cell_array.rowsize,
			    primi->primi.cell_array.colour,
			    &(primi->primi.cell_array.dim));
	ufree((voidp)primi->primi.cell_array.colour);
	ufree((voidp)primi);
	break;

    case 23:
	size = (XGKSMSIZE *) record;
	(void) gsetlinewidth(size->size);
	break;
    case 27:
	size = (XGKSMSIZE *) record;
	(void) gsetmarkersize(size->size);
	break;
    case 31:
	size = (XGKSMSIZE *) record;
	(void) gsetcharexpan(size->size);
	break;
    case 32:
	size = (XGKSMSIZE *) record;
	(void) gsetcharspace(size->size);
	break;

    case 34:
	vec = (XGKSMCHARVEC *) record;
	VecNdcToWc(&(vec->up), &siz);
	height = sqrt((siz.x * siz.x) + (siz.y * siz.y));
	xgks_state.gks_chattr.up.x = siz.x / height;
	xgks_state.gks_chattr.up.y = siz.y / height;
	xgks_state.gks_chattr.height = height;
	VecNdcToWc(&(vec->base), &siz);
	height = sqrt((siz.x * siz.x) + (siz.y * siz.y));
	xgks_state.gks_chattr.base.x = siz.x / height;
	xgks_state.gks_chattr.base.y = siz.y / height;
	xgks_state.gks_chattr.chwidth = height;
	break;

    case 43:
	asf = (XGKSMASF *) record;
	asfs.ln_type = (asf->asf[0] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.ln_width = (asf->asf[1] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.ln_colour = (asf->asf[2] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.mk_type = (asf->asf[3] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.mk_size = (asf->asf[4] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.mk_colour = (asf->asf[5] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.tx_fp = (asf->asf[6] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.tx_exp = (asf->asf[7] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.tx_space = (asf->asf[8] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.tx_colour = (asf->asf[9] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.fl_inter = (asf->asf[10] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.fl_style = (asf->asf[11] == 0 ? GBUNDLED : GINDIVIDUAL);
	asfs.fl_colour = (asf->asf[12] == 0 ? GBUNDLED : GINDIVIDUAL);
	(void) gsetasf(&asfs);
	break;

    case 51:
	lmrep = (XGKSMLMREP *) record;
	lnrep.type = lmrep->style;
	lnrep.width = lmrep->size;
	lnrep.colour = lmrep->colour;

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {
		/* c1138:  don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gsetlinerep(xgks_state.activews[cnt].ws_id,
				       lmrep->idx, &lnrep);
		}
	    }
	}
	break;
    case 52:
	lmrep = (XGKSMLMREP *) record;
	mkrep.type = lmrep->style;
	mkrep.size = lmrep->size;
	mkrep.colour = lmrep->colour;

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {
		/* c1138:  don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gsetmarkerrep(xgks_state.activews[cnt].ws_id,
					 lmrep->idx, &mkrep);
		}
	    }
	}
	break;

    case 53:
	txrep = (XGKSMTEXTREP *) record;
	textrep.fp.font = txrep->font;
	textrep.ch_exp = txrep->tx_exp;
	textrep.space = txrep->space;
	textrep.colour = txrep->colour;
	if (txrep->prec == 0)
	    textrep.fp.prec = GSTRING;
	else if (txrep->prec == 1)
	    textrep.fp.prec = GCHAR;
	else
	    textrep.fp.prec = GSTROKE;

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {
		/* c1138:  don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gsettextrep(xgks_state.activews[cnt].ws_id,
				       txrep->idx, &textrep);
		}
	    }
	}
	break;

    case 54:
	flrep = (XGKSMFILLREP *) record;
	fillrep.style = flrep->style;
	fillrep.colour = flrep->colour;
	if (flrep->intstyle == 0)
	    fillrep.inter = GHOLLOW;
	else if (flrep->intstyle == 1)
	    fillrep.inter = GSOLID;
	else if (flrep->intstyle == 2)
	    fillrep.inter = GPATTERN;
	else
	    fillrep.inter = GHATCH;

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {
		/* c1138:  don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gsetfillrep(xgks_state.activews[cnt].ws_id,
				       flrep->idx, &fillrep);
		}
	    }
	}
	break;

    case 55:
	patrep = (XGKSMPATREP *) record;
	ptrep.size.x = patrep->size.x;
	ptrep.size.y = patrep->size.y;
	j = ptrep.size.x * ptrep.size.y;
	GKSERROR(((ptrep.array = (Gint *) malloc((size_t) (j *
			sizeof(Gint)))) == NULL),
		 300,
		 errXgksExecData);
	for (i = 0; i < j; i++)
	    ptrep.array[i] = patrep->array[i];

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {

		/* don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gsetpatrep(xgks_state.activews[cnt].ws_id,
				      patrep->idx, &ptrep);
		}
	    }
	}
	break;

    case 56:
	corep = (XGKSMCOLOURREP *) record;
	colourrep.red = corep->red;
	colourrep.green = corep->green;
	colourrep.blue = corep->blue;

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {

		/* don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gsetcolourrep(xgks_state.activews[cnt].ws_id,
					 corep->idx, &colourrep);
		}
	    }
	}
	break;

    case 61:
	limit = (XGKSMLIMIT *) record;
	xgks_state.cliprec.rec = limit->rect;
	XgksProcessClip(&xgks_state.cliprec.rec);
	break;

    case 71:
	limit = (XGKSMLIMIT *) record;

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {

		/* don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gsetwswindow(xgks_state.activews[cnt].ws_id,
					&(limit->rect));
		}
	    }
	}
	break;
    case 72:
	limit = (XGKSMLIMIT *) record;

	/* Do it on all active ws */
	for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++) {
	    if (xgks_state.activews[cnt].ws_id != INVALID) {
		/* c1138:  don't do this on WISS */
		if (xgks_state.activews[cnt].ws->ewstype != WISS) {
		    (void) gsetwsviewport(xgks_state.activews[cnt].ws_id,
					  &(limit->rect));
		}
	    }
	}
	break;

    case 91:
	tran = (XGKSMSEGTRAN *) record;
	segattr.seg = tran->name;
	(void) ginqsegattr(&segattr);
	segattr.segtran[0][0] = tran->matrix[0][0];
	segattr.segtran[0][1] = tran->matrix[0][1];
	segattr.segtran[0][2] = tran->matrix[0][2];
	segattr.segtran[1][0] = tran->matrix[1][0];
	segattr.segtran[1][1] = tran->matrix[1][1];
	segattr.segtran[1][2] = tran->matrix[1][2];
	(void) gsetsegattr(tran->name, &segattr);
	break;

    case 94:
	pri = (XGKSMSEGPRI *) record;
	segattr.seg = pri->name;
	(void) ginqsegattr(&segattr);
	segattr.pri = pri->pri;
	(void) gsetsegattr(pri->name, &segattr);
	break;

    default:
	return 1;
    }
    return 0;
}


/*
 * Indicate whether or not a Metafile item of the given type is valid.
 */
    static int
XgksValidGksMItem(type)
    Gint            type;
{
    if (type >= 0 && type <= 6)
	return OK;
    if (type >= 11 && type <= 16)
	return OK;
    if (type >= 21 && type <= 44)
	return OK;
    if (type >= 51 && type <= 56)
	return OK;
    if (type == 61)
	return OK;
    if (type == 71 || type == 72)
	return OK;
    if (type >= 81 && type <= 84)
	return OK;
    if (type >= 91 && type <= 95)
	return OK;
    if (type > 100)
	return OK;

    return INVALID;
}


/*
 * WRITE ITEM TO GKSM
 */
    int
gwritegksm(ws_id, type, length, data)
    Gint            ws_id;	/* workstation identifier */
    Gint	    type;	/* item type */
    Gint	    length;	/* item length */
    Gchar          *data;	/* item data-record */
{
    WS_STATE_PTR    ws;
    Metafile       *mf;

    GKSERROR((xgks_state.gks_state != GWSAC && xgks_state.gks_state != GSGOP), 
	     5, errgwritegksm);

    GKSERROR((!VALID_WSID(ws_id)), 20, errgwritegksm);

    /* if it isn't open, it can't be active... */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 30, errgwritegksm);

    GKSERROR((ws->wsstate != GACTIVE), 30, errgwritegksm);

    GKSERROR((WS_CAT(ws) != GMO), 32, errgwritegksm);

    GKSERROR((type <= 100), 160, errgwritegksm);

    GKSERROR((length < 0), 161, errgwritegksm);

    mf	= &ws->mf;

    return MO_WRITE_ITEM(mf, 1, type, length, data);
}


/*
 * GET ITEM TYPE FROM GKSM
 */
    int
ggetgksm(ws_id, result)
    Gint            ws_id;	/* workstation identifier */
    Ggksmit        *result;	/* input metafile item information */
{
    WS_STATE_PTR    ws;

    GKSERROR((xgks_state.gks_state != GWSOP && xgks_state.gks_state != 
		GWSAC && xgks_state.gks_state != GSGOP),
	     7,
	     errggetgksm);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errggetgksm);

    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errggetgksm);

    GKSERROR((WS_CAT(ws) != GMI), 34, errggetgksm);

    GKSERROR((ws->mf.any->GksmEmpty == TRUE), 162, errggetgksm);

    if (XgksValidGksMItem(ws->mf.any->CurItem.type) == INVALID)
	ws->mf.any->filestat = MF_ITEM_ERR;
    GKSERROR((ws->mf.any->filestat != METAFILE_OK), 163, errggetgksm);

    *result	= ws->mf.any->CurItem;

    return OK;
}


/*
 * READ ITEM FROM GKSM
 *
 * The filestat field has been added to the workstation state structure to
 * retain MI error information between calls to ggetgksm and greadgksm.  The
 * field has one of four possible integer values (defined in metafile.h):
 *    CAIROFILE_OK -- no errors so far reading from metafile
 *    MF_DATA_ERR -- type and length of latest item read (current item) are
 *                   ok, but XgksReadData couldn't read the data (eg. non-
 *                   numeric characters found when trying to read an integer)
 *                   The item may be skipped (via greadgksm w/ length 0) and
 *                   MI processing can continue.
 *    MF_ITEM_ERR -- something more serious than a data error found in latest
 *                   item; eg. type invalid, length invalid, data read ter-
 *                   minated prematurely.  This error condition can be detected
 *                   while going on to the next item, so the current item is
 *                   returned correctly, but subsequent attempts to get/read
 *                   will fail.  Since the exact cause of the error is unknown,
 *                   this is not a recoverable condition.
 *    MF_FILE_ERR -- the system reported an I/O error during a read attempt.
 *                   This error is not recoverable.
 * The first function to detect the error will report it, while attempting to
 * process the item it applies to.  In other words, if greadgksm encounters a
 * file error while trying to go on to the next item after successfully reading
 * the current item, the error will not be reported until the next get/read
 * call.  After a fatal error has been reported (via GKS error 163, item is
 * invalid), subsequent get/read attempts will return error 162, no items left
 * in MI, since the error is unrecoverable and no more reading is allowed.
 */
    int
greadgksm(ws_id, length, record)
    Gint            ws_id;	/* workstation identifier */
    Gint	    length;	/* maximum item data record length */
    char           *record;	/* input metafile item data-record */
{
    int             istat;
    WS_STATE_PTR   ws;

    GKSERROR((xgks_state.gks_state != GWSOP && xgks_state.gks_state !=
		GWSAC && xgks_state.gks_state != GSGOP),
	     7,
	     errgreadgksm);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgreadgksm);

    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgreadgksm);

    GKSERROR((WS_CAT(ws) != GMI), 34, errgreadgksm);

    if (ws->mf.any->CurItem.type == 0)
	ws->mf.any->GksmEmpty = TRUE;

    GKSERROR((ws->mf.any->GksmEmpty == TRUE), 162, errgreadgksm);

    if (ws->mf.any->filestat == MF_FILE_ERR) {
	ws->mf.any->GksmEmpty = TRUE;
	(void) gerrorhand(162, errgreadgksm, xgks_state.gks_err_file);
	return 162;
    }
    if (XgksValidGksMItem(ws->mf.any->CurItem.type) == INVALID)
	ws->mf.any->filestat = MF_ITEM_ERR;

    GKSERROR(((ws->mf.any->filestat == MF_ITEM_ERR) && (length != 0)),
	     MF_ITEM_ERR, errgreadgksm);

    GKSERROR(((ws->mf.any->filestat == MF_DATA_ERR) && (length != 0)),
	     MF_DATA_ERR, errgreadgksm);

    GKSERROR((length < 0), 166, errgreadgksm);

    if ((istat = MI_READ_ITEM(&ws->mf, record)) != 0)
	return istat;

    ws->mf.any->filestat	= MI_GET_NEXT_ITEM(&ws->mf);

    return OK;
}


/*
 * Return the minimum possible size for the data associated with a GKSM 
 * item-type.
 *
 * Note, since this routine is used by ginterpret(), we return the size of the 
 * decoded, binary structures -- not the file-encoded size.
 */
    static int
recSize(type)
    Gint            type;
{
    switch (type) {

    case 0:
    case 2:
    case 82:
	return 0;

    case 1:
    case 3:
    case 21:
    case 22:
    case 24:
    case 25:
    case 26:
    case 28:
    case 29:
    case 33:
    case 35:
    case 37:
    case 38:
    case 39:
    case 40:
    case 44:
    case 81:
    case 84:
	return sizeof(XGKSMONE);

    case 4:
    case 30:
    case 36:
    case 83:
    case 92:
    case 93:
    case 95:
	return sizeof(XGKSMTWO);

    case 5:
	return sizeof(XGKSMONE);

    case 11:
    case 12:
    case 14:
	return sizeof(XGKSMONE);

    case 13:
	return sizeof(XGKSMTEXT);

    case 15:
	return sizeof(XGKSMCELLARRAY);

    case 23:
    case 27:
    case 31:
    case 32:
	return sizeof(XGKSMSIZE);

    case 34:
	return sizeof(XGKSMCHARVEC);

    case 43:
	return sizeof(XGKSMASF);

    case 41:
	return sizeof(XGKSMPATSIZ);

    case 42:
	return sizeof(XGKSMPATREF);

    case 51:
    case 52:
	return sizeof(XGKSMLMREP);

    case 53:
	return sizeof(XGKSMTEXTREP);

    case 54:
	return sizeof(XGKSMFILLREP);

    case 55:
	return sizeof(XGKSMPATREP);

    case 56:
	return sizeof(XGKSMCOLOURREP);

    case 61:
    case 71:
    case 72:
	return sizeof(XGKSMLIMIT);

    case 91:
	return sizeof(XGKSMSEGTRAN);

    case 94:
	return sizeof(XGKSMSEGPRI);

    default:
	return INVALID;
    }
}


/*
 * INTERPRET GKSM ITEM
 */
    int
ginterpret(recInfo, data)
    Ggksmit        *recInfo;	/* item type and length */
    char           *data;	/* item data-record */
{
    GKSERROR((xgks_state.gks_state != GWSOP && xgks_state.gks_state !=
		GWSAC && xgks_state.gks_state != GSGOP),
	     7,
	     errginterpret);

    GKSERROR((recInfo == NULL), 163, errginterpret);

    GKSERROR(((recInfo->length > 0) && (data == NULL)), 165, errginterpret);

    /*
     * We no longer check for invalid size (error 161) because the GKSM
     * backend returns the length of the formatted data record rather than the
     * size of the internally-used binary structures.
     */
#if 0
    GKSERROR((recInfo->length < recSize(recInfo->type)),
	     161, errginterpret);
#endif

    GKSERROR((XgksValidGksMItem(recInfo->type) == INVALID), 164,
	     errginterpret);

    /*
     * Can't check for 165 in ginterpret due to file format.
     * Can't really check for 163, either.
     */

    GKSERROR((recInfo->type > 100), 167, errginterpret);

    GKSERROR((NOTSUPPORTED(recInfo->type)), 168, errginterpret);

    GKSERROR((XgksExecData(recInfo->type, data) != 0), 164, errginterpret);

    return OK;
}


/*
 * Open an input Metafile: scan header and get first item.
 */
    int
XgksMiOpenWs(ws)
    WS_STATE_PTR    ws;
{
    int		status	= 26;		/* return status = workstation cannot
					 * be opened */

    ws->wscolour	= 256;
    ws->set_colour_rep	= NULL;

    if ((strstr(ws->conn, ".cgm") == NULL
	    ? GMmiOpen(&ws->mf, ws->conn)
	    : CGMmiOpen(&ws->mf, ws->conn))
	== OK) {

	ws->mf.any->filestat		= MI_GET_NEXT_ITEM(&ws->mf);

	if (ws->mf.any->filestat == METAFILE_OK) {
	    status	= OK;
	} else {
	    MI_CLOSE(&ws->mf);
	}
    }

    return status;
}


/*
 * Open an output Metafile.
 */
    int
XgksMoOpenWs(ws)
    WS_STATE_PTR    ws;
{
  Gint status;

    ws->wscolour	= 256;
    ws->set_colour_rep	= NULL;

    /* Make sure window boundaries are set to zero to avoid */
    /* floating point exceptions on DEC OSF *js* 9.97 */
    ws->wbound.x = 0;
    ws->wbound.y = 0;
      
    if (strstr(ws->conn, ".cgm") != NULL || strstr(ws->conn, ".CGM") != NULL) {
      status = CGMmoOpen(ws);
    } else if (ws->ws_id==3) {
#ifdef CAIRODRAW
      status = CAIROmoOpen(ws);
#endif
    } else {
      status = GMmoOpen(ws);
    }
    return status == OK ? OK :26;
}


/*
 * Close an input Metafile.
 */
    int
XgksMiCloseWs(ws)
    WS_STATE_PTR    ws;
{
    return MI_CLOSE(&ws->mf);
}


/*
 * Close an output Metafile.
 */
    int
XgksMoCloseWs(ws)
    WS_STATE_PTR    ws;
{
    return MO_CLOSE(&ws->mf);
}


/*
 * Set the clear flag in an output Metafile.
 */
    int
XgksMoClearWs(ws, flag)
    WS_STATE_PTR    ws;
    Gclrflag        flag;
{
    Metafile       *mf	= &ws->mf;

    return MO_CLEAR(mf, 1, flag);
}


/*
 * Redraw all segments in an output Metafile.
 */
    int
XgksMoReDrawAllSeg(ws)
    WS_STATE_PTR    ws;
{
    Metafile       *mf	= &ws->mf;

    return MO_REDRAW_ALL_SEG(mf, 1);
}


/*
 * Set the update flag in an output Metafile.
 */
    int
XgksMoUpdateWs(ws, regenflag)
    WS_STATE_PTR    ws;
    Gregen          regenflag;
{
    Metafile       *mf	= &ws->mf;

    return MO_UPDATE(mf, 1, regenflag);
}


/*
 * Set the deferal state in an output Metafile.
 */
    int
XgksMoDeferWs(ws, defer_mode, regen_mode)
    WS_STATE_PTR    ws;
    Gdefmode        defer_mode;
    Girgmode        regen_mode;
{
    Metafile       *mf	= &ws->mf;

    return MO_DEFER(mf, 1, defer_mode, regen_mode);
}


/*
 * Write a message to an output Metafile.
 */
    int
XgksMoMessage(ws, string)
    WS_STATE_PTR    ws;
    Gchar          *string;
{
    Metafile       *mf	= &ws->mf;

    return MO_MESSAGE(mf, 1, string);
}


/*
 * Write a graphic to an output Metafile.
 */
    int
XgksMoGraphicOutputToWs(ws, code, num_pt, pos)
    WS_STATE_PTR    ws;
    Gint            code;
    Gint            num_pt;
    Gpoint         *pos;
{
    Metafile       *mf	= &ws->mf;
#ifdef CAIRODRAW
    extern cairo_t *metafile_cr;
    metafile_cr = ws->cr;
#endif
    return MO_GRAPHIC(mf, 1, code, num_pt, pos);
}


/*
 * Write a graphic to all, appropriate, output Metafiles.
 */
    int
XgksMoGraphicOutput(code, num_pt, pos)
    Gint            code;
    Gint            num_pt;
    Gpoint         *pos;
{
    if (num_gksmo > 0)
	GMoutputGraphic(active_gksmo, num_gksmo, code, num_pt, pos);
    if (num_cgmo > 0)
	CGMoutputGraphic(active_cgmo, num_cgmo, code, num_pt, pos);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
      CAIROoutputGraphic(active_cairomo, num_cairomo, code, num_pt, pos);
#endif

    return OK;
}


/*
 * Write text to an output Metafile.
 */
    int
XgksMoTextToWs(ws, at, string)
    WS_STATE_PTR    ws;
    Gpoint         *at;
    Gchar          *string;
{
    Metafile       *mf	= &ws->mf;

    return MO_TEXT(mf, 1, at, string);
}


/*
 * Write text to all, appropriate, output Metafiles.
 */
    int
XgksMoText(at, string)
    Gpoint         *at;
    Gchar          *string;
{
    if (num_gksmo > 0)
	GMtext(active_gksmo, num_gksmo, at, string);
    if (num_cgmo > 0)
	CGMtext(active_cgmo, num_cgmo, at, string);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROtext(active_cairomo, num_cairomo, at, string);
#endif
    return OK;
}


/*
 * Write a cell array to an output Metafile.
 */
    int
XgksMoCellArrayToWs(ws, ll, ur, lr, row, colour, dim)
    WS_STATE_PTR    ws;
    Gpoint         *ll, *ur, *lr;
    Gint            row, *colour;
    Gipoint        *dim;
{
    Metafile       *mf	= &ws->mf;

    return MO_CELL_ARRAY(mf, 1, ll, ur, lr, row, colour, dim);
}


/*
 * Write a cell array to all, appropriate, output Metafiles.
 */
    int
XgksMoCellArray(ll, ur, lr, row, colour, dim)
    Gpoint         *ll, *ur, *lr;
    Gint           row, *colour;
    Gipoint        *dim;
{
    if (num_gksmo > 0)
	GMcellArray(active_gksmo, num_gksmo, ll, ur, lr, row, 
				 colour, dim);
    if (num_cgmo > 0)
	CGMcellArray(active_cgmo, num_cgmo, ll, ur, lr, row, 
				 colour, dim);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROcellArray(active_cairomo, num_cairomo, ll, ur, lr, row, 
				 colour, dim);
#endif
    return OK;
}


/*
 * Set the size of graphics in an output Metafile.
 */
    int
XgksMoSetGraphicSizeOnWs(ws, code, size)
    WS_STATE_PTR    ws;
    Gint            code;
    double          size;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_GRAPH_SIZE(mf, 1, code, size);
}


/*
 * Set the size of graphics in all, appropriate, output Metafiles.
 */
    int
XgksMoSetGraphicSize(code, size)
    Gint            code;
    double          size;
{
    if (num_gksmo > 0)
	GMsetGraphSize(active_gksmo, num_gksmo, code, size);
    if (num_cgmo > 0)
	CGMsetGraphSize(active_cgmo, num_cgmo, code, size);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetGraphSize(active_cairomo, num_cairomo, code, size);
#endif
    return OK;
}


/*
 * Close the open segment in an output Metafile.
 */
    int
XgksMoCloseSegOnWs(ws)
    WS_STATE_PTR    ws;
{
    Metafile       *mf	= &ws->mf;

    return MO_CLOSE_SEG(mf, 1);
}


/*
 * Close the open segment in all, appropriate, output Metafiles.
 */
    int
XgksMoCloseSeg()
{
    if (num_gksmo > 0)
	GMcloseSeg(active_gksmo, num_gksmo);
    if (num_cgmo > 0)
	CGMcloseSeg(active_cgmo, num_cgmo);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROcloseSeg(active_cairomo, num_cairomo);
#endif
    return OK;
}


/*
 * Set the graphic attributes in an output Metafile.
 */
    int
XgksMoSetGraphicAttrOnWs(ws, code, attr)
    WS_STATE_PTR    ws;
    Gint            code;
    Gint            attr;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_GRAPH_ATTR(mf, 1, code, attr);
}


/*
 * Set the graphic attributes in all, appropriate, output Metafiles.
 */
    int
XgksMoSetGraphicAttr(code, attr)
    Gint            code;
    Gint            attr;
{
    if (num_gksmo > 0)
	GMsetGraphAttr(active_gksmo, num_gksmo, code, attr);
    if (num_cgmo > 0)
	CGMsetGraphAttr(active_cgmo, num_cgmo, code, attr);
#ifdef CAIRODRAW
    if (num_cairomo > 0) {
      CAIROsetGraphAttr(active_cairomo, num_cairomo, code, attr);
      //fprintf(stderr,"ok we sent: %i\n",attr);
    }
#endif
    return OK;
}


/*
 * Set the font precision in an output Metafile.
 */
    int
XgksMoSetTextFPOnWs(ws, txfp)
    WS_STATE_PTR    ws;
    Gtxfp          *txfp;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_TEXT_FP(mf, 1, txfp);
}


/*
 * Set the font precision in all, appropriate, output Metafiles.
 */
    int
XgksMoSetTextFP(txfp)
    Gtxfp          *txfp;
{
    if (num_gksmo > 0)
	GMsetTextFP(active_gksmo, num_gksmo, txfp);
    if (num_cgmo > 0)
	CGMsetTextFP(active_cgmo, num_cgmo, txfp);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetTextFP(active_cairomo, num_cairomo, txfp);
#endif
    return OK;
}


/*
 * Set the character up-vector in an output Metafile.
 */
    int
XgksMoSetCharUpOnWs(ws, up, base)
    WS_STATE_PTR    ws;
    Gpoint         *up, *base;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_CHAR_UP(mf, 1, up, base);
}


/*
 * Set the character up-vector in all, appropriate, output Metafiles.
 */
    int
XgksMoSetCharUp()
{
    if (num_gksmo > 0)
	GMsetCharUp(active_gksmo, num_gksmo, (Gpoint*)NULL,
				      (Gpoint*)NULL);
    if (num_cgmo > 0)
	CGMsetCharUp(active_cgmo, num_cgmo, (Gpoint*)NULL,
				      (Gpoint*)NULL);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetCharUp(active_cairomo, num_cairomo, &xgks_state.gks_chattr.up,
				      &xgks_state.gks_chattr.base);
#endif
    return OK;
}


/*
 * Set the text-path in an output Metafile.
 */
    int
XgksMoSetTextPathOnWs(ws, path)
    WS_STATE_PTR    ws;
    Gtxpath         path;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_TEXT_PATH(mf, 1, path);
}


/*
 * Set the text-path in all, appropriate, output Metafiles.
 */
    int
XgksMoSetTextPath(path)
    Gtxpath         path;
{
    if (num_gksmo > 0)
	GMsetTextPath(active_gksmo, num_gksmo, path);
    if (num_cgmo > 0)
	CGMsetTextPath(active_cgmo, num_cgmo, path);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetTextPath(active_cairomo, num_cairomo, path);
#endif
    return OK;
}


/*
 * Set the text-alignment in an output Metafile.
 */
    int
XgksMoSetTextAlignOnWs(ws, align)
    WS_STATE_PTR    ws;
    Gtxalign       *align;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_TEXT_ALIGN(mf, 1, align);
}


/*
 * Set the text-alignment in all, appropriate, output Metafiles.
 */
    int
XgksMoSetTextAlign(align)
    Gtxalign       *align;
{
    if (num_gksmo > 0)
	GMsetTextAlign(active_gksmo, num_gksmo, align);
    if (num_cgmo > 0)
	CGMsetTextAlign(active_cgmo, num_cgmo, align);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetTextAlign(active_cairomo, num_cairomo, align);
#endif
    return OK;
}


/*
 * Set the interior fill-style in an output Metafile.
 */
    int
XgksMoSetFillIntStyleOnWs(ws, style)
    WS_STATE_PTR    ws;
    Gflinter        style;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_FILL_STYLE(mf, 1, style);
}


/*
 * Set the interior fill-style in all, appropriate, output Metafiles.
 */
    int
XgksMoSetFillIntStyle(style)
    Gflinter        style;
{
    if (num_gksmo > 0)
	GMsetFillStyle(active_gksmo, num_gksmo, style);
    if (num_cgmo > 0)
	CGMsetFillStyle(active_cgmo, num_cgmo, style);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetFillStyle(active_cairomo, num_cairomo, style);
#endif
    return OK;
}


/*
 * Set the pattern size in an output Metafile.
 */
    int
XgksMoSetPatSizeOnWs(ws)
    WS_STATE_PTR    ws;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_PATTERN_SIZE(mf, 1);
}


/*
 * Set the pattern size in all, appropriate, output Metafiles.
 */
    int
XgksMoSetPatSize()
{
    if (num_gksmo > 0)
	GMsetPatSize(active_gksmo, num_gksmo);
    if (num_cgmo > 0)
	CGMsetPatSize(active_cgmo, num_cgmo);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetPatSize(active_cairomo, num_cairomo);
#endif
    return OK;
}


/*
 * Set the pattern reference-point in an output Metafile.
 */
    int
XgksMoSetPatRefOnWs(ws)
    WS_STATE_PTR    ws;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_PATTERN_REFPT(mf, 1);
}


/*
 * Set the pattern reference-point in all, appropriate, output Metafiles.
 */
    int
XgksMoSetPatRef()
{
    if (num_gksmo > 0)
	GMsetPatRefpt(active_gksmo, num_gksmo);
    if (num_cgmo > 0)
	CGMsetPatRefpt(active_cgmo, num_cgmo);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetPatRefpt(active_cairomo, num_cairomo);
#endif
    return OK;
}


/*
 * Set the ASF in an output Metafile.
 */
    int
XgksMoSetAsfOnWs(ws)
    WS_STATE_PTR    ws;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_ASF(mf, 1);
}


/*
 * Set the ASF in all, appropriate, output Metafiles.
 */
    int
XgksMoSetAsf()
{
    if (num_gksmo > 0)
	GMsetAsf(active_gksmo, num_gksmo);
    if (num_cgmo > 0)
	CGMsetAsf(active_cgmo, num_cgmo);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetAsf(active_cairomo, num_cairomo);
#endif
    return OK;
}


/*
 * Set the line and marker representation in an output Metafile.
 */
    int
XgksMoSetLineMarkRep(ws, code, idx, type, size, colour)
    WS_STATE_PTR    ws;
    Gint            code, idx, type;
    double          size;
    Gint            colour;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_LINE_MARKER_REP(mf, 1, code, idx, type, size, colour);
}


/*
 * Set the text representation in an output Metafile.
 */
    int
XgksMoSetTextRep(ws, idx, rep)
    WS_STATE_PTR    ws;
    Gint            idx;
    Gtxbundl       *rep;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_TEXT_REP(mf, 1, idx, rep);
}


/*
 * Set the fill representation in an output Metafile.
 */
    int
XgksMoSetFillRep(ws, idx, rep)
    WS_STATE_PTR    ws;
    Gint            idx;
    Gflbundl       *rep;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_FILL_REP(mf, 1, idx, rep);
}


/*
 * Set the pattern representation in an output Metafile.
 */
    int
XgksMoSetPatRep(ws, idx, rep)
    WS_STATE_PTR    ws;
    Gint            idx;
    Gptbundl       *rep;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_PATTERN_REP(mf, 1, idx, rep);
}


/*
 * Set the colour representation in an output Metafile.
 */
    int
XgksMoSetColourRep(ws, idx, rep)
    WS_STATE_PTR    ws;
    Gint            idx;
    Gcobundl       *rep;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_COLOUR_REP(mf, 1, idx, rep);
}


/*
 * Set the clipping rectangle in an output Metafile.
 */
    int
XgksMoSetClipOnWs(ws, crect)
    WS_STATE_PTR    ws;
/*			RLM replaced Dec 15, 1994.		*/
/*    Glimit         *rect;					*/
    Gcliprec         *crect;
{
    Metafile       *mf	= &ws->mf;

/*			RLM replaced Dec 15, 1994.		*/
/*    return MO_SET_CLIPPING(mf, 1, rect);			*/
    if ( mf->any->type == MF_CGM )
      {
	CGMsetClip(mf, num_cgmo, crect);
	return OK;
      }
    else
/* 	GMsetClip(mf, num_gksmo, &(crect->rec)); */
    /* further modified by C. Doutriaux apr 3, 2007. */
      return MO_SET_CLIPPING(mf, 1, &(crect->rec));

}


/*
 * Set the clipping rectangle in all, appropriate, output Metafiles.
 */
    int
XgksMoSetClip(crect)
/*			RLM replaced Dec 15, 1994.		*/
/*    Glimit         *rect;					*/
    Gcliprec         *crect;
{
    if (num_gksmo > 0)
/*			RLM replaced Dec 15, 1994.		*/
/*	GMsetClip(active_gksmo, num_gksmo, rect);		*/
	GMsetClip(active_gksmo, num_gksmo, &(crect->rec));
    if (num_cgmo > 0)
/*			RLM replaced Dec 15, 1994.		*/
/*	CGMsetClip(active_cgmo, num_cgmo, rect);		*/
	CGMsetClip(active_cgmo, num_cgmo, crect);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetClip(active_cairomo, num_cairomo, &(crect->rec));
#endif
    return OK;
}


/*
 * Set the viewport limits in an output Metafile.
 */
    int
XgksMoSetLimit(ws, code, rect)
    WS_STATE_PTR    ws;
    Gint            code;
    Glimit         *rect;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_LIMIT(mf, 1, code, rect);
}


/*
 * Rename a segment in all, appropriate, output Metafiles.
 */
    int
XgksMoRenameSeg(old, new)
    Gint            old, new;
{
    if (num_gksmo > 0)
	GMrenameSeg(active_gksmo, num_gksmo, old, new);
    if (num_cgmo > 0)
	CGMrenameSeg(active_cgmo, num_cgmo, old, new);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROrenameSeg(active_cairomo, num_cairomo, old, new);
#endif
    return OK;
}


/*
 * Set the segment transformation in an output Metafile.
 */
    int
XgksMoSetSegTransOnWs(ws, name, matrix)
    WS_STATE_PTR    ws;
    Gint            name;
    Gfloat          matrix[2][3];
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_SEG_TRANS(mf, 1, name, matrix);
}


/*
 * Set the segment transformation in all, appropriate, output Metafiles.
 */
    int
XgksMoSetSegTrans(name, matrix)
    Gint            name;
    Gfloat          matrix[2][3];
{
    if (num_gksmo > 0)
	GMsetSegTran(active_gksmo, num_gksmo, name, matrix);
    if (num_cgmo > 0)
	CGMsetSegTran(active_cgmo, num_cgmo, name, matrix);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetSegTran(active_cairomo, num_cairomo, name, matrix);
#endif
    return OK;
}


/*
 * Set the segment attributes in an output Metafile.
 */
    int
XgksMoSetSegAttrOnWs(ws, name, code, attr)
    WS_STATE_PTR    ws;
    Gint            name, code, attr;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_SEG_ATTR(mf, 1, name, code, attr);
}


/*
 * Set the segment visibility in all, appropriate, output Metafiles.
 */
    int
XgksMoSetSegVis(name, vis)
    Gint            name;
    Gsegvis         vis;
{
    if (num_gksmo > 0)
	GMsetSegVis(active_gksmo, num_gksmo, name, vis);
    if (num_cgmo > 0)
	CGMsetSegVis(active_cgmo, num_cgmo, name, vis);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetSegVis(active_cairomo, num_cairomo, name, vis);
#endif
    return OK;
}


/*
 * Set the segment highlighting in all, appropriate, output Metafiles.
 */
    int
XgksMoSetSegHiLight(name, hilight)
    Gint            name;
    Gseghi          hilight;
{
    if (num_gksmo > 0)
	GMsetSegHilight(active_gksmo, num_gksmo, name, hilight);
    if (num_cgmo > 0)
	CGMsetSegHilight(active_cgmo, num_cgmo, name, hilight);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetSegHilight(active_cairomo, num_cairomo, name, hilight);
#endif
    return OK;
}


/*
 * Set the segment priority in an output Metafile.
 */
    int
XgksMoSetSegPriOnWs(ws, name, pri)
    WS_STATE_PTR    ws;
    Gint            name;
    double          pri;
{
    Metafile       *mf	= &ws->mf;

    return MO_SET_SEG_PRI(mf, 1, name, pri);
}


/*
 * Set the segment priority in all, appropriate output Metafiles.
 */
    int
XgksMoSetSegPri(name, pri)
    Gint            name;
    double          pri;
{
    if (num_gksmo > 0)
	GMsetSegPri(active_gksmo, num_gksmo, name, pri);
    if (num_cgmo > 0)
	CGMsetSegPri(active_cgmo, num_cgmo, name, pri);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetSegPri(active_cairomo, num_cairomo, name, pri);
#endif
    return OK;
}


/*
 * Set segment detectability in all, appropriate output Metafiles.
 */
    int
XgksMoSetSegDet(name, det)
    Gint            name;
    Gsegdet         det;
{
    if (num_gksmo > 0)
	GMsetSegDetect(active_gksmo, num_gksmo, name, det);
    if (num_cgmo > 0)
	CGMsetSegDetect(active_cgmo, num_cgmo, name, det);
#ifdef CAIRODRAW
    if (num_cairomo > 0)
	CAIROsetSegDetect(active_cairomo, num_cairomo, name, det);
#endif
    return OK;
}


/*
 * Add an output Metafile to a list of active, output Metafiles.
 */
    static void
add_mo(mo, list, num)
    Metafile	*mo;
    Metafile	*list;
    int		*num;
{
    assert(*num >= 0);
    assert(*num < MAX_ACTIVE_WS);

    list[(*num)++]	= *mo;
}


/*
 * Remove an output Metafile from a list of active, output Metafiles.
 */
    static void
remove_mo(mo, list, num)
    Metafile	*mo;
    Metafile	*list;
    int		*num;
{
    Metafile	*outp;

    assert(*num > 0);
    assert(*num <= MAX_ACTIVE_WS);

    /* Find the Metafile to be removed. */
    for (outp = list + *num; list < outp; ++list)
	if (list->any == mo->any)
	    break;

    assert(list < outp);

    if (list < outp) {

	/* Shift the list down over the found Metafile. */
	for (--outp; list < outp; ++list)
	    list[0].any	= list[1].any;

	--*num;
    }
}


/*
 * Activate an output Metafile: add it to the list of active, output
 * Metafiles and write initial output Metafile attributes.
 */
    int
XgksMoActivateWs(ws)
    WS_STATE_PTR    ws;
{
  /* changes Mar 3, 2007, C. Doutriaux */
    switch(ws->mf.any->type){
    case MF_GKSM:
      add_mo(&ws->mf, active_gksmo, &num_gksmo);
      break;
#ifdef CAIRODRAW
    case MF_CAIRO:
      add_mo(&ws->mf, active_cairomo, &num_cairomo);
      break;
#endif    case MF_CGM:
    default:
      add_mo(&ws->mf, active_cgmo, &num_cgmo);
      break;
    }
/*     if (ws->mf.any->type	== MF_GKSM) { */
/* 	add_mo(&ws->mf, active_gksmo, &num_gksmo); */
/*     } else { */
/* 	add_mo(&ws->mf, active_cgmo, &num_cgmo); */
/*     } */

/*		RLM Feb 3, 1995 added line.			*/
/*    mo_mode(ws->mf.cgmo, 1, CGMO_NOT_EMPTY);			*/
/*			RLM replaced Dec 15, 1994.		*/
/*    XgksMoSetClipOnWs(ws, &xgks_state.cliprec.rec);		*/
    XgksMoSetClipOnWs(ws, &xgks_state.cliprec);
    XgksSetLineAttrMo(ws, &xgks_state.gks_lnattr);
    XgksSetMarkAttrMo(ws, &xgks_state.gks_mkattr);
    XgksSetTextAttrMo(ws, &xgks_state.gks_txattr, &xgks_state.gks_chattr);
    XgksMoSetCharUpOnWs(ws, (Gpoint *) NULL, (Gpoint *) NULL);
    XgksSetFillPatAttrMo(ws, &xgks_state.gks_flattr, &xgks_state.gks_ptattr);

    XgksMoSetPatSizeOnWs(ws);
    XgksMoSetPatRefOnWs(ws);
    XgksMoSetAsfOnWs(ws);
    XgksMoSetGraphicAttrOnWs(ws, 44, xgks_state.gks_pick_id);

    return OK;
}


/*
 * Deactivate an output Metafile: remove it from the list of active, output
 * Metafiles.
 */
    int
XgksMoDeactivateWs(ws)
    WS_STATE_PTR    ws;
{
    /* Changed mar 3, 2007, C. Doutriaux */
     switch(ws->mf.any->type){
      case MF_GKSM:
	remove_mo(&ws->mf, active_gksmo, &num_gksmo);
	break;
#ifdef CAIRODRAW
      case MF_CAIRO:
	remove_mo(&ws->mf, active_cairomo, &num_cairomo);
	break;
#endif
      case MF_CGM:
      default:
	remove_mo(&ws->mf, active_cgmo, &num_cgmo);
	break;
    }
    return OK;

/*    if (ws->mf.any->type	== MF_GKSM) { */
/* 	remove_mo(&ws->mf, active_gksmo, &num_gksmo); */
/*     } else { */
/* 	remove_mo(&ws->mf, active_cgmo, &num_cgmo); */
/*     } */

/*     return OK; */
}


/*
 * Initialize the Metafile system.
 */
    int
XgksInitGksM()
{
    return OK;
}

