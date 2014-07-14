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
 * This header-file depends upon header-file "xgks.h".
 * 
 * $Id$
 */

/*
 * Metafiles
 */

#ifndef XGKS_CGM_H
#define XGKS_CGM_H


/*
 * CGM API:
 */
extern int CGMrecSize	PROTO((
    Gint            type
));
extern int CGMnextItem	PROTO((
    Metafile	*mf		/* Metafile structure */
));
extern int CGMwriteItem	PROTO((
    Metafile	*mf,		/* Metafile structures */
    int		num,		/* Number of Metafiles */
    Gint	type,		/* item type */
    Gint	length,		/* item length */
    Gchar	*data		/* item data-record */
));
extern int CGMreadItem	PROTO((
    Metafile	*mf,		/* Metafile structure  */
    char        *record	/* input data-record */
));
extern int CGMmiOpen	PROTO((
    Metafile	*mf,		/* Metafile structure */
    char	*conn		/* Metafile identifier (filename) */
));
extern int CGMmiClose	PROTO((
    Metafile	*mf		/* Metafile structure */
));
extern int CGMmoOpen	PROTO((
    WS_STATE_PTR	ws
));
extern int CGMmoClose	PROTO((
    Metafile	*mf
));
extern int CGMclear	PROTO((
    Metafile	*mf,
    int		num,
    Gclrflag	flag
));
extern int CGMredrawAllSeg	PROTO((
    Metafile	*mf,
    int		num
));
extern int CGMupdate	PROTO((
    Metafile	*mf,
    int		num,
    Gregen	regenflag
));
extern int CGMdefer	PROTO((
    Metafile	*mf,
    int		num,
    Gdefmode	defer_mode,
    Girgmode	regen_mode
));
extern int CGMmessage	PROTO((
    Metafile	*mf,
    int		num,
    Gchar	*string
));
extern int CGMoutputGraphic	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Gint	num_pt,
    Gpoint	*pos
));
extern int CGMtext	PROTO((
    Metafile	*mf,
    int		num,
    Gpoint	*at,
    Gchar	*string
));
extern int CGMcellArray	PROTO((
    Metafile	*mf,
    int		num,
    Gpoint	*ll,
    Gpoint	*ur,
    Gpoint	*lr,
    Gint	row,
    Gint	*colour,
    Gipoint	*dim
));
extern int CGMsetGraphSize	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    double	size
));
extern int CGMcloseSeg	PROTO((
    Metafile	*mf,
    int		num
));
extern int CGMsetGraphAttr	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Gint	attr
));
extern int CGMsetTextFP	PROTO((
    Metafile	*mf,
    int		num,
    Gtxfp	*txfp
));
extern int CGMsetCharUp	PROTO((
    Metafile	*mf,
    int		num,
    Gpoint	*up,
    Gpoint	*base
));
extern int CGMsetTextPath	PROTO((
    Metafile	*mf,
    int		num,
    Gtxpath	path
));
extern int CGMsetTextAlign	PROTO((
    Metafile	*mf,
    int		num,
    Gtxalign	*align
));
extern int CGMsetFillStyle	PROTO((
    Metafile	*mf,
    int		num,
    Gflinter	style
));
extern int CGMsetPatSize	PROTO((
    Metafile	*mf,
    int		num
));
extern int CGMsetPatRefpt	PROTO((
    Metafile	*mf,
    int		num
));
extern int CGMsetAsf	PROTO((
    Metafile	*mf,
    int		num
));
extern int CGMsetLineMarkRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Gint	idx,
    Gint	type,
    double	size,
    Gint	colour
));
extern int CGMsetTextRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gtxbundl	*rep
));
extern int CGMsetFillRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gflbundl	*rep
));
extern int CGMsetPatRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gptbundl	*rep
));
extern int CGMsetColRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gcobundl	*rep
));
extern int CGMsetClip	PROTO((
    Metafile	*mf,
    int		num,
/*				RLM changed Dec 15, 1994.		*/
    Gcliprec	*rect
));
extern int CGMsetLimit	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Glimit	*rect
));
extern int CGMrenameSeg	PROTO((
    Metafile	*mf,
    int		num,
    Gint	old,
    Gint	new
));
extern int CGMsetSegTran	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gfloat	matrix[2][3]
));
extern int CGMsetSegAttr	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gint	code,
    Gint	attr
));
extern int CGMsetSegVis	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gsegvis	vis
));
extern int CGMsetSegHilight	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gseghi	hilight
));
extern int CGMsetSegPri	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    double	pri
));
extern int CGMsetSegDetect	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gsegdet	det
));

#endif	/* XGKS_CGM_H not defined above */
