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
 * PostScript driver for XGKS metafiles
 * Created by Joe Sirott, Pacific Marine Environmental Lab
 * $Id$
 */


#ifndef XGKS_CAIRO_H
#define XGKS_CAIRO_H


extern int CAIROrecSize	PROTO((
    Gint            type
));
extern int CAIROnextItem	PROTO((
    Metafile	*mf		/* Metafile structure */
));
extern int CAIROwriteItem	PROTO((
    Metafile      **mf,		/* Metafile structures */
    int             num,	/* Number of Metafiles */
    Gint	    type,	/* item type */
    Gint	    length,	/* item length */
    Gchar          *data	/* item data-record */
));
extern int CAIROreadItem	PROTO((
    Metafile	*mf,		/* Metafile structure  */
    char        *record	/* input data-record */
));
extern int CAIROmiOpen	PROTO((
    Metafile	*mf		/* Metafile structure */
));
extern int CAIROclear	PROTO((
    Metafile	*mf,
    int		num,
    Gclrflag	flag
));
extern int CAIROredrawAllSeg	PROTO((
    Metafile	**mf,
    int		num
));
extern int CAIROupdate	PROTO((
    Metafile	**mf,
    int		num,
    Gregen	regenflag
));
extern int CAIROdefer	PROTO((
    Metafile	**mf,
    int		num,
    Gdefmode	defer_mode,
    Girgmode	regen_mode
));
extern int CAIROmessage	PROTO((
    Metafile	**mf,
    int		num,
    Gchar	*string
));
extern int CAIROoutputGraphic	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Gint	num_pt,
    Gpoint	*pos
));
extern int CAIROtext	PROTO((
    Metafile	*mf,
    int		num,
    Gpoint	*at,
    Gchar	*string
));
extern int CAIROcellArray	PROTO((
    Metafile	*mf,
    int		num,
    Gpoint	*ll,
    Gpoint	*ur,
    Gpoint	*lr,
    Gint	row,
    Gint	*colour,
    Gipoint	*dim
));
extern int CAIROsetGraphSize	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    double	size
));
extern int CAIROcloseSeg	PROTO((
    Metafile	*mf,
    int		num
));
extern int CAIROsetGraphAttr	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Gint	attr
));
extern int CAIROsetTextFP	PROTO((
    Metafile	*mf,
    int		num,
    Gtxfp	*txfp
));
extern int CAIROsetCharUp	PROTO((
    Metafile	*mf,
    int		num,
    Gpoint	*up,
    Gpoint	*base
));
extern int CAIROsetTextPath	PROTO((
    Metafile	*mf,
    int		num,
    Gtxpath	path
));
extern int CAIROsetTextAlign	PROTO((
    Metafile	*mf,
    int		num,
    Gtxalign	*align
));
extern int CAIROsetFillStyle	PROTO((
    Metafile	*mf,
    int		num,
    Gflinter	style
));
extern int CAIROsetPatSize	PROTO((
    Metafile	*mf,
    int		num
));
extern int CAIROsetPatRefpt	PROTO((
    Metafile	*mf,
    int		num
));
extern int CAIROsetAsf	PROTO((
    Metafile	*mf,
    int		num
));
extern int CAIROsetLineMarkRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Gint	idx,
    Gint	type,
    double	size,
    Gint	colour
));
extern int CAIROsetTextRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gtxbundl	*rep
));
extern int CAIROsetFillRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gflbundl	*rep
));
extern int CAIROsetPatRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gptbundl	*rep
));
extern int CAIROsetColRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gcobundl	*rep
));
extern int CAIROsetClip	PROTO((
    Metafile	*mf,
    int		num,
    Glimit	*rect
));
extern int CAIROsetLimit	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Glimit	*rect
));
extern int CAIROrenameSeg	PROTO((
    Metafile	*mf,
    int		num,
    Gint	old,
    Gint	new
));
extern int CAIROsetSegTran	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gfloat	matrix[2][3]
));
extern int CAIROsetSegAttr	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gint	code,
    Gint	attr
));
extern int CAIROsetSegVis	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gsegvis	vis
));
extern int CAIROsetSegHilight	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gseghi	hilight
));
extern int CAIROsetSegPri	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    double	pri
));
extern int CAIROsetSegDetect	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gsegdet	det
));
extern int CAIROmoOpen	PROTO((
    WS_STATE_PTR ws
));
extern int CAIROmoClose	PROTO((
    Metafile	*mf
));
extern int CAIROmessage	PROTO((
    Metafile	**mf,
    int		num,
    Gchar	*string
));

#endif	/* XGKS_CAIRO_H not defined */
