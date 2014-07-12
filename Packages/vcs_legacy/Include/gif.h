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
 * PostScript driver for XGKS metafiles
 * Created by Joe Sirott, Pacific Marine Environmental Lab
 * $Id$
 */


#ifndef XGKS_GIF_H
#define XGKS_GIF_H

extern int GIFrecSize	PROTO((
    Gint            type
));
extern int GIFnextItem	PROTO((
    Metafile	*mf		/* Metafile structure */
));
extern int GIFwriteItem	PROTO((
    Metafile      **mf,		/* Metafile structures */
    int             num,	/* Number of Metafiles */
    Gint	    type,	/* item type */
    Gint	    length,	/* item length */
    Gchar          *data	/* item data-record */
));
extern int GIFreadItem	PROTO((
    Metafile	*mf,		/* Metafile structure  */
    char        *record	/* input data-record */
));
extern int GIFmiOpen	PROTO((
    Metafile	*mf		/* Metafile structure */
));
extern int GIFmoOpen	PROTO((
    WS_STATE_PTR ws
));
extern int GIFmoClose	PROTO((
    Metafile	*mf
));
extern int GIFclear	PROTO((
    Metafile	*mf,
    int		num,
    Gclrflag	flag
));
extern int GIFredrawAllSeg	PROTO((
    Metafile	**mf,
    int		num
));
extern int GIFupdate	PROTO((
    Metafile	**mf,
    int		num,
    Gregen	regenflag
));
extern int GIFdefer	PROTO((
    Metafile	**mf,
    int		num,
    Gdefmode	defer_mode,
    Girgmode	regen_mode
));
extern int GIFmessage	PROTO((
    Metafile	**mf,
    int		num,
    Gchar	*string
));
extern int GIFoutputGraphic	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Gint	num_pt,
    Gpoint	*pos
));
extern int GIFtext	PROTO((
    Metafile	*mf,
    int		num,
    Gpoint	*at,
    Gchar	*string
));
extern int GIFcellArray	PROTO((
    Metafile	*mf,
    int		num,
    Gpoint	*ll,
    Gpoint	*ur,
    Gpoint	*lr,
    Gint	row,
    Gint	*colour,
    Gipoint	*dim
));
extern int GIFsetGraphSize	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    double	size
));
extern int GIFcloseSeg	PROTO((
    Metafile	*mf,
    int		num
));
extern int GIFsetGraphAttr	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Gint	attr
));
extern int GIFsetTextFP	PROTO((
    Metafile	*mf,
    int		num,
    Gtxfp	*txfp
));
extern int GIFsetCharUp	PROTO((
    Metafile	*mf,
    int		num,
    Gpoint	*up,
    Gpoint	*base
));
extern int GIFsetTextPath	PROTO((
    Metafile	*mf,
    int		num,
    Gtxpath	path
));
extern int GIFsetTextAlign	PROTO((
    Metafile	*mf,
    int		num,
    Gtxalign	*align
));
extern int GIFsetFillStyle	PROTO((
    Metafile	*mf,
    int		num,
    Gflinter	style
));
extern int GIFsetPatSize	PROTO((
    Metafile	*mf,
    int		num
));
extern int GIFsetPatRefpt	PROTO((
    Metafile	*mf,
    int		num
));
extern int GIFsetAsf	PROTO((
    Metafile	*mf,
    int		num
));
extern int GIFsetLineMarkRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Gint	idx,
    Gint	type,
    double	size,
    Gint	colour
));
extern int GIFsetTextRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gtxbundl	*rep
));
extern int GIFsetFillRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gflbundl	*rep
));
extern int GIFsetPatRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gptbundl	*rep
));
extern int GIFsetColRep	PROTO((
    Metafile	*mf,
    int		num,
    Gint	idx,
    Gcobundl	*rep
));
extern int GIFsetClip	PROTO((
    Metafile	*mf,
    int		num,
    Glimit	*rect
));
extern int GIFsetLimit	PROTO((
    Metafile	*mf,
    int		num,
    Gint	code,
    Glimit	*rect
));
extern int GIFrenameSeg	PROTO((
    Metafile	*mf,
    int		num,
    Gint	old,
    Gint	new
));
extern int GIFsetSegTran	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gfloat	matrix[2][3]
));
extern int GIFsetSegAttr	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gint	code,
    Gint	attr
));
extern int GIFsetSegVis	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gsegvis	vis
));
extern int GIFsetSegHilight	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gseghi	hilight
));
extern int GIFsetSegPri	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    double	pri
));
extern int GIFsetSegDetect	PROTO((
    Metafile	*mf,
    int		num,
    Gint	name,
    Gsegdet	det
));

#endif	/* XGKS_PS_H not defined */
