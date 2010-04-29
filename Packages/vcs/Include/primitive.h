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
 * Define file for : GKS output primitive structures
 *
 * This header-file depends upon header-files "xgks.h" and "gks_defines.h".
 *
 * $Id$
 * $__Header$
 */

/*
 * All structures for output primitives are subjected to changes.
 */

#ifndef PRIMITIVE_H
#define PRIMITIVE_H


typedef struct {
    Gint            num_pts;		/* Number of points for polyline */
    Gpoint         *pts;		/* Array of points in NDC */
    Glnattr         plnattr;		/* Polyline attrribute */
}               PLINE_ST;		/* Output Primitive structure for
					 * polyline */

typedef struct {
    Gint            num_pts;		/* Number of points to plot markers
					 * at */
    Gpoint         *location;		/* Location of the polymarker */
    Gmkattr         mkattr;		/* Polymarker attributes */
}               PMARK_ST;		/* Output Primitive structure for
					 * polymarker */

typedef struct {
    Gchar          *string;		/* The string to be output */
}               MESG_ST;		/* Structure for implementation
					 * dependent message */

typedef struct {
    Gpoint         *location;		/* Starting location of text */
    Gchar          *string;		/* The string to be output */
    Gpoint          up_vec, base_vec;	/* Orientation and magnituce in NDC
					 * space */
    Gtxattr         txattr;		/* Text attributes */
    CHATTR          chattr;
}               TEXT_ST;		/* Output Primitive structure for
					 * text */

typedef struct {
    Gint            num_pts;		/* Number of points for the area */
    Gpoint         *pts;		/* Array of points in NDC */
    Gflattr         flattr;		/* Fill area attibutes */
    PTATTR          ptattr;		/* Pattern attributes */
} FILL_AREA_ST;				/* Output primitive structure for
					 * fill area */

typedef struct {
    Gpoint          ll, lr, ur, ul;	/* the NDC values of the cell
					 * rectangle */
    Gipoint         dim;		/* colour index array dimensions */
    Gint            rowsize;		/* colour index array row length */
    Gint           *colour;		/* colour index array */
} CELL_ARRAY_ST;			/* Outptu promitive structure for
					 * cell array */

typedef struct {
    Gint            segment;		/* Indicating if is a clip in segment */
    Glimit          rec;		/* Current clipping rectangle */
} CLIP_REC_ST;

typedef struct {
    Gint            dummy;		/* don't know what to do yet */
} GDP_ST;				/* Output primitive structure for
					 * generous purpose primitive */

typedef struct entry {
    PID             pid;		/* Identify which type of output
					 * primitive is active */
    Gint            seg_cnt;		/* Segment counter to non_segment
					 * primitives */
    Gint            pickid;		/* Identify which of the primitives
					 * in the segment is picked */
    struct entry   *next;		/* Pointer to next output primitive
					 * structure */
    union {
	PLINE_ST        pline;
	PMARK_ST        pmark;
	TEXT_ST         text;
	FILL_AREA_ST    fill_area;
	CELL_ARRAY_ST   cell_array;
	CLIP_REC_ST     clip;
	MESG_ST         mesg;
	GDP_ST          gdp;
    }               primi;
}               OUT_PRIMI;		/* The structure for all output
					 * primitives */

OUT_PRIMI      *XgksNewPrimi();
OUT_PRIMI      *XgksAppendSegPrimi();

#define UPDATE_SEG_CNT(ptr)	((ptr->seg_cnt)++)

#endif
