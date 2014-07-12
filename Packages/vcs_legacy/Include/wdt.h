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
 * wdt.h - GKS workstation description table structure
 *
 * This header-file depends upon header-files "xgks.h" and "gks_defines.h".
 * 
 * $Id$
 * $__Header$
 */

#ifndef	WDT_H
#define WDT_H

#define WS_CLASS	GRASTER
#define DEV_UNIT	GDC_OTHER

#define WS_MAX_DCX	1280
#define WS_MAX_DCY	1024

#define WS_LINE_TYPE(t)	   ( (t)>=GLN_LDASH && (t)<=GLN_DOTDASH )
#define WS_MARKER_TYPE(t)  ((t)>0 && (t)<=GMK_X)

/* Goes from 1 to 9 */
#define WS_FONT_TYPE(t)    ((t)>0 && (t)<=DEFINED_FONT_TYPE)

#define WS_FILL_TYPE(s,t)       ((((s)==GPATTERN) && ((t)>0 && \
				  (t)<MAX_BUNDL_TBL)) ||  \
				 (((s)==GHATCH) && ((t)<0 && \
				  (t)> -MAX_BUNDL_TBL)))

#define WS_FILL_IMPLEMENT	GHATCH

#define WS_AVAIL_COLOUR(ws, colour) ((((colour)>=0) && \
				      ((colour)<(ws)->wscolour)) || \
				     ((ws)->ewstype==MO) || \
				     ((ws)->ewstype==MI))

#endif
