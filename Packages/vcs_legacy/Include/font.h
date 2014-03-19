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
 * font.h - vector fonts for UIGKS servers.
 *
 * $Id$
 * $__Header$
 */

#define bits16	short			/* MACHINE DEPENDENT */

/* vc_type : semantics of vc_x and vc_y:
 *	s = character extent minimum
 *	S = character extent maximum
 *	m = move current position to
 *	d = draw to 
 *	e = end of character definition
 */
struct vcharst {
	char	vc_type;
	bits16	vc_x;
	bits16	vc_y;
};

/*
 * Each font has 256 characters (some may be undefined) each of which is
 * defined by an array of vcharst structures
 */
typedef struct {
	char	fname[30];		/* font name */
	bits16	fnominalx, fnominaly;	/* nominal character size (size 'm') */
	bits16	ftop,			/* displacment from y == 0 */
		fcap,
		fhalf,
		fbase,
		fbottom;
	int	fcharoffset[256];	/* offset into fchars to beginning
					 * of each character. -1 means no
					 * definition for that character */
	struct	vcharst fchars[1];	/* the chars */
} FONT;
