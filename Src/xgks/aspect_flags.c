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
 * aspect_flags.c - functions dealing with the Aspect Source Flages.
 *		s_aspect_flags();
 */

/* LINTLIBRARY */

#ifndef lint
    static char afsid[] = "$__Header$";
    static char rcsid[] = "$Id$";
#endif

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include "gks_implem.h"


/*
 * gsetasf (asf) - SET ASPECT SOURCE FLAGS
 * Gasfs  *asf;	   Array of the 13 aspect flags INDIVIDUAL | BUNDLED.
 *
 * returns: 0=OK or 8
 *
 * See Also: ANSI Standard p.101
 */
gsetasf(asf)
    Gasfs          *asf;
{
    /* check for proper gks state */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgsetasf);

    /* Set the asf in gks list */
    xgks_state.gks_lnattr.type = asf->ln_type;
    xgks_state.gks_lnattr.width = asf->ln_width;
    xgks_state.gks_lnattr.colour = asf->ln_colour;
    xgks_state.gks_mkattr.type = asf->mk_type;
    xgks_state.gks_mkattr.size = asf->mk_size;
    xgks_state.gks_mkattr.colour = asf->mk_colour;
    xgks_state.gks_txattr.fp = asf->tx_fp;
    xgks_state.gks_txattr.tx_exp = asf->tx_exp;
    xgks_state.gks_txattr.space = asf->tx_space;
    xgks_state.gks_txattr.colour = asf->tx_colour;
    xgks_state.gks_flattr.inter = asf->fl_inter;
    xgks_state.gks_flattr.style = asf->fl_style;
    xgks_state.gks_flattr.colour = asf->fl_colour;

    if (MO_OPENED == TRUE)
	XgksMoSetAsf();

    return 0;
}


/*
 * XgksInitGksAsf() - give gks_asfs the value GINDIVIDUAL.
 */
XgksInitGksAsf()
{
    xgks_state.gks_lnattr.type = GINDIVIDUAL;
    xgks_state.gks_lnattr.width = GINDIVIDUAL;
    xgks_state.gks_lnattr.colour = GINDIVIDUAL;
    xgks_state.gks_mkattr.type = GINDIVIDUAL;
    xgks_state.gks_mkattr.size = GINDIVIDUAL;
    xgks_state.gks_mkattr.colour = GINDIVIDUAL;
    xgks_state.gks_txattr.fp = GINDIVIDUAL;
    xgks_state.gks_txattr.tx_exp = GINDIVIDUAL;
    xgks_state.gks_txattr.space = GINDIVIDUAL;
    xgks_state.gks_txattr.colour = GINDIVIDUAL;
    xgks_state.gks_flattr.inter = GINDIVIDUAL;
    xgks_state.gks_flattr.style = GINDIVIDUAL;
    xgks_state.gks_flattr.colour = GINDIVIDUAL;
}
