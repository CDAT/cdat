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
 * This file is the header-file for the XGKS implementation.  It includes most
 * necessary X header-files and most XGKS header-files -- it does not include
 * some "specialty" header-files or those specific to the Fortran 
 * implementation).  All XGKS library implementation-files should include this
 * header-file.
 *
 * This header-file should be included before any "specialty" X header-files
 * that depend upon <X11/Xlib.h>.
 *
 * $Id$
 * $__Header$
 */

#ifndef XGKS_GKS_IMPLEM_H
#define XGKS_GKS_IMPLEM_H

#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#endif
#include "gks.h"
#include "xgks.h"
#include "gks_defines.h"
#include "primitive.h"
#include "input.h"
#include "wdt.h"
#include "wslist.h"
#include "gkslist.h"
#include "gks_errors.h"
#include "event.h"
#include "metafile.h"

#endif	/* XGKS_GKS_IMPLEM_H not defined */
