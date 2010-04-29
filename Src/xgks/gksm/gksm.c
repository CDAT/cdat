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
#include <sys/utsname.h>	/* for uname() */
#include <unistd.h>		/* for getlogin() */
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>
#include "gks_implem.h"
#include "gksm.h"
#include "gksm_implem.h"

#ifndef lint
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif

static Gint	gksm_version = 1;

/* 11 bytes dummy for this implementation */
static Gchar	dummy[] = "dummy info.";

/* String array for formats created on the fly */
static Gchar	fmt[80];


/*
 * Return the size of the data-record associated with a GKSM item-type.
 */
    static int
GMrecSize(type)
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
	return INT_FIELD_LENGTH;

    case 4:
    case 30:
    case 36:
    case 83:
    case 92:
    case 93:
    case 95:
	return 2 * INT_FIELD_LENGTH;

    case 5:
	return INT_FIELD_LENGTH;

    case 11:
    case 12:
    case 14:
	return INT_FIELD_LENGTH;

    case 13:
	return 2 * FLOAT_FIELD_LENGTH + INT_FIELD_LENGTH;

    case 15:
	return 6 * FLOAT_FIELD_LENGTH + INT_FIELD_LENGTH;

    case 23:
    case 27:
    case 31:
    case 32:
	return FLOAT_FIELD_LENGTH;

    case 34:
	return 4 * FLOAT_FIELD_LENGTH;

    case 43:
	return 13 * INT_FIELD_LENGTH;

    case 41:
	return 4 * FLOAT_FIELD_LENGTH;

    case 42:
	return 2 * FLOAT_FIELD_LENGTH;

    case 51:
    case 52:
	return FLOAT_FIELD_LENGTH + 3 * INT_FIELD_LENGTH;

    case 53:
	return 2 * FLOAT_FIELD_LENGTH + 4 * INT_FIELD_LENGTH;

    case 54:
	return 4 * INT_FIELD_LENGTH;

    case 55:
	return 3 * INT_FIELD_LENGTH;

    case 56:
	return 3 * FLOAT_FIELD_LENGTH + INT_FIELD_LENGTH;

    case 61:
    case 71:
    case 72:
	return 4 * FLOAT_FIELD_LENGTH;

    case 91:
	return 6 * FLOAT_FIELD_LENGTH + INT_FIELD_LENGTH;

    case 94:
	return FLOAT_FIELD_LENGTH + INT_FIELD_LENGTH;

    default:
	return INVALID;
    }
}


/*
 * Indicate whether or not the end-of-line has been reached.
 */
    static Gint
XgksFeoln(fp)
    FILE           *fp;
{
    int             i;

    if ((i = getc(fp)) == EOF)
	return TRUE;
    (void) ungetc(i, fp);
    if (i == '\n')
	return TRUE;
    else
	return FALSE;
}


/*
 * Read-in the data-record of the current item.
 */
    static Gint
XgksInputData(fp, type, length, record)
    Gfile          *fp;
    Gint            type;
    Gint	    length;
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
    Gint            i, j;
    Gchar           scratch[80];
    Gint            readcnt = 1;

    clearerr(fp);

    if (type > 100) {
	for (i = 0; i < length; i++) {
	    (void) READCHR(fp, *(((char *) record) + i));
	    if (ferror(fp)) {
		return MF_FILE_ERR;
	    }
	    if (*(((char *) record) + i) == '\n') {
		return MF_ITEM_ERR;
	    }
	}
	return METAFILE_OK;
    }
    switch (type) {

    case 0:
    case 2:
    case 82:
	*record = 0;
	break;

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
	ptr1 = (XGKSMONE *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, ptr1->flag);
	break;

    case 4:
    case 30:
    case 36:
    case 83:
    case 92:
    case 93:
    case 95:
	ptr2 = (XGKSMTWO *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, ptr2->item1);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, ptr2->item2);
	break;

    case 5:
	msg = (XGKSMMESG *) record;
	msg->string = &(((char *) record)[sizeof(XGKSMMESG)]);
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, msg->strlen);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READCHR(fp, msg->string[0]);
	if (!readcnt)
	    break;
	if (msg->string[0] == '\n') {
	    readcnt = 0;
	    break;
	}
	for (i = 1; i < msg->strlen; i++) {
	    readcnt = READCHR(fp, msg->string[i]);
	    if (!readcnt)
		break;
	    if (msg->string[i] == '\n') {
		readcnt = 0;
		break;
	    }
	}
	msg->string[i] = 0;
	break;

    case 11:
    case 12:
    case 14:
	graph = (XGKSMGRAPH *) record;
	graph->pts = (Gpoint *) (((char *) record) + sizeof(XGKSMGRAPH));
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, graph->num_pts);
	if (!readcnt)
	    break;
	for (i = 0; i < graph->num_pts; i++) {
	    if (XgksFeoln(fp)) {
		readcnt = 0;
		break;
	    }
	    readcnt = READFTP(fp, graph->pts[i].x);
	    if (!readcnt)
		break;
	    if (XgksFeoln(fp)) {
		readcnt = 0;
		break;
	    }
	    readcnt = READFTP(fp, graph->pts[i].y);
	    if (!readcnt)
		break;
	}
	break;

    case 13:
	text = (XGKSMTEXT *) record;
	text->string = &(((char *) record)[sizeof(XGKSMTEXT)]);
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = fscanf(fp, "%f", &(text->location.x));
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	(void) fscanf(fp, FLOAT_FMT_SCAN, scratch);
	scratch[(int) FLOAT_FIELD_LENGTH] = 0;
	readcnt = sscanf(scratch, "%f", &(text->location.y));
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	(void) fscanf(fp, INT_FMT_SCAN, scratch);
	scratch[INT_FIELD_LENGTH] = 0;
	readcnt = sscanf(scratch, "%d", &(text->strlen));
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READCHR(fp, text->string[0]);
	if (!readcnt)
	    break;
	if (text->string[0] == '\n') {
	    readcnt = 0;
	    break;
	}
	for (i = 1; i < text->strlen; i++) {
	    readcnt = READCHR(fp, text->string[i]);
	    if (text->string[i] == '\n') {
		readcnt = 0;
		break;
	    }
	    if (!readcnt)
		break;
	}
	text->string[i] = 0;
	break;

    case 15:
	cell = (XGKSMCELLARRAY *) record;
	cell->colour = (Gint *) (((char *) record) + sizeof(XGKSMCELLARRAY));
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, cell->ll.x);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, cell->ll.y);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, cell->ur.x);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, cell->ur.y);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, cell->lr.x);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, cell->lr.y);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, cell->dim.x);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, cell->dim.y);
	if (!readcnt)
	    break;
	j = cell->dim.x * cell->dim.y;
	for (i = 0; i < j; i++) {
	    if (XgksFeoln(fp)) {
		readcnt = 0;
		break;
	    }
	    readcnt = READINT(fp, cell->colour[i]);
	    if (!readcnt)
		break;
	}
	break;

    case 23:
    case 27:
    case 31:
    case 32:
	size = (XGKSMSIZE *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, size->size);
	break;

    case 34:
	vec = (XGKSMCHARVEC *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, vec->up.x);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, vec->up.y);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, vec->base.x);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, vec->base.y);
	break;

    case 41:
	patsiz = (XGKSMPATSIZ *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, patsiz->wid.x);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, patsiz->wid.y);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, patsiz->hgt.x);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, patsiz->hgt.y);
	break;

    case 42:
	patref = (XGKSMPATREF *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, patref->ref.x);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, patref->ref.y);
	break;

    case 43:
	asf = (XGKSMASF *) record;
	for (i = 0; i < 13; i++) {
	    if (XgksFeoln(fp)) {
		readcnt = 0;
		break;
	    }
	    readcnt = READINT(fp, asf->asf[i]);
	    if (!readcnt)
		break;
	}
	break;

    case 51:
    case 52:
	lmrep = (XGKSMLMREP *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, lmrep->idx);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, lmrep->style);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, lmrep->size);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, lmrep->colour);
	break;

    case 53:
	txrep = (XGKSMTEXTREP *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, txrep->idx);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, txrep->font);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, txrep->prec);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, txrep->tx_exp);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, txrep->space);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, txrep->colour);
	break;

    case 54:
	flrep = (XGKSMFILLREP *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, flrep->idx);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, flrep->intstyle);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, flrep->style);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, flrep->colour);
	break;

    case 55:
	patrep = (XGKSMPATREP *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, patrep->idx);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, patrep->size.x);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, patrep->size.y);
	if (!readcnt)
	    break;
	patrep->array = (Gint *) (((char *) record) + sizeof(XGKSMPATREP));
	j = patrep->size.x * patrep->size.y;
	for (i = 0; i < j; i++) {
	    if (XgksFeoln(fp)) {
		readcnt = 0;
		break;
	    }
	    readcnt = READINT(fp, patrep->array[i]);
	    if (!readcnt)
		break;
	}
	break;

    case 56:
	corep = (XGKSMCOLOURREP *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, corep->idx);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, corep->red);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, corep->green);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, corep->blue);
	break;

    case 61:
    case 71:
    case 72:
	limit = (XGKSMLIMIT *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, limit->rect.xmin);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, limit->rect.xmax);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, limit->rect.ymin);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, limit->rect.ymax);
	break;

    case 91:
	tran = (XGKSMSEGTRAN *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, tran->name);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, tran->matrix[0][0]);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, tran->matrix[0][1]);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, tran->matrix[0][2]);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, tran->matrix[1][0]);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, tran->matrix[1][1]);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, tran->matrix[1][2]);
	break;

    case 94:
	pri = (XGKSMSEGPRI *) record;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READINT(fp, pri->name);
	if (!readcnt)
	    break;
	if (XgksFeoln(fp)) {
	    readcnt = 0;
	    break;
	}
	readcnt = READFTP(fp, pri->pri);
	break;

    default:
	readcnt = 0;
	break;
    }

    if (!readcnt) {
	if (ferror(fp)) {
	    return MF_FILE_ERR;
	} else {
	    return MF_DATA_ERR;
	}
    }

    return METAFILE_OK;
}


/*
 * Read the next item from a GKSM.
 */
    Gint
GMnextItem(mf)
    Metafile	*mf;		/* Metafile structure */
{
    Gint	i;
    mf_gksm	*gksm	= mf->gksm;
    Gfile	*fp	= gksm->fp;

    (void) fscanf(fp, " ");
    if (feof(fp)) {
	gksm->GksmEmpty = TRUE;
	gksm->CurItem.type = INVALID;
	gksm->CurItem.length = INVALID;
	return METAFILE_OK;
    }
    for (i = 0; i < gksm->h; i++)
	(void) READCHR(fp, gksm->std[i]);
    if (READINT(fp, gksm->CurItem.type) != 1)
	return MF_FILE_ERR;
    if (feof(fp)) {
	gksm->GksmEmpty = TRUE;
	gksm->CurItem.type = INVALID;
	gksm->CurItem.length = INVALID;
	return MF_FILE_ERR;
    }
    if (XgksFeoln(fp))
	return MF_FILE_ERR;
    if (READINT(fp, gksm->CurItem.length) != 1)
	return MF_FILE_ERR;
    gksm->GksmEmpty = FALSE;

    return METAFILE_OK;
}


/*
 * Return a string identifying the user and installation.
 */
    static Gchar*
XgksMAuthor()
{
    char		*username	= getlogin();
    struct utsname	name;
    static Gchar	buffer[41];

    buffer[0]	= 0;

    if (username != NULL)
	(void) strncat(buffer, username, sizeof(buffer) - 1);

    if (uname(&name) != -1) {
	int	nchr	= strlen(buffer);

	if (nchr < sizeof(buffer) - 1) {
	    buffer[nchr++]	= '@';
	    (void) strncpy(buffer + nchr, name.nodename, 
			   sizeof(buffer) - nchr - 1);
	}
    }

    return buffer;
}


/*
 * Return a date-string.
 */
    static Gchar*
XgksMDate()
{
    time_t          clock = time((time_t *) NULL);
    static Gchar    date[9];

    (void) strftime(date, (size_t)sizeof(date), "%y/%m/%d", 
                    localtime(&clock));

    return date;
}


/*
 * Write an item to a GKSM.
 */
    int
GMwriteItem(mf, num, type, length, data)
    Metafile       *mf;
    int             num;
    Gint	    type;	/* item type */
    Gint	    length;	/* item length */
    Gchar          *data;	/* item data-record */
{
    int             imf;
    Gint            i;

    (void) sprintf(fmt, "%s%s", ITEM_TYPE_FMT, ITEM_LENGTH_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, type, length);
	for (i = 0; i < length; i++)
	    (void) fprintf(fp, "%c", data[i]);
	(void) fprintf(fp, "\n");
    }

    return OK;
}


/*
 * Read the data-record of the current item from a GKSM.
 *
 * The filestat field has been added to the workstation state structure to
 * retain MI error information between calls to ggetgksm and greadgksm.  The
 * field has one of four possible integer values (defined in metafile.h):
 *    METAFILE_OK -- no errors so far reading from metafile
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
GMreadItem(mf, record)
    Metafile	*mf;		/* Metafile structure  */
    char        *record;	/* input data-record */
{
    mf_gksm	*gksm	= mf->gksm;
    FILE	*fp	= gksm->fp;

    if (gksm->CurItem.length > 0) {
	gksm->filestat	= XgksInputData(fp, gksm->CurItem.type, 
					gksm->CurItem.length, record);
	if (feof(fp)) {
	    gksm->GksmEmpty	= TRUE;
	    (void) gerrorhand(162, errgreadgksm, xgks_state.gks_err_file);
	    return 162;
	}
	GKSERROR((gksm->filestat == MF_ITEM_ERR) ||
		     (gksm->filestat == MF_FILE_ERR),
		 163, errgreadgksm);
	GKSERROR((gksm->filestat == MF_DATA_ERR), 165, errgreadgksm);
    } else {
	/* skip to end of current item */
	(void) fscanf(fp, "%*[^\n]");
    }

    return OK;
}


/*
 * Open an input GKSM: scan in header and the first item.
 */
    int
GMmiOpen(mf, conn)
    Metafile	*mf;		/* Metafile structure */
    char	*conn;		/* Metafile identifier (filename) */
{
    int		status	= 1;	/* return status = error */

    if ((mf->gksm = (mf_gksm*)umalloc(sizeof(mf_gksm))) != NULL) {
	mf_gksm	*gksm	= mf->gksm;

	/* GKSM data-structure is allocated. */

	if ((gksm->fp = fopen(conn, "r")) != NULL) {
	    FILE	*fp	= gksm->fp;
	    int		i;

	    gksm->type			= MF_GKSM;
	    gksm->filestat		= METAFILE_OK;
	    gksm->GksmEmpty		= 0;
	    gksm->CurItem.type		= INVALID;
	    gksm->CurItem.length	= INVALID;

	    for (i = 0; i < 4; i++) {
		(void) READCHR(fp, gksm->std[i]);
		if ((ferror(fp)) || (gksm->std[i] == '\n'))
		    break;
	    }

	    if (i == 4) {
		gksm->std[4] = 0;

		for (i = 0; i < 40; i++) {
		    (void) READCHR(fp, gksm->info[i]);
		    if ((ferror(fp)) || (gksm->info[i] == '\n'))
			break;
		}

		if (i == 40) {
		    gksm->info[40] = 0;

		    for (i = 0; i < 8; i++) {
			(void) READCHR(fp, gksm->date[i]);
			if ((ferror(fp)) || (gksm->date[i] == '\n'))
			    break;
		    }

		    if (i == 8) {
			gksm->date[8] = 0;
			if (READHINT(fp, gksm->ver) == 1 && !XgksFeoln(fp) &&
			    READHINT(fp, gksm->h)   == 1 && !XgksFeoln(fp) &&
			    READHINT(fp, gksm->t)   == 1 && !XgksFeoln(fp) &&
			    READHINT(fp, gksm->l)   == 1 && !XgksFeoln(fp) &&
			    READHINT(fp, gksm->i)   == 1 && !XgksFeoln(fp) &&
			    READHINT(fp, gksm->r)   == 1 && !XgksFeoln(fp) &&
			    READHINT(fp, gksm->f)   == 1 && !XgksFeoln(fp) &&
			    READHINT(fp, gksm->ri)  == 1) {

			    for (i = 0; i < 11; i++) {
				if (XgksFeoln(fp))
				    break;
				(void) READCHR(fp, gksm->d1[i]);
				if ((ferror(fp)) || (gksm->d1[i] == '\n'))
				    break;
			    }

			    if (i == 11) {
				gksm->d1[11] = 0;
				for (i = 0; i < 11; i++) {
				    (void) READCHR(fp, gksm->d2[i]);
				    if ((ferror(fp)) || (gksm->std[i] == '\n'))
					break;
				}

				if (i == 11) {
				    gksm->d2[11] = 0;
				    status	= OK;
				}
			    }
			}
		    }
		}
	    }
	}

	if (status != OK)
	    (void)GMmiClose(mf);
    }

    return status;
}


/*
 * Close an input GKSM.
 */
    int
GMmiClose(mf)
    Metafile	*mf;
{
    int		status	= 1;		/* return status = error */

    if (mf != NULL && mf->gksm != NULL) {
	if (mf->gksm->fp != NULL)
	    if (!ferror(mf->gksm->fp) & fclose(mf->gksm->fp) != EOF)
		status	= OK;
	ufree((voidp)mf->gksm);
	mf->gksm	= NULL;
    }

    return status;
}


/*
 * Open an output GKSM.
 */
    int
GMmoOpen(ws)
    WS_STATE_PTR	ws;
{
    int		status	= 1;	/* return status = error */

    assert(ws != NULL);

    if ((ws->mf.gksm = (mf_gksm*)umalloc(sizeof(mf_gksm))) != NULL) {
	Gfile		*fp	= fopen(ws->conn, "w");

	if (fp != NULL) {
	    ws->mf.gksm->fp	= fp;
	    ws->mf.gksm->type	= MF_GKSM;

	    (void) fprintf(fp, "%-4.4s%-40.40s%-8.8s", 
			   "GKSM", XgksMAuthor(), XgksMDate());
	    (void) fprintf(fp,
			   "%2d%2d%2d%2d%2d%2d%2d%2d",
			   gksm_version,
			   GKSM_LENGTH,
			   ITEM_TYPE_LENGTH,
			   ITEM_DREC_LENGTH,
			   INT_FIELD_LENGTH,
			   FLOAT_FIELD_LENGTH,
			   OUTPUT_FMT_TYPE,
			   NUMBER_REP);
	    (void) fprintf(fp, "%s%s\n", dummy, dummy);

	    if (!ferror(fp))
		status	= OK;
	}

	if (status != OK)
	    (void) GMmoClose(&ws->mf);
    }

    return status;
}


/*
 * Close an output GKSM.
 */
    int
GMmoClose(mf)
    Metafile	*mf;
{
    int		status	= 1;		/* return status = error */

    if (mf != NULL && mf->gksm != NULL) {
	if (mf->gksm->fp != NULL)
	    if (!ferror(mf->gksm->fp) & fclose(mf->gksm->fp) != EOF)
		status	= OK;
	ufree((voidp)mf->gksm);
	mf->gksm	= NULL;
    }

    return status;
}


/*
 * Set the clear flag in an output GKSM.
 */
    int
GMclear(mf, num, flag)
    Metafile	*mf;
    int		num;
    Gclrflag	flag;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s\n", ITEM_TYPE_FMT, ITEM_LENGTH_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       1,
		       GMrecSize(1),
		       (flag == GCONDITIONALLY ? 0 : 1));
    }

    return OK;
}


/*
 * Redraw all segments in an output GKSM.
 */
    int
GMredrawAllSeg(mf, num)
    Metafile	*mf;
    int		num;
{
    int		imf;

    (void) sprintf(fmt, "%s%s\n", ITEM_TYPE_FMT, ITEM_LENGTH_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, 2, GMrecSize(2));
    }

    return OK;
}


/*
 * Set the update flag in an output GKSM.
 */
     int
GMupdate(mf, num, regenflag)
    Metafile	*mf;
    int		num;
    Gregen	regenflag;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s\n", ITEM_TYPE_FMT, ITEM_LENGTH_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       3,
		       GMrecSize(3),
		       (regenflag == GPERFORM ? 0 : 1));
    }

    return OK;
}


/*
 * Set the deferal state in an output GKSM.
 */
    int
GMdefer(mf, num, defer_mode, regen_mode)
    Metafile	*mf;
    int		num;
    Gdefmode	defer_mode;
    Girgmode	regen_mode;
{
    int		imf;
    Gint	defer, regen;

    if (defer_mode == GASAP)
	defer = 0;
    else if (defer_mode == GBNIG)
	defer = 1;
    else if (defer_mode == GBNIL)
	defer = 2;
    else
	defer = 3;

    if (regen_mode == GSUPPRESSED)
	regen = 0;
    else
	regen = 1;

    (void) sprintf(fmt, "%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, 4, GMrecSize(4), defer, regen);
    }

    return OK;
}


/*
 * Write a message to an output GKSM.
 */
    int
GMmessage(mf, num, string)
    Metafile	*mf;
    int		num;
    Gchar	*string;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       5,
		       STRLEN(string) + 1 + GMrecSize(5),
		       STRLEN(string),
		       string);
    }

    return OK;
}


/*
 * Write a graphic to output GKSMs.
 *
 * This routine is suitable for
 *
 *	POLYLINE    -- code == 11
 *	POLYMARKER  -- code == 12
 *	FILLAREA    -- code == 14
 */
    int
GMoutputGraphic(mf, num, code, num_pt, pos)
    Metafile	*mf;
    int		num;
    Gint	code;
    Gint	num_pt;
    Gpoint	*pos;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       code,
		       (num_pt * 2 * FLOAT_FIELD_LENGTH) + GMrecSize(code),
		       num_pt);
    }

    (void) sprintf(fmt, "%s%s", FLOAT_FMT, FLOAT_FMT);

    for (imf = 0; imf < num; ++imf) {
	Gint	i;
	FILE	*fp	= mf[imf].gksm->fp;

	for (i = 0; i < num_pt; i++) {
	    (void) fprintf(fp, fmt, pos->x, pos->y);
	    pos++;
	}
	(void) fprintf(fp, "\n");
    }

    return OK;
}


/*
 * Write text to an output GKSM.
 */
    int
GMtext(mf, num, at, string)
    Metafile	*mf;
    int		num;
    Gpoint	*at;
    Gchar	*string;
{
    int		imf;
    Gint	length;

    length = STRLEN(string);
    (void) sprintf(fmt, "%s%s%s%s%s%%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       13,
		       length + GMrecSize(13),
		       at->x,
		       at->y,
		       length,
		       string);
    }

    return OK;
}


/*
 * Write a cell array to an output GKSM.
 */
    int
GMcellArray(mf, num, ll, ur, lr, row, colour, dim)
    Metafile	*mf;
    int		num;
    Gpoint	*ll, *ur, *lr;
    Gint	row, *colour;
    Gipoint	*dim;
{
    int		imf;
    Gint	i, j, size;

    size = dim->x * dim->y;
    (void) sprintf(fmt, "%s%s%s%s%s%s%s%s%s%s",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       15,
		       GMrecSize(15) + (size * INT_SIZE),
		       ll->x,
		       ll->y,
		       ur->x,
		       ur->y,
		       lr->x,
		       lr->y,
		       dim->x,
		       dim->y);
	for (i = 0; i < dim->y; i++)
	    for (j = 0; j < row; j++)
		if (j < dim->x)
		    (void) fprintf(fp, INT_FMT, colour[i * row + j]);
	(void) fprintf(fp, "\n");
    }

    return OK;
}


/*
 * Set the size of graphics in an output GKSM.
 */
    int
GMsetGraphSize(mf, num, code, size)
    Metafile	*mf;
    int		num;
    Gint	code;
    double	size;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   FLOAT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, code, GMrecSize(code), size);
    }

    return OK;
}


/*
 * Close a segment in an output GKSM.
 */
    int
GMcloseSeg(mf, num)
    Metafile	*mf;
    int		num;
{
    int		imf;

    (void) sprintf(fmt, "%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, 82, GMrecSize(82));
    }

    return OK;
}


/*
 * Set the graphic attributes in an output GKSM.
 */
    int
GMsetGraphAttr(mf, num, code, attr)
    Metafile	*mf;
    int		num;
    Gint	code;
    Gint	attr;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, code, GMrecSize(code), attr);
    }

    return OK;
}


/*
 * Set the font precision in an output GKSM.
 */
    int
GMsetTextFP(mf, num, txfp)
    Metafile	*mf;
    int		num;
    Gtxfp	*txfp;
{
    int		imf;
    Gint	prec;

    if (txfp->prec == GSTRING)
	prec = 0;
    else if (txfp->prec == GCHAR)
	prec = 1;
    else
	prec = 2;

    (void) sprintf(fmt, "%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, 30, GMrecSize(30), txfp->font, prec);
    }

    return OK;
}


/*
 * Set the character up-vector in an output GKSM.
 */
    int
GMsetCharUp(mf, num, up, base)
    Metafile	*mf;
    int		num;
    Gpoint	*up, *base;
{
    int		imf;
    Gpoint	ndc_up, ndc_base;

    if (up == NULL) {
	XgksComputeVec(&ndc_up, &ndc_base);
    } else {
	ndc_up		= *up;
	ndc_base	= *base;
    }
    (void) sprintf(fmt, "%s%s%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       34,
		       GMrecSize(34),
		       ndc_up.x,
		       ndc_up.y,
		       ndc_base.x,
		       ndc_base.y);
    }

    return OK;
}


/*
 * Set the text-path in an output GKSM.
 */
    int
GMsetTextPath(mf, num, path)
    Metafile	*mf;
    int		num;
    Gtxpath	path;
{
    int		imf;
    Gint	txpath;

    if (path == GTP_RIGHT)
	txpath = 0;
    else if (path == GTP_LEFT)
	txpath = 1;
    else if (path == GTP_UP)
	txpath = 2;
    else
	txpath = 3;

    (void) sprintf(fmt, "%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, 35, GMrecSize(35), txpath);
    }

    return OK;
}


/*
 * Set the text-alignment in an output GKSM.
 */
    int
GMsetTextAlign(mf, num, align)
    Metafile	*mf;
    int		num;
    Gtxalign	*align;
{
    int		imf;
    Gint	hor, ver;

    if (align->hor == GTH_NORMAL)
	hor = 0;
    else if (align->hor == GTH_LEFT)
	hor = 1;
    else if (align->hor == GTH_CENTRE)
	hor = 2;
    else
	hor = 3;

    if (align->ver == GTV_NORMAL)
	ver = 0;
    else if (align->ver == GTV_TOP)
	ver = 1;
    else if (align->ver == GTV_CAP)
	ver = 2;
    else if (align->ver == GTV_HALF)
	ver = 3;
    else if (align->ver == GTV_BASE)
	ver = 4;
    else
	ver = 5;

    (void) sprintf(fmt, "%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	/*
	 * Fix silly bug in GKSM->PostScript translator in SunGKS 3.0 that
	 * causes it to mishandle text when there are changes to the Text
	 * Alignment in a metacode input file.  This patch forces a line in the
	 * metacode file to set the text alignment to normal before changing it
	 * to something else.  Thanks to Harry Edmond
	 * <harry@atmos.washington.edu> for the fix.
	 */
	if (hor != 0 || ver != 0)
	    (void) fprintf(fp, fmt, 36, GMrecSize(36), 0, 0);

	(void) fprintf(fp, fmt, 36, GMrecSize(36), hor, ver);
    }

    return OK;
}


/*
 * Set the interior fill-style in an output GKSM.
 */
    int
GMsetFillStyle(mf, num, style)
    Metafile	*mf;
    int		num;
    Gflinter	style;
{
    int		imf;
    Gint	intstyle;

    if (style == GHOLLOW)
	intstyle = 0;
    else if (style == GSOLID)
	intstyle = 1;
    else if (style == GPATTERN)
	intstyle = 2;
    else
	intstyle = 3;

    (void) sprintf(fmt, "%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, 38, GMrecSize(38), intstyle);
    }

    return OK;
}


/*
 * Set the pattern size in an output GKSM.
 */
    int
GMsetPatSize(mf, num)
    Metafile	*mf;
    int		num;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, 41, GMrecSize(41),
		       xgks_state.gks_ptattr.widthvec.x,
		       xgks_state.gks_ptattr.widthvec.y,
		       xgks_state.gks_ptattr.heightvec.x,
		       xgks_state.gks_ptattr.heightvec.y);
    }

    return OK;
}


/*
 * Set the pattern reference-point in an output GKSM.
 */
    int
GMsetPatRefpt(mf, num)
    Metafile	*mf;
    int		num;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, 42, GMrecSize(42),
		       xgks_state.gks_ptattr.ptp.x,
		       xgks_state.gks_ptattr.ptp.y);
    }

    return OK;
}


/*
 * Set the ASF in an output GKSM.
 */
    int
GMsetAsf(mf, num)
    Metafile	*mf;
    int		num;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       43,
		       GMrecSize(43),
		       (xgks_state.gks_lnattr.type   == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_lnattr.width  == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_lnattr.colour == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_mkattr.type   == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_mkattr.size   == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_mkattr.colour == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_txattr.fp     == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_txattr.tx_exp == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_txattr.space  == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_txattr.colour == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_flattr.inter  == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_flattr.style  == GBUNDLED ? 0 : 1),
		       (xgks_state.gks_flattr.colour == GBUNDLED ? 0 : 1));
    }

    return OK;
}


/*
 * Set the line and marker representation in an output GKSM.
 */
    int
GMsetLineMarkRep(mf, num, code, idx, type, size, colour)
    Metafile	*mf;
    int		num;
    Gint	code, idx, type;
    double	size;
    Gint        colour;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT,
		   FLOAT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       code,
		       GMrecSize(code),
		       idx,
		       type,
		       size,
		       colour);
    }

    return OK;
}


/*
 * Set the text representation in an output GKSM.
 */
    int
GMsetTextRep(mf, num, idx, rep)
    Metafile	*mf;
    int		num;
    Gint	idx;
    Gtxbundl	*rep;
{
    int		imf;
    Gint	prec;

    if (rep->fp.prec == GSTRING)
	prec = 0;
    else if (rep->fp.prec == GCHAR)
	prec = 1;
    else
	prec = 2;

    (void) sprintf(fmt, "%s%s%s%s%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp, fmt, 53, GMrecSize(53), idx,
		       rep->fp.font, prec, rep->ch_exp, rep->space, 
		       rep->colour);
    }

    return OK;
}


/*
 * Set the fill representation in an output GKSM.  
 */
    int
GMsetFillRep(mf, num, idx, rep)
    Metafile	*mf;
    int		num;
    Gint	idx;
    Gflbundl	*rep;
{
    int		imf;
    Gint	intstyle;

    if (rep->inter == GHOLLOW)
	intstyle = 0;
    else if (rep->inter == GSOLID)
	intstyle = 1;
    else if (rep->inter == GPATTERN)
	intstyle = 2;
    else
	intstyle = 3;

    (void) sprintf(fmt, "%s%s%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       54,
		       GMrecSize(54),
		       idx,
		       intstyle,
		       rep->style,
		       rep->colour);
    }

    return OK;
}


/*
 * Set the pattern representation in an output GKSM.
 */
    int
GMsetPatRep(mf, num, idx, rep)
    Metafile	*mf;
    int		num;
    Gint	idx;
    Gptbundl	*rep;
{
    int		imf;
    Gint	size, i;

    size = rep->size.x * rep->size.y;

    (void) sprintf(fmt, "%s%s%s%s%s",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       55,
		       (size * INT_FIELD_LENGTH) + GMrecSize(55),
		       idx,
		       rep->size.x,
		       rep->size.y);

	for (i = 0; i < size; i++)
	    (void) fprintf(fp, INT_FMT, rep->array[i]);

	(void) fprintf(fp, "\n");
    }

    return OK;
}


/*
 * Set the colour representation in an output GKSM.
 */
    int
GMsetColRep(mf, num, idx, rep)
    Metafile	*mf;
    int		num;
    Gint	idx;
    Gcobundl	*rep;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       56,
		       GMrecSize(56),
		       idx,
		       rep->red,
		       rep->green,
		       rep->blue);
    }

    return OK;
}


/*
 * Set the clipping rectangle in an output GKSM.
 */
    int
GMsetClip(mf, num, rect)
    Metafile	*mf;
    int		num;
    Glimit	*rect;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       61,
		       GMrecSize(61),
		       rect->xmin,
		       rect->xmax,
		       rect->ymin,
		       rect->ymax);
    }

    return OK;
}


/*
 * Set the viewport limits in an output GKSM.
 */
    int
GMsetLimit(mf, num, code, rect)
    Metafile	*mf;
    int		num;
    Gint	code;
    Glimit	*rect;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       code,
		       GMrecSize(code),
		       rect->xmin,
		       rect->xmax,
		       rect->ymin,
		       rect->ymax);
    }

    return OK;
}


/*
 * Rename a segment in an output GKSM.
 */
    int
GMrenameSeg(mf, num, old, new)
    Metafile	*mf;
    int		num;
    Gint	old, new;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       83,
		       GMrecSize(83),
		       old,
		       new);
    }

    return OK;
}


/*
 * Set the segment transformation in an output GKSM.
 */
    int
GMsetSegTran(mf, num, name, matrix)
    Metafile	*mf;
    int		num;
    Gint	name;
    Gfloat	matrix[2][3];
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s%s%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT,
		   FLOAT_FMT);
    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       91,
		       GMrecSize(91),
		       name,
		       matrix[0][0],
		       matrix[0][1],
		       matrix[0][2],
		       matrix[1][0],
		       matrix[1][1],
		       matrix[1][2]);
    }

    return OK;
}


/*
 * Set the segment attributes in an output GKSM.
 */
    int
GMsetSegAttr(mf, num, name, code, attr)
    Metafile	*mf;
    int		num;
    Gint	name, code, attr;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       code,
		       GMrecSize(code),
		       name,
		       attr);
    }

    return OK;
}


/*
 * Set the segment visibility in an output Metafile.
 */
    int
GMsetSegVis(mf, num, name, vis)
    Metafile	*mf;
    int		num;
    Gint	name;
    Gsegvis	vis;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       92,
		       GMrecSize(92),
		       name,
		       (vis == GVISIBLE ? 0 : 1));
    }

    return OK;
}


/*
 * Set segment highlighting in an output GKSM.
 */
    int
GMsetSegHilight(mf, num, name, hilight)
    Metafile	*mf;
    int		num;
    Gint	name;
    Gseghi	hilight;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       93,
		       GMrecSize(93),
		       name,
		       (hilight == GNORMAL ? 0 : 1));
    }

    return OK;
}


/*
 * Set segment priority in an output GKSM.
 */
    int
GMsetSegPri(mf, num, name, pri)
    Metafile	*mf;
    int		num;
    Gint	name;
    double	pri;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   FLOAT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       94,
		       GMrecSize(94),
		       name,
		       pri);
    }

    return OK;
}


/*
 * Set segment detectability in an output GKSM.
 */
    int
GMsetSegDetect(mf, num, name, det)
    Metafile	*mf;
    int		num;
    Gint	name;
    Gsegdet	det;
{
    int		imf;

    (void) sprintf(fmt, "%s%s%s%s\n",
		   ITEM_TYPE_FMT,
		   ITEM_LENGTH_FMT,
		   INT_FMT,
		   INT_FMT);

    for (imf = 0; imf < num; ++imf) {
	FILE	*fp	= mf[imf].gksm->fp;

	(void) fprintf(fp,
		       fmt,
		       95,
		       GMrecSize(95),
		       name,
		       (det == GUNDETECTABLE ? 0 : 1));
    }

    return OK;
}
