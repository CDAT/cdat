/* drwwld2.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int drwwld2_(lun, x1, x2, y1, y2)
integer *lun;
real *x1, *x2, *y1, *y2;
{
    /* Format strings */
    static char fmt_1001[] = "(2i8,4f8.3)";
    static char fmt_1002[] = "(10f8.3)";
    static char fmt_1003[] = "(\002 LINE \002,i5,i3,8f9.3)";
    static char fmt_1004[] = "(\002 END OF GROUP \002,i2,\002 LINES =\002,i6)"
	    ;

    /* System generated locals */
    integer i__1, i__2;
    olist o__1;

    /* Builtin functions */
    integer f_open(), s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe();

    /* Local variables */
    static integer igid, ngrp;
    static real xlat[100], xlon[100];
    static shortint npts;
    static integer i, nline;
    static real xlatmn, xlonmn, xlatmx, xlonmx;
    static integer npr;

    /* Fortran I/O blocks */
    static cilist io___3 = { 0, 1, 1, fmt_1001, 0 };
    static cilist io___11 = { 0, 1, 1, fmt_1002, 0 };
    static cilist io___15 = { 0, 6, 0, fmt_1003, 0 };
    static cilist io___16 = { 0, 6, 0, fmt_1004, 0 };


/* C    SUBROUTINE DRWWLD2(LUN,X1,X2,Y1,Y2,XS1,XS2,YS1,YS2) */
/* C */
/* C */
/* C  THIS ROUTINE DRAWS */
/* C  1) ACTUAL MODERN WORLD CONTINENTAL OUTLINES (..B1.DAT) */
/* C  2) US STATE OUTLINES (..B2.DAT) */
/* C */
/* C  INPUT:      LUN = TEMPORARY LUN TO USE FOR THE OUTLINE DATA FILE. */
/* C          [X1,X2] = LONGITUDINAL DOMAIN WITHIN WHICH TO DRAW. */
/* C          [Y1,Y1] = LATITUDINAL DOMAIN WITHIN WHICH TO DRAW. */
/*C        [XS1,XS2] = NORMALIZED LONGITUDINAL DOMAIN WITHIN WHICH TO DRAW
.*/
/*C        [YS1,YS2] = NORMALIZED LATITUDINAL DOMAIN WITHIN WHICH TO DRAW.
*/
/* C             FNAM = THE DATA FILE WITH THE X,Y LOCATIONS */
/* C */

/* VARIABLE DEFINITIONS */
/*     NPTS = TOTAL NUMBER OF POINTS IN NEXT LINE (NPTS/2 LAT-LON PAIRS). 
*/
/*            IF NPTS < 2 THIS RECORD IS A GROUP SEPARATOR AND NO POINT */
/*            PAIRS FOLLOW. */
/*     IGID = GROUP ID FOR THESE POINTS WHERE */
/*             1 = CONTINENTAL OUTLINES */
/*             2 = US STATE BOUNDARIES (HIGHER RESOLUTION THAN 1) */
/*             3 = INTERNATIONAL POLITICAL BOUNDARIES */

/*     XLATMX,XLATMN,XLONMX,XLONMN  = THE LAT-LON LIMITS FOR POINTS IN */
/*               THE FOLLOWING LINE. */

/*     XLAT,XLON ARE THE COORDINATES OF A CONTINUOUS LINE IN THE OUTLINE 
*/
/*       CONTAINING NPTS/2 POINTS. */

/* TAPE STRUCTURE */
/*     THE TAPE IS DIVIDED INTO FOUR GROUPS, SEPARATED BY LINE HEADERS */
/*     WITH POINT COUNTS LESS THAN 2. */
/*     GROUP     CONTENT */
/*      1        CONTINENTAL OUTLINES ONLY. */
/*      2        US STATE OULINES ONLY (HIGHER RESOLUTION THAN 1). */
/*      3        CONTINENTAL AND US STATE AND INTERNATIONAL POLITICAL */
/*               BOUNDARIES (USES 2 WHERE IT OVERLAPS 1). */
/*      4        CONTINENTAL AND INTERNATIONAL POLITICAL BOUNDARIES. */
    ngrp = 0;

/*C      OPEN(1,FILE='/data1/vcs_legacy_release2_5/vcs_legacy_script/data',STATUS='OLD')
*/
    o__1.oerr = 0;
    o__1.ounit = *lun;
    o__1.ofnmlen = 37;
    o__1.ofnm = "/data1/vcs_legacy_release2_5/vcs_legacy_script/data";
    o__1.orl = 0;
    o__1.osta = "OLD";
    o__1.oacc = 0;
    o__1.ofm = "UNFORMATTED";
    o__1.oblnk = 0;
    f_open(&o__1);
L5:
    nline = 0;
    ++ngrp;
/* READ LINE HEADER */
L10:
    i__1 = s_rsfe(&io___3);
    if (i__1 != 0) {
	goto L90;
    }
    i__1 = do_fio(&c__1, (char *)&npts, (ftnlen)sizeof(shortint));
    if (i__1 != 0) {
	goto L90;
    }
    i__1 = do_fio(&c__1, (char *)&igid, (ftnlen)sizeof(integer));
    if (i__1 != 0) {
	goto L90;
    }
    i__1 = do_fio(&c__1, (char *)&xlatmx, (ftnlen)sizeof(real));
    if (i__1 != 0) {
	goto L90;
    }
    i__1 = do_fio(&c__1, (char *)&xlatmn, (ftnlen)sizeof(real));
    if (i__1 != 0) {
	goto L90;
    }
    i__1 = do_fio(&c__1, (char *)&xlonmx, (ftnlen)sizeof(real));
    if (i__1 != 0) {
	goto L90;
    }
    i__1 = do_fio(&c__1, (char *)&xlonmn, (ftnlen)sizeof(real));
    if (i__1 != 0) {
	goto L90;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L90;
    }
    if (npts < 2) {
	goto L90;
    }
    npr = (npts + 1) / 2;
/* READ LINE */
    i__1 = s_rsfe(&io___11);
    if (i__1 != 0) {
	goto L90;
    }
    i__2 = npr;
    for (i = 1; i <= i__2; ++i) {
	i__1 = do_fio(&c__1, (char *)&xlat[i - 1], (ftnlen)sizeof(real));
	if (i__1 != 0) {
	    goto L90;
	}
	i__1 = do_fio(&c__1, (char *)&xlon[i - 1], (ftnlen)sizeof(real));
	if (i__1 != 0) {
	    goto L90;
	}
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L90;
    }
    ++nline;
    if (nline > 10) {
	goto L10;
    }
    s_wsfe(&io___15);
    do_fio(&c__1, (char *)&npts, (ftnlen)sizeof(shortint));
    do_fio(&c__1, (char *)&igid, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&xlatmx, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&xlatmn, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&xlonmx, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&xlonmn, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&xlat[0], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&xlon[0], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&xlat[npr - 1], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&xlon[npr - 1], (ftnlen)sizeof(real));
    e_wsfe();
    goto L10;
L90:
    s_wsfe(&io___16);
    do_fio(&c__1, (char *)&ngrp, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nline, (ftnlen)sizeof(integer));
    e_wsfe();
    if (nline > 0) {
	goto L5;
    }
} /* drwwld2_ */

