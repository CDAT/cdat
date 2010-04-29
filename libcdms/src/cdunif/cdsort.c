/* -*-Mode: C;-*-
 * Module:      CuSort
 *
 * Copyright:	1995, Regents of the University of California
 *		This software may not be distributed to others without
 *		permission of the author.
 *
 * Author:      See below
 *
 * Version:     $Id$
 *
 * Revision History:
 *
 * $Log: cdsort.c,v $
 * Revision 1.1  1995/07/31  17:38:02  drach
 * - Initial version
 *
 *
 */
/* Sort an array ix of longs, and optionally (kflag>0) make the same interchanges */
/* in an auxiliary array iy.  The array may be sorted in increasing (kflag=2) */
/* or decreasing (kflag=1) order. If kflag is negative, array iy is ignored: */
/* kflag == -2 --> sort ix in increasing order, and ignore iy; */
/* kflag == -1 --> sort ix in decreasing order, and ignore iy; */
/* A slightly modified QUICKSORT algorithm is used. */

/* Note: This routine was derived from the SLATEC FORTRAN isort routine, */
/* via the f2c converter. */


void CuSort(long ix[], long iy[], long n, int kflag)
{
    /* System generated locals */
    long i__1;

    /* Local variables */
    long i, j, k, l, m;
    float r;
    long t, ij, il[21], kk, nn, iu[21], tt, ty;
    long tty;

/* ***BEGIN PROLOGUE  ISORT */
/* ***PURPOSE  Sort an array and optionally make the same interchanges in 
*/
/*            an auxiliary array.  The array may be sorted in increasing 
*/
/*            or decreasing order.  A slightly modified QUICKSORT */
/*            algorithm is used. */
/* ***LIBRARY   SLATEC */
/* ***CATEGORY  N6A2A */
/* ***TYPE      INTEGER (SSORT-S, DSORT-D, ISORT-I) */
/* ***KEYWORDS  SINGLETON QUICKSORT, SORT, SORTING */
/* ***AUTHOR  Jones, R. E., (SNLA) */
/*           Kahaner, D. K., (NBS) */
/*           Wisniewski, J. A., (SNLA) */
/* ***DESCRIPTION */

/*   ISORT sorts array IX and optionally makes the same interchanges in */
/*   array IY.  The array IX may be sorted in increasing order or */
/*   decreasing order.  A slightly modified quicksort algorithm is used. 
*/

/*   Description of Parameters */
/*      IX - long array of values to be sorted */
/*      IY - long array to be (optionally) carried along */
/*      N  - number of values in long array IX to be sorted */
/*      KFLAG - control parameter */
/*            =  2  means sort IX in increasing order and carry IY along. 
*/
/*            =  1  means sort IX in increasing order (ignoring IY) */
/*            = -1  means sort IX in decreasing order (ignoring IY) */
/*            = -2  means sort IX in decreasing order and carry IY along. 
*/

/* ***REFERENCES  R. C. Singleton, Algorithm 347, An efficient algorithm 
*/
/*                 for sorting with minimal storage, Communications of */
/*                 the ACM, 12, 3 (1969), pp. 185-187. */
/* ***ROUTINES CALLED  XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   761118  DATE WRITTEN */
/*   810801  Modified by David K. Kahaner. */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891009  Removed unreferenced statement labels.  (WRB) */
/*   891009  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   901012  Declared all variables; changed X,Y to IX,IY. (M. McClain) */
/*   920501  Reformatted the REFERENCES section.  (DWL, WRB) */
/*   920519  Clarified error messages.  (DWL) */
/*   920801  Declarations section rebuilt and code restructured to use */
/*           IF-THEN-ELSE-ENDIF.  (RWC, WRB) */
/* ***END PROLOGUE  ISORT */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. Local Scalars .. */
/*     .. Local Arrays .. */
/*     .. External Subroutines .. */
/*     .. Intrinsic Functions .. */
/* ***FIRST EXECUTABLE STATEMENT  ISORT */
    /* Parameter adjustments */
    --iy;
    --ix;

    /* Function Body */
    nn = n;

    kk = abs(kflag);

/*     Alter array IX to get decreasing order if needed */

    if (kflag <= -1) {
	i__1 = nn;
	for (i = 1; i <= i__1; ++i) {
	    ix[i] = -ix[i];
/* L10: */
	}
    }

    if (kk == 2) {
	goto L100;
    }

/*     Sort IX only */

    m = 1;
    i = 1;
    j = nn;
    r = (float).375;

L20:
    if (i == j) {
	goto L60;
    }
    if (r <= (float).5898437) {
	r += (float).0390625;
    } else {
	r += (float)-.21875;
    }

L30:
    k = i;

/*     Select a central element of the array and save it in location T */

    ij = i + (long) ((j - i) * r);
    t = ix[ij];

/*     If first element of array is greater than T, interchange with T */

    if (ix[i] > t) {
	ix[ij] = ix[i];
	ix[i] = t;
	t = ix[ij];
    }
    l = j;

/*     If last element of array is less than than T, interchange with T */

    if (ix[j] < t) {
	ix[ij] = ix[j];
	ix[j] = t;
	t = ix[ij];

/*        If first element of array is greater than T, interchange wit
h T */

	if (ix[i] > t) {
	    ix[ij] = ix[i];
	    ix[i] = t;
	    t = ix[ij];
	}
    }

/*     Find an element in the second half of the array which is smaller */
/*     than T */

L40:
    --l;
    if (ix[l] > t) {
	goto L40;
    }

/*     Find an element in the first half of the array which is greater */
/*     than T */

L50:
    ++k;
    if (ix[k] < t) {
	goto L50;
    }

/*     Interchange these elements */

    if (k <= l) {
	tt = ix[l];
	ix[l] = ix[k];
	ix[k] = tt;
	goto L40;
    }

/*     Save upper and lower subscripts of the array yet to be sorted */

    if (l - i > j - k) {
	il[m - 1] = i;
	iu[m - 1] = l;
	i = k;
	++m;
    } else {
	il[m - 1] = k;
	iu[m - 1] = j;
	j = l;
	++m;
    }
    goto L70;

/*     Begin again on another portion of the unsorted array */

L60:
    --m;
    if (m == 0) {
	goto L190;
    }
    i = il[m - 1];
    j = iu[m - 1];

L70:
    if (j - i >= 1) {
	goto L30;
    }
    if (i == 1) {
	goto L20;
    }
    --i;

L80:
    ++i;
    if (i == j) {
	goto L60;
    }
    t = ix[i + 1];
    if (ix[i] <= t) {
	goto L80;
    }
    k = i;

L90:
    ix[k + 1] = ix[k];
    --k;
    if (t < ix[k]) {
	goto L90;
    }
    ix[k + 1] = t;
    goto L80;

/*     Sort IX and carry IY along */

L100:
    m = 1;
    i = 1;
    j = nn;
    r = (float).375;

L110:
    if (i == j) {
	goto L150;
    }
    if (r <= (float).5898437) {
	r += (float).0390625;
    } else {
	r += (float)-.21875;
    }

L120:
    k = i;

/*     Select a central element of the array and save it in location T */

    ij = i + (long) ((j - i) * r);
    t = ix[ij];
    ty = iy[ij];

/*     If first element of array is greater than T, interchange with T */

    if (ix[i] > t) {
	ix[ij] = ix[i];
	ix[i] = t;
	t = ix[ij];
	iy[ij] = iy[i];
	iy[i] = ty;
	ty = iy[ij];
    }
    l = j;

/*     If last element of array is less than T, interchange with T */

    if (ix[j] < t) {
	ix[ij] = ix[j];
	ix[j] = t;
	t = ix[ij];
	iy[ij] = iy[j];
	iy[j] = ty;
	ty = iy[ij];

/*        If first element of array is greater than T, interchange wit
h T */

	if (ix[i] > t) {
	    ix[ij] = ix[i];
	    ix[i] = t;
	    t = ix[ij];
	    iy[ij] = iy[i];
	    iy[i] = ty;
	    ty = iy[ij];
	}
    }

/*     Find an element in the second half of the array which is smaller */
/*     than T */

L130:
    --l;
    if (ix[l] > t) {
	goto L130;
    }

/*     Find an element in the first half of the array which is greater */
/*     than T */

L140:
    ++k;
    if (ix[k] < t) {
	goto L140;
    }

/*     Interchange these elements */

    if (k <= l) {
	tt = ix[l];
	ix[l] = ix[k];
	ix[k] = tt;
	tty = iy[l];
	iy[l] = iy[k];
	iy[k] = tty;
	goto L130;
    }

/*     Save upper and lower subscripts of the array yet to be sorted */

    if (l - i > j - k) {
	il[m - 1] = i;
	iu[m - 1] = l;
	i = k;
	++m;
    } else {
	il[m - 1] = k;
	iu[m - 1] = j;
	j = l;
	++m;
    }
    goto L160;

/*     Begin again on another portion of the unsorted array */

L150:
    --m;
    if (m == 0) {
	goto L190;
    }
    i = il[m - 1];
    j = iu[m - 1];

L160:
    if (j - i >= 1) {
	goto L120;
    }
    if (i == 1) {
	goto L110;
    }
    --i;

L170:
    ++i;
    if (i == j) {
	goto L150;
    }
    t = ix[i + 1];
    ty = iy[i + 1];
    if (ix[i] <= t) {
	goto L170;
    }
    k = i;

L180:
    ix[k + 1] = ix[k];
    iy[k + 1] = iy[k];
    --k;
    if (t < ix[k]) {
	goto L180;
    }
    ix[k + 1] = t;
    iy[k + 1] = ty;
    goto L170;

/*     Clean up */

L190:
    if (kflag <= -1) {
	i__1 = nn;
	for (i = 1; i <= i__1; ++i) {
	    ix[i] = -ix[i];
/* L200: */
	}
    }
    return;
}

