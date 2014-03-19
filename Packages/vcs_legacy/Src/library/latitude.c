/* latitude.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int gauaw_(pa, pw, k)
doublereal *pa, *pw;
integer *k;
{
    /* Initialized data */

    static doublereal eps = 1e-14;

    /* Format strings */
    static char fmt_9901[] = "(//,\002  GAUAW FAILED TO CONVERGE AFTER 10 IT\
ERATIONS.\002)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(), cos();
    integer s_wsfe(), e_wsfe();
    /* Subroutine * int s_stop();*/

    /* Local variables */
    static integer iter;
    static doublereal avsp;
    static integer nout;
    static doublereal c;
    static integer l, n;
    static doublereal pkmrk, fk, fn;
    static integer kk;
    static doublereal pk;
    static integer is;
    static doublereal sp, xz;
    extern /* Subroutine */ int bsslzr_();
    static doublereal pkm1, pkm2;

    /* Fortran I/O blocks */
    static cilist io___18 = { 0, 0, 0, fmt_9901, 0 };



/* **** *GAUAW* - COMPUTE ABSCISSAS AND WEIGHTS FOR *GAUSSIAN INTEGRATION.
 */

/*     PURPOSE. */
/*     -------- */

/*          *GAUAW* IS CALLED TO COMPUTE THE ABSCISSAS AND WEIGHTS REQUIR 
*/
/*     TO PERFORM *GAUSSIAN INTEGRATION. */

/* **   INTERFACE. */
/*     ---------- */

/*          *CALL* *GAUAW(PA,PW,K)* */

/*               *PA*     - ARRAY, LENGTH AT LEAST *K,* TO RECEIVE ABSCIS 
*/
/*                          ABSCISSAS. */
/*               *PW*     - ARRAY, LENGTH AT LEAST *K,* TO RECEIVE */
/*                          WEIGHTS. */

/*     METHOD. */
/*     ------- */

/*          THE ZEROS OF THE *BESSEL FUNCTIONS ARE USED AS STARTING */
/*     APPROXIMATIONS FOR THE ABSCISSAS. NEWTON ITERATION IS USED TO */
/*     IMPROVE THE VALUES TO WITHIN A TOLLERENCE OF *EPS.* */

/*     EXTERNAL. */
/*     --------- */

/*          *BSSLZR* - ROUTINE TO OBTAIN ZEROS OF *BESSEL FUNCTIONS. */

/*     REFERENCE. */
/*     ---------- */

    /* Parameter adjustments */
    --pw;
    --pa;

    /* Function Body */

/*     ------------------------------------------------------------------ 
*/

/* *         1.     SET CONSTANTS AND FIND ZEROS OF BESSEL FUNCTION. */
/*                 --- --------- --- ---- ----- -- ------ --------- */

/* L100: */
    c = (float).148678816357662;
    fk = (doublereal) (*k);
    kk = *k / 2;
    bsslzr_(&pa[1], &kk);

    i__1 = kk;
    for (is = 1; is <= i__1; ++is) {
/* Computing 2nd power */
	d__1 = fk + (float).5;
	xz = cos(pa[is] / sqrt(d__1 * d__1 + c));
/* *                 GIVING THE FIRST APPROXIMATION FOR *XZ.* */
	iter = 0;

/*     --------------------------------------------------------------
---- */

/* *         2.     COMPUTE ABSCISSAS AND WEIGHTS. */
/*                 ------- --------- --- ------- */

/* L200: */

/* *         2.1     SET VALUES FOR NEXT ITERATION. */
L210:
	pkm2 = (float)1.;
	pkm1 = xz;
	++iter;
	if (iter > 10) {
	    goto L300;
	}

/* *         2.2     COMPUTATION OF THE *LEGENDRE POLYNOMIAL. */
/* L220: */

	i__2 = *k;
	for (n = 2; n <= i__2; ++n) {
	    fn = (doublereal) n;
	    pk = ((fn * (float)2. - (float)1.) * xz * pkm1 - (fn - (float)1.) 
		    * pkm2) / fn;
	    pkm2 = pkm1;
	    pkm1 = pk;
/* L222: */
	}

	pkm1 = pkm2;
/* Computing 2nd power */
	d__1 = xz;
	pkmrk = fk * (pkm1 - xz * pk) / ((float)1. - d__1 * d__1);
	sp = pk / pkmrk;
	xz -= sp;
	avsp = abs(sp);
	if (avsp > eps) {
	    goto L210;
	}

/* *         2.3     ABSCISSAS AND WEIGHTS. */
/* L230: */
	pa[is] = xz;
/* Computing 2nd power */
	d__1 = xz;
/* Computing 2nd power */
	d__2 = fk * pkm1;
	pw[is] = ((float)1. - d__1 * d__1) * (float)2. / (d__2 * d__2);

/* *         2.4     ODD *K* COMPUTATION OF WEIGHT AT THE EQUATOR. */
/* L240: */
	if (*k != kk << 1) {
	    pa[kk + 1] = (float)0.;
/* Computing 2nd power */
	    d__1 = fk;
	    pk = (float)2. / (d__1 * d__1);

	    i__2 = *k;
	    for (n = 2; n <= i__2; n += 2) {
		fn = (doublereal) n;
/* Computing 2nd power */
		d__1 = fn;
/* Computing 2nd power */
		d__2 = fn - (float)1.;
		pk = pk * (d__1 * d__1) / (d__2 * d__2);
/* L242: */
	    }

	    pw[kk + 1] = pk;
	} else {

/* *         2.5     USE SYMMETRY TO OBTAIN REMAINING VALUES. */

/* L250: */

	    i__2 = kk;
	    for (n = 1; n <= i__2; ++n) {
		l = *k + 1 - n;
		pa[l] = -pa[n];
		pw[l] = pw[n];
/* L252: */
	    }

	}
/* L290: */
    }

    return 0;

/*     ------------------------------------------------------------------ 
*/

/* *         3.     ERROR PROCESSING. */
/*                 ----- ----------- */

L300:
    io___18.ciunit = nout;
    s_wsfe(&io___18);
    e_wsfe();
    /*s_stop("", 0L);*/
    return 0;

/*     ------------------------------------------------------------------ 
*/

} /* gauaw_ */

/* Subroutine */ int bsslzr_(pbes, knum)
doublereal *pbes;
integer *knum;
{
    /* Initialized data */

    static doublereal zbes[50] = { 2.4048255577,5.5200781103,8.6537279129,
	    11.7915344391,14.9309177086,18.0710639679,21.2116366299,
	    24.3524715308,27.493479132,30.6346064684,33.7758202136,
	    36.9170983537,40.0584257646,43.1997917132,46.3411883717,
	    49.4826098974,52.6240518411,55.765510755,58.9069839261,
	    62.0484691902,65.1899648002,68.3314693299,71.4729816036,
	    74.6145006437,77.7560256304,80.8975558711,84.0390907769,
	    87.1806298436,90.3221726372,93.4637187819,96.605267951,
	    99.7468198587,102.8883742542,106.0299309165,109.1714896498,
	    112.3130502805,115.4546126537,118.5961766309,121.737742088,
	    124.8793089132,128.0208770059,131.1624462752,134.3040166383,
	    137.4455880203,140.5871603528,143.7287335737,146.8703076258,
	    150.011882457,153.1534580192,156.2950342685 };

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double asin();

    /* Local variables */
    static integer inum, j;
    static doublereal api, zpi;


/* **** *BSSLZR* - ROUTINE TO RETURN ZEROS OF THE J0 *BESSEL FUNCTION. */

/*     PURPOSE. */
/*     -------- */

/*          *BSSLZR* RETURNS *KNUM* ZEROS, OR IF *KNUM>50,* *KNUM* */
/*     APPROXIMATE ZEROS OF THE *BESSEL FUNCTION J0. */

/* **   INTERFACE. */
/*     ---------- */

/*          *CALL* *NSSLZR(PBES,KNUM)* */

/*               *PBES*   - ARRAY, DIMENSIONED *KNUM,* TO RECEIVE THE */
/*                          VALUES. */
/*               *KNUM*   - NUMBER OF ZEROS REQUESTED. */

/*     METHOD. */
/*     ------- */

/*          THE FIRST 50 VALUES ARE OBTAINED FROM A LOOK UP TABLE. ANY */
/*     ADDITIONAL VALUES REQUESTED ARE INTERPOLATED. */

/*     EXTERNALS. */
/*     ---------- */

/*          NONE. */

/*     REFERENCE. */
/*     ---------- */

/* *CALL COMCON */
    /* Parameter adjustments */
    --pbes;

    /* Function Body */

/*     ------------------------------------------------------------------ 
*/

/* *         1.     EXTRACT VALUES FROM LOOK UP TABLE. */
/*                 ------- ------ ---- ---- -- ------ */

/*                 SET API */

    api = asin((float)1.) * (float)2.;
/* L100: */
    inum = min(*knum,50);

    i__1 = inum;
    for (j = 1; j <= i__1; ++j) {
	pbes[j] = zbes[j - 1];
/* L110: */
    }

/*     ------------------------------------------------------------------ 
*/

/* *         2.     INTERPOLATE REMAINING VALUES. */
/*                 ----------- --------- ------- */

/* L200: */

    zpi = api;
    i__1 = *knum;
    for (j = 51; j <= i__1; ++j) {
	pbes[j] = pbes[j - 1] + api;
/* L210: */
    }

/*     ------------------------------------------------------------------ 
*/

    return 0;
} /* bsslzr_ */

/* Subroutine */ int awi_(idl, il, alon, jl, alat, a, mask, idlo, ilo, alono, 
	jlo, alato, ao, masko, ier)
integer *idl, *il;
real *alon;
integer *jl;
real *alat, *a;
logical *mask;
integer *idlo, *ilo;
real *alono;
integer *jlo;
real *alato, *ao;
logical *masko;
integer *ier;
{
    /* Initialized data */

    static real api = (float)3.1415926536;

    /* System generated locals */
    integer mask_dim1, mask_offset, masko_dim1, masko_offset, a_dim1, 
	    a_offset, ao_dim1, ao_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2;

    /* Builtin functions */
    double sin();

    /* Local variables */
    static real almn;
    static integer iilo;
    static real dlno, almx, wlat, slon;
    static integer i, j, k;
    static real delon, amnln, almno, amnlt, amxln, almxo, amxlt;
    static integer i1, j1, j2, i2;
    static real slonp, al;
    static integer ii, jj;
    static real wt, amnlno, amnlto, amxlno, slatmn, amxlto, slonmn, slatmx, 
	    slonmx;
    static integer iii, iil;
    static real dln, avg, sgn;
    static integer iip;


/*          This subroutine does an area weighted average from one grid, 
*/
/*         on a spherical earth, to another.  Logical masks may be assigne
d*/
/*          for each grid, and only those grid boxes which are masked true
 */
/*          on both grids will be used.  A value of 1.E20 will be assigned
 */
/*          to all nodes of the new grid which are initially false or have
 */
/*         no data from the old grid.  The new mask will also be changed t
o*/
/*          false where no data is available. */

/*          Restrictions:  Longitude must be the first dimension and it */
/*                         be monotonically increasing (West to East). */

/*                         Latitude must be the second dimension and it */
/*                         must be monotonic. */

/*                         Values for longitude and latitude must be in */
/*                         degrees. */

/*                         Arrays that wrap around must repeat longitudes 
*/
/*                        with a 360 degree increment.  It will be assumed
*/
/*                        that values in the wrapped input and mask arrays
*/
/*                        will also repeat (wrapped values in these arrays
*/
/*                         will not be used). */

/*        INPUT */

/*   integer   IDL    First dimension of input A and MASK. */
/*   integer   IL     Number of grid boxes in longitude for A and MASK. */
/*   real      ALON   Longitude (deg) limits of grid boxes for A and MASK.
 */
/*   integer   JL     Number of grid boxes in latitude for A and MASK. */
/*   real      ALAT   Latitude (deg) limits of grid boxes for A and MASK. 
*/
/*   real      A      Array of input data. */
/*   logical   MASK   Mask for input data (.FALSE. to mask out data). */

/*        OUTPUT */

/*   integer   IDLO   First dimension of ouþþþþþþþþtput AO and MASKO. */
/*   integer   ILO    Number of grid boxes in longitude for AO and MASKO. 
*/
/*  real      ALONO  Longitude (deg) limits of grid boxes for AO and MASKO
.*/
/*   integer   JLO    Number of grid boxes in latitude for AO and MASKO. 
*/
/*  real      ALATO  Latitude (deg) limits of grid boxes for AO and MASKO.
*/
/*   real      AO     Array of output data. */
/*   logical   MASKO  Mask for output data (.FALSE. to mask out data). */
/*   integer   IER    Error indication: */
/*                    (values may be summed for multiple errors) */
/*                    0  no errors */
/*                    1  input longitude dimension and/or length <=0. */
/*                    2  output dimension and/or length <=0. */
/*                    4  input latititude dimension <=0. */
/*                    8  output latitude dimension <=0. */
/*                   16  wrap-around on input longitude grid doesn't */
/*                       repeat (+360). */
/*                   32  wrap-around on output longitude grid doesn't */
/*                       repeat (+360). */
/*                   64  longitude of input is not monotonic increasing. 
*/
/*                  128  longitude of output is not monotonic increasing. 
*/
/*                  256  latitude of input is not monotonic. */
/*                  512  latitude of output is not monotonic. */
/*                1024  input longitude wraps but doesn't repeat identical
ly.*/
/*                2048  output longitude wraps but doesn't repeat identica
lly.*/
/*                   -1  output mask is changed. */
/*                   -2  output mask contains all false values. */


    /* Parameter adjustments */
    --alon;
    mask_dim1 = *idl;
    mask_offset = mask_dim1 + 1;
    mask -= mask_offset;
    a_dim1 = *idl;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --alat;
    --alono;
    masko_dim1 = *idlo;
    masko_offset = masko_dim1 + 1;
    masko -= masko_offset;
    ao_dim1 = *idlo;
    ao_offset = ao_dim1 + 1;
    ao -= ao_offset;
    --alato;

    /* Function Body */
/*                       Check dimensions and lengths. */
    *ier = 0;
    if (*idl < *il || *il <= 0) {
	*ier = 1;
    }
    if (*idlo < *ilo || *ilo <= 0) {
	*ier += 2;
    }
    if (*jl <= 0) {
	*ier += 4;
    }
    if (*jlo <= 0) {
	*ier += 8;
    }

    if (*ier > 0) {
	return 0;
    }
/*                        Check monotonic increasing input longitudes. */
    i__1 = *il;
    for (i = 2; i <= i__1; ++i) {
	if (alon[i] <= alon[i - 1]) {
	    *ier += 64;
	    goto L2;
	}
/* L1: */
    }
L2:
/*                       Check monotonic increasing output longitudes. */
    i__1 = *ilo;
    for (i = 2; i <= i__1; ++i) {
	if (alono[i] <= alono[i - 1]) {
	    *ier += 128;
	    goto L4;
	}
/* L3: */
    }
L4:
/*                     Check monotonicity of input latitudes. */
    sgn = alat[2] - alat[1];
    i__1 = *jl;
    for (j = 2; j <= i__1; ++j) {
	if (sgn < (float)0.) {
	    if (alat[j] - alat[j - 1] >= (float)0.) {
		*ier += 256;
		goto L6;
	    }
	} else if (sgn > (float)0.) {
	    if (alat[j] - alat[j - 1] <= (float)0.) {
		*ier += 256;
		goto L6;
	    }
	} else {
	    *ier += 256;
	    goto L6;
	}
/* L5: */
    }
L6:
/*                    Check monotonicity of output latitudes. */
    sgn = alato[2] - alato[1];
    i__1 = *jlo;
    for (j = 2; j <= i__1; ++j) {
	if (sgn < (float)0.) {
	    if (alato[j] - alato[j - 1] >= (float)0.) {
		*ier += 512;
		goto L8;
	    }
	} else if (sgn > (float)0.) {
	    if (alato[j] - alato[j - 1] <= (float)0.) {
		*ier += 512;
		goto L8;
	    }
	} else {
	    *ier += 512;
	    goto L8;
	}
/* L7: */
    }
L8:
/*                       Find wrap around of input grid, if it exists. */
    iil = *il;
    almx = alon[1];
    almn = alon[1];
    i__1 = *il + 1;
    for (i = 2; i <= i__1; ++i) {
/* Computing MAX */
	r__1 = almx, r__2 = alon[i];
	almx = dmax(r__1,r__2);
/* Computing MIN */
	r__1 = almn, r__2 = alon[i];
	almn = dmin(r__1,r__2);
	al = (r__1 = alon[i] - alon[1], dabs(r__1)) - (float)360.;
	if (dabs(al) <= (float)1e-4) {
	    iil = i - 1;
	    goto L11;
	} else if (al > (float)0.) {
	    *ier += 1024;
	    goto L12;
	}
/* L10: */
    }
L11:
    dln = (float)0.;
    if (almn < (float)0.) {
	dln = (integer) (-(doublereal)almn / (float)360. + (float).001) * (
		float)360.;
    } else if (almn > (float)360.) {
	dln = -((integer) (almn / (float)360. + (float).001)) * (float)360.;
    }
L12:
/*                       Find wrap around of output grid, if it exists. */
    iilo = *ilo;
    almxo = alono[1];
    almno = alono[1];
    i__1 = *ilo + 1;
    for (i = 2; i <= i__1; ++i) {
/* Computing MAX */
	r__1 = almxo, r__2 = alono[i];
	almxo = dmax(r__1,r__2);
/* Computing MIN */
	r__1 = almno, r__2 = alono[i];
	almno = dmin(r__1,r__2);
	al = (r__1 = alono[i] - alono[1], dabs(r__1)) - (float)360.;
	if (dabs(al) <= (float)1e-4) {
	    iilo = i - 1;
	    goto L14;
	} else if (al > (float)0.) {
	    *ier += 2048;
	    goto L15;
	}
/* L13: */
    }
L14:
    dlno = (float)0.;
    if (almno < (float)0.) {
	dlno = (integer) (-(doublereal)almno / (float)360. + (float).001) * (
		float)360.;
    } else if (almno > (float)360.) {
	dlno = -((integer) (almno / (float)360. + (float).001)) * (float)360.;
    }
L15:
/*                     Test for errors.  Return if any. */
    if (*ier != 0) {
	return 0;
    }
/*                     The output grid needs to begin with or after the */
/*                     input grid. */
    if (almno + dlno < almn + dln) {
	dlno += (float)360.;
    }

    i__1 = *jlo;
    for (j = 1; j <= i__1; ++j) {
/*              Find index limits In latitude to cover the new grid. 
*/
	j1 = *jl + 1;
	j2 = 0;
/* Computing MIN */
	r__1 = alato[j], r__2 = alato[j + 1];
	amnlto = dmin(r__1,r__2);
/* Computing MAX */
	r__1 = alato[j], r__2 = alato[j + 1];
	amxlto = dmax(r__1,r__2);

/*                     Search for index limits in J. */

	i__2 = *jl;
	for (jj = 1; jj <= i__2; ++jj) {
/* Computing MIN */
	    r__1 = alat[jj], r__2 = alat[jj + 1];
	    amnlt = dmin(r__1,r__2);
/* Computing MAX */
	    r__1 = alat[jj], r__2 = alat[jj + 1];
	    amxlt = dmax(r__1,r__2);
/*                     FIND JJ LIMITS */
	    if (amxlt > amnlto && amnlt < amxlto) {
		j1 = min(jj,j1);
		j2 = max(jj,j2);
	    }

/* L17: */
	}
/*                     If input grid doesn't at least partially cover 
the */
/*                    output grid box, no values will be assigned.  Ma
sk out*/
/*                     all values for the latitude. */

	if (j2 < j1) {
	    i__2 = iilo;
	    for (i = 1; i <= i__2; ++i) {
		ao[i + j * ao_dim1] = (float)1e20;
		if (masko[i + j * masko_dim1]) {
		    *ier = -1;
		}
		masko[i + j * masko_dim1] = FALSE_;
/* L20: */
	    }
	    goto L200;
	}
/*  TEMPORARY ******************************************************* 
*/
/*      AMNLT=MIN(MIN(ALAT(J1),ALAT(J1+1)),MIN(ALAT(J2),ALAT(J2+1))) 
*/
/*      AMXLT=MAX(MAX(ALAT(J1),ALAT(J1+1)),MAX(ALAT(J2),ALAT(J2+1))) 
*/

/*      WRITE (NOUT,90) J1,J2,AMNLT,AMXLT */
/*  90  FORMAT ('J',2I6,2F9.3) */

	i__2 = iilo;
	for (i = 1; i <= i__2; ++i) {
/*              No need to compute if it is masked out. */
	    if (! masko[i + j * masko_dim1]) {
		goto L100;
	    }
/*              Find index limits in longitude to cover the new gr
id. */
	    i1 = *il * 3 + 1;
	    i2 = 0;
/* Computing MIN */
	    r__1 = alono[i], r__2 = alono[i + 1];
	    amnlno = dmin(r__1,r__2) + dlno;
/* Computing MAX */
	    r__1 = alono[i], r__2 = alono[i + 1];
	    amxlno = dmax(r__1,r__2) + dlno;

/*                     Search for index limits in I. */
/*                     Because of wrap around it is necessary to 
*/
/*                     look through the data twice. */
/*                     The output grid longitudes have been adjust
ed */
/*                     (using DLNO) such that the first longitude 
in */
/*                     the output grid is greater than the first 
*/
/*                     longitude on the input grid. */

	    for (k = 0; k <= 1; ++k) {
		i__3 = iil;
		for (ii = 1; ii <= i__3; ++ii) {
/* Computing MIN */
		    r__1 = alon[ii], r__2 = alon[ii + 1];
		    amnln = dmin(r__1,r__2) + dln + k * (float)360.;
/* Computing MAX */
		    r__1 = alon[ii], r__2 = alon[ii + 1];
		    amxln = dmax(r__1,r__2) + dln + k * (float)360.;
/*                     FIND II LIMITS */
		    if (amxln > amnlno && amnln < amxlno) {
/* Computing MIN */
			i__4 = ii + k * *il;
			i1 = min(i__4,i1);
/* Computing MAX */
			i__4 = ii + k * *il;
			i2 = max(i__4,i2);
		    }

/* L30: */
		}
/* L35: */
	    }
/*                     If input grid doesn't partially cover the o
utput */
/*                     grid box, no values will be assigned.  Mask
 out */
/*                     the grid box. */
	    if (i2 < i1) {
		ao[i + j * ao_dim1] = (float)1e20;
		if (masko[i + j * masko_dim1]) {
		    *ier = -1;
		}
		masko[i + j * masko_dim1] = FALSE_;
		goto L100;
	    }
/*  TEMPORARY */
/*      WRITE (NOUT,91) I1,I2,AMNLNO,AMXLNO */
/*  91  FORMAT ('I',2I6,2F9.3) */

	    wt = (float)0.;
	    avg = (float)0.;
	    i__3 = j2;
	    for (jj = j1; jj <= i__3; ++jj) {
/* Computing MAX */
		r__1 = alat[jj], r__2 = alat[jj + 1];
		slatmx = dmax(r__1,r__2);
/* Computing MIN */
		r__1 = alat[jj], r__2 = alat[jj + 1];
		slatmn = dmin(r__1,r__2);
/* Computing MAX */
		r__1 = sin(dmin(amxlto,slatmx) * api / (float)180.) - sin(
			dmax(amnlto,slatmn) * api / (float)180.);
		wlat = dmax(r__1,(float)0.);
		if (wlat != (float)0.) {
		    i__4 = i2;
		    for (iii = i1; iii <= i__4; ++iii) {
			slon = dln;
			slonp = dln;
			if (iii > iil) {
			    slon += (float)360.;
			    slonp += (float)360.;
			}
			ii = (iii - 1) % iil + 1;
			iip = ii + 1;
			if (mask[ii + jj * mask_dim1]) {
			    slon += alon[ii];
			    slonp += alon[iip];
			    slonmx = dmax(slon,slonp);
			    slonmn = dmin(slon,slonp);
/* Computing MAX */
			    r__1 = dmin(amxlno,slonmx) - dmax(amnlno,slonmn);
			    delon = dmax(r__1,(float)0.);
/*          WRITE (NOUT,92) MAX(SLATMN,AMNLTO),MIN
(SLATMX,AMXLTO), */
/*     1                 MAX(SLONMN,AMNLNO),MIN(SL
ONMX,AMXLNO),DELON */
/*  92      FORMAT (' LAT ',2F9.3,' LON ',2F9.3,' 
DLON ',F9.3) */
/*          WRITE (NOUT,93) SLONMN,SLONMX,AMNLNO,A
MXLNO */
/*  93      FORMAT (' IN ',2F9.3,' OUT ',2F9.3) */
			    wt += wlat * delon;
			    avg += a[ii + jj * a_dim1] * wlat * delon;
			}
/* L40: */
		    }
		}
/* L50: */
	    }
	    if (wt > (float)0.) {
		ao[i + j * ao_dim1] = avg / wt;
	    } else {
		ao[i + j * ao_dim1] = (float)1e20;
		if (masko[i + j * masko_dim1]) {
		    *ier = -1;
		}
		masko[i + j * masko_dim1] = FALSE_;
	    }
L100:
	    ;
	}
L200:
	;
    }
/*                     Finish filling the output array from wrap-around. 
*/
    if (iilo < *ilo) {
	i__1 = *jlo;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *ilo;
	    for (i = iilo + 1; i <= i__2; ++i) {
		ao[i + j * ao_dim1] = ao[i - iilo + j * ao_dim1];
		masko[i + j * masko_dim1] = masko[i - iilo + j * masko_dim1];
/* L300: */
	    }
	}
    }
/*                     Check if output MASKO is all false. */
    i__2 = *jlo;
    for (j = 1; j <= i__2; ++j) {
	i__1 = *ilo;
	for (i = 1; i <= i__1; ++i) {
	    if (masko[i + j * masko_dim1]) {
		goto L500;
	    }
/* L400: */
	}
    }
    *ier = -2;
L500:
    return 0;
} /* awi_ */

