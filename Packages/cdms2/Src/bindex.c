/* Functions for bin indexing and intersection of lat/lon regions
   with data on non-rectilinear grids.
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/* NBINI, NBINJ should match definition in bindex.bindexHorizontalGrid */
/* #define NBINI 720			     /\* number of bins in the i (longitude) direction *\/ */
/* #define NBINJ 360			     /\* number of bins in the j (latitude) direction *\/ */
/* #define BINLEN NBINI*NBINJ */
#define MAX(a,b) (a)>(b)?(a):(b)
#define MIN(a,b) (a)<(b)?(a):(b)
/* #define XBINI (360.0/((double)NBINI))	     /\* width of a longitude bin *\/ */
/* #define XBINJ (180.0/((double)NBINJ))	     /\* width of a latitude bin *\/ */

int NBINI = 720;
int NBINJ = 360;
long BINLEN = 720*360;
double XBINI = .5;
double XBINJ = .5;

/* Index lons, lats into NBINJ*NBINI bins in lat/lon space.
   lats is a 1-D array of latitudes in degrees north, length n.
   lons is a 1-D array of longitudes in degrees east, length n.
   head[i] is the first grid index in bin i. head must be input with length BINLEN.
   next[i] points to the next lat/lon entry for bin i, or -1 if end-of-list.
     next must be input with length BINLEN.
*/

void setDeltas(double dX, double dY) {
  NBINI = (int) (360./dX);
  NBINJ = (int) (180./dY);
  BINLEN = NBINI*NBINJ;
  XBINI = (360.0/((double)NBINI));
  XBINJ = (180.0/((double)NBINJ));
}

void getLens(int *X, int *Y) {
  *X=NBINI;
  *Y=NBINJ;
}

void bindex(long n, long nbins, double *lats, double *lons, long *head, long *next){

  //fprintf(stderr,"mallcing: %i, %i, %i, %f, %f\n",NBINI,NBINJ,BINLEN,XBINI,XBINJ);

    long i, j, oind, ireg;
    int *last;//[BINLEN];


    double xbini = XBINI;
    double xbinj = XBINJ;

    last = (int *) malloc(sizeof(int)*BINLEN);

					     /* Initialize head, last values to -1 */
    for (i=0; i<BINLEN; i++){
	head[i] = last[i] = -1;
    }
					     /* Scan lons and lats */
    for (ireg=0; ireg<n; ireg++){

					     /* Get i, j index into overlay grid, transform */
					     /* to oind */
	i = (long)floor(lons[ireg]/xbini);
	i %= NBINI;
	if (i<0) i+=NBINI;
	j = (long)floor((lats[ireg]+90.0)/xbinj);
	j = MIN(MAX(j,0),NBINJ-1);
	
	oind = NBINJ*i + j;

					     /* If this is the first entry, set both head and last, */
					     /* otherwise just last */
	/*	printf("ireg=%d, i=%d, j=%d, xbini=%lf, lat=%lf, lon=%lf\n", ireg, i, j, xbini, lats[ireg], lons[ireg]); */
	next[ireg] = -1;
	if (head[oind]==-1){
	    head[oind] = last[oind] = ireg;
	}
	else {
	    next[last[oind]] = ireg;
	    last[oind] = ireg;
	}
    }
    free(last);
}

/* Same as intersect for the longitude range [0,360).
 */
long intersect_1(double slat, double slon, double elat, double elon, double lats[], double lons[], long head[], long ngrid, long next[], long points[], long npoints, char *latind, char *lonind){

    long si, sj, ei, ej, i, j, hind, rind;
    double lat, lon;
    char latind0, latind1, lonind0, lonind1;
    double xbini = XBINI;
    double xbinj = XBINJ;

    si = (long)floor(slon/xbini);
    si = MIN(MAX(si,0),(NBINI-1));
    sj = (long)floor((slat+90.0)/xbinj);
    sj = MIN(MAX(sj,0),NBINJ-1);
    ei = (long)floor(elon/xbini);
    ei = MIN(MAX(ei,0),(NBINI-1));
    ej = (long)floor((elat+90.0)/xbinj);
    ej = MIN(MAX(ej,0),NBINJ-1);

    latind0 = latind[0];
    latind1 = latind[1];
    lonind0 = lonind[0];
    lonind1 = lonind[1];

/*     printf("slon=%lf, elon=%lf, lonind=%s, slat=%lf, elat=%lf\, latind=%s\n",slon,elon,lonind,slat,elat,latind); */

    /* Skip empty slices */
    if (slon==elon && (lonind0=='o' || lonind1=='o')){
	return npoints;
    }

    for(i=si; i<ei+1; i++){
	for(j=sj; j<ej+1; j++){
	    hind = NBINJ*i + j;
	    rind = head[hind];
	    if (i==si || i==ei || j==sj || j==ej){
		while(rind!=-1){
		    lat = lats[rind];
		    lon = lons[rind];
		    if (((latind0=='c' && slat<=lat) || (latind0=='o' && slat<lat)) &&
			((latind1=='c' && lat<=elat) || (latind1=='o' && lat<elat)) &&
			((lonind0=='c' && slon<=lon) || (lonind0=='o' && slon<lon)) &&
			((lonind1=='c' && lon<=elon) || (lonind1=='o' && lon<elon))) {
			if (npoints==ngrid){
			    fprintf(stderr,"Internal error in intersect.\n");
			    return npoints;
			}
			points[npoints++] = rind;
		    }
		    rind = next[rind];
		}
	    }
	    else{
		while(rind!=-1){
		    if (npoints==ngrid){
		        fprintf(stderr,"Internal error in intersect.\n");
			return npoints;
		    }
		    points[npoints++] = rind;
		    rind = next[rind];
		}
	    }
	}
    }
/*     printf("npoints=%d\n",npoints); */
    return npoints;
}

/* Intersect the grid and the lat-lon region with lower-left point <slat,slon>
   and upper-right point <elat,elon>. head and next are as returned by bindex.
   points is returned as the array of grid indices. The region may wrap around
   latitude 360.0. head has length BINLEN, next and points have length ngrid.
   latind and lonind are like 'cc', 'co', etc. The function returns the number
   of points in the intersection.

   lats and lons (other than slon, elon) must be normalized to the range [0,360).

   Note: if the longitude range is greater than 360, then npoints may
   be greater than ngrid, but intersect_1 will not allow points to
   overflow. So if points is allocated ngrid elements, the longitude
   range MUST be <=360 - and if equal to 360 must have lonind of 'co'
   or 'oc' - otherwise some grid cells in the intersection may be missed.
   
 */
long intersect(long ngrid, long nbins, double slat, double slon, double elat, double elon, double lats[], double lons[], long head[], long next[], long points[], char *latind, char *lonind){

    long npoints = 0, schunk, echunk;
    double xslon, xelon;
    char loni[2];

    schunk = (long)floor(slon/360.0);
    echunk = (long)floor(elon/360.0);
    xslon = slon-360.0*schunk;
    xelon = elon-360.0*echunk;
    if (schunk==echunk){
        npoints = intersect_1(slat, xslon, elat, xelon, lats, lons, head, ngrid, next, points, npoints, latind, lonind);
    }
    else{
	loni[0] = lonind[0];
	loni[1] = 'o';
        npoints = intersect_1(slat, xslon, elat, 360.0, lats, lons, head, ngrid, next, points, npoints, latind, loni);
	loni[0] = 'c';
	loni[1] = lonind[1];
        npoints = intersect_1(slat, 0.0, elat, xelon, lats, lons, head, ngrid, next, points, npoints, latind, loni);
    }
    return npoints;
}
