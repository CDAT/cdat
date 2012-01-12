
/*
 * set of routines to transform between x-y and lat-lon on registered grids
 */
#include <stdio.h> 
#include <math.h> 
#include <isdb.h> 
#include "gaussLats.h"

#define LOGE10   2.302585093		/* natural log of 10 */
#define ERTH_RAD 6371.229		/* mean earth radius (km) */
#define PI180    0.017453293		/*  pi / 180.  */ 
#define MIN_LAT  -90.
#define MAX_LAT   90.
#define MIN_LON  -180.
#define MAX_LON   180.

/***************************************************************/
/*
 * transform from x, y coordinates within a registered grid to the index
 */
void xy_index(geom, ix, iy, index, status)
REG_GEOM *geom;			/* registered geometry structure from isdb.h */	
long	 *ix;			/* column number, left column is 1 */
long	 *iy;			/* row number, top row is 1 */
long	 *index;		/* index within grid */
int	 *status;		/* return status */
{

/*  Check ix is within 1, nx */
    if ((*ix < 1) || (*ix > geom->nx)) {
	*status = -1;
	return;
    }

/*  Check iy is within 1, ny */
    if ((*iy < 1) || (*iy > geom->ny)) {
	*status = -1;
	return;
    }

/*  Compute index from ix, iy depending on storage order of points in grid */
/*  Next portion of code can be speeded up */
    if (strcmp(geom->stor_dsc, "+x in -y") == 0) {
	*index = (*iy - 1) * geom->nx + *ix;

    } else if (strcmp(geom->stor_dsc, "+x in +y") == 0) {
	*index = (geom->ny - *iy) * geom->nx + *ix; 

    } else if (strcmp(geom->stor_dsc, "-y in +x") == 0) {
	*index = (*ix - 1) * geom->ny + *iy;

    } else if (strcmp(geom->stor_dsc, "+y in +x") == 0) {
	*index = (*ix - 1) * geom->ny + geom->ny - *iy + 1;

    } else {
	fprintf(stderr, "Error in xy_index;  stor_dsc: %s not supported\n",
		geom->stor_dsc);
	*status = -1;
	return;
    }
    *status = 0;
    return;

}

/***************************************************************/
/*
 * transform from the index within a registered grid to x, y coordinates 
 */
void index_xy(geom, index, ix, iy, status)
REG_GEOM *geom;			/* registered geometry structure from isdb.h */	
long	 *index;		/* index within grid */
long	 *ix;			/* column number, left column is 1 */
long	 *iy;			/* row number, top row is 1 */
int	 *status;		/* return status */
{

/*  Check index is within 1, nx*ny */
    if ((*index < 1) || (*index > geom->nx * geom->ny)) {
	fprintf(stderr, "Error in index_xy;  index: %d  nx*ny: %d\n",
		*index, geom->nx * geom->ny);
	*status = -1;
	return;
    }

/*  Compute ix, iy from index depending on storage order of points in grid */
/*  Next portion of code can be speeded up */
    if (strcmp(geom->stor_dsc, "+x in -y") == 0) {
	*iy = (*index + (geom->nx - 1)) / geom->nx; 
	*ix = *index - (*iy - 1) * geom->nx;

    } else if (strcmp(geom->stor_dsc, "+x in +y") == 0) {
	*iy = (geom->nx * geom->ny - *index + geom->nx) / geom->nx; 
	*ix = *index - (geom->ny - *iy) * geom->nx;

    } else if (strcmp(geom->stor_dsc, "-y in +x") == 0) {
	*ix = (*index - 1) / geom->ny + 1; 
	*iy = *index - (*ix - 1) * geom->ny;

    } else if (strcmp(geom->stor_dsc, "+y in +x") == 0) {
	*ix = (*index - 1) / geom->ny + 1; 
	*iy = *ix * geom->ny - *index + 1;

    } else {
	fprintf(stderr, "Error in index_xy;  stor_dsc: %s not supported\n",
		geom->stor_dsc);
	*status = -1;
	return;
    }
    *status = 0;
    return;

}

/***************************************************************/
/*
 * transform from latitude and longitude to x, y coordinates within 
 * a registered grid
 */
void latlon_xy(geom, lat, lon, x, y, status)
REG_GEOM *geom;			/* registered geometry structure from isdb.h */	
double	 *lat;			/* latitude in degrees */
double	 *lon;			/* longitude in degrees */
double	 *x;			/* x coord in units of columns, left col is 1 */
double	 *y;			/* y coord in units of rows, top row is 1 */
int	 *status;		/* return status */
{
    extern void CdLookup(double *, long, double, long *);
    double temp1, temp2, temp3, temp4, temp5, *tempp;	/* temporary variables */
    double sign;		/* temporary variable containing sign */
    double diff_lon;		/* longitude difference holder */
    long len,k;

/*  Check if latitude is within limits */
    if ((*lat < MIN_LAT) || (*lat > MAX_LAT)) { 
	fprintf(stderr, "Error in latlon_xy;  lat: %lf  not in  %f, %f\n",
		*lat, MIN_LAT, MAX_LAT);
	*status = -1;
	return;
    }

/*  Check if longitude is within limits */
    if ((*lon < MIN_LON) || (*lon > MAX_LON)) { 
	fprintf(stderr, "Error in latlon_xy;  lon: %lf  not in  %f, %f\n",
		*lon, MIN_LON, MAX_LON);
	*status = -1;
	return;
    }

/*  
 *  Compute x, y from lat, lon depending on projection 
 */
    if (strcmp(geom->prjn_name, "gaussian") == 0) {
	diff_lon  = *lon - geom->lon;
	*x    	  = diff_lon / geom->parm_2 + (double) geom->orig_ix;

	    /*  	Take care of longitude wrap-around */

	if (*x < 0.) *x += (double) geom->nx;
	if (*x > (double) geom->nx) *x -= (double) geom->nx;
		
	switch (geom->ny){
	  case T21_LEN:
		tempp = t21Lats;
		len = T21_LEN;
		break;
	  case R15_LEN:
		tempp = r15Lats;
		len = R15_LEN;
		break;
	  case T32_LEN:
		tempp = t32Lats;
		len = T32_LEN;
		break;
	  case R21_LEN:
		tempp = r21Lats;
		len = R21_LEN;
		break;
	  case T42_LEN:
		tempp = t42Lats;
		len = T42_LEN;
		break;
	  case R31_LEN:
		tempp = r31Lats;
		len = R31_LEN;
		break;
	  case T63_LEN:
		tempp = t63Lats;
		len = T63_LEN;
		break;
	  case R40_LEN:
		tempp = r40Lats;
		len = R40_LEN;
		break;
	  case T106_LEN:
		tempp = t106Lats;
		len = T106_LEN;
		break;
	  default:
		  fprintf(stderr, "Error in latlon_xy; no Gaussian latitude of length %d\n",
			  geom->ny);
		  *status = -1;
		  return;
	};
				/* Lookup latitude; vector include poles */
	CdLookup(tempp,len+2,*lat,&k);
	if(k == -1)
		*y = 0.;
	else if (k == (len+1))
		*y = (double)(len+1);
	else
		*y = (double) k + (*lat - (*(tempp+k)))/((*(tempp+k+1)) - (*(tempp+k)));
    }
				/* Cylindrical equal-area, e.g., LMD grid */
				/* Note: returns 0.5 <= y <= geom->ny+0.5 */
    else if (strcmp(geom->prjn_name, "cylindrical_eq_area") == 0) {
	diff_lon  = *lon - geom->lon;
	*x    	  = diff_lon / geom->parm_2 + (double) geom->orig_ix;

	    /*  	Take care of longitude wrap-around */

	if (*x < 0.) *x += (double) geom->nx;
	if (*x > (double) geom->nx) *x -= (double) geom->nx;

	*y = 0.5 * (geom->ny * (1.0 - sin(PI180*(*lat))) + 1.0);
    }
    else if (strcmp(geom->prjn_name, "spherical") == 0) {
	diff_lon  = *lon - geom->lon;
	*x    	  = diff_lon / geom->parm_2 + (double) geom->orig_ix;

/*  	Take care of longitude wrap-around */
	if ((*x < 1.) || (*x > (double) geom->nx)) {
	    temp1 = (diff_lon - 360.) / geom->parm_2 + (double) geom->orig_ix;
	    if (fabs((double)(geom->orig_ix + 1) / 2. - temp1) < 
		fabs((double)(geom->orig_ix + 1) / 2. - *x)) *x = temp1;
	    temp1 = (diff_lon + 360.) / geom->parm_2 + (double) geom->orig_ix;
	    if (fabs((double)(geom->orig_ix + 1) / 2. - temp1) <
		fabs((double)(geom->orig_ix + 1) / 2. - *x)) *x = temp1;
	}

	*y    = (geom->lat - *lat) / geom->parm_1 + (double) geom->orig_iy;

    } else if (strcmp(geom->prjn_name, "mercator") == 0) {
/*      Mercator requires: 0 <= parm_1 < 90;
 * 			   other standard latitude = -parm_1
 *			   top of grid always toward N */ 
/*	Check lat is not equal to 90. or -90. (singular points) */
    	if ((*lat == MIN_LAT) || (*lat == MAX_LAT)) { 
	    fprintf(stderr, "Error in latlon_xy;  lat: %f = %f or %f\n",
		    *lat, MIN_LAT, MAX_LAT);
	    *status = -1;
	    return;
    	}
	diff_lon  = *lon - geom->lon;
	*x    	  = diff_lon / geom->parm_2 + (double) geom->orig_ix;

/*  	Take care of longitude wrap-around */
	if ((*x < 1.) || (*x > (double) geom->nx)) {
	    temp1 = (diff_lon - 360.) / geom->parm_2 + (double) geom->orig_ix;
	    if (fabs((double)(geom->orig_ix + 1) / 2. - temp1) < 
		fabs((double)(geom->orig_ix + 1) / 2. - *x)) *x = temp1;
	    temp1 = (diff_lon + 360.) / geom->parm_2 + (double) geom->orig_ix;
	    if (fabs((double)(geom->orig_ix + 1) / 2. - temp1) <
		fabs((double)(geom->orig_ix + 1) / 2. - *x)) *x = temp1;
	}

 	temp1 = ERTH_RAD * cos((double) geom->parm_1 * PI180) / 
		geom->y_int_dis; 
	temp2 = (geom->lat + 90.) * PI180 / 2.;
	temp3 = (double) geom->orig_iy + temp1 * LOGE10 * log10( tan(temp2) );
	temp4 = (*lat + 90.) * PI180 / 2.;
	*y    = temp3 - temp1 * LOGE10 * log10( tan(temp4) );

    } else if (strcmp(geom->prjn_name, "polar_stereo") == 0) {
	sign = (geom->parm_1 > 0.) ? 1. : -1.;
 	temp1 = (*lon - (double) geom->parm_2) * PI180; 
	temp2 = tan((45. - sign * *lat / 2.) * PI180);
	temp3 = ERTH_RAD * (1. + sin(fabs((double) geom->parm_1) * PI180));
 	temp4 = (geom->lon - (double) geom->parm_2) * PI180; 
	temp5 = tan((45. - sign * geom->lat / 2.) * PI180);
	*x    = (double) geom->orig_ix - temp3 / geom->x_int_dis *
		(temp5 * sin(temp4) - temp2 * sin(temp1));
	*y    = (double) geom->orig_iy - sign * temp3 / geom->y_int_dis *
		(temp5 * cos(temp4) - temp2 * cos(temp1));

    } else if (strcmp(geom->prjn_name, "lambert") == 0) {
/*      Lambert requires: 0 < |parm_1,2| < 90
 *			  |parm_1| <= |parm_2|
 *			  parm_1 * parm_2 > 0
 *			  top of grid always toward N */ 
/*	Check lat is not equal to 90. or -90. (singular points) */
    	if ((*lat == MIN_LAT) || (*lat == MAX_LAT)) { 
	    fprintf(stderr, "Error in latlon_xy;  lat: %f = %f or %f\n",
		    *lat, MIN_LAT, MAX_LAT);
	    *status = -1;
	    return;
    	}
	sign = (geom->parm_1 > 0.) ? 1. : -1.;
	if (geom->parm_1 == geom->parm_2) {
	    temp1 = sign * sin(geom->parm_1 * PI180);
	} else {
	    temp1 = log(cos(geom->parm_1 * PI180) / 
		        cos(geom->parm_2 * PI180)) / 
		    log(tan((45. + sign * geom->parm_2 / 2.) * PI180) /
		        tan((45. + sign * geom->parm_1 / 2.) * PI180));
	}

/*	Take care of longitude wrap-around for (*lon - geom->parm_3) */
	diff_lon = *lon - geom->parm_3;
	if (fabs(*lon - geom->parm_3 - 360.) < fabs(diff_lon)) 
	    diff_lon = *lon - geom->parm_3 - 360.;
	if (fabs(*lon - geom->parm_3 + 360.) < fabs(diff_lon)) 
	    diff_lon = *lon - geom->parm_3 + 360.;

/*	Take care of longitude wrap-around for (geom->lon - geom->parm_3) */
	temp5 = geom->lon - geom->parm_3;
	if (fabs(geom->lon - geom->parm_3 - 360.) < fabs(temp5)) 
	    temp5 = geom->lon - geom->parm_3 - 360.;
	if (fabs(geom->lon - geom->parm_3 + 360.) < fabs(temp5)) 
	    temp5 = geom->lon - geom->parm_3 + 360.;

	temp2 = cos(geom->parm_1 * PI180) * ERTH_RAD / temp1 *
		pow(tan((45. + sign * geom->parm_1 / 2.) * PI180), temp1);
	temp3 = pow(tan((45. + sign * geom->lat / 2.) * PI180), temp1);
	temp4 = pow(tan((45. + sign * *lat / 2.) * PI180), temp1);
	*x    = (double) geom->orig_ix - temp2 / geom->x_int_dis *
		(sin(temp1 * temp5 * PI180) / temp3 -
		 sin(temp1 * diff_lon * PI180) / temp4);
	*y    = (double) geom->orig_iy - temp2 / geom->y_int_dis * sign * 
		(cos(temp1 * temp5 * PI180) / temp3 -
		 cos(temp1 * diff_lon * PI180) / temp4);

    } else {
	fprintf(stderr, "Error in latlon_xy;  prjn_name: %s not supported\n",
		geom->prjn_name);
	*status = -1;
	return;
    }
/*
 printf("ll_xy; lat: %f  lon: %f  x: %lf  y: %lf\n", *lat, *lon, *x, *y);
 */
    *status = 0;
    return;
}

/***************************************************************/
/*
 * transform from x, y coordinates within a registered grid to 
 * latitude and longitude
 */
void xy_latlon(geom, x, y, lat, lon, status)
REG_GEOM *geom;			/* registered geometry structure from isdb.h */	
double	 *x;			/* x coord in units of columns, left col is 1 */
double	 *y;			/* y coord in units of rows, top row is 1 */
double	 *lat;			/* latitude in degrees */
double	 *lon;			/* longitude in degrees */
int	 *status;	
{
    double temp1, temp2, temp3, temp4, temp5, *tempp;  /* temporary variables */
    double sign;		/* temporary variable containing sign */
    long i;

/*  Check if x is within 0, nx + 1 */
    if ((*x < 0.) || (*x > (double) geom->nx + 1.)) { 
	fprintf(stderr, "Error in xy_latlon;  x: %lf  not in  0, %d\n",
		*x, geom->nx + 1);
	*status = -1;
	return;
    }

/*  Check if y is within 0, ny + 1 */
    if ((*y < 0.) || (*y > (double) geom->ny + 1.)) { 
	fprintf(stderr, "Error in xy_latlon;  y: %lf  not in  0, %d\n",
		*y, geom->ny + 1);
	*status = -1;
	return;
    }

/*  
 *  Compute lat, lon from x, y depending on projection 
 */
    
    if (strcmp(geom->prjn_name, "gaussian") == 0) {
	*lon  = (*x  - (double) geom->orig_ix) * geom->parm_2 + geom->lon;
	i = *y;
	temp1 = *y - (double)i;
	switch (geom->ny){
	  case T21_LEN:
		tempp = t21Lats+i;
		break;
	  case R15_LEN:
		tempp = r15Lats+i;
		break;
	  case T32_LEN:
		tempp = t32Lats+i;
		break;
	  case R21_LEN:
		tempp = r21Lats+i;
		break;
	  case T42_LEN:
		tempp = t42Lats+i;
		break;
	  case R31_LEN:
		tempp = r31Lats+i;
		break;
	  case T63_LEN:
		tempp = t63Lats+i;
		break;
	  case R40_LEN:
		tempp = r40Lats+i;
		break;
	  case T106_LEN:
		tempp = t106Lats+i;
		break;
	  default:
		  fprintf(stderr, "Error in xy_latlon; no Gaussian latitude of length %d\n",
			  geom->ny);
		  *status = -1;
		  return;
	};
	if (i == geom->ny + 1)
		*lat = *tempp;
	else
		*lat = temp1*(*(tempp+1)) + (1.-temp1)*(*tempp);
    }
				/* Cylindrical equal-area, e.g., LMD grid */
    else if (strcmp(geom->prjn_name, "cylindrical_eq_area") == 0) {

	                        /*  Must have 0.5 <= y <= ny+0.5*/
    if ((*y < 0.5) || (*y > (double) geom->ny + 0.5)) { 
	fprintf(stderr, "Error in xy_latlon;  y: %lf  not in  0.5, %lf\n",
		*y, (double) geom->ny + 0.5);
	*status = -1;
	return;
    }

    *lon  = (*x  - (double) geom->orig_ix) * geom->parm_2 + geom->lon;
    
    *lat = asin(1.0 - (2.0*(*y) - 1.0)/(double)geom->ny)/PI180;

    }
    else if (strcmp(geom->prjn_name, "spherical") == 0) {
	*lon  = (*x  - (double) geom->orig_ix) * geom->parm_2 + geom->lon;
	*lat  = ((double) geom->orig_iy - *y) * geom->parm_1 + geom->lat;

    } else if (strcmp(geom->prjn_name, "mercator") == 0) {
/*      Mercator requires: 0 <= parm_1 < 90;
 * 			   other standard latitude = -parm_1
 *			   top of grid always toward N */ 
	*lon  = (*x  - (double) geom->orig_ix) * geom->parm_2 + geom->lon;
 	temp1 = ERTH_RAD * cos((double) geom->parm_1 * PI180) / 
		geom->y_int_dis; 
	temp2 = (geom->lat + 90.) * PI180 / 2.;
	temp3 = (double) geom->orig_iy + temp1 *
		LOGE10 * log10( tan(temp2) );
	*lat  = 2. / PI180 * atan( exp((temp3 - *y) / temp1) ) - 90.;

    } else if (strcmp(geom->prjn_name, "polar_stereo") == 0) {
	sign  = (geom->parm_1 > 0.) ? 1. : -1.;
 	temp1 = (geom->lon - (double) geom->parm_2) * PI180; 
	temp2 = tan((45. - sign * geom->lat / 2.) * PI180);
	temp3 = ERTH_RAD * (1. + sin(fabs((double) geom->parm_1) * PI180));
        temp4 = (*x - (double) geom->orig_ix) * geom->x_int_dis /
		temp3 + temp2 * sin(temp1);
        temp5 = sign * (*y - (double) geom->orig_iy) * geom->y_int_dis /
		temp3 + temp2 * cos(temp1);
        *lon  = 1. / PI180 * atan2(temp4, temp5) + (double) geom->parm_2;
	if (fabs(cos((double) (*lon - geom->parm_2) * PI180)) > .7071) {
  	    *lat  = sign * (90. - 2. / PI180 * atan(temp5 / 
		    cos((double) (*lon - geom->parm_2) * PI180)));
	} else {  	/* use sine if cosine approaches 0. */
  	    *lat  = sign * (90. - 2. / PI180 * atan(temp4 /
		    sin((double) (*lon - geom->parm_2) * PI180)));
	}

    } else if (strcmp(geom->prjn_name, "lambert") == 0) {
/*      Lambert requires: 0 < |parm_1,2| < 90
 *			  |parm_1| <= |parm_2|
 *			  parm_1 * parm_2 > 0
 *			  top of grid always toward N */ 
	sign = (geom->parm_1 > 0.) ? 1. : -1.;
	if (geom->parm_1 == geom->parm_2) {
	    temp1 = sign * sin(geom->parm_1 * PI180);
	} else {
	    temp1 = log(cos(geom->parm_1 * PI180) / 
		        cos(geom->parm_2 * PI180)) / 
		    log(tan((45. + sign * geom->parm_2 / 2.) * PI180) /
		        tan((45. + sign * geom->parm_1 / 2.) * PI180));
	}
	temp2 = cos(geom->parm_1 * PI180) / temp1 * 
		pow(tan((45. + sign * geom->parm_1 / 2.) * PI180), temp1);
	temp3 = pow(tan((45. + sign * geom->lat / 2.) * PI180), temp1);
        *lon  = geom->parm_3 + 1. / PI180 / temp1 * atan2( 
		((*x - (double) geom->orig_ix) * geom->x_int_dis / ERTH_RAD +  
		sin(temp1 * (geom->lon - geom->parm_3) * PI180) * temp2 /
		temp3), ((*y - (double) geom->orig_iy) * geom->y_int_dis / 
		ERTH_RAD * sign + cos(temp1 * (geom->lon - geom->parm_3) *
		PI180) * temp2 / temp3));
	*lat  = sign * (-90. + 2. / PI180 * atan(
		pow((temp2 * cos(temp1 * (*lon - geom->parm_3) * PI180)) /
		((*y - (double) geom->orig_iy) * geom->y_int_dis / ERTH_RAD *
		sign + cos(temp1 * (geom->lon - geom->parm_3) * PI180) *
		temp2 / temp3), 1. / temp1)));

    } else {
	fprintf(stderr, "Error in xy_latlon;  prjn_name: %s not supported\n",
		geom->prjn_name);
	*status = -1;
	return;
    }

/*  Take care of longitude wrap-around */
    if (*lon < MIN_LON) *lon += 360.;
    if (*lon > MAX_LON) *lon -= 360.;
/*
 printf("xy_ll; lat: %f  lon: %f  x: %lf  y: %lf\n", *lat, *lon, *x, *y);
 */
    *status = 0;
    return;
}

/***************************************************************/
/*
 * transform from the index within a registered grid to latitude and longitude
 */
void index_latlon(geom, index, lat, lon, status)
REG_GEOM *geom;			/* registered geometry structure from isdb.h */	
long	 *index;		/* index within grid */
double	 *lat;			/* latitude in degrees */
double	 *lon;			/* longitude in degrees */
int	 *status;
{
    double x;			/* x coord in units of columns, left col is 1 */
    double y;			/* y coord in units of rows, top row is 1 */
    long  ix;			/* column number, left column is 1 */
    long  iy;			/* row number, top row is 1 */
    extern void index_xy(), xy_latlon();

/*  Convert from index to ix, iy */
    index_xy(geom, index, &ix, &iy, status);
    if (*status < 0) {
    	return;
    }

/*  
    fprintf(stderr, "\n index_latlon ix: %d  iy: %d\n", ix, iy); 
*/
    x	= (double) ix;
    y	= (double) iy;
   
/*  Convert from x, y to lat, lon */
    xy_latlon(geom, &x, &y, lat, lon, status);
    if (*status < 0) {
    	return;
    }

    *status = 0;
    return;
}

/***************************************************************/
/* 
 * transform from latitude and longitude to the index within a registered grid
 * (find the closest grid point to some lat, lon)
 */
void latlon_index(geom, lat, lon, index, status)
REG_GEOM *geom;			/* registered geometry structure from isdb.h */	
double	 *lat;			/* latitude in degrees */
double	 *lon;			/* longitude in degrees */
long	 *index;		/* index within grid */
int	 *status;		/* return status */
{
    double x;			/* x coord in units of columns, left col is 1 */
    double y;			/* y coord in units of rows, top row is 1 */
    long   ix;			/* column number, left column is 1 */
    long   iy;			/* row number, top row is 1 */
    extern void latlon_xy(), xy_index();

   
/*  Convert from lat, lon to x, y */
    latlon_xy(geom, lat, lon, &x, &y, status);
    if (*status < 0) {
    	return;
    }

/*  Round to nearest grid point */
    ix	= (long) (x + .5);
    iy	= (long) (y + .5);
/*  
    fprintf(stderr, "latlon_index ix: %d  iy: %d\n", ix, iy); 
*/

/*  Convert from ix, iy to index */
    xy_index(geom, &ix, &iy, index, status);
    if (*status < 0) {
    	return;
    }

    *status = 0;
    return;
}

/***************************************************************/
/*
 * get the (float) value from a registered grid at some latitude
 * and longitude (interpolate from 4 nearest grid points)
 */
void getf_latlon(geom, lat, lon, fbuff, fpixel, status)
REG_GEOM *geom;			/* registered geometry structure from isdb.h */	
double	 *lat;			/* latitude in degrees */
double	 *lon;			/* longitude in degrees */
float    *fbuff;		/* array containing values at all grid points */
float	 *fpixel;		/* value at lat, lon */
int	 *status;		/* return status */
{
    long   index;		/* index within grid */
    long   ix, ix1;		/* column number, left column is 1 */
    long   iy, iy1;		/* row number, top row is 1 */
    double x;			/* x coord in units of columns, left col is 1 */
    double y;			/* y coord in units of rows, top row is 1 */
    extern void latlon_xy(), xy_index();

    *fpixel = 0.;

/*  Convert from lat, lon to x, y */
    latlon_xy(geom, lat, lon, &x, &y, status);
    if (*status < 0) {
	return;
    }
/*
fprintf(stdout, "lat: %f  lon: %f  x: %lf  y: %lf\n", *lat, *lon, x, y);
 */

/*  Find nearest 4 points */
    ix  = (long) x;
    ix1 = ix + 1; 
    iy  = (long) y;
    iy1 = iy + 1; 

/*  Check point ix,iy lies inside grid */
    if ((ix1 < 1) || (ix > geom->nx)) {
	*status = -1;
	return;
    }
    if ((iy1 < 1) || (iy > geom->ny)) {
	*status = -1;
	return;
    }

/*  Do point (ix+1, iy+1) */
    xy_index(geom, &ix1, &iy1, &index, status);
    if (*status == 0) {
	*fpixel += *(fbuff + index-1) * 
			(x - (double) ix) * (y - (double) iy);
    }

/*  Do point (ix+1, iy) */
    xy_index(geom, &ix1, &iy, &index, status);
    if (*status == 0) {
	*fpixel += *(fbuff + index-1) * 
			(x - (double) ix) * ((double) iy1 - y);
    }

/*  Do point (ix, iy+1) */
    xy_index(geom, &ix, &iy1, &index, status);
    if (*status == 0) {
	*fpixel += *(fbuff + index-1) * 
			((double) ix1 - x) * (y - (double) iy);
    }

/*  Do point (ix, iy) */
    xy_index(geom, &ix, &iy, &index, status);
    if (*status == 0) {
	*fpixel += *(fbuff + index-1) * 
			((double) ix1 - x) * ((double) iy1 - y);
    }

    *status = 0;
    return;
}

/***************************************************************/
/*
 * put the (float) value onto a registered grid at some latitude
 * and longitude (smear over 4 nearest grid points)
 */
void putf_latlon(geom, lat, lon, fpixel, fbuff, status)
REG_GEOM *geom;			/* registered geometry structure from isdb.h */	
double	 *lat;			/* latitude in degrees */
double	 *lon;			/* longitude in degrees */
float	 *fpixel;		/* value at lat, lon */
float    *fbuff;		/* array containing values at all grid points */
int	 *status;		/* return status */
{
    long   index;		/* index within grid */
    long   ix, ix1;		/* column number, left column is 1 */
    long   iy, iy1;		/* row number, top row is 1 */
    double x;			/* x coord in units of columns, left col is 1 */
    double y;			/* y coord in units of rows, top row is 1 */
    extern void latlon_xy(), xy_index();

/*  Convert from lat, lon to x, y */
    latlon_xy(geom, lat, lon, &x, &y, status);
    if (*status < 0) {
	return;
    }
/*
fprintf(stdout, "lat: %f  lon: %f  x: %lf  y: %lf\n", *lat, *lon, x, y);
 */

/*  Find nearest 4 points */
    ix  = (long) x;
    ix1 = ix + 1; 
    iy  = (long) y;
    iy1 = iy + 1; 

/*  Check point ix,iy lies inside grid */
    if ((ix1 < 1) || (ix > geom->nx)) {
	*status = -1;
	return;
    }
    if ((iy1 < 1) || (iy > geom->ny)) {
	*status = -1;
	return;
    }

/*  Do point (ix+1, iy+1) */
    xy_index(geom, &ix1, &iy1, &index, status);
    if (*status == 0) {
	*(fbuff + index-1) = *fpixel * 
			(x - (double) ix) * (y - (double) iy);
    }

/*  Do point (ix+1, iy) */
    xy_index(geom, &ix1, &iy, &index, status);
    if (*status == 0) {
	*(fbuff + index-1) = *fpixel * 
			(x - (double) ix) * ((double) iy1 - y);
    }

/*  Do point (ix, iy+1) */
    xy_index(geom, &ix, &iy1, &index, status);
    if (*status == 0) {
	*(fbuff + index-1) = *fpixel * 
			((double) ix1 - x) * (y - (double) iy);
    }

/*  Do point (ix, iy) */
    xy_index(geom, &ix, &iy, &index, status);
    if (*status == 0) {
	*(fbuff + index-1) = *fpixel * 
			((double) ix1 - x) * ((double) iy1 - y);
    }

    *status = 0;
    return;
}


/***************************************************************/
/*
 * compute the grid interval distance for a registered grid using a
 * previously-located grid point
 */
void get_int_dis(geom, x, y, lat, lon, status)
REG_GEOM *geom;			/* registered geometry structure from isdb.h */	
double	 *x;			/* x coord of known point in units of columns */
double	 *y;			/* y coord of known point in units of rows */
double	 *lat;			/* latitude of known point in degrees */
double	 *lon;			/* longitude of known point in degrees */
int	 *status;		/* return status */
{
    double temp1, temp2, temp3, temp4, temp5;	/* temporary variables */
    double sign;		/* temporary variable containing sign */

/*  Check lat is within limits */
    if ((*lat < MIN_LAT) || (*lat > MAX_LAT)) { 
	fprintf(stderr, "Error in get_int_dis;  lat: %lf  not in  %f, %f\n",
		*lat, MIN_LAT, MAX_LAT);
	*status = -1;
	return;
    }

/*  Check lon is within limits */
    if ((*lon < MIN_LON) || (*lon > MAX_LON)) { 
	fprintf(stderr, "Error in get_int_dis;  lon: %lf  not in  %f, %f\n",
		*lon, MIN_LON, MAX_LON);
	*status = -1;
	return;
    }

/*  Check y value different from y of origin */
    if (fabs(*y - (double) geom->orig_iy) < 1.e-5) {
	fprintf(stderr, "Error in get_int_dis;  y: %f  equals  orig_iy: %d\n",
		*y, geom->orig_iy);
	*status = -1;
	return;
    }

/*  Check x value different from x of origin */
    if (fabs(*x - (double) geom->orig_ix) < 1.e-5) {
	fprintf(stderr, "Error in get_int_dis;  x: %f  equals  orig_ix: %d\n",
		*x, geom->orig_ix);
	*status = -1;
	return;
    }

/*
 *  Compute y_int_dis, x_int_dis from rest of geom info and x, y, lat, lon 
 */
    if (strcmp(geom->prjn_name, "spherical") == 0) {
	geom->y_int_dis = ERTH_RAD * PI180 * geom->parm_1;
	geom->x_int_dis = ERTH_RAD * PI180 * geom->parm_2;

    } else if (strcmp(geom->prjn_name, "mercator") == 0) {
/*      Mercator requires: 0 <= parm_1 < 90;
 * 			   other standard latitude = -parm_1
 *			   top of grid always toward N */ 
 	temp1 = ERTH_RAD * cos((double) geom->parm_1 * PI180);
	temp2 = PI180 / 2. * (geom->lat + 90.);
	temp3 = PI180 / 2. * (*lat + 90.);
	geom->y_int_dis = temp1 / (*y - (double) geom->orig_iy) *
		LOGE10 * ( log10(tan(temp2)) - log10(tan(temp3)) );
	geom->x_int_dis = geom->y_int_dis;

    } else if (strcmp(geom->prjn_name, "polar_stereo") == 0) {
	sign = (geom->lat > 0.) ? 1. : -1.;
 	temp1 = (*lon - (double) geom->parm_2) * PI180; 
	temp2 = tan((45. - sign * *lat / 2.) * PI180);
	temp3 = ERTH_RAD * (1. + sin(fabs((double) geom->parm_1) * PI180));
 	temp4 = (geom->lon - (double) geom->parm_2) * PI180; 
	temp5 = tan((45. - sign * geom->lat / 2.) * PI180);
	geom->y_int_dis = 1. / ((double) geom->orig_iy - *y) *
		sign * temp3 * (temp5 * cos(temp4) - temp2 * cos(temp1));
	geom->x_int_dis = 1. / ((double) geom->orig_ix - *x) *
		temp3 * (temp5 * sin(temp4) - temp2 * sin(temp1));

    } else if (strcmp(geom->prjn_name, "lambert") == 0) {
/*      Lambert requires: 0 < |parm_1,2| < 90
 *			  |parm_1| <= |parm_2|
 *			  parm_1 * parm_2 > 0
 *			  top of grid always toward N */ 
	sign = (geom->parm_1 > 0.) ? 1. : -1.;
	if (geom->parm_1 == geom->parm_2) {
	    temp1 = sign * sin(geom->parm_1 * PI180);
	} else {
	    temp1 = log(cos(geom->parm_1 * PI180) / 
		        cos(geom->parm_2 * PI180)) / 
		    log(tan((45. + sign * geom->parm_2 / 2.) * PI180) /
		        tan((45. + sign * geom->parm_1 / 2.) * PI180));
	}
	temp2 = cos(geom->parm_1 * PI180) * ERTH_RAD / temp1 *
		pow(tan((45. + sign * geom->parm_1 / 2.) * PI180), temp1);
	temp3 = pow(tan((45. + sign * geom->lat / 2.) * PI180), temp1);
	temp4 = pow(tan((45. + sign * *lat / 2.) * PI180), temp1);
	geom->x_int_dis = temp2 / ((double) geom->orig_ix - *x) * 
		(sin(temp1 * (geom->lon - geom->parm_3) * PI180) / temp3 -
		 sin(temp1 * (*lon - geom->parm_3) * PI180) / temp4);
	geom->y_int_dis = temp2 / ((double) geom->orig_iy - *y) * sign *
		(cos(temp1 * (geom->lon - geom->parm_3) * PI180) / temp3 - 
		 cos(temp1 * (*lon - geom->parm_3) * PI180) / temp4);

    } else {
	fprintf(stderr, "Error in get_int_dis;  prjn_name: %s not supported\n",
		geom->prjn_name);
	*status = -1;
	return;
    }

    *status = 0;
    return;
}
/***************************************************************/
/*
 *  Transform gridded data from one registered grid geometry to another. 
 */
void grid_map(geom_a, geom_b, fbuff_a, fbuff_b, status)
REG_GEOM  *geom_a;		/* geom structure from isdb.h - input grid */
REG_GEOM  *geom_b;		/* geom structure from isdb.h - output grid */
float	  *fbuff_a;		/* array containing grid values - input */
float	  *fbuff_b;		/* array containing grid values - output */
int	  *status;	        /* return status from routine */
{
    long        i;                      /* index variable */
    long        j;                      /* index variable */
    long        index;                  /* index number for array */
    float       value;                  /* calculated value of function */ 
    double      x;                      /* col within grid */
    double      y;                      /* row within grid */
    double	lat;			/* latitude in degrees */
    double	lon;			/* longitude in degrees */

    extern void   xy_index();
    extern void   xy_latlon();
    extern void   getf_latlon();

/*  Loop over gridpoints of output grid */
    for(j = 1; j <= geom_b->ny; j++){
      for(i = 1; i <= geom_b->nx; i++){

    	x = (double) i;
    	y = (double) j;

/*  	Transform the output grid coordinates to lat-lon coordinates */
    	xy_latlon(geom_b, &x, &y, &lat, &lon, status);
    	if (*status < 0) {
    	    fprintf(stderr, "Grid_map: Error from xy_latlon\n");
	    return;
    	}

/*  	Use two-d interpoplation to get value of input grid at lat, lon */
    	getf_latlon(geom_a, &lat, &lon, fbuff_a, &value, status);
    	if (*status < 0) {
    	    fprintf(stderr, "Grid_map: Error from getf_latlon\n");
	    return;
    	}

/*  	Convert from row, col number to linear array index */ 
    	xy_index(geom_b, &i, &j, &index, status);
    	if (*status < 0) {
    	    fprintf(stderr, "Grid_map: Error from xy_index\n");
	    return;
    	}

/*  	Assign calculated grid point value to grid point */
   	*(fbuff_b + index - 1) = value;

      } 		/* end of loop over columns */ 
    } 			/* end of loop over rows    */ 

}
					     /* Binary lookup */
					     /* Lookup x, in strictly monotonic vector tab (increasing or decreasing) */
					     /* of length n, return index k. */
					     /* k is interpreted as follows: */
					     /* Case: tab is increasing: */
					     /*   k == -1 iff x <= tab[0] */
					     /*   k == (n-1) iff x > tab[n-1] */
					     /*   else tab[k] < x <= tab[k+1] */
					     /* Case: tab is decreasing: */
					     /*   k == -1 iff x > tab[0] */
					     /*   k == (n-1) iff x <= tab[n-1] */
					     /*   else tab[k] >= x > tab[k+1]*/

void CdLookup(double tab[], long n, double x, long *k)
{
	long kupper,kmid,klower;
	int incr;

	klower=-1;
	kupper=n;
	incr=(tab[n-1] > tab[0]);
	while (kupper-klower > 1) {
		kmid=(kupper+klower) >> 1;
		if (x > tab[kmid] == incr)
			klower=kmid;
		else
			kupper=kmid;
	}
	*k=klower;
}
