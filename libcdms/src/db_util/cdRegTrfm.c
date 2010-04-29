/* -*-Mode: C;-*-
 * Module:      index <-> x-y <-> latlon wrapper routines
 *              CdXyIndex     - x-y -> index
 *              CdIndexXy     - index -> x-y
 *              CdLatLonXy    - latlon -> x-y
 *              CdXyLatLon    - x-y -> latlon
 *              CdIndexLatLon - index -> latlon
 *              CdLatLonIndex - latlon -> index
 *              CdMapGeom     - map CDMS geom/order to NEONS geom
 *
 * Copyright:	1994, Regents of the University of California
 *		This software may not be distributed to others without
 *		permission of the author.
 *
 * Author:      Bob Drach, Lawrence Livermore National Laboratory
 *              drach@llnl.gov
 *
 * Version:     $Id$
 *
 * Revision History:
 *
 * $Log: cdRegTrfm.c,v $
 * Revision 1.2  1996/02/21  23:56:46  drach
 * - Overlayed cdtime routines in cdTimeConv.c:
 * - Added seconds, julian calendar, changed include to cdmsint.h for old
 *   time routines in timeArith.c and timeConv.c
 *
 * Revision 1.1  1994/08/12  19:03:53  drach
 * Entered into CVS
 *
 *
 */

#include <stdio.h>
#include <cdmsint.h>
#include <isdb.h>
					     /* copy CDMS geom to NEONS ngeom */
void
CdCopyGeom(CdRegGeom *geom, REG_GEOM *ngeom)
{
	strncpy(ngeom->prjn_name,geom->prjnName,21);
	ngeom->nx = geom->nx;
	ngeom->ny = geom->ny;
	ngeom->lat = geom->lat;
	ngeom->lon = geom->lon;
	ngeom->orig_ix = geom->orig_ix;
	ngeom->orig_iy = geom->orig_iy;
	ngeom->x_int_dis = geom->x_int_dis;
	ngeom->y_int_dis = geom->y_int_dis;
	ngeom->parm_1 = geom->parm_1;
	ngeom->parm_2 = geom->parm_2;
	ngeom->parm_3 = geom->parm_3;

	return;
}

					     /* Copy CDMS geom & order to NEONS ngeom */
void
CdMapGeom(CdRegGeom *geom, CdOrder *order, REG_GEOM *ngeom)
{
	if(order->lonDir < 0){
		fprintf(stderr,"CDMS error: longitude direction must be non-negative.\n");
	}

	if(order->lonDir < order->latDir)
		if(order->latDir > 0)
			strcpy(ngeom->stor_dsc,"+x in +y");
		else
			strcpy(ngeom->stor_dsc,"+x in -y");
	else
		if(order->latDir > 0)
			strcpy(ngeom->stor_dsc,"+y in +x");
		else
			strcpy(ngeom->stor_dsc,"-y in +x");

	CdCopyGeom(geom,ngeom);
	return;
}

int
CdXyIndex(CdRegGeom *geom, CdOrder *order, long ix, long iy, long *index)
{
	REG_GEOM ngeom;
	int status;

	CdMapGeom(geom,order,&ngeom);
	xy_index(&ngeom,&ix,&iy,index,&status);
	return (status == 0 ? 0 : 1);
}
int
CdIndexXy(CdRegGeom *geom, CdOrder *order, long index, long *ix, long *iy)
{
	REG_GEOM ngeom;
	int status;

	CdMapGeom(geom,order,&ngeom);
	index_xy(&ngeom,&index,ix,iy,&status);
	return (status == 0 ? 0 : 1);
}
int
CdLatLonXy(CdRegGeom *geom, double lat, double lon, double *x, double *y)
{
	REG_GEOM ngeom;
	int status;

	CdCopyGeom(geom,&ngeom);	     /* order is unused */
	latlon_xy(&ngeom,&lat,&lon,x,y,&status);
	return (status == 0 ? 0 : 1);
}
int
CdXyLatLon(CdRegGeom *geom, double x, double y, double *lat, double *lon)
{
	REG_GEOM ngeom;
	int status;

	CdCopyGeom(geom,&ngeom);	     /* order is unused */
	xy_latlon(&ngeom,&x,&y,lat,lon,&status);
	return (status == 0 ? 0 : 1);
}
int
CdIndexLatLon(CdRegGeom *geom, CdOrder *order, long index, double *lat, double *lon)
{
	REG_GEOM ngeom;
	int status;

	CdMapGeom(geom,order,&ngeom);
	index_latlon(&ngeom,&index,lat,lon,&status);
	return (status == 0 ? 0 : 1);
}
int
CdLatLonIndex(CdRegGeom *geom, CdOrder *order, double lat, double lon, long *index)
{
	REG_GEOM ngeom;
	int status;

	CdMapGeom(geom,order,&ngeom);
	latlon_index(&ngeom,&lat,&lon,index,&status);
	return (status == 0 ? 0 : 1);
}
