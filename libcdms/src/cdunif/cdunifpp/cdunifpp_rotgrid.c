/*
 *
 *    Copyright (C) 2004-2006 NERC DataGrid
 *    This software may be distributed under the terms of the
 *    CCLRC Licence for CCLRC Software
 * <CDATDIR>/External_License/CCLRC_CDAT_License.txt 
 *
 */

#ifdef HAVE_PP
#include "cdunifpp.h"


PProtmap *pp_get_rotmap(  const PPhdr *hdr, PPlist *rotmaplist, PPlist *heaplist )
{

  PProtmap *rotmap;

  if (pp_is_rotated_grid(hdr)) {
    
    CKP(  rotmap = pp_malloc( sizeof(PProtmap), heaplist)  );

    rotmap->pole_lon = hdr->BPLON;
    rotmap->pole_lat = hdr->BPLAT;
    rotmap->truepole_gridlon = 0.;
    rotmap->map_var = NULL;  /* we'll set this properly later */

    CKI(  pp_list_add_or_find( rotmaplist, &rotmap, pp_compare_rotmaps, 0, 
			       pp_free, NULL, heaplist )  );
    
    return rotmap;

  } else {
    return NON_ROTATED;
  }

  ERRBLKP("pp_get_rotmap");
}


PProtgrid *pp_get_rotgrid(  PPgenaxis *xaxis, PPgenaxis *yaxis, PPlist *rotgridlist, PPlist *heaplist )
{
  PProtmap *rotmap, *rotmap_y;
  PProtgrid *rotgrid;

  rotmap   = pp_genaxis_rotmap(xaxis);
  rotmap_y = pp_genaxis_rotmap(yaxis);
  ERRIF ( rotmap_y != rotmap );

  if (rotmap == NON_ROTATED)
    return NON_ROTATED_GRID;

  CKP(  rotgrid = pp_malloc(  sizeof(PProtgrid), heaplist)  );
  rotgrid->rotmap = rotmap;
  rotgrid->xaxis = xaxis;
  rotgrid->yaxis = yaxis;

  CKI(  pp_list_add_or_find( rotgridlist, &rotgrid, pp_compare_rotgrids, 0, 
			     pp_free, NULL, heaplist )  );  

  return rotgrid;

  ERRBLKP("pp_get_rotgrid");
}


/* --------------------------------------------------------------------- */


int pp_calc_rot_grid(PProtgrid *rotgrid, PPdata **lons_return, PPdata **lats_return, PPlist *heaplist)
{

  int nx, ny, i, j;
  int offset, offset1;
  PPgenaxis *xaxis;
  PPgenaxis *yaxis;

  PPdata *londata, *latdata, *rlondata, *rlatdata;

  Freal *lons, *lats, *rlons, *rlats; /* "r" stands for rotated */

  double latpole_rad, coslatpole, sinlatpole, cosrlat, sinrlat;
  double *cosdrlon, *sindrlon;
  double rlonN, lonpole, drlon_rad, dlon_rad, rlat_rad, lon;
  double cycdx, sinlat;
  

  const double dtor = M_PI / 180.;

  CKP(rotgrid);

  xaxis = rotgrid->xaxis;
  yaxis = rotgrid->yaxis;

  nx = pp_genaxis_len(xaxis);
  ny = pp_genaxis_len(yaxis);

  /* get input, output and workspace arrays */

  CKP(   rlondata = pp_genaxis_to_values(xaxis,0,heaplist)  );
  ERRIF(rlondata->type != realtype);

  CKP(   rlatdata = pp_genaxis_to_values(yaxis,0,heaplist)  );
  ERRIF(rlatdata->type != realtype);

  CKP(   londata = pp_data_new(realtype,nx*ny,heaplist)   );
  CKP(   latdata = pp_data_new(realtype,nx*ny,heaplist)   );
  CKP(   cosdrlon = pp_malloc(nx*sizeof(double),heaplist)  );
  CKP(   sindrlon = pp_malloc(nx*sizeof(double),heaplist)  );
  
  /* some pointers for convenience (and speed?) */
  rlons = (Freal*) rlondata->values;
  rlats = (Freal*) rlatdata->values;
  lons = londata->values; 
  lats = latdata->values;

  latpole_rad = rotgrid->rotmap->pole_lat * dtor;
  coslatpole = cos(latpole_rad);
  sinlatpole = sin(latpole_rad);
  
  rlonN = rotgrid->rotmap->truepole_gridlon;
  lonpole = rotgrid->rotmap->pole_lon;

  for (i=0; i<nx; i++) {

    drlon_rad = (rlons[i] - rlonN) * dtor;
    cosdrlon[i] = cos(drlon_rad);
    sindrlon[i] = sin(drlon_rad);
  }

  for (j=0; j<ny; j++) {

    rlat_rad = rlats[j] * dtor;
    cosrlat = cos(rlat_rad);
    sinrlat = sin(rlat_rad);

    offset1 = j*nx;

    for (i=0; i<nx; i++) {

      offset = offset1 + i;
      
      cycdx = cosrlat * cosdrlon[i];

      dlon_rad = atan2( -cosrlat*sindrlon[i], sinrlat*coslatpole - cycdx*sinlatpole );

      lon = (dlon_rad/dtor + lonpole);

      /* put in range 0 <= lon < 360
       * NOTE: This code previously put in range -180 to 180.
       *       The actual code was the following:
       *           lon -= lon_modulo * floor(lon / lon_modulo + 0.5);
       *       This was changed because the subsetting functions in CDAT
       *       didn't like the negative longitudes.
       */
      lon -= lon_modulo * floor(lon / lon_modulo);

      sinlat = cycdx * coslatpole + sinrlat * sinlatpole;
      if (sinlat > 1.)
	sinlat = 1.;
      else if (sinlat < -1.)
	sinlat = -1.;

      lons[offset] = lon;
      lats[offset] = asin(sinlat) / dtor;

    }
  }
  

  /* free workspace arrays */
  CKI(   pp_free(rlondata, heaplist)  );
  CKI(   pp_free(rlatdata, heaplist)  );
  CKI(   pp_free(cosdrlon, heaplist)  );
  CKI(   pp_free(sindrlon, heaplist)  );

  /* return pointers */
  if (lons_return != NULL)
    *lons_return = londata;

  if (lats_return != NULL)
    *lats_return = latdata;

  return 0;
  ERRBLKI("pp_calc_rot_grid");
}
#endif
