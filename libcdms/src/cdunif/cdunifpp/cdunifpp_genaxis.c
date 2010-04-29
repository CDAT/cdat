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

/*
 *
 *  Routines to deal with generic axes
 *  JAK 2005-01-06
 *
 * Note: much of this code could replace multiple switch with dispatch functions.
 *       this has been left until later, until requirements of coping with psuedo levels
 *       and arbitary cross sections has been accommodated for.
 */

#define checkaxistype(A, B) if (A->gentype != B) {pp_error("axis type error"); ERR;}
#define getregaxis(A, B) B=(PPregaxis *) (A)->axis
#define getzaxis(A, B)   B=(PPzaxis *) (A)->axis
#define gettaxis(A, B)   B=(PPtaxis *) (A)->axis
#define getxsaxis(A, B)  B=(PPxsaxis *) (A)->axis

PPgenaxis *pp_genaxis_new(const PPaxisgentype type, const PPdir dir, PPlist *heaplist) {  

  PPgenaxis *axis;
  PPregaxis *raxis;
  PPtaxis *taxis;
  PPzaxis *zaxis;
  /* PPxsaxis *xsaxis;   unused at present - commented to silence gcc -Wall */

  CKP(  axis=pp_malloc(sizeof(PPgenaxis),heaplist)  );

  axis->gentype=type;
  axis->dir=dir;
  axis->dimid=UNSET_INT;


  switch (type) {
  case regaxis_type:
    CKP(  axis->axis=pp_malloc(sizeof(PPregaxis),heaplist)  );
    getregaxis(axis,raxis);
    raxis->type=UNSET_INT;
    raxis->n=UNSET_INT;
    raxis->start=UNSET_INT;
    raxis->interval=UNSET_INT;
    break;
  case zaxis_type:
    if (dir != zdir) pp_error_mesg("pp_genaxis_new","z type-direction mismatch");
    CKP(  axis->axis=pp_malloc(sizeof(PPzaxis),heaplist)  );
    zaxis=(PPzaxis *) axis->axis;
    CKP(  zaxis->values=pp_list_new(heaplist)  );
    break;
  case taxis_type:
    if (dir != tdir) pp_error_mesg("pp_genaxis_new","t type-direction mismatch");
    CKP(  axis->axis=pp_malloc(sizeof(PPtaxis),heaplist)  );
    taxis=(PPtaxis *) axis->axis;
    CKP( taxis->values=pp_list_new(heaplist)  );
    break;
  case xsaxis_type:
    CKP(  axis->axis=pp_malloc(sizeof(PPxsaxis),heaplist)  );
    break;
  default:
    pp_switch_bug("pp_genaxis_new");
    ERR;
  }

  return axis;
  
  ERRBLKP("pp_new_genaxis");
}

int pp_genaxis_free(PPgenaxis *axis, PPlist *heaplist) {
  PPzaxis *zaxis;
  PPtaxis *taxis;
  PPxsaxis *xsaxis;

  CKP(axis);

  switch (axis->gentype) {
  case regaxis_type:
    break;
  case zaxis_type:
    getzaxis(axis,zaxis);
    CKI(  pp_list_free(zaxis->values,1,heaplist)  );
    break;
  case taxis_type:
    gettaxis(axis,taxis);
    CKI(  pp_list_free(taxis->values,1,heaplist)  );
    break;
  case xsaxis_type:
    getxsaxis(axis,xsaxis);
    CKI(  pp_free(xsaxis->data->values,heaplist)  );
    CKI(  pp_free(xsaxis->data,heaplist)  );
    break;
  default: 
    pp_switch_bug("pp_genaxis_free");
    ERR;
  }

  CKI(  pp_free(axis->axis,heaplist)  );
  CKI(  pp_free(axis,heaplist)  );

  return 0;
  
  ERRBLKI("pp_free_genaxis");
}

int pp_genaxis_compare(const void *p1, const void *p2 ) {
  const PPgenaxis *a = *(PPgenaxis **)p1;
  const PPgenaxis *b = *(PPgenaxis **)p2;
  int cmp;

  if ((cmp=pp_compare_ints(a->gentype, b->gentype)) != 0) return cmp;
  if ((cmp=pp_compare_ints(a->dir, b->dir)) != 0) return cmp;

  switch (a->gentype) {
  case regaxis_type:
    return pp_compare_regaxes(&a->axis, &b->axis);
  case zaxis_type:
    return pp_compare_zaxes(&a->axis, &b->axis);
  case taxis_type:
    return pp_compare_taxes(&a->axis, &b->axis);
  case xsaxis_type:
    return pp_compare_xsaxes(&a->axis, &b->axis);
  default:
      pp_switch_bug("pp_genaxis_compare");
      ERR;
  }

  ERRBLKI("pp_genaxis_compare");
}

PPdata *pp_genaxis_to_values(const PPgenaxis *axis, int valtype, PPlist *heaplist) {

  /* valtype is an direction-dependent value specifying type of axis value
   * currently it is only used for vertical axis, for everything else it is ignored
   */

  PPlevvaltype levvaltype;

  switch (axis->gentype) {
  case regaxis_type:
    return pp_regaxis_to_values((PPregaxis *) axis->axis, heaplist);
  case zaxis_type:
    levvaltype = valtype;
    return pp_zaxis_to_values((PPzaxis *) axis->axis, levvaltype, heaplist); 
  case taxis_type:
    return pp_taxis_to_values((PPtaxis *) axis->axis, heaplist);
  case xsaxis_type:
    return pp_xsaxis_to_values((PPxsaxis *) axis->axis, heaplist);
  default:
    pp_switch_bug("pp_genaxis_to_values");
    ERR;
  }
  
  ERRBLKP("pp_genaxis_to_values");
}

Fint pp_genaxis_len(const PPgenaxis *axis) {
  Fint len;
  PPregaxis *raxis;
  PPzaxis *zaxis;
  PPtaxis *taxis;
  PPxsaxis *xsaxis;

  switch (axis->gentype) {
  case regaxis_type:
    getregaxis(axis,raxis);
    len=raxis->n;
    break;
  case zaxis_type:
    getzaxis(axis,zaxis);
    len=pp_list_size(zaxis->values);
    break;
  case taxis_type:
    gettaxis(axis,taxis);
    len=pp_list_size(taxis->values);
    break;
  case xsaxis_type:
    getxsaxis(axis,xsaxis);
    len=xsaxis->data->n;
    break;
  default:
    pp_switch_bug("pp_genaxis_len");
    ERR;
  }
  return len;

  ERRBLKI("pp_genaxis_len");
}

PProtmap *pp_genaxis_rotmap(const PPgenaxis *axis)
{
  PPregaxis *raxis;
  PProtmap *rotmap;
  switch (axis->gentype) {
  case regaxis_type:
    getregaxis(axis,raxis);
    rotmap=raxis->rotmap;
    break; /*notreached*/
  case xsaxis_type:
    /* encountering cross-section type (e.g. used for irregular latitudes)
     * is not an error condition, but we'll assume it's non-rotated 
     */
    rotmap=NON_ROTATED;
    break;
  default:
    /* any unexpected axis type is an error condition; report error 
     * and return NULL */
    ERR;
  }
  return rotmap;

  ERRBLKP("pp_genaxis_rotmap");
}


PPdata *pp_genaxis_getCF(const PPgenaxis *axis, char *yname, char *yunits, PPlist *atts, PPlist *heaplist) {
  /* pp_genaxis_getCF: takes a genaxis and returns the relevant CF compliant data. 
   *                   Also sets the dimension name, units, and some attributes.
   *
   *    axis is the input;  yname, yunits, atts are all outputs
   */

  PPdata *data;
  PPzaxis *zaxis;
  PPtaxis *taxis;
  PPregaxis *regaxis;
  PPxsaxis *xsaxis;

  int ilev;
  int iaxis;
  char *name, *units;
  char *tunits, *calatt;
  char *pointspacing, *standardname, *axisname, *posdir, *longname;

  /* constants copied into local variables */
  /*  Freal lon_modulo = longitude_modulo; */
  Freal plev_scale = pressure_scaling;

  pointspacing = standardname = longname = axisname = posdir = units = NULL;

  /* for z axes, pp_genaxis_to_values has an extra argument we care about.
   * for other axis types, set it to something arbitary.
   *
   * (And yes, lev_type happens to equal 0.  But ... :-)
   */
  switch (axis->gentype) {
  case zaxis_type:
    CKP(  data = pp_genaxis_to_values(axis,lev_type,heaplist)  );
    break;
  default:
    CKP(  data = pp_genaxis_to_values(axis,0,heaplist)  );
    break;
  }
  /* FIXME - could add longname for some of the following.
   * (Though stdnames are the most important)
   */
    
  switch(axis->gentype){
  case regaxis_type:
    regaxis=(PPregaxis *) axis->axis;
    switch (regaxis->type){
    case xregtype:
      if (regaxis->rotmap == NON_ROTATED) {
	name="longitude%d";
	units="degrees_east";
	standardname="longitude";
      } else {
	name="rlon%d";
	units="degrees";
	standardname="grid_longitude";
	longname="longitude in rotated pole grid";
      }
      axisname="X";
      pointspacing="even";
      CKI(   pp_add_att(atts,"modulo",realtype,1,&lon_modulo,heaplist)   );
      break;
    case yregtype:
      if (regaxis->rotmap == NON_ROTATED) {
	name="latitude%d";
	units="degrees_north";
	standardname="latitude";
      } else {
	name="rlat%d";
	units="degrees";
	standardname="grid_latitude";
	longname="latitude in rotated pole grid";	
      }
      axisname="Y";
      pointspacing="even";
      break;
    default:
      pp_switch_bug("pp_genaxis_getCF");
      ERR;
    }
    break;
   
  case zaxis_type:
    getzaxis(axis,zaxis);
    axisname="Z";
    switch (zaxis->lev_type) {
      
    case pseudo_lev_type:
      longname = "pseudo-level (non-level coordinate)";
      name="z_pseudo%d";
      axisname=NULL;
      break;
    case height_lev_type:
      longname = "height above surface";
      standardname="height";
      units="m";
      name="z%d_height";
      posdir="up";
      break;
    case depth_lev_type:
      name="z%d_depth";
      longname = "depth below surface";
      standardname="depth";
      units="m";
      posdir="down";
      break;
    case hybrid_sigmap_lev_type:
      longname = "atmosphere hybrid sigma-pressure coordinate";
      standardname="atmosphere_hybrid_sigma_pressure_coordinate";
      name="z%d_hybrid_sigmap";
      posdir="down";
      break;
    case hybrid_height_lev_type:
      longname = "atmosphere hybrid height coordinate";
      standardname="atmosphere_hybrid_height_coordinate";
      name="z%d_hybrid_height";
      units="m";
      posdir="up";
      break;
    case pressure_lev_type:
      longname = "air pressure (vertical level)";
      standardname="air_pressure";
      units="Pa";
      name="z%d_p_level";
      
      /* scale the values to Pa */
      for (ilev=0; ilev<data->n; ilev++)
	((Freal *)(data->values))[ilev] *= plev_scale;
      
      posdir="down";
      break;
    case soil_lev_type:
      longname = "soil level";
      name="z%d_soil";
      posdir="up";
      break;
    case boundary_layer_top_lev_type:
      longname = "top of boundary layer (dummy level coordinate)";
      name="z%d_bdylyrtop";
      break;
    case top_of_atmos_lev_type:
      longname = "top of atmosphere (dummy level coordinate)";
      name="z%d_toa";
      break;
    case mean_sea_lev_type:
      longname = "mean sea level (dummy level coordinate)";
      name="z%d_msl";
      break;
    case tropopause_lev_type:
      longname = "tropopause (dummy level coordinate)";
      name="z%d_tropo";
      break;
    case surface_lev_type:
      longname = "surface (dummy level coordinate)";
      name="z%d_surface";
      break;
    case other_lev_type:
    default:
      longname = "level coordinate of unspecified type";
      name="z%d_level";
      break;      
    }
    break;
    
  case taxis_type:
    name="time%d";
    gettaxis(axis,taxis);
    switch(pp_calendar_type(taxis->type)) {
    case gregorian:
      calatt="proleptic_gregorian"; /* see pp_gregorian_to_secs() */
      break;
    case cal360day:
      calatt="360_day";
      break;
    case model:
    default:
      calatt="none";
      break;      
    }
    
    CKI(   pp_add_string_att(atts,"calendar",
			    calatt,
			    heaplist)   );
    
    CKP(   tunits=pp_t_units(taxis,heaplist)   );
    axisname="T";
    standardname="time";
    units=tunits;
    break;	

  case xsaxis_type:
    getxsaxis(axis,xsaxis);
    iaxis = xsaxis->axiscode;
    switch (iaxis) {
    case 11:
      name="longitude%d";
      units="degrees_east";
      axisname="X";
      standardname="longitude";
      CKI(   pp_add_att(atts,"modulo",realtype,1,&lon_modulo,heaplist)   );
      break;
    case 10:
      name="latitude%d";
      units="degrees_north";
      axisname="Y";
      standardname="latitude";
      break;
    default:
      CKP(   name = pp_malloc(15, heaplist)   );
      sprintf(name,"axis%d%%d",iaxis);
      units="tobefilled";
    }
    break;
  
  default:
    pp_switch_bug("pp_genaxis_CFget");
    ERR;
  }

  if (units != NULL) 
    CKI(   pp_add_string_att(atts,"units",units,heaplist)   );
  
  if (axisname != NULL)
    CKI(   pp_add_string_att(atts,"axis",axisname,heaplist)   );
  
  if (standardname != NULL)
    CKI(   pp_add_string_att(atts,"standard_name",standardname,heaplist)   );
  
  if (longname != NULL)
    CKI(   pp_add_string_att(atts,"long_name",longname,heaplist)   );
  
  if (pointspacing != NULL)
    CKI(  pp_add_string_att(atts,"point_spacing",pointspacing,heaplist)   );
  
  if (posdir != NULL)
    CKI(   pp_add_string_att(atts,"positive",posdir,heaplist)   );

  strcpy(yunits,"");
  if (units != NULL) strcpy(yunits,units);
  strcpy(yname,name);

  return data;

  ERRBLKP("pp_genaxis_CFget");
}

int pp_genaxis_print(const PPgenaxis *axis, const char *name) {

  PPregaxis *raxis;

  printf(" NAME: %s\n", name);
  printf(" dimid: %d\n", axis->dimid);
  printf(" gentype: %d\n",axis->gentype);
  switch (axis->gentype) {
  case regaxis_type:
    getregaxis(axis,raxis);
    printf("  type: %d\n  n: %d\n  start: %f\n  interval: %f\n", raxis->type, raxis->n, raxis->start, raxis->interval);
    break;
  case zaxis_type:
    ;
    break;
  case taxis_type:
    ;
    break;
  default:
    pp_switch_bug("pp_genaxis_print");
    ERR;
  }
  return 0;

  ERRBLKI("pp_genaxis_print");
}

PPdata *pp_xsaxis_to_values(const PPxsaxis *axis, PPlist *heaplist) {
  return axis->data;
}

int pp_xsaxis_set(PPgenaxis *axis, const PPrec *rec, const PPfile *ppfile, const PPextravec extra, PPlist *heaplist) {

  PPxsaxis *xsaxis;
  const PPhdr *hdrp;
  Fint gridcode;
  int ix, iy;

  checkaxistype(axis,xsaxis_type);
  getxsaxis(axis,xsaxis);
  hdrp=&rec->hdr;

  gridcode=pp_get_var_gridcode(hdrp);

  if (gridcode/10000 == 1) {
    ix = (gridcode/100) % 100;
    iy = gridcode % 100;
    if (extra == extra_x) {
      xsaxis->axiscode = ix;
    } else {
      xsaxis->axiscode = iy;
    }
    CKP(  xsaxis->data=pp_read_extradata(rec, ppfile, heaplist, extra)  );
    return xsaxis->data->n;
  } else {
    xsaxis->data=NULL;
    return 0;
  }

  ERRBLKI("pp_xsaxis_set");
}

PPlevtype pp_zaxis_lev_type(const PPgenaxis *axis) {
  
  PPzaxis *zaxis;

  checkaxistype(axis,zaxis_type);  
  getzaxis(axis,zaxis);

  return zaxis->lev_type;

  ERRBLKI("pp_zaxis_lev_type");
}

int pp_zaxis_set(PPgenaxis *axis, const PPhdr *hdr) {

  PPzaxis *zaxis;

  checkaxistype(axis,zaxis_type);
  getzaxis(axis,zaxis);

  zaxis->lbvc=hdr->LBVC;
  zaxis->lev_type = pp_level_type(hdr);
  return 0;

  ERRBLKI("pp_zaxis_set");
}

PPlevtype pp_level_type(const PPhdr *hdr) {

  if (hdr->LBUSER5 != 0 
      && hdr->LBUSER5 != int_missing_data)
    return pseudo_lev_type;
  
  switch (hdr->LBVC){

    /*
     *         1  Height (m)              8  Pressure (mb)
     *         9  Hybrid co-ordinates    10  Sigma (=p/p*)
     *         128  Mean sea level      129  Surface
     *         130  Tropopause level    131  Maximum wind level
     *         132  Freezing level      142  Upper hybrid level
     *         143  Lower hybrid level  176  Latitude (deg)
     *         177  Longitude (deg)
     */
    /* also new dynamics:  65  hybrid height */

  case 1:
    return height_lev_type;
  case 2:
    return depth_lev_type;
  case 5:
    return boundary_layer_top_lev_type;
  case 6:
    return soil_lev_type;
  case 8:
    return pressure_lev_type;
  case 9:
    return hybrid_sigmap_lev_type;
  case 65:
    return hybrid_height_lev_type;
  case 128:
    return mean_sea_lev_type;
  case 129:
    return surface_lev_type;
  case 130:
    return tropopause_lev_type;
  case 133:
    return top_of_atmos_lev_type;
  default:
    return other_lev_type;
  }
}


/* pp_zaxis_add adds a level to the z axis.
 *
 * Return values are analogous to pp_taxis_add, see below.
 */

int pp_zaxis_add(PPgenaxis *axis, const PPlevel *lev, int *index_return, PPlist *heaplist) {
  PPlevel *levcopy;
  PPzaxis *zaxis;

  checkaxistype(axis,zaxis_type);
  getzaxis(axis,zaxis);

  CKP(   levcopy = pp_dup(lev, sizeof(PPlevel), heaplist)   );
  return pp_list_add_or_find(zaxis->values, &levcopy, pp_compare_levels, 0, pp_free, index_return, heaplist);

  ERRBLKI("pp_zaxis_add");
}

int pp_taxis_set(PPgenaxis *axis, const PPhdr *hdrp) {
  PPtaxis *taxis;

  checkaxistype(axis,taxis_type);
  gettaxis(axis,taxis);
  taxis->type=hdrp->LBTIM;

  /*=== set reference time -- 
   *
   *  FIXME (?) for forecast fields could possibly change this to use
   *            the initialisation time (though that is not necessarily desirable)
   */

  if (pp_calendar_type(taxis->type) == model) {
    taxis->time_orig.year    = hdrp->LBYR; 
    taxis->time_orig.month   = hdrp->LBMON;
    taxis->time_orig.day     = hdrp->LBDAT;
    taxis->time_orig.hour    = hdrp->LBHR;
    taxis->time_orig.minute  = hdrp->LBMIN;
    taxis->time_orig.second  = 0;
  } else {
    taxis->time_orig.year  = default_year_orig; 
    taxis->time_orig.month = default_month_orig;
    taxis->time_orig.day   = default_day_orig;
    taxis->time_orig.hour  = default_hour_orig;
    taxis->time_orig.minute= default_minute_orig;
    taxis->time_orig.second= default_second_orig;
  }
  return 0;

  ERRBLKI("pp_taxis_set");
}

/* pp_taxis_add adds a time to the t axis.
 *
 * Return values: 
 *   0  time already existed in axis
 *   1  time has been added to axis
 *  -1  an error occurred (probably in memory allocation)
 *
 * NOTE: the return value of this function may be tested with the CKI() macro.
 * Do not add non-error cases with negative return values.
 */
int pp_taxis_add(PPgenaxis *axis, const PPtime *time, int *index_return, PPlist *heaplist) {  
  PPtime *timecopy;
  PPtaxis *taxis;

  checkaxistype(axis,taxis_type);
  gettaxis(axis,taxis);

  /* use a copy of the input PPtime structure; this is because the input is supposed to be const,
   * and specifically is something that we might not want to free when we do pp_list_free.
   */

  CKP(   timecopy = pp_dup(time, sizeof(PPtime), heaplist)   );
  return pp_list_add_or_find(taxis->values, &timecopy, pp_compare_times, 0, pp_free, index_return, heaplist);

  ERRBLKI("pp_taxis_add");
}

int pp_taxis_is_time_mean(PPgenaxis *axis) {
  PPtaxis *taxis;

  checkaxistype(axis,taxis_type);
  gettaxis(axis,taxis);

  return pp_is_time_mean(taxis->type);

  ERRBLKI("pp_taxis_is_time_mean");
}

int pp_regaxis_set(PPgenaxis *ax, PPaxisregtype type, const PPhdr *hdr, PPlist *rotmaplist, PPlist *heaplist) {

  PPregaxis *axis;

  getregaxis(ax,axis);

  switch(type) {
  case xregtype:
    axis->start    = hdr->BZX + hdr->BDX;
    axis->interval = hdr->BDX;
    axis->n        = hdr->LBNPT;
    break;
  case yregtype:
    axis->start    = hdr->BZY + hdr->BDY;
    axis->interval = hdr->BDY;
    axis->n        = hdr->LBROW;
    break;
  }

  axis->type=type;

  CKP(  axis->rotmap = pp_get_rotmap(hdr,rotmaplist,heaplist)  );

  return 0;

  ERRBLKI("pp_regaxis_set");
}

int pp_set_horizontal_axes(PPrec *recp, PPfile *ppfile, 
			   PPgenaxis **xaxis_return, PPgenaxis **yaxis_return,
			   PPlist *rotmaps, PPlist *heaplist)
{
  PPgenaxis *xaxis, *yaxis;
  PPhdr *hdrp;

  CKP(xaxis_return);
  CKP(yaxis_return);
  CKP(recp);

  hdrp=&recp->hdr;

  if (pp_axis_regular(extra_x,recp,ppfile) == 1) {
    CKP(   xaxis=pp_genaxis_new(regaxis_type,xdir,heaplist)   );
    CKI(   pp_regaxis_set(xaxis,xregtype,hdrp,rotmaps,heaplist)  );
  } else {
    CKP(   xaxis=pp_genaxis_new(xsaxis_type,xdir,heaplist)   );
    CKI(  pp_xsaxis_set(xaxis,recp,ppfile,extra_x,heaplist)  );
  }
	
  if (pp_axis_regular(extra_y,recp,ppfile) == 1) {
    CKP(   yaxis=pp_genaxis_new(regaxis_type,ydir,heaplist)   );
    CKI(   pp_regaxis_set(yaxis,yregtype,hdrp,rotmaps,heaplist)  );
  } else {
    CKP(   yaxis=pp_genaxis_new(xsaxis_type,ydir,heaplist)   );
    CKI(  pp_xsaxis_set(yaxis,recp,ppfile,extra_y,heaplist)  );
  }

  *xaxis_return=xaxis;
  *yaxis_return=yaxis;
  return 0;

  ERRBLKI("pp_set_horizontal_axes");
}

/* Some helper functions for pp_var_get_*axis  -- can make them public if there is
 * ever a need to call them from other routines
 */

static int pp_genaxis_is_regaxis_of_type(const PPgenaxis *axis, PPaxisregtype type) {
  PPregaxis *regaxis;
  if (axis->gentype != regaxis_type)
    return 0;
  getregaxis(axis, regaxis);
  return (regaxis->type == type);
}

static int pp_genaxis_is_x(const PPgenaxis *axis) {
  return pp_genaxis_is_regaxis_of_type(axis, xregtype);
}

static int pp_genaxis_is_y(const PPgenaxis *axis) {
  return pp_genaxis_is_regaxis_of_type(axis, yregtype);
}

static int pp_genaxis_is_z(const PPgenaxis *axis) {
  return axis->gentype == zaxis_type;
}

static int pp_genaxis_is_t(const PPgenaxis *axis) {
  return axis->gentype == taxis_type;
}

static PPgenaxis *pp_get_axis_satisfying(const PPlist *axes, int (*test)(const PPgenaxis *)) {
  PPlisthandle handle;
  PPgenaxis *axis;

  pp_list_startwalk(axes, &handle);
  while ((axis = pp_list_walk(&handle, 0)) != NULL)
    if (test(axis))
      return axis;
  return NULL;
}

/* ---------------------- */

PPgenaxis *pp_get_xaxis_from_list(const PPlist *axes) {
  return pp_get_axis_satisfying(axes, pp_genaxis_is_x);
}
PPgenaxis *pp_get_yaxis_from_list(const PPlist *axes) {
  return pp_get_axis_satisfying(axes, pp_genaxis_is_y);
}
PPgenaxis *pp_get_zaxis_from_list(const PPlist *axes) {
  return pp_get_axis_satisfying(axes, pp_genaxis_is_z);
}
PPgenaxis *pp_get_taxis_from_list(const PPlist *axes) {
  return pp_get_axis_satisfying(axes, pp_genaxis_is_t);
}

/* ---------------------- */


/* these routines should probably go somewhere else */

int pp_lev_set(PPlevel *lev, PPhdr *hdrp) {
  
  lev->type         = pp_level_type(hdrp);

  switch (lev->type) {

  case hybrid_height_lev_type:
    lev->values.hybrid_height.a = hdrp->BLEV;
    lev->values.hybrid_height.b = hdrp->BHLEV;
#ifdef BDY_LEVS
    lev->values.hybrid_height.ubdy_a = hdrp->BULEV;
    lev->values.hybrid_height.ubdy_b = hdrp->BHULEV;
    lev->values.hybrid_height.lbdy_a = hdrp->BRLEV;
    lev->values.hybrid_height.lbdy_b = hdrp->BHRLEV;
#endif
    break;

  case hybrid_sigmap_lev_type:
    lev->values.hybrid_sigmap.a = hdrp->BHLEV;
    lev->values.hybrid_sigmap.b = hdrp->BLEV;
#ifdef BDY_LEVS
    lev->values.hybrid_sigmap.ubdy_a = hdrp->BHULEV;
    lev->values.hybrid_sigmap.ubdy_b = hdrp->BULEV;
    lev->values.hybrid_sigmap.lbdy_a = hdrp->BHRLEV;
    lev->values.hybrid_sigmap.lbdy_b = hdrp->BRLEV;
#endif
    break;

  case pseudo_lev_type:
    lev->values.pseudo.index = hdrp->LBUSER5;
    break;

  default:
    if (hdrp->BLEV == 0 
	&& hdrp->LBLEV != 9999
	&& hdrp->LBLEV != 8888) 
      lev->values.misc.level = hdrp->LBLEV;
    else
      lev->values.misc.level = hdrp->BLEV;
#ifdef BDY_LEVS
    lev->values.misc.ubdy_level = hdrp->BULEV;
    lev->values.misc.lbdy_level = hdrp->BRLEV;
#endif
    break;
  }
  return 0;
}

int pp_time_set(PPtime *time, PPhdr *hdrp) {

  time->type    = hdrp->LBTIM;
  
  time->time1.year    = hdrp->LBYR; /* JAK set_time(&(time->time1),dates) */
  time->time1.month   = hdrp->LBMON;
  time->time1.day     = hdrp->LBDAT;
  time->time1.hour    = hdrp->LBHR;
  time->time1.minute  = hdrp->LBMIN;
  time->time1.second  = 0;
  
  time->time2.year    = hdrp->LBYRD; /* JAK set_time(&(time->time2),dates) */
  time->time2.month   = hdrp->LBMOND;
  time->time2.day     = hdrp->LBDATD;
  time->time2.hour    = hdrp->LBHRD;
  time->time2.minute  = hdrp->LBMIND;
  time->time2.second  = 0;

  return 0;
}

#endif
