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

PPdata *pp_data_new(CuType type, int n, PPlist *heaplist)
{
  int size;
  PPdata *data;

  size = cutypelen(type);

  CKP(   data=pp_malloc(sizeof(PPdata),heaplist)   );
  CKP(   data->values=pp_malloc(n*size,heaplist)   );

  data->type = type;
  data->n = n;

  return data;

  ERRBLKP("pp_data_new");
}

PPdata *pp_regaxis_to_values(const PPregaxis *a, PPlist *heaplist)
{
  PPdata *data;
  int i;
  Freal val;

  CKP(   data=pp_data_new(realtype,a->n,heaplist)   );
  val = a->start;
  for (i=0; i < a->n; i++) {
    ((Freal*)(data->values))[i]=val;
    val += a->interval;
  }
  return data;
  
  ERRBLKP("pp_regaxis_to_values");
}

/*-----------------------------------------------------------------------------*/

PPdata *pp_zaxis_to_values(const PPzaxis *zaxis, PPlevvaltype vtype, PPlist *heaplist)
{
  PPdata *data;
  int i;
  int n;
  Freal val;
  PPlevel *lev;
  PPlist *list;
  PPlisthandle handle;

  list=zaxis->values;
  n=pp_list_size(list);

  CKP(   data=pp_data_new(realtype,n,heaplist)   );
  
  pp_list_startwalk(list,&handle);
  for (i=0; i < n; i++) {
    lev=pp_list_walk(&handle,0);
    
    switch (zaxis->lev_type) {

    case hybrid_sigmap_lev_type:
      switch (vtype) {
      case lev_type:
	val = lev->values.hybrid_sigmap.a / reference_pressure + lev->values.hybrid_sigmap.b;
	break;
      case hybrid_sigmap_a_type:
	val = lev->values.hybrid_sigmap.a;
	break;
      case hybrid_sigmap_b_type:
	val = lev->values.hybrid_sigmap.b;
	break;
      default:
	ERR;
      }
      break;

    case hybrid_height_lev_type:
      switch (vtype) {
      case lev_type:
	val = lev->values.hybrid_height.a;
	break;
      case hybrid_height_a_type:
	val = lev->values.hybrid_height.a;
	break;
      case hybrid_height_b_type:
	val = lev->values.hybrid_height.b;
	break;
      default:
	ERR;
      }
      break;

    case pseudo_lev_type:
      switch (vtype) {
      case lev_type:
	val = (Freal) lev->values.pseudo.index;
	break;
      default:
	ERR;
      }      
      break;

    default:
      switch (vtype) {
      case lev_type:
	val = lev->values.misc.level;
	break;
      default:
	ERR;
      }      
      break;
      
    }
      
    ((Freal*)(data->values))[i]=val;
  }
  return data;

  ERRBLKP("pp_zaxis_to_values");
}

PPdata *pp_taxis_to_values(const PPtaxis *taxis, PPlist *heaplist)
{
  PPdata *data;
  PPlist *list;
  PPtime *t;
  int mean;
  int i;
  int n;
  Freal val,val2;
  PPlisthandle handle;

  list=taxis->values;
  n=pp_list_size(list);

  CKP(   data=pp_data_new(realtype,n,heaplist)   );
  
  mean = pp_is_time_mean(taxis->type);

  pp_list_startwalk(list,&handle);
  for (i=0; i < n; i++) {
    t=pp_list_walk(&handle,0);

    val=pp_time_diff(t->type,
		     &t->time1,
		     &taxis->time_orig);

    if (mean) {

      /* FIXME(?): with difference field (time 2 - time 1), maybe want to do likewise and
       * use average time also?
       */

      /* FIXME(?): in the event of a seasonal composite, 
       *   (I think this is LBTIM==3 but doc not to hand at time of writing this), 
       * time1 is meaning start date and first year, time2 is meaning start date and end year;
       * in event of even number of years, the date could end up 6 months out from the time
       * of year (e.g. Jan mean over ten years 2000-2009 becomes average of 1 Jan 2000 and 
       * 1 Feb 2009, which is mid-July 2004 -- maybe more helpful to report e.g. mid-Jan 2004?)
       */

      val2=pp_time_diff(t->type,
			&t->time2,
			&taxis->time_orig);

      val = (val + val2) / 2;
    }

    ((Freal*)(data->values))[i]=val;
  }
  return data;
  
  ERRBLKP("pp_taxis_to_values");
}

PPdata *pp_taxis_to_boundary_values(const PPtaxis *taxis, PPlist *heaplist)
{
  PPdata *data;
  PPlist *list;
  PPtime *t;
  int i;
  int n;
  Freal val,val2;
  PPlisthandle handle;

  /* only valid for time mean */
  ERRIF(!pp_is_time_mean(taxis->type));

  list=taxis->values;
  n=pp_list_size(list);

  CKP(   data=pp_data_new(realtype,n*2,heaplist)   );
  
  pp_list_startwalk(list,&handle);
  for (i=0; i < n; i++) {
    t=pp_list_walk(&handle,0);

    val=pp_time_diff(t->type,
		     &t->time1,
		     &taxis->time_orig);

    val2=pp_time_diff(t->type,
		      &t->time2,
		      &taxis->time_orig);

    ((Freal*)(data->values))[ 2*i ]=val;

    ((Freal*)(data->values))[ 2*i+1 ]=val2;
  }
  return data;

  ERRBLKP("pp_taxis_to_boundary_values");
}


/*-----------------------------------------------------------------------------*/

/* FIXME - there is also a function pp_var_is_time_mean in cdunifpp_varinfo.c
 * which applies a different test.  Investigate exactly how these are called,
 * and if there should be two different functions then document why, otherwise
 * merge.
 */
int pp_is_time_mean(Fint LBTIM) {
  int ib;
  ib = (LBTIM / 10) % 10;
  return (ib == 2) || (ib == 3);
}


int pp_grid_supported(const PPhdr *hdrp) {
  Fint gridcode;
  gridcode=pp_get_var_gridcode(hdrp);

  switch(gridcode) {

  case 1:
  case 101:
  case 11110:
    return 1;

  default:
    return 0;
  }
}

int pp_axis_regular(const PPextravec extra, const PPrec *rec, const PPfile *ppfile) {
  const PPhdr *hdrp;
  Fint gridcode;

  hdrp=&rec->hdr;

  gridcode=pp_get_var_gridcode(hdrp);
  if (gridcode == 1) return 1;
  if (gridcode == 101) return 1;
  if (gridcode/10000 == 1) {
    if (pp_extra_has_vector(extra,rec,ppfile) == 0) return 1;
  }
  return 0;
}

int pp_is_rotated_grid(const PPhdr *hdrp) {

  Fint gridcode;

  gridcode = pp_get_var_gridcode(hdrp);

  return (gridcode / 100 == 1);
}


Freal pp_mean_period(const PPtime *time) {
  /* returns the averaging period in days, or 0. if it is not a mean field */
  Fint lbtim = time->type;
  if (!pp_is_time_mean(lbtim)) {
    return 0.;
  }
  return pp_time_diff(lbtim, &time->time2, &time->time1);
}

/* float_time returns time in days since origin time */

Freal pp_time_diff(Fint LBTIM, const PPdate *date, const PPdate *orig_date)
{
  long long secs;

  switch(pp_calendar_type(LBTIM)) {
  case gregorian:
    return pp_sec_to_day(pp_gregorian_to_secs(date) - pp_gregorian_to_secs(orig_date));
    break; /* notreached */
  case cal360day:
    secs =
      date->second - orig_date->second + 
      60 * (date->minute - orig_date->minute + 
	    60 * (date->hour - orig_date->hour + 
		  24 * (date->day - orig_date->day + 
			30 * (date->month - orig_date->month +
			      12 * (long long) (date->year - orig_date->year) ))));

    return pp_sec_to_day(secs);
    break; /* notreached */
  case model:
    secs =
      date->second - orig_date->second + 
      60 * (date->minute - orig_date->minute + 
	    60 * (date->hour - orig_date->hour + 
		  24 * (long long) (date->day - orig_date->day)));

    return pp_sec_to_day(secs);
    break; /* notreached */

  default:
    pp_switch_bug("pp_time_diff");
    ERR;
  }

  ERRBLKF("pp_time_diff");
}


Freal pp_sec_to_day(long long seconds) {
  /* convert seconds to days, avoiding rounding where possible 
   * by using integer arithmetic for the whole days
   */
  const int secs_per_day = 86400;
  
  long long days, remainder;
  days = seconds / secs_per_day;
  remainder = seconds % secs_per_day;

  return days + remainder / (Freal) secs_per_day;
}


PPcalendartype pp_calendar_type(Fint type){

  switch(type%10) {

  case 0:
    /* fallthrough */
  case 3:
    return model;
    break; /* notreached */
  case 1:
    return gregorian;
    break; /* notreached */
  case 2:
    return cal360day;
    break; /* notreached */
  default:
    pp_switch_bug("pp_calendar_type");
    ERR;    
  }

  /* on error return -1 (though only useful to calling routine if stored in an int
   * not a PPcalendartype)
   */
  ERRBLKI("pp_calendar_type");
}

/*
 *  This superseded routine uses builtin time functions; these have a range 1902 - 2038 (approx).
 *  
 *  Can be with respect to any arbitary origin, because return values from this are differenced.  
 *  Note that this superseded routine had a different time origin from the one in use below.
 *      
 * long long pp_gregorian_to_secs(const PPdate *date)
 * {
 * 
 *     time_t t;
 *     struct tm tm;
 * 
 *     tm.tm_year = date->year - 1900;
 *     tm.tm_mon = date->month - 1;
 *     tm.tm_mday = date->day ;
 *     tm.tm_hour = date->hour ;
 *     tm.tm_min = date->minute ;
 *     tm.tm_sec = date->second ;
 * 
 *     t=mktime(&tm);
 * 
 *     return (long long) t;
 * }
 */

long long pp_gregorian_to_secs(const PPdate *date)
{
  /* Convert from Gregorian calendar to seconds since a fixed origin
   * 
   * Can be with respect to any arbitary origin, because return values from this are
   * differenced.
   *
   * Arbitrary origin is what would be 1st Jan in the year 0 if hypothetically the
   * system was completely consistent going back this far.  This simplifies the 
   * calculation.
   * 
   * Strictly, this is proleptic_gregorian rather than gregorian (see CF docs)
   * as this is more likely to match the code actually in the model.  The UM
   * docs call it "gregorian" but I'm speculating (without checking model code)
   * that the model really doesn't have all that jazz with Julian calendar
   * before fifteen-something.
   */

  const int sid = 86400; /* seconds in day */
  const int sih = 3600;
  const int sim = 60;

  long long nsec;
  int year,nleap,nday,isleap;

  /* offsets from 1st Jan to 1st of each month in non-leap year */
  int dayno[12] = { 0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334 };

  year = date->year;

  /* is the year leap? */
  if (year % 400 == 0) isleap=1;
  else if (year % 100 == 0) isleap=0;
  else if (year % 4 == 0) isleap=1;
  else isleap=0;

  /* nleap is number of 29th Febs passed between origin date and supplied date. */
  nleap = year/4 - year/100 + year/400;
  if (isleap && date->month <= 2)
    nleap--;

  nday = (year * 365) + dayno[date->month - 1] + (date->day - 1) + nleap;

  nsec = (long long) nday * sid + date->hour * sih + date->minute * sim + date->second;

  return nsec;
}

char *pp_t_units(const PPtaxis *taxis, PPlist *heaplist)
{
  char *units;

  const int string_length = 30;
  const char *fmt = "days since %04d-%02d-%02d %02d:%02d:%02d";

  const PPdate *orig;

  orig = &taxis->time_orig;

  CKP(   units=pp_malloc(string_length+1,heaplist)    );

  snprintf(units,string_length+1,fmt,
	   orig->year, orig->month, orig->day,
	   orig->hour, orig->minute, orig->second);

  return units;

  ERRBLKP("pp_t_units");
}

#endif
