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
 * COMPARISON FUNCTIONS.
 *
 * NOTE: functions which take arguments of type void* (except for pp_compare_ptrs)
 *   are designed to be used with generic routines:
 *   pp_compare_records is envisaged for use with qsort;
 *   several other functions are envisaged for use with pp_compare_lists (below).
 *
 *   In these cases if supplying pointers directly to the relevant structures, need to generate
 *   an extra level of pointer with "&" syntax.
 *
 * But not all functions below are like that.  Don't assume functions can be used analogously
 * without first examining the argument lists.
 */

/* The code profiler suggests that pp_compare_ints and pp_compare_reals are candidates for
 * inlining; however, unfortunately this sometimes gets compiled with c89 which doesn't support
 * inline functions.  Use a #define for pp_compare_ints.  pp_compare_reals, which is more
 * awkward to #define, is just going to have to stay as it is for now (it's called less often).
 */

/* Now included in cdunifpp.h
 * #define pp_compare_ints(a,b) ((a)<(b)?(-1):(a)>(b)?1:0)
 */

/*
 * static int pp_compare_ints(Fint a, Fint b)
 * {
 *   if (a<b) return -1;
 *   if (a>b) return 1;
 *   return 0;
 * }
 */

static int pp_compare_reals(Freal a, Freal b)
{
  Freal delta;

  /* first test for special case (unnecessary, but code profiler shows 
   * slightly more efficient)
   */
  if (a==b) return 0;

  delta = fabs(b * tolerance);
  if (a<b-delta) return -1;
  if (a>b+delta) return 1;

  return 0;
}

/* compares two pointers - not in an intelligent way, but
 * just checks if they point to same memory, and if not then
 * returns +1 or -1 which will give a (pretty arbitrary) ordering
 */

static int pp_compare_ptrs(const void *a, const void *b)
{
  if (a<b) return -1;
  if (a>b) return 1;
  return 0;
}


#define COMPARE_INTS(tag) {int cmp=pp_compare_ints(a->hdr.tag,b->hdr.tag); if (cmp!=0) return cmp;}
#define COMPARE_REALS(tag) {int cmp=pp_compare_reals(a->hdr.tag,b->hdr.tag); if (cmp!=0) return cmp;}


/* routine to compare two PP records, to see if they are in the same 
 * variable
 *
 * returns: 
 *
 *    -1 or 1  headers are from different variables;
 *               sign of return value gives consistent ordering
 *
 *       0     headers are from same variable
 */
int pp_compare_records_between_vars(const PPrec *a, const PPrec *b) {

  COMPARE_INTS(LBUSER4);
  COMPARE_INTS(LBUSER7);
  COMPARE_INTS(LBCODE);
  COMPARE_INTS(LBVC);
  COMPARE_INTS(LBTIM);
  COMPARE_INTS(LBPROC);
  COMPARE_REALS(BPLAT);
  COMPARE_REALS(BPLON);
  COMPARE_INTS(LBHEM);
  COMPARE_INTS(LBROW);
  COMPARE_INTS(LBNPT);

  COMPARE_REALS(BGOR);
  COMPARE_REALS(BZY);
  COMPARE_REALS(BDY);
  COMPARE_REALS(BZX);
  COMPARE_REALS(BDX);
  
  
  {
    int cmp = pp_compare_mean_periods(a, b);
    if (cmp!=0) return cmp;
  }

  /* Disambig index is used to force distinction between variables for records whose headers
   * are the same.  It is initialised to the same value for all records (in fact -1), but may
   * later be set to different values according to some heuristic.
   */

  {
    int cmp=pp_compare_ints(a->disambig_index,b->disambig_index);
    if (cmp!=0) return cmp;
  }

  return 0;
}


/* Routine to compare if two PP records have different meaning periods, such
 * that they should be considered to be part of different variables.  Normally
 * this will be true if the mean periods differ by more than "delta", but in
 * the case of Gregorian calendar, we allow some tolerance relating to
 * climatology data.
 *
 * This should only get called if both records have already been checked for
 * having the same LBTIM and LBPROC.
 */

int pp_compare_mean_periods(const PPrec *a, const PPrec *b) {
    
  int cmp;

  cmp = pp_compare_reals(a->mean_period, b->mean_period);
  if (cmp == 0) return 0;

  /* if we get here, times differ - but for gregorian cut some slack */
  if (pp_calendar_type(a->hdr.LBTIM) == gregorian) {
    if (pp_both_values_in_range(28., 31., 
				a->mean_period, b->mean_period)  /* monthly */
	|| pp_both_values_in_range(90., 92.,
				   a->mean_period, b->mean_period)  /* seasonal */
	|| pp_both_values_in_range(365., 366.,
				   a->mean_period, b->mean_period)) /* annual */
      return 0;
  }

  return cmp;
}

/* helper routine for pp_compare_mean_periods - test if both periods are in specified range
 * note - assumes that low, high are positive
 */
int pp_both_values_in_range(Freal low, Freal high, Freal a, Freal b) {
  Freal low1 = low * (1. - tolerance);
  Freal high1 = high * (1. + tolerance);
  return (a >= low1) && (a <= high1) && (b >= low1) & (b <= high1);  
}



/* routine to compare two PP records that the calling routine 
 * has already established are in the same variable.
 * 
 * returns: 
 *
 *    -1 or 1  times or levels differ;
 *               sign of return value gives consistent ordering
 *               of times and levels
 *
 *       0     records do not differ within values tested
 */
int pp_compare_records_within_var(const PPrec *a, const PPrec *b) {
  
  COMPARE_INTS(LBFT);

  COMPARE_INTS(LBYR);
  COMPARE_INTS(LBMON);
  COMPARE_INTS(LBDAT);
  COMPARE_INTS(LBDAY);
  COMPARE_INTS(LBHR);
  COMPARE_INTS(LBMIN);

  COMPARE_INTS(LBYRD);
  COMPARE_INTS(LBMOND);
  COMPARE_INTS(LBDATD);
  COMPARE_INTS(LBDAYD);
  COMPARE_INTS(LBHRD);
  COMPARE_INTS(LBMIND);

  /*
   *  Ordering of levels:
   * 
   *  Generally we want to sort on LBLEV before sorting on BLEV.
   *
   *  This is because in the case of hybrid levels, BLEV contains the B values
   *  (in p = A + B p_s), which won't do for sorting, and fortunately in this
   *  case LBLEV contains the model level index which is fine.
   *
   *  But there is a nasty special case: surface and boundary layer heat flux
   *  has LBLEV = 9999, 2, 3, 4, ... where 9999 is the surface layer.  In this
   *  case we *could* in fact sort on BLEV, but then we need to know when it's 
   *  okay to do this (STASH code?).  
   *
   *  Maybe safer, treat 9999 lower than any level if comparing it with
   *  another level.  (9999 should always be a special value and it is rare
   *  for it to be mixed with non-special values in the same variable.)
   */
  {
    int a_surface = (a->hdr.LBLEV == 9999);
    int b_surface = (b->hdr.LBLEV == 9999);
    if (a_surface && !b_surface) {
      return -1;
    }
    else if (b_surface && !a_surface) {
      return 1;
    }
  }


  COMPARE_INTS(LBLEV);
  COMPARE_REALS(BLEV);
  COMPARE_REALS(BHLEV);

  return 0;

}


/* routine to compare two PP records.
 * returns: 
 *    -2 or 2  headers are from different variable
 *    -1 or 1  headers are from same variable
 *       0     difference not found in elements inspected
 */
int pp_compare_records(const void *p1, const void *p2)
{
  const PPrec *a = *(PPrec **)p1;
  const PPrec *b = *(PPrec **)p2;

  int cmp;

  cmp = pp_compare_records_between_vars(a, b);
  if (cmp != 0)
    return cmp * 2;

  cmp = pp_compare_records_within_var(a, b);
  if (cmp != 0)
    return cmp;

  return 0;
}


int pp_records_from_different_vars(const PPrec *a, const PPrec *b)
{
  return (pp_compare_records_between_vars(a, b) != 0);
}

/*---------------------------------------------------------*/

int pp_compare_regaxes(const void *p1, const void *p2) {

  const PPregaxis *a = *(PPregaxis **)p1;
  const PPregaxis *b = *(PPregaxis **)p2;
  int cmp;
  if ((cmp=pp_compare_ints(a->n,b->n))!=0) return cmp;
  if ((cmp=pp_compare_reals(a->start,b->start))!=0) return cmp;
  if ((cmp=pp_compare_reals(a->interval,b->interval))!=0) return cmp;
  if ((cmp=pp_compare_rotmaps(&a->rotmap,&b->rotmap))!=0) return cmp;
  
  return 0;
}

int pp_compare_rotmaps( const void *p1, const void *p2 ) {

  const PProtmap *a = *(PProtmap **)p1;
  const PProtmap *b = *(PProtmap **)p2;
  int cmp;
  if (a == NON_ROTATED || b==NON_ROTATED)
    return pp_compare_ptrs(a,b);

  else {
    if ((cmp=pp_compare_reals(a->pole_lon,b->pole_lon))!=0) return cmp;
    if ((cmp=pp_compare_reals(a->pole_lat,b->pole_lat))!=0) return cmp;
    if ((cmp=pp_compare_reals(a->truepole_gridlon,b->truepole_gridlon))!=0) return cmp;
    return 0;
  }
}

int pp_compare_rotgrids( const void *p1, const void *p2 ) {

  const PProtgrid *a = *(PProtgrid **)p1;
  const PProtgrid *b = *(PProtgrid **)p2;
  int cmp;

  /* use comparison functions which compare the actual elements --
   * but this may be overkill, we could use pp_compare_ptr instead
   * in these 3 lines
   */

  if ((cmp=pp_compare_rotmaps(&a->rotmap,&b->rotmap))!=0) return cmp;
  if ((cmp=pp_genaxis_compare(&a->xaxis,&b->xaxis))!=0) return cmp;
  if ((cmp=pp_genaxis_compare(&a->yaxis,&b->yaxis))!=0) return cmp;
  return 0;
}


int pp_compare_xsaxes( const void *p1, const void *p2 ) {
  const PPxsaxis *a = *(PPxsaxis **)p1;
  const PPxsaxis *b = *(PPxsaxis **)p2;
  int cmp;
  int i;
  Freal *values1, *values2;

  if ((cmp=pp_compare_ints(a->data->n, b->data->n)) != 0) return cmp;

  values1=(Freal *) a->data->values;
  values2=(Freal *) b->data->values;

  for (i=0; i< a->data->n; i++) {
    if ((cmp=pp_compare_reals(values1[i], values2[i])) !=0 ) return cmp;
  }
  return 0;
}

int pp_compare_lists(const PPlist *l1, const PPlist *l2, int (*compfunc)(const void*, const void*)) {
  int i,n,cmp;
  const void *item1, *item2;
  PPlisthandle handle1, handle2;

  /* differ if number of items differs */
  n=pp_list_size(l1);
  if ((cmp=pp_compare_ints(n,pp_list_size(l2)))!=0) return cmp;
  
  /* differ if any individual item differs */
  pp_list_startwalk(l1,&handle1);
  pp_list_startwalk(l2,&handle2);
  for (i=0; i<n; i++) {
    item1=pp_list_walk(&handle1,0);
    item2=pp_list_walk(&handle2,0);
    if ((cmp=compfunc(&item1,&item2))!=0) return cmp;
  }
  return 0;
}

int pp_compare_levels(const void *p1, const void *p2) {
  const PPlevel *a = *(PPlevel **)p1;
  const PPlevel *b = *(PPlevel **)p2;

  /* macros called LCOMPARE_INTS and LCOMPARE_REALS to emphasise difference from those in pp_compare_records */

#define LCOMPARE_INTS(tag) {int cmp=pp_compare_ints(a->tag,b->tag); if (cmp!=0) return cmp;}
#define LCOMPARE_REALS(tag) {int cmp=pp_compare_reals(a->tag,b->tag); if (cmp!=0) return cmp;}

  LCOMPARE_INTS(type);

  switch (a->type) {
  case hybrid_height_lev_type:
    LCOMPARE_REALS(values.hybrid_height.a);
    LCOMPARE_REALS(values.hybrid_height.b);
#ifdef BDY_LEVS
    LCOMPARE_REALS(values.hybrid_height.ubdy_a);
    LCOMPARE_REALS(values.hybrid_height.ubdy_b);
    LCOMPARE_REALS(values.hybrid_height.lbdy_a);
    LCOMPARE_REALS(values.hybrid_height.lbdy_b);
#endif
    break;
  case hybrid_sigmap_lev_type:
    LCOMPARE_REALS(values.hybrid_sigmap.a);
    LCOMPARE_REALS(values.hybrid_sigmap.b);
#ifdef BDY_LEVS
    LCOMPARE_REALS(values.hybrid_sigmap.ubdy_a);
    LCOMPARE_REALS(values.hybrid_sigmap.ubdy_b);
    LCOMPARE_REALS(values.hybrid_sigmap.lbdy_a);
    LCOMPARE_REALS(values.hybrid_sigmap.lbdy_b);
#endif
    break;
  case pseudo_lev_type:
    LCOMPARE_INTS(values.pseudo.index);
    break;
  default:
    LCOMPARE_REALS(values.misc.level);
#ifdef BDY_LEVS
    LCOMPARE_REALS(values.misc.ubdy_level);
    LCOMPARE_REALS(values.misc.lbdy_level);
#endif
    break;
  }
  return 0;
}

int pp_compare_zaxes(const void *p1, const void *p2) {
  const PPzaxis *a = *(PPzaxis **)p1;
  const PPzaxis *b = *(PPzaxis **)p2;
  int cmp;
  /* differ if level type differs */
  /*   if ((cmp=pp_compare_ints(a->type,b->type))!=0) return cmp;  */
  /* differ if level lists differ */
  if ((cmp=pp_compare_lists(a->values,b->values,pp_compare_levels))!=0) return cmp;
  return 0;
}

int pp_compare_times(const void *p1, const void *p2) {
  const PPtime *a = *(PPtime **)p1;
  const PPtime *b = *(PPtime **)p2;
  int cmp;

  /* LBTYP: ignore 100s digit = sampling frequency, as we don't use it for anything */
  if ((cmp=pp_compare_ints(a->type%100,b->type%100))!=0) return cmp;

  if ((cmp=pp_compare_dates(&a->time1,&b->time1))!=0) return cmp;
  if ((cmp=pp_compare_dates(&a->time2,&b->time2))!=0) return cmp;
  return 0;
}

int pp_compare_dates(const PPdate *a, const PPdate *b) {
  int cmp;
  if ((cmp=pp_compare_ints(a->year  ,b->year  ))!=0) return cmp;
  if ((cmp=pp_compare_ints(a->month ,b->month ))!=0) return cmp;
  if ((cmp=pp_compare_ints(a->day   ,b->day   ))!=0) return cmp;
  if ((cmp=pp_compare_ints(a->hour  ,b->hour  ))!=0) return cmp;
  if ((cmp=pp_compare_ints(a->minute,b->minute))!=0) return cmp;
  if ((cmp=pp_compare_ints(a->second,b->second))!=0) return cmp;
  return 0;
}

int pp_compare_taxes(const void *p1, const void *p2) {
  const PPtaxis *a = *(PPtaxis **)p1;
  const PPtaxis *b = *(PPtaxis **)p2;
  int cmp;
  /* differ if time type differs */
  /*  if ((cmp=pp_compare_ints(a->type%100,b->type%100))!=0) return cmp;  */

  /* differ if time origin differs */
  if ((cmp=pp_compare_dates(&a->time_orig,&b->time_orig))!=0) return cmp;

  /* differ if time lists differ */
  if ((cmp=pp_compare_lists(a->values,b->values,pp_compare_times))!=0) return cmp;
  return 0;
}

#endif
