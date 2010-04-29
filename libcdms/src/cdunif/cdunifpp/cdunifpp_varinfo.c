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


/* pp_var_lookup looks up the description (long name, short name, units)
 * from the stash codes
 */

int pp_var_lookup(const PPhdr *hdr, PPstashmeta *sm) {

  /* so far the only source supported is the compiled-in defaults */
  /* FIXME: add here support for table-driven input */

  /* initialise to null strings */
  sm->shortname[0]='\0';
  sm->longname[0]='\0';
  sm->units[0]='\0';
  sm->stdname[0]='\0';
  sm->source[0]='\0';

  /* use compiled-in defaults */

  if (hdr->LBUSER7 == 1 && hdr->LBUSER4 == 1)
    /* insist that surface pressure is called "ps", as referred to in 
     * "formula_terms" attribute for hybrid sigma-p coordinates
     */
    strncpy(sm->shortname,"ps",SM_MAX_LEN);
  else if (hdr->LBUSER7 == 1 && hdr->LBUSER4 == 33)
    /* similarly insist that orography is called "orog", as referred to in 
     * "formula_terms" attribute for hybrid height coordinates
     */
    strncpy(sm->shortname,"orography",SM_MAX_LEN);
  else
    pp_get_var_default_shortname(hdr, sm->shortname, SM_MAX_LEN);

  pp_get_var_default_longname(hdr, sm->longname, SM_MAX_LEN);
  pp_get_var_default_units(hdr, sm->units, SM_MAX_LEN);

  strncpy(sm->source,"defaults (cdunifpp V" CDUNIFPP_VERSION ")",SM_MAX_LEN);
  sm->source[SM_MAX_LEN] = '\0';

  return 0;
}

/*-----------------------------------------------------------------*/

/* the functions pp_get_var_default_* are just wrappers to the relevant
 * routines which look up the compiled-in defaults; the wrappers copy
 * the compiled-in string into the provided storage
 */

int pp_get_var_default_shortname(const PPhdr *hdr, char *name, int name_max){

  char *lookupname;

  lookupname = pp_ppshortname(hdr->LBFC);

  if (hdr->LBFC != 0 && lookupname != NULL)
    snprintf(name,name_max,"%s",lookupname);
  else 
    snprintf(name,name_max,"m%ds%di%d",
	     pp_get_var_stash_model(hdr),
	     pp_get_var_stash_section(hdr),
	     pp_get_var_stash_item(hdr));

  name[name_max]='\0';
  return 0;
}

int pp_get_var_default_longname(const PPhdr *hdr, char *name, int name_max)
{
  char *stashname;
  stashname = pp_stashname(pp_get_var_stash_model(hdr),
			   pp_get_var_stash_section(hdr),
			   pp_get_var_stash_item(hdr));

  if (stashname==NULL) return -1;

  strncpy(name,stashname,name_max);
  name[name_max]='\0';

  return 0;
}


int pp_get_var_default_units(const PPhdr *hdr, char *units, int units_max)
{
  char *ppunit;

  ppunit = pp_ppunit(hdr->LBFC);
  if (ppunit==NULL) return -1;
  
  strncpy(units,ppunit,units_max);
  units[units_max]='\0';

  return 0;  
}

/*-----------------------------------------------------------------*/

int pp_get_var_stash_model(const PPhdr *hdr)
{
  return hdr->LBUSER7;
}

int pp_get_var_stash_section(const PPhdr *hdr)
{
  return hdr->LBUSER4 / 1000;
}

int pp_get_var_stash_item(const PPhdr *hdr)
{
  return hdr->LBUSER4 % 1000;
}

int pp_get_var_packing(const PPhdr *hdr)
{
  return hdr->LBPACK % 10;
}

int pp_get_var_compression(const PPhdr *hdr)
{
  return (hdr->LBPACK / 10) % 10;
}

int pp_get_var_processing(const PPhdr *hdr)
{
  return hdr->LBPROC;
}

int pp_get_var_gridcode(const PPhdr *hdr)
{
  return hdr->LBCODE;
}

/*-----------------------------------------------------------------*/

int pp_get_var_name(int varid, const char *prefix, CuVar *cuvars) {
  
  int okay, var, ext;
  
  char *name;

  name = cuvars[varid].name;

  okay=0;

  /* keep writing new name based on basename with a possible extension number,
   * until condition "okay", which is that name does not already exist
   */
  for (ext=0; !okay; ext++) {

    if (ext==0)
      strncpy(name,prefix,CU_MAX_NAME);
    else
      snprintf(name,CU_MAX_NAME,"%s_%d",prefix,ext);
    name[CU_MAX_NAME]='\0';
    
    okay=1;
    for (var=0; var<varid; var++)
      if (!strcmp(name,cuvars[var].name)) {
	okay=0;
	break;
      }

    /* we shouldn't have to try more times than there are vars so far */
    ERRIF(ext>varid);
  }
  return 0;

  ERRBLKI("cdunifpp_varname");
}


/* pp_get_fill_value returns fill value
 * (as pointer to static memory -- before dereferencing, needs to be cast to 
 * Fint* or Freal* as appropriate) 
 */
void *pp_get_var_fill_value(const PPhdr *hdr)
{
  /* vars used by pp_get_fill_value */
  static Fint intfill;
  static Freal realfill;

  CuType vartype;
 
  vartype=pp_get_var_type(hdr);

  if (vartype==inttype) {
    intfill = int_fill_value;
    return &intfill;
  }
  else if (vartype==realtype) {
    realfill = hdr->BMDI;
    return &realfill;
  }
  else {
    pp_switch_bug("pp_get_var_fill_value");
    return NULL;
  }
}

int pp_var_is_land_mask(const PPhdr *hdr)
{
  int model,section,item; 
  model=pp_get_var_stash_model(hdr);
  section=pp_get_var_stash_section(hdr);
  item=pp_get_var_stash_item(hdr);
  return ((model==1 && section==0 && item==30) || 
	  (model==4 && section==0 && item==2));
}

CuType pp_get_var_type(const PPhdr *hdr){

  switch (hdr->LBUSER1) {
  case(2):
  case(-2):
  case(3):
  case(-3):
    return inttype;
    /* break; */
  case(1):
  case(-1):
    return realtype;
    /* break; */
  default:
    CuError(CU_EINTERN,"Warning: datatype %d not recognised in pp_get_var_type, assuming real",
	    hdr->LBUSER1);
    return realtype;
  }

  return 0;
}

int pp_var_is_time_mean(const PPhdr *hdr)
{
  return (hdr->LBPROC & 128);
}

int pp_var_is_time_min(const PPhdr *hdr)
{
  return (hdr->LBPROC & 4096);
}

int pp_var_is_time_max(const PPhdr *hdr)
{
  return (hdr->LBPROC & 8192);
}

int pp_var_is_time_variance(const PPhdr *hdr)
{
  return (hdr->LBPROC & 65536);
}

int pp_var_is_zonal_mean(const PPhdr *hdr)
{
  return (hdr->LBPROC & 64);
}

int pp_var_is_vertical_mean(const PPhdr *hdr)
{
  return (hdr->LBPROC & 2048);
}

/* sometimes a variable is included but which has some
 * really essential header elements to missing data flag,
 * so the variable is essentially missing in that any
 * attempt to process the variable is only going to
 * lead to errors
 *
 * pp_var_missing() tests for this.
 *
 * FIXME: expand to test other header elements
 */
int pp_var_is_missing(const PPhdr *hdr)
{
  if (hdr->LBNPT == int_missing_data)
    return 1;

  if (hdr->LBROW == int_missing_data)
    return 1;

  return 0;
}
#endif
