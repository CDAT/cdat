#include <stdlib.h>
#include <string.h>
#include "cmor.h"

int cmor_ftn_get_tlen_cff_(int *var_id) {
  int length;
  int ierr;
  ierr = cmor_get_variable_time_length(var_id,&length);
  return length;
}

int cmor_get_original_shape_cff_(int *var_id,int *shape_array) {
  int tmp,i;
  i=7;
  tmp = cmor_get_original_shape(var_id,shape_array,&i,1);
  /* reverse it for fortran order */
  for (i=0;i<3;i++) {
    tmp = shape_array[i];
    shape_array[i]=shape_array[6-i];
    shape_array[6-i]=tmp;
  }
  /* now need to put the -1 back at the end */
  tmp=0;
  for (i=0;i<7;i++) if (shape_array[i]==-1) tmp+=1;
  for (i=0;i<7-tmp;i++) shape_array[i]=shape_array[i+tmp];
  for (i=0;i<tmp;i++) shape_array[i+7-tmp]=-1;
  return 0;
}

void cmor_create_output_path_cff_(int *var_id, char *path, int *slen) {
  int isfixed;
  isfixed = cmor_create_output_path(*var_id,path);
  *slen = strlen(path);
}

int cmor_set_cur_dset_attribute_cff_(char *name, char *value) {
  return cmor_set_cur_dataset_attribute(name, value, 1);
}

int cmor_get_cur_dset_attribute_cff_(char *name, char *value) {
  return cmor_get_cur_dataset_attribute(name, value) ;
}

int cmor_has_cur_dset_attribute_cff_(char *name) {
  return  cmor_has_cur_dataset_attribute(name);
}

int cmor_set_variable_attribute_cff_(int *varid, char *name, char *value) {
  return cmor_set_variable_attribute(*varid, name, 'c', value );
}

int cmor_get_variable_attribute_cff_(int *varid, char *name, char *value) {
  return cmor_get_variable_attribute(*varid, name, value) ;
}

int cmor_has_variable_attribute_cff_(int *varid, char *name) {
  return  cmor_has_variable_attribute(*varid, name);
}

int cmor_close_cff_(void) {
  return cmor_close();
}

int cmor_close_var_nofnm_cff_(int *var_id) {
  return cmor_close_variable(*var_id,NULL,NULL);
}

int cmor_close_var_fnm_cff_(int *var_id, char *fnm) {
  return cmor_close_variable(*var_id,fnm,NULL);
}

int cmor_close_var_nofnm_preserve_cff_(int *var_id, int *preserve) {
  return cmor_close_variable(*var_id,NULL,preserve);
}

int cmor_close_var_fnm_preserve_cff_(int *var_id, char *fnm, int *preserve) {
  return cmor_close_variable(*var_id,fnm,preserve);
}

int cmor_set_grd_map_cff_(int *gid, char *name, int *nparam, char **attributes_names, int *lparam, double *values, char **units, int *lnunits){
  return cmor_set_grid_mapping(*gid, name, *nparam, attributes_names, *lparam, values, units, *lnunits);
}

/* int cmor_grid_cff_long_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *blon,void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'l',lat,lon,*nvertices,blat,blon,area); */
/* } */
/* int cmor_grid_cff_noarea_long_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *blon){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'l',lat,lon,*nvertices,blat,blon); */
/* } */
/* int cmor_grid_cff_noblon_long_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'l',lat,lon,*nvertices,blat,NULL,area); */
/* } */
/* int cmor_grid_cff_noblat_long_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blon,void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'l',lat,lon,*nvertices,NULL,blon,area); */
/* } */
/* int cmor_grid_cff_nobnds_long_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'l',lat,lon,*nvertices,NULL,NULL,area); */
/* } */
/* int cmor_grid_cff_noblaar_long_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blon){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'l',lat,lon,*nvertices,NULL,blon); */
/* } */
/* int cmor_grid_cff_nobloar_long_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'l',lat,lon,*nvertices,blat,NULL); */
/* } */
/* int cmor_grid_cff_nothg_long_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *blon,void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'l',lat,lon,*nvertices,NULL,NULL,NULL); */
/* } */

/* int cmor_grid_cff_double_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *blon,void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'d',lat,lon,*nvertices,blat,blon,area); */
/* } */
int cmor_grid_cff_noarea_double_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *blon){
  return cmor_grid(grid_id,*ndims,axes_ids,'d',lat,lon,*nvertices,blat,blon);
}

/* int cmor_grid_cff_noblon_double_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'d',lat,lon,*nvertices,blat,NULL,area); */
/* } */
/* int cmor_grid_cff_noblat_double_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blon,void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'d',lat,lon,*nvertices,NULL,blon,area); */
/* } */
/* int cmor_grid_cff_nobnds_double_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'d',lat,lon,*nvertices,NULL,NULL,area); */
/* } */
int cmor_grid_cff_noblaar_double_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blon){
  return cmor_grid(grid_id,*ndims,axes_ids,'d',lat,lon,*nvertices,NULL,blon);
}
int cmor_grid_cff_nobloar_double_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat){
  return cmor_grid(grid_id,*ndims,axes_ids,'d',lat,lon,*nvertices,blat,NULL);
}
int cmor_grid_cff_nothg_double_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices){
  return cmor_grid(grid_id,*ndims,axes_ids,'d',lat,lon,*nvertices,NULL,NULL);
}

/* int cmor_grid_cff_real_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *blon,void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'f',lat,lon,*nvertices,blat,blon,area); */
/* } */
int cmor_grid_cff_noarea_real_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *blon){
  return cmor_grid(grid_id,*ndims,axes_ids,'f',lat,lon,*nvertices,blat,blon);
}
/* int cmor_grid_cff_noblon_real_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'f',lat,lon,*nvertices,blat,NULL,area); */
/* } */
/* int cmor_grid_cff_noblat_real_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blon,void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'f',lat,lon,*nvertices,NULL,blon,area); */
/* } */
/* int cmor_grid_cff_nobnds_real_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'f',lat,lon,*nvertices,NULL,NULL,area); */
/* } */
int cmor_grid_cff_noblaar_real_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blon){
  return cmor_grid(grid_id,*ndims,axes_ids,'f',lat,lon,*nvertices,NULL,blon);
}
int cmor_grid_cff_nobloar_real_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat){
  return cmor_grid(grid_id,*ndims,axes_ids,'f',lat,lon,*nvertices,blat,NULL);
}
int cmor_grid_cff_nothg_real_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices){
  return cmor_grid(grid_id,*ndims,axes_ids,'f',lat,lon,*nvertices,NULL,NULL);
}
int cmor_grid_cff_nocoords_(int *grid_id, int *ndims, int *axes_ids, int *nvert) {
  return cmor_grid(grid_id,*ndims,axes_ids,'f',NULL,NULL,*nvert,NULL,NULL);
}

/* int cmor_grid_cff_int_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *blon,void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'i',lat,lon,*nvertices,blat,blon,area); */
/* } */
int cmor_grid_cff_noarea_int_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *blon){
  return cmor_grid(grid_id,*ndims,axes_ids,'i',lat,lon,*nvertices,blat,blon);
}
/* int cmor_grid_cff_noblon_int_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat, void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'i',lat,lon,*nvertices,blat,NULL,area); */
/* } */
/* int cmor_grid_cff_noblat_int_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blon,void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'i',lat,lon,*nvertices,NULL,blon,area); */
/* } */
/* int cmor_grid_cff_nobnds_int_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *area){ */
/*   return cmor_grid(grid_id,*ndims,axes_ids,'i',lat,lon,*nvertices,NULL,NULL,area); */
/* } */

int cmor_grid_cff_noblaar_int_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blon){
  return cmor_grid(grid_id,*ndims,axes_ids,'i',lat,lon,*nvertices,NULL,blon);
}
int cmor_grid_cff_nobloar_int_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices, void *blat){
  return cmor_grid(grid_id,*ndims,axes_ids,'i',lat,lon,*nvertices,blat,NULL);
}
int cmor_grid_cff_nothg_int_(int *grid_id, int *ndims, int *axes_ids, void *lat, void *lon, int *nvertices){
  return cmor_grid(grid_id,*ndims,axes_ids,'i',lat,lon,*nvertices,NULL,NULL);
}

int cmor_grid_cff_tvc_f_(int *coord_grid_id, int *grid_id, char *name, char *units, void *missing) {
  return cmor_time_varying_grid_coordinate(coord_grid_id, *grid_id, name, units, 'f', missing, NULL);
}
int cmor_grid_cff_tvc_d_(int *coord_grid_id, int *grid_id, char *name, char *units, void *missing) {
  return cmor_time_varying_grid_coordinate(coord_grid_id, *grid_id, name, units, 'd', missing, NULL);
}
int cmor_grid_cff_tvc_i_(int *coord_grid_id, int *grid_id, char *name, char *units, void *missing) {
  return cmor_time_varying_grid_coordinate(coord_grid_id, *grid_id, name, units, 'i', missing, NULL);
}
int cmor_grid_cff_tvc_no_(int *coord_grid_id, int *grid_id, char *name, char *units) {
  return cmor_time_varying_grid_coordinate(coord_grid_id, *grid_id, name, units, 'f', NULL, NULL);
}



int cmor_load_table_cff_(char table[CMOR_MAX_STRING], int *table_id) {
  return cmor_load_table(table,table_id);
}

void cmor_set_table_cff_(int *table) {
  int i;
  i = cmor_set_table(*table);
}

void cmor_handle_error_cff_(char error_msg[CMOR_MAX_STRING],int *level) {
  cmor_handle_error(error_msg,*level);
}


int cmor_setup_cff_nolog_(char *path, int *netcdf, int *verbosity, int *mode, int *crsub)
{ 
  return  cmor_setup(path,netcdf,verbosity,mode,NULL,crsub);
}
int cmor_setup_cff_(char *path, int *netcdf, int *verbosity, int *mode, char *logfile, int *crsub)
{ 
  return  cmor_setup(path,netcdf,verbosity,mode,logfile,crsub);
}

int cmor_dataset_cff_(char *outpath, 
		      char *experiment_id,
		      char *institution, 
		      char *source,   
		      char *calendar, 
		      int *realization, 
		      char *contact, 
		      char *history, 
		      char *comment, 
		      char *references,	 
		      int *leap_year, 
		      int *leap_month, 
		      int *month_lengths,
		      char *model_name,
		      char *forcing,
		      int *initialization_method,
		      int *physics_version,
		      char *institute_id,
		      char *parent_exp_id,
		      double *branch_time,
		      char *parent_exp_rip)
{
  return cmor_dataset(outpath,experiment_id,institution,source,calendar,*realization,contact,history,comment,references,*leap_year,*leap_month,month_lengths,model_name,forcing,*initialization_method,*physics_version,institute_id,parent_exp_id,branch_time,parent_exp_rip);
}
int cmor_dataset_cff_null_(char *outpath, 
			   char *experiment_id,
			   char *institution, 
			   char *source,   
			   char *calendar, 
			   int  *realization, 
			   char *contact, 
			   char *history, 
			   char *comment, 
			   char *references,	 
			   int  *leap_year, 
			   int  *leap_month,
			   char *model_name,
			   char *forcing,
			   int  *initialization_method,
			   int  *physics_version,
			   char *institute_id,
			   char *parent_exp_id,
			   double *branch_time,
			   char *parent_exp_rip)
{
  return cmor_dataset(outpath,experiment_id,institution,source,calendar,*realization,contact,history,comment,references,*leap_year,*leap_month,NULL,model_name,forcing,*initialization_method,*physics_version,institute_id,parent_exp_id,branch_time,parent_exp_rip);
}
int cmor_dataset_cff_nobrch_(char *outpath, 
			     char *experiment_id,
			     char *institution, 
			     char *source,   
			     char *calendar, 
			     int *realization, 
			     char *contact, 
			     char *history, 
			     char *comment, 
			     char *references,	 
			     int *leap_year, 
			     int *leap_month, 
			     int *month_lengths,
			     char *model_name,
			     char *forcing,
			     int *initialization_method,
			     int *physics_version,
			     char *institute_id,
			     char *parent_exp_id,
			     char *parent_exp_rip)
{
  return cmor_dataset(outpath,experiment_id,institution,source,calendar,*realization,contact,history,comment,references,*leap_year,*leap_month,month_lengths,model_name,forcing,*initialization_method,*physics_version,institute_id,parent_exp_id,NULL,parent_exp_rip);
}
int cmor_dataset_cff_null_nobrch_(char *outpath, 
				  char *experiment_id,
				  char *institution, 
				  char *source,   
				  char *calendar, 
				  int  *realization, 
				  char *contact, 
				  char *history, 
				  char *comment, 
				  char *references,	 
				  int  *leap_year, 
				  int  *leap_month,
				  char *model_name,
				  char *forcing,
				  int  *initialization_method,
				  int  *physics_version,
				  char *institute_id,
				  char *parent_exp_id,
				  char *parent_exp_rip)
{
  return cmor_dataset(outpath,experiment_id,institution,source,calendar,*realization,contact,history,comment,references,*leap_year,*leap_month,NULL,model_name,forcing,*initialization_method,*physics_version,institute_id,parent_exp_id,NULL,parent_exp_rip);
}

int cmor_axis_cff_double_(int *axis_id, char *name,char *units, int *length,void *coord_vals, void *cell_bounds,int *cell_bounds_ndim,char *interval) {
  return cmor_axis(axis_id,name,units,*length,coord_vals,'d', cell_bounds, *cell_bounds_ndim, interval);
}
int cmor_axis_cff_real_(int *axis_id, char *name,char *units, int *length,void *coord_vals, void *cell_bounds,int *cell_bounds_ndim,char *interval) {
  return cmor_axis(axis_id,name,units,*length,coord_vals,'f', cell_bounds, *cell_bounds_ndim, interval);
}
int cmor_axis_cff_int_(int *axis_id, char *name,char *units, int *length,void *coord_vals, void *cell_bounds,int *cell_bounds_ndim,char *interval) {
  return cmor_axis(axis_id,name,units,*length,coord_vals,'i', cell_bounds, *cell_bounds_ndim, interval);
}
int cmor_axis_cff_long_(int *axis_id, char *name,char *units, int *length,void *coord_vals, void *cell_bounds,int *cell_bounds_ndim,char *interval) {
  return cmor_axis(axis_id,name,units,*length,coord_vals,'l', cell_bounds, *cell_bounds_ndim, interval);
}
int cmor_axis_cff_nobnds_double_(int *axis_id, char *name,char *units, int *length,void *coord_vals,char *interval) {
  return cmor_axis(axis_id,name,units,*length,coord_vals,'d', NULL, 0, interval);
}
int cmor_axis_cff_nobnds_real_(int *axis_id, char *name,char *units, int *length,void *coord_vals,char *interval) {
  return cmor_axis(axis_id,name,units,*length,coord_vals,'f', NULL, 0, interval);
}
int cmor_axis_cff_nobnds_int_(int *axis_id, char *name,char *units, int *length,void *coord_vals,char *interval) {
  return cmor_axis(axis_id,name,units,*length,coord_vals,'i', NULL, 0, interval);
}
int cmor_axis_cff_nobnds_long_(int *axis_id, char *name,char *units, int *length,void *coord_vals,char *interval) {
  return cmor_axis(axis_id,name,units,*length,coord_vals,'l', NULL, 0, interval);
}
int cmor_axis_cff_nobnds_char_(int *axis_id, char *name,char *units, int *length,void *coord_vals, int *cell_bounds_ndim,char *interval) {
  return cmor_axis(axis_id,name,units,*length,coord_vals,'c', NULL, *cell_bounds_ndim, interval);
}
int cmor_axis_cff_nocoords_(int *axis_id, char *name,char *units, int *length,char *interval) {
  return cmor_axis(axis_id,name,units,*length,NULL,'d', NULL, 0, interval);
}

int cmor_zfactor_cff_double_(int *zvar_id,int *axis_id, char *name, char *units, int *ndims, int *axes_ids, void *values, void *bounds) {
  return cmor_zfactor(zvar_id, *axis_id, name, units, *ndims, axes_ids, 'd', values, bounds);
}
int cmor_zfactor_cff_double_nobnds_(int *zvar_id,int *axis_id, char *name, char *units, int *ndims, int *axes_ids, void *values) {
  return cmor_zfactor(zvar_id, *axis_id, name, units, *ndims, axes_ids, 'd', values, NULL);
}
int cmor_zfactor_cff_int_(int *zvar_id,int *axis_id, char *name, char *units, int *ndims, int *axes_ids, void *values, void *bounds) {
  return cmor_zfactor(zvar_id, *axis_id, name, units, *ndims, axes_ids, 'i', values, bounds);
}
int cmor_zfactor_cff_int_nobnds_(int *zvar_id,int *axis_id, char *name, char *units, int *ndims, int *axes_ids, void *values) {
  return cmor_zfactor(zvar_id, *axis_id, name, units, *ndims, axes_ids, 'i', values, NULL);
}
int cmor_zfactor_cff_long_(int *zvar_id,int *axis_id, char *name, char *units, int *ndims, int *axes_ids, void *values, void *bounds) {
  return cmor_zfactor(zvar_id, *axis_id, name, units, *ndims, axes_ids, 'l', values, bounds);
}
int cmor_zfactor_cff_long_nobnds_(int *zvar_id,int *axis_id, char *name, char *units, int *ndims, int *axes_ids, void *values) {
  return cmor_zfactor(zvar_id, *axis_id, name, units, *ndims, axes_ids, 'l', values, NULL);
}
int cmor_zfactor_cff_real_(int *zvar_id,int *axis_id, char *name, char *units, int *ndims, int *axes_ids, void *values, void *bounds) {
  return cmor_zfactor(zvar_id, *axis_id, name, units, *ndims, axes_ids, 'f', values, bounds);
}
int cmor_zfactor_cff_real_nobnds_(int *zvar_id,int *axis_id, char *name, char *units, int *ndims, int *axes_ids, void *values) {
  return cmor_zfactor(zvar_id, *axis_id, name, units, *ndims, axes_ids, 'f', values, NULL);
}
int cmor_zfactor_cff_novals_(int *zvar_id,int *axis_id, char *name, char *units, int *ndims, int *axes_ids) {
  return cmor_zfactor(zvar_id, *axis_id, name, units, *ndims, axes_ids, 'd', NULL, NULL);
}

int cmor_variable_cff_double_(int *var_id, char *name, char *units, int *ndims, int *axes_ids, double *missing, double *tolerance, char *positive, char*original_name, char *history, char *comment) {
  return cmor_variable(var_id,name,units,*ndims,axes_ids,'d',missing,tolerance,positive,original_name,history,comment);
}
int cmor_variable_cff_real_(int *var_id, char *name, char *units, int *ndims, int *axes_ids, float *missing, double *tolerance, char *positive, char*original_name, char *history, char *comment) {
  return cmor_variable(var_id,name,units,*ndims,axes_ids,'f',missing,tolerance,positive,original_name,history,comment);
}
int cmor_variable_cff_int_(int *var_id, char *name, char *units, int *ndims, int *axes_ids, int *missing, double *tolerance, char *positive, char*original_name, char *history, char *comment) {
  return cmor_variable(var_id,name,units,*ndims,axes_ids,'i',missing,tolerance,positive,original_name,history,comment);
}
int cmor_variable_cff_long_(int *var_id, char *name, char *units, int *ndims, int *axes_ids, long *missing, double *tolerance, char *positive, char*original_name, char *history, char *comment) {
  return cmor_variable(var_id,name,units,*ndims,axes_ids,'l',missing,tolerance,positive,original_name,history,comment);
}
int cmor_variable_cff_nomiss_(int *var_id, char *name, char *units, int *ndims, int *axes_ids, double *tolerance, char *positive, char*original_name, char *history, char *comment) {
  return cmor_variable(var_id,name,units,*ndims,axes_ids,'f',NULL,tolerance,positive,original_name,history,comment);
}

int cmor_write_cff_double_(int *var_id,void *data, char *suffix, int *ntimes_passed, double *time_vals, double *time_bounds, int *refvar) {
  if (*refvar<0) {
    return cmor_write(*var_id,data,'d',suffix,*ntimes_passed,time_vals,time_bounds,NULL);
  }
  else {
    return cmor_write(*var_id,data,'d',suffix,*ntimes_passed,time_vals,time_bounds,refvar);
  }
}
int cmor_write_cff_double_nobnds_(int *var_id,void *data, char *suffix, int *ntimes_passed, double *time_vals, int *refvar) {
  if (*refvar<0) {
    return cmor_write(*var_id,data,'d',suffix,*ntimes_passed,time_vals,NULL,NULL);
  }
  else {
    return cmor_write(*var_id,data,'d',suffix,*ntimes_passed,time_vals,NULL,refvar);
  }
}
int cmor_write_cff_double_notime_(int *var_id,void *data, char *suffix, int *ntimes_passed, int *refvar) {
  if (*refvar<0) {
    return cmor_write(*var_id,data,'d',suffix,*ntimes_passed,NULL,NULL,NULL);
  }
  else {
    return cmor_write(*var_id,data,'d',suffix,*ntimes_passed,NULL,NULL,refvar);
  }
}

int cmor_write_cff_real_(int *var_id,void *data, char *suffix, int *ntimes_passed, double *time_vals, double *time_bounds, int *refvar) {
  /*   int i; */
  /*   printf("in interface\n"); */
  /*   for(i=0;i<12;i++) printf("ok in here we got: %i, %i, %f\n",*var_id,*refvar,(float)((float *)data)[i]); */
  if (*refvar<0) {
    return cmor_write(*var_id,data,'f',suffix,*ntimes_passed,time_vals,time_bounds,NULL);
  }
  else {
    return cmor_write(*var_id,data,'f',suffix,*ntimes_passed,time_vals,time_bounds,refvar);
  }
}
int cmor_write_cff_real_nobnds_(int *var_id,void *data, char *suffix, int *ntimes_passed, double *time_vals, int *refvar) {
  if (*refvar<0) {
    return cmor_write(*var_id,data,'f',suffix,*ntimes_passed,time_vals,NULL,NULL);
  }
  else {
    return cmor_write(*var_id,data,'f',suffix,*ntimes_passed,time_vals,NULL,refvar);
  }
}
int cmor_write_cff_real_notime_(int *var_id,void *data, char *suffix, int *ntimes_passed, int *refvar) {
  /*   int i; */
  /*   for(i=0;i<12;i++) printf("ok in here we got: %i, %i, %f\n",*var_id,*refvar,(float)((float *)data)[i]); */
  if (*refvar<0) {
    return cmor_write(*var_id,data,'f',suffix,*ntimes_passed,NULL,NULL,NULL);
  }
  else {
    return cmor_write(*var_id,data,'f',suffix,*ntimes_passed,NULL,NULL,refvar);
  }
}

int cmor_write_cff_int_(int *var_id,void *data, char *suffix, int *ntimes_passed, double *time_vals, double *time_bounds, int *refvar) {
  if (*refvar<0) {
    return cmor_write(*var_id,data,'i',suffix,*ntimes_passed,time_vals,time_bounds,NULL);
  }
  else {
    return cmor_write(*var_id,data,'i',suffix,*ntimes_passed,time_vals,time_bounds,refvar);
  }
}
int cmor_write_cff_int_nobnds_(int *var_id,void *data, char *suffix, int *ntimes_passed, double *time_vals, int *refvar) {
  if (*refvar<0) {
    return cmor_write(*var_id,data,'i',suffix,*ntimes_passed,time_vals,NULL,NULL);
  }
  else {
    return cmor_write(*var_id,data,'i',suffix,*ntimes_passed,time_vals,NULL,refvar);
  }
}
int cmor_write_cff_int_notime_(int *var_id,void *data, char *suffix, int *ntimes_passed, int *refvar) {
  if (*refvar<0) {
    return cmor_write(*var_id,data,'i',suffix,*ntimes_passed,NULL,NULL,NULL);
  }
  else {
    return cmor_write(*var_id,data,'i',suffix,*ntimes_passed,NULL,NULL,refvar);
  }
}
int cmor_write_cff_long_(int *var_id,void *data, char *suffix, int *ntimes_passed, double *time_vals, double *time_bounds, int *refvar) {
  if (*refvar<0) {
    return cmor_write(*var_id,data,'l',suffix,*ntimes_passed,time_vals,time_bounds,NULL);
  }
  else {
    return cmor_write(*var_id,data,'l',suffix,*ntimes_passed,time_vals,time_bounds,refvar);
  }
}
int cmor_write_cff_long_nobnds_(int *var_id,void *data, char *suffix, int *ntimes_passed, double *time_vals, int *refvar) {
  if (*refvar<0) {
    return cmor_write(*var_id,data,'l',suffix,*ntimes_passed,time_vals,NULL,NULL);
  }
  else {
    return cmor_write(*var_id,data,'l',suffix,*ntimes_passed,time_vals,NULL,refvar);
  }
}
int cmor_write_cff_long_notime_(int *var_id,void *data, char *suffix, int *ntimes_passed, int *refvar) {
  if (*refvar<0) {
    return cmor_write(*var_id,data,'l',suffix,*ntimes_passed,NULL,NULL,NULL);
  }
  else {
    return cmor_write(*var_id,data,'l',suffix,*ntimes_passed,NULL,NULL,refvar);
  }
}
