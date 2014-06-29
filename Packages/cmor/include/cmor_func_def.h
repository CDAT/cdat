/* this file contains function definitions for cmor */
/* cmor.c */
#ifndef CMOR_FUNC_H
#define CMOR_FUNC_H
#include <udunits2.h>
#include "cdmsint.h"
#include <stdio.h>

extern void cmor_md5 (FILE *inputfile,unsigned char checksum[16]);

extern void cmor_is_setup(void);
extern void cmor_add_traceback(char *name);
extern void cmor_pop_traceback(void);
extern int cmor_prep_units(char *uunits, char *cunits, ut_unit **user_units,ut_unit **cmor_units,cv_converter **ut_cmor_converter);
extern int cmor_have_NetCDF4(void);
extern int cmor_have_NetCDF41min(void);
extern int cmor_have_NetCDF3(void);
extern int cmor_have_NetCDF363(void);
extern void cmor_handle_error(char error_msg[CMOR_MAX_STRING],int level);
extern int cmor_setup(char *path,int *netcdf, int *verbosity, int *mode, char *logfile, int *cmor_create_subdirectories);
extern int cmor_put_nc_num_attribute(int ncid,int nc_var_id,char *name, char type, double value, char *var_name);
extern int cmor_put_nc_char_attribute(int ncid,int nc_var_id,char *name,char *value,char *var_name);
extern int cmor_set_cur_dataset_attribute(char *name, char *value, int optional);
extern int cmor_get_cur_dataset_attribute(char *name, char *value);
extern int cmor_has_cur_dataset_attribute(char *name);
extern int cmor_dataset(char *outpath, 
			char *experiment_id,
			char *institution, 
			char *source,   
			char *calendar, 
			int realization, 
			char *contact, 
			char *history, 
			char *comment, 
			char *references,	 
			int leap_year, 
			int leap_month, 
			int month_lengths[12],
			char *model_id,
			char *forcing,
			int initialization_method,
			int physics_version,
			char *institute_id,
			char *parent_experiment_id,
			double *branch_time,
			char *parent_experiment_rip);
extern int strncpytrim(char *out, char *in, int max);
extern int cmor_convert_string_to_list(char *values,char type, void **target, int *nelts);
extern int cmor_define_zfactors_vars(int var_id,int ncid, int *nc_dim,char *formula_terms,int *nzfactors, int *zfactors, int *nc_zfactors,int i,int dim_bnds);
extern int cmor_create_output_path(int var_id,char *outpath);
extern double cmor_convert_interval_to_seconds(double val, char *units);
extern int cmor_write(int var_id,void *data, char type, char *suffix, int ntimes_passed, double *time_vals, double *time_bounds, int *refvar);
extern int cmor_close_variable(int var_id, char *file_name, int *preserve);
extern int cmor_close(void);
/* cmor_axis.c */
extern void cmor_init_axis_def(cmor_axis_def_t *axis, int table_id);
extern int cmor_set_axis_def_att(cmor_axis_def_t *axis,char att[CMOR_MAX_STRING],char val[CMOR_MAX_STRING] );
extern void cmor_trim_string(char *in,char *out);
extern int cmor_calendar_c2i(char *calendar, cdCalenType *ical);
extern int cmor_convert_time_units( char *inunits, char *outunits, char *loutunits);
extern int cmor_convert_time_values( void *values_in, char type, int nvalues, double *values_out, char *inunits, char *outunits, char *calin, char *calout);
extern int cmor_set_axis_attribute(int id, char *attribute_name, char type, void *value);
extern int cmor_get_axis_attribute(int id, char *attribute_name, char type, void *value);
extern int cmor_has_axis_attribute(int id, char *attribute_name);
extern int cmor_check_values_inside_bounds(double *values,double *bounds, int length, char *name);
extern int cmor_check_monotonic(double *values,int length, char *name,int isbounds, int axis_id);
extern int cmor_treat_axis_values(int axis_id, double *values, int length, int n_requested, char *units, char *name, int isbounds);
extern int cmor_check_interval(int axis_id, char *interval, double *values, int nvalues, int isbounds);
extern int cmor_axis(int *axis_id, char *name,char *units, int length,void *coord_vals, char type, void *cell_bounds,int cell_bounds_ndim,char *interval);
/* cmor_variable.c */
extern void cmor_init_var_def(cmor_var_def_t *var, int table_id);
extern int cmor_is_required_variable_attribute(cmor_var_def_t var, char *attribute_name);
extern int cmor_has_required_variable_attributes(int var_id);
extern int cmor_set_variable_attribute(int id, char *attribute_name, char type, void *value);
extern int cmor_get_variable_attribute(int id, char *attribute_name, void *value);
extern int cmor_has_variable_attribute(int id, char *attribute_name);
extern int cmor_get_variable_attribute_names(int id, int *nattributes,char attributes_names[][CMOR_MAX_STRING]);
extern int cmor_get_variable_attribute_type(int id, char *attribute_name, char *type);
extern int cmor_zfactor (int *zvar_id,int axis_id, char *name, char *units, int ndims, int axes_ids[], char type, void *values, void *bounds);
extern int cmor_update_history(int var_id,char *add);
extern int cmor_variable(int *var_id, char *name, char *units, int ndims, int axes_ids[], char type, void *missing, double *tolerance, char *positive, char*original_name, char *history, char *comment);
extern int cmor_set_var_def_att(cmor_var_def_t *var,char att[CMOR_MAX_STRING],char val[CMOR_MAX_STRING] );
extern int cmor_get_variable_time_length(int *var_id, int *length);
extern int cmor_get_original_shape(int *var_id, int *shape_array, int *rank, int blank_time);
extern int cmor_write_var_to_file(int ncid,cmor_var_t *avar,void *data,char itype, int ntimes_passed, double *time_vals, double *time_bounds);
/* cmor_grid.c */
extern void cmor_set_mapping_attribute(cmor_mappings_t *mapping,char att[CMOR_MAX_STRING],char val[CMOR_MAX_STRING] );
extern void cmor_init_grid_mapping(cmor_mappings_t *mapping,char *id);
extern int cmor_copy_data(double **dest1, void *data, char type, int nelts);
extern int cmor_has_grid_attribute(int gid, char *name);
extern int cmor_get_grid_attribute(int gid, char *name, double *value);
extern void cmor_convert_value(char *units,char *ctmp,double *tmp);
extern int cmor_set_grid_attribute(int gid, char *name, double *value, char *units);
extern int cmor_attribute_in_list(char *name, int n, char (*atts)[CMOR_MAX_STRING]);
extern int cmor_grid_valid_mapping_attribute_names(char *name, int *natt, char (*att)[CMOR_MAX_STRING],int *ndims, char (*dims)[CMOR_MAX_STRING]);
extern int cmor_set_grid_mapping(int gid, char *name, int nparam, char **attributes_names, int lparams, double attributes_values[CMOR_MAX_GRID_ATTRIBUTES], char **units, int lnunits );
extern int cmor_grid(int *grid_id, int ndims, int *axes_ids, char type, void *lat, void *lon, int nvertices, void *blat, void *blon);
extern void cmor_init_table(cmor_table_t *table, int id);
extern int cmor_set_dataset_att(cmor_table_t *table, char att[CMOR_MAX_STRING],char val[CMOR_MAX_STRING] );
extern int cmor_set_table(int table);
extern int cmor_load_table(char table[CMOR_MAX_STRING], int *table_id);
extern int cmor_time_varying_grid_coordinate(int *coord_grid_id, int  grid_id, char *name, char *units, char type, void *missing, int *coordinate_type);
extern void cmor_cat_unique_string (char* dest, char* src);
extern int cmor_stringinstring (char* dest, char* src);
#endif
