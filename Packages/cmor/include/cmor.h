#ifndef CMOR_H
#define CMOR_H

#define CMOR_VERSION_MAJOR 2
#define CMOR_VERSION_MINOR 8
#define CMOR_VERSION_PATCH 0

#define CMOR_CF_VERSION_MAJOR 1
#define CMOR_CF_VERSION_MINOR 4

#define CMOR_MAX_STRING 1024
#define CMOR_DEF_ATT_STR_LEN 256
#define CMOR_MAX_ELEMENTS 500
#define CMOR_MAX_AXES CMOR_MAX_ELEMENTS*3
#define CMOR_MAX_VARIABLES CMOR_MAX_ELEMENTS
#define CMOR_MAX_GRIDS 100
#define CMOR_MAX_DIMENSIONS 7
#define CMOR_MAX_ATTRIBUTES 100
#define CMOR_MAX_ERRORS 10
#define CMOR_MAX_TABLES 10
#define CMOR_MAX_GRID_ATTRIBUTES 25

#define CMOR_QUIET 0

#define CMOR_EXIT_ON_MAJOR 0
#define CMOR_EXIT 1
#define CMOR_EXIT_ON_WARNING 2

#define CMOR_WARNING 20
#define CMOR_NORMAL 21
#define CMOR_CRITICAL 22

#define CMOR_N_VALID_CALS 8

#define CMOR_PRESERVE_4 10
#define CMOR_APPEND_4 11
#define CMOR_REPLACE_4 12
#define CMOR_PRESERVE_3 13
#define CMOR_APPEND_3 14
#define CMOR_REPLACE_3 15
#define CMOR_PRESERVE CMOR_PRESERVE_3
#define CMOR_APPEND CMOR_APPEND_3
#define CMOR_REPLACE CMOR_REPLACE_3

extern int USE_NETCDF_4;
extern int CMOR_MODE;
extern int CMOR_TABLE;
extern int CMOR_VERBOSITY;
extern int CMOR_NETCDF_MODE;

extern int cleanup_varid;

extern int cmor_naxes;
extern int cmor_nvars;
extern int cmor_ntables;
extern int cmor_ngrids;

extern int cmor_nerrors;
extern int cmor_nwarnings;

extern char cmor_input_path[CMOR_MAX_STRING];

extern char cmor_traceback_info[CMOR_MAX_STRING];

typedef struct cmor_grid_ {
  int id;
  char mapping[CMOR_MAX_STRING];
  int nattributes;
  char attributes_names[CMOR_MAX_GRID_ATTRIBUTES][CMOR_MAX_STRING];
  double attributes_values[CMOR_MAX_GRID_ATTRIBUTES];
  int axes_ids[CMOR_MAX_DIMENSIONS];
  int original_axes_ids[CMOR_MAX_DIMENSIONS];
  int ndims;
  int istimevarying;
  int nvertices;
  double *lons;
  double *lats;
  double *blons;
  double *blats;
/*   double *area; */
/*   double *volumes; */
  int associated_variables[6]; /* for lon/lat/blon/blat/area/volumes */
} cmor_grid_t;

extern cmor_grid_t cmor_grids[CMOR_MAX_GRIDS];

typedef struct cmor_axis_def_ {
  int table_id;
  int climatology;
  char id[CMOR_MAX_STRING];
  char standard_name[CMOR_MAX_STRING];
  char units[CMOR_MAX_STRING];
  char axis;
  char positive;
  char long_name[CMOR_MAX_STRING];
  char out_name[CMOR_MAX_STRING];
  char type;
  char stored_direction;
  double valid_min;
  double valid_max;
  int n_requested;
  double *requested;
  char *crequested;
  char cname[CMOR_MAX_STRING];
  int n_requested_bounds;
  double *requested_bounds;
  double tolerance;
  double value;
  char cvalue[CMOR_MAX_STRING];
  double bounds_value[2];
  char required[CMOR_MAX_STRING];
  char formula[CMOR_MAX_STRING];
  char convert_to[CMOR_MAX_STRING];
  char z_factors[CMOR_MAX_STRING];
  char z_bounds_factors[CMOR_MAX_STRING];
  char index_only;
  int must_have_bounds;
  int must_call_cmor_grid;
} cmor_axis_def_t;

typedef struct cmor_axis_ {
  int ref_table_id;
  int ref_axis_id;
  int isgridaxis;
  char axis;
  char iunits[CMOR_MAX_STRING];
  char id[CMOR_MAX_STRING];
  int length;
  double *values;
  double *bounds;
  char **cvalues;
  int revert;
  int offset;
  char type;
  char attributes_values_char[CMOR_MAX_ATTRIBUTES][CMOR_MAX_STRING];
  double attributes_values_num[CMOR_MAX_ATTRIBUTES];
  char attributes_type[CMOR_MAX_ATTRIBUTES]; /*stores attributes type */
  char attributes[CMOR_MAX_ATTRIBUTES][CMOR_MAX_STRING]; /*stores attributes names */
  int nattributes ; /* number of character type attributes */
  int hybrid_in;
  int hybrid_out;
  int store_in_netcdf;
} cmor_axis_t;
extern cmor_axis_t cmor_axes[CMOR_MAX_AXES];

typedef struct cmor_variable_def_ {
  int table_id;
  char id[CMOR_MAX_STRING];
  char standard_name[CMOR_MAX_STRING];
  char units[CMOR_MAX_STRING];
  char cell_methods[CMOR_MAX_STRING];
  char cell_measures[CMOR_MAX_STRING];
  char positive;
  char flag_values[CMOR_MAX_STRING];
  char flag_meanings[CMOR_MAX_STRING];
  char long_name[CMOR_MAX_STRING];
  char comment[CMOR_MAX_STRING];
  int ndims;
  int dimensions[CMOR_MAX_DIMENSIONS];
  char type;
  float valid_min;
  float valid_max;
  float ok_min_mean_abs;
  float ok_max_mean_abs;
  int shuffle;
  int deflate;
  int deflate_level;
  char required[CMOR_MAX_STRING];
  char realm[CMOR_MAX_STRING];
  char frequency[CMOR_MAX_STRING];
  char out_name[CMOR_MAX_STRING];
} cmor_var_def_t;

typedef struct cmor_var_ {
  int self;
  int grid_id;
  int sign;
  int zfactor;
  int ref_table_id;
  int ref_var_id;
  int initialized;
  int closed;
  int nc_var_id;
  int nc_zfactors[CMOR_MAX_VARIABLES];
  int nzfactor;
  int ntimes_written;
  int ntimes_written_coords[10];
  int associated_ids[10];
  int ntimes_written_associated[10];
  int time_nc_id;
  int time_bnds_nc_id;
  char id[CMOR_MAX_STRING];
  int ndims;
  int singleton_ids[CMOR_MAX_DIMENSIONS];
  int axes_ids[CMOR_MAX_DIMENSIONS];
  int original_order[CMOR_MAX_DIMENSIONS];
  char attributes_values_char[CMOR_MAX_ATTRIBUTES][CMOR_MAX_STRING];
  double attributes_values_num[CMOR_MAX_ATTRIBUTES];
  char attributes_type[CMOR_MAX_ATTRIBUTES]; /*stores attributes type */
  char attributes[CMOR_MAX_ATTRIBUTES][CMOR_MAX_STRING]; /*stores attributes names */
  int nattributes ; /* number of  attributes */
  char type;
  char itype;
  double missing;
  double omissing;
  double tolerance;
  float valid_min;
  float valid_max;
  float ok_min_mean_abs;
  float ok_max_mean_abs;
  int shuffle;
  int deflate;
  int deflate_level;
  int nomissing;
  char iunits[CMOR_MAX_STRING];
  char ounits[CMOR_MAX_STRING];
  int isbounds;
  int needsinit; /* need to be init or associated to file */
  int zaxis; /* for z vars, associated axis stored here */
  double *values;
  double first_time;
  double last_time;
  double first_bound;
  double last_bound;
  char base_path[CMOR_MAX_STRING];
  char current_path[CMOR_MAX_STRING];
  char suffix[CMOR_MAX_STRING];
  int suffix_has_date;
} cmor_var_t;

extern cmor_var_t cmor_vars[CMOR_MAX_VARIABLES];

typedef struct cmor_mappings_ {
  int nattributes;
  char id[CMOR_MAX_STRING];
  char attributes_names[CMOR_MAX_GRID_ATTRIBUTES][CMOR_MAX_STRING];
  char coordinates[CMOR_MAX_STRING];
} cmor_mappings_t;

typedef struct cmor_table_ {
  int id;
  int nvars;
  int naxes;
  int nexps;
  int nmappings;
  float cf_version;
  float cmor_version;
  char project_id[CMOR_MAX_STRING];
  char table_id[CMOR_MAX_STRING];
  char expt_ids[CMOR_MAX_ELEMENTS][CMOR_MAX_STRING];
  char sht_expt_ids[CMOR_MAX_ELEMENTS][CMOR_MAX_STRING];
  char date[CMOR_MAX_STRING];
  cmor_axis_def_t axes[CMOR_MAX_ELEMENTS];
  cmor_var_def_t vars[CMOR_MAX_ELEMENTS];
  cmor_mappings_t mappings[CMOR_MAX_ELEMENTS];
  float missing_value;
  double interval;
  float interval_warning;
  float interval_error;
  char URL[CMOR_MAX_STRING];
  char product[CMOR_MAX_STRING];
  char realm[CMOR_MAX_STRING];
  char path[CMOR_MAX_STRING];
  char required_gbl_att[CMOR_MAX_STRING];
  char frequency[CMOR_MAX_STRING];
  char **forcings;
  int nforcings;
  unsigned char md5[16];
  char generic_levels[CMOR_MAX_ELEMENTS][CMOR_MAX_STRING];
} cmor_table_t;

extern cmor_table_t cmor_tables[CMOR_MAX_TABLES];

typedef struct cmor_dataset_def_ {
  char outpath[CMOR_MAX_STRING];
  char conventions[CMOR_MAX_STRING];
  char attributes_names[CMOR_MAX_ATTRIBUTES][CMOR_MAX_STRING];
  char attributes_values[CMOR_MAX_ATTRIBUTES][CMOR_MAX_STRING];
  int nattributes;
  int realization;
  int leap_year;
  int leap_month;
  int month_lengths[12];
  int initiated;
  int associate_file; /*flag to store associated variables separately */
  int associated_file; /* ncid of associated file */
  char associated_file_name[CMOR_MAX_STRING]; /*associated file path */
  char tracking_id[CMOR_MAX_STRING]; /*associated tracking id */
} cmor_dataset_def;

extern cmor_dataset_def cmor_current_dataset;

/* Now the funcs declarations */
#include "cmor_func_def.h"
#endif
