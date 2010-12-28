#ifndef NC_UNSPECIFIED
#define NC_UNSPECIFIED 0
#endif

/* id: netcdf_std.h,v 1.1 1995/05/02 20:23:12 jac Exp $ */
/* Revision 1.1  1995/05/02  20:23:12  jac */
/* Initial revision */
/* */

/* 	=== Header file for standard netCDF format ===		*/

#define NUM_REQD_DIMS		3
#define NUM_REQD_VARS		3
#define NUM_REQD_GATTS		3
#define NUM_REQD_VATTS		13
#define NUM_REQD_TATTS		7
#define NUM_OPT_TATTS		2
#define NUM_REQD_LLATTS		3
#define NUM_OPT_ATTS		1
#define NUM_FREQUENCY_VARS	5
#define NUM_FREQUENCY_DIMS	1

#define NUM_REQD_COOP_VATTS	14
#define NUM_REQD_LEVEL_ATTS	4
#define NUM_REQD_COOP_VARS	3
#define NUM_REQD_COOP_DIMS	3

int         num_reqd_vatts,
            num_reqd_vars,
            num_reqd_dims;

/* this value should reflect the current max number of 		*/
/* attributes that are required for any of the variables.	*/
#define MAX_NUM_REQD_ATTS	14

/* this value should reflect the current max number of 		*/
/* dimensions that are required for any of the variables.	*/
#define MAX_NUM_REQD_DIMS	4

/* this value should reflect the max number of any sort of 	*/
/* required netCDF file component, be it dimensions, variables, */
/* or attributes.						*/
#define MAX_REQD		13

#define TIME_IX			0
#define LAT_IX			1
#define LON_IX			2
#define T_UNITS_IX		1
#define DELTA_T_IX		2
#define AVG_PER_IX		3
#define TIME_ARANGE		4
#define LTM_RANGE_IX		5
#define PREV_AVG_PER		6
#define DATASET_IX		0
#define DESC_IX			1
#define LEVEL_IX		2
#define STAT_IX			3
#define PSTAT_IX		4
#define VRANGE_IX		5
#define ARANGE_IX		6
#define PRECIS_IX		7
#define UNITS_IX		8
#define MISSING_IX		9
#define VTITLE_IX		10
#define OFFSET_IX		11
#define SCALE_IX		12
#define FILL_IX			0
#define GTITLE_IX		1
#define HISTORY_IX		2
#define LL_UNITS_IX		1
#define LL_ARANGE		2
#define FREQUENCY_IX		0
#define POWERX_IX		1
#define POWERY_IX		2
#define PHASE_IX		3
#define COH2_IX			4

char       **dims;
static char *cdc_dims[NUM_REQD_DIMS] = {
  "time",
  "lat",
  "lon"
};

static char *coop_dims[NUM_REQD_COOP_DIMS] = {
  "time",
  "lat",
  "lon"
};

/* even though this duplicates the required dimensions (since these */
/* are coordinate variables), define variables for readability.     */
char       **vars;
static char *cdc_vars[NUM_REQD_VARS] = {
  "time",
  "lat",
  "lon"
};

static char *coop_vars[NUM_REQD_COOP_VARS] = {
  "time",
  "lat",
  "lon"
};

nc_type     *var_type;
static nc_type cdc_var_type[NUM_REQD_VARS] = {
  NC_DOUBLE,
  NC_FLOAT,
  NC_FLOAT
};

static nc_type coop_var_type[NUM_REQD_COOP_VARS] = {
  NC_DOUBLE,
  NC_FLOAT,
  NC_FLOAT
};



char       **var_atts;
static char *cdc_var_atts[NUM_REQD_VATTS] = {
  "dataset",
  "var_desc",
  "level_desc",
  "statistic",
  "parent_stat",
  "valid_range",
  "actual_range",
  "precision",
  "units",
  "missing_value",
  "title",
  "add_offset",
  "scale_factor"
};

static char *coop_var_atts[NUM_REQD_COOP_VATTS] = {
  "dataset",
  "var_desc",
  "level_desc",
  "statistic",
  "parent_stat",
  "valid_range",
  "actual_range",
  "precision",
  "units",
  "missing_value",
  "long_name",
  "add_offset",
  "scale_factor",
  "least_significant_digit"
};

nc_type     *var_atts_type;
static nc_type cdc_var_atts_type[NUM_REQD_VATTS] = {
  NC_CHAR,
  NC_CHAR,
  NC_CHAR,
  NC_CHAR,
  NC_CHAR,
  NC_UNSPECIFIED,
  NC_UNSPECIFIED,
  NC_SHORT,
  NC_CHAR,
  NC_UNSPECIFIED,
  NC_CHAR,
  NC_UNSPECIFIED,
  NC_UNSPECIFIED
};

static nc_type coop_var_atts_type[NUM_REQD_COOP_VATTS] = {
  NC_CHAR,
  NC_CHAR,
  NC_CHAR,
  NC_CHAR,
  NC_CHAR,
  NC_UNSPECIFIED,
  NC_UNSPECIFIED,
  NC_SHORT,
  NC_CHAR,
  NC_UNSPECIFIED,
  NC_CHAR,
  NC_UNSPECIFIED,
  NC_UNSPECIFIED,
  NC_SHORT
};

char       **var_atts_val;
static char *cdc_var_atts_val[NUM_REQD_VATTS] = {
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  "MISS",			/* lower valid range value only */
  "MISS",			/* lower actual range value only */
  "-9999",
  NULL,
  "MISS",
  NULL,
  "0",				/* use default offset for variable type */
  "1"				/* use default scale for variable type */
};

static char *coop_var_atts_val[NUM_REQD_COOP_VATTS] = {
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  "MISS",			/* lower valid range value only */
  "MISS",			/* lower actual range value only */
  "-9999",
  NULL,
  "MISS",
  NULL,
  "0",				/* use default offset for variable type */
  "1",				/* use default scale for variable type */
  "2"
};

char       **obs_atts_val;
static char *cdc_obs_atts_val[NUM_REQD_VATTS] = {
  NULL,
  NULL,
  NULL,
  "N",
  "I",
  "0",				/* lower valid range value only */
  "0",				/* lower actual range value only */
  "0",
  "observations",
  "MISS",
  NULL,
  "0",				/* use default offset for variable type */
  "1"				/* use default scale for variable type */
};

static char *coop_obs_atts_val[NUM_REQD_COOP_VATTS] = {
  NULL,
  NULL,
  NULL,
  "N",
  "I",
  "0",				/* lower valid range value only */
  "0",				/* lower actual range value only */
  "0",
  "observations",
  "MISS",
  NULL,
  "0",				/* use default offset for variable type */
  "1",				/* use default scale for variable type */
  "2"
};

/* reserve X to indicate none */
char       *vatts_abbrev;
static char cdc_vatts_abbrev[NUM_REQD_VATTS] = {
  'D',
  'V',
  'L',
  'S',
  'P',
  'R',
  'A',
  'E',
  'U',
  'M',
  'T',
  'O',
  'F'
};

/* reserve X to indicate none */
static char coop_vatts_abbrev[NUM_REQD_COOP_VATTS] = {
  'D',
  'V',
  'L',
  'S',
  'P',
  'R',
  'A',
  'E',
  'U',
  'M',
  'T',
  'O',
  'F',
  'Q'
};


char       **time_atts;
static char *cdc_time_atts[NUM_REQD_TATTS] = {
  "title",
  "units",
  "delta_t",
  "avg_period",
  "valid_range",
  "ltm_range",
  "prev_avg_period"
};

static char *coop_time_atts[NUM_REQD_TATTS] = {
  "long_name",
  "units",
  "delta_t",
  "avg_period",
  "actual_range",
  "ltm_range",
  "prev_avg_period"
};


char       **time_atts_val;
static char *cdc_time_atts_val[NUM_REQD_TATTS] = {
  "Time",
  "yyyymmddhhmmss",
  NULL,
  "0000-00-00 00:00:00",
  NULL,
  "MISSING",			/* this is the default for both range values */
  "0000-00-00 00:00:00"
};

static char *coop_time_atts_val[NUM_REQD_TATTS] = {
  "Time",
  "hours since 0001-01-01 00:00:00",
  NULL,
  "0000-00-00 00:00:00",
  NULL,
  "MISSING",			/* this is the default for both range values */
  "0000-00-00 00:00:00"
};


char       **latlon_atts;
static char *cdc_latlon_atts[NUM_REQD_LLATTS] = {
  "title",
  "units",
  "valid_range"
};

static char *coop_latlon_atts[NUM_REQD_LLATTS] = {
  "long_name",
  "units",
  "actual_range"
};



/* id: time.h,v 1.1 1995/05/31 23:56:27 jac Exp $ */
/* Revision 1.1  1995/05/31  23:56:27  jac */
/* Initial revision */
/* */

/* time definitions */
#define HOURS_PER_DAY		24
#define HOURS_PER_YR		8760
#define HOURS_PER_LEAP_YR	8784
#define DAYS_PER_PENTAD		5
#define DAYS_PER_YR		365
#define DAYS_PER_LEAP_YR	366
#define MONTHS_PER_SEASON	3
#define MONTHS_PER_YR		12
#define PENTADS_PER_YR		73
#define SEASONS_PER_YR		4

#define PMONTHS		0
#define PSEASONS	1
#define PYEARS		2
#define PDAYS		3
#define PHOURS		4
#define PPENTADS	5
#define MAXPER		6
