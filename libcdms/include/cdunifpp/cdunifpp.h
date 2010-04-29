/*
 *
 *    Copyright (C) 2004-2006 NERC DataGrid
 *    This software may be distributed under the terms of the
 *    CCLRC Licence for CCLRC Software
 * <CDATDIR>/External_License/CCLRC_CDAT_License.txt 
 *
 */
/* Header file for stuff used internally by cdunif_pp */


/* all the headers we use */
#include "cdunif.h"
#include "cdunifint.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <ctype.h>
#include <unistd.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/*---------------------------------------------------------*/

#define CDUNIFPP_VERSION "0.13"

/*---------------------------------------------------------*/

/* Data types for internal calculations:
 *
 * NOTE: Fint and Freal must be the same length as each other
 * and inttype and realtype must be set accordingly
 *
 * Currently, this *must* be four-byte, because there is NO CuType
 * corresponding to 8-byte integer on Linux/gcc (would need to be
 * long long, but CuLong corresponds to a long)
 *  sizes:   int = 4,  long = 4,  long long = 8
 * 
 */

typedef int Fint;
typedef float Freal;

static const CuType inttype  = CuInt;
static const CuType realtype = CuFloat;

static const int wordsize = sizeof(Fint);

/* for float comparisons - related to word size */
static const float tolerance = 1e-4;


/* data types of exact length -- may need to change according to platform
 * (these are double-checked at run-time in pp_check_sizes() 
 */

typedef int Fint4;
typedef long long Fint8;
typedef float Freal4;
typedef double Freal8;

/*---------------------------------------------------------*/

/* endian-ness - set or clear LITTLE_ENDIAN_MACHINE *
 * currently seems that BYTESWAP is set for us, so use that,
 * but could change to include endian.h and then
 * test using #if (__BYTE_ORDER == __LITTLE_ENDIAN)
 */
#ifdef BYTESWAP
#define LITTLE_ENDIAN_MACHINE
#else
#undef LITTLE_ENDIAN_MACHINE
#endif

/*---------------------------------------------------------*/

/*longest string attribute value */
#define MAX_ATT_LEN 120

/*---------------------------------------------------------*/

/* Define the following symbol if you want to read in the half-level
 * values and use them in pp_compare_levels() as extra conditions
 * for two levels to match.  (Grep the code for BDY_LEVS for details.)
 *
 * Currently undefined, as if these have *not* been set consistently
 * in the header, this could cause more problems than it solves.  In
 * particular, BULEV and BHULEV header elements were still BRSVD1 and
 * BRSVD2 ("reserved for future use") at UM vn 4.5.
 */

/* #define BDY_LEVS */


/*---------------------------------------------------------*/

/* constants */

static const int n_int_hdr = 45;
static const int n_real_hdr = 19;
/* static const int n_hdr = n_int_hdr + n_real_hdr; */
static const int n_hdr = 64;

/* int_fill_value is output */
static const int int_fill_value = -32768;

/* int_missing_data is convention in input file */
static const int int_missing_data = -32768;

static const Freal reference_pressure = 1e5; /* Pa */

/* origin time */
static const int default_year_orig = 1970;
static const int default_month_orig = 1;
static const int default_day_orig = 1;
static const int default_hour_orig =  0;
static const int default_minute_orig = 0;
static const int default_second_orig = 0;

static const Freal lon_modulo=360.;

static const Freal pressure_scaling=100.; /* hPa to Pa */


/*---------------------------------------------------------*/

/* first, for convenience, typedefs for all the structs */
typedef struct pp_file PPfile;
typedef struct pp_var PPvar;
typedef struct pp_dim PPdim;
typedef struct pp_rec PPrec;
typedef struct pp_hdr PPhdr;
typedef struct pp_stashmeta PPstashmeta;
typedef struct pp_dimnames PPdimnames;
typedef struct pp_data PPdata;
typedef struct pp_list PPlist;
typedef struct pp_listel PPlistel;
typedef struct pp_listhandle PPlisthandle;

typedef struct pp_fieldvar PPfieldvar;
typedef struct pp_genaxis PPgenaxis;
typedef struct pp_regaxis PPregaxis;
typedef struct pp_rotgrid PProtgrid;
typedef struct pp_rotmap PProtmap;
typedef struct pp_zaxis PPzaxis;
typedef struct pp_level PPlevel;
typedef struct pp_taxis PPtaxis;
typedef struct pp_time PPtime;
typedef struct pp_xsaxis PPxsaxis;
typedef struct pp_date PPdate;
typedef struct pp_landmask PPlandmask;
typedef struct pp_extra PPextra;

/* similar typedefs for enums are below */

enum pp_calendartype {
  gregorian, 
  cal360day,
  model
};

enum pp_filetype {
  pp_type,
  um_type
};

enum pp_convert {
  convert_none,
  convert_int,
  convert_real
};

enum pp_axistype {
  xaxistype,
  yaxistype,
  zaxistype,
  taxistype,
  num_axistype
};

enum pp_axisgentype {
  regaxis_type,
  zaxis_type,
  taxis_type,
  xsaxis_type
}; /* JAK 2005-01-5 */

enum pp_axisregtype {
  xregtype,
  yregtype
};

enum pp_lev_type {
  pseudo_lev_type,
  height_lev_type,
  depth_lev_type,
  hybrid_sigmap_lev_type,
  hybrid_height_lev_type,
  pressure_lev_type,
  soil_lev_type,
  boundary_layer_top_lev_type,
  top_of_atmos_lev_type,
  mean_sea_lev_type,
  surface_lev_type,
  tropopause_lev_type,
  other_lev_type
};

enum pp_lev_val_type {
  lev_type,
  hybrid_sigmap_a_type,
  hybrid_sigmap_b_type,
  hybrid_height_a_type,
  hybrid_height_b_type
};

enum pp_dir {
  xdir,
  ydir,
  zdir,
  tdir
};

enum pp_extra_vec {
  extra_x=1, 
  extra_y=2,
  extra_title=10
};

typedef enum pp_convert PPconvert;
typedef enum pp_filetype PPfiletype;
typedef enum pp_axistype PPaxistype;
typedef enum pp_lev_type PPlevtype;
typedef enum pp_lev_val_type PPlevvaltype;
typedef enum pp_calendartype PPcalendartype;
typedef enum pp_axisgentype PPaxisgentype; /* JAK 2005-01-05 */
typedef enum pp_axisregtype PPaxisregtype;
typedef enum pp_dir PPdir;
typedef enum pp_extra_vec PPextravec;

/* Structures for all sorts of things. */

/* (In the rare case where structures are included in another directly,
 * not as a pointer, need to be defined in correct order.  This currently
 * only affects struct pp_date)
 */

/* --- these three are internal structures we will hang off the internp 
 * elements of CuFile, CuVar, CuDim
 */
struct pp_file {
  FILE *fh; /* stdio FILE handle */
  int nrec; /* number of PP records */
  PPrec **recs; /* records */
  PPlist *heaplist; /* heap memory */
  PPfiletype type; /* PP or UM */
  int swap; /* true if byte swap */
  int wordsize; /* in bytes */
  PPlandmask *landmask;
};
struct pp_var {
  int firstrecno; /* for fieldvar */
  int lastrecno;  /* for fieldvar */
  PPdata *data;  /* for dim var */

  PPlist *atts;  /* temporary place to store attributes 
		  * until the number is known and they can be
		  * added properly
		  */
};

/*
 * struct pp_dim {
 * };
 */

/*------------------------------------------------------*/

/* metadata which is not contained in the header but looked up 
 * as a function of the stash codes
 */
#define SM_MAX_LEN MAX_ATT_LEN
struct pp_stashmeta {
  char longname[SM_MAX_LEN+1];
  char units[SM_MAX_LEN+1];
  char shortname[SM_MAX_LEN+1];
  char stdname[SM_MAX_LEN+1];

  /* "source" is where the lookup comes from.
   *
   * Quite deliberately there is only one "source", rather than separate sources 
   * for the various names and units (even though the STASHmaster file has long
   * name but not units in a usable form).  These have to go together.  It is
   * no good using a STASHmaster file to override some diagnostics and then using 
   * compiled-in units lookup.  Therefore the means of overriding the lookup
   * with table-driven input is going to have to be something better than
   * STASHmaster files.
   */
  char source[SM_MAX_LEN+1];
};

/* dimension names (for setting cell methods) */
struct pp_dimnames {
  /* we will not be modifying these strings, so declare as const
   * in order to copy in pointers that are declared const without warnings
   */
  const char *x;
  const char *y;
  const char *z;
  const char *t;
};

/* structure for temporary use while scanning fieldvars */
struct pp_fieldvar {
  int firstrecno; /* first record number */
  int lastrecno; /* last record number */
  PPrec *firstrec; /* first record */
  PPlist *axes; /* JAK 2005-01-05 */
  PProtgrid *rotgrid;  /* link to rotated grid, which will 
			* be used for CF "coordinates" stuff
			*/
  PPdimnames dim_names;
  PPstashmeta stashmeta;
  PPhdr *first_header;
};


struct pp_data {
  CuType type;
  int n;
  void *values;
};

/* for linked list */
struct pp_list {
  int n;
  PPlistel *first;
  PPlistel *last;
};

struct pp_listel {
  void *ptr;
  PPlistel *prev;
  PPlistel *next;
};

struct pp_listhandle {
  /* This is a little structure which stores the information needed for pp_list_walk.
   * Its main purpose is to store the position outside the list structure itself,
   * so that for read-only scanning of the list, the PPlist* can be declared
   * as const.
   */
  PPlistel *current;
  const PPlist *list;
};

/* land mask */
struct pp_landmask {
  PPgenaxis *xaxis;
  PPgenaxis *yaxis;
  PPdata *data;
};

struct pp_genaxis { /* JAK 2005-01-04 */
  int dimid;
  PPaxisgentype gentype;
  PPdir dir;
  void *axis;
};

struct pp_rotmap {
  Freal pole_lon;
  Freal pole_lat;
  Freal truepole_gridlon;
  CuVar *map_var;
  char name[CU_MAX_NAME+1];
};


struct pp_rotgrid {
  /* A rotated grid consists of a mapping and actual x and y values 
   *
   * Very typically, a mesoscale file will contain only a single mapping
   * but will contain u and p grids with this mapping.  These two grids
   * will need separate lon,lat coordinate variables.
   */
  PProtmap *rotmap;
  PPgenaxis *xaxis;
  PPgenaxis *yaxis;
  char coords[MAX_ATT_LEN+1];
};


/* NON_ROTATED is a pointer value which can be set to imply that an axis
 * is not rotated, which is non-zero but will not match any real-life memory
 * location, so that pp_get_rotmap() can return it for a non-rotated axis, 
 * while returning NULL for a problem such as unable to allocate memory
 */

#define NON_ROTATED ((PProtmap *) 1)

/* and analogously... */

#define NON_ROTATED_GRID ((PProtgrid *) 1)


/* regaxis used for x or y axes */
struct pp_regaxis {
  int dimid;
  PPaxisregtype type;
  Fint n;
  Freal start;
  Freal interval;
  PProtmap *rotmap; 
};

/* zaxis: on reading in a vertical axis, we will ASAP set pp_lev_type to correct
 * type (see the enum above) by calling pp_level_type().  All tests should ideally 
 * be done on pp_lev_type.  Additionally we store lbvc which is a copy of the level
 * code in the header, for use "in extremis", but really lbvc should not be used
 * outside of pp_zaxis_set() and pp_level_type() itself, so if you are looking for
 * something to fix then grep the code for other instances of lbvc...
 */

struct pp_zaxis {
  int dimid;
  PPlevtype lev_type;
  Fint lbvc;
  PPlist *values;
};

struct pp_level {
  PPlevtype type;

  union {

    struct { 
      Freal level;
#ifdef BDY_LEVS
      Freal ubdy_level;
      Freal lbdy_level;
#endif
    } misc;

    struct {
      Freal a;
      Freal b;
#ifdef BDY_LEVS
      Freal ubdy_a;
      Freal ubdy_b;
      Freal lbdy_a;
      Freal lbdy_b;
#endif
    } hybrid_sigmap;

    struct { 
      Freal a;
      Freal b;
#ifdef BDY_LEVS
      Freal ubdy_a;
      Freal ubdy_b;
      Freal lbdy_a;
      Freal lbdy_b;
#endif
    } hybrid_height;
    
    struct { 
      Fint index;
    } pseudo;
    
  } values;
};

struct pp_date {
  /* this is a generic date */
  Fint year;
  Fint month;
  Fint day;
  Fint hour;
  Fint minute;
  Fint second;
};

struct pp_taxis {
  int dimid;
  Fint type;
  PPlist *values;
  PPdate time_orig;
};

struct pp_time {
  /* this is a value on time axis */
  Fint type;
  PPdate time1;
  PPdate time2;
};

struct pp_xsaxis {  /* could be pp cross section? */
  Fint axiscode;
  PPdata *data;
};

/* These #defines control which of the PP header elements are to be stored in memory.
 * To reduce memory use, comment out the ones which are not required.
 * If you comment out one which is needed, the error should be at compile time.
 */

#define PP_STORE_LBYR
#define PP_STORE_LBMON
#define PP_STORE_LBDAT
#define PP_STORE_LBHR
#define PP_STORE_LBMIN
#define PP_STORE_LBDAY
#define PP_STORE_LBYRD
#define PP_STORE_LBMOND
#define PP_STORE_LBDATD
#define PP_STORE_LBHRD
#define PP_STORE_LBMIND
#define PP_STORE_LBDAYD
#define PP_STORE_LBTIM
#define PP_STORE_LBFT
#define PP_STORE_LBLREC
#define PP_STORE_LBCODE
#define PP_STORE_LBHEM
#define PP_STORE_LBROW
#define PP_STORE_LBNPT
#define PP_STORE_LBEXT
#define PP_STORE_LBPACK
#define PP_STORE_LBREL
#define PP_STORE_LBFC
#define PP_STORE_LBCFC
#define PP_STORE_LBPROC
#define PP_STORE_LBVC
#define PP_STORE_LBRVC
#define PP_STORE_LBEXP
#define PP_STORE_LBBEGIN
#define PP_STORE_LBNREC
#define PP_STORE_LBPROJ
#define PP_STORE_LBTYP
#define PP_STORE_LBLEV
#define PP_STORE_LBRSVD1
#define PP_STORE_LBRSVD2
#define PP_STORE_LBRSVD3
#define PP_STORE_LBRSVD4
#define PP_STORE_LBSRCE
#define PP_STORE_LBUSER1
#define PP_STORE_LBUSER2
#define PP_STORE_LBUSER3
#define PP_STORE_LBUSER4
#define PP_STORE_LBUSER5
#define PP_STORE_LBUSER6
#define PP_STORE_LBUSER7
#define PP_STORE_BULEV
#define PP_STORE_BHULEV
#define PP_STORE_BRSVD3
#define PP_STORE_BRSVD4
#define PP_STORE_BDATUM
#define PP_STORE_BACC
#define PP_STORE_BLEV
#define PP_STORE_BRLEV
#define PP_STORE_BHLEV
#define PP_STORE_BHRLEV
#define PP_STORE_BPLAT
#define PP_STORE_BPLON
#define PP_STORE_BGOR
#define PP_STORE_BZY
#define PP_STORE_BDY
#define PP_STORE_BZX
#define PP_STORE_BDX
#define PP_STORE_BMDI
#define PP_STORE_BMKS


struct pp_hdr {
#ifdef PP_STORE_LBYR
  Fint LBYR;     
#endif
#ifdef PP_STORE_LBMON
  Fint LBMON;    
#endif
#ifdef PP_STORE_LBDAT
  Fint LBDAT;    
#endif
#ifdef PP_STORE_LBHR
  Fint LBHR;     
#endif
#ifdef PP_STORE_LBMIN
  Fint LBMIN;    
#endif
#ifdef PP_STORE_LBDAY
  Fint LBDAY;    
#endif
#ifdef PP_STORE_LBYRD
  Fint LBYRD;    
#endif
#ifdef PP_STORE_LBMOND
  Fint LBMOND;   
#endif
#ifdef PP_STORE_LBDATD
  Fint LBDATD;   
#endif
#ifdef PP_STORE_LBHRD
  Fint LBHRD;    
#endif
#ifdef PP_STORE_LBMIND
  Fint LBMIND;   
#endif
#ifdef PP_STORE_LBDAYD
  Fint LBDAYD;   
#endif
#ifdef PP_STORE_LBTIM
  Fint LBTIM;    
#endif
#ifdef PP_STORE_LBFT
  Fint LBFT;     
#endif
#ifdef PP_STORE_LBLREC
  Fint LBLREC;   
#endif
#ifdef PP_STORE_LBCODE
  Fint LBCODE;   
#endif
#ifdef PP_STORE_LBHEM
  Fint LBHEM;    
#endif
#ifdef PP_STORE_LBROW
  Fint LBROW;    
#endif
#ifdef PP_STORE_LBNPT
  Fint LBNPT;    
#endif
#ifdef PP_STORE_LBEXT
  Fint LBEXT;    
#endif
#ifdef PP_STORE_LBPACK
  Fint LBPACK;   
#endif
#ifdef PP_STORE_LBREL
  Fint LBREL;    
#endif
#ifdef PP_STORE_LBFC
  Fint LBFC;     
#endif
#ifdef PP_STORE_LBCFC
  Fint LBCFC;    
#endif
#ifdef PP_STORE_LBPROC
  Fint LBPROC;   
#endif
#ifdef PP_STORE_LBVC
  Fint LBVC;     
#endif
#ifdef PP_STORE_LBRVC
  Fint LBRVC;    
#endif
#ifdef PP_STORE_LBEXP
  Fint LBEXP;    
#endif
#ifdef PP_STORE_LBBEGIN
  Fint LBBEGIN;   
#endif
#ifdef PP_STORE_LBNREC
  Fint LBNREC;   
#endif
#ifdef PP_STORE_LBPROJ
  Fint LBPROJ;   
#endif
#ifdef PP_STORE_LBTYP
  Fint LBTYP;    
#endif
#ifdef PP_STORE_LBLEV
  Fint LBLEV;    
#endif
#ifdef PP_STORE_LBRSVD1
  Fint LBRSVD1;  
#endif
#ifdef PP_STORE_LBRSVD2
  Fint LBRSVD2;  
#endif
#ifdef PP_STORE_LBRSVD3
  Fint LBRSVD3;  
#endif
#ifdef PP_STORE_LBRSVD4
  Fint LBRSVD4;  
#endif
#ifdef PP_STORE_LBSRCE
  Fint LBSRCE;   
#endif
#ifdef PP_STORE_LBUSER1
  Fint LBUSER1;  
#endif
#ifdef PP_STORE_LBUSER2
  Fint LBUSER2;  
#endif
#ifdef PP_STORE_LBUSER3
  Fint LBUSER3;  
#endif
#ifdef PP_STORE_LBUSER4
  Fint LBUSER4;  
#endif
#ifdef PP_STORE_LBUSER5
  Fint LBUSER5;  
#endif
#ifdef PP_STORE_LBUSER6
  Fint LBUSER6;  
#endif
#ifdef PP_STORE_LBUSER7
  Fint LBUSER7;  
#endif
#ifdef PP_STORE_BULEV
  Freal BULEV;
#endif
#ifdef PP_STORE_BHULEV
  Freal BHULEV;
#endif
#ifdef PP_STORE_BRSVD3
  Freal BRSVD3;
#endif
#ifdef PP_STORE_BRSVD4
  Freal BRSVD4;
#endif
#ifdef PP_STORE_BDATUM
  Freal BDATUM;
#endif
#ifdef PP_STORE_BACC
  Freal BACC;  
#endif
#ifdef PP_STORE_BLEV
  Freal BLEV;  
#endif
#ifdef PP_STORE_BRLEV
  Freal BRLEV; 
#endif
#ifdef PP_STORE_BHLEV
  Freal BHLEV; 
#endif
#ifdef PP_STORE_BHRLEV
  Freal BHRLEV;
#endif
#ifdef PP_STORE_BPLAT
  Freal BPLAT; 
#endif
#ifdef PP_STORE_BPLON
  Freal BPLON; 
#endif
#ifdef PP_STORE_BGOR
  Freal BGOR;  
#endif
#ifdef PP_STORE_BZY
  Freal BZY;   
#endif
#ifdef PP_STORE_BDY
  Freal BDY;   
#endif
#ifdef PP_STORE_BZX
  Freal BZX;   
#endif
#ifdef PP_STORE_BDX
  Freal BDX;   
#endif
#ifdef PP_STORE_BMDI
  Freal BMDI;  
#endif
#ifdef PP_STORE_BMKS
  Freal BMKS;  
#endif
};

struct pp_rec {
  int recno; /* record number */
  PPhdr hdr; /* header structure */
  long datapos; /* file pos data start (after any fortran record length int) in bytes */
  long disklen; /* length on disks (words) -- including padding + before unpacking */
  long datalen; /* data length (words) */ 

  PPlevel *lev;
  PPtime *time;
  int zindex; /* index on z axis within a variable - used for detecting vars with irreg z,t */
  int tindex; /* index on t axis within a variable - used for detecting vars with irreg z,t */
  int disambig_index; /* index used for splitting variables with irreg z,t into 
		       * sets of variables with regular z,t */
  int supervar_index; /* when a variable is split, this is set to an index which is common
		       * across the set, but different from sets generated from other
		       * super-variables
		       */
  Freal mean_period; /* period (in days) of time mean 
			(store here so as to calculate once only) */
};


/*---------------------------------------------------------*/

/* Function like macros */
/* The code profiler suggests that pp_compare_ints and pp_compare_reals are candidates for
 * inlining; however, unfortunately this sometimes gets compiled with c89 which doesn't support
 * inline functions.  Use a #define for pp_compare_ints.  pp_compare_reals, which is more
 * awkward to #define, is just going to have to stay as it is for now (it's called less often).
 */

/* ----------------------------------------------------------- */

/* Define some values which can be used for initialising variables
 * to no particular value, but may aid debugging to not have them
 * completely uninitialised.  Should not be tested for in production
 * code, as these could still happen to match a genuine value.
 */

#define UNSET_INT -999
#define UNSET_REAL 1e9

/* ----------------------------------------------------------- */

#define pp_compare_ints(a,b) ((a)<(b)?(-1):(a)>(b)?1:0)

/* ----------------------------------------------------------- */

/* error-checking macros */

/* these are to allow a compact way of incorporating error-checking of 
 * the return value of a function call, without obfuscating the basic purpose
 * of the line of code, which is executing the function call.
 *
 * CKI used for integer functions which return negative value on failure
 * CKP used for pointer functions which return NULL on failure
 * CKF for floats for good measure (probably not used)
 *
 * put the ERRBLK (or ERRBLKI or ERRBLKP) at the end of the subroutine, with 
 * the "label" argument set to the subroutine name (as a string)
 */

#define FLT_ERR -1e38

#ifdef DEBUG
#define ERR abort();
#else
/* ERR: unconditional branch */
#define ERR goto err;
#endif

#define CKI(i)  if ((i) < 0){ ERR }
#define CKP(p)  if ((p) == NULL){ ERR }
#define CKF(f)  if ((f) == FLT_ERR){ ERR }

/* ERRIF: conditional branch */
#define ERRIF(i)  if (i){ ERR }

#define ERRBLK(label,rtn) err: pp_error(label); return (rtn);
#define ERRBLKI(label) ERRBLK((label),-1);
#define ERRBLKP(label) ERRBLK((label),NULL);
#define ERRBLKF(label) ERRBLK((label),FLT_ERR);


/* ----------------------------------------------------------- */

/* prototypes */

/* in cdunifpp_attribute.c: */
CuAtt *pp_att_new(const char *, CuType, long, const void *, PPlist *);
int pp_add_att(PPlist *, const char *, CuType, long, const void *, PPlist *);
CuAtt *pp_string_att_new(const char *, const char *, PPlist *);
int pp_add_string_att(PPlist *, const char *, const char *, PPlist *);
int pp_add_string_att_if_set(PPlist *, const char *, const char *, PPlist *);
int pp_copy_and_free_atts(CuFile *, CuVar *, PPlist *, PPlist *);

/* in cdunifpp_axisvals.c: */
PPdata *pp_data_new(CuType,int,PPlist *);
PPdata *pp_regaxis_to_values(const PPregaxis *, PPlist *);
PPdata *pp_zaxis_to_values(const PPzaxis *, PPlevvaltype, PPlist *);
PPdata *pp_taxis_to_values(const PPtaxis *, PPlist *);
PPdata *pp_taxis_to_boundary_values(const PPtaxis *, PPlist *);
int pp_is_time_mean(Fint);
int pp_grid_supported(const PPhdr *);
int pp_axis_regular(const PPextravec, const PPrec *, const PPfile *);
int pp_is_rotated_grid(const PPhdr *);
Freal pp_mean_period(const PPtime *);
Freal pp_time_diff(Fint, const PPdate *, const PPdate *);
Freal pp_sec_to_day(long long);
PPcalendartype pp_calendar_type(Fint);
long long pp_gregorian_to_secs(const PPdate *);
char *pp_t_units(const PPtaxis *, PPlist *);

/* in cdunifpp_check.c: */
int pp_check_sizes();


/* in cdunifpp_compare.c: */
/* int pp_compare_ints(Fint, Fint); */
/* int pp_compare_reals(Freal, Freal); */
/* int pp_compare_ptrs(const void *, const void *); */
int pp_compare_records_between_vars(const PPrec *, const PPrec *);
int pp_compare_mean_periods(const PPrec *, const PPrec *);
int pp_both_values_in_range(Freal, Freal, Freal, Freal);
int pp_compare_records_within_var(const PPrec *, const PPrec *);
int pp_compare_records(const void *, const void *);
int pp_records_from_different_vars(const PPrec *, const PPrec *);
int pp_compare_regaxes(const void *, const void *);
int pp_compare_rotmaps(const void *, const void *);
int pp_compare_rotgrids(const void *, const void *);
int pp_compare_xsaxes( const void *, const void *);
int pp_compare_lists(const PPlist *, const PPlist *, int (*)(const void*, const void*));
int pp_compare_levels(const void *, const void *);
int pp_compare_zaxes(const void *, const void *);
int pp_compare_times(const void *, const void *);
int pp_compare_dates(const PPdate *, const PPdate *);
int pp_compare_taxes(const void *, const void *);

/* in cdunifpp_data.c: */
int pp_data_copy(const CuFile *, const CuVar *, const long [], const long [], void *);
int pp_data_read(const CuFile *, const CuVar *, const long [], const long [], void *);

/* in cdunifpp_error.c: */
int pp_switch_bug();
int pp_error(const char*);
int pp_errorhandle_init();
int pp_error_mesg(const char *, const char *);

/* in cdunifpp_filetype.c: */
int pp_determine_file_type(PPfile *, const char *, int);
int pp_determine_file_type_by_name(PPfile *, const char *);
int pp_is_ppum_file(const char *, FILE *);
int pp_determine_file_type_by_contents(PPfile *);
int pp_valid_um_word2(Fint8);
int pp_valid_pp_word1(Fint8,int);


int pp_string_ends_with(const char *, const char *);

/* in cdunifpp_linklist.c: */
void *pp_list_new(PPlist *);
int pp_list_free(PPlist *, int, PPlist *);
int pp_list_size(const PPlist *);
int pp_list_add(PPlist *, void *, PPlist *);
int pp_list_del(PPlist *, void *, PPlist *);
int pp_list_del_by_listel(PPlist *, PPlistel *, PPlist *);
int pp_list_startwalk(const PPlist *, PPlisthandle *);
void *pp_list_walk(PPlisthandle *,int);
void *pp_list_find(PPlist *, const void *, int (*)(const void *, const void *), int, int *);

typedef int(*free_func) (void *, PPlist *);

int pp_list_add_or_find(PPlist *, void *,
			int (*)(const void *, const void *), int, 
			free_func, int *, PPlist *);

/* in cdunifpp_malloc.c: */
void *pp_malloc(size_t, PPlist *);
void *pp_dup(const void *, size_t, PPlist *);
int pp_free(void *, PPlist *);
int pp_free_all(PPlist *);

/* in cdunifpp_ppcode.c: */
char *pp_pplongname(int);
char *pp_ppshortname(int);
char *pp_ppunit(int);

/* in cdunifpp_process.c: */
int pp_process(CuFile *);
int pp_test_skip_var(const PPhdr *, const PPlandmask *);
int pp_initialise_records(PPrec**, int, PPlist *);
int pp_set_disambig_index(PPgenaxis *, PPgenaxis *, PPrec **, int, int);
int pp_var_has_regular_z_t(PPgenaxis *, PPgenaxis *, PPrec **, int);
int pp_store_dim_names(PPdimnames *, const PPlist *, const CuDim *);
int pp_get_cell_methods (const PPlist *, const PPhdr *, const CuDim *, char []);
int pp_append_cell_method(char [], const char *, const char *);
int pp_var_get_extra_atts(const CuVar *, const PPfieldvar *, const CuDim *, PPlist *, PPlist *);
int pp_append_string(char *, const char *, int);
PPlist *pp_get_global_attributes(const char *, const PPfile *, PPlist *);
int pp_free_tmp_vars(PPlist *, PPlist *, PPlist *, PPlist *, PPlist *, PPlist *);

/* in cdunifpp_read.c: */
size_t pp_read_words(void *, size_t, PPconvert, const PPfile *);
int pp_swapbytes(void *, int, int);
void * pp_read_data_record(const PPrec *, const PPfile *, PPlist *);
int pp_swap32couplets(char *,int);
int pp_skip_fortran_record(const PPfile *);
int pp_skip_word(const PPfile *);
void *pp_read_header(const PPfile *, PPlist *);
int pp_read_all_headers(CuFile *);
int pp_store_header(PPhdr *, const void *);
int pp_evaluate_lengths (const PPhdr *, const PPfile *, long *, long *);
PPdata *pp_read_extradata(const PPrec *, const PPfile *, PPlist *, const PPextravec);
int pp_extra_has_vector(const PPextravec,const PPrec *, const PPfile *);

/* in cdunifpp_rotgrid.c */

PProtmap *pp_get_rotmap(const PPhdr *, PPlist *, PPlist *);

PProtgrid *pp_get_rotgrid(PPgenaxis *, PPgenaxis *, PPlist *, PPlist *);

int pp_calc_rot_grid(PProtgrid *, PPdata **, PPdata **, PPlist *);

/* in cdunifpp_stashname.c: */
char *pp_stashname(int, int, int);

/* in cdunifpp_stash2pp.c: */
int pp_stashpp(int, int, int);

/* in cdunifpp_struct.c: */
CuFile *pp_create_file(const char *);
int pp_delete_file(CuFile *);

/* in cdunifpp_unwgdoswrap.c: */
int pp_unwgdos_wrap(const void *, int, void *, long, Freal, PPlist *);

/* in cdunifpp_varinfo.c: */
int pp_var_lookup(const PPhdr *, PPstashmeta *) ;
int pp_get_var_default_shortname(const PPhdr *, char *, int);
int pp_get_var_default_longname(const PPhdr *, char *, int);
int pp_get_var_default_units(const PPhdr *, char *, int);
int pp_get_var_stash_model(const PPhdr *);
int pp_get_var_stash_section(const PPhdr *);
int pp_get_var_stash_item(const PPhdr *);
int pp_get_var_packing(const PPhdr *);
int pp_get_var_compression(const PPhdr *);
int pp_get_var_processing(const PPhdr *);
int pp_get_var_gridcode(const PPhdr *);
int pp_get_var_name(int varid, const char *, CuVar *) ;
void *pp_get_var_fill_value(const PPhdr *);
int pp_var_is_land_mask(const PPhdr *);
CuType pp_get_var_type(const PPhdr *);
int pp_var_is_time_mean(const PPhdr *);
int pp_var_is_time_min(const PPhdr *);
int pp_var_is_time_max(const PPhdr *);
int pp_var_is_time_variance(const PPhdr *);
int pp_var_is_zonal_mean(const PPhdr *);
int pp_var_is_vertical_mean(const PPhdr *);
int pp_var_is_missing(const PPhdr *hdr);

/* in cdunifpp_debug.c: */
void pp_dump_header(const PPhdr *);
void pp_dump_date(PPdate *);
void pp_dump_time(PPtime *);
void pp_dump_list(PPlist *, void (*)(void *));
void pp_dump_taxis(PPgenaxis *);



/* in cdunifpp_genaxis.c: */
/* JAK 2005-01-10 */

PPgenaxis *pp_genaxis_new(const PPaxisgentype, const PPdir, PPlist *);
int pp_genaxis_free(PPgenaxis *, PPlist *);
int pp_genaxis_compare(const void *, const void *);
PPdata *pp_genaxis_to_values(const PPgenaxis *, int, PPlist *);
PProtmap *pp_genaxis_rotmap(const PPgenaxis *);
Fint pp_genaxis_len(const PPgenaxis *);
PPdata *pp_genaxis_getCF(const PPgenaxis *, char *, char *, PPlist *, PPlist *);

PPdata *pp_xsaxis_to_values(const PPxsaxis *, PPlist *);
int pp_xsaxis_set(PPgenaxis *, const PPrec *, const PPfile *, const PPextravec, PPlist *);
int pp_axistype(const PPxsaxis *);

PPlevtype pp_zaxis_lev_type(const PPgenaxis *);
PPlevtype pp_level_type(const PPhdr *);
int pp_zaxis_set(PPgenaxis *, const PPhdr *);
int pp_zaxis_add(PPgenaxis *, const PPlevel *, int *, PPlist *);

int pp_taxis_set(PPgenaxis *, const PPhdr *);
int pp_taxis_add(PPgenaxis *, const PPtime *, int *,PPlist *);
int pp_taxis_is_time_mean(PPgenaxis *);

int pp_regaxis_set(PPgenaxis *, PPaxisregtype, const PPhdr *, PPlist *, PPlist *);
int pp_set_horizontal_axes(PPrec *, PPfile *, 
			   PPgenaxis **, PPgenaxis **, 
			   PPlist *, PPlist *);

PPgenaxis *pp_get_taxis_from_list(const PPlist *);
PPgenaxis *pp_get_zaxis_from_list(const PPlist *);
PPgenaxis *pp_get_yaxis_from_list(const PPlist *);
PPgenaxis *pp_get_xaxis_from_list(const PPlist *);

int pp_genaxis_print(const PPgenaxis *, const char *);
int pp_lev_set(PPlevel *, PPhdr *);
int pp_time_set(PPtime *, PPhdr *);

#define D(x) {printf("debug: %d\n",x);}

#ifdef DEBUG
#define DMESS(expr) printf("DBG: " #expr "\n")
#else
#define DMESS(expr)
#endif
