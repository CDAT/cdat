/* Copyright (c) 1988-1995 by Brian Doty
   See file COPYRIGHT for complete rights and liability information. */

/* Handling of missing data values.  If SETMISS is 0, it is assumed
   that missing data values will exactly match the value parsed from
   the data description file (true on many machines).  If SETMISS is 1,
   a test is done for a small range (+-value/EPSILON) and if true the
   value is set to exactly the missing data value when read.  */

#include <stdio.h>

#define SETMISS 1
#define EPSILON 1e5
#define FUZZ_SCALE 1e-5

/* RPTNUM: Number of garpt blocks to allocate per memory block
   BLKNUM: Max number of memory requests
   A max of RPTNUM*BLKNUM stations can be held per request         */

#define RPTNUM 200
#define BLKNUM 500

#if GRADS_CRAY == 1
#define CPULIMSIG SIGCPULIM
#else
#define CPULIMSIG SIGXCPU
#endif

 /*******************\
 *  Data Structures  *
 \*******************/

/* Pointer to data object */

union gadata {
  struct gagrid *pgr;
  struct gastn *stn;
};

/* Date/time structure.         */

struct dt {
  int yr;
  int mo;
  int dy;
  int hr;
  int mn;
};

/* Collection structure */

struct gaclct {
  struct gaclct *forw;       /* Forward Pointer */
  struct gastn *stn;         /* Pointer to station data */
  struct gastn *stn2;        /* Pointer to station data */
};

/* info for LATS */

struct galats {

/*=== lats_parmtab ===*/
  char ptname[256];         /* parameter file name */

/*=== lats_create ===*/
  int convention;           /* data convention (enum) */
  int frequency;            /* data frequency (enum) e.g., hourly */
  int calendar;             /* calendar of the time model (enum) e.g., standard */
  int deltat;               /* number of time units between outputs, e.g., 12 for every 12 h */
  char model[256];          /* model COMMENT */
  char center[32];          /* data source which is the GRIB PROCESS ! NOT the GRIB CENTER */
  char comment[256];        /* comment on data set which will go into the title of the .ctl file */
  char oname[256];          /* name of the output file (dset) */

/*=== lats_basetime ===*/
  int lyr;                 /* lats basetime year */
  int lmo;                 /* lats basetime month */
  int lda;                 /* lats basetime day */
  int lhr;                 /* lats basetime hour */

/*=== lats_vertdim ===*/
  char vertdimname[32];     /* name of the vertial dimension */
  double levels[128];       /* vertical levels MAX IS 128!!!*/
  int nlev;                 /* number of levels */

/*=== lats_grid ===*/
  int gridtype;             /* grid type (enum), calculated in GrADS except when LATS_GAUSSIAN */
  char gridname[32];        /* name of the vertial dimension */
  int ilinear;              /* linearity of the grid in x */
  int jlinear;              /* linearity of the grid in y */
  int nlon;                 /* # of points in x */
  int nlat;                 /* # of points in y */
  float lon_1;              /* starting longitide */
  float lat_1;              /* starting latitude */
  float lon_nlon;           /* ending longitude */
  float lat_nlat;           /* ending latitude */

/*=== lats_var ===*/
  char var[32];             /* variable name */
  char var_comment[256];    /* variable name */
  int timestat;             /* variable time statistic (enum) */

/*=== lats_write ===*/
  double varlev;            /* level value of the variable  for lats_write*/

/*--- time options ---*/
  int time_opt;             /* 0 - grid relative ; 1 - dimension environment relative ; 2 - set time using baset time ; 3 - forecast hourly */
  int fhour;                /* forecast hour if using lats forecast_hourly */

/*--- internal id's to pass to the lats routines ---*/
  int id_file;
  int id_lev;
  int id_grid;
  int id_var;

  int id_user_file;
  int id_user_var;
  int id_user_grid;
  int id_user_lev;
  int id_user_write;

};


/*mf 9612105 Contains global information for Mike Fiorino mf*/

struct gamfcmn {
  int cal365 ;               /* 365 (no leap year) calendar */
  int fullyear ;             /* 1 - must specify full year 0 old default */
  int warnflg;               /* warning level flag for messages */
};

/* Contains information about the user interface and graphics output. */
struct gacmn {

  int pass;                  /* Number of passes since last clear     */
  int gpass[10];             /* Number of passes for each gx type     */
  int loopdim;               /* Looping dimension                     */
  int loopflg;               /* Looping on or off                     */
  float loopincr;            /* Looping increment                     */

  struct gafile *pfi1;       /* Pointer to first gafile in chain      */
  struct gafile *pfid;       /* Pointer to default gafile             */
  int fnum;                  /* File count                            */
  int dfnum;                 /* Default file number   */

  struct gadefn *pdf1;       /* Pointer to first define block         */

  float dmin[3],dmax[3];     /* Current absolute coordinate limits    */
  struct dt tmin,tmax;
  int vdim[4];               /* Which dimensions vary?                */

  float pxsize,pysize;       /* Physical page size in inches          */
  int orient;                /* Page orientation                      */
  int vpflag;                /* If 1, virtual page being used         */
  float xsiz,xsiz1,xsiz2;    /* Physical plotting size in X direction */
  float ysiz,ysiz1,ysiz2;    /* Physical plotting size in Y direction */
  int paflg;                 /* User has specified plotting area      */
  float pxmin,pxmax;         /* User specified plotting area          */
  float pymin,pymax;
  float aspect;              /* User specified aspect ratio           */

  int clab;                  /* control contour labels.               */
  int clskip;                /* Contour label skipping       */
  char *clstr;               /* Contour label template */
  float rainmn,rainmx;       /* control rainbow colors                */
  int rbflg;                 /* User rainbow colors specified         */
  int rbcols[100];           /* User rainbow colors                   */

  float cmin,cmax,cint;      /* User specified contour limits         */
  int cflag;                 /* If true, user specifies contour levels*/
  float clevs[100];          /* User specified contour levels         */
  int ccflg;                 /* If true, user specifies contour colors*/
  int ccols[100];            /* User specified contour colors         */
  int shdcls[120];           /* Shade colors after shading            */
  float shdlvs[120];         /* Shade levels                          */
  int shdcnt;                /* Number of shdlvs, shdcls              */
  int ccolor,cstyle;         /* User contour/line appearance          */
  int cthick;                /* User gx display line thickness        */
  int cmark;                 /* Line marker type                      */
  int csmth;                 /* Contour smoothing on or off           */
  int cterp;                 /* Spline fit on or off                  */
  float rmin,rmax,rint;      /* Axis limits for 1-D plots             */
  float rmin2,rmax2,rint2;   /* Axis limits for 1-D plots             */
  int aflag,aflag2;          /* Keep 1D axis limits fixed             */
  int grflag,grstyl,grcolr;  /* Grid flag, linestyle, color           */
  int dignum;                /* grid value plot control (gxout=grid)  */
  float digsiz;
  int arrflg;                /* Use already set arrow scaling         */
  float arrsiz;              /* Arrow size in inches                  */
  int arlflg;                /* Arrow label flag */
  float arrmag;              /* Vector magnitude producing arrsiz arrw*/
  float ahdsiz;              /* Arrow head size.       */
  int miconn;                /* Connect line graph accross missing    */
  int strmden;               /* Streamline density indicator  */
  int mdlblnk,mdldig3;       /* Station model plot opts */

  int fgvals[50];            /* Values for grid fill */
  int fgcols[50];
  int fgcnt;

  int stidflg;               /* Plot station ids with values      */

  float axmin,axmax,axint;   /* Overrides for X-axis labels           */
  float aymin,aymax,ayint;   /* Overrides for Y-axis labels           */
  int axflg, ayflg;          /* Is override in effect for the axis?   */
  int frame;                 /* Display frame?  */

  int rotate;                /* Rotate plot from default orientation  */
  int xflip, yflip;          /* Flip X or Y axes                      */
  int zlog;                  /* Z coordinate in log scale */

  int mproj;                 /* Map projection -- used for X,Y plot   */
                             /*  only.  0 = no map.                   */
  int mpdraw;                /* Draw map outline - 0=no               */
  float mpvals[10];          /* Map projection option values.         */
  int mpflg;                 /* Map projection option values are set. */
  char *mpdset[8];           /* Map data set names.                   */
  int mpcols[256];           /* Map Color array                       */
  int mpstls[256];           /* Map line styles array                 */
  int mpthks[256];           /* Map line widths array                 */
  int mapcol,mapstl,mapthk;  /* Default map color, style, thickness   */

  int gout0;                 /* Graphics output type for stat.        */
  int gout1;                 /* Graphics output type for 1-D.         */
  int gout2a;                /* Graphics output type for 2-D.         */
  int gout2b;                /* Graphics output type for 2-D.         */
  int goutstn;               /* Graphics output type for stns */

  int blkflg;                /* Leave certain values black when shadng*/
  float blkmin, blkmax;      /* Black range */

  int reccol,recthk;         /* Draw Rectangle color, brdr thickness  */
  int lincol,linstl,linthk;  /* Draw line color, style, thickness     */
  int mcolor;                /* auto color (orange or grey)           */
  int strcol,strthk,strjst;  /* Draw string color, thckns, justifictn */
  float strrot;              /* Draw string rotation */
  float strhsz,strvsz;       /* Draw string hor. size, vert. size     */
  int anncol,annthk;         /* Draw title color, thickness           */
  int grflg;                 /* Grey Scale flag   */
  int devbck;                /* Device background */
  int xlcol,xlthck,ylcol,ylthck,clcol,clthck;  /* color, thickness */
  int xlside,ylside,ylpflg;
  float xlsiz,ylsiz,clsiz,xlpos,ylpos,yllow;         /* Axis lable size */
  float xlevs[50],ylevs[50]; /* User specified x/y axis labels  */
  int xlflg,ylflg;           /* Number of user specified labels */
  int xtick,ytick;           /* Number of extra tick marks      */
  float xlint,ylint;         /* User specified label increment */
  char *xlstr, *ylstr;       /* user substitution string for labels */
  int xlab,ylab;             /* Axis label options */
  char *xlabs, *ylabs;       /* User specifies all labels */
  int ixlabs, iylabs;        /* Count of user labels */
  int lfc1,lfc2;             /* Linefill colors */
  int wxcols[5];             /* wx symbol colors */
  int wxopt;                 /* wx options */
  int tser;                  /* station time series type */

  int bargap;                /* Bar Gap in percent  */
  int barolin;               /* Bar outline flag */
  float barbase;             /* Bar Base Value      */
  int barflg;                /* Bar flag: 1, use base value  */
                             /*           0, draw from plot base */
                             /*          -1, draw from plot top  */

  int btnfc,btnbc,btnoc,btnoc2;     /* Current button attributes */
  int btnftc,btnbtc,btnotc,btnotc2;
  int btnthk;

  int drvals[15];            /* Attributes for drop menus */

  FILE *ffile;               /* grads.fwrite file handle */
  char *fwname;              /* fwrite file name */

  int grdsflg;               /* Indicate whether to put grads atrib.  */

  int timelabflg;            /* Indicate whether to put cur time atrib.  */

  int dbflg;                 /* Double buffer mode flag     */

  int batflg;                /* Batch mode */

  int numgrd,relnum;         /* Number of data objects held           */
  int type[16];              /* Data type of each data object         */
  union gadata result[16];   /* Pointers to held data objects         */
  struct gaclct *clct[32];   /* Anchor for collection */
  int clctnm[32];            /* Number of items collected */
  int clcttp[32];            /* Varying dimension of collection */

  int lastgx;                /* Last gx plotted */
  int xdim, ydim;            /* Which dimensions on X and Y axis */
                             /* Grid-to-world conversion info follows */

  float (*xgr2ab) (float *, float);
  float (*ygr2ab) (float *, float);
  float (*xab2gr) (float *, float);
  float (*yab2gr) (float *, float);
  float *xgrval;
  float *ygrval;
  float *xabval;
  float *yabval;

  int impflg;                /* Implied run flag */
  char *impnam;              /* Implided run script name */

  int impcmd;                /* Implicit run */

  int sig;                   /* User has signalled */

  int ptflg;		     /* Pattern fill flag */
  int ptopt;		     /* Pattern option: */
			     /*		0, open  */
			     /*		1, solid */
			     /*		2, dot */
			     /*		3, line  */
  int ptden;		     /* Dot or line pattern density */
  int ptang;		     /* Line pattern angle */

  struct galats glats ;      /* LATS option struct */

};



/* GA status structure.  Contains necessary info about the scaling
   and file structure in force.                                       */

struct gastat {

  struct gafile *pfi1;       /* Pointer to first gafile in chain      */

  struct gafile *pfid;       /* Pointer to default gafile             */

  int fnum;                  /* Default file number                   */

  struct gadefn *pdf1;       /* Pointer to first define block         */

  struct gaclct **pclct;     /* Pointer to the collection pointers    */

  union gadata result;       /* Result goes here                      */

  int type;                  /* Result type (grid or stn)             */

  int idim,jdim;             /* Varying dimensions                    */

  float dmin[3],dmax[3];     /* Range of absolute dimensions          */
  struct dt tmin,tmax;

};

/* Description of a data file.                                        */

/* This is to define the pointer added at the end of the gafile structure */
/* -hoop, 95/07/10 and 95/11/10 */
#if USESDF == 1
#include "gasdf.h"
#endif

struct gafile {

  struct gafile *pforw;      /* Forward pointer to next gafile block.
                                List is anchored within gastat.       */

  char name[256];            /* File name.                            */

  char *tempname;            /* File name of open file (different with templates) */

  char dnam[256];            /* Descriptor file name                  */

  char *mnam;                /* Map(index) file name */

  FILE *infile;              /* File pointer.                         */

  int type;                  /* Type of file:  1 = grid
                                               2 = simple station
                                               3 = mapped station
                                               4 = defined grid       */

  char title[80];            /* Title -- describes the file.          */
  float undef;               /* Global undefined value for this file  */
  float ulow,uhi;            /* Undefined limits for SETMISS test     */

  float *rbuf;               /* Buffer for file I/O equal in length
                                to one grid row in the file, or equal
                                in length to the size needed to hold
                                the largest station report            */
  char *pbuf;                /* Same as rbuf, for unpacking           */
  char *bbuf;                /* Same as rbuf, for bit map I/O         */

  int bswap;                 /* Byte swapping needed */

  int mtype;                 /* Stn map file type                     */
  int *tstrt;               /* Pointer to list length dnum[3] of
                                start points of times in the file     */
  int *tcnt;                /* Count of stns for assctd time         */
  int stcnt;                /* Count of mapped stids when stn data
                                and map file is type stidmap.         */
  int stpos;                /* Position in map file of start of
                                stid info for map file type stidmap.  */
  FILE *mfile;               /* File pointer to stidmap file          */

  int dnum[4];               /* Dimension sizes for this file.        */

  int tlpflg;                /* Circular file flag                    */

  int tlpst;                 /* Start time offset in circular file    */

  int vnum;                  /* Number of variables.                  */

  int ivnum;                 /* Number of level independent variables
                                for station data file                 */

  int lvnum;                 /* Number of level dependent variables
                                for station data file                 */

  struct gavar *pvar1;       /* Pointer to an array of structures.
                                Each structure in the array has info
                                about the specific variable.          */

  int gsiz;                 /* Number of elements in a grid (x*y)    */
                            /* This is for actual grid on disk,
                               not psuedo grid (when pp in force) */

  int tsiz;                 /* Number of elements in an entire time
                                group (all variables at all levels
                                for one time).                        */
  int trecs;                /* Number of records (XY grids) per time
                                group.                                */
  int  fhdr;                 /* Number of bytes to ignore at file head*/

  int wrap;                  /* The grid globally 'wraps' in X        */

  int seqflg, yrflg, zrflg;  /* Format flags */

  int ppflag;                /* Pre-projection type.
                                0 = none
                                1 = polar stereo
                                2 = lambert conformal    */

  int ppwrot;                /* Pre-projection wind rotation flag */

  int ppisiz, ppjsiz;        /* Actual size of preprojected grid */

  float ppvals[20];          /* Projection constants for pre-projected
                                grids.  Values depend on projection. */

  int *ppi[8];               /* Pointers to offsets for pre-projected
                                grid interpolation */

  float *ppf[8];             /* Pointers to interpolation constants
                                for pre-projected grids */

  float *ppw;                /* Pointer to wind rotation array */

  float (*gr2ab[3]) (float *, float);
                             /* Addresses of routines to do conversion
                                from grid coordinates to absolute
                                coordinates for X, Y, Z.  All Date/time
                                conversions handled by gr2t.          */

  float (*ab2gr[3]) (float *, float);
                             /* Addresses of routines to do conversion
                                from absolute coordinates to grid
                                coordinates for X,Y,Z.  All date/time
                                conversions handled by t2gr.          */

  float *grvals[4];          /* Pointers to conversion information for
                                grid-to-absolute conversion routines. */

  float *abvals[4];          /* Pointers to conversion information for
                                absolute-to-grid conversion routines. */

  int linear[4];             /* Indicates if a dimension has a linear
                                grid/absolute coord transformation
                                (Time coordinate always linear).      */

  int dimoff[4];             /* Dimension offsets for defined grids   */
  int climo;                 /* Climatological Flag (defined grids)   */
  int cysiz;                 /* Cycle size for climo grids            */

  int idxflg;                /* File records are indexed */
  int grbgrd;                /* GRIB Grid type */
  struct gaindx *pindx;      /* Index Strucure if indexed file */

  int tmplat;                /* File name template */
  int *fnums;                /* File number for each time */
  int fnumc;                 /* Current file number that is open */

  int errcnt;                /* Current error count */
  int errflg;                /* Current error flag */

/*mf  mf*/
  int  xyhdr;                /* Number of bytes to ignore at head of xy grids*/
  int  thdr;                 /* Number of bytes to ignore at head of time chunk*/
  int calendar;              /* Support for 365-day calendars */
  int cray_ieee;             /* Support 32-bit IEEE data on cray */
/*mf  mf*/


/* Use of this with the ifdef has implications for checkpoint/restart, */
/* as it changes the size of the gafile structure.  -hoop 95/07/10 */
#if USESDF == 1
  IO_STD *sdf_ptr ;		/* Pointer to SDF file structure */
#endif

};


/* Structure that describes a grid (requestor or descriptor block).  */

struct gagrid {

  float *grid;               /* Address of the grid.                 */

  struct gafile *pfile;      /* Address of the associated gafile
                                structure to get the data from
                                (requestor block only)               */

  float undef;               /* Undefined value for this grid.       */

  float rmin,rmax;           /* Minimum/Maximum grid value
                                (rmin is set to the grid value when
                                isiz=jsiz=1.  *grid points to here.) */

  int isiz,jsiz;             /* isiz = number of elements per row.
                                jsiz = number of rows.               */

  int idim,jdim;             /* Dimension of rows and columns.
                                 -1 = This dimension does not vary
                                  1 = X dimension (usually longitude)
                                  2 = Y dimension (usually lattitude)
                                  3 = Z dimension (usually pressure)
                                  4 = Time

                                If both dimensions are -1, then the
                                grid has one value, which will be
                                placed in rmin.                      */

  int iwrld, jwrld;          /* World coordinates valid?   */

  int dimmin[4],dimmax[4];   /* Dimension limits for each dimension
                                (X,Y,Z,T) in grid units.             */

  struct gavar *pvar;        /* Pointer to the structure with info
                                on this particular variable.  If
                                NULL, this grid is the result of
                                an expression evaluation where the
                                variable type is unkown.             */

  char *exprsn;              /* If grid is a 'final' result, this
                                will point to a character string that
                                contains the original expression.    */

  int alocf;                 /* Scaling info allocated for us only */

  float (*igrab) (float *, float);
  float (*jgrab) (float *, float);
                             /* Addresses of routines to perform
                                grid-to-absolute coordinate
                                transforms for this grid's i and j
                                dimensions (unless i or j = 3).      */

  float (*iabgr) (float *, float);
  float (*jabgr) (float *, float);
                             /* Absolute to grid conversion routines */

  float *ivals, *jvals;      /* Pointers to conversion info for the
                                grid to abs conversion routines      */
  float *iavals, *javals;    /* Conversion info for abs to grid      */

  int ilinr,jlinr;           /* Indicates if linear transformation   */


};

/* Structure that describes a report header in a stn file */

struct rpthdr {
  char id[8];                     /* Character station id           */
  float lat;                      /* Latitude of report             */
  float lon;                      /* Longitude of report            */
  float t;                        /* Time in relative grid units    */
  int  nlev;                      /* Number of levels following     */
  int flag;                       /* Level independent var set flag */
};

/* Structure that describes a stid info block within a stidmap file */

struct stninf {
  char stid[8];
  int offset;
  int rcnt;
};

/* Structure that describes a single report                          */

struct garpt {

  struct garpt *rpt;         /* Address of next report               */

  char stid[8];              /* Station id                           */

  float lat,lon,lev,tim;     /* Location of station                  */

  int work;                  /* Work area                            */

  float val;                 /* Value of variable                    */

};

/* Structure that describes a collection of station reports.         */

struct gastn {

  struct garpt *rpt;         /* Address of start of link list        */

  int rnum;                  /* Number of reports.                   */

  struct garpt *blks[BLKNUM];    /* ptrs to memory holding rpts      */

  struct gafile *pfi;        /* Address of the associated gafile
                                structure to get the data from
                                (requestor block only)               */

  float undef;               /* Undefined value for this data.       */

  int idim,jdim;             /* Varying dimensions for this data
                                 -1 = This dimension does not vary
                                  1 = X dimension (longitude)
                                  2 = Y dimension (lattitude)
                                  3 = Z dimension (pressure)
                                  4 = Time                           */

  float dmin[3],dmax[3];     /* Dimension limits for each dimension
                                 (X,Y,Z) in world coords.
                                 Non-varying dimensions can have
                                 limits in this structure.           */

  int rflag;                 /* Get stations within specified radius in
                                degrees of fixed lat and lon         */
  float radius;              /* Radius */

  int sflag;                 /* Get specific station  */
  char stid[8];              /* Station id to get */

  int tmin,tmax;             /* Grid limits of time */

  float *tvals;              /* Pointer to conversion info for the
                                time conversion routines.            */

  struct gavar *pvar;        /* Pointer to the structure with info
                                on this particular variable.  If
                                NULL, this grid is the result of
                                an expression evaluation where the
                                variable type is unkown.             */

  struct garpt **prev;       /* Used for allocating rpt structures   */
  struct garpt *crpt;
  int rptcnt,blkcnt;

};

/* Structure that describes a variable in a file.  These structures
   are built in arrays that are hung off of gafile structures.       */


struct gavar {

  char varnm[128];            /* Variable name.                       */

  char abbrv[16];            /* Variable abbreviation.               */

  int units[4];              /* Units indicator.                     */

  int offset;               /* Offset in grid elements of the start
                                of this variable within a time group
                                within this file.                    */

  int recoff;                /* Record (XY grid) offset of the start
                                of this variable within a time group */

  int levels;                /* Number of levels for this variable.
                                0 is special and indiates one grid is
                                available for the surface only.      */
/*mf

  new variable attribute to handles:

  dfrm:

  1-byte
  int dat

  var_t: variable t transform
  var_z: variable z transform
  y_x  : x-y transform

mf*/

  int dfrm;                  /* format  type indicator
				1 - unsigned char
				4 - int
			     */

  int var_t ;
  int var_z ;
  int y_x ;

};

/* Structure that describes a function call.                         */

struct gafunc {

  int argnum;                /* Number of arguments found by fncprs  */

  char *argpnt[20];          /* Pointers to the argument strings     */

  char buff[1000];           /* Argument string buffer               */

};

/* Structure that describes a user defined function */

struct gaufb {
  struct gaufb *ufb;  /* Forward pointer */
  char name[8];       /* Function name   */
  int alo,ahi;        /* Limits on number of args */
  int atype[8];       /* Types of args. 0=expr,1=float,2=int,3=char */
  int sflg;           /* Sequential or direct */
  char *fname;        /* Name of user executable */
  char *oname;        /* File name for data transfer to user */
  char *iname;        /* File name for data transfer from user */
};

/* Structure that describes a defined grid */

struct gadefn {

  struct gadefn *pforw;      /* Linked list pointer                  */

  struct gafile *pfi;        /* File Structure containing the data   */

  char abbrv[20];            /* Abbreviation assigned to this        */

};

/* Stack to evaluate the expression.  The stack consists of an
   array of structures.                                               */

struct smem {
  int type;        /* Entry type: -2 stn,-1 grid,1=op,2='(',3=')'    */
  union sobj {
    int op;        /* Operator: 0=*, 1=/, 2=+                        */
    struct gagrid *pgr; /* Operand (grid or stn)                      */
    struct gastn *stn;
  } obj;
};

/* Index structure, for when the records in a data file are indexed.
   The indexing file will contain this structure at the front, followed
   by the specified number of header and indexing values.  These
   header and indexing values are file format specific. */

struct gaindx {
  int type;                 /* Indexing file type */
  int hinum;                /* Number of ints in header */
  int hfnum;                /* Number of floats in header */
  int intnum;               /* Number of index ints (long) */
  int fltnum;               /* Number of index floats */
  int *hipnt;               /* Pointer to header int values */
  float *hfpnt;              /* Pointer to header float values */
  int *intpnt;              /* Pointer to int index values */
  float *fltpnt;             /* Pointer to float index values */
} ;


 /***********************\
 *  Function Prototypes  *
 \***********************/

/* Functions in GRADS.C */

void gasig (int);
int gaqsig (void);

/* Functions in GAUSER:
    gacmd:  Process a user command
    gacln:  Reset variables
    gaenab: Process an enable command
    gadraw: Process a draw command
    gadef:  Process a define command
    gaudef: Process undefine command
    gamodf: Modify defined grid
    gaqury: Process a query command
    gahelp: Process a help command
    gaset:  Process the SET command
    gadspl: Process a display command
    gapars: Parse a compound expression for gadspl
    gagrel: Release all held grids
    gaopen: Open a data file from a descriptor file
    cleanup: Clean up an input record
    gaprnt: Process output messages
    gagsdo: Execute command for a script
    getpst: Allocate and initialize a gastat block              */

void gainit (void);

int gacmd (char *, struct gacmn *, int);
void gacln (struct gacmn *,int);
int gaenab (char *, struct gacmn *);
int gadraw (char *, struct gacmn *);
int gardrw (char *, struct gacmn *);
int gaexec (char *, struct gacmn *);
char *gagsdo (char *, int *);
int gadef (char *, struct gacmn *, int);
int gaudef (char *, struct gacmn *);
int gamodf (char *, struct gacmn *);
int gaqdef (char *, struct gacmn *, int);
int gaqury (char *, char *, struct gacmn *);
int gahelp (char *, struct gacmn *);
int gaset (char *, char *, struct gacmn *);
int gacoll (char *, struct gacmn *);
int gadspl (char *, struct gacmn *);
int gaspcl (char *, struct gacmn *);
int gapars (char *, struct gastat *, struct gacmn *) ;
void gagrel (struct gacmn *);
int gaopen (char *, struct gacmn *);
void cleanup (char *);
struct gastat *getpst (struct gacmn *);
void gaprnt (int, char *);

int gaddes (char *, struct gafile *, int);
int deflin (char *, struct gafile *, int, int);
int deflev (char *, char *, struct gafile *, int);
struct gafile *getpfi (void);

/* Functions in GAEXPR:
    gaexpr: Evaluate an expression by creating a stack
    eval:   Process the expression stack
    gaoper: Perforam operation between two operands
    gagrop: Perform a grid operation
    gastop: Perform a station operation
    gascop: Perform an op between a constant and stations
    gagrvl: Put a constant in grid format
    varprs: Parse a variable in the expression
    getdfn: Search defined grid chain for a predifined grid
    gagchk: Check validity of operation between two grids
    stnvar: Handle specialized parsing of stn variable                */

int gaexpr (char *, struct gastat *) ;
int eval (int, struct smem *, int *);
int gaoper (struct smem *, int, int, int, int);
struct gagrid *gagrop (struct gagrid *, struct gagrid *, int, int);
struct gastn *gastop (struct gastn *, struct gastn *, int, int);
struct gastn *gascop (struct gastn *, float, int, int);
struct gagrid *gagrvl (float);
char *varprs (char *, struct gastat *) ;
int gagchk (struct gagrid *, struct gagrid *, int);
struct gafile *getdfn (char *, struct gastat *);
char *stnvar (char *, char *, struct gafile *, struct gavar *,
              struct gastat *);

/* Functions in GAFUNC:
    rtnprs: Parse and execute a function call
    gafopr: Perform opration for two-op function call
    gafdef: Read user function definition table
                                                                     */

char *rtnprs (char *, char *, struct gastat *) ;
int gafopr (struct gastat *, struct gastat *, int );
void gafdef (void);

/* Functions in GAIO:
    gaggrd: Get a grid from a data file
    gagrow: Get a row of data, possibly wrapped, from the file
    gafcyx: Calculate file position of an item of data with x-y transposed
    gafcor: Calculate file position of an item of data
    garrow: Get an item or items from the data file
    gagstn: Get a collection of stations
    garead:
    gardyx: garead for x-y transposed
    gaglvs: Get appropriate var and levs from rpt
    gaarpt: Allocate report block
    gasstn: Seek to location in station file
    garstn: Read station data
    gacstn: Cache a station report
    gagdef: Get grid from defined variable
    clicyc: Adjust for cyclic climatological defined variable
    gagpre: Predefined variable access         */

int gaggrd (struct gagrid *);
int gagrow (float *, int *);
int gafcor (int, int, int, int);
int gafcyx (int, int, int, int);
int garrow (int, int, int, int, int, float *);
int garead (int, int, float *);
int gardyx (int, int, float *);
int gagstn (struct gastn *);
int gaglvs (int, struct rpthdr *, struct gastn *);
struct garpt *gaarpt (struct gastn *);
int gasstn (int);
int garstn (int, char *, int);
void gacstn (char *, char *, int, int);
void gagcst (int, char *);
int gagdef (void);
void clicyc(int *);
int gagpre (void);
int gairow (int, int, int, int, int, int, float *);
int gaird (int, int, int, int, int, int);
int gaprow (int, int, int, int, int, int, float *);
int gaopfn (int,int *);
int gappcn (struct gafile *, int, int);
void w3fb04 (float, float, float, float, float *, float *);
void ll2lc (float *, float, float, float *, float *);
void gaiomg (void);

/* Functions in GAGX:
    gagx:   Initialize graphics interface
    gaplot: Generalized graphics output routine
    gas1d:  Set up scaling for a 1-D grid
    gas2d:  Set up scaling for a 2-D grid
    gagrph: Plot line graph
    gacntr: Plot contour plot
    gastrm: Plot streamline plot
    gafgrd: Fill grid boxes
    gashad: Plot shaded plot
    gavect: Plot vector feild
    gascat: Plot scatter plot from two grids
    gaarrw: Plot individual arrow
    gaplvl: Plot grid values
    gawmap: Plot appropriate map
    gacsel: Select a contour interval
    gaaxis: Generate proper axis labels for axes
    galnch: Convert a longitude value to character form
    galtch: Convert a latitude value to character form
    gaconv: Perform grid level scaling
    gatinc: Get date/time increment for time axis
    gasfil: Shade fill a grid sqaure
    trfill: Fill half a grid square (triangle)
    gagfil: Fill grids with shaded ranges
                                                                      */
void gagx (struct gacmn *);
void gaplot (struct gacmn *);
void gas1d (struct gacmn *, float, float, int, int, struct gagrid *,
             struct gastn *);
void gas2d (struct gacmn *, struct gagrid *, int);
void gagrph (struct gacmn *,int);
void gastts (struct gacmn *);
void galfil (struct gacmn *);
void lfint (float, float, float, float, float *, float *);
void lfout (float *, float *, float *, float *, int, int);
void gacntr (struct gacmn *, int);
void gastrm (struct gacmn *);
void gashad (struct gacmn *);
void gavect (struct gacmn *, int);
void gascat (struct gacmn *);
void gafgrd (struct gacmn *);
void gafwrt (struct gacmn *);
int  galats (struct gacmn *, int, int); /*mf --- GrADS-lats interface mf*/
void gaarrw (float, float, float, float, float);
void gaplvl (struct gacmn *);
void gamscl (struct gacmn *);
void gawmap (struct gacmn *, int);
void gacsel (float, float, float *, float *, float *);
void gaaxis (int, struct gacmn *, int);
int galnch (float, char *);
int galtch (float, char *);
void gaconv (float, float, float *, float *);
void gagexp (float *, int, int, float *, int, int, float);
void gaglin (float *, int, int, float *, int, int, float);
struct gagrid *gaflip (struct gagrid *, struct gacmn *);
int gatinc (struct gacmn *, struct dt *, struct dt *);
void gasfil (int, int, float, float, float, float, float, float, float, float);
void trfill (float, float, float, float, float, float, float, float,
     float, int, int);
void gafstn (struct gacmn *);
void gapstn (struct gacmn *);
void gawsym (struct gacmn *);
void gabarb (float, float, float, float, float, float, float, int);
void gapmdl (struct gacmn *);
void gasmdl (struct gacmn *, struct garpt *, float *, float *);
float wndexit (float, float, float, float, float, float *,
                                   float *, float *, float *);
void gapprf (struct gacmn *);
void gatser (struct gacmn *);
void gampax (struct gacmn *);
void wxsym (int, float, float, float, int, int *);
void wxprim (int, float, float, float);
void gagsav (int, struct gacmn *, struct gagrid *);
void galnx (float, float, float *, float *);
void galny (float, float, float *, float *);
void gaalnx (float, float, float *, float *);
void gaalny (float, float, float *, float *);
void gagfil ( float *, int, int, float *, int *, int, float);
void gafram (struct gacmn *);
void gaaxpl (struct gacmn *, int, int);
void gaselc (struct gacmn *, float, float);
int gashdc (struct gacmn *, float);

/* Functions in GAUTIL:
    nxtcmd: Get next command from the user
    timadd: Add an increment to a time
    timsub: Subtract an increment from a time
    t2gr:   Convert an absolute time to a grid value
    gr2t:   Convert a grid value to an absolute time
    timdif: Calculate difference between two times
    qleap:  Determine if a year is a leap year
    adtprs: Parse an absolute date/time expression
    rdtprs: Parse a relative date/time expression
    gaedit: Convert from float to character
    cmpwrd: Compare two character strings
    nxtwrd: Point to the next blank delimited word in a string
    liconv: Linear conversion routine
    gr2lev: Discrete level scaling routine
    lev2gr: Discrete level scaling routine
    intprs: Parse an integer expression
    valprs: Parse a floating number expression
    dimprs: Parse a dimension expression
    lowcas: Convert a string to lower case
    uppcas: Convert a string to upper case
    getstr: Move part of a string to another string
    getwrd: Get next word in a string as a string
    gamnmx: Get minimum and maximum grid value
    garemb: Remove blanks from and terminate a string
    gagaus: Set up scaling for gaussian grid (R40)
    gags30: Set up scaling for gaussian grid (R30)
    gags20: Set up scaling for gaussian grid (R20)
    gags15: Set up scaling for Ocean Grid (MOM32)
    gamo32: Set up scaling for gaussian grid (R15)
    gat2ch: Date/Time to character form
    cmpch:  Compare two strings of specified length
    gafree: Free stuff hung off pst block
    gagfre: Free gagrid block and associated data
    gasfre: Free gastn block and associated data
    gagbb:  Unpack bit value
    gagby:  Unpack byte value
    gabswp: Byte swap data values
    gahswp: Byte swap report header
    flt2ibm: convert float to ibm float (ebisuzaki)
    ibm2flt: convert ibm float to float (ebisuzaki)
                                                                      */

int nxtcmd (char *, char *);
void timadd (struct dt *, struct dt *);
void timsub (struct dt *, struct dt *);
float t2gr (float *, struct dt *);
void gr2t (float *, float, struct dt *);
int timdif (struct dt *, struct dt *);
int qleap (int);
char *adtprs (char *, struct dt *, struct dt *);
char *rdtprs (char *, struct dt *);
int gaedit (float, char *, int);
int cmpwrd (char *, char *);
char *nxtwrd (char *);
float liconv (float *, float);
float gr2lev (float *, float);
float lev2gr (float *, float);
char *intprs (char *, int *);
char *valprs (char *, float *);
char *dimprs (char *, struct gastat *, struct gafile *,
              int *, float *, int, int *);
void lowcas (char *);
void uppcas (char *);
void getstr (char *, char *, int);
void getwrd (char *, char *, int);
void gamnmx (struct gagrid *);
int garemb (char *);
float *gagaus(int,int);
float *gags30(int,int);
float *gags20(int,int);
float *gags15(int,int);
float *gamo32(int,int);
int gat2ch (struct dt *, int, char *);
int cmpch (char *, char *, int);
void gafree (struct gastat *);
void gagfre (struct gagrid *);
void gasfre (struct gastn *);
void fnmexp (char *, char *, char *);
int gagbb (char *, int, int);
int gagby (char *, int, int);
char *gafndt (char *, struct dt *, struct dt *, float *);
void gabswp (float *, int);
void gahswp (struct rpthdr *);
int dayweek (struct dt *);

/*mf -- Wesley Ebisuzaki routines -- mf*/

int flt2ibm(float x, unsigned char *ibm);
float ibm2flt(unsigned char *ibm);
float ieee2flt(unsigned char *);
int flt2ieee(float , unsigned char *);
int be_int2int(unsigned char *) ;



/* Functions in GASRCP:
    gsfile: run a script file used in gauser.c
*/

char *gsfile (char *, int *, int);
