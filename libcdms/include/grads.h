/*  Copyright (C) 1988-2010 by Brian Doty and the 
    Institute of Global Environment and Society (IGES).  
    See file COPYRIGHT for more information.   */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "gabufr.h"
/* #include <gatypes.h>  JMA why is this causing problems? */
#if GRIB2==1
#include "grib2.h"
#endif
#if USESHP==1
#include "shapefil.h"
#endif
#if USEHDF5==1
#include <hdf5.h>
#endif

/* Handling of missing data values. After the data I/O is done, 
   grid values are tested to see if they are within a small range 
   (+-value/EPSILON) of the missing value. If true, then the undef 
   mask is set to 0. If false, then the grid data values are good, 
   and the undef mask is set to 1. Everywhere else in the code, 
   undef tests are done on the mask values, not the data. */

#define EPSILON 1e5
#define FUZZ_SCALE 1e-5

/* RPTNUM: Number of garpt blocks to allocate per memory block
   BLKNUM: Max number of memory requests
   A max of RPTNUM*BLKNUM stations can be held per request
   Static memory usage is sizeof(pointer) * BLKNUM bytes */

#define RPTNUM 200
#define BLKNUM 5000

#ifdef __hpux
#define CPULIMSIG _SIGXCPU
#else
#define CPULIMSIG SIGXCPU
#endif

 /*******************\
 *  Data Structures  *
 \*******************/

/* Pointer to data object */
union gadata {
  struct gagrid *pgr;
  struct gastn  *stn;
};

/* Date/time structure */
struct dt {
  gaint yr;
  gaint mo;
  gaint dy;
  gaint hr;
  gaint mn;
};

/* Collection structure */
struct gaclct {
  struct gaclct *forw;        /* Forward Pointer */
  struct gastn  *stn;         /* Pointer to station data */
  struct gastn  *stn2;        /* Pointer to station data */
};

/* Structure for info for the gxout "writegds" option, 
   for writing data for the use of the GDS */
struct gawgds {
  char *fname;                /* File name to write */
  char *opts;                 /* User specified options */
};


/*mf 9612105 Contains global information for Mike Fiorino and Gary Love 980114 mf*/

struct gamfcmn {
  gaint cal365 ;               /* 365 (no leap year) calendar */
  gaint fullyear ;             /* 1 - must specify full year 0 old default */
  gaint warnflg;               /* warning level flag for messages */
  gaint winid;                 /* Window ID */
  gaint winx;                  /* Window X position (upper left) */
  gaint winy;                  /* Window Y position (upper left) */
  gauint winw;                 /* Window width */
  gauint winh;                 /* Window height */
  gauint winb;                 /* Window border width */

};

/* Contains information about the user interface and graphics output. */
struct gacmn {
 
  gadouble dmin[5],dmax[5];    /* Current absolute coordinate limits    */
                               /* Grid-to-world conversion info follows */
  gadouble (*xgr2ab) (gadouble *, gadouble);
  gadouble (*ygr2ab) (gadouble *, gadouble);
  gadouble (*xab2gr) (gadouble *, gadouble);
  gadouble (*yab2gr) (gadouble *, gadouble);
  gadouble *xgrval;
  gadouble *ygrval;
  gadouble *xabval;
  gadouble *yabval;
  struct gawgds *wgds;         /* Pointer to gds output structure       */
  gaint hbufsz;                /* Metafile buffer size                  */
  gaint g2bufsz;               /* Grib2 cache buffer size               */
  gaint pass;                  /* Number of passes since last clear     */
  gaint gpass[10];             /* Number of passes for each gx type     */
  gaint loopdim;               /* Looping dimension                     */
  gaint loopflg;               /* Looping on or off                     */
  struct gafile *pfi1;         /* Pointer to first gafile in chain      */
  struct gafile *pfid;         /* Pointer to default gafile             */
  gaint fnum;                  /* File count                            */
  gaint dfnum;                 /* Default file number   */
  gaint fseq;                  /* Unique sequence num for files opened  */
  struct gadefn *pdf1;         /* Pointer to first define block         */
  struct dt tmin,tmax;
  gaint vdim[5];               /* Which dimensions vary?                */
  gaint x1ex,x2ex,y1ex,y2ex;   /* For -ex flag on fwrite */
  gaint xexflg,yexflg;         /* -ex -- are dims valid? */
  gadouble pxsize,pysize;      /* Physical page size in inches          */
  gaint orient;                /* Page orientation                      */
  gaint vpflag;                /* If 1, virtual page being used         */
  gadouble xsiz,xsiz1,xsiz2;   /* Physical plotting size in X direction */
  gadouble ysiz,ysiz1,ysiz2;   /* Physical plotting size in Y direction */
  gaint paflg;                 /* User has specified plotting area      */
  gadouble pxmin,pxmax;        /* User specified plotting area          */
  gadouble pymin,pymax;
  gaint clab;                  /* control contour labels.               */
  gaint clskip;                /* Contour label skipping                */
  char *clstr;                 /* Contour label template                */
  gadouble rainmn,rainmx;      /* control rainbow colors                */
  gaint rbflg;                 /* User rainbow colors specified         */
  gaint rbcols[256];           /* User rainbow colors                   */
  gadouble cmin,cmax,cint;     /* User specified contour limits         */
  gaint cflag;                 /* If true, user specifies contour levels*/
  gadouble clevs[256];         /* User specified contour levels         */
  gaint ccflg;                 /* If true, user specifies contour colors*/
  gaint ccols[256];            /* User specified contour colors         */
  gaint shdcls[256];           /* Shade colors after shading            */
  gadouble shdlvs[256];        /* Shade levels                          */
  gaint shdcnt;                /* Number of shdlvs, shdcls              */
  gaint cntrcnt;               /* Number of contours (after countouring)*/
  gaint cntrcols[256];         /* Contour colors (after contouring)     */
  gadouble cntrlevs[256];      /* Contour levels (after contouring)     */
  gaint ccolor,cstyle;         /* User contour/line appearance          */
  gaint cthick;                /* User gx display line thickness        */
  gaint cmark;                 /* Line marker type                      */
  gaint csmth;                 /* Contour smoothing on or off           */
  gaint cterp;                 /* Spline fit on or off                  */
  gadouble rmin,rmax,rint;     /* Axis limits for 1-D plots             */
  gadouble rmin2,rmax2,rint2;  /* Axis limits for 1-D plots             */
  gaint aflag,aflag2;          /* Keep 1D axis limits fixed             */
  gaint grflag,grstyl,grcolr;  /* Grid flag, linestyle, color           */
  gaint dignum;                /* grid value plot control (gxout=grid)  */
  gadouble digsiz;
  gaint arrflg;                /* Use already set arrow scaling         */
  gadouble arrsiz;             /* Arrow size in inches                  */
  gaint arlflg;                /* Arrow label flag */
  gadouble arrmag;             /* Vector magnitude producing arrsiz arrw*/
  gadouble ahdsiz;             /* Arrow head size.       */
  gaint hemflg;                /* -1; auto  0; nhem  1; shem */
  gaint miconn;                /* Connect line graph accross missing    */
  gaint strmden;               /* Streamline density indicator  */
  gaint mdlblnk,mdldig3;       /* Station model plot opts */
  char *prstr;                 /* Format string for gxout print */
  gaint prlnum;                /* Number of values per record */
  gaint prbnum;                /* Number of blanks to add between values */
  gaint prudef;                /* Undef printed as "undef" or value */
  gaint fgvals[50];            /* Values for grid fill */
  gaint fgcols[50];
  gaint fgcnt;
  gaint gridln;                /* Line attributes for gxout grid */
  gaint stidflg;               /* Plot station ids with values      */
  gadouble axmin,axmax,axint;  /* Overrides for X-axis labels           */
  gadouble aymin,aymax,ayint;  /* Overrides for Y-axis labels           */
  gaint axflg, ayflg;          /* Is override in effect for the axis?   */
  gaint frame;                 /* Display frame?  */
  gaint rotate;                /* Rotate plot from default orientation  */
  gaint xflip, yflip;          /* Flip X or Y axes                      */
  gaint zlog;                  /* Z coordinate in log scale */
  gaint coslat;                /* Lat coordinate scaled as cos lat */
  gaint mproj;                 /* Map projection -- used for X,Y plot   */
                               /*  only.  0 = no map.                   */
  gaint mpdraw;                /* Draw map outline - 0=no               */
  gadouble mpvals[10];         /* Map projection option values.         */
  gaint mpflg;                 /* Map projection option values are set. */
  char *mpdset[8];             /* Map data set names.                   */
  gaint mpcols[256];           /* Map Color array                       */
  gaint mpstls[256];           /* Map line styles array                 */
  gaint mpthks[256];           /* Map line widths array                 */
  gaint mapcol,mapstl,mapthk;  /* Default map color, style, thickness   */
  gaint gout0;                 /* Graphics output type for stat.        */
  gaint gout1;                 /* Graphics output type for 1-D.         */
  gaint gout1a;                /* Graphics output type for 1-D.         */
  gaint gout2a;                /* Graphics output type for 2-D.         */
  gaint gout2b;                /* Graphics output type for 2-D.         */
  gaint goutstn;               /* Graphics output type for stns */
  gaint blkflg;                /* Leave certain values black when shadng*/
  gadouble blkmin, blkmax;     /* Black range */
  gaint reccol,recthk;         /* Draw Rectangle color, brdr thickness  */
  gaint lincol,linstl,linthk;  /* Draw line color, style, thickness     */
  gaint mcolor;                /* auto color (orange or grey)           */
  gaint strcol,strthk,strjst;  /* Draw string color, thckns, justifictn */
  gadouble strrot;             /* Draw string rotation */
  gadouble strhsz,strvsz;      /* Draw string hor. size, vert. size     */
  gaint anncol,annthk;         /* Draw title color, thickness           */
  gaint grflg;                 /* Grey Scale flag   */
  gaint devbck;                /* Device background */
  gaint xlcol,xlthck,ylcol,ylthck,clcol,clthck;  /* color, thickness */
  gaint xlside,ylside,ylpflg;
  gadouble xlsiz,ylsiz,clsiz,xlpos,ylpos,yllow;         /* Axis lable size */
  gadouble xlevs[50],ylevs[50]; /* User specified x/y axis labels  */
  gaint xlflg,ylflg;           /* Number of user specified labels */
  gaint xtick,ytick;           /* Number of extra tick marks      */
  gadouble xlint,ylint;        /* User specified label increment */
  char *xlstr, *ylstr;         /* user substitution string for labels */
  gaint xlab,ylab;             /* Axis label options */
  char *xlabs, *ylabs;         /* User specifies all labels */
  gaint ixlabs, iylabs;        /* Count of user labels */
  gaint tlsupp;                /* Suppress year or month of time labels */
  gaint lfc1,lfc2;             /* Linefill colors */
  gaint wxcols[5];             /* wx symbol colors */
  gaint wxopt;                 /* wx options */
  gaint tser;                  /* station time series type */
  gaint bargap;                /* Bar Gap in percent  */
  gaint barolin;               /* Bar outline flag */
  gadouble barbase;            /* Bar Base Value      */
  gaint barflg;                /* Bar flag: 1, use base value  */
                               /*           0, draw from plot base */
                               /*          -1, draw from plot top  */
  gaint btnfc,btnbc,btnoc,btnoc2;     /* Current button attributes */
  gaint btnftc,btnbtc,btnotc,btnotc2;
  gaint btnthk;
  gaint dlgfc,dlgbc,dlgoc;   /* Current dialog attributes */
  gaint dlgpc,dlgth,dlgnu;
  gaint drvals[15];          /* Attributes for drop menus */
  char *gtifname;            /* geotiff write file name */
  char *tifname;             /* kml image  file name */
  char *kmlname;             /* kml text file name */
  char *sdfwname;            /* netcdf/hdf write file name */
  gaint sdfwtype;            /* type of sdf output: 1=classic, 2=nc4 */
  gaint sdfwpad;             /* pad the sdf output with extra dims: 1=4D, 2=5D */
  gaint sdfprec;             /* precision (8==double, 4==float, etc.) */
  gaint sdfchunk;            /* flag to indicate whether or not to chunk */
  gaint sdfzip;              /* flag to indicate whether or not to compress */
  gaint ncwid;               /* netcdf write file id  */
  gaint xchunk;              /* size of sdfoutput file chunk in X dimension */
  gaint ychunk;              /* size of sdfoutput file chunk in Y dimension */
  gaint zchunk;              /* size of sdfoutput file chunk in Z dimension */
  gaint tchunk;              /* size of sdfoutput file chunk in T dimension */
  gaint echunk;              /* size of sdfoutput file chunk in E dimension */
  struct gaattr *attr;       /* pointer to link list of user-specified attributes */
  FILE *ffile;               /* grads.fwrite file handle */
  FILE *sfile;               /* grads.stnwrt file handle */
  char *fwname;              /* fwrite file name */
  gaint fwenflg;             /* fwrite byte order control */
  gaint fwsqflg;             /* fwrite stream vs fortran seq */
  gaint fwappend;            /* write mode (1): append */
  gaint fwexflg;             /* fwrite exact grid dims */
  gaint gtifflg;             /* geotiff data type: 0=image 1=float 2=double */
  gaint grdsflg;             /* Indicate whether to put grads atrib.  */
  gaint timelabflg;          /* Indicate whether to put cur time atrib.  */
  gaint stnprintflg;         /* Indicate whether to put cur time atrib.  */
  gaint dbflg;               /* Double buffer mode flag     */
  gaint batflg;              /* Batch mode */
  gaint numgrd,relnum;       /* Number of data objects held           */
  gaint type[16];            /* Data type of each data object         */
  union gadata result[16];   /* Pointers to held data objects         */
  struct gaclct *clct[32];   /* Anchor for collection */
  gaint clctnm[32];          /* Number of items collected */
  gaint clcttp[32];          /* Varying dimension of collection */
  gaint lastgx;              /* Last gx plotted */
  gaint xdim, ydim;          /* Which dimensions on X and Y axis */
  gaint statflg;             /* stat txt output on all displays */
  gaint impflg;              /* Implied run flag */
  char *impnam;              /* Implided run script name */
  gaint impcmd;              /* Implicit run */
  gaint sig;                 /* User has signalled */
  gaint ptflg;		     /* Pattern fill flag */
  gaint ptopt;		     /* Pattern option: */
			     /*		0, open  */
			     /*		1, solid */
			     /*		2, dot */
			     /*		3, line  */
  gaint ptden;		     /* Dot or line pattern density */
  gaint ptang;		     /* Line pattern angle */
  gaint dwrnflg;             /* Issue, or not, warnings about missing or constant data */
  gadouble undef;            /* default or user-defined undef value for print and file output */
  gadouble cachesf;          /* global scale factor for netcdf4/hdf5 cache size */
  gaint fillpoly;            /* color to fill shapfile polygons, -1 for no fill */
  gaint marktype;            /* type of mark for shapefile points */
  gadouble marksize;         /* size of mark for shapefile points */
};

/* Sructure for string substitution in templating -- the %ch template.  
   This forms a linked list chained from pchsub1 in gafile */
struct gachsub {
  struct gachsub *forw;       /* Forward pointer */
  gaint t1;                   /* First time for this substitution */
  gaint t2;                   /* Last time.  -99 indicates open ended */
  char *ch;                   /* Substitution string */
};

/* Structure for ensemble metadata */
struct gaens {
  char name[16];             /* name of ensemble */
  gaint length;              /* length of time axis */
  struct dt tinit;           /* initial time */
  gaint gt;                  /* initial time in grid units */
  gaint grbcode[4];          /* grib2 codes */
};

/* GA status structure.  Contains necessary info about the scaling
   and file structure in force.                                       */
struct gastat {
  struct gafile *pfi1;       /* Pointer to first gafile in chain      */
  struct gafile *pfid;       /* Pointer to default gafile             */
  struct gadefn *pdf1;       /* Pointer to first define block         */
  struct gaclct **pclct;     /* Pointer to the collection pointers    */
  union gadata result;       /* Result goes here                      */
  struct dt tmin,tmax;
  gadouble dmin[5],dmax[5];  /* Range of absolute dimensions          */
  gaint fnum;                /* Default file number                   */
  gaint type;                /* Result type (grid==1 or stn==0)       */
  gaint idim,jdim;           /* Varying dimensions                    */
};


/* Description of a data file.                                        */
struct gafile {
  struct gafile *pforw;      /* Forward pointer to next gafile block.
                                List is anchored within gastat.       */
  gaint fseq;                /* Unique sequence number for cache detection */
  char name[4096];           /* File name or URL                      */
  char *tempname;            /* File name of open file (differs with templates) */
  char dnam[4096];           /* Descriptor file name                  */
  char *mnam;                /* Map(index) file name */
  FILE *infile;              /* File pointer.                         */
  gaint type;                /* Type of file:  1 = grid
                                               2 = simple station
                                               3 = mapped station
                                               4 = defined grid       */
  char title[512];           /* Title -- describes the file.          */
  gadouble undef;            /* Global undefined value for this file  */
  gadouble ulow,uhi;         /* Undefined limits for missing data test  */
  gafloat *sbuf;             /* Buffer for file I/O equal in length
                                to the size needed to hold
                                the largest station report            */
  gadouble *rbuf;            /* Buffer for file I/O equal in length
                                to one grid row in the file           */
  unsigned char *pbuf;       /* Same as rbuf, for unpacking           */
  char *bbuf;                /* Same as rbuf, for bit map I/O         */
  char *ubuf;                /* Same as rbuf, for undef mask          */
  gaint bswap;               /* Byte swapping needed */
  gaint dhandle;             /* libgadap file handle.                 */
  gaint dapinf[5];           /* pointer to coordinate variable indices
				(first four elements are lon,lat,lev,time
				fifth is station id)
				for opendap station data only */
  gaint mtype;               /* Stn map file type                     */
  gaint *tstrt;              /* Pointer to list length dnum[3] of
                                start points of times in the file     */
  gaint *tcnt;               /* Count of stns for assctd time         */
  gaint stcnt;               /* Count of mapped stids when stn data
                                and map file is type stidmap.         */
  gaint stpos;               /* Position in map file of start of
                                stid info for map file type stidmap.  */
  FILE *mfile;               /* File pointer to stidmap file          */
  gaint dnum[5];               /* Dimension sizes for this file.        */
  gaint tlpflg;                /* Circular file flag                    */
  gaint tlpst;                 /* Start time offset in circular file    */
  gaint vnum;                  /* Number of variables.                  */
  gaint ivnum;                 /* Number of level independent variables
                                for station data file                 */
  gaint lvnum;                 /* Number of level dependent variables
                                for station data file                 */
  struct gavar *pvar1;       /* Pointer to an array of structures.
                                Each structure in the array has info
                                about the specific variable.          */
  struct gaens *ens1;          /* pointer to array of ensemble structures */
  long gsiz;                   /* Number of elements in a grid (x*y)    */
                               /* This is for actual grid on disk,
                                  not psuedo grid (when pp in force) */
  long tsiz;                   /* Number of elements in an entire time
                                  group (all variables at all levels
                                  for one time).                        */
  gaint trecs;                 /* Number of records (XY grids) per time
                                  group.                                */
  long fhdr;                   /* Number of bytes to ignore at file head*/
  gaint wrap;                  /* The grid globally 'wraps' in X        */
  gaint seqflg, yrflg, zrflg;  /* Format flags */
  gaint ppflag;                /* Pre-projected data in use */
  gaint pdefgnrl;              /* Keyword 'general' used instead of 'file' */
  gaint ppwrot;                /* Pre-projection wind rotation flag */
  gaint ppisiz, ppjsiz;        /* Actual size of preprojected grid */
  gadouble ppvals[20];         /* Projection constants for pre-projected
                                  grids.  Values depend on projection. */
  gaint *ppi[9];               /* Pointers to offsets for pre-projected
                                  grid interpolation */
  gadouble *ppf[9];            /* Pointers to interpolation constants
                                  for pre-projected grids */
  gadouble *ppw;               /* Pointer to wind rotation array */
  gadouble (*gr2ab[5]) (gadouble *, gadouble);
                               /* Addresses of routines to do conversion
                                  from grid coordinates to absolute
                                  coordinates for X, Y, Z.  All Date/time
                                  conversions handled by gr2t.          */
  gadouble (*ab2gr[5]) (gadouble *, gadouble);
                               /* Addresses of routines to do conversion
                                  from absolute coordinates to grid
                                  coordinates for X,Y,Z.  All date/time
                                  conversions handled by t2gr.          */
  gadouble *grvals[5];         /* Pointers to conversion information for
                                  grid-to-absolute conversion routines. */
  gadouble *abvals[5];         /* Pointers to conversion information for
                                  absolute-to-grid conversion routines. */
  gaint linear[5];             /* Indicates if a dimension has a linear
                                  grid/absolute coord transformation
                                  (Time coordinate always linear).      */
  gaint dimoff[5];             /* Dimension offsets for defined grids   */
  gaint climo;                 /* Climatological Flag (defined grids)   */
  gaint cysiz;                 /* Cycle size for climo grids            */
  gaint idxflg;                /* File records are indexed; 1==grib,station 2==grib2 */
  gaint grbgrd;                /* GRIB Grid type */
  struct gaindx *pindx;        /* Index Strucure if indexed file */
  struct gaindxb *pindxb;      /* Index Strucure if off_t offsets are being used */
#if GRIB2
  struct gag2indx *g2indx;     /* Index Strucure if GRIB2 file */
#endif
  gaint tmplat;                /* File name templating:
                                   3==templating on E and T 
                                   2==templating only on E 
                                   1==templating only on T, or when 
                                      ddf has 'options template', but no % in dset 
                                   0==no templating  */
  gaint *fnums;                /* File number for each time */
  gaint fnumc;                 /* Current file number that is open */
  gaint fnume;                 /* Current ensemble file number that is open */
  struct gachsub *pchsub1;     /* Pointer to first %ch substitution */
  gaint errcnt;                /* Current error count */
  gaint errflg;                /* Current error flag */
  gaint ncflg;                 /* 1==netcdf  2==hdfsds */
  gaint ncid;                  /* netcdf file id */
  gaint sdid;                  /* hdf-sds file id */
  gaint h5id;                  /* hdf5 file id */
  gaint packflg;               /* Data are packed with scale and offset values */
  gaint undefattrflg;          /* Undefined values are retrieved individually  */
  char *scattr;                /* scale factor attribute name for unpacking data */
  char *ofattr;                /* offset attribute name for unpacking data */
  char *undefattr;             /* undef attribute name */
  long xyhdr;                  /* Number of bytes to ignore at head of xy grids*/
  gaint calendar;              /* Support for 365-day calendars */
  gaint pa2mb;                 /* convert pressure values in descriptor file from Pa -> mb */
  gaint bufrflg;               /* 1==dtype bufr */
  struct bufrinfo *bufrinfo;   /* x,y pairs from descriptor file */ 
  gabufr_dset *bufrdset;       /* pointer to parsed bufr data */
  struct gaattr *attr;         /* pointer to link list of attribute metadata */
  gaint nsdfdims; 
  gaint sdfdimids[100];
  gaint sdfdimsiz[100];
  gaint time_type;             /* temporary flag for SDF time handling */
  char sdfdimnam[100][129];
  long cachesize;            /* default netcdf4/hdf5 cache size */
};

/* Structure that describes a grid (requestor or descriptor block).  */
struct gagrid {
  struct gafile *pfile;        /* Address of the associated gafile
                                  structure to get the data from
                                  (requestor block only)               */
  gadouble *grid;              /* Address of the grid.                 */
  gaint mnum;                  /* Number of grids when a multiple
                                  grid result.  Note in this case, *grid
                                  points to more than one grid, with the
                                  "default" result being the 1st grid  */ 
  gaint mtype;                 /* Type of multiple result grid         */
  gaint *mnums;                /* See mvals  */
  gadouble *mvals;             /* Metadata associated with a multiple
                                  grid result.  What is here depends on
                                  the value of mtype.                  */
  gadouble undef;              /* Undefined value for this grid.       */
  gadouble rmin,rmax;          /* Minimum/Maximum grid value
                                  (rmin is set to the grid value when
                                  isiz=jsiz=1.  *grid points to here.) */
  char *umask;                 /* Mask for undefined values in the grid */
  char umin,umax;              /* Min/max undefined mask values. 
                                  (when isiz=jsiz=1, umin is set to the 
                                  mask value and *umask points to umin) */
  gaint isiz,jsiz;             /* isiz = number of elements per row.
                                  jsiz = number of rows.               */
  gaint idim,jdim;             /* Dimension of rows and columns.
                                  -1 = This dimension does not vary
                                   0 = X dimension (usually longitude)
                                   1 = Y dimension (usually lattitude)
                                   2 = Z dimension (usually pressure)
                                   3 = Time
                                   4 = Ensemble
                                  If both dimensions are -1, then the
                                  grid has one value, which will be
                                  placed in rmin.                      */
  gaint iwrld, jwrld;          /* World coordinates valid?             */
  gaint dimmin[5],dimmax[5];   /* Dimension limits for each dimension
                                  (X,Y,Z,T,E) in grid units.           */
  struct gavar *pvar;          /* Pointer to the structure with info
                                  on this particular variable.  If
                                  NULL, this grid is the result of
                                  an expression evaluation where the
                                  variable type is unkown.             */
  char *exprsn;                /* If grid is a 'final' result, this
                                  will point to a character string that
                                  contains the original expression.    */
  gaint alocf;                  /* Scaling info allocated for us only  */
  gadouble (*igrab) (gadouble *, gadouble);
  gadouble (*jgrab) (gadouble *, gadouble);
                                /* Addresses of routines to perform
                                   grid-to-absolute coordinate
                                   transforms for this grid's i and j
                                   dimensions (unless i or j = 3).      */
  gadouble (*iabgr) (gadouble *, gadouble);
  gadouble (*jabgr) (gadouble *, gadouble);
                                /* Absolute to grid conversion routines */
  gadouble *ivals, *jvals;      /* Conversion info for grid to abs      */
  gadouble *iavals, *javals;    /* Conversion info for abs to grid      */
  gaint ilinr,jlinr;            /* Indicates if linear transformation   */
  gaint toff;                   /* Indicates if T dim values are forecast offsets */
};

/* Structure that contains attribute metadata */
struct gaattr {
  struct gaattr *next;          /* Address of next attribute */
  char  varname[129];           /* Name of variable or 'global' */
  char  name[129];              /* Name of attribute -- e.g. "units" */
  char  type[129];              /* Type of attribute -- e.g. "String", "Float32", etc. */
  gaint nctype;                 /* NetCDF (or HDF) data type index value */
  gaint len;                    /* Length of this attribute */
  gaint fromddf;                /* Flag for attributes from descriptor file */
  void  *value;                 /* Attribute value -- strings may contains blanks. */
};

#if USESHP==1
/* Structure that contains dBase field metadata */
struct dbfld {
  DBFFieldType type;
  char name[12];
  gaint len;
  gaint prec; 
};
#endif

/* Structure that contains the x,y pairs for bufr time values */
struct bufrtimeinfo {
  gaint yrxy[2];
  gaint moxy[2];
  gaint dyxy[2]; 
  gaint hrxy[2]; 
  gaint mnxy[2];  
  gaint scxy[2];  
};

/* Structure that contains the x,y pairs for file-wide bufr variables */
struct bufrinfo {
  gaint lonxy[2];
  gaint latxy[2];
  gaint levxy[2];
  gaint stidxy[2];
  struct bufrtimeinfo base,offset;   /* structures for base and offset time values */
};

/* Structure that contains the header (coordinate) info for a gabufr_msg */
struct bufrhdr {
  double lon;
  double lat;
  double lev;
  double sec,offsec;
  char   stid[8];
  struct dt tvals,toffvals;
};

/* Structure that describes a report header in a stn file */
struct rpthdr {
  char id[8];                  /* Character station id           */
  gafloat lat;                 /* Latitude of report             */
  gafloat lon;                 /* Longitude of report            */
  gafloat t;                   /* Time in relative grid units    */
  gaint nlev;                  /* Number of levels following     */
  gaint flag;                  /* Level independent var set flag */
};

/* Structure that describes a stid info block within a stidmap file */
struct stninf {
  char stid[8];
  gaint offset;
  gaint rcnt;
};

/* Structure that describes a single report                          */
struct garpt {
  struct garpt *rpt;           /* Address of next report               */
  char stid[8];                /* Station id                           */
  gadouble lat,lon,lev,tim;    /* Location of station                  */
  gaint work;                  /* Work area                            */
  gadouble val;                /* Value of variable                    */
  char umask;                  /* Undef mask                           */
};

/* Structure that describes a collection of station reports.         */
struct gastn {
  struct garpt *rpt;           /* Address of start of link list        */
  gaint rnum;                  /* Number of reports.                   */
  struct garpt *blks[BLKNUM];  /* ptrs to memory holding rpts      */
  struct gafile *pfi;          /* Address of the associated gafile
                                  structure to get the data from
                                  (requestor block only)               */
  gadouble undef;              /* Undefined value for this data.       */
  gadouble smin, smax;         /* Min and Max values for this data     */
  gaint idim,jdim;             /* Varying dimensions for this data
                                 -1 = This dimension does not vary
                                  1 = X dimension (longitude)
                                  2 = Y dimension (lattitude)
                                  3 = Z dimension (pressure)
                                  4 = Time                           */
  gadouble dmin[5],dmax[5];    /* Dimension limits for each dimension
                                  (X,Y,Z) in world coords.
                                  Non-varying dimensions can have
                                  limits in this structure.           */
  gaint rflag;                 /* Get stations within specified radius in
                                  degrees of fixed lat and lon         */
  gadouble radius;             /* Radius */
  gaint sflag;                 /* Get specific station  */
  char stid[8];                /* Station id to get */
  gaint tmin,tmax;             /* Grid limits of time */
  gadouble ftmin,ftmax;        /* Float-valued grid limits of time, 
			  	  equivalent to dmin[3],dmax[3]         */
  gadouble *tvals;             /* Pointer to conversion info for the
                                  time conversion routines.            */
  struct gavar *pvar;          /* Pointer to the structure with info
                                  on this particular variable.  If
                                  NULL, this grid is the result of
                                  an expression evaluation where the
                                  variable type is unkown.             */
  struct garpt **prev;         /* Used for allocating rpt structures   */
  struct garpt *crpt;
  gaint rptcnt,blkcnt;
};

/* Structure that describes a variable in a file.  These structures
   are built in arrays that are hung off of gafile structures.         */
struct gavar {
  char varnm[128];             /* Variable description.                */
  char abbrv[16];              /* Variable abbreviation.               */
  char longnm[257];            /* netcdf/hdf var name if different     */
  gadouble units[16];          /* Units indicator.                     
				  Vals 0-7 are for variable codes:
				  grib, non-float data, nc/hdf dims
				  Vals  8-11 are for grib level codes  */
  gaint offset;                /* Offset in grid elements of the start
                                  of this variable within a time group
                                  within this file.                    */
  gaint recoff;                /* Record (XY grid) offset of the start
                                  of this variable within a time group */
  gaint ncvid;                 /* netcdf vid for this variable         */
  gaint sdvid;                 /* hdf vid for this variable            */
  gaint h5vid;                 /* hdf5 dataset id for this variable    */
  gaint levels;                /* Number of levels for this variable.
                                  0 is special and indiates one grid is
                                  available for the surface only.      */
  gaint dfrm;                  /* format  type indicator
  				  1 - unsigned char
				  4 - int  			       */
  gaint var_t ;                /* variable t transform                 */
  gadouble scale;              /* scale factor for unpacking data      */
  gadouble add;                /* offset value for unpacking data      */
  gadouble undef;              /* undefined value                      */
  gaint vecpair;               /* Variable has a vector pair           */
  gaint isu;                   /* Variable is the u-component of a vector pair */
  gaint isdvar;                /* Variable is a valid data variable (for SDF files) */
  gaint nvardims;              /* Number of variable dimensions        */
  gaint vardimids[100];        /* Variable dimension IDs. 	       */
#if USEHDF5==1
  hid_t h5varflg;              /* hdf5 variable has been opened */
  hid_t dataspace;             /* dataspace allocated for hdf5 variable */
#endif
};

/* Structure that describes a function call.                           */
struct gafunc {
  gaint argnum;                /* Number of arguments found by fncprs  */
  char *argpnt[20];            /* Pointers to the argument strings     */
  char buff[1000];             /* Argument string buffer               */
};

/* Structure that describes a user defined function                    */
struct gaufb {
  struct gaufb *ufb;           /* Forward pointer                      */
  char name[8];                /* Function name                        */
  gaint alo,ahi;               /* Limits on number of args             */
  gaint atype[8];              /* Types of args. 0=expr,1=float,2=int,3=char */
  gaint sflg;                  /* Sequential or direct                 */
  char *fname;                 /* Name of user executable              */
  char *oname;                 /* File name for data transfer to user  */
  char *iname;                 /* File name for data transfer from user */
};

/* Structure that describes a defined grid */
struct gadefn {
  struct gadefn *pforw;        /* Linked list pointer                  */
  struct gafile *pfi;          /* File Structure containing the data   */
  char abbrv[20];              /* Abbreviation assigned to this        */
};

/* Stack to evaluate the expression.  The stack consists of an
   array of structures.                                               */
struct smem {
  gaint type;        /* Entry type: -2 stn,-1 grid,1=op,2='(',3=')'    */
  union sobj {
    gaint op;        /* Operator: 0=*, 1=/, 2=+                        */
    struct gagrid *pgr; /* Operand (grid or stn)                      */
    struct gastn *stn;
  } obj;
};

/* Index structure, for when the records in a data file are indexed.
   The indexing file will contain this structure at the front, followed
   by the specified number of header and indexing values.  These
   header and indexing values are file format specific. */

struct gaindx {
  gaint type;                   /* Indexing file type */
  gaint hinum;                  /* Number of header ints */	       
  gaint hfnum;                  /* Number of header floats */      
  gaint intnum;                 /* Number of index ints (long) */  
  gaint fltnum;                 /* Number of index floats */       
  gaint *hipnt;                 /* Pointer to header int values */ 
  gafloat *hfpnt;               /* Pointer to header float values */
  gaint *intpnt;                /* Pointer to index int values */  
  gafloat *fltpnt;              /* Pointer to index float values */
} ;
struct gaindxb {
  gaint bignum;                 /* Number of off_t values */	       
  off_t *bigpnt;                /* Pointer to off_t values */
} ;

#if GRIB2
/* Structures for GRIB2 data */
struct gag2indx {
  gaint version;                /* Version number: 1: gaint offsets  2: off_t offsets */
  gaint g2intnum;               /* Number of index offset values */  
  gaint *g2intpnt;              /* Pointer to index g2ints */
  off_t *g2bigpnt;              /* Pointer to record offsets when off_t offsets in use */
} ;

struct g2anchor {               /* structure for grib2 cache anchor */
  struct g2buff *start;         /* pointer to next grid in cache */
  struct g2buff *end;           /* pointer to previous grid in cache */
  gaint total;                  /* size of cache */
};

struct g2buff {                 /* structure for grib2 cache grids*/
  struct g2buff *next;          /* pointer to next grid in cache */
  struct g2buff *prev;          /* pointer to previous grid in cache */
  gaint fseq;                   /* file sequence number */
  gaint z,t,e;                  /* grid coordinates of non-varying dimensions */
  gaint size;                   /* number of grid points in the grid */
  char abbrv[16];               /* name of variable */
  gafloat *fld;                 /* grib2 field (returned by g2_getfld) */
  char *mask;                   /* undef mask for grib2 field */
};  
#endif

#if (USENETCDF == 1 || USEHDF == 1)
typedef struct {
    struct sdfnames *names1;                   /* Pointer to an array of varname structures. */
    gaint xsrch,ysrch,zsrch,tsrch,esrch ;      /* if these need to be searched for */
    gaint dvsrch;                              /* data var names need to be searched */
    gaint isxdf;                               /* is it xdf */
    gaint xsetup,ysetup,zsetup,tsetup,esetup;  /* if these need setting up */
    gaint needtitle, needundef, needunpack;
    gaint dvcount ;                            /* number of data variables */
    gaint *dvsetup ;                           /* does this var need levelcnt and longname? */
    gaint hasDDFundef ;
    char *xdimname;
    char *ydimname;
    char *zdimname;
    char *tdimname;
    char *edimname;
} GASDFPARMS ;

struct sdfnames {
  char abbrv[16];            /* GrADS var name                    */
  char longnm[257];          /* netcdf/hdf var name if different  */
};
#endif
 /***********************\
 *  Function Prototypes  *
 \***********************/

/* Functions in GRADS.C */

void gasig (gaint);
gaint gaqsig (void);

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
    prntgaattr: Print out a descriptor attribute 
    gagsdo: Execute command for a script
    getpst: Allocate and initialize a gastat block              */

void gainit (void);
gadouble qcachesf (void);

gaint gacmd (char *, struct gacmn *, gaint);
void gacln (struct gacmn *,gaint);
gaint gaenab (char *, struct gacmn *);
gaint gadraw (char *, struct gacmn *);
gaint gardrw (char *, struct gacmn *);
gaint gaexec (char *, struct gacmn *);
char *gagsdo (char *, gaint *);
gaint gadef (char *, struct gacmn *, gaint);
gaint gaudef (char *, struct gacmn *);
gaint gamodf (char *, struct gacmn *);
gaint gaqdef (char *, struct gacmn *, gaint);
gaint gaqury (char *, char *, struct gacmn *);
gaint gahelp (char *, struct gacmn *);
gaint gaset (char *, char *, struct gacmn *);
void set_nc_cache(size_t);
gaint gacoll (char *, struct gacmn *);
gaint gadspl (char *, struct gacmn *);
gaint gaspcl (char *, struct gacmn *);
gaint gapars (char *, struct gastat *, struct gacmn *) ;
void gagrel (struct gacmn *);
gaint gaopen (char *, struct gacmn *);
void cleanup (char *);
void mygreta(char *);
struct gastat *getpst (struct gacmn *);
void gaprnt (gaint, char *);
gaint prntgaattr (struct gafile *, char *, gaint, gaint);
#if READLINE == 1
gaint gahistory(char*, char *, struct gacmn *);
#endif /* READLINE == 1 */
gaint ncwrite (char *, struct gacmn *);
gaint sdfwatt (struct gacmn*, gaint, char *, char *, char *);
gaint sdfwdim (struct gafile *, struct gacmn *, gaint, gaint);
gaint sdfdefdim (gaint, char *, gaint, gaint *, gaint *);

gaint gaddes (char *, struct gafile *, gaint);
gaint deflin (char *, struct gafile *, gaint, gaint);
gaint deflev (char *, char *, struct gafile *, gaint);
gaint ddfattr (char *, struct gafile *);
struct gaattr *parseattr (char *);
struct gafile *getpfi (void);
void frepfi (struct gafile *, gaint);

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

gaint gaexpr (char *, struct gastat *) ;
gaint eval (gaint, struct smem *, gaint *);
gaint gaoper (struct smem *, gaint, gaint, gaint, gaint);
struct gagrid *gagrop (struct gagrid *, struct gagrid *, gaint, gaint);
struct gastn *gastop (struct gastn *, struct gastn *, gaint, gaint);
struct gastn *gascop (struct gastn *, gadouble, gaint, gaint);
struct gagrid *gagrvl (gadouble);
char *varprs (char *, struct gastat *) ;
gaint gagchk (struct gagrid *, struct gagrid *, gaint);
struct gafile *getdfn (char *, struct gastat *);
char *stnvar (char *, char *, struct gafile *, struct gavar *,
              struct gastat *);

/* Functions in GAFUNC:
    rtnprs: Parse and execute a function call
    gafopr: Perform opration for two-op function call
    gafdef: Read user function definition table
                                                                     */

char *rtnprs (char *, char *, struct gastat *) ;
gaint gafopr (struct gastat *, struct gastat *, gaint );
void gafdef (void);

/* Functions in GAIO:
    gaggrd: Get a grid from a data file
    gagrow: Get a row of data, possibly wrapped, from the file
    gafcor: Calculate file position of an item of data
    garrow: Get an item or items from the data file
    gagstn: Get a collection of stations
    garead:
    gaglvs: Get appropriate var and levs from rpt
    gaarpt: Allocate report block
    gasstn: Seek to location in station file
    garstn: Read station data
    gacstn: Cache a station report
    gagdef: Get grid from defined variable
    clicyc: Adjust for cyclic climatological defined variable
    gagpre: Predefined variable access        
    ganrow: Get an item or items from the netcdf data file
    ganhow: Get an item or items from the HDF-SDS data file
    gaopnc: Open a netcdf file 
    gaophdf: Open an HDF-SDS file

Note:  function prototype for garead is now in gaio.c
*/

gaint gaggrd (struct gagrid *);
gaint gagrow (gadouble *, char *, gaint *);
long gafcor (gaint, gaint, gaint, gaint);
gaint garrow (gaint, gaint, gaint, gaint, gaint, gaint, gadouble *, char *, gaint);
gaint gagstn (struct gastn *);
gaint gaglvs (gaint, struct rpthdr *, struct gastn *);
struct garpt *gaarpt (struct gastn *);
gaint gasstn (off_t);
gaint garstn (gaint, char *, off_t);
void gacstn (char *, char *, gaint, gaint);
void gagcst (gaint, char *);
gaint gagdef (void);
void clicyc(gaint *);
gaint gagpre (void);
gaint gairow (gaint, gaint, gaint, gaint, gaint, gaint, gaint, gadouble *, char *);
gaint gaird (off_t, gaint, gaint, gaint, gaint, gaint);
gaint gaprow (gaint, gaint, gaint, gaint, gaint, gaint, gaint, gadouble *, char *);
gaint gaopfn (gaint, gaint, gaint *, gaint *, struct gafile *);
gaint gappcn (struct gafile *, gaint, gaint);
void w3fb04 (gadouble, gadouble, gadouble, gadouble, gadouble *, gadouble *);
void ll2lc (gadouble *, gadouble, gadouble, gadouble *, gadouble *, gadouble *);
void ll2rotll (gadouble *, gadouble, gadouble, gadouble *, gadouble *, gadouble *);
void gaiomg (void);
gaint gancsetup (void);
gaint gancgrid(gadouble *, char *, gaint, gaint);
gaint gancrow (gaint, gaint, gaint, gaint, gaint, gaint, gadouble *, char *);
gaint gahrow  (gaint, gaint, gaint, gaint, gaint, gaint, gadouble *, char *);
gaint gah5row  (gaint, gaint, gaint, gaint, gaint, gaint, gadouble *, char *);
gaint gaopnc (struct gafile *, gaint, gaint);
gaint gaclosenc (struct gafile *);
gaint gaclosehdf (struct gafile *);
gaint gacloseh5 (struct gafile *);
gaint gaophdf (struct gafile *, gaint, gaint);
gaint gaoph5 (struct gafile *, gaint, gaint);
gaint h5setup (void);
#if USEHDF5==1
gaint h5openvar (gaint,char*,hid_t*,hid_t*);
gaint h5closevar (hid_t, hid_t);
#endif
gaint h5attr(gaint, char *, char *, gadouble *);
gaint hdfattr (gaint, char *, gadouble *);
gaint ncpattrs(gaint, char *, char *, gaint, gaint, char *);
gaint hdfpattrs(gaint, char *, char *, gaint, gaint, char *);
gaint h5pattrs(gaint, char *, char *, gaint, gaint, char *);
void prntwrap(char *, char *, char *);
#if GRIB2
struct g2buff * g2check (gaint, gaint, gaint);
struct g2buff * g2read (off_t, g2int, gaint, gaint, gaint);
void g2clear(void);
#endif

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
    gaimap: Output grid with image dump
                                                                      */
void gagx (struct gacmn *);
void gaplot (struct gacmn *);
void gas1d (struct gacmn *, gadouble, gadouble, gaint, gaint, struct gagrid *, struct gastn *);
void gas2d (struct gacmn *, struct gagrid *, gaint);
void gagrph (struct gacmn *,gaint);
void gastts (struct gacmn *);
void gadprnt (struct gacmn *);
void gaoutgds (struct gacmn *);
void galfil (struct gacmn *);
void lfint (gadouble, gadouble, gadouble, gadouble, gadouble *, gadouble *);
void lfout (gadouble *, gadouble *, gadouble *, gadouble *, gaint, gaint);
void gacntr (struct gacmn *, gaint);
void gastrm (struct gacmn *);
void gashad (struct gacmn *);
void gavect (struct gacmn *, gaint);
void gascat (struct gacmn *);
void gafgrd (struct gacmn *);
void gagtif (struct gacmn *, gaint);
void getcorners(struct gacmn *, struct gagrid *, gadouble *);
void ij2ll (struct gacmn *, gadouble, gadouble, gadouble, gadouble, gadouble *, gaint);
gaint write_kml(struct gacmn *, gadouble *);
void gafwrt (struct gacmn *);
void gastnwrt (struct gacmn *);
void gaarrw (gadouble, gadouble, gadouble, gadouble, gadouble);
void gaplvl (struct gacmn *);
void gamscl (struct gacmn *);
void gawmap (struct gacmn *, gaint);
void gacsel (gadouble, gadouble, gadouble *, gadouble *, gadouble *);
void gaaxis (gaint, struct gacmn *, gaint);
gaint galnch (gadouble, char *);
gaint galtch (gadouble, char *);
void gaconv (gadouble, gadouble, gadouble *, gadouble *);
void gagexp (gadouble *, gaint, gaint, gadouble *, gaint, gaint, char *, char *);
void gaglin (gadouble *, gaint, gaint, gadouble *, gaint, gaint, char *, char *);
struct gagrid *gaflip (struct gagrid *, struct gacmn *);
gaint gatinc (struct gacmn *, struct dt *, struct dt *);
void gasfil (gaint, gaint, float, float, float, float, float, float, float, float);
void trfill (float, float, float, float, float, float, float, float,
     float, gaint, gaint);
void gafstn (struct gacmn *);
void gapstn (struct gacmn *);
void gawsym (struct gacmn *);
void gasmrk (struct gacmn *);
void gabarb (gadouble, gadouble, gadouble, gadouble, gadouble, gadouble, gadouble, gaint);
void gapmdl (struct gacmn *);
void gasmdl (struct gacmn *, struct garpt *, gadouble *, char *);
gadouble wndexit (gadouble, gadouble, gadouble, gadouble, gadouble, gadouble *,
                                   gadouble *, gadouble *, gadouble *);
void gapprf (struct gacmn *);
void gatser (struct gacmn *);
void gampax (struct gacmn *);
void wxsym (gaint, gadouble, gadouble, gadouble, gaint, gaint *);
void wxprim (gaint, gadouble, gadouble, gadouble);
void gagsav (gaint, struct gacmn *, struct gagrid *);
void galnx (gadouble, gadouble, gadouble *, gadouble *);
void galny (gadouble, gadouble, gadouble *, gadouble *);
void gaalnx (gadouble, gadouble, gadouble *, gadouble *);
void gaalny (gadouble, gadouble, gadouble *, gadouble *);
void gaclx (gadouble, gadouble, gadouble *, gadouble *);
void gacly (gadouble, gadouble, gadouble *, gadouble *);
void gaaclx (gadouble, gadouble, gadouble *, gadouble *);
void gaacly (gadouble, gadouble, gadouble *, gadouble *);
void gagfil (gadouble *, gaint, gaint, gadouble *, gaint *, gaint, char *);
void gaimap (gadouble *, gaint, gaint, gadouble *, gaint *, gaint, char *, 
                   gadouble, gadouble, gadouble, gadouble);
void gafram (struct gacmn *);
void gaaxpl (struct gacmn *, gaint, gaint);
void gaselc (struct gacmn *, gadouble, gadouble);
gaint gashdc (struct gacmn *, gadouble);

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
    cmpwrd: Compare two character strings
    nxtwrd: Point to the next blank delimited word in a string
    liconv: Linear conversion routine
    gr2lev: Discrete level scaling routine
    lev2gr: Discrete level scaling routine
    intprs: Parse an integer expression
    longprs: Parse an long integer expression kk 020624 --- 
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
    cpscal: Copy grid scaling info
    getvnm: Get variable long name and abbrv 
    e2ens:  Converts ensemble number to name
    getenm: Get ensemble name
    dqual: test if two doubles are equal                                                 */

gaint nxtcmd (char *, char *);
void timadd (struct dt *, struct dt *);
void timsub (struct dt *, struct dt *);
gadouble t2gr (gadouble *, struct dt *);
void gr2t (gadouble *, gadouble, struct dt *);
gaint timdif (struct dt *, struct dt *);
gaint qleap (gaint);
char *adtprs (char *, struct dt *, struct dt *);
char *rdtprs (char *, struct dt *);
gaint cmpwrd (char *, char *);
gaint cmpwrdl (char *, char *);
char *nxtwrd (char *);
gadouble liconv (gadouble *, gadouble);
gadouble gr2lev (gadouble *, gadouble);
gadouble lev2gr (gadouble *, gadouble);
char *e2ens (struct gafile *, gadouble );
char *intprs (char *, gaint *);
char *longprs (char *, long *);
char *getdbl (char *, gadouble *);
char *getflt (char *, gafloat *);
char *dimprs (char *, struct gastat *, struct gafile *, gaint *, gadouble *, gaint, gaint *);
void lowcas (char *);
void uppcas (char *);
void getstr (char *, char *, gaint);
void getwrd (char *, char *, gaint);
void gamnmx (struct gagrid *);
void gasmnmx (struct gastn *);
gaint garemb (char *);
gadouble *gagaus(gaint,gaint);
gadouble *gags30(gaint,gaint);
gadouble *gagst62(gaint,gaint);
gadouble *gags20(gaint,gaint);
gadouble *gags15(gaint,gaint);
gadouble *gamo32(gaint,gaint);
gaint gat2ch (struct dt *, gaint, char *, gaint);
gaint cmpch (char *, char *, gaint);
void gafree (struct gastat *);
void gagfre (struct gagrid *);
void gasfre (struct gastn *);
void fnmexp (char *, char *, char *);
gaint gagbb (unsigned char *, gaint, gaint);
gaint gagby (unsigned char *, gaint, gaint);
void gapby (gaint, unsigned char *, gaint, gaint);
void gapbb (gaint, unsigned char *, gaint, gaint);
char *gafndt (char *, struct dt *, struct dt *, gadouble *, 
	      struct gachsub *, struct gaens *, gaint, gaint, gaint *);
void gabswp (void *, gaint);
void gabswp8 (void *, gaint);
void gahswp (struct rpthdr *);
gaint dayweek (struct dt *);
gaint wrdlen (char *);
void ganbswp(char *, gaint);
/* Wesley Ebisuzaki routines */
gaint flt2ibm(gafloat x, unsigned char *ibm);
gafloat ibm2flt(unsigned char *ibm);
gafloat ieee2flt(unsigned char *);
gadouble ieee2dbl(unsigned char *);
gaint flt2ieee(gafloat , unsigned char *);
gadouble scaled2dbl(gaint, gaint);
gadouble Int_Power(gadouble, gaint );

gadouble *cpscal (gadouble *, gaint, gaint, gaint);
gaint getvnm (struct gavar *, char *);
gaint getenm (struct gaens *, char *);
gaint dequal(gadouble, gadouble, gadouble);
gaint nxrdln (char *, char *);

/* Functions in GASRCP:
    gsfile: run a script file used in gauser.c */
char *gsfile (char *, gaint *, gaint);

/* Functions in gxX:
    gxwdln: use X server for wide lines */
void gxwdln(void);


#ifdef USEGADAP
/* Function in dodstn */
gaint dappfi (char *, struct gafile *);  
gaint dapget (struct gastn *);
void  dapclo (struct gafile *);
#endif


/* Functions in bufrstn.c */
void getbufrhdr (gabufr_val *, gabufr_val *, struct bufrinfo *, struct bufrhdr *, gaint, gaint *);
gaint  getbufr (struct gastn *);
struct garpt * sortrpt(struct garpt *);



void gacfg (gaint); 
void gaqufb (void); 
gaint gxhpng (char *, gaint, gaint, gaint, gaint, char *, char *, gaint) ;


#if (USENETCDF == 1 || USEHDF == 1)
/* Functions in gasdf.c */
gaint gasdfopen(char *, struct gacmn *) ;
gaint gaxdfopen(char *, struct gacmn *) ;
struct gaattr *find_att(char *, struct gaattr *, char *);
struct gavar *find_var(struct gafile *, char *);
gaint findX(struct gafile *, struct gavar **);
gaint findY(struct gafile *, struct gavar **);
gaint findZ(struct gafile *, struct gavar **, gaint *);
gaint findT(struct gafile *, struct gavar **);
gaint findE(struct gafile *, struct gavar **);
gaint isdvar(struct gafile *, struct gavar *, gaint,  gaint, gaint, gaint, gaint) ;
gaint read_metadata(struct gafile *);
gaint read_hdfatts (gaint, char *, gaint, struct gafile *);
gaint read_ncatts (gaint, gaint, char *, gaint, struct gafile *);
gaint set_time_type (struct gafile *);
void  initparms(GASDFPARMS *);
void  freeparms(GASDFPARMS *);
gaint read_one_dimension (struct gafile *, struct gavar *, gaint, gaint, gadouble *);
gaint compare_units(char *, char *);
gaint find_dim(struct gafile *, char *);
void  handle_error(gaint);
void  close_sdf(struct gafile *);
gaint decode_standard_time(gadouble, gaint *, gaint *, gaint *, gaint *, gaint *, gafloat *);
gaint decode_delta_t(char *, gaint *, gaint *, gaint *, gaint *, gaint *, gaint *);
gaint init_standard_arrays (gaint);
gaint gadxdf(struct gafile *, GASDFPARMS *);
gaint gadsdf(struct gafile *, GASDFPARMS);
gaint sdfdeflev(struct gafile *, struct gavar *, gaint, gaint) ;
gaint getncvnm (struct sdfnames *, char *);
#endif

void *galloc(size_t,char *);
void gree();
void glook();
