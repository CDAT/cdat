
/* Copyright (c) 1988,1989,1990,1991 by Brian Doty
   See file COPYRIGHT for complete rights and liability information. */

void *galloc(int,char *);
void gree();
void glook();

/* Installation options for the GX package. */

/* HBUFSZ is the size of the metafile output buffer in
   number of short integers.  The metafile buffer should be as
   large as is convenient for the target system.  Frames larger
   than the buffer will get bufferred into the meta file on disk,
   when BUFOPT is 1.  Otherwise multiple buffers of size HBUFSZ
   will be allocated as needed. */

#define HBUFSZ 100000L
#define BUFOPT 0

/*------------------------------------------------------------------------------
  New projections:    Mollweide Projection (mollweide)
                      Orthographic Projection (orthogr)

  Changes in GrADS C source code files:
                      gxwmap.c
                      gagx.c
                      gauser.c
                      gx.h
   DKRZ
     10.08.95   Karin Meier (karin.meier@dkrz.de)

------------------------------------------------------------------------------*/

#define pi 3.14159265358979
static float lomin, lomax, lamin, lamax;


/* Default directory containing the stroke and map data sets.
   User can override this default via setenv GADDIR */

static char *datad = "/usr/local/lib/grads";

/* Option flag.  If 0, map data set is only read once into a
   dynamically allocated memory area.  The memory is held onto
   for the next call (about 35K).  If 1, the memory is allocated for
   each call and the map data set is read each time.             */
/* Lowres map only */

#define MAPOPT 0

/* Spacing to use for shading to get a 'solid' fill when drawing
   lines side by side at lineweight 3.  */

#define SDIFF 0.005

/* Structure for setting up map projections.  Used to call
   map projection routines.                                          */

struct mapprj {
  float lnmn,lnmx,ltmn,ltmx;        /* Lat,lon limits for projections */
  float lnref;                      /* Reference longitude            */
  float ltref1,ltref2;              /* Reference latitudes            */
  float xmn,xmx,ymn,ymx;            /* Put map in this page area      */
  float axmn,axmx,aymn,aymx;        /* Actual page area used by proj. */
};

/* Structure for holding info on displayed widgets. */

struct gobj {
  int type;                 /* Basic type of object. -1 - end of list;
                                0 - none; 1 - btn; 2 - rbb; 3 = popm */
  int i1,i2,j1,j2;          /* Extent of object */
  int mb;                   /* Mouse button that invokes object */
  union tobj {
    struct gbtn *btn;       /* Pointer to button struct */
    struct grbb *rbb;       /* Pointer to rubber-band struct */
    struct gdmu *dmu;       /* Pointer to drop menu struct */
  } iob;
};

/* Structure for holding information about GrADS button widgets */
/* Also used for popmenus, which display on the screen the same
   as buttons */

struct gbtn {
  int num;                       /* Button number (-1, unset) */
  float x,y,w,h;                 /* Button location, size   */
  int ilo,ihi,jlo,jhi;
  int fc,bc,oc1,oc2,ftc,btc,otc1,otc2;  /* Colors           */
  int thk;                       /* Thickness of outline    */
  int state;                     /* Toggled or not?         */
  int len;                       /* Length of string        */
  char *ch;                      /* String content of btn   */
};

/* Structure holds info on rubber-band regions */

struct grbb {
  int num;                       /* Region number (-1, unset) */
  int mb;                        /* Mouse button specific   */
  float xlo,xhi,ylo,yhi;         /* Rubber band region      */
  int type;                      /* 0 for box, 1 for line   */
};

/* Structure for info on drop menus */

struct gdmu {
  int num;                       /* Menu number             */
  int casc;                      /* Anchored?               */
  float x,y,w,h;                 /* Header button loc,size  */
  int ilo,ihi,jlo,jhi;
  int fc,bc,oc1,oc2;             /* Colors of base          */
  int tfc,tbc,toc1,toc2;         /* Colors of selected base */
  int bfc,bbc,boc1,boc2;         /* Colors of box           */
  int soc1,soc2;                 /* Colors of selected item */
  int thk;                       /* Thickness of outlines   */
  int len;                       /* Length of string        */
  char *ch;                      /* String content of menu  */
};


/* GrADS event queue. This queue is built as the mouse button
   is clicked, and is cleared by a GrADS clear event.  Events
   are removed from the queue via the 'q pos' command.  */

struct gevent {
  struct gevent *forw;   /* Forward pointer */
  float x, y;            /* X and Y position of cursor */
  int mbtn;              /* Mouse button pressed */
  int type;              /* Type of event */
  int info[10];          /* Integer info about event */
  float rinfo[4];        /* Floating point info about event */
};

/* Structure for passing information on map plotting
   options */

struct mapopt {
  int dcol,dstl,dthk;    /* Default color, style, thickness */
  int *mcol,*mstl,*mthk;  /* Arrays of map line attributes */
  float lnmin,lnmax,ltmin,ltmax;  /* Plot bounds */
  char *mpdset;          /* Map data set name */
};

/* Function prototypes for GX library routines  */

/* Functions in gxdev:
   gxdbgn: Initialize hardware
   gxdend: Terminate hardware
   gxdfrm: New frame
   gxdcol: New color
   gxadcl: Assign rgb color
   gxdwid: New line width
   gxdmov: Move pen
   gxddrw: Draw
   gxdrec: Filled rectangle
   gxdsgl: Set single buffer mode
   gxddbl: Set doulbe buffer mode
   gxdswp: Swap buffers
   gxqfil: Query availability of hardware polygon fill
   gxdfil: Hardware Polygon fill
   gxdxsz: Resize X Window (X only)
   gxdbtn: Get pointer pos at mouse btn press
   gxgrey: Set grey scale
   gxdbck: Set hardware background/foreground
   gxrswd: Reset Widget Structures
   gxcpwd: Copy widgets on swap in double buffer mode
   gxevbn: Handle button press event
   gxevrb: Handle rubber-band event
   gxdptn: Set fill pattern
                                                           */

void gxdbgn (float, float);
void gxdend (void);
void gxdfrm (int);
void gxdcol (int);
void gxdacl (int, int, int, int);
void gxdwid (int);
void gxdmov (float, float);
void gxddrw (float, float);
void gxdrec (float, float, float, float);
void gxdsgl (void);
void gxdbl (void);
void gxdswp (void);
int  gxqfil (void);
void gxdfil (float *, int);
void gxdfl2 (float *, int);
void gxdxsz (int, int);
void gxgrey (int);
void gxdbck (int);
int gxdeve (int);
void gxdbtn (int, float *, float *, int *, int *, int *, float *);
void gxdpbn (int, struct gbtn *, int, int, int);
void gxdrmu (int, struct gdmu *, int, int);
void gxdsfn (void);
void gxdtxt (char *, float, float);
void gxdcf (void);
int gxdfsw (char *, int, int);
void gxdrdw (void);
void gxrdrw (int);
void gxrswd (int);
void gxcpwd (void);
void gxevbn (struct gevent *, int);
void gxevrb (struct gevent *, int, int, int);
int gxevdm (struct gevent *, int, int, int);
int gxpopdm(struct gdmu *, int, int, int);
void gxdrbb (int, int, float, float, float, float,int);
char *gxdlg (char *);
void gxdptn (int, int, int);
void gxdssv (int);
void gxdssh (int);
void gxdsfr (int);

/* Routines in gxsubs:
   gxstrt: Initialize graphics output
   gxend:  Terminate graphics output
   gxfrme: Start new frame
   gxcolr: Set color attribute
   gxacol: Assign new rgb to color number from 16-99
   gxbckg: Set background color
   gxqbck: Query background color
   gxwide: Set line width attribute
   gxmove: Move to X, Y
   gxdraw: Draw solid line to X, Y using current color and linewidth
   gxstyl: Set linestyle
   gxplot: Move or draw using linestyles
   gxclip: Set clipping region
   gxchin: Initialize stroke font
   gxchpl: Draw character(s)
   gxtitl: Draw centered title
   gxvpag: Set up virtual page
   gxvcon: Do virtual page scaling
   gxscal: Set up level 1 (linear) scaling
   gxproj: Set up level 2 (projection) scaling
   gxgrid: Set up level 3 (grid) scaling
   gxback: Set up level 1 to level 2 back transform
   gxconv: Convert coordinates to level 0 (hardware)
   gxxy2w: Convert level 0 to level 2
   gxcord: Convert array of coordinates to level 0
   gxrset: Reset projection or grid level scaling
   gxrecf: Draw filled rectangle
   gxqclr: Query current color value
   gxqstl: Query current linestyle value
   gxmark: Draw marker
   gxfill: Polygon fill
   bdterp: Clipping Boundry Interpolation
   gxgsym: Get env var
   gxgnam: Get full path name
   gxptrn: Set fill pattern
                                                                */

void gxstrt (float, float,int);
void gxend (void);
void gxfrme (int);
void gxsfrm (void);
void gxcolr (int);
void gxacol (int, int, int, int);
void gxbckg (int);
int gxqbck (void);
void gxwide (int);
void gxmove (float, float);
void gxdraw (float, float);
void gxstyl (int);
void gxplot (float, float, int);
void gxclip (float, float, float, float);
void gxtitl (char *, float, float, float, float, float);
void gxvpag (float, float, float, float, float, float);
void gxvcon (float, float, float *, float *);
void gxppvp (float, float, float *, float *);
void gxscal (float, float, float, float, float, float, float, float);
void gxproj ( void (*) (float, float, float*, float*) );
void gxgrid ( void (*) (float, float, float*, float*) );
void gxback ( void (*) (float, float, float*, float*) );
void gxconv (float, float, float *, float *, int);
void gxxy2w (float, float, float *, float *);
void gxgrmp (float, float, float *, float *);
void gxcord (float *, int, int);
void gxpoly (float *, int, int);
void gxrset (int);
void gxrecf (float, float, float, float);
int gxqclr (void);
int gxqstl (void);
void gxmark (int, float, float, float);
void gxfill (float *, int);
void bdterp (float, float, float, float, float *, float *);
char *gxgsym(char *);
char *gxgnam(char *);
void gxptrn (int, int, int);

/* Gxmeta routines handle graphics buffering and metafile output.
   Routines in gxmeta:
   gxhopt: Specify buffering option before open
   gxhnew: Buffering initialization on startup
   gxhbgn: Enable hardcopy (metafile) output
   hout0:  Buffer 0 arg metafile command
   hout1:  Buffer one arg metafile command
   hout2:  Buffer two arg metafile command
   hout4:  Buffer four arg metafile command
   hout2i: Buffer two arg int metafile command
   hout3i: Buffer three arg int metafile command
   hout4i: Buffer four arg int metafile command
   hfull:  Deal with full metafile memory buffer
   gxhprt: Handle print command (output to metafile)
   gxhwri: Write buffer to metafile
   gxhend: Close output metafile
   gxhfrm: Handle new frame action
   gxhdrw: Handle redraw operation
                                           */

void gxhopt (int);
void gxhnew (float, float);
int gxhbgn (char *);
void hout0 (int);
void hout1 (int, int);
void hout2 (int, float, float);
void hout4 (int, float, float, float, float);
void hout2i (int, int, int);
void hout3i (int, int, int, int);
void hout4i (int, int, int, int, int);
void hfull (void);
void gxhprt (void);
int gxhwri (void *, int);
void gxhend (void);
void gxhfrm (int);
void gxhdrw (int);

/* Routines in gxchpl:
   gxchii: Initialize character plotting
   gxchdf: Set default font
   gxchpl: Plot character string
   gxchln: Determine length (in plotting units) of a string
   gxchgc: Get character info given character and font
   gxchrd: Read in a font
                            */
void gxchii (void);
void gxchdf (int);
void gxchpl (char *, int, float, float, float, float, float);
int gxchln (char *, int, float, float *);
char *gxchgc (int, int, int *);
int gxchrd (int);

/* Routine in gxcntr:
   gxclmn: Specify minimum distance between labels
   gxclev: Plot contour at specified value
   gxcflw: Follow a contour segment
   gxcspl: Spline fit a contour segment
   gxclab: Draw buffered contour labels.
   pathln: Find shortest col path through grid box
   gxcrel: Release storage used by the contouring system
                                                        */
void gxclmn (float);
void gxclev (float *, int, int, int, int, int, int,
                                    float, float, char *, int, int);
void gxcflw (int, int, int, int);
void gxcspl (void);
void gxclab (float,int,int);
int pathln (float, float, float, float);
void gxcrel (void);

/* Routines in gxshad -- color filled contour routine:

   gxshad -- do color filled contours
   gxsflw -- Follow shade area boundries
   spathl -- Calculate col path lengths
   undcol -- Determine undefined-grid-side col characteristics
   putxy  -- Buffer current coordinate
   shdcmp -- Compress contour line
   shdmax -- Determine max or min interior
                                                                  */
void gxshad ( float *, int, int, float *, int *, int, float);
int gxsflw (int, int, int);
int spathl (float, float, float, float);
int undcol (int, int);
int putxy (float, float);
void shdcmp (void);
int shdmax (void);

/* routines in gxstrm:  gxstrm (do streamlines) */

void gxstrm (float *, float *, float *, int, int, float, float, float,
   int, float *, int *, int, int);
void strmar (float, float, float, float);
int gxshdc (float *, int *, int, float);

/* Routines in gxwmap:
   gxwmap: Draw world map
   gxnmap: Draw medium res n.am. map
   gxmout: Output section of world map
   gxnste: Set up projection scaling for north polar stereographic
   gxnpst: Scaling routine for north polar stereographic projection
   gxaarw: Direction adjustment for map projection
   gxgmap: Medium and hi res map drawer
   gxhqpt: Plot quadrant of medium or hi res map
                                                                  */

void gxdmap (struct mapopt *);
void gxwmap (float, float, float, float);
void gxnmap (float, float, float, float);
void gxmout (int, float, float, float, float, float);
int  gxltln (struct mapprj *);
int  gxscld (struct mapprj *, int, int);
int  gxnste (struct mapprj *);
void gxnpst (float, float, float *, float *);
void gxnrev (float, float, float *, float *);
int  gxsste (struct mapprj *);
void gxspst (float, float, float *, float *);
void gxsrev (float, float, float *, float *);
float gxaarw (float, float);
void gxgmap (int, int, float, float, float, float);
void gxhqpt (int, int, int, float, float, float, float, float);
int  gxrobi (struct mapprj *);
void gxrobp (float, float, float *, float *);
void gxrobb (float, float, float *, float *);
/*---- DKRZ: appending new projections ---  10.08.95 Karin Meier

    gxmoll: Setup scaling Mollweide projection
    gxortg: Setup scaling Orthographic projection

---- DKRZ: end of new projections ---  karin.meier@dkrz.de ----*/

/*---- Mollweide Projection ----*/

int  gxmoll (struct mapprj *);
void gxmollp (float, float, float *, float *);
void gxmollb (float, float, float *, float *);

/*---- Orthographic Projection ----*/

int  gxortg (struct mapprj *);
void gxortgp (float, float, float *, float *);
void gxortgb (float, float, float *, float *);

/*------- DKRZ appendingd end ------*/

/*---- Lambert Conformal Projection ----*/

int  gxlamc (struct mapprj *);
void gxlamcp (float, float, float *, float *);
void gxlamcb (float, float, float *, float *);

/*------- DKRZ appendingd end ------*/
