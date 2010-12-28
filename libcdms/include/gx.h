/*  Copyright (C) 1988-2010 by Brian Doty and the 
    Institute of Global Environment and Society (IGES).  
    See file COPYRIGHT for more information.   */

#include <stdlib.h>

/* Installation options for the GX package. */

/* HBUFSZ is the size of the metafile output buffer in
   number of short integers.  The metafile buffer should be as
   large as is convenient for the target system.  Frames larger
   than the buffer will get bufferred into the meta file on disk,
   when BUFOPT is 1.  Otherwise multiple buffers of size HBUFSZ
   will be allocated as needed. */

#define HBUFSZ 10000000L 
#define BUFOPT 0
#define pi 3.14159265358979

/* Default directory containing the stroke and map data sets.
   User can override this default via setenv GADDIR */

/* static char *datad = "/usr/local/lib/grads"; */


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
  gadouble lnmn,lnmx,ltmn,ltmx;        /* Lat,lon limits for projections */
  gadouble lnref;                      /* Reference longitude            */
  gadouble ltref1,ltref2;              /* Reference latitudes            */
  gadouble xmn,xmx,ymn,ymx;            /* Put map in this page area      */
  gadouble axmn,axmx,aymn,aymx;        /* Actual page area used by proj. */
};

/* Structure for holding info on displayed widgets. */

struct gobj {
  gaint type;                 /* Basic type of object. -1 - end of list;
                                0 - none; 1 - btn; 2 - rbb; 3 = popm */
  gaint i1,i2,j1,j2;          /* Extent of object */
  gaint mb;                   /* Mouse button that invokes object */
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
  gadouble x,y,w,h;              /* Button location, size   */     
  gaint num;                     /* Button number (-1, unset) */
  gaint ilo,ihi,jlo,jhi;
  gaint fc,bc,oc1,oc2,ftc,btc,otc1,otc2;  /* Colors           */
  gaint thk;                     /* Thickness of outline    */
  gaint state;                   /* Toggled or not?         */
  gaint len;                     /* Length of string        */
  char *ch;                      /* String content of btn   */
};

/* Structure holds info on rubber-band regions */

struct grbb {
  gadouble xlo,xhi,ylo,yhi;        /* Rubber band region      */  
  gaint num;                       /* Region number (-1, unset) */
  gaint mb;                        /* Mouse button specific   */
  gaint type;                      /* 0 for box, 1 for line   */
};

/* Structure for info on drop menus */

struct gdmu {
  gadouble x,y,w,h;                /* Header button loc,size  */
  gaint num;                       /* Menu number             */
  gaint casc;                      /* Anchored?               */
  gaint ilo,ihi,jlo,jhi;
  gaint fc,bc,oc1,oc2;             /* Colors of base          */
  gaint tfc,tbc,toc1,toc2;         /* Colors of selected base */
  gaint bfc,bbc,boc1,boc2;         /* Colors of box           */
  gaint soc1,soc2;                 /* Colors of selected item */
  gaint thk;                       /* Thickness of outlines   */
  gaint len;                       /* Length of string        */
  char *ch;                        /* String content of menu  */
};

/* Structure holds info on dialog */

struct gdlg {
  gadouble x,y,w,h;                /* Button location, size   */
  gaint pc,fc,bc,oc;  	  	   /* Colors           	    */
  gaint th;                        /* Thickness of outline    */
  gaint len;                       /* Length of string        */
  gaint nu;                        /* Flag for numeric args   */
  char *ch;                        /* String content of btn   */
};


/* GrADS event queue. This queue is built as the mouse button
   is clicked, and is cleared by a GrADS clear event.  Events
   are removed from the queue via the 'q pos' command.  */

struct gevent {
  struct gevent *forw;     /* Forward pointer */
  gadouble x, y;           /* X and Y position of cursor */
  gaint mbtn;              /* Mouse button pressed */
  gaint type;              /* Type of event */
  gaint info[10];          /* Integer info about event */
  gadouble rinfo[4];       /* info about event */
};

/* Structure for passing information on map plotting options */

struct mapopt {
  gadouble lnmin,lnmax,ltmin,ltmax;  /* Plot bounds */
  gaint dcol,dstl,dthk;              /* Default color, style, thickness */
  gaint *mcol,*mstl,*mthk;           /* Arrays of map line attributes */
  char *mpdset;                      /* Map data set name */
};

/* Structure for passing information on the currently open X Window */

struct xinfo {
  gaint winid;                 /* Window ID */
  gaint winx;                  /* Window X position (upper left) */
  gaint winy;                  /* Window Y position (upper left) */
  gauint winw;                 /* Window width */ 
  gauint winh;                 /* Window height */ 
  gauint winb;                 /* Window border width */
};

/* Struct for passing contour options */

struct gxcntr {
  gadouble labsiz;             /* Size of contour label, plotting inches */
  gaint spline;                /* Spline fit flag - 0 no, 1 yes */
  gaint ltype;                 /* Label type (off, on, masked, forced */
  gaint mask;                  /* Label masking flag - 0 no, 1 yes */
  gaint labcol;                /* Override label color, -1 uses contour color */
  gaint labwid;                /* Override label width, -1 uses contour width,
                                         -999 does double plot */
  gaint ccol;                  /* Contour color */
  char *label;                 /* Contour label */
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
   gxrs1wd: Reset one widget
   gxcpwd: Copy widgets on swap in double buffer mode
   gxevbn: Handle button press event
   gxevrb: Handle rubber-band event
   gxdptn: Set fill pattern
   gxmaskrec: Set mask for a rectangle
   gxmaskrq: query mask for a rectangular area
   gxmaskclear: Clear (unset) mask array
                                                           */

void gxdbgn (gadouble, gadouble);
void gxdbat (void);
void gxdend (void);
void gxdfrm (int);
void gxdcol (int);
gaint gxdacl (int, int, int, int);
void gxdwid (int);
void gxdmov (gadouble, gadouble);
void gxddrw (gadouble, gadouble);
void gxdrec (gadouble, gadouble, gadouble, gadouble);
void gxdsgl (void);
void gxdbl (void);
void gxdswp (void);
gaint gxqfil (void);
void gxdfil (gadouble *, gaint);
void gxdxsz (int, int);
void gxgrey (int);
void gxdbck (int);
gaint gxdbkq (void);
void gxdeve (int);
void gxdbtn (gaint, gadouble *, gadouble *, gaint *, gaint *, gaint *, gadouble *);
void gxdpbn (int, struct gbtn *, int, int, int);
void gxdrmu (int, struct gdmu *, int, int);
void gxdsfn (void);
void gxdcf (void);
void gxdrdw (void);
void gxrdrw (int);
void gxrswd (int);
void gxrs1wd (int, int);
void gxcpwd (void);
void gxevbn (struct gevent *, int);
void gxevrb (struct gevent *, int, int, int);
gaint gxevdm (struct gevent *, int, int, int);
gaint gxpopdm(struct gdmu *, int, int, int, int);
void gxdrbb (gaint, gaint, gadouble, gadouble, gadouble, gadouble, gaint);
char *gxdlg (struct gdlg *);
void gxdptn (int, int, int);
void gxdssv (int);
void gxdssh (int);
void gxdsfr (int);
gaint win_data (struct xinfo *);
void gxmaskrec (gadouble, gadouble, gadouble, gadouble);
gaint gxmaskrq (gadouble, gadouble, gadouble, gadouble);
void gxmaskclear (void);
void dump_back_buffer (char *);
void dump_front_buffer (char *);
void gxdgeo (char *);
int gxheps(char*);
void gxdimg(gaint *, gaint, gaint, gaint, gaint);
void gxdgcoord(gadouble, gadouble, gaint *, gaint *);

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
   gxsdrw: Draw, split into small segments to allow masking
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
   gxqwid: Query current line width
   gxqrgb: Query color rgb values
   gxqstl: Query current linestyle value
   gxmark: Draw marker
   gxfill: Polygon fill
   bdterp: Clipping Boundry Interpolation
   gxgsym: Get env var
   gxgnam: Get full path name
   gxptrn: Set fill pattern
                                                                */

void gxstrt (gadouble, gadouble, int, int);
void gxend (void);
void gxfrme (int);
void gxsfrm (void);
void gxcolr (int);
gaint gxacol (int, int, int, int);
void gxbckg (int);
gaint gxqbck (void);
void gxwide (int);
void gxmove (gadouble, gadouble);
void gxdraw (gadouble, gadouble);
void gxsdrw (gadouble, gadouble);
void gxstyl (int);
void gxplot (gadouble, gadouble, int);
void gxclip (gadouble, gadouble, gadouble, gadouble);
void gxtitl (char *, gadouble, gadouble, gadouble, gadouble, gadouble);
void gxvpag (gadouble, gadouble, gadouble, gadouble, gadouble, gadouble);
void gxvcon (gadouble, gadouble, gadouble *, gadouble *);
void gxppvp (gadouble, gadouble, gadouble *, gadouble *);
void gxscal (gadouble, gadouble, gadouble, gadouble, gadouble, gadouble, gadouble, gadouble);
void gxproj ( void (*) (gadouble, gadouble, gadouble*, gadouble*) );
void gxgrid ( void (*) (gadouble, gadouble, gadouble*, gadouble*) );
void gxback ( void (*) (gadouble, gadouble, gadouble*, gadouble*) );
void gxconv (gadouble, gadouble, gadouble *, gadouble *, gaint);
void gxxy2w (gadouble, gadouble, gadouble *, gadouble *); 
void gxgrmp (gadouble, gadouble, gadouble *, gadouble *);
void gxcord (gadouble *, gaint, gaint);
void gxrset (int);
void gxrecf (gadouble, gadouble, gadouble, gadouble);
gaint gxqwid (void);
gaint gxqclr (void);
void gxqrgb (gaint, gaint *, gaint *, gaint *);
gaint gxqstl (void);
void gxmark (gaint, gadouble, gadouble, gadouble);
void gxfill (gadouble *, gaint);
void bdterp (gadouble, gadouble, gadouble, gadouble, gadouble *, gadouble *);
void gxptrn (int, int, int);
char *gxgsym(char *);
char *gxgnam(char *);

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
void gxhnew (gadouble, gadouble, int);
gaint gxhbgn (char *);
void hout0 (int);
void hout1 (int, int);
void hout2 (int, gadouble, gadouble);
void hout4 (int, gadouble, gadouble, gadouble, gadouble);
void hout2i (int, int, int);
void hout3i (int, int, int, int);
void hout4i (int, int, int, int, int);
void hfull (void);
void gxhprt (char *);
gaint gxhwri (void *, int);
void gxhend (void);
void gxhfrm (int);
void gxhdrw (int);
void gxddbl (void);

/* Routines in gxchpl:
   gxchii: Initialize character plotting
   gxchdf: Set default font
   gxchpl: Plot character string
   gxchln: Determine length (in plotting units) of a string
   gxchgc: Get character info given character and font
   gxchrd: Read in a font
                            */
void  gxchii (void);
void  gxchdf (gaint);
void  gxchpl (char *, gaint, gadouble, gadouble, gadouble, gadouble, gadouble);
gaint gxchln (char *, gaint, gadouble, gadouble *);
char *gxchgc (gaint, gaint, gaint *);
gaint gxchrd (gaint);

/* Routine in gxcntr:
   gxclmn: Specify minimum distance between labels
   gxclev: Plot contour at specified value
   gxcflw: Follow a contour segment
   gxcspl: Spline fit a contour segment
   gxclab: Draw buffered contour labels.
   gxpclab: Plot contour labels, buffered or masked
   gxqclab: When masked labels, test for label overlap
   pathln: Find shortest col path through grid box
   gxcrel: Release storage used by the contouring system
                                                        */
void gxclmn (gadouble);
void gxclev (gadouble *, gaint, gaint, gaint, gaint, gaint, gaint,
                                    gadouble, char *, struct gxcntr *);
void gxcflw (gaint, gaint, gaint, gaint);
gaint gxcspl (gaint, struct gxcntr *);
void gxclab (gadouble, gaint, gaint);
void gxpclab (gadouble, gadouble, gadouble, gaint, struct gxcntr *);
int gxqclab (gadouble, gadouble, gadouble);
gaint pathln (gadouble, gadouble, gadouble, gadouble);
void gxcrel (void);
void gxpclin (void);

/* Routines in gxshad -- color filled contour routine:

   gxshad -- do color filled contours
   gxsflw -- Follow shade area boundries
   spathl -- Calculate col path lengths
   undcol -- Determine undefined-grid-side col characteristics
   putxy  -- Buffer current coordinate
   shdcmp -- Compress contour line
   shdmax -- Determine max or min interior
                                                                  */
void  gxshad (gadouble *, gaint, gaint, gadouble *, gaint *, gaint, char *);
gaint gxsflw (gaint, gaint, gaint);
gaint spathl (gadouble, gadouble, gadouble, gadouble);
gaint undcol (gaint, gaint);
gaint putxy  (gadouble, gadouble);
void  shdcmp (void);
gaint shdmax (void);

/* routines in gxstrm:  gxstrm (do streamlines) */

void gxstrm (gadouble *, gadouble *, gadouble *, gaint, gaint, char *, char *, char *,
	     gaint, gadouble *, gaint *, gaint, gaint);
void strmar (gadouble, gadouble, gadouble, gadouble);
gaint gxshdc (gadouble *, gaint *, gaint, gadouble);

/* Routines in gxwmap:
   gxwmap: Draw world map
   gxnmap: Draw medium res n.am. map
   gxmout: Output section of world map
   gxnste: Set up projection scaling for north polar stereographic
   gxnpst: Scaling routine for north polar stereographic projection
   gxaarw: Direction adjustment for map projection
   gxgmap: Medium and hi res map drawer
   gxhqpt: Plot quadrant of medium or hi res map
   gxmpoly: Interpolate polygon sides for drawing in non-linear map space
                                                                  */

void gxdmap (struct mapopt *);
void gxwmap (gadouble, gadouble, gadouble, gadouble);
void gxnmap (gadouble, gadouble, gadouble, gadouble);
void gxmout (int, gadouble, gadouble, gadouble, gadouble, gadouble);
int  gxltln (struct mapprj *);
int  gxscld (struct mapprj *, int, int);
int  gxnste (struct mapprj *);
void gxnpst (gadouble, gadouble, gadouble *, gadouble *);
void gxnrev (gadouble, gadouble, gadouble *, gadouble *);
int  gxsste (struct mapprj *);
void gxspst (gadouble, gadouble, gadouble *, gadouble *);
void gxsrev (gadouble, gadouble, gadouble *, gadouble *);
gadouble gxaarw (gadouble, gadouble);
void gxgmap (int, int, gadouble, gadouble, gadouble, gadouble);
void gxhqpt (int, int, int, gadouble, gadouble, gadouble, gadouble, gadouble);
int  gxrobi (struct mapprj *);
void gxrobp (gadouble, gadouble, gadouble *, gadouble *);
void gxrobb (gadouble, gadouble, gadouble *, gadouble *);
int  gxmoll (struct mapprj *);
void gxmollp (gadouble, gadouble, gadouble *, gadouble *);
void gxmollb (gadouble, gadouble, gadouble *, gadouble *);
int  gxortg (struct mapprj *);
void gxortgp (gadouble, gadouble, gadouble *, gadouble *);
void gxortgb (gadouble, gadouble, gadouble *, gadouble *);
int  gxlamc (struct mapprj *);
void gxlamcp (gadouble, gadouble, gadouble *, gadouble *);
void gxlamcb (gadouble, gadouble, gadouble *, gadouble *);
gadouble *gxmpoly(gadouble *xy, gaint cnt, gadouble llinc, gaint *newcnt); 
void gree();
