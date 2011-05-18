/*  Copyright (C) 1988-2010 by Brian Doty and the 
    Institute of Global Environment and Society (IGES).  
    See file COPYRIGHT for more information.   */

#ifndef DRIVER_GAGMAP
#define WHERE extern
#else
#define WHERE
#endif

WHERE FILE *gfile;
WHERE FILE *mfile;

/* Following structures hold all the unpacked header info from a grib record. */

struct grhdr {
  gaint vers;
  gaint len;
  gaint pdslen,gdslen,bmslen,bdslen;
  gaint id;
  gaint gdsflg,bmsflg;
  gaint parm;
  gaint ltyp;
  gaint level;
  gaint l1,l2;
  struct dt dtim;
  struct dt btim;
  gaint ftu,p1,p2,tri;
  gaint fcstu,fcstt;
  gaint cent;
  gafloat dsf;
  gaint gtyp,gicnt,gjcnt,gsf1,gsf2,gsf3;
  gaint bnumr;
  gaint bpos;
  off_t lbpos;
  gaint iflg;
  gafloat bsf;
  gafloat ref;
  gaint bnum;
  gaint dpos;
  off_t ldpos;
};

struct gr2hdr {
  gaint discipline,parcat,parnum;     /* Parameter identifiers */
  gaint yr,mo,dy,hr,mn,sc;            /* Reference Time */
  gaint sig;                          /* Significance of Reference Time */
  gaint numdp;                        /* Number of data points */
  gaint gdt;                          /* Grid Definition Template */
  gaint pdt;                          /* Product Definition Template */
  gaint drt;                          /* Data Representation Template */
  gaint trui;                         /* Time range units indicator */
  gaint ftime;                        /* Forecast time */
  gaint lev1type,lev1sf,lev1;         /* Level 1 type, scale factor, scaled value */
  gaint lev2type,lev2sf,lev2;         /* Level 2 type, scale factor, scaled value */
  gaint enstype,enspertnum,ensderiv;  /* Ensemble metadata */
  gaint comptype;                     /* Compression type (for JPEG2000 compression) */
};

/* ---------------- global variables ------------------- */
 
WHERE off_t fpos;           /* File pointer into GRIB file */
WHERE gaint verb;           /* Verbose option */
WHERE gaint bigflg;         /* Use 8 byte "off_t" pointers for >2GB file sizes */
WHERE gaint no_min;	    /* ignore minutes if == 1 */
WHERE gaint quiet;          /* quiet option */
WHERE gaint g1ver,g2ver;    /* version numbers */
WHERE gaint diag;           /* Verbose option */
WHERE gaint irec;
WHERE gaint scanflg;        /* general scan between GRIB records ASSUMED */
WHERE gaint scaneof;        /* option to ignore failure to find data at end of file */
WHERE gaint scanEOF;        /* option to ignore failure to find data at end of file */
WHERE gaint scanlim;        /* the default # of max bytes between records */
WHERE gaint notau;          /* force time to be base time */
WHERE gaint tauflg;         /* search for a fixed tau in filling the 4-D volume */
WHERE gaint tauoff;         /* the fixed tau in h */
WHERE gaint tau0;           /* set the base dtg for tau search */
WHERE gaint forceok;        /* set the base dtg for tau search */
WHERE gaint mpiflg;         /* Artificial initial date/time same as tau0!!*/
WHERE gaint write_map;      /* write out the map (testing only) */
WHERE gaint update;         /* update mode for templated files for NCEP CPC */
WHERE struct dt btimdd;     /* initial base time from dd file */
WHERE gaint tauave;         /* use p1 rather than p2 for time offset when tri is 3 -- 
                               eg, when product is an average, set the valid time
                               at the start of the averaged period rather than the end */
WHERE gaint nrec;           /* Number of records per grid */
WHERE gaint gtype[16];      /* Grid types for this grid set */
WHERE struct gafile *pfi;

WHERE struct gaindx *pindx;
WHERE struct gaindxb *pindxb;
WHERE struct dt dtim, dtimi;
WHERE gaint cnt,rc,i,flg,iarg,tcur,told;
WHERE char cmd[256];
WHERE unsigned char rec[512];
WHERE char crec[512],*ch, *ifile;

WHERE gaint len, skip;
WHERE struct grhdr ghdr;
WHERE struct gr2hdr g2hdr;
WHERE size_t sz;


/* ---------------- prototypes ------------------- */

extern gaint gribmap (void) ;
extern gaint gribhdr(struct grhdr *);
extern gaint grib2hdr(struct gr2hdr *);
extern gaint gribrec(struct grhdr *, struct gafile *, struct gaindx *, gaint, gaint, gaint);
extern void gribfill (gaint, gaint, gaint, gaint, struct grhdr *, struct gaindx *);
extern void gribpr (struct grhdr *);

/* function prototypes */
gaint wtgmap(void) ;
void  putint(gaint, unsigned char *,gaint *) ;
#if GRIB2
void  g2fill (gaint, gaint, gaint, off_t, g2int, struct gag2indx *);
gaint wtg2map (struct gafile *, struct gag2indx *);
gaint g2grid_check (gribfield *, struct gafile *pfi, gaint r, gaint f);
gaint g2time_check (gribfield *, g2int *, struct gafile *, gaint, gaint, gaint, gaint);
gaint g2var_match (gribfield *, struct gafile *, gaint);
gaint g2ens_match (gribfield *, struct gafile *);
gaint g2ens_check (struct gaens *, gribfield *);
void g2prnt (gribfield *, gaint, g2int, gaint);
gaint g2sp (gribfield *);
#endif
