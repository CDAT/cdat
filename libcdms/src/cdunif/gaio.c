/* Copyright (c) 1987-1993 by Brian Doty.  All Rights Reserved.

   See file COPYRIGHT for liability information.   */

#include <math.h>
#include <stdio.h>
#ifndef __APPLE__
#include <malloc.h>
#endif
#include "grads.h"

/*mf 971022 --- expose Mike Fiorino's global struct to these routines for warning level setting mf*/
extern struct gamfcmn mfcmn;
/*mf 971022 --- expose Mike Fiorino's global struct to these routines for warning level setting mf*/


/*
if GRADS_CRAY == 1
include <fortran.h>
fortran void IEEE2CRAY(float *, float *, int);
#endif
*/

int cvhdr (unsigned char *, struct rpthdr *);
int cvflt (unsigned char *, int);

static char pout[256];    /* Build error msgs here */

/* Global pointers for this file */

static struct gafile *pfi;
static struct gagrid *pgr;
static struct gavar *pvr;
static struct gaindx *pindx;
static int timerr;
static int msgflg=1;

/* GRIB I/O caching.  GRIB data is chached, as well as the bit
   maps, if present.  Sometimes the expanded bit map is cached. */

static char *cache;           /* I/O cache for GRIB */
static char *bcache;          /* Bit map cache */
static int cflag=0;           /* cache flag */
static int bcflag=0;          /* Bit cache flag */
static int bpsav = -999;      /* Bit cache pointer */
static int bssav = -999;      /* Bit cache size */
static int *bpcach;           /* expanded bit map cache */

/* Station data I/O caching.  We will cache fairly small I/O
   requests that fit within the specified size buffer.  If the buffer
   gets overfilled, we just forget the caching.  */

static int scflg = 0;         /* Anything cached? */
static int scuca = 0;         /* Can use cache for this request */
static int scerr = 0;         /* Buffer full? */
static int scok;              /* Ok to fill buffer */
static int scpnt;             /* Current cache offset */
static struct gastn scstn;    /* Previous request */
static char *scbuf=NULL;      /* Cache */
      /* Size of cache */
#define SCNUM 50000


/* Routine resets flag to allow warning in regards to interpolation */

void gaiomg () {
  msgflg = 1;
}

/* Routine to obtain a grid.  The addresses of the gagrid
   structure is passed to this routine.  The storage for the
   grid is obtained and the grid is filled with data.                 */

int gaggrd (struct gagrid *pgrid) {
int fipnt;
float *gr;
int x,i,id,jd,d[4],dx[4];
int incr,rc,dflag;
int size,ssz;
#if USESDF == 1
/* prototype for SDF reader */
    int gagsdf(struct gagrid *gridptr, float grid[]) ;
#endif

  if (cflag) free(cache);
  cache = NULL;
  cflag = 0;
  if (bcflag) {
    free(bcache);
    free(bpcach);
  }
  bcache = NULL;
  bpcach = NULL;
  bcflag = 0;
  bssav = -999;
  bpsav = -999;


  pgr = pgrid;
  pvr = pgr->pvar;
  pfi = pgr->pfile;
  timerr = 0;
  if (pfi->idxflg) pindx = pfi->pindx;

  if (pfi->ppflag && msgflg) {
    gaprnt (3,"Notice:  Automatic Grid Interpolation Taking Place\n");
    msgflg = 0;
  }

  if (pfi->type==4) {
    rc = gagdef();
    return (rc);
  }

  /* Check dimensions we were given */

  if (pgr->idim < -1 || pgr->idim > 3 ||
      pgr->jdim < -1 || pgr->jdim > 3 ||
      ( pgr->idim == -1 && pgr->jdim!=-1 ) ) {
    sprintf (pout,"Internal logic check 16:  %i %i  \n", pgr->idim,
            pgr->jdim);
    gaprnt (0,pout);
    return (16);
  }

  /* Calc sizes and get storage for the grid */

  id = pgr->idim;
  jd = pgr->jdim;
  if (id > -1)  pgr->isiz = pgr->dimmax[id] - pgr->dimmin[id] + 1;
  else pgr->isiz = 1;
  if (jd > -1)  pgr->jsiz = pgr->dimmax[jd] - pgr->dimmin[jd] + 1;
  else pgr->jsiz = 1;

  size = pgr->isiz*pgr->jsiz;
  if (size>1) {
    ssz  = size * sizeof(float);
    gr = (float *)malloc(ssz);
    if (gr==NULL) {
      gaprnt (0,"Memory Allocation Error:  grid storage \n");
      return (1);
    }
    pgr->grid = gr;
  } else {
    pgr->grid = &(pgr->rmin);
    gr = pgr->grid;
  }

  /* Handle predefined variable */

  if (pvr->levels<-900) {
    rc = gagpre();
    return (rc);
  }

  for (i=0; i<4; i++) {d[i] = pgr->dimmin[i]; dx[i] = pfi->dnum[i];}
  dx[2] = pvr->levels;
  if (dx[2]==0) {
    if (id==2 || jd==2) goto nodat;
    dx[2] = 1;
    d[2] = 1;
  }

  incr = pgr->isiz;

  /* If X does not vary, make sure the X coordinate is normalized.    */

  if (id!=0 && pfi->wrap) {
    x=pgr->dimmin[0];
    while (x<1) x=x+dx[0];
    while (x>dx[0]) x=x-dx[0];
    pgr->dimmin[0]=x;
    pgr->dimmax[0]=x;
    d[0] = x;
  }

  /* If any of the non-varying dimensions are out of bounds of the
     file dimension limits, then we have a grid of missing data.
     Check for this.                                                  */

  for (i=0; i<4; i++) {
    if (id!=i && jd!=i &&
       (d[i]<1 || d[i]>dx[i]) ) goto nodat;
  }

#if USESDF == 1
/* If we don't have a regular FILE*, but we do have an IO_STD*, use SDF read */
  if (!(pfi->infile) && (pfi->sdf_ptr)) {
    return(gagsdf(pgr, gr)) ;
  }
#endif

  /* Handle case where X varies.                                      */

  dflag = 0;
  if ( id == 0 ) {
    if (jd<0) jd = 1;
    for (d[jd]=pgr->dimmin[jd]; d[jd]<=pgr->dimmax[jd]; d[jd]++) {
      if (d[jd]<1 || d[jd]>dx[jd]) {
        for (i=0; i<incr; i++) *(gr+i) = pgr->undef;
      } else {
        rc = gagrow(gr, d);
        if (rc > 0 ) return (1);
        if (rc==0) dflag=1;
      }
      gr += incr;
    }
    if (!dflag) goto nodatmsg;
    return (0);
  }

  /* Handle cases where X does not vary.  We will have to read
     each point in the grid seperately.                               */

  if (jd<0) {
    if (id<0) { id=0; jd=1; }
    else jd=0;
  }

  for (d[jd]=pgr->dimmin[jd]; d[jd]<=pgr->dimmax[jd]; d[jd]++) {
    if (d[jd]<1 || d[jd]>dx[jd]) {
      for (i=0; i<incr; i++,gr++) *gr = pgr->undef;
    } else {
      for (d[id]=pgr->dimmin[id]; d[id]<=pgr->dimmax[id]; d[id]++) {
        if (d[id]<1 || d[id]>dx[id]) *gr = pgr->undef;
        else {
          rc = garrow (d[0], d[1], d[2], d[3], 1, gr);
          if (rc != 0 ) return (1);
          dflag=1;
        }
        gr++;
      }
    }
  }
  if (!dflag) goto nodatmsg;
  return (0);

nodat:
  for (i=0; i<size; i++,gr++) *gr = pgr->undef;

nodatmsg:
  if(mfcmn.warnflg>0) {
    gaprnt (1,"Data Request Warning:  Request beyond file limits\n");
    gaprnt (2,"  Entire grid contents are set to missing data \n");
    sprintf (pout,"  Dimension ranges are:  X = %i %i  Y = %i %i ",
	     d[0],dx[0],d[1],dx[1]);
    gaprnt (2,pout);
    sprintf (pout," Z = %i %i  T = %i %i \n",d[2],dx[2],d[3],dx[3]);
    gaprnt (2,pout);
  }
  return (-1);

}


/* gagrow gets a row of data from the file.  The row of data can
   be 'wrapped' if the x direction of the grid spans the globe.       */


int gagrow ( float *gr, int *d ) {
int fpnt;
int rc,i,x,j;
int y,z,t;


  y = *(d+1);
  z = *(d+2);
  t = *(d+3);

  /* If the needed data is within the bounds of the file dimensions
     then read the data directly.                                     */

  if (pgr->dimmin[0] >= 1 && pgr->dimmax[0] <= pfi->dnum[0]) {
    rc = garrow (pgr->dimmin[0], y, z, t,
                      (pgr->dimmax[0]-pgr->dimmin[0]+1), gr);
    if (rc != 0 ) return (1);
    return (0);
  }

  /* If the file does not wrap, then read the data directly, if
     possible.  If the requested data lies outside the file's bounds,
     fill in with missing data where appropriate.                   */

  if (!pfi->wrap) {
    if ( pgr->dimmin[0]>=1 && pgr->dimmax[0]<=pfi->dnum[0] ) {
      rc = garrow (pgr->dimmin[0], y, z, t,
                    (pgr->dimmax[0]-pgr->dimmin[0]+1), gr);
      if (rc != 0 ) return (1);
      return (0);
    }

    for (i=0; i<pgr->isiz; i++) *(gr+i) = pgr->undef;
    if (pgr->dimmin[0]<1 && pgr->dimmax[0]<1 ) return (-1);
    if (pgr->dimmin[0]>pfi->dnum[0] &&
        pgr->dimmax[0]>pfi->dnum[0] ) return (-1);
    i = 1 - pgr->dimmin[0];
    if (i>0) gr+=i;
    i = 1;
    if (pgr->dimmin[0]>1) i = pgr->dimmin[0];
    j = pgr->dimmax[0];
    if (j > pfi->dnum[0]) j = pfi->dnum[0];
    j = 1 + (j - i);
    rc = garrow (i, y, z, t, j, gr);
    if (rc != 0 ) return (1);
    return (0);
  }

  /* When the file wraps, we read the entire row into the row buffer, and
     copy the values as needed into locations in the requested row.    */
  rc = garrow (1, y, z, t, pfi->dnum[0], pfi->rbuf);
  if (rc != 0 ) return (1);

  for (x=pgr->dimmin[0];x<=pgr->dimmax[0];x++) {
    i=x;
    while (i<1) i = i + pfi->dnum[0];
    while (i>pfi->dnum[0]) i = i-(pfi->dnum[0]);    /* Best way??? */
    *gr = *((pfi->rbuf)+i-1);
    gr++;
  }
  return (0);
}

int gafcor ( int x, int y, int z, int t) {
int pos;
int yy,zz;

/*mf........mf*/

int levs;
  levs=pvr->levels;
  if(levs == 0) levs=1;

/*mf........mf*/

  if (pfi->tlpflg) {
    t = t + pfi->tlpst;
    if (t > pfi->dnum[3]) t = t - pfi->dnum[3];
  }

  if (pfi->yrflg) yy = pfi->dnum[1] - y;
  else yy = y-1;

  if (pfi->zrflg) {
    if (pvr->levels==0) zz=0;
    else zz = pvr->levels-z;
  } else zz = z-1;

/*mf.........mf*/

  if(pvr->var_t) {

    pos = (t-1)*(pfi->gsiz)*levs +
      pvr->offset +
	zz*(pfi->gsiz) +
	  yy*(pfi->dnum[0]) +
	    (x-1);

  } else {

    pos = (t-1)*(pfi->tsiz) +
      pvr->offset +
	zz*(pfi->gsiz)*(pvr->var_z) +
	  yy*(pfi->dnum[0]) +
	    (x-1);

  }


  if (pfi->xyhdr) pos = pos + pfi->xyhdr;
  if (pfi->thdr) pos = pos + pfi->thdr;

/*mf..........mf*/

  return (pos);
}

/*mf..........mf*/

int gafcyx ( int x, int y, int z, int t) {
int pos;
int yy,zz;

  if (pfi->tlpflg) {
    t = t + pfi->tlpst;
    if (t > pfi->dnum[3]) t = t - pfi->dnum[3];
  }

  if (pfi->yrflg) yy = pfi->dnum[0] - y;
  else yy = y-1;

  if (pfi->zrflg) {
    if (pvr->levels==0) zz=0;
    else zz = pvr->levels-z;
  } else zz = z-1;

  if(pvr->var_t) {

    pos = (t-1)*(pfi->gsiz)*(pvr->levels) +
      pvr->offset +
	zz*(pfi->gsiz) +
	  (x-1)*(pfi->dnum[1]) +
	    yy;

  } else {

    pos = (t-1)*(pfi->tsiz) +
      pvr->offset +
	zz*(pfi->gsiz)*(pvr->var_z) +
	  (x-1)*(pfi->dnum[1]) +
	    yy;

  }

  if (pfi->xyhdr) pos = pos + pfi->xyhdr;
  if (pfi->thdr) pos = pos + pfi->thdr;

  return (pos);
}
/*mf..........mf*/

/*mf..........mf*/
int gardyx (int fpos, int len, float *gr) {
char *ch1,*ch2,*ch3,*ch4,cc1,cc2;
int rc,i,j,pos;

pos = fpos;

for (i=1;i<=len;i++) {

  rc = fseek (pfi->infile, pos*sizeof(float)+pfi->fhdr, 0);
  if (rc!=0) {
    gaprnt (0,"Low Level I/O Error:  Seek error on data file \n");
    sprintf (pout,"  Data file name = %s \n",pfi->name);
    gaprnt (0,pout);
    sprintf (0,"  Error occurred when seeking to byte %li \n",fpos);
    gaprnt (0,pout);
    return (1);
  }
  rc = fread (gr, sizeof(float), 1, pfi->infile);
  if (rc<1) {
    gaprnt (0,"Low Level I/O Error:  Read error on data file \n");
    sprintf (pout,"  Data file name = %s \n",pfi->name);
    gaprnt (0,pout);
    sprintf (pout,"  Error reading %i bytes at location %li \n",
	     len, fpos);
    gaprnt (0,pout);
    return (1);
  }

  /* Do byte swapping if needed */

  if (pfi->bswap) {
    ch1 = (char *)gr;
    ch2 = ch1+1;
    ch3 = ch2+1;
    ch4 = ch3+1;
    for (i=0; i<len; i++) {
      cc1 = *ch1;
      cc2 = *ch2;
      *ch1 = *ch4;
      *ch2 = *ch3;
      *ch3 = cc2;
      *ch4 = cc1;
      ch1+=4; ch2+=4; ch3+=4; ch4+=4;
    }
  }

  /* Set missing data values to exact value if specified */

  if (SETMISS) {
    for (j=0;j<len;j++) {
      if (*gr > pfi->ulow && *gr < pfi->uhi)  *gr = pgr->undef;
    }
  }

  gr++;
  pos=pos+pfi->dnum[1];

}

return (0);

}
/*mf..........mf*/

int garrow (int x, int y, int z, int t, int len, float *gr) {
int rc,i=0,tt,oflg;
int fpos;

  tt = t;
  if (pfi->tmplat) {
    tt = gaopfn(t,&oflg);
    if (tt==-99999) return(1);
    if (tt==-88888) {
      for (i=0; i<len; i++) *(gr+i) = pfi->undef;
      return (0);
    }
    if (oflg) bpsav = -999;  /* Force new bit map cache if new file opened */
  }

  if (pfi->ppflag) {
    rc = gaprow (x, y, z, t, tt, len, gr);
    return (rc);
  }

  if (pfi->idxflg) {
    rc = gairow (x, y, z, t, i, len, gr);
    return (rc);
  }

/*mf...........mf*/
  if(pvr->y_x) {
    fpos = gafcyx (x,y,z,tt);
    rc = gardyx (fpos, len, gr);
  } else {
    fpos = gafcor (x,y,z,tt);
    rc = garead (fpos, len, gr);
  }
/*mf...........mf*/

  return (rc);
}

int garead (int fpos, int len, float *gr) {
char *ch1,*ch2,*ch3,*ch4,cc1,cc2;
unsigned char *uch1,*uch2,ucc1,ucc2;
int rc,i;
/*mf...........mf*/
int j,cnt,ival,*ig;
float *fg;

unsigned char *igr;
unsigned char *cgr;
signed char chsign;
unsigned char chunsign;

  if(pvr->dfrm == 1) {
    rc = fseek (pfi->infile, fpos*sizeof(char)+pfi->fhdr, 0);
  } else if(pvr->dfrm == 2 || pvr->dfrm == -2 ) {
    rc = fseek (pfi->infile, fpos*2 + pfi->fhdr, 0);
  } else if(pvr->dfrm == 4) {
    rc = fseek (pfi->infile, fpos*sizeof(int)+pfi->fhdr, 0);
  } else if(pfi->cray_ieee) {
    rc = fseek (pfi->infile, fpos*4+pfi->fhdr, 0);
  } else {
    rc = fseek (pfi->infile, fpos*sizeof(float)+pfi->fhdr, 0);
  }

/*mf...........mf*/


  if (rc!=0) {
    gaprnt (0,"Low Level I/O Error:  Seek error on data file \n");
    sprintf (pout,"  Data file name = %s \n",pfi->name);
    gaprnt (0,pout);
    sprintf (0,"  Error occurred when seeking to byte %li \n",fpos);
    gaprnt (0,pout);
    return (1);
  }


/*mf...........mf*/
  if(pvr->dfrm == 1) {

    j = len*sizeof(char);
    igr = (unsigned char *)malloc(len*sizeof(char));
    if (igr==NULL) {
      gaprnt (0,"Memory Allocation Error:  char grid storage \n");
      return (1);
    }

    rc = fread (igr, 1, len, pfi->infile);
    for(i=0;i<len;i++) {
      *(gr+i) = (float)(*(igr+i));
    }
    free(igr);

/*mf 961127 handle integer*2 for NCEP CPC  does byteswapping too mf*/

  } else if(pvr->dfrm == 2 || pvr->dfrm == -2 ) {

    j = len*2;
    cgr = (unsigned char *)malloc(len*2);
    if (cgr==NULL) {
      gaprnt (0,"Memory Allocation Error:  integer*2 storage \n");
      return (1);
    }

    rc = fread (cgr, 2, len, pfi->infile);
    cnt=0;

/*mf - 961127 - byteswapping mf*/

    if(pfi->bswap) {
      uch1 = cgr;
      uch2 = uch1+1;
      for (i=0; i<len; i++) {
	ucc1 = *uch1;
	ucc2 = *uch2;
	*uch1 = ucc2;
	*uch2 = ucc1;
	uch1+=2; uch2+=2;
      }
    }

/*mf - 961127 - signed integer*2 mf*/

    if(pvr->dfrm == -2) {

      for(i=0;i<len;i++) {
	ival=(int)(*(cgr+cnt)*256) + (int)((*(cgr+cnt+1))) - 65536*(*(cgr+cnt)>>7);
	/* printf("i2 %d %d %d %d %d %d\n",(*(cgr+cnt)>>7),i,cnt,((*(cgr+cnt))),((*(cgr+cnt+1))),ival); */
	*(gr+i) = (float)ival;
	cnt+=2;
      }

/*mf - 961127 - unsigned integer*2 mf*/

    } else {

      for(i=0;i<len;i++) {
	ival=(int)(*(cgr+cnt)*256) + (int)((*(cgr+cnt+1)));
	*(gr+i) = (float)ival;
	cnt+=2;
      }

    }

    free(cgr);

  } else if(pvr->dfrm == 4) {

    j = len*sizeof(int);
    ig = (int *)malloc(len*sizeof(int));
    if (ig==NULL) {
      gaprnt (0,"Memory Allocation Error:  integer*4 storage \n");
      return (1);
    }

    rc = fread (ig, sizeof(int), len, pfi->infile);

    for(i=0;i<len;i++) {
      *(gr+i) = (float)(*(ig+i));
    }
    free(ig);

  } else if(pfi->cray_ieee) {

/*mf 970112 kill off cray fortran convert */

/*
    j = (len+1)/2;
    fg = (float *)malloc(j*sizeof(float));

    rc = fread (fg, sizeof(float), j, pfi->infile);
    if(rc==j) rc=len;
    IEEE2CRAY(fg,gr,len);
    free(fg);
*/

/*mf 970112 -- use wesley ebisuzaki's routines ---mf*/

    cgr = (unsigned char *)malloc(len*4);
    if (cgr==NULL) {
      gaprnt (0,"Memory Allocation Error:  32-bit grid storage on cray \n");
      sprintf (pout,"len*4 = %d\n",len*4);
      gaprnt (0,pout);
      return (1);
    }

    rc = fread (cgr, 4, len, pfi->infile);
    if (rc<len) {
      gaprnt (0,"Low Level I/O Error:  Read error on 32-bit grid data file on cray \n");
      sprintf (pout,"  Data file name = %s \n",pfi->name);
      gaprnt (0,pout);
      sprintf (pout,"  Error reading %i bytes at location %li \n",
	       len, fpos);
      gaprnt (0,pout);
      free(cgr);
      return (1);
    }

    for(i=0;i<len;i++) {

      if (pfi->bswap) {
	ch1 = (char *)cgr;
	ch2 = ch1+1;
	ch3 = ch2+1;
	ch4 = ch3+1;
	cc1 = *ch1;
	cc2 = *ch2;
	*ch1 = *ch4;
	*ch2 = *ch3;
	*ch3 = cc2;
	*ch4 = cc1;
      }

      *(gr+i) = ieee2flt(cgr);
      cgr+=4;

    }

    free(cgr);

  } else {

/*---
  standard direct access read
---*/

      rc = fread (gr, sizeof(float), len, pfi->infile);

  }

/*mf...........mf*/

  if (rc<len) {
    gaprnt (0,"Low Level I/O Error:  Read error on data file \n");
    sprintf (pout,"  Data file name = %s \n",pfi->name);
    gaprnt (0,pout);
    sprintf (pout,"  Error reading %i bytes at location %li \n",
            len, fpos);
    gaprnt (0,pout);
    return (1);
  }

  /* Do byte swapping if needed, only if !CRAY  and !cray_ieee*/

  if (pfi->bswap && !GRADS_CRAY && ( abs(pvr->dfrm) != 2 ) && ( pvr->dfrm != 1 ) && !pfi->cray_ieee ) {
    ch1 = (char *)gr;
    ch2 = ch1+1;
    ch3 = ch2+1;
    ch4 = ch3+1;
    for (i=0; i<len; i++) {
      cc1 = *ch1;
      cc2 = *ch2;
      *ch1 = *ch4;
      *ch2 = *ch3;
      *ch3 = cc2;
      *ch4 = cc1;
      ch1+=4; ch2+=4; ch3+=4; ch4+=4;
    }
  }

  /* Set missing data values to exact value if specified */

  if (SETMISS) {
    for (i=0;i<len;i++) {
      if (*gr > pfi->ulow && *gr < pfi->uhi)  *gr = pgr->undef;
      gr++;
    }
  }

  return (0);
}

/* Handle a station data request */

int gagstn (struct gastn *stn) {
  struct garpt *rpt;
  struct stninf sts;
  struct rpthdr ghdr, *hdr;
  int i,k,rc,ii,flag,tim,fnum,rtot,rdw,nsiz;
  int fpos,dpos,selflg,oflg;
  int sizhdrf,sizhdrd,sizf,sizd,idum;
  float lnmin,lnmax,ltmin,ltmax,hlon;
  char ch1[16],ch2[16],*ch;
  char rec[256];

  stn->rpt = NULL;
  for (i=0; i<BLKNUM; i++) {
    stn->blks[i] = NULL;
  }

  /* Determine cache situation */

  if (scbuf==NULL && !scerr) {
    scbuf = (char *)malloc(SCNUM);
    if (scbuf==NULL) {
      gaprnt (0,"Memory allocation error:  Stn data cache buffer\n");
      gaprnt (0,"  Station data cache disabled\n");
      scerr = 1;
    }
    scflg = 0;
  }

  scuca = 0;
  scpnt = 0;
  if (!scerr) scok = 1;
  if (scflg && scstn.pfi==stn->pfi && scstn.idim==stn->idim &&
      scstn.jdim==stn->jdim && scstn.tmin==stn->tmin &&
      scstn.tmax==stn->tmax && scstn.rflag==stn->rflag &&
      scstn.sflag==stn->sflag) {
    rc = 1;
    if (stn->rflag && scstn.radius!=stn->radius) rc = 0;
    if (stn->sflag) {
      for (i=0; i<8; i++) if (scstn.stid[i]!=stn->stid[i]) rc=0;
    } else {
      if (scstn.dmin[0]!=stn->dmin[0]) rc = 0;
      if (scstn.dmin[1]!=stn->dmin[1]) rc = 0;
      if (scstn.dmax[0]!=stn->dmax[0]) rc = 0;
      if (scstn.dmax[1]!=stn->dmax[1]) rc = 0;
    }
    if (rc) {
      scuca = 1;
      scok = 0;
    }
  }

  pvr = stn->pvar;
  pfi = stn->pfi;
  hdr = &ghdr;

  lnmin = stn->dmin[0]; lnmax = stn->dmax[0];
  ltmin = stn->dmin[1]; ltmax = stn->dmax[1];
  if (stn->rflag) {
    lnmin = lnmin - stn->radius;
    lnmax = lnmax + stn->radius;
    ltmin = ltmin - stn->radius;
    ltmax = ltmax + stn->radius;
  }
  stn->rnum = 0;

/*mf---
  set size of the file and data hdr
  ---mf*/

  sizhdrf = sizeof(struct rpthdr);
  sizhdrd = sizeof(struct rpthdr);

  if(pfi->cray_ieee) {
    sizhdrf=8+((sizeof(struct rpthdr)-8)/2);
  }

  /* Loop through times looking for appropriate stations */

  for (tim=stn->tmin; tim<=stn->tmax; tim++) {

    if (tim<1) continue;
    if (tim > pfi->dnum[3]) break;

    if (!scuca && pfi->tmplat) {
      rc = gaopfn(tim,&oflg);
      if (rc==-99999) goto err;
      if (rc==-88888) {
        if (scok) {
          hdr->nlev = 0;
          gacstn((char *)hdr, NULL, 0, sizhdrd);
        }
        continue;
      }
    }

    /* Loop through stations for this time looking for valid reports */

    if (!scuca) {
      fpos = *(pfi->tstrt+tim-1);
      rc = gasstn(fpos);
      if (rc) goto err;
    }

    while (1) {

/*----------------------

  get the header

-----------------------*/

      if (scuca) {                             /* from the cache */
	gagcst (sizhdrd, (char *)hdr);
      } else {                                 /* from the file */
        if (pfi->seqflg) {
          rc = garstn(4,(char *)(&rdw),fpos);  /*mf changed 4 to sizeof(int) */
          if (rc) goto err;
          if (pfi->bswap) gabswp((float *)(&rdw),1);
/*mf --- mf*/
	  if(pfi->cray_ieee) {
	    idum=be_int2int((unsigned char*)(&rdw));
	    rdw=idum;
/*mf --- mf*/
	  }

        }

        rc = garstn (sizhdrf, (char *)hdr, fpos);
        if (rc) goto err;
        if (pfi->bswap) gahswp(hdr);

/*mf --- mf*/
	if(pfi->cray_ieee) {
	  memcpy(rec,hdr,sizhdrf);
	  rc=cvhdr((unsigned char *)rec,hdr);
	}
/*mf --- mf*/

      }

      if (hdr->nlev==0) break;   /* END OF DATA CHECK */
/*----------------------

  Determine if we want to read the data portion of this  report

-----------------------*/

      selflg = 1;
      if (stn->sflag) {
        getwrd (ch1,hdr->id,8);
        lowcas(ch1);
        getwrd(ch2,stn->stid,8);
        if (!cmpwrd(ch1,ch2)) selflg = 0;
      } else {
        hlon = hdr->lon;
        if (hlon<lnmin) hlon+=360.0;
        if (hlon>lnmax) hlon-=360.0;
        if (hlon<lnmin || hlon>lnmax ||
            hdr->lat<ltmin || hdr->lat>ltmax ) selflg=0;
        if (selflg && stn->rflag &&
            hypot(hlon-stn->dmin[0],hdr->lat-stn->dmin[1])>stn->radius) {
          selflg = 0;
        }
      }

      /* Determine size of the data portion of this report */

      if (hdr->flag) {
	fnum = (hdr->nlev-1) * (pfi->lvnum+1) + pfi->ivnum;
      } else {
	fnum =  hdr->nlev * (pfi->lvnum+1);
      }

/* --- calc size of floating point data section in the FILE not the machine --- */

      sizd=fnum*sizeof(float);
      if(pfi->cray_ieee) {
	sizf= fnum*4;
      } else {
	sizf=fnum*sizeof(float);
      }

/*------------------------------

  Read the data portion of this report, byteswap it if needed,
  and set exact missing data values if specified.

------------------------------*/

      if (selflg) {

        if (scuca) {                                  /* from the cache */
	  gagcst (sizd, (char *)pfi->rbuf);
        }else {                                       /* from the file */

          if (pfi->seqflg) {
            ch = (char *)(pfi->rbuf);
            nsiz = rdw - sizhdrf;
            if (nsiz>0) {
              rc = garstn(nsiz,ch,fpos);
              if (rc) goto err;
              ch += nsiz;
            }
            rtot = rdw;
            nsiz = sizf + sizhdrf;
            while (rtot<=nsiz) {
              fpos = fpos + rdw + 8;
              rc = gasstn(fpos);
              if (rc) goto err;
              if (rtot==nsiz) break;
              rc = garstn(4,(char *)(&rdw),fpos);  /*mf changed 4 to sizeof(int) */
              if (rc) goto err;
              if (pfi->bswap) gabswp((float *)(&rdw),1);
/*mf --- mf*/
	      if(pfi->cray_ieee) {
		idum=be_int2int((unsigned char*)(&rdw));
		rdw=idum;
	      }
/*mf --- mf*/
              rtot +=rdw;
              if (rtot>nsiz) break;
              rc = garstn(rdw,ch,fpos);

              if (rc) goto err;
	      idum=rdw*2;
              ch += idum;
            }
            if (rtot>nsiz) {
              gaprnt (0,"Low Level I/O Error:  Sequential read error\n");
              gaprnt (0,"  Record size exceeds report size\n");
              sprintf (pout,"  Data file name = %s \n",pfi->name);
              gaprnt (0,pout);
              goto err;
            }

/*---
  normal read -- NON sequential
---*/

          } else {
            rc = garstn (sizf, (char *)pfi->rbuf, fpos+sizhdrf);
            fpos = fpos + sizf + sizhdrf;
            if (rc) goto err;
          }

          if (pfi->bswap) gabswp(pfi->rbuf,fnum);


/*mf --------------- convert to float on cray ------ mf*/
	  if(pfi->cray_ieee) {
	    rc=cvflt((unsigned char*)pfi->rbuf,fnum);
	  }
/*mf --------------- convert to float on cray ------ mf*/

          if (SETMISS) {
            for (i=0; i<fnum; i++) {
              if ((*(pfi->rbuf+i) > pfi->ulow) && (*(pfi->rbuf+i) < pfi->uhi))
                 *(pfi->rbuf+i) = pfi->undef;
            }
          }
        }

        /* Check the data portion for any matches.  */

        rc = gaglvs (tim,hdr,stn);
        if (rc) goto err;

        /* Cache this report if appropriate */

        if (scok) gacstn((char *)hdr,(char *)pfi->rbuf,sizd,sizhdrd);

/*----
   Skip the data portion of this report.
---*/

      } else {

        if (scuca) {
          gaprnt (0,"Logic Error 8 in gaio\n");
          goto err;
        }
        if (pfi->seqflg) {
          rtot = rdw;
          sizf += sizhdrf;
          while (rtot<=sizf) {
            fpos = fpos + rdw + 8;
            rc = gasstn(fpos);
            if (rc) goto err;
            if (rtot==sizf) break;
            rc = garstn(4,(char *)(&rdw),fpos);    /*mf changed 4 to sizeof(int) */
            if (rc) goto err;
            if (pfi->bswap) gabswp((float *)(&rdw),1);
/*mf --- mf*/
	      if(pfi->cray_ieee) {
		idum=be_int2int((unsigned char*)(&rdw));
		rdw=idum;
	      }
/*mf --- mf*/
            rtot +=rdw;
          }
          if (rtot>sizf) {
            gaprnt (0,"Low Level I/O Error:  Sequential read error\n");
            gaprnt (0,"  Record size exceeds report size\n");
            sprintf (pout,"  Data file name = %s \n",pfi->name);
            gaprnt (0,pout);
            goto err;
          }
        } else {
          fpos = fpos + sizf + sizhdrf;
          rc = gasstn(fpos);
          if (rc) goto err;
        }


      }  /* END OF if (scuca) -- use the cache or not */


    }    /* END OF  while (1) */


    if (scok) {
      hdr->nlev = 0;
      gacstn((char *)hdr, NULL, 0,sizhdrd);
    }
  }
  if (scok) {
    scflg = 1;
    scstn = *stn;
  } else scflg = 0;
  if (scuca) scflg = 1;
  return (0);

err:
  for (i=0; i<BLKNUM; i++) {
    if (stn->blks[i] != NULL) free (stn->blks[i]);
  }
  return (1);
}

/* Select appropriate variable and levels from report, and chain
   them off the stn block.  */

int gaglvs (int tim, struct rpthdr *hdr, struct gastn *stn) {
struct garpt *rpt;
float *vals,*pval;
int i,k,voff,mlev;

  vals = pfi->rbuf;
  voff = pvr->offset;
  if (pvr->levels==0) {
    if (hdr->flag) {
      pval = vals+voff;
      rpt = gaarpt (stn);
      if (rpt==NULL) return(1);
      rpt->lat = hdr->lat;
      rpt->lon = hdr->lon;
      rpt->lev = -9.99e33;
      rpt->tim = tim + hdr->t;
      rpt->val = *pval;
      for (k=0; k<8; k++) *(rpt->stid+k) = *(hdr->id+k);
      stn->rnum++;
    }
  } else {
    if (hdr->flag) vals = vals + pfi->ivnum;
    mlev = hdr->nlev;
    if (hdr->flag) mlev--;
    for (i=0; i<mlev; i++) {
      pval = vals+(i*(pfi->lvnum+1));
      if (stn->dmax[2]==stn->dmin[2]) {
        if (fabs(*pval-stn->dmin[2])>0.01) continue;
      } else {
        if (*pval<stn->dmax[2] || *pval>stn->dmin[2]) continue;
      }
      rpt = gaarpt (stn);
      if (rpt==NULL) return(1);
      rpt->lat = hdr->lat;
      rpt->lon = hdr->lon;
      rpt->lev = *pval;
      rpt->tim = tim + hdr->t;
      rpt->val = *(pval+voff+1);
      for (k=0; k<8; k++) *(rpt->stid+k) = *(hdr->id+k);
      stn->rnum++;
    }
  }
  return (0);
}

/* Allocate a rpt structure, return pointer to allocated buffer.
   On the first request, stn->rpt should be set to NULL. */

struct garpt *gaarpt (struct gastn *stn) {
struct garpt *rpt;
int i;

  /* First time through, define the static variables. */

  if (stn->rpt == NULL) {
    stn->prev = &(stn->rpt);
    for (i=0; i<BLKNUM; i++) {
      stn->blks[i] = NULL;
    }
    stn->rptcnt = RPTNUM;    /* Force new block allocation */
    stn->blkcnt = -1;
  }

  stn->rptcnt++;
  rpt = stn->crpt;

  if (stn->rptcnt>=RPTNUM) {
    stn->blkcnt++;
    if (stn->blkcnt==BLKNUM) {
      printf ("Out of memory blocks to allocate \n");
      return(NULL);
    }
    rpt = (struct garpt *)malloc(sizeof(struct garpt)*RPTNUM);
    if (rpt==NULL) {
      printf ("Couldn't allocate memory for stn block \n");
      return(NULL);
    }
    stn->blks[stn->blkcnt] = rpt;
    stn->rptcnt = 0;
  } else rpt++;

  *(stn->prev) = rpt;
  stn->prev = &(rpt->rpt);
  rpt->rpt = NULL;
  stn->crpt = rpt;
  return(rpt);
}

void gacstn (char *hdr, char *rdat, int siz, int sizhdr) {
int i;
  if (scpnt+sizhdr*2+siz+10 > SCNUM) {
    scok = 0;
  } else {
    for (i=0; i<sizhdr; i++) *(scbuf+scpnt+i) = *(hdr+i);
    scpnt += sizhdr;
    if (siz>0) {
      for (i=0; i<siz; i++) *(scbuf+scpnt+i) = *(rdat+i);
      scpnt += siz;
    }
  }
}

/* Return info from the station data cache */

void gagcst (int siz, char *ch) {
int i;
  for (i=0; i<siz; i++) *(ch+i) = *(scbuf+scpnt+i);
  scpnt += siz;
}

/* Seek to specified location in a station data file */

int gasstn (int fpos) {
int rc;

  rc = fseek (pfi->infile, fpos, 0);
  if (rc!=0) {
    gaprnt (0,"Low Level I/O Error:  Seek error on data file \n");
    sprintf (pout,"  Data file name = %s \n",pfi->name);
    gaprnt (0,pout);
    sprintf (pout,"  Error occurred when seeking to byte %li \n",fpos);
    gaprnt (0,pout);
    return (1);
  }
  return (0);
}

/* Read specified amount of data from a station data file */

int garstn (int siz, char *val, int fpos) {
int rc;

  rc = fread (val, siz, 1, pfi->infile);
  if (rc<1) {
    gaprnt (0,"Low Level I/O Error:  Read error on data file \n");
    sprintf (pout,"  Data file name = %s \n",pfi->name);
    gaprnt (0,pout);
    sprintf (pout,"  Error reading %i bytes at location %li \n",
            siz, fpos);
    gaprnt (0,pout);
    return (1);
  }
  return (0);
}

/*  Obtain user requested grid from defined variable */

int gagdef (void) {
int id, jd, i, flag;
int ys,zs,ts,siz,pos;
int d[4],d1min,d1max,d2min,d2max,xt,yt;
float *v;

  /* If a dimension is a fixed dimension in the defined
     variable, it must be a fixed dimension in the output
     grid.  */

  id = pgr->idim;
  jd = pgr->jdim;
  if (jd>-1) {
    if (pfi->dnum[jd]==1) {
      jd = -1;
      pgr->jdim = -1;
      pgr->jsiz = -1;
    }
  }
  if (id>-1) {
    if (pfi->dnum[id]==1) {
      id = jd;
      pgr->idim = pgr->jdim;
      pgr->isiz = pgr->jsiz;
      pgr->igrab = pgr->jgrab;
      pgr->iabgr = pgr->jabgr;
      pgr->ivals = pgr->jvals;
      pgr->iavals = pgr->javals;
      pgr->ilinr = pgr->jlinr;
      jd = -1;
      pgr->jdim = -1;
      pgr->jsiz = 1;
    }
  }

  /* Set up constants for array subscripting */

  ys = pfi->dnum[0];
  zs = ys * pfi->dnum[1];
  ts = zs * pfi->dnum[2];

  /* Set up dimension ranges */

  for (i=0; i<4; i++) d[i] = pgr->dimmin[i] - pfi->dimoff[i] - 1;
  for (i=0; i<4; i++) if (pfi->dnum[i]==1) d[i] = 0;
  if (id>-1) {
    d1min = d[id];
    d1max = pgr->dimmax[id] - pfi->dimoff[id] - 1;
  }
  if (jd>-1) {
    d2min = d[jd];
    d2max = pgr->dimmax[jd] - pfi->dimoff[jd] - 1;
  }

  /* Get storage for output grid */

  pgr->isiz = 1;
  pgr->jsiz = 1;
  if (id>-1) pgr->isiz = 1 + d1max - d1min;
  if (jd>-1) pgr->jsiz = 1 + d2max - d2min;
  siz = pgr->isiz * pgr->jsiz;
  if (siz>1) {
    pgr->grid = (float *)malloc(sizeof(float)*siz);
    if (pgr->grid==NULL) {
      gaprnt (0,"Memory Allocation Error: Grid Request\n");
      return (2);
    }
  } else {
    pgr->grid = &(pgr->rmin);
  }

  /* Normalize time coordinate if not varying */
  /* This does not handle leap years properly!!!!  Gotta fix this
     someday */

  if (pfi->climo && id!=3 && jd!=3) clicyc(d+3);

  /* Check for entirely undefined grid */

  flag = 0;
  for (i=0; i<4; i++) {
    if (i!=id && i!=jd && (d[i]<0 || d[i]>=pfi->dnum[i])) flag = 1;
  }
  if (flag) {
    for (i=0; i<siz; i++) *(pgr->grid+i) = pfi->undef;
    return (0);
  }

  /* Move appropriate grid values */

  if (id==-1 && jd==-1) {
    pos = d[0] + d[1]*ys + d[2]*zs + d[3]*ts;
    pgr->rmin = *(pfi->rbuf+pos);
    return (0);
  }

  v = pgr->grid;

  if (jd==-1) {
    for (xt=d1min; xt<=d1max; xt++) {
      d[id] = xt;
      if (id==3 && pfi->climo) clicyc(d+3);
      if (d[id]<0 || d[id]>=pfi->dnum[id]) *v = pfi->undef;
      else {
        pos = d[0] + d[1]*ys + d[2]*zs + d[3]*ts;
        *v = *(pfi->rbuf+pos);
      }
      v++;
    }
    return (0);
  }

  for (yt=d2min; yt<=d2max; yt++) {
    d[jd] = yt;
    if (jd==3 && pfi->climo) clicyc(d+3);
    for (d[id]=d1min; d[id]<=d1max; d[id]++) {
      if (d[jd]<0 || d[jd]>=pfi->dnum[jd] ||
          d[id]<0 || d[id]>=pfi->dnum[id]) *v = pfi->undef;
      else {
        pos = d[0] + d[1]*ys + d[2]*zs + d[3]*ts;
        *v = *(pfi->rbuf+pos);
      }
      v++;
    }
  }
  return(0);
}

void clicyc (int *ti) {
struct dt dtim,otim;
float tt;

  if (pfi->climo>0) {
    while (*ti>pfi->cysiz-1) *ti = *ti - pfi->cysiz;
    while (*ti<0) *ti = *ti + pfi->cysiz;
  }
}

/* Fill in grid for predefined variable */

int gagpre (void) {
float (*conv)(float *, float);
int d[4],id,jd,i,j,dim;
float *gr,*vals,t;

  id = pgr->idim;
  jd = pgr->jdim;
  for (i=0; i<4; i++) d[i] = pgr->dimmin[i];

  dim = pvr->offset;
  conv = pfi->gr2ab[dim];
  vals = pfi->grvals[dim];

  gr = pgr->grid;

  if (id>-1 && jd>-1) {
    for (d[jd]=pgr->dimmin[jd]; d[jd]<=pgr->dimmax[jd]; d[jd]++) {
      for (d[id]=pgr->dimmin[id]; d[id]<=pgr->dimmax[id]; d[id]++) {
        t = (float)(d[dim]);
        *gr = conv(vals, t);
        gr++;
      }
    }
  } else if (id>-1) {
    for (d[id]=pgr->dimmin[id]; d[id]<=pgr->dimmax[id]; d[id]++) {
      t = (float)(d[dim]);
      *gr = conv(vals, t);
      gr++;
    }
  } else {
    t = (float)(d[dim]);
    *gr = conv(vals, t);
  }
  return (0);
}


/* Read index data, in this case GRIB type data.
   Currently assumes no pole point, and only one record
   per grid.  */

int gairow (int x, int y, int z, int t, int offset, int len, float *gr) {
int irec,ioff,bstrt,bend,blen,cstrt,cend,clen,rc;
int ival,i,fpos,yy,bpos,boff,siz,gtyp,xsiz,ysiz;
float dsf,bsf,ref;

  /* Figure out position and length of the I/O */

  gtyp = *(pindx->hipnt+3);
  irec = (t-1)*pfi->trecs + pvr->recoff + z - 1;
  if (pfi->ppflag) {xsiz = pfi->ppisiz; ysiz = pfi->ppjsiz;}
  else  {xsiz = pfi->dnum[0]; ysiz = pfi->dnum[1];}
  if (gtyp==29) {
    xsiz = 145;
    irec = irec*6;
    if (y<37) y--;
    else {irec+=3; y-=37; }
    yy = y;
  } else {
    irec = irec*3;
    if (pfi->yrflg) yy = ysiz - y;
    else yy = y-1;
  }
  if (pfi->ppflag) ioff = offset;
  else ioff = yy*xsiz + x - 1;
  boff = ioff;
  blen = *(pindx->intpnt + irec + 2);
  if (blen<0) {
    for (i=0; i<len; i++) *(gr+i) = pfi->undef;
    return (0);
  }
  bpos = *(pindx->intpnt + irec + 1);
  dsf = *(pindx->fltpnt+irec);
  bsf = *(pindx->fltpnt+irec+1);
  ref = *(pindx->fltpnt+irec+2);
  if (bpos>-900 && bpos!=bpsav) {
    bpsav = bpos;
    siz = 2+(xsiz*ysiz)/8;
    if (siz>bssav) {
      if (bcflag) {
        free(bcache);
        free(bpcach);
      }
      bcache = (char *)malloc(siz);
      bpcach = (int *)malloc(sizeof(int)*(xsiz*ysiz+1));
      if (bcache==NULL||bpcach==NULL) {
        gaprnt(0,"Memory Allocation Error During GRIB I/O\n");
        return (1);
      }
      bssav = siz;
      bcflag = 1;
    }
    rc = fseek(pfi->infile, bpos, 0);
    rc = fread(bcache,1,siz,pfi->infile);
    if (rc!=siz) {
      gaprnt(0,"GRIB I/O Error: Bit Map I/O\n");
      return(1);
    }
    boff=1;
    for (i=0; i<xsiz*ysiz; i++) {
      if (gagbb(bcache,i,1)) {
        *(bpcach+i) = boff;
        boff++;
      } else {
        *(bpcach+i) = -1*boff;
      }
    }
    *(bpcach+xsiz*ysiz) = boff;  /* Provide an ending offset */
  }
  if (bpos>-900) {
    boff = *(bpcach+ioff);
    if (boff<0) boff = -1*boff;
    boff--;
    bstrt = blen * boff;
    boff = *(bpcach+ioff+len);
    if (boff<0) boff = -1*boff;
    boff--;
    bend = blen * boff - 1;
  } else {
    bstrt = blen * boff;
    bend = bstrt + blen*len;
  }
  cstrt = bstrt/8;
  cend = bend/8;
  clen = cend-cstrt+2;
  fpos = *(pindx->intpnt+irec);
  rc = gaird(fpos,cstrt,clen,xsiz,ysiz,blen);
  if (rc) return(rc);
  bstrt = bstrt - cstrt*8;
  for (i=0; i<len; i++) {
    if (bpos>-900 && *(bpcach+ioff+i)<0) *(gr+i) = pfi->undef;
    else {
      ival = gagbb(pfi->pbuf,bstrt,blen);
      *(gr+i) = ( ref + (float)ival * bsf )/dsf;
      bstrt += blen;
    }
  }
  return (0);
}

int gaird (int fpos, int cstrt, int clen, int xsiz, int ysiz, int blen) {
int rc,siz,i;
  if (pfi->ppflag && pgr->idim==0 && pgr->jdim==1) {
    if (!cflag) {
      cflag = 1;
      siz = 5 + xsiz*ysiz*blen/8;  /* qqq  Warning:  siz calc does not */
                                   /* qqq  take into account bms!!! */
      cache = (char *)malloc(siz);
      if (cache==NULL) {
        gaprnt(0,"GRIB Memory Allocation Error\n");
        return (1);
      }
      rc = fseek (pfi->infile, fpos, 0);
      rc = fread(cache,sizeof(char),siz,pfi->infile);
/*
      if (rc!=siz) {
        gaprnt(0,"GRIB I/O Error\n");
        return (1);
      }  */
      if (rc==0) {
        sprintf (pout,"GRIB I/O Error reading %i bytes at %i\n",siz,fpos);  /* xxx */
        gaprnt (0,pout);
        gaprnt (0,"  File name is: ");
        if (pfi->tempname) gaprnt(0,pfi->tempname);
        else gaprnt(0,pfi->name);
        gaprnt (0,"\n");
        return (1);
      }
    }
    if (cache==NULL) return(1);
    for (i=0; i<clen; i++) {
      *(pfi->pbuf+i) = *(cache+cstrt+i);
    }
  } else {
    rc = fseek (pfi->infile, fpos+cstrt, 0);
    rc = fread (pfi->pbuf, sizeof(char), clen, pfi->infile);
    if (rc==0) {
      sprintf (pout,"GRIB I/O Error reading %i bytes at %i\n",clen,fpos+cstrt);
      gaprnt (0,pout);
      gaprnt (0,"  File name is: ");
      if (pfi->tempname) gaprnt(0,pfi->tempname);
      else gaprnt(0,pfi->name);
      gaprnt (0,"\n");
      return(1);
    }
  }
  return(0);
}

/* Routine to open appropriate file when using file templates */
/* Warning -- changes time value to time with respect to this file */

int gaopfn(int t, int *oflg) {
int i;
struct dt dtim, dtimi;
char *fn;

  *oflg = 0;
  if (t<1 || t>pfi->dnum[3]) return(-99999);
  i = *(pfi->fnums+t-1);
  if (i != pfi->fnumc) {
    if (pfi->infile!=NULL) fclose(pfi->infile);
    if (pfi->tempname!=NULL) free(pfi->tempname);
    gr2t(pfi->grvals[3], (float)t, &dtim);
    gr2t(pfi->grvals[3], 1.0, &dtimi);
    fn = gafndt(pfi->name, &dtim, &dtimi, pfi->abvals[3]);
    if (fn==NULL) return (-99999);
    pfi->infile = fopen (fn, "rb");
    pfi->fnumc = i;
    pfi->tempname = fn;
    if (pfi->infile==NULL) {
      if (pfi->errflg && timerr!=t) {
        gaprnt(1,"Warning: Open error, fn = ");
        gaprnt(1,fn);
        gaprnt(1,"\n");
        timerr = t;
      }
      pfi->fnumc = 0;
      return(-88888);
    }
    *oflg = 1;
  }
  t = 1 + t - pfi->fnumc;
  return (t);
}

/* Read in a row of data from a pre-projected grid data set.
   This involves doing interpolation to the lat-lon
   grid */

int gaprow (int x, int y, int z, int t, int tt, int len, float *gr) {
float p[4],dx,dy,g1,g2;
int rc,pos,i,ig,ioff,ix,iy;

  for (i=0; i<len; i++) {
    ig = (y-1) * pfi->dnum[0] + x + i - 1;
    ioff = *(pfi->ppi[0]+ig);
    if (ioff<0) *gr = pgr->undef;
    else {
      dx = *(pfi->ppf[0]+ig);
      dy = *(pfi->ppf[1]+ig);
      pos = (tt-1)*(pfi->tsiz) + pvr->offset + (z-1)*(pfi->gsiz) + ioff;
      if (pfi->idxflg) rc = gairow(x,y,z,t,ioff,2,p);
      else rc = garead(pos,2,p);
      if (rc) return(rc);
      if (pfi->idxflg) rc = gairow(x,y,z,t,ioff+pfi->ppisiz,2,p+2);
      else rc = garead(pos+pfi->ppisiz,2,p+2);
      if (rc) return(rc);
      if (p[0]==pgr->undef || p[1]==pgr->undef ||
          p[2]==pgr->undef || p[3]==pgr->undef) *gr = pgr->undef;
      else {
        g1 = p[0] + (p[1]-p[0])*dx;
        g2 = p[2] + (p[3]-p[2])*dx;
        *gr = g1 + (g2-g1)*dy;
      }
    }
    gr++;
  }
  return(0);
}

/*mf --- convert header --- mf*/

int cvhdr (unsigned char *rec, struct rpthdr *hdr) {
  int rc;
  memcpy(hdr->id,&rec[0],8);
  hdr->lat=ieee2flt(&rec[8]);
  hdr->lon=ieee2flt(&rec[12]);
  hdr->t=ieee2flt(&rec[16]);
  hdr->nlev=be_int2int(&rec[20]);
  hdr->flag=be_int2int(&rec[24]);
  return (0);
}


int cvflt(unsigned char *rec, int fnum) {
  float *val;
  int i;

  val=(float *)malloc(sizeof(float)*fnum);

  for(i=0;i<fnum;i++) {
    *(val+i)=ieee2flt(&rec[i*4]);
  }
  memcpy(rec,val,fnum*sizeof(float));
  free(val);
  return (0);
}
