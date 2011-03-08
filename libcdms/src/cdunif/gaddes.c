/* Copyright (c) 1987-1995 by Brian Doty.  All Rights Reserved.

   See file COPYRIGHT for liability information.

   Permission is granted for usage of source code on an individual
   basis only.  You are in violation of copyright law if you have
   possession of this source code without specific permission from
   Brian Doty.  This source code may not be redistributed in any way,
   in whole or in part.  You may use this source code to:
        - understand GrADs functionality
        - make minor enhancements to the GrADS program
        - correct bugs in the GrADs program
   All other uses ARE PROHIBITED, without specific written permission
   for such use.  You agree that if you make any modifications to the
   source code, you will provide such modifications to Brian Doty for
   possible inclusion into the distributed version of GrADS (attribution
   will be made if requested).  

   $Id: $

*/


#include <stdio.h>
#include <math.h>
#include <ctype.h>
#ifndef __APPLE__
#include <malloc.h>
#endif
#include "grads.h"
#include <string.h>

/*mf 961205 --- expose Mike Fiorino's global struct to these routines for 365 day calandars mf*/
extern struct gamfcmn mfcmn;
/*mf 961205 --- expose Mike Fiorino's global struct to these routines for 365 day calandars mf*/

static char pout[512];
FILE *descr;      /* File descriptor pointer */
static FILE *pdfi;       /* File descriptor for pdef file */

void ll2eg (int , int, float *, float, float, float *, float *, float *) ;
void ll2pse(int, int, float *, float, float, float *, float *);
void ll2ops (float *, float , float , float *, float *) ;


/* Read a GrADS data descriptor file and fill in the information
   into a gafile structure.  The gafile structure should be
   allocated and must be initialized by getpfi.  If this routine
   returns an error, release the pfi structure and allocated
   storage via frepfi.  mflag indicates whether to read the
   stnmap/index file; if this routine is being called to
   preprocess the dd file then this flag should be 0. */

int gaddes (char *name, struct gafile *pfi, int mflag) {
  struct gavar *pvar;
  struct dt tdef,tdefi,dt1,dt2;
  struct gaindx *pindx;
  float *vals;
  int size,rc,len,swpflg,cnt;
  char rec[512], mrec[512], *ch, *dn, *pos, *sname ; /*mf mf*/
  int flgs[8],cflg,i,j,err,hdrb,trlb,mflflg,crayflg=0;
  int mcnt,maxlv,maxct;
  int levs,acum,acumvz,fpos,recacm;
  float temp,v1,v2;
  FILE *mfile;
  char pdefnm[256];
  int pdefop1, pdefop2;

  /*mf .... mf*/
  int nzstride=0,first=1;
  int levsua=0,acumstride=0;
  unsigned char vermap;
  int idum;
  float fdum;
  unsigned char urec[4];
  int diag=0;
  /*mf .... mf*/

  static char *errs[9] = {"XDEF","YDEF","ZDEF","TDEF","UNDEF",
			    "DSET","VARS","TITLE","DTYPE"};

 /*mf --- define here vice grads.c for cdunif.c mf*/
  mfcmn.fullyear=-999; 

  /*
#if GRADS_CDUNIF == 1
   mfcmn.fullyear=-999; 
#endif
*/
  hdrb = 0;
  trlb = 0;
  pdfi = NULL;
  mflflg = 0;			/* map file not open */
  pfi->mfile = NULL;

  mcnt = -1;

  descr = fopen (name, "r");

  /* default suffix of .ctl */

  sname=NULL;
  if (descr == NULL) {
    sname = (char *)malloc(strlen(name)+5);
    if(sname == NULL) {
      gaprnt(0,"malloc error in creating date descriptor file name\n");
      return(1);
    }
    for(i=0;i<=strlen(name);i++) *(sname+i)=*(name+i);
    strcat(sname,".ctl");
    descr = fopen (sname, "r");
  }

  if (descr==NULL) {
    gaprnt (0,"Open Error:  Can't open description file\n");
    return(1);
  }

  if(sname !=NULL) {
    getwrd (pfi->dnam,sname,256);
    free(sname);
  } else {
    getwrd (pfi->dnam,name,256);
  }

  /* initialize error flags */

  for (i=0;i<8;i++) flgs[i] = 1;

  /*mf initialize the calendar to standard and cray_ieee to 0 mf*/

  pfi->calendar=0;
  pfi->cray_ieee=0;

/*mf---------mf*/

  /* parse the dd  file */

  while (fgets(rec,512,descr)!=NULL) {
    strcpy (mrec,rec);
    lowcas(rec);

    if ( (cmpwrd("*",rec)) || !isalnum(rec[0]) ) {
      cflg = 1;

    } else if (cmpwrd("byteswapped",rec)) {
      pfi->bswap = 1;


    } else if (cmpwrd("fileheader",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing fileheader length\n");
      } else {
        ch = intprs(ch,&(pfi->fhdr));
        if (ch==NULL) {
          gaprnt (1,"Fileheader record invalid\n");
          pfi->fhdr = 0;
        }
      }


    } else if (cmpwrd("xyheader",rec)) {


      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing xy grid header length\n");
      } else {
        ch = intprs(ch,&(pfi->xyhdr));
        if (ch==NULL) {
	  gaprnt (1,"xy grid header length invalid\n");
	  pfi->xyhdr = 0;
        } else {
	  pfi->xyhdr = pfi->xyhdr / sizeof(float);
	}
      }


    } else if (cmpwrd("theader",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing time chunk header length\n");
      } else {
        ch = intprs(ch,&(pfi->thdr));
        if (ch==NULL) {
          gaprnt (1,"time chunk grid header length invalid\n");
          pfi->thdr = 0;
        } else {
	  pfi->thdr = pfi->thdr / sizeof(float);
	}
      }


    } else if (cmpwrd("format",rec) || cmpwrd("options",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing options keyword\n");
      } else {
        while (ch!=NULL) {
          if (cmpwrd("sequential",ch)) pfi->seqflg = 1;
          else if (cmpwrd("yrev",ch)) pfi->yrflg = 1;
          else if (cmpwrd("zrev",ch)) pfi->zrflg = 1;
          else if (cmpwrd("template",ch)) pfi->tmplat = 1;
          else if (cmpwrd("byteswapped",ch)) pfi->bswap = 1;
          else if (cmpwrd("cray_32bit_ieee",ch) && GRADS_CRAY) pfi->cray_ieee = 1;
          else if (cmpwrd("cray_32bit_ieee",ch) && !GRADS_CRAY) pfi->cray_ieee = 0;
          else if (cmpwrd("365_day_calendar",ch)) pfi->calendar=1;
          else if (cmpwrd("big_endian",ch)) {
	    if (!BYTEORDER) pfi->bswap = 1;
          }
          else if (cmpwrd("little_endian",ch)) {
            if (BYTEORDER) pfi->bswap = 1;
          }
	  else {
	    gaprnt (0,"Open Error:  Data file type invalid\n");
	    goto err9;
          }
          ch = nxtwrd(ch);
        }
      }

    } else if (cmpwrd("trailerbytes",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Trailerbytes record invalid\n");
      } else {
        ch = intprs(ch,&trlb);
        if (ch==NULL) {
          gaprnt (1,"Trailerbytes record invalid\n");
          trlb = 0;
        } else {
          trlb = trlb/4;
        }
      }

    } else if (cmpwrd("headerbytes",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Headerbytes record invalid\n");
      } else {
        ch = intprs(ch,&hdrb);
        if (ch==NULL) {
          gaprnt (1,"Headerbytes record invalid\n");
          hdrb = 0;
        } else {
          hdrb = hdrb/4;
        }
      }

    } else if (cmpwrd("dtype",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing data type\n");
        gaprnt (1,"   Assuming data file type is grid\n");
      }
      if (cmpwrd("station",ch)) {
        pfi->type = 2;
        flgs[0] = 0;
        flgs[1] = 0;
        flgs[2] = 0;

      } else if (cmpwrd("grib",ch)) {
        pfi->idxflg = 1;
        if ( (ch=nxtwrd(ch))!=NULL ) {
          if ( intprs(ch,&(pfi->grbgrd))==NULL) {
            gaprnt (1,"Description file warning: Invalid GRIB option\n");
            pfi->grbgrd = -999;
          }
        }
      } else {
        gaprnt (0,"Open Error:  Data file type invalid\n");
        goto err9;
      }

    } else if (cmpwrd("title",rec)) {
      if ( (ch=nxtwrd(mrec))==NULL ) {
        gaprnt (1,"Description file warning: Missing title string\n");
      } else {
        getstr (pfi->title,ch,79);
        flgs[7] = 0;
      }

    } else if (cmpwrd("dset",rec)) {
      ch = nxtwrd(mrec);
      if (ch==NULL) {
        gaprnt (0,"Open Error:  Data file name is missing\n");
        goto err9;
      }
      if (*ch=='^' || *ch=='$') {
        fnmexp (pfi->name,ch,name);
      } else {
        getwrd (pfi->name,ch,256);
      }
      flgs[5] = 0;

    } else if (cmpwrd("stnmap",rec) || cmpwrd("index",rec)) {

/*mf -----

  map file processing -- gribmap

-----mf*/

      if (cmpwrd("index",rec)) pfi->idxflg = 1;
      ch = nxtwrd(mrec);
      if (ch==NULL) {
        gaprnt (0,"Open Error:  Station or Index Map file name is missing\n");
        goto err9;
      }
      if (*ch=='^' || *ch=='$') {
        fnmexp (pout, ch, name);
      } else {
        getwrd (pout,ch,100);
      }
      len = 0;
      while (*(pout+len)) len++;
      pfi->mnam = (char *)malloc(len+3);
      if (pfi->mnam==NULL) goto err8;
      strcpy (pfi->mnam,pout);
      if (mflag) {
	mfile = NULL;
        mfile = fopen (pout, "rb");
        if (mfile==NULL) {
          gaprnt (0,"Open Error:  Can't open Station/Index map file ");
          gaprnt (0,pout);
          gaprnt (0,"\n");
          goto err9;
        }


        mflflg = 1;
        swpflg = 0;

/*---
  GRIB data - load the map
---*/

        if (pfi->idxflg) {
          pindx = (struct gaindx *)malloc(sizeof(struct gaindx));
          if (pindx==NULL) goto err8;
          pfi->pindx = pindx;

/*
   check the version number
*/

	  fseek(mfile,1,0);
	  fread(&vermap,sizeof(unsigned char),1,mfile);
	  if(diag) printf("ddd gribmap vermap =%d\n",vermap);

	  if(vermap == 2) {

	    fseek(mfile,2,0);
	    fread(mrec,sizeof(unsigned char),4,mfile);
	    pindx->hinum=gagby(mrec,0,4);
	    if(diag) printf("ddd  hinum = %d\n",pindx->hinum);

	    fread(mrec,sizeof(unsigned char),4,mfile);
	    pindx->hfnum=gagby(mrec,0,4);
	    if(diag) printf("ddd hfnum = %d\n",pindx->hfnum);

	    fread(mrec,sizeof(unsigned char),4,mfile);
	    pindx->intnum=gagby(mrec,0,4);
	    if(diag) printf("ddd intnum = %d\n",pindx->intnum);

	    fread(mrec,sizeof(unsigned char),4,mfile);
	    pindx->fltnum=gagby(mrec,0,4);
	    if(diag) printf("ddd fltnum = %d\n",pindx->fltnum);

/* skip the begining time struct info */

	    fread(mrec,sizeof(unsigned char),7,mfile);

	    pindx->hipnt = NULL;
	    pindx->hfpnt = NULL;
	    pindx->intpnt = NULL;
	    pindx->fltpnt = NULL;

	    if (pindx->hinum>0) {
	      pindx->hipnt = (int *)malloc(sizeof(int)*pindx->hinum);
	      if (pindx->hipnt==NULL) goto err8;

	      for(i=0;i<pindx->hinum;i++) {
		fread(mrec,sizeof(unsigned char),4,mfile);
		idum=gagby(mrec,0,4);
		if(gagbb(mrec,0,1)) idum=-idum;
		*(pindx->hipnt+i)=idum;
		if(diag) printf("ddd 1 i = %d pindx->hipnt = %d\n",i,idum);
	      }

	    }


	    if (pindx->hfnum>0) {
	      pindx->hfpnt = (float *)malloc(sizeof(float)*pindx->hfnum);
	      if (pindx->hfpnt==NULL) goto err8;
	      fread (pindx->hfpnt,sizeof(float),pindx->hfnum,mfile);
	    }

	    if (pindx->intnum>0) {
	      pindx->intpnt = (int *)malloc(sizeof(int)*pindx->intnum);
	      if (pindx->intpnt==NULL) goto err8;
	      for(i=0;i<pindx->intnum;i++) {
		fread(mrec,sizeof(unsigned char),4,mfile);
		idum=gagby(mrec,0,4);
		if(gagbb(mrec,0,1)) idum=-gagbb(mrec,1,31);
		*(pindx->intpnt+i)=idum;
		if(diag) printf("ddd 2 i = %d pindx->intpnt = %d\n",i,idum);
	      }

	    }

	    if (pindx->fltnum>0) {
	      pindx->fltpnt = (float *)malloc(sizeof(float)*pindx->fltnum);
	      if (pindx->fltpnt==NULL) goto err8;

	      for(i=0;i<pindx->fltnum;i++) {
		fread(urec,sizeof(unsigned char),4,mfile);
		fdum=ibm2flt(urec);
		*(pindx->fltpnt+i)=fdum;
		if(diag) printf("ddd 3 i = %d pindx->fltpnt = %g\n",i,fdum);
	      }

	    }

	  } else {

	    fseek (mfile,0L,0);
	    fread (pindx,sizeof(struct gaindx),1,mfile);
	    if (pindx->type>>24 > 0) swpflg=1;
	    if (swpflg) gabswp((float *)pindx,5);
	    pindx->hipnt = NULL;
	    pindx->hfpnt = NULL;
	    pindx->intpnt = NULL;
	    pindx->fltpnt = NULL;
	    if (pindx->hinum>0) {
	      pindx->hipnt = (int *)malloc(sizeof(int)*pindx->hinum);
	      if (pindx->hipnt==NULL) goto err8;
	      fread (pindx->hipnt,sizeof(int),pindx->hinum,mfile);
	      if (swpflg) gabswp((float *)(pindx->hipnt),pindx->hinum);
	    }
	    if (pindx->hfnum>0) {
	      pindx->hfpnt = (float *)malloc(sizeof(float)*pindx->hfnum);
	      if (pindx->hfpnt==NULL) goto err8;
	      fread (pindx->hfpnt,sizeof(float),pindx->hfnum,mfile);
	      if (swpflg) gabswp(pindx->hfpnt,pindx->hfnum);
	    }
	    if (pindx->intnum>0) {
	      pindx->intpnt = (int *)malloc(sizeof(int)*pindx->intnum);
	      if (pindx->intpnt==NULL) goto err8;
	      fread (pindx->intpnt,sizeof(int),pindx->intnum,mfile);
	      if (swpflg) gabswp((float *)(pindx->intpnt),pindx->intnum);
	    }
	    if (pindx->fltnum>0) {
	      pindx->fltpnt = (float *)malloc(sizeof(float)*pindx->fltnum);
	      if (pindx->fltpnt==NULL) goto err8;
	      fread (pindx->fltpnt,sizeof(float),pindx->fltnum,mfile);
	      if (swpflg) gabswp(pindx->fltpnt,pindx->fltnum);
	    }

	  }

/*mf -----

  map file processing -- stnmap

-----mf*/

        } else {

/*mf version detection mf*/

	  fread(rec,1,16,mfile);  /* minimum map file is 16 bytes */

	  vermap=1;
	  if(strncmp(rec,"GrADS_stnmapV002",16) == 0) {
	    vermap=2;
	  }

	  if(vermap == 2) {

	    fread(mrec,sizeof(unsigned char),4,mfile);
	    idum=gagby(mrec,0,4);
	    if(gagbb(mrec,0,1)) idum=-idum;
	    mcnt=idum;

	    fread(mrec,sizeof(unsigned char),4,mfile);
	    idum=gagby(mrec,0,4);
	    if(gagbb(mrec,0,1)) idum=-idum;
	    maxlv=idum;

	    pfi->tstrt = (int *)malloc(sizeof(int)*mcnt);
	    pfi->tcnt = (int *)malloc(sizeof(int)*mcnt);

	    for(i=0;i<mcnt;i++) {
	      fread(mrec,sizeof(unsigned char),4,mfile);
	      idum=gagby(mrec,0,4);
	      if(gagbb(mrec,0,1)) idum=-idum;
	      *(pfi->tstrt+i)=idum;
	    }

	    for(i=0;i<mcnt;i++) {
	      fread(mrec,sizeof(unsigned char),4,mfile);
	      idum=gagby(mrec,0,4);
	      if(gagbb(mrec,0,1)) idum=-idum;
	      *(pfi->tcnt+i)=idum;
	    }

	
	  } else if(vermap ==1) {

/* reposition and read two local ints for version 1 map*/

	    fseek (mfile,0L,0);

	    fread (&mcnt,sizeof(int),1,mfile);
	    fread (&maxlv,sizeof(int),1,mfile);

	    if (maxlv>>24 > 0) swpflg = 1;
	    if (swpflg) {
	      gabswp((float *)(&mcnt),1);
	      gabswp((float *)(&maxlv),1);
	    }
	    pfi->tstrt = (int *)malloc(sizeof(int)*mcnt);
	    pfi->tcnt = (int *)malloc(sizeof(int)*mcnt);
	    fread (pfi->tstrt,sizeof(int),mcnt,mfile);
	    fread (pfi->tcnt,sizeof(int),mcnt,mfile);
	    if (swpflg) {
	      gabswp((float *)pfi->tstrt,1);
	      gabswp((float *)pfi->tcnt,1);
	    }
	    pfi->mtype = 1;

	  }

	}

	if(mfile != NULL) fclose (mfile);
	mflflg = 0;

      } /* END OF mpfile check */


    } else if (cmpwrd("toff",rec)) {
      ch = nxtwrd(rec);
      if (ch==NULL) {
        gaprnt (0,"Open Error:  Missing toff value\n");
        goto err9;
      }
      pos = intprs(ch,&(pfi->tlpst));
      if (pos==NULL || pfi->tlpst>=pfi->dnum[3]) {
        gaprnt (0,"Open Error:  Invalid toff value\n");
        goto err9;
      }
      pfi->tlpflg = 1;


    }  else if (cmpwrd("undef",rec)) {
      ch = nxtwrd(rec);
      if (ch==NULL) {
        gaprnt (0,"Open Error:  Missing undef value\n");
        goto err9;
      }
      pos = valprs(ch,&(pfi->undef));
      if (pos==NULL) {
        gaprnt (0,"Open Error:  Invalid undef value\n");
        goto err9;
      }
      if (SETMISS) {
        pfi->ulow = fabs(pfi->undef/EPSILON);
        pfi->uhi = pfi->undef + pfi->ulow;
        pfi->ulow = pfi->undef - pfi->ulow;
      }
      flgs[4] = 0;

    } else if (cmpwrd("pdef",rec)) {
      if ( (ch = nxtwrd(rec)) == NULL) goto errm;
      if ( (pos = intprs(ch,&(pfi->ppisiz)))==NULL) goto errm;
      if ( (ch = nxtwrd(ch)) == NULL) goto errm;
      if ( (pos = intprs(ch,&(pfi->ppjsiz)))==NULL) goto errm;
      if ( (ch = nxtwrd(ch)) == NULL) goto errm;
      if (cmpwrd("nps",ch)) {pfi->ppflag=1; pfi->ppwrot=1; cnt=4;}
      else if (cmpwrd("sps",ch)) {pfi->ppflag=2; pfi->ppwrot=1; cnt=4;}
      else if (cmpwrd("lcc",ch)) {pfi->ppflag=3; pfi->ppwrot=0; cnt=9;}
      else if (cmpwrd("eta.u",ch)) {pfi->ppflag=4; pfi->ppwrot=1; cnt=4;}
      else if (cmpwrd("pse",ch)) {pfi->ppflag=5; pfi->ppwrot=0; cnt=7;}
      else if (cmpwrd("ops",ch)) {pfi->ppflag=6; pfi->ppwrot=0; cnt=8;}
      else if (cmpwrd("bilin",ch)) {pfi->ppflag=7; pfi->ppwrot=1; cnt=0;}
      else if (cmpwrd("file",ch)) {pfi->ppflag=8; pfi->ppwrot=1; cnt=1;}
      else goto errm;
      for (i=0; i<cnt; i++) {
        if ( (ch = nxtwrd(ch)) == NULL) goto errm;
        if ( (pos = valprs(ch,&(pfi->ppvals[i])))==NULL) goto errm;
      }
      pdefop1 = 0; pdefop2 = 0;
      if (pfi->ppflag==8) {
        i = (int)(pfi->ppvals[0]+0.1);
        if (i<1 || i>8) goto errm;
      }
      if (pfi->ppflag==7 || pfi->ppflag==8) {
        if ( (ch = nxtwrd(ch)) == NULL) goto errm;
        if (cmpwrd("stream",ch)) pdefop1 = 1;
        else if (cmpwrd("sequential",ch)) pdefop1 = 2;
        else goto errm;
        if ( (ch = nxtwrd(ch)) == NULL) goto errm;
        if (cmpwrd("binary",ch)) pdefop2 = 1;
        if (cmpwrd("binary-big",ch)) pdefop2 = 2;
        if (cmpwrd("binary-little",ch)) pdefop2 = 3;
        else if (cmpwrd("packed",ch)) pdefop2 = 4;
        else goto errm;
        if ( (ch = nxtwrd(ch)) == NULL) goto errm;
        if (*ch=='^' || *ch=='$') {
          fnmexp (pdefnm,ch,name);
        } else {
          getwrd (pdefnm,ch,256);
        }
        pdfi = fopen(pdefnm,"rb");
        if (pdfi==NULL) {
          gaprnt (0,"Error opening pdef file\n");
          sprintf (pout, "  File name is:  %s\n",pdefnm);
          gaprnt (0,pout);
          goto errm;
        }
      }

    }  else if (cmpwrd("xdef",rec)) {

      if (pfi->type == 2) continue;
      if ( (ch = nxtwrd(rec)) == NULL) goto err1;
      if ( (pos = intprs(ch,&(pfi->dnum[0])))==NULL) goto err1;
      if (*pos!=' ') goto err1;
      if ( (ch = nxtwrd(ch))==NULL) goto err2;
      if (cmpwrd("linear",ch)) {
        rc = deflin(ch, pfi, 0, 0);
        if (rc==-1) goto err8;
        if (rc) goto err9;
        v2 = *(pfi->grvals[0]);
        v1 = *(pfi->grvals[0]+1) + v2;
        temp = v1+((float)(pfi->dnum[0]))*v2;
        temp=temp-360.0;
        if (fabs(temp-v1)<0.01) pfi->wrap = 1;
      }
      else if (cmpwrd("levels",ch)) {
        if (pfi->dnum[0]<1) goto err7;
        rc = deflev (ch, rec, pfi, 0);
        if (rc==-1) goto err8;
        if (rc) goto err9;
      } else goto err2;
      flgs[0] = 0;

    } else if (cmpwrd("ydef",rec)) {
      if (pfi->type == 2) continue;
      if ( (ch = nxtwrd(rec)) == NULL) goto err1;
      if ( (pos = intprs(ch,&(pfi->dnum[1])))==NULL) goto err1;
      if (*pos!=' ') goto err1;
      if ( (ch = nxtwrd(ch))==NULL) goto err2;
      if (cmpwrd("linear",ch)) {
        rc = deflin(ch, pfi, 1, 0);
        if (rc==-1) goto err8;
        if (rc) goto err9;
      } else if (cmpwrd("levels",ch)) {
        if (pfi->dnum[1]<1) goto err7;
        rc = deflev (ch, rec, pfi, 1);
        if (rc==-1) goto err8;
        if (rc) goto err9;
      } else if (cmpwrd("gausr40",ch)) {
        if ( (ch = nxtwrd(ch))==NULL) goto err3;
        if ( (pos = intprs(ch,&i))==NULL) goto err3;
        pfi->grvals[1] = gagaus(i,pfi->dnum[1]);
        if (pfi->grvals[1]==NULL) goto err9;
        pfi->abvals[1] = pfi->grvals[1];
        pfi->ab2gr[1] = lev2gr;
        pfi->gr2ab[1] = gr2lev;
        pfi->linear[1] = 0;
      } else if (cmpwrd("mom32",ch)) {
        if ( (ch = nxtwrd(ch))==NULL) goto err3;
        if ( (pos = intprs(ch,&i))==NULL) goto err3;
        pfi->grvals[1] = gamo32(i,pfi->dnum[1]);
        if (pfi->grvals[1]==NULL) goto err9;
        pfi->abvals[1] = pfi->grvals[1];
        pfi->ab2gr[1] = lev2gr;
        pfi->gr2ab[1] = gr2lev;
        pfi->linear[1] = 0;
      } else if (cmpwrd("gausr30",ch)) {
        if ( (ch = nxtwrd(ch))==NULL) goto err3;
        if ( (pos = intprs(ch,&i))==NULL) goto err3;
        pfi->grvals[1] = gags30(i,pfi->dnum[1]);
        if (pfi->grvals[1]==NULL) goto err9;
        pfi->abvals[1] = pfi->grvals[1];
        pfi->ab2gr[1] = lev2gr;
        pfi->gr2ab[1] = gr2lev;
        pfi->linear[1] = 0;
      } else if (cmpwrd("gausr20",ch)) {
        if ( (ch = nxtwrd(ch))==NULL) goto err3;
        if ( (pos = intprs(ch,&i))==NULL) goto err3;
        pfi->grvals[1] = gags20(i,pfi->dnum[1]);
        if (pfi->grvals[1]==NULL) goto err9;
        pfi->abvals[1] = pfi->grvals[1];
        pfi->ab2gr[1] = lev2gr;
        pfi->gr2ab[1] = gr2lev;
        pfi->linear[1] = 0;
      } else if (cmpwrd("gausr15",ch)) {
        if ( (ch = nxtwrd(ch))==NULL) goto err3;
        if ( (pos = intprs(ch,&i))==NULL) goto err3;
        pfi->grvals[1] = gags15(i,pfi->dnum[1]);
        if (pfi->grvals[1]==NULL) goto err9;
        pfi->abvals[1] = pfi->grvals[1];
        pfi->ab2gr[1] = lev2gr;
        pfi->gr2ab[1] = gr2lev;
        pfi->linear[1] = 0;
      } else goto err2;
      flgs[1] = 0;

    } else if (cmpwrd("zdef",rec)) {
      if (pfi->type == 2) continue;
      if ( (ch = nxtwrd(rec)) == NULL) goto err1;
      if ( (pos = intprs(ch,&(pfi->dnum[2])))==NULL) goto err1;
      if (*pos!=' ') goto err1;
      if ( (ch = nxtwrd(ch))==NULL) goto err2;
      if (cmpwrd("linear",ch)) {
        rc = deflin(ch, pfi, 2, 0);
        if (rc==-1) goto err8;
        if (rc) goto err9;
      }
      else if (cmpwrd("levels",ch)) {
        if (pfi->dnum[2]<1) goto err7;
        rc = deflev (ch, rec, pfi, 2);
        if (rc==-1) goto err8;
        if (rc) goto err9;
      } else goto err2;
      flgs[2] = 0;

    } else if (cmpwrd("tdef",rec)) {
      if ( (ch = nxtwrd(rec)) == NULL) goto err1;
      if ( (pos = intprs(ch,&(pfi->dnum[3])))==NULL) goto err1;
      if (*pos!=' ') goto err1;
      if ( (ch = nxtwrd(ch))==NULL) goto err2;
      if (cmpwrd("linear",ch)) {
        if ( (ch = nxtwrd(ch))==NULL) goto err3;
        tdef.yr = -1000;
        tdef.mo = -1000;
        tdef.dy = -1000;
        if ( (pos = adtprs(ch,&tdef,&dt1))==NULL) goto err3;
        if (*pos!=' ' || dt1.yr == -1000 || dt1.mo == -1000.0 ||
            dt1.dy == -1000) goto err3;
        if ( (ch = nxtwrd(ch))==NULL) goto err4;
        if ( (pos = rdtprs(ch,&dt2))==NULL) goto err4;
        v1 = (dt2.yr * 12) + dt2.mo;
        v2 = (dt2.dy * 1440) + (dt2.hr * 60) + dt2.mn;
	/*mf --- check if 0 dt ---mf*/
	if( (v1 == 0) && (v2 == 0) ) goto err4a;
        vals = (float *)malloc(sizeof(float)*8);
        if (vals==NULL) goto err8;
        *(vals) = dt1.yr;
        *(vals+1) = dt1.mo;
        *(vals+2) = dt1.dy;
        *(vals+3) = dt1.hr;
        *(vals+4) = dt1.mn;
        *(vals+5) = v1;
        *(vals+6) = v2;
        *(vals+7) = -999.9;
        pfi->grvals[3] = vals;
        pfi->abvals[3] = vals;
        pfi->linear[3] = 1;
      } else goto err2;
      flgs[3] = 0;

    } else if (cmpwrd("vars",rec)) {
      if ( (ch = nxtwrd(rec)) == NULL) goto err5;
      if ( (pos = intprs(ch,&(pfi->vnum)))==NULL) goto err5;
      size = pfi->vnum * (sizeof(struct gavar) + 7 );
      pvar = (struct gavar *)malloc(size);
      pfi->pvar1 = pvar;
      i = 0;
      while (i<pfi->vnum) {
        if (fgets(rec,256,descr)==NULL) {
          gaprnt (0,"Open Error:  Unexpected EOF reading variables\n");
          sprintf (pout, "Was expecting %i records.  Found %i.\n",
                   pfi->vnum, i);
          gaprnt (2,pout);
          goto retrn;
        }
        strcpy (mrec,rec);
        lowcas(rec);
        if (cmpwrd("endvars",rec)) {
          gaprnt (0,"Open Error:  Unexpected ENDVARS record\n");
          sprintf (pout, "Was expecting %i records.  Found %i.\n",
                   pfi->vnum, i);
          gaprnt (2,pout);
          goto err9;
        }
        getwrd (pvar->abbrv,rec,15);
        if ( (ch=nxtwrd(rec))==NULL) goto err6;
        if ( (pos=intprs(ch,&(pvar->levels)))==NULL ) goto err6;
        if ( (ch=nxtwrd(ch))==NULL) goto err6;
        for (j=0;j<4;j++) pvar->units[j] = -999;
        j = 0;
        while (1) {
          if ( (ch=intprs(ch,&(pvar->units[j])))==NULL ) goto err6;
          while (*ch==' ') ch++;
          if (*ch=='\0' || *ch=='\n') goto err6;
          if (*ch!=',') break;
          ch++;
          while (*ch==' ') ch++;
          if (*ch=='\0' || *ch=='\n') goto err6;
          j++;
          if (j>3) goto err6;
        }
        getstr (pvar->varnm,mrec+(ch-rec),127);

/*mf...........mf*/
/* initialize the var_z counter for NASA GLA format */
	pvar->var_z = 1;
	if(pvar->units[0] == -1 && pvar->units[1] == 10 ) {
	  nzstride++;
	}

/* var_t is for DRS var-t transforms */

	pvar->var_t = 0;
	if(pvar->units[0] == -1 && pvar->units[1] == 20 ) pvar->var_t = 1;

/* x-y transpose for lat/lon vice lon/lat data VERY INEFFICIENT!!! */

	pvar->y_x = 0;
	if(pvar->units[0] == -1 && pvar->units[1] == 30) pvar->y_x = 1;

/*mf 961126 -  integer data types mf*/

	pvar->dfrm = 0;
	if( (pvar->units[0] == -1 && pvar->units[1] == 40 ) &&
	    (pvar->units[2] >=1 && pvar->units[2] <= 4 ) ) {
	  pvar->dfrm= pvar->units[2];
	  if ( pvar->units[2] == 2 ) pvar->dfrm=2;
	  if ( pvar->units[2] == 2 && pvar->units[3] == -1) pvar->dfrm=-2;

	}
/* 32-bit big endian ieee to cray float */

#if GRADS_CRAY == 1
	if( (pvar->units[0]==-1 && pvar->units[1]==50) || crayflg ) pvar->dfrm=8;
#endif

/*mf...........mf*/

        i++; pvar++;
      }
      if (fgets(rec,256,descr)==NULL) {
        gaprnt (0,"Open Error:  Missing ENDVARS statement.\n");
        goto retrn;
      }

      lowcas(rec);
      if (!cmpwrd("endvars",rec)) {
        gaprnt (0,"Open Error:  Looking for ENDVARS statement\n");
        gaprnt (0,"             Instead, found:  \n");
        goto err9;
      }
      flgs[6] = 0;
      /*
	 parse error of .ctl file
	 */
    } else {
      gaprnt (0,"Open Error:  Unknown keyword in description file\n");
      goto err9;
    }


  }

  err=0;
  for (i=0; i<7; i++) {
    if (flgs[i]) {
      sprintf (pout,"Open Error:  missing %s record \n",errs[i]);
      gaprnt (0,pout);
      err=1;
    }
  }

  if (err) goto retrn;

  if (pfi->type>1 && mflag) {
    if (mcnt==-1) {
      gaprnt (0,"Open Error: missing STNMAP record\n");
      err=1;
    } else if (mcnt != pfi->dnum[3]) {
      gaprnt (0,"Open Error: Inconsistent time count\n");
      sprintf (pout,"  Count in station map file = %li\n",mcnt);
      gaprnt (0,pout);
      sprintf (pout,"  Count in descriptor file = %i\n",pfi->dnum[3]);
      gaprnt (0,pout);
      err=1;
    }
  }

  if (err) goto retrn;

  /* Figure out locations of variables within a time group */

  pvar = pfi->pvar1;

/*mf------------------mf*/
/*

   Grid data
*/
  if (pfi->type==1) {
    pfi->gsiz = pfi->dnum[0] * pfi->dnum[1];
    if (pfi->ppflag) pfi->gsiz = pfi->ppisiz * pfi->ppjsiz;

/*mf
  add a constant xy header
mf*/
    if (pfi->xyhdr) {
      pfi->gsiz = pfi->gsiz + pfi->xyhdr;
      }

    if (pfi->seqflg) {
      pfi->gsiz+=2;
      if (hdrb>0) hdrb+=2;
      pvar->offset = 1+hdrb;
      acum = 1+hdrb;
    } else {
      pvar->offset = hdrb;
      acum = hdrb;
    }
    levs = pvar->levels;
    if (levs==0) levs=1;
    pvar->recoff = 0;
    recacm = 0;
    pvar++;

    acumvz=acum;

    for (i=1; i<pfi->vnum; i++) {

/* NASA GLA FORMAT CHECKS */
/* upper air fields which var and z are transposed*/

      if( (pvar->units[0]==-1) &&
	 (pvar->units[1]==10) &&
	 (pvar->units[2]==1) ) {
						
	levsua = pvar->levels;
	acum = acum + pfi->gsiz;
	pvar->var_z = nzstride;

/* diagnstotic fields AFTER  upper air fields which are in GrADS normal order */

      } else if( (pvar->units[0]==-1) &&
		(pvar->units[1]==10) &&
		(pvar->units[2]==2) ) {
	if(first) {
	  acumstride = acumstride + nzstride*levsua*pfi->gsiz;
	  acum = acumstride + (pvar->levels*pfi->gsiz) ;
	  first = 0 ;
	} else {
	  acum = acum + (levs*pfi->gsiz);
	}

      } else if(pvar->var_t) {   /* DRS transposition of var and t */

	if(pfi->tmplat) {   /* time template, read the third unit param
			       for the size of each chunk in a file */

	  if(pvar->units[2] != -999) {
	    acum = acum + levs*(pfi->gsiz)*(pvar->units[1]);
	  } else {
	    gaprnt (0,"Using time templat and 4-D variables, # times / file not specified\n");
	    gaprnt (0,"Defaulting to the number of times in the .ctl file\n");
	    acum = acum + levs*(pfi->gsiz)*(pfi->dnum[3]);
	  }
	} else {
	  acum = acum + levs*(pfi->gsiz)*(pfi->dnum[3]);
	}

      } else  {                                 /* simple GrADS */

	acum = acum + (levs*pfi->gsiz);
	acumstride = acum ;

      }

      recacm += levs;
      pvar->offset = acum;
      pvar->recoff = recacm;
      levs = pvar->levels;
      if (levs==0) levs=1;
      pvar++;

    }

    recacm += levs;

/*mf 960514 correct for case where the last variable is a NASA UA */

    pvar--;
    if( (pvar->units[0]==-1) &&
       (pvar->units[1]==10) &&
       (pvar->units[2]==1) ) {
      acum = acumvz + recacm*pfi->gsiz;
    }else {					
      acum = acum + (levs*pfi->gsiz);
    }

/*mf --------------mf*/
/*
   time chunk header; the default is 0
*/
    if(pfi->seqflg && pfi->thdr>0) {
      pfi->thdr+=2;
    }
    pfi->tsiz = acum + pfi->thdr;

/*mf --------------mf*/

    pfi->trecs = recacm;
    if (pfi->seqflg) pfi->tsiz-=1;
    pfi->tsiz += trlb;

/*------------------- non grid data???? -------- */

  } else {

    for (i=0; i<pfi->vnum; i++) {
      if (pvar->levels!=0) break;
      pvar->offset = i;
      pvar++;
    }
    for (j=i; j<pfi->vnum; j++) {
      if (pvar->levels==0) {
	gaprnt (0,"Open Error: Variables out of order\n");
	gaprnt (0,"  Non-vertical variables must go first\n");
	goto retrn;
      }
      pvar->offset = j-i;
      pvar++;
    }
    pfi->lvnum = pfi->vnum - i;
    pfi->ivnum = i;
  }

/*mf---

  961204

  set the global calendar and check if we are trying to change with a new file...
  we do this here to set the calandar for templating

----mf*/

/*    if(mfcmn.cal365<0) { */
/*      mfcmn.cal365=pfi->calendar; */
/*    } else { */
/*      if(pfi->calendar != mfcmn.cal365) { */
/*        gaprnt(0,"Attempt to change the global calendar...\n"); */
/*        if(mfcmn.cal365) { */
/*  	gaprnt(0,"The calendar is NOW 365 DAYS and you attempted to open a standard calendar file\n"); */
/*        } else { */
/*  	gaprnt(0,"The calendar is NOW STANDARD and you attempted to open a 365-day calendar file\n"); */
/*        } */
/*        goto retrn; */
/*      } */
/*    } */

  /* Allocate an I/O buffer the size of one row */

  if (pfi->type > 1) {
    size = maxlv * sizeof(float);
  } else {
    size = pfi->dnum[0] * sizeof(float);
  }
  pfi->rbuf = (float *)malloc(size);
  if (pfi->idxflg) pfi->pbuf = (char *)malloc(size);

  /* If a pre-projected grid, set up the interpolation
     constants.  */

  if (pfi->ppflag) {
    rc = gappcn(pfi,pdefop1,pdefop2);
    if (rc) goto retrn;
  }

  /* If the file name is a time series template, figure out
     which times go with which files, so we don't waste a lot
     of time later opening and closing files unnecessarily. */

  if (pfi->tmplat) {
    pfi->fnums = (int *)malloc(sizeof(int)*pfi->dnum[3]);
    if (pfi->fnums==NULL) goto err8;
    j = 1;
    gr2t(pfi->grvals[3],1.0,&tdefi);
    ch = gafndt(pfi->name,&tdefi,&tdefi,pfi->abvals[3]);
    if (ch==NULL) goto err8;
    *(pfi->fnums) = j;
    for (i=2; i<=pfi->dnum[3]; i++) {
      gr2t(pfi->grvals[3],(float)i,&tdef);
      pos = gafndt(pfi->name,&tdef,&tdefi,pfi->abvals[3]);
      if (pos==NULL) goto err8;
      if (strcmp(ch,pos)!=0) {
	j = i;
	free(ch);
	ch = pos;
      }
      else{
	free(pos);
      }
      *(pfi->fnums+i-1) = j;
    }
    free(ch);
    pfi->fnumc = 0;
  }

  fclose (descr);
  if (pdfi) fclose(pdfi);

  return(0);

 errm:
  gaprnt(0,"Open Error: Invalid pdef record.\n");
  pfi->ppflag = 0;
  goto err9;

 err1:
  gaprnt (0,"Open Error:  Missing or invalid dimension size.\n");
  goto err9;

 err2:
  gaprnt (0,"Open Error:  Missing or invalid dimension");
  gaprnt (0," scaling type\n");
  goto err9;

 err3:
  gaprnt (0,"Open Error:  Missing or invalid dimension");
  gaprnt (0," starting value\n");
  goto err9;

 err4:
  gaprnt (0,"Open Error:  Missing or invalid dimension");
  gaprnt (0," increment value\n");
  goto err9;

 err4a:
  gaprnt (0,"Open Error:  0 time increment in tdef\n");
  gaprnt (0," use 1 for single time data\n");
  goto err9;

 err5:
  gaprnt (0,"Open Error:  Missing or invalid variable");
  gaprnt (0," count\n");
  goto err9;

 err6:
  gaprnt (0,"Open Error:  Invalid variable record\n");
  goto err9;

 err7:
  gaprnt (0,"Open Error:  Invalid number of levels\n");
  goto err9;

 err8:
  gaprnt (0,"Open Error:  Memory allocation Error\n");
  goto retrn;

 err9:
  gaprnt (0,"  --> The invalid description file record is: \n");
  gaprnt (0,"  --> ");
  gaprnt (0,rec);
  gaprnt (0,"\n");

 retrn:
  gaprnt (0,"  The data file was not opened. \n");
  fclose (descr);
  if (mflflg) fclose(mfile);
  if (pdfi) fclose(pdfi);
  return(1);

}



/* Process linear scaling args */

int deflin (char *ch, struct gafile *pfi, int dim, int flag) {
float *vals,v1,v2;

  vals = (float *)malloc(sizeof(float)*6);
  if (vals==NULL) return (-1);

  if ( (ch = nxtwrd(ch))==NULL) goto err1;
  if ( valprs(ch,&v1)==NULL) goto err1;
  if (flag) v2 = 1.0;
  else {
    if ( (ch = nxtwrd(ch))==NULL) goto err2;
    if ( valprs(ch,&v2)==NULL) goto err2;
  }
  if (dim<2 && v2<=0.0) goto err2;
  *(vals+1) = v1 - v2;
  *(vals) = v2;
  *(vals+2) = -999.9;
  pfi->grvals[dim] = vals;
  *(vals+4) = -1.0 * ( (v1-v2)/v2 );
  *(vals+3) = 1.0/v2;
  *(vals+5) = -999.9;
  pfi->abvals[dim] = vals+3;
  pfi->ab2gr[dim] = liconv;
  pfi->gr2ab[dim] = liconv;
  pfi->linear[dim] = 1;
  return (0);

err1:
  gaprnt (0,"Open Error:  Missing or invalid dimension");
  gaprnt (0," starting value\n");
  free (vals);
  return (1);

err2:
  gaprnt (0,"Open Error:  Missing or invalid dimension");
  gaprnt (0," increment value\n");
  free (vals);
  return (1);
}

/* Process levels values in def record */
/* Return codes:  -1 is memory allocation error, 1 is other error */

int deflev (char *ch, char *rec, struct gafile *pfi, int dim) {
float *vvs,*vals,v1,v2;
int i;

  if (pfi->dnum[dim]==1) {
    i = deflin (ch, pfi, dim, 1);
    return (i);
  }

  vals = (float *)malloc((pfi->dnum[dim]+5)*sizeof(float));
  if (vals==NULL) return (-1);

  vvs = vals;
  *vvs = (float)pfi->dnum[dim];
  vvs++;
  for (i=0; i<pfi->dnum[dim]; i++) {
    if ( (ch = nxtwrd(ch))==NULL) {
      if (fgets(rec,256,descr)==NULL) goto err2;
      if (nxtwrd(rec)==NULL) goto err3;
      ch = rec;
      while (*ch==' ') ch++;
    }
    if (valprs(ch,&v1)==NULL) goto err1;
    *vvs = v1;
    vvs++;
  }
  *vvs = -999.9;
  pfi->abvals[dim] = vals;
  pfi->grvals[dim] = vals;
  pfi->ab2gr[dim] = lev2gr;
  pfi->gr2ab[dim] = gr2lev;
  pfi->linear[dim] = 0;
  return (0);

err1:
  gaprnt (0,"Open Error:  Invalid value in LEVELS data\n");
  free (vals);
  return (1);

err2:
  gaprnt (0,"Open Error:  Unexpected EOF reading descriptor file\n");
  gaprnt (0,"   EOF occurred reading LEVELS values\n");
  free (vals);
  return (1);

err3:
  gaprnt (0,"Open Error:  Blank Record found in LEVELS data\n");
  free (vals);
  return (1);
}

/* Allocate and initialize a gafile structure */

struct gafile *getpfi (void) {
struct gafile *pfi;
int i;
/* #ifdef USESDF */
/* int init_io_std(IO_STD**) ; */
/* #endif */

  pfi = (struct gafile *)malloc(sizeof(struct gafile));
  if (pfi==NULL) return (NULL);

  pfi->type = 1;        /* Assume grid unless told otherwise */
  pfi->tlpflg = 0;      /* Assume file not circular */
  pfi->bswap = 0;       /* Assume no byte swapping needed */
  pfi->seqflg = 0;      /* Assume direct access */
  pfi->yrflg = 0;       /* Assume south to north */
  pfi->zrflg = 0;       /* Assume bottom to top */
  pfi->idxflg = 0;      /* Assume binary */
  pfi->fhdr = 0;        /* Assume no file header */
  pfi->thdr=0;          /*mf assume no time header mf*/
  pfi->xyhdr=0;         /*mf assume no xyheader mf*/
  pfi->pindx = NULL;
  pfi->rbuf = NULL;
  pfi->pbuf = NULL;
  pfi->wrap = 0;        /* Assume no wrapping */
  for (i=0; i<4; i++) pfi->dimoff[i] = 0;
  pfi->title[0] = '\0';
  pfi->infile = NULL;
  pfi->pvar1 = NULL;
  pfi->tstrt = NULL;
  pfi->tcnt = NULL;
  pfi->grvals[0] = NULL;
  pfi->grvals[1] = NULL;
  pfi->grvals[2] = NULL;
  pfi->grvals[3] = NULL;
  pfi->grbgrd = -999;
  pfi->tmplat = 0;
  pfi->tempname = NULL;
  pfi->fnums = NULL;
  pfi->infile = NULL;
  pfi->mnam = NULL;
  pfi->errcnt = 0;
  pfi->errflg = 0;
  pfi->ppflag = 0;  /* Assume lat-lon grid */
  pfi->ppwrot = 0;  /* Assume no wind rotataion */
  for (i=0; i<8; i++) pfi->ppi[i] = NULL;
  for (i=0; i<8; i++) pfi->ppf[i] = NULL;
  pfi->ppw = NULL;
#if USESDF == 1
  pfi->sdf_ptr = NULL ;
/*  if (!init_io_std(&(pfi->sdf_ptr))) { */
/*    gaprnt(0, */
/* "Initialize file error:  Couldn't allocate and initialize SDF structure.\n") ; */
/*    return(NULL) ; */
/*  } */
#endif

  return (pfi);
}

/* Free a gafile structure and associated storage.  If the flag is
   true, DO NOT free the storage related to scaling transforms,
   since someone, somewhere,  may still be pointing to that. */

void frepfi (struct gafile *pfi, int flag) {
struct gaindx *pindx;
int i;
#if USESDF == 1
void free_io_std(IO_STD**) ;
#endif

  if (pfi->pindx) {
    pindx = pfi->pindx;
    if (pindx->hipnt) free(pindx->hipnt);
    if (pindx->hfpnt) free(pindx->hfpnt);
    if (pindx->intpnt) free(pindx->intpnt);
    if (pindx->fltpnt) free(pindx->fltpnt);
    free (pindx);
  }
  if (pfi->fnums) free(pfi->fnums);
  if (pfi->tstrt) free(pfi->tstrt);
  if (pfi->tcnt) free(pfi->tcnt);
  if (pfi->pvar1) free(pfi->pvar1);
  if (pfi->rbuf) free(pfi->rbuf);
  if (pfi->pbuf) free(pfi->pbuf);
  if (pfi->mnam) free(pfi->mnam);
  if (pfi->tempname!=NULL) {
	  free(pfi->tempname);
	  pfi->tempname=NULL;
  }
  for (i=0; i<8; i++) if (pfi->ppi[i]) free(pfi->ppi[i]);
  for (i=0; i<8; i++) if (pfi->ppf[i]) free(pfi->ppf[i]);
  if (!flag) {
    for (i=0; i<4; i++) if (pfi->grvals[i]) free(pfi->grvals[i]);
  }
#if USESDF == 1
  if (pfi->sdf_ptr) {
    free_io_std(&(pfi->sdf_ptr)) ;
  }
#endif
  free (pfi);
}

/* Routine to calculate or input the interpolation constants needed for
   the implicit interpolation from pre-projected grids to lat-lon. */

int gappcn (struct gafile *pfi, int pdefop1, int pdefop2) {
FILE *pdfi;
int size,i,j,ii,jj;
float lat,lon,rii,rjj;
float *dx, *dy, *etarot;
int *ioff, rdw, rc, pnum;

  size = pfi->dnum[0]*pfi->dnum[1];

  if (pfi->ppflag != 8) {

    /* Allocate space needed for the grids of interpolation constants */

    pfi->ppi[0] = (int *)malloc(sizeof(int)*size);
    if (pfi->ppi[0]==NULL) goto merr;
    pfi->ppf[0] = (float *)malloc(sizeof(float)*size);
    if (pfi->ppf[0]==NULL) goto merr;
    pfi->ppf[1] = (float *)malloc(sizeof(float)*size);
    if (pfi->ppf[1]==NULL) goto merr;
    if (pfi->ppwrot) {
      pfi->ppw = (float *)malloc(sizeof(float)*size);
      if (pfi->ppw==NULL) goto merr;
    }
  }

  if (pfi->ppflag==7) {
    pfi->ppi[1] = (int *)malloc(sizeof(int)*size);
    if (pfi->ppi[1]==NULL) goto merr;
    if (pdefop1==2) {
      rc = fread(&rdw, sizeof(int), 1, pdfi);
      if (rc!=1) goto merr2;
    }
    rc = fread(pfi->ppf[0], sizeof(float), size, pdfi);
    if (rc!=size) goto merr2;
    if (pdefop1==2) {
      rc = fread(&rdw, sizeof(int), 1, pdfi);
      if (rc!=1) goto merr2;
      rc = fread(&rdw, sizeof(int), 1, pdfi);
      if (rc!=1) goto merr2;
    }
    rc = fread(pfi->ppf[1], sizeof(float), size, pdfi);
    if (rc!=size) goto merr2;
    if (pdefop1==2) {
      rc = fread(&rdw, sizeof(int), 1, pdfi);
      if (rc!=1) goto merr2;
      rc = fread(&rdw, sizeof(int), 1, pdfi);
      if (rc!=1) goto merr2;
    }
    rc = fread(pfi->ppw, sizeof(float), size, pdfi);
    if (rc!=size) goto merr2;

    if (  (pdefop2==2 && !BYTEORDER) ||
          (pdefop2==3 && BYTEORDER) ) {
      gabswp (pfi->ppf[0],size);
      gabswp (pfi->ppf[1],size);
      gabswp (pfi->ppw,size);
    }

    ioff = pfi->ppi[0];
    dx = pfi->ppf[0];
    dy = pfi->ppf[1];
    for (j=0; j<pfi->dnum[1]; j++) {
      for (i=0; i<pfi->dnum[0]; i++) {
        if (*dx < 0.0) *ioff = -1;
        else {
          ii = (int)(*dx);
          jj = (int)(*dy);
          *dx = *dx - (float)ii;
          *dy = *dy - (float)jj;
          if (ii<1 || ii>pfi->ppisiz-1 || jj<1 || jj>pfi->ppjsiz-1) {
            *ioff = -1;
          } else {
            *ioff = (jj-1)*pfi->ppisiz + ii - 1;
          }
          ioff++; dx++; dy++;
        }
      }
    }

  /* When pdef is a file, read in the offsets of the points
     to use and their weights, as well as the array of
     wind rotation values to use */

  } else if (pfi->ppflag==8) {
    pnum = (int)(pfi->ppvals[0]+0.1);
    for (i=0; i<pnum; i++) {
      pfi->ppi[i] = (int *)malloc(sizeof(int)*size);
      if (pfi->ppi[i]==NULL) goto merr;
      if (pdefop1==2) {
        rc = fread(&rdw, sizeof(int), 1, pdfi);
        if (rc!=1) goto merr2;
      }
      rc = fread(pfi->ppi[i], sizeof(int), size, pdfi);
      if (rc!=size) goto merr2;
      if (pdefop1==2) {
        rc = fread(&rdw, sizeof(int), 1, pdfi);
        if (rc!=1) goto merr2;
      }
      pfi->ppf[i] = (float *)malloc(sizeof(int)*size);
      if (pfi->ppf[i]==NULL) goto merr;
      if (pdefop1==2) {
        rc = fread(&rdw, sizeof(int), 1, pdfi);
        if (rc!=1) goto merr2;
      }
      rc = fread(pfi->ppf[i], sizeof(float), size, pdfi);
      if (rc!=size) goto merr2;
      if (pdefop1==2) {
        rc = fread(&rdw, sizeof(int), 1, pdfi);
        if (rc!=1) goto merr2;
      }
      if (  (pdefop2==2 && !BYTEORDER) ||
            (pdefop2==3 && BYTEORDER) ) {
        gabswp ((float *)(pfi->ppi[i]),size);
        gabswp (pfi->ppf[i],size);
      }
    }
    pfi->ppw = (float *)malloc(sizeof(float)*size);
    if (pfi->ppw==NULL) goto merr;
    if (pdefop1==2) {
      rc = fread(&rdw, sizeof(int), 1, pdfi);
      if (rc!=1) goto merr2;
    }
    rc = fread(pfi->ppw, sizeof(float), size, pdfi);
    if (rc!=size) goto merr2;

  /* We calculate three constants at each lat-lon grid point:
     offset of the ij gridpoint, and the delta x and delta y
     values. */

  } else {
    ioff = pfi->ppi[0];
    dx = pfi->ppf[0];
    dy = pfi->ppf[1];
    if ( pfi->ppflag == 4) etarot=pfi->ppw;

    for (j=0; j<pfi->dnum[1]; j++) {
      lat = pfi->gr2ab[1](pfi->grvals[1],(float)(j+1));
      for (i=0; i<pfi->dnum[0]; i++) {
        lon = pfi->gr2ab[0](pfi->grvals[0],(float)(i+1));
        if (pfi->ppflag==3) {
          ll2lc (pfi->ppvals, lat, lon, &rii, &rjj);
        } else if (pfi->ppflag==4) {
          ll2eg (pfi->ppisiz,pfi->ppjsiz,pfi->ppvals, lon, lat, &rii, &rjj, etarot);
        } else if (pfi->ppflag==5) {
          ll2pse (pfi->ppisiz,pfi->ppjsiz,pfi->ppvals, lon, lat, &rii, &rjj);
        } else if (pfi->ppflag==6) {
          ll2ops (pfi->ppvals, lon, lat, &rii, &rjj);
        } else {
          w3fb04(lat, -1.0*lon, pfi->ppvals[3], -1.0*pfi->ppvals[2], &rii, &rjj);
          rii = rii + pfi->ppvals[0];
          rjj = rjj + pfi->ppvals[1];
        }
        ii = (int)rii;
        jj = (int)rjj;
        *dx = rii - (float)ii;
        *dy = rjj - (float)jj;
        if (ii<1 || ii>pfi->ppisiz-1 || jj<1 || jj>pfi->ppjsiz-1) {
          *ioff = -1;
        } else {
          *ioff = (jj-1)*pfi->ppisiz + ii - 1;
        }
        ioff++; dx++; dy++;

        if ( pfi->ppflag == 4) etarot++;

      }
    }
  }
  return(0);

merr:
  gaprnt (0,"Open Error:  Memory allocation error in pdef handler\n");
  return(1);

merr2:
  gaprnt (0,"Open Error:  I/O Error on pdef file read\n");
  return(1);

}


void w3fb04 (float alat, float along, float xmeshl, float orient,
     float *xi, float *xj) {

/*
C
C SUBPROGRAM: W3FB04         LATITUDE, LONGITUDE TO GRID COORDINATES
C   AUTHOR: MCDONELL,J.      ORG: W345       DATE: 90-06-04
C
C ABSTRACT: CONVERTS THE COORDINATES OF A LOCATION ON EARTH FROM THE
C   NATURAL COORDINATE SYSTEM OF LATITUDE/LONGITUDE TO THE GRID (I,J)
C   COORDINATE SYSTEM OVERLAID ON A POLAR STEREOGRAPHIC MAP PRO-
C   JECTION TRUE AT 60 DEGREES N OR S LATITUDE. W3FB04 IS THE REVERSE
C   OF W3FB05.
C
C PROGRAM HISTORY LOG:
C   77-05-01  J. MCDONELL
C   89-01-10  R.E.JONES   CONVERT TO MICROSOFT FORTRAN 4.1
C   90-06-04  R.E.JONES   CONVERT TO SUN FORTRAN 1.3
C   93-01-26  B. Doty     converted to C
C
C USAGE:  CALL W3FB04 (ALAT, ALONG, XMESHL, ORIENT, XI, XJ)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     ALAT   ARG LIST  LATITUDE IN DEGREES (<0 IF SH)
C     ALONG  ARG LIST  WEST LONGITUDE IN DEGREES
C     XMESHL ARG LIST  MESH LENGTH OF GRID IN KM AT 60 DEG LAT(<0 IF SH)
C                   (190.5 LFM GRID, 381.0 NH PE GRID,-381.0 SH PE GRID)
C     ORIENT ARG LIST  ORIENTATION WEST LONGITUDE OF THE GRID
C                   (105.0 LFM GRID, 80.0 NH PE GRID, 260.0 SH PE GRID)
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     XI     ARG LIST  I OF THE POINT RELATIVE TO NORTH OR SOUTH POLE
C     XJ     ARG LIST  J OF THE POINT RELATIVE TO NORTH OR SOUTH POLE
C
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     COS SIN                                                 SYSLIB
C
C   REMARKS: ALL PARAMETERS IN THE CALLING STATEMENT MUST BE
C     REAL. THE RANGE OF ALLOWABLE LATITUDES IS FROM A POLE TO
C     30 DEGREES INTO THE OPPOSITE HEMISPHERE.
C     THE GRID USED IN THIS SUBROUTINE HAS ITS ORIGIN (I=0,J=0)
C     AT THE POLE IN EITHER HEMISPHERE, SO IF THE USER'S GRID HAS ITS
C     ORIGIN AT A POINT OTHER THAN THE POLE, A TRANSLATION IS NEEDED
C     TO GET I AND J. THE GRIDLINES OF I=CONSTANT ARE PARALLEL TO A
C     LONGITUDE DESIGNATED BY THE USER. THE EARTH'S RADIUS IS TAKEN
C     TO BE 6371.2 KM.
C
C ATTRIBUTES:
C   LANGUAGE: SUN FORTRAN 1.4
C   MACHINE:  SUN SPARCSTATION 1+
C*/

static float radpd = 0.01745329;
static float earthr = 6371.2;

float re,xlat,wlong,r;

      re    = (earthr * 1.86603) / xmeshl;
      xlat =  alat * radpd;

      if (xmeshl>0.0) {
        wlong = (along + 180.0 - orient) * radpd;
        r     = (re * cos(xlat)) / (1.0 + sin(xlat));
        *xi    = r * sin(wlong);
        *xj    = r * cos(wlong);

      } else {

        re    = -re;
        xlat =  -xlat;
        wlong = (along - orient) * radpd;
        r     = (re * cos(xlat)) / (1.0+ sin(xlat));
        *xi   =  r * sin(wlong);
        *xj   = -r * cos(wlong);
      }
}

/* Lambert conformal conversion */

void ll2lc (float *vals, float grdlat, float grdlon,
                                 float *grdi, float *grdj) {

/*  Subroutine to convert from lat-lon to lambert conformal i,j.
    Provided by NRL Monterey; converted to C 6/15/94.

c          SUBROUTINE: ll2lc
c
c          PURPOSE: To compute i- and j-coordinates of a specified
c                   grid given the latitude and longitude points.
c                   All latitudes in this routine start
c                   with -90.0 at the south pole and increase
c                   northward to +90.0 at the north pole.  The
c                   longitudes start with 0.0 at the Greenwich
c                   meridian and increase to the east, so that
c                   90.0 refers to 90.0E, 180.0 is the inter-
c                   national dateline and 270.0 is 90.0W.
c
c          INPUT VARIABLES:
c
c  vals+0    reflat: latitude at reference point (iref,jref)
c
c  vals+1    reflon: longitude at reference point (iref,jref)
c
c  vals+2    iref:   i-coordinate value of reference point
c
c  vals+3    jref:   j-coordinate value of reference point
c
c  vals+4    stdlt1: standard latitude of grid
c
c  vals+5    stdlt2: second standard latitude of grid (only required
c                    if igrid = 2, lambert conformal)
c
c  vals+6    stdlon: standard longitude of grid (longitude that
c                     points to the north)
c
c  vals+7    delx:   grid spacing of grid in x-direction
c                    for igrid = 1,2,3 or 4, delx must be in meters
c                    for igrid = 5, delx must be in degrees
c
c  vals+8    dely:   grid spacing (in meters) of grid in y-direction
c                    for igrid = 1,2,3 or 4, delx must be in meters
c                    for igrid = 5, dely must be in degrees
c
c            grdlat: latitude of point (grdi,grdj)
c
c            grdlon: longitude of point (grdi,grdj)
c
c            grdi:   i-coordinate(s) that this routine will generate
c                    information for
c
c            grdj:   j-coordinate(s) that this routine will generate
c                    information for
c
*/

  float pi, pi2, pi4, d2r, r2d, radius, omega4;
  float gcon,ogcon,ahem,deg,cn1,cn2,cn3,cn4,rih,xih,yih,rrih,check;
  float alnfix,alon,x,y;

  pi = 4.0*atan(1.0);
  pi2 = pi/2.0;
  pi4 = pi/4.0;
  d2r = pi/180.0;
  r2d = 180.0/pi;
  radius = 6371229.0;
  omega4 = 4.0*pi/86400.0;

/*mf -------------- mf*/
/*case where standard lats are the same */

  if(*(vals+4) == *(vals+5)) {
    gcon = sin(*(vals+4)*d2r);
  } else {
    gcon = (log(sin((90.0-*(vals+4))*d2r))
	    -log(sin((90.0-*(vals+5))*d2r)))
      /(log(tan((90.0-*(vals+4))*0.5*d2r))
	-log(tan((90.0-*(vals+5))*0.5*d2r)));
  }
/*mf -------------- mf*/
  ogcon = 1.0/gcon;
  ahem = fabs(*(vals+4))/(*(vals+4));
  deg = (90.0-fabs(*(vals+4)))*d2r;
  cn1 = sin(deg);
  cn2 = radius*cn1*ogcon;
  deg = deg*0.5;
  cn3 = tan(deg);
  deg = (90.0-fabs(*vals))*0.5*d2r;
  cn4 = tan(deg);
  rih = cn2*pow((cn4/cn3),gcon);
  deg = (*(vals+1)-*(vals+6))*d2r*gcon;
  xih = rih*sin(deg);
  yih = -rih*cos(deg)*ahem;
  deg = (90.0-grdlat*ahem)*0.5*d2r;
  cn4 = tan(deg);
  rrih = cn2*pow((cn4/cn3),gcon);
  check = 180.0-*(vals+6);
  alnfix = *(vals+6)+check;
  alon = grdlon+check;
  while (alon<0.0) alon = alon+360.0;
  while (alon>360.0) alon = alon-360.0;
  deg = (alon-alnfix)*gcon*d2r;
  x = rrih*sin(deg);
  y = -rrih*cos(deg)*ahem;
  *grdi = *(vals+2)+(x-xih)/(*(vals+7));
  *grdj = *(vals+3)+(y-yih)/(*(vals+8));
}

/* NMC eta ll to xy map  */

void ll2eg (int im, int jm, float *vals,  float grdlon, float grdlat,
	    float *grdi, float *grdj, float *alpha) {

/*  Subroutine to convert from lat-lon to NMC eta i,j.

    Provided by Eric Rogers NMC; converted to C 3/29/95 by Mike Fiorino.

c          SUBROUTINE: ll2eg
c
c          PURPOSE: To compute i- and j-coordinates of a specified
c                   grid given the latitude and longitude points.
c                   All latitudes in this routine start
c                   with -90.0 at the south pole and increase
c                   northward to +90.0 at the north pole.  The
c                   longitudes start with 0.0 at the Greenwich
c                   meridian and increase to the east, so that
c                   90.0 refers to 90.0E, 180.0 is the inter-
c                   national dateline and 270.0 is 90.0W.
c
c          INPUT VARIABLES:
c
c  vals+0    tlm0d: longitude of the reference center point
c
c  vals+1    tph0d: latitude of the reference center point
c
c  vals+2    dlam:  dlon grid increment in deg
c
c  vals+3    dphi:  dlat grid increment in deg
c
c
c            grdlat: latitude of point (grdi,grdj)
c
c            grdlon: longitude of point (grdi,grdj)
c
c            grdi:   i-coordinate(s) that this routine will generate
c                    information for
c
c            grdj:   j-coordinate(s) that this routine will generate
c                    information for
c

*/

  float pi,d2r,r2d, earthr;
  float tlm0d,tph0d,dlam,dphi;
  float phi,lam,lame,lam0,phi0,lam0e,cosphi,sinphi,sinphi0,cosphi0,sinlamr,coslamr;
  float x1,x,y,z,bigphi,biglam,cc,num,den,tlm,tph;

  int idim,jdim;

  pi=3.141592654;

  d2r=pi/180.0;
  r2d=1.0/d2r;
  earthr=6371.2;

  tlm0d=-*(vals+0); /* convert + W to + E, the grads standard for longitude */
  tph0d=*(vals+1);
  dlam=(*(vals+2))*0.5;
  dphi=(*(vals+3))*0.5;

  /* grid point and center of eta grid trig */

  /* convert to radians */

  phi    = grdlat*d2r;
  lam    = -grdlon*d2r; /* convert + W to + E, the grads standard for longitude */
  lame   = (grdlon)*d2r;

  phi0   = tph0d*d2r;
  lam0   = tlm0d*d2r;
  lam0e  = ( 360.0 + *(vals+0) )*d2r;

  /* cos and sin */

  cosphi = cos(phi);
  sinphi = sin(phi);

  sinphi0 = sin(phi0);
  cosphi0 = cos(phi0);

  sinlamr=sin(lame-lam0e);
  coslamr=cos(lame-lam0e);

  x1     = cosphi*cos(lam-lam0);
  x      = cosphi0*x1+sinphi0*sinphi;
  y      = -cosphi*sin(lam-lam0);
  z      = -sinphi0*x1+cosphi0*sinphi;

  /* params for wind rotation alpha */

  cc=cosphi*coslamr;
  num=cosphi*sinlamr;
  den=cosphi0*cc+sinphi0*sinphi;

  tlm=atan2(num,den);


  /* parms for lat/lon -> i,j */

  bigphi = atan(z/(sqrt(x*x+y*y)))*r2d;
  biglam = atan(y/x)*r2d;

  idim = im*2-1;
  jdim = jm*2-1 ;

  *grdi  = (biglam/dlam)+(idim+1)*0.5;
  *grdj  = (bigphi/dphi)+(jdim+1)*0.5;
  *grdi  = (*grdi+1)*0.5-1;
  *grdj  = (*grdj+1)*0.5-1;

  *alpha = asin( ( sinphi0*sin(tlm)) / cosphi ) ;

/*
  printf("ddd %6.2f %6.2f %6.2f %6.2f %g %g %g %g\n",
    grdlon,grdlat,*grdi,*grdj,*alpha,tlm*r2d,cosphi,sinphi0);
*/

}

void ll2pse (int im, int jm, float *vals, float lon, float lat,
	     float *grdi, float *grdj) {


  /* Convert from geodetic latitude and longitude to polar stereographic
     grid coordinates.  Follows mapll by V. J. Troisi.         */
  /* Conventions include that slat and lat must be absolute values */
  /* The hemispheres are controlled by the sgn parameter */
  /* Bob Grumbine 15 April 1994. */

  const float rearth = 6378.273e3;
  const float eccen2 = 0.006693883;
  const float pi = 3.141592654;

  float cdr, alat, along, e, e2;
  float t, x, y, rho, sl, tc, mc;
  float slat,slon,xorig,yorig,sgn,polei,polej,dx,dy;

  slat=*(vals+0);
  slon=*(vals+1);
  polei=*(vals+2);
  polej=*(vals+3);
  dx=*(vals+4)*1000;
  dy=*(vals+5)*1000;
  sgn=*(vals+6);


  xorig = -polei*dx;
  yorig = -polej*dy;

  /*printf("ppp %g %g %g %g %g %g %g\n",slat,slon,polei,polej,dx,dy,sgn);*/

  cdr   = 180./pi;
  alat  = lat/cdr;
  along = lon/cdr;
  e2 = eccen2;
  e  = sqrt(eccen2);

  if ( fabs(lat) > 90.)  {
    *grdi = -1;
    *grdj = -1;
    return;
  }
  else {
    t = tan(pi/4. - alat/2.) /
      pow( (1.-e*sin(alat))/(1.+e*sin(alat)) , e/2.);

    if ( fabs(90. - slat) < 1.E-3) {
      rho = 2.*rearth*t/
	pow( pow(1.+e,1.+e) * pow(1.-e,1.-e) , e/2.);
    }
    else {
      sl = slat/cdr;
      tc = tan(pi/4.-sl/2.) /
	pow( (1.-e*sin(sl))/(1.+e*sin(sl)), (e/2.) );
      mc = cos(sl)/ sqrt(1.-e2*sin(sl)*sin(sl) );
      rho = rearth * mc*t/tc;
    }

    x = rho*sgn*cos(sgn*(along+slon/cdr));
    y = rho*sgn*sin(sgn*(along+slon/cdr));

    *grdi = (x - xorig)/dx+1;
    *grdj = (y - yorig)/dy+1;

    /*printf("ppp (%g %g) (%g %g %g) %g %g\n",lat,lon,x,y,rho,*grdi,*grdj);*/

    return;
  }

}

void ll2ops(float *vals, float lni, float lti, float *grdi, float *grdj) {

  const float radius = 6371229.0 ;
  const float pi = 3.141592654;

  float stdlat, stdlon, xref, yref, xiref, yjref, delx , dely;

  float plt,pln;
  double pi180,c1,c2,c3,c4,c5,c6,arg2a,bb,plt1,alpha, pln1,plt90,argu1,argu2;

  double hsign,glor,rstdlon,glolim,facpla,x,y;

  stdlat = *(vals+0);
  stdlon = *(vals+1);
  xref = *(vals+2);
  yref = *(vals+3);
  xiref = *(vals+4);
  yjref = *(vals+5);
  delx = *(vals+6);
  dely = *(vals+7);

  c1=1.0 ;
  pi180 = asin(c1)/90.0;

/*
c
c     set flag for n/s hemisphere and convert longitude to <0 ; 360> interval
c
*/
  if(stdlat >= 0.0) {
    hsign= 1.0 ;
  } else {
    hsign=-1.0 ;
  }
/*
c
c     set flag for n/s hemisphere and convert longitude to <0 ; 360> interval
c
*/
  glor=lni ;
  if(glor <= 0.0) glor=360.0+glor ;
  rstdlon=stdlon;
  if(rstdlon < 0.0) rstdlon=360.0+stdlon;

/*
c
c     test for a n/s pole case
c
*/
  if(stdlat == 90.0) {
    plt=lti ;
    pln=fmod(glor+270.0,360.0) ;
    goto l2000;
  }

  if(stdlat == -90.0) {
    plt=-lti ;
    pln=fmod(glor+270.0,360.0) ;
    goto l2000;
  }


/*
c
c     test for longitude on 'greenwich or date line'
c
*/
  if(glor == rstdlon) {
    if(lti > stdlat) {
      plt=90.0-lti+stdlat;
      pln=90.0;
    } else {
      plt=90.0-stdlat+lti;
      pln=270.0;;
    }
    goto l2000;
  }

  if(fmod(glor+180.0,360.0) == rstdlon) {
    plt=stdlat-90.0+lti;
    if(plt < -90.0) {
      plt=-180.0-plt;
      pln=270.0;
    } else {
      pln= 90.0;
    }
    goto l2000 ;
  }

/*
c
c     determine longitude distance relative to rstdlon so it belongs to
c     the absolute interval 0 - 180
c
*/
  argu1 = glor-rstdlon;
  if(argu1 > 180.0) argu1 = argu1-360.0;
  if(argu1 < -180.0) argu1 = argu1+360.0;

/*
c
c     1. get the help circle bb and angle alpha (legalize arguments)
c
*/

  c2=lti*pi180 ;
  c3=argu1*pi180 ;
  arg2a = cos(c2)*cos(c3) ;
  if( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = max1(arg2a,-c1)  */
  if(  c1 < arg2a ) arg2a = c1 ; /* min1(arg2a, c1)         */
  bb = acos(arg2a) ;

  c4=hsign*lti*pi180 ;
  arg2a = sin(c4)/sin(bb) ;
  if( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if(  c1 < arg2a ) arg2a = c1  ; /* arg2a = dmin1(arg2a, c1) */
  alpha = asin(arg2a) ;
/*
c
c     2. get plt and pln (still legalizing arguments)
c
*/
  c5=stdlat*pi180 ;
  c6=hsign*stdlat*pi180 ;
  arg2a = cos(c5)*cos(bb) + sin(c6)*sin(c4) ;
  if( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if(  c1 < arg2a ) arg2a = c1  ; /* arg2a = dmin1(arg2a, c1) */
  plt1   = asin(arg2a) ;

  arg2a = sin(bb)*cos(alpha)/cos(plt1) ;

  if( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if(  c1 < arg2a ) arg2a =  c1 ; /* arg2a = dmin1(arg2a, c1) */
  pln1   = asin(arg2a) ;


/*
c
c    test for passage of the 90 degree longitude (duallity in pln)
c         get plt for which pln=90 when lti is the latitude
c
*/
  arg2a = sin(c4)/sin(c6) ;
  if( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if(  c1 < arg2a ) arg2a =  c1 ; /* arg2a = dmin1(arg2a, c1) */
  plt90 = asin(arg2a) ;

/*
c
c         get help arc bb and angle alpha
c
*/
  arg2a = cos(c5)*sin(plt90) ;
  if( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if(  c1 < arg2a ) arg2a =  c1 ; /* arg2a = dmin1(arg2a, c1) */
  bb    = acos(arg2a) ;

  arg2a = sin(c4)/sin(bb) ;
  if( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if(  c1 < arg2a ) arg2a =  c1 ; /* arg2a = dmin1(arg2a, c1) */
  alpha = asin(arg2a) ;

/*
c
c         get glolim - it is nesc. to test for the existence of solution
c
*/
  argu2  = cos(c2)*cos(bb) / (1.-sin(c4)*sin(bb)*sin(alpha)) ;
  if( fabs(argu2) > c1 ) {
    glolim = 999.0;
  } else {
    glolim = acos(argu2)/pi180;
  }

/*
c
c     modify (if nesc.) the pln solution
c
*/
  if( ( fabs(argu1) > glolim && lti <= stdlat ) || ( lti > stdlat ) ) {
    pln1 = pi180*180.0 - pln1;
  }
/*
c
c     the solution is symmetric so the direction must be if'ed
c
*/
  if(argu1 < 0.0) {
    pln1 = -pln1;
  }
/*
c
c     convert the radians to degrees
c
*/
  plt = plt1/pi180 ;
  pln = pln1/pi180 ;


/*
c
c     to obtain a rotated value (ie so x-axis in pol.ste. points east)
c     add 270 to longitude
c
*/
  pln=fmod(pln+270.0,360.0) ;

 l2000:

/*
c
c     this program convert polar stereographic coordinates to x,y ditto
c     longitude:   0 - 360  ; positive to the east
c     latitude : -90 -  90  ; positive for northern hemisphere
c     it is assumed that the x-axis point towards the east and
c     corresponds to longitude = 0
c
c     tsp 20/06-89
c
c     constants and functions
c
*/
  facpla = radius*2.0/(1.0+sin(plt*pi180))*cos(plt*pi180);
  x = facpla*cos(pln*pi180) ;
  y = facpla*sin(pln*pi180)  ;

  *grdi=(x-xref)/delx + xiref;
  *grdj=(y-yref)/dely + yjref;

  return;

}


#if USESDF == 1
#ifdef STNDALN

#define Success 1
#define Failure 0

/* initialize a netCDF standard file structure */
int
init_io_std (std_ptr)
IO_STD    **std_ptr;		/* pointer to netCDF data structure */
{
  int         init_dim_info ();
  void        free_io_std ();

  if ((*std_ptr) != NULL)
    free_io_std (std_ptr);

  if (((*std_ptr) = (IO_STD *) malloc (sizeof (IO_STD))) == NULL)
  {
	printf ("Could not allocate memory for netCDF/HDF-SDS structure.\n");
	return Failure;
  }

  /* initialize contents of structure */
  (*std_ptr)->cdfid = -1;
  (*std_ptr)->ndims = 0;
  (*std_ptr)->nvars = 0;
  (*std_ptr)->ngatts = 0;
  (*std_ptr)->recdim = -1;
  (*std_ptr)->time_type = CDC;

  (*std_ptr)->first_gattr = NULL;

  /* don't create variable list quite yet */
  (*std_ptr)->var = NULL;

  /* intialize dimension information */
  if (init_dim_info ((*std_ptr)->dimids, (*std_ptr)->dimnam,
		     (*std_ptr)->dimsiz) != Success)
    return Failure;

  return Success;
}

/* initialize dimension information */
int
init_dim_info (dimids, dimnam, dimsiz)
int        *dimids;		/* array of dimension IDs   */
char        dimnam[MAX_NC_DIMS][MAX_NC_NAME + 1];	/* array of dimension
							 * names */
long       *dimsiz;		/* array of dimension sizes */
{
  int         i;

  /* set all dimension information to bogus values */
  for (i = 0; i < MAX_NC_DIMS; i++)
  {
    dimids[i] = -1;
    dimnam[i][0] = '\0';
    dimsiz[i] = -1;
  }

  return Success;
}

/* free a netCDF standard file structure */
void
free_io_std (std_ptr)
IO_STD    **std_ptr;
{
  void        free_var_info ();
  int         free_netcdf_att_list ();

  if (*std_ptr != NULL)
  {
    /* if the variable list exists, free it */
    if ((*std_ptr)->var != NULL)
      free_var_info (&((*std_ptr)->var));

    /* free the global attributes if they have been allocated */
    if ((*std_ptr)->first_gattr != NULL)
      free_netcdf_att_list (&((*std_ptr)->first_gattr));

    /* now free the IO_STD structure itself */
    free (*std_ptr);
    *std_ptr = NULL;
  }

  return;
}

/* free a variable structure */
void
free_var_info (var)
VAR_INFO  **var;
{
  void        free_var_info ();
  int	      free_netcdf_att_list ();

  /* go through list to the end and free all variable structures */
  if ((*var)->next != NULL)
    free_var_info (&((*var)->next));

  if ((*var)->first_vattr != NULL)
    free_netcdf_att_list (&((*var)->first_vattr));

  /* if data space has been allocated, free it */
  if ((*var)->data != NULL)
    free ((*var)->data);

  /* now free the variable structure itself */
  free (*var);
  *var = NULL;

  return;
}

/* Free a netCDF attribute list. */
int
free_netcdf_att_list (first_attr)
struct attrib_list **first_attr;	/* First attribute in list. */
{
  struct attrib_list *temp_attr,
             *save_attr;

  temp_attr = *first_attr;
  while (temp_attr != NULL)
  {
    save_attr = temp_attr->next;
    if (temp_attr->data != NULL)
      free (temp_attr->data);
    free (temp_attr);
    temp_attr = save_attr;
  }
  *first_attr = NULL;
  return Success;
}

#endif
#endif
