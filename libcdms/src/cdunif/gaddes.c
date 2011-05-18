/* Copyright (C) 1988-2010 by Brian Doty and the 
   Institute of Global Environment and Society (IGES).  
   See file COPYRIGHT for more information.   */

/* Authored by B. Doty */

#ifdef HAVE_CONFIG_H
#include "config.h"
/* If autoconfed, only include malloc.h when it's present */
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#else /* undef HAVE_CONFIG_H */
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "grads.h"
#include <string.h>

extern struct gamfcmn mfcmn;
static char pout[256];
static FILE *pdfi;       /* File descriptor for pdef file */
FILE *descr;             /* File descriptor pointer */

void ll2eg  (gaint, gaint, gadouble *, gadouble, gadouble, gadouble *, gadouble *, gadouble *);
void ll2pse (gaint, gaint, gadouble *, gadouble, gadouble, gadouble *, gadouble *);
void ll2ops (gadouble *, gadouble , gadouble , gadouble *, gadouble *);


/* Read a GrADS data descriptor file and fill in the information
   into a gafile structure.  The gafile structure should be
   allocated and must be initialized by getpfi.  If this routine
   returns an error, release the pfi structure and allocated
   storage via frepfi.  mflag indicates whether to read the
   stnmap/index file; if this routine is being called to
   preprocess the dd file then this flag should be 0. 
   A mflag value of 2 will check if the stnmap/index file can be
   opened, but will not read it; mflag==2 will also turn off 
   calculation of the pdef interpolation values */

gaint gaddes (char *name, struct gafile *pfi, gaint mflag) {
  struct gavar *pvar,*pvar2;
  struct gaens *ens;
  struct dt tdef,tdefe,tdefi,dt1,dt2;
  struct gaindx *pindx;
  struct gaindxb *pindxb;
  struct gag2indx *g2indx;
  struct gaattr *attrib;
  struct gachsub *pchsub;
  gadouble *vals,sf;
  gadouble v1,v2,ev1,ev2,temp;
  FILE *mfile;
  gafloat fdum;
  off_t levs,acum,acumvz,recacm;
  gaint pdefop1=0, pdefop2=0;
  gaint acumstride=0, npairs, idum, reclen;
  gaint size=0,rc,len,swpflg,cnt,flag,tim1,tim2,ichar;
  gaint flgs[8],e,t,i,j,ii,jj,err,hdrb,trlb,mflflg;
  gaint mcnt,maxlv,foundvar1,foundvar2;
  size_t sz;
  char rec[512], mrec[512], *ch, *pos, *sname, *vectorpairs, *pair, *vplist;
  char pdefnm[256],var1[256],var2[256];
  char *varname,*attrname,*attrtype;
  unsigned char vermap, urec[8];
  static char *errs[9] = {"XDEF","YDEF","ZDEF","TDEF","UNDEF",
			    "DSET","VARS","TITLE","DTYPE"};

 /*mf --- define here vice grads.c for cdunif.c mf*/
  mfcmn.fullyear=-999; 

  /* initialize variables */
  hdrb = 0;
  trlb = 0;
  pdfi = NULL;
  mflflg = 0;		
  mfile = NULL;
  pfi->mfile = NULL;
  mcnt = -1;
  vectorpairs = NULL;
  pair = NULL;
  vplist = NULL;
  varname = attrname = attrtype = NULL;
  attrib = NULL;
  sname=NULL;

  /* Try to open descriptor file */
  descr = fopen (name, "r");
  if (descr == NULL) {
    /* Try adding default suffix of .ctl */
    sz = strlen(name)+5;
    if ((sname = (char *)galloc(sz,"sname2")) == NULL) {
      gaprnt(0,"memory allocation error in creating date descriptor file name\n");
      return(1);
    }
    for(i=0;i<=strlen(name);i++) *(sname+i)=*(name+i);
    strcat(sname,".ctl");
    descr = fopen (sname, "r");
  }

  /* If still can't open descriptor file, give up */
  if (descr == NULL) {
    gaprnt (0,"Open Error:  Can't open description file\n");
    if (sname) gree(sname,"f172");
    return(1);
  }

  /* Copy descriptor file name into gafile structure */
  if (sname != NULL) {
    getwrd (pfi->dnam,sname,512);
    gree(sname,"f173");
  } else {
    getwrd (pfi->dnam,name,512);
  }

  /* initialize error flags */
  for (i=0;i<8;i++) flgs[i] = 1;

  /* Parse the data descriptor file */
  while (fgets(rec,512,descr)!=NULL) {

    /* Remove any leading blanks from rec */
    reclen = strlen(rec);
    jj = 0;
    while (jj<reclen && rec[0]==' ') {
      for (ii=0; ii<reclen; ii++) rec[ii] = rec[ii+1];
      jj++;
    }
    /* replace newline with null at end of record */
    for (ichar = strlen(rec) - 1 ;  ichar >= 0 ;  --ichar) {
      if (rec[ichar] == '\n') {
	rec[ichar] = '\0' ;
	break ; 
      }
    }
    /* Keep mixed case and lower case versions of rec handy */
    strcpy (mrec,rec);   
    lowcas(rec);

    if (!isalnum(mrec[0])) {
      /* check if comment contains attribute metadata */
      if ((strncmp("*:attr",mrec,6)==0) || (strncmp("@",mrec,1)==0)) {
      	if ((ddfattr(mrec,pfi)) == -1) goto retrn;
      }

    } else if (cmpwrd("byteswapped",rec)) {
      pfi->bswap = 1;

    } else if (cmpwrd("fileheader",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing fileheader length\n");
      } else {
        ch = longprs(ch,&(pfi->fhdr));
        if (ch==NULL) {
          gaprnt (1,"Fileheader record invalid\n");
          pfi->fhdr = 0;
        }
      }

    } else if (cmpwrd("cachesize",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing cachesize value\n");
      } else {
        ch = longprs(ch,&(pfi->cachesize));
        if (ch==NULL) {
	  gaprnt (1,"cachesize value invalid\n");
	  pfi->cachesize = -1;
        } 
      }

    } else if (cmpwrd("xyheader",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing xy grid header length\n");
      } else {
        ch = longprs(ch,&(pfi->xyhdr));
        if (ch==NULL) {
	  gaprnt (1,"xy grid header length invalid\n");
	  pfi->xyhdr = 0;
        } else {
	  pfi->xyhdr = pfi->xyhdr/4;
	}
      }

    } else if (cmpwrd("unpack",rec)) {
      if ( (ch=nxtwrd(mrec))==NULL ) {
        gaprnt (1,"Descriptor File Warning: Missing attribute names in unpack record\n");
      } else {
	/* get the scale factor attribute name */
	len = 0;
	while (*(ch+len)!=' ' && *(ch+len)!='\n' && *(ch+len)!='\t') len++;
	sz = len+3;
	if ((pfi->scattr = (char *)galloc(sz,"scattr")) == NULL) goto err8;
	for (i=0; i<len; i++) *(pfi->scattr+i) = *(ch+i);
	*(pfi->scattr+len) = '\0';
	/* set the packflg to 1, meaning only scale factor has been retrieved */
	pfi->packflg = 1;
	
	/* get the offset attribute name */
	if ( (ch=nxtwrd(ch)) == NULL ) {
	  gaprnt (2,"Descriptor File Warning: No offset attribute name in unpack record\n");
	} else {
	  len = 0;
	  while (*(ch+len)!=' ' && *(ch+len)!='\n' && *(ch+len)!='\t') len++;
	  sz = len+3;
	  if ((pfi->ofattr = (char *)galloc(sz,"ofattr")) == NULL) goto err8;
	  for (i=0; i<len; i++) *(pfi->ofattr+i) = *(ch+i);
	  *(pfi->ofattr+len) = '\0';
	  /* Set the packflg to 2, meaning scale factor and offset have been retrieved */
	  pfi->packflg = 2;
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
#if GRIB2
          else if (cmpwrd("pascals",ch)) pfi->pa2mb = 1;
#endif
          else if (cmpwrd("365_day_calendar",ch)) {
	    pfi->calendar=1;
	    mfcmn.cal365=pfi->calendar;
	  }
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

    } else if (cmpwrd("headerbytes",rec)|| cmpwrd("theader",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"headerbytes/theader record invalid\n");
      } else {
        ch = intprs(ch,&hdrb);
        if (ch==NULL) {
          gaprnt (1,"headerbytes/theader record invalid\n");
          hdrb = 0;
        } else {
          hdrb = hdrb/4;
        }
      }

    /* Handle the chsub records.  time1, time2, then a string,  multiple times */
    } else if (cmpwrd("chsub",rec)) {
      /* point to first block in chain */
      pchsub = pfi->pchsub1;    
      if (pchsub!=NULL) {
        while (pchsub->forw!=NULL) {
          pchsub = pchsub->forw;       /* advance to end of chain */
        }
      }
      flag = 0;
      ch = mrec;
      while (1) {
        if ( (ch=nxtwrd(ch)) == NULL ) break;
        flag = 1;
        if ( (ch = intprs(ch,&tim1)) == NULL) break;
        if ( (ch=nxtwrd(ch)) == NULL ) break;
        if (*ch=='*' && (*(ch+1)==' '||*(ch+1)=='\t')) tim2 = -99;
        else if ( (ch = intprs(ch,&tim2)) == NULL) break;
        if ( (ch=nxtwrd(ch)) == NULL ) break;
        flag = 0;
        if (pchsub) {   /* chain exists */
	  sz = sizeof(struct gachsub);
          pchsub->forw = (struct gachsub *)galloc(sz,"chsubnew");
          if (pchsub->forw==NULL) {
	    gaprnt(0,"Open Error: memory allocation failed for pchsub\n");
	    goto err8; 
	  }
          pchsub = pchsub->forw;
	  pchsub->forw = NULL;
        } else {        /* start a new chain */
	  sz = sizeof(struct gachsub);
          pfi->pchsub1 = (struct gachsub *)galloc(sz,"chsub1");
          if (pfi->pchsub1==NULL)  {
	    gaprnt(0,"Open Error: memory allocation failed for pchsub1\n");
	    goto err8; 
	  }
          pchsub = pfi->pchsub1;
	  pchsub->forw = NULL;
        }
        len = wrdlen(ch);
	sz = len+1;
        if ((pchsub->ch = (char *)galloc(sz,"chsubstr")) == NULL) goto err8;
        getwrd(pchsub->ch,ch,len);
        pchsub->t1 = tim1;
        pchsub->t2 = tim2;
      }
      if (flag) {
        gaprnt (1,"Description file warning: Invalid chsub record; Ignored\n");
      }

    } else if (cmpwrd("dtype",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing data type\n");
        gaprnt (1,"   Assuming data file type is grid\n");
      }
      if (cmpwrd("station",ch)) {
        pfi->idxflg = 1;
        pfi->type = 2;
        flgs[0] = 0;
        flgs[1] = 0;
        flgs[2] = 0;

      } else if (cmpwrd("bufr",ch)) {
	pfi->idxflg = 0;  /* bufr data is not indexed */
	pfi->type = 2;    /* station data type */
	mflag = 0;        /* don't try to read a stnmap file */
	pfi->bufrflg = 1; 
        flgs[0] = 0;
        flgs[1] = 0;
        flgs[2] = 0;
	/* allocate memory for the bufrinfo structure and the two bufrtimeinfo structures */
	sz = sizeof(struct bufrinfo);
	if ((pfi->bufrinfo = (struct bufrinfo *)galloc(sz,"bufrinfo")) == NULL) goto err8;
	/* initialize with bad values */
	for (j=0;j<2;j++) {
	  pfi->bufrinfo->lonxy[j] = pfi->bufrinfo->latxy[j] = -999;;
	  pfi->bufrinfo->levxy[j] = pfi->bufrinfo->stidxy[j] = -999;;
	  pfi->bufrinfo->base.yrxy[j] = pfi->bufrinfo->base.moxy[j] = -999;;
	  pfi->bufrinfo->base.dyxy[j] = pfi->bufrinfo->base.hrxy[j] = -999;;
	  pfi->bufrinfo->base.mnxy[j] = pfi->bufrinfo->offset.yrxy[j] = -999;;
	  pfi->bufrinfo->offset.moxy[j] = pfi->bufrinfo->offset.dyxy[j] = -999;;
	  pfi->bufrinfo->offset.hrxy[j] = pfi->bufrinfo->offset.mnxy[j] = -999;;
	}
      } else if (cmpwrd("grib",ch)) {
        pfi->idxflg = 1;
        if ( (ch=nxtwrd(ch))!=NULL ) {
          if ( intprs(ch,&(pfi->grbgrd))==NULL) {
            gaprnt (1,"Description file warning: Invalid GRIB option\n");
            pfi->grbgrd = -999;
          }
        }
      }
#if GRIB2
      else if (cmpwrd("grib2",ch)) pfi->idxflg = 2;
#endif
#if USENETCDF
      else if (cmpwrd("netcdf",ch)) pfi->ncflg = 1;
#endif
#if USEHDF
      else if (cmpwrd("hdfsds",ch) || cmpwrd("hdf4",ch)) pfi->ncflg = 2;
#endif
#if USEHDF5
      else if (cmpwrd("hdf5_grid",ch)) pfi->ncflg = 3;
#endif
      else {
        gaprnt (0,"Open Error:  Data file type invalid\n");
        goto err9;
      }

    } else if (cmpwrd("xvar",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing x,y pair for XVAR entry\n");
      } else {
        j = 0;
        while (1) {
 	  if ( (ch=intprs(ch,&(pfi->bufrinfo->lonxy[j])))==NULL ) goto err6a;
          while (*ch==' ') ch++;
          if (*ch!=',') break;
          ch++;
          while (*ch==' ') ch++;
          j++;
          if (j>1) goto err6a;
        }
	if (pfi->bufrinfo->lonxy[0]==-999 || pfi->bufrinfo->lonxy[1]==-999) goto err6a;
      }
  
    } else if (cmpwrd("yvar",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing x,y pair for YVAR entry\n");
      } else {
        j = 0;
        while (1) {
 	  if ( (ch=intprs(ch,&(pfi->bufrinfo->latxy[j])))==NULL ) goto err6a;
          while (*ch==' ') ch++;
          if (*ch!=',') break;
          ch++;
          while (*ch==' ') ch++;
          j++;
          if (j>1) goto err6a;
        }
	if (pfi->bufrinfo->latxy[0]==-999 || pfi->bufrinfo->latxy[1]==-999) goto err6a;
      }
  
    } else if (cmpwrd("zvar",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing x,y pair for ZVAR entry\n");
      } else {
        j = 0;
        while (1) {
 	  if ( (ch=intprs(ch,&(pfi->bufrinfo->levxy[j])))==NULL ) goto err6a;
          while (*ch==' ') ch++;
          if (*ch!=',') break;
          ch++;
          while (*ch==' ') ch++;
          j++;
          if (j>1) goto err6a;
        }
	if (pfi->bufrinfo->levxy[0]==-999 || pfi->bufrinfo->levxy[1]==-999) goto err6a;
      }
  
    } else if (cmpwrd("tvar",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing x,y pairs for TVAR entry\n");
      } else {
	while (nxtwrd(ch)!=NULL) {
	  if (cmpwrd("yr",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TVAR yr entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->base.yrxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->base.yrxy[0]==-999 || pfi->bufrinfo->base.yrxy[1]==-999) goto err6a;
	    }

	  } else if (cmpwrd("mo",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TVAR mo entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->base.moxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->base.moxy[0]==-999 || pfi->bufrinfo->base.moxy[1]==-999) goto err6a;
	    }
	  } else if (cmpwrd("dy",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TVAR dy entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->base.dyxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->base.dyxy[0]==-999 || pfi->bufrinfo->base.dyxy[1]==-999) goto err6a;
	    }
	  } else if (cmpwrd("hr",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TVAR hr entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->base.hrxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->base.hrxy[0]==-999 || pfi->bufrinfo->base.hrxy[1]==-999) goto err6a;
	    }
	  } else if (cmpwrd("mn",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TVAR mn entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->base.mnxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->base.mnxy[0]==-999 || pfi->bufrinfo->base.mnxy[1]==-999) goto err6a;
	    }
	  } else if (cmpwrd("sc",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TVAR sc entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->base.scxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->base.scxy[0]==-999 || pfi->bufrinfo->base.scxy[1]==-999) goto err6a;
	    }
	  } else {
	    goto err6a;
	  }
	} /* end of while loop */
      }

    } else if (cmpwrd("toffvar",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing x,y pairs for TOFFVAR entry\n");
      } else {
	while (nxtwrd(ch)!=NULL) {
	  if (cmpwrd("yr",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TOFFVAR yr entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->offset.yrxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->offset.yrxy[0]==-999 || pfi->bufrinfo->offset.yrxy[1]==-999) goto err6a;
	    }

	  } else if (cmpwrd("mo",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TOFFVAR mo entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->offset.moxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->offset.moxy[0]==-999 || pfi->bufrinfo->offset.moxy[1]==-999) goto err6a;
	    }
	  } else if (cmpwrd("dy",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TOFFVAR dy entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->offset.dyxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->offset.dyxy[0]==-999 || pfi->bufrinfo->offset.dyxy[1]==-999) goto err6a;
	    }
	  } else if (cmpwrd("hr",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TOFFVAR hr entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->offset.hrxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->offset.hrxy[0]==-999 || pfi->bufrinfo->offset.hrxy[1]==-999) goto err6a;
	    }
	  } else if (cmpwrd("mn",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TOFFVAR mn entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->offset.mnxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->offset.mnxy[0]==-999 || pfi->bufrinfo->offset.mnxy[1]==-999) goto err6a;
	    }
	  } else if (cmpwrd("sc",ch)) {
	    if ((ch=nxtwrd(ch))==NULL) {
	      gaprnt (1,"Description file warning: Missing x,y pair for TOFFVAR sc entry\n");
	    } else {
	      j = 0;
	      while (1) {
		if ( (ch=intprs(ch,&(pfi->bufrinfo->offset.scxy[j])))==NULL ) goto err6a;
		while (*ch==' ') ch++;
		if (*ch!=',') break;
		ch++;
		while (*ch==' ') ch++;
		j++;
		if (j>1) goto err6a;
	      }
	      if (pfi->bufrinfo->offset.scxy[0]==-999 || pfi->bufrinfo->offset.scxy[1]==-999) goto err6a;
	    }
	  } else {
	    goto err6a;
	  }
	} /* end of while loop */
      }

    } else if (cmpwrd("stid",rec)) {
      if ( (ch=nxtwrd(rec))==NULL ) {
        gaprnt (1,"Description file warning: Missing x,y pair for STID entry\n");
      } else {
        j = 0;
        while (1) {
 	  if ( (ch=intprs(ch,&(pfi->bufrinfo->stidxy[j])))==NULL ) goto err6a;
          while (*ch==' ') ch++;
          if (*ch!=',') break;
          ch++;
          while (*ch==' ') ch++;
          j++;
          if (j>1) goto err6a;
        }
	if (pfi->bufrinfo->stidxy[0]==-999 || pfi->bufrinfo->stidxy[1]==-999) goto err6a;
      }

    } else if (cmpwrd("title",rec)) {
      if ( (ch=nxtwrd(mrec))==NULL ) {
        gaprnt (1,"Description file warning: Missing title string\n");
      } else {
        getstr (pfi->title,ch,512);
        flgs[7] = 0;
      }

    } else if (cmpwrd("dset",rec)) {
      ch = nxtwrd(mrec);
      if (ch==NULL) {
        gaprnt (0,"Descriptor File Error:  Data file name is missing\n");
        goto err9;
      }
      if (*ch=='^' || *ch=='$') {
        fnmexp (pfi->name,ch,name);
      } else {
        getwrd (pfi->name,ch,512);
      }
      flgs[5] = 0;

    } else if (cmpwrd("stnmap",rec) || cmpwrd("index",rec)) {
      ch = nxtwrd(mrec);
      if (ch==NULL) {
        gaprnt (0,"Open Error:  Station or Index Map file name is missing\n");
        goto err9;
      }
      if (*ch=='^' || *ch=='$') {
        fnmexp (pout, ch, name);
      } else {
        getwrd (pout, ch, 500);  
      }
      len = 0;
      while (*(pout+len)) len++;
      sz = len+3;
      if ((pfi->mnam = (char *)galloc(sz,"mnam")) == NULL) goto err8; 
      strcpy (pfi->mnam,pout);

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
      ch = nxtwrd(mrec);
      if (ch==NULL) {
        gaprnt (0,"Open Error:  Missing undef value\n");
        goto err9;
      }
      
      pos = getdbl(ch,&(pfi->undef));
      if (pos==NULL) {
        gaprnt (0,"Open Error:  Invalid undef value\n");
        goto err9;
      } 

      /* Get the undef attribute name, if it's there */
      if ( (ch=nxtwrd(ch))!=NULL ) {
	len = 0;
	while (*(ch+len)!=' ' && *(ch+len)!='\n' && *(ch+len)!='\t') len++;
	sz = len+3;
	if ((pfi->undefattr = (char *)galloc(sz,"undefattr3")) == NULL) goto err8;
	for (i=0; i<len; i++) *(pfi->undefattr+i) = *(ch+i);
	*(pfi->undefattr+len) = '\0';
	/* Set the undef attribute flag */
	pfi->undefattrflg = 1;
      }
      pfi->ulow = fabs(pfi->undef/EPSILON);
      pfi->uhi  = pfi->undef + pfi->ulow;
      pfi->ulow = pfi->undef - pfi->ulow;
      flgs[4] = 0;

    } else if (cmpwrd("pdef",rec)) {
      if ((ch = nxtwrd(rec)) == NULL) goto errm;
      /* parse the i and j dimensions of the pre-projected grid */
      if ((pos = intprs(ch,&(pfi->ppisiz)))==NULL) goto errm;
      if ((ch = nxtwrd(ch)) == NULL) goto errm;
      if ((pos = intprs(ch,&(pfi->ppjsiz)))==NULL) goto errm;
      if ((ch = nxtwrd(ch)) == NULL) goto errm;

      /* set the pre-projected grid type and wind rotation flags */
      if (cmpwrd("nps",ch))         {pfi->ppflag=1; pfi->ppwrot=1; cnt=4;}
      else if (cmpwrd("sps",ch))    {pfi->ppflag=2; pfi->ppwrot=1; cnt=4;}
      else if (cmpwrd("lcc",ch))    {pfi->ppflag=3; pfi->ppwrot=0; cnt=9;}
      else if (cmpwrd("lccr",ch))   {pfi->ppflag=3; pfi->ppwrot=1; cnt=9;}
      else if (cmpwrd("eta.u",ch))  {pfi->ppflag=4; pfi->ppwrot=1; cnt=4;}
      else if (cmpwrd("pse",ch))    {pfi->ppflag=5; pfi->ppwrot=0; cnt=7;}
      else if (cmpwrd("ops",ch))    {pfi->ppflag=6; pfi->ppwrot=0; cnt=8;}
      else if (cmpwrd("bilin",ch))  {pfi->ppflag=7; pfi->ppwrot=1; cnt=0;}
      else if (cmpwrd("file",ch))   {pfi->ppflag=8; pfi->ppwrot=1; cnt=1; pfi->pdefgnrl=0;}
      else if (cmpwrd("general",ch)){pfi->ppflag=8; pfi->ppwrot=1; cnt=1; pfi->pdefgnrl=1;}
      else if (cmpwrd("rotll",ch))  {pfi->ppflag=9; pfi->ppwrot=0; cnt=6;}
      else if (cmpwrd("rotllr",ch)) {pfi->ppflag=9; pfi->ppwrot=1; cnt=6;}
      else goto errm;
      /* parse the pre-projected grid parameters */
      for (i=0; i<cnt; i++) {
        if ( (ch = nxtwrd(ch)) == NULL) goto errm;
        if ( (pos = getdbl(ch,&(pfi->ppvals[i])))==NULL) goto errm;
      }
      /* check "num" argument to pdef file/general option */
      if (pfi->ppflag==8) {
        i = (gaint)(pfi->ppvals[0]+0.1);
        if (i<1 || i>9) goto errm;
      }
      /* parse file type, byte order, and name for pdef 'bilin' and 'file' and 'general' */
      if (pfi->ppflag==7 || pfi->ppflag==8) {
        if ( (ch = nxtwrd(ch)) == NULL) goto errm;
        if (cmpwrd("stream",ch))             pdefop1 = 1;
        else if (cmpwrd("sequential",ch))    pdefop1 = 2;
        else goto errm;

        if ( (ch = nxtwrd(ch)) == NULL) goto errm;
        if (cmpwrd("binary",ch))             pdefop2 = 1;
        else if (cmpwrd("binary-big",ch))    pdefop2 = 2;
        else if (cmpwrd("binary-little",ch)) pdefop2 = 3;
        else if (cmpwrd("packed",ch))        pdefop2 = 4;
        else goto errm;

        if ( (ch = nxtwrd(ch)) == NULL) goto errm;
	ch = mrec + (ch-rec);
        if (*ch=='^' || *ch=='$') {
          fnmexp (pdefnm,ch,name);
        } else {
          getwrd (pdefnm,ch,256);
        }
	/* open the pdef file */
        pdfi = fopen(pdefnm,"rb");
        if (pdfi==NULL) {
          snprintf(pout,255, "  Error opening pdef file:  %s\n",pdefnm);
          gaprnt (0,pout);
          goto errm;
        }
      }

    }  else if (cmpwrd("vectorpairs",rec)) {
      if ( (ch=nxtwrd(mrec))==NULL ) {
        gaprnt (1,"Description file warning: No vector pairs listed\n");
      } else {
	sz = strlen(ch)+1;
	if ((vectorpairs = (char *)galloc(sz,"vecpairs")) == NULL) goto err8;
        getstr(vectorpairs,ch,strlen(ch)+1);
      }
	
    }  else if (cmpwrd("xdef",rec)) {

      if (pfi->type == 2) continue;
      if ( (ch = nxtwrd(rec)) == NULL) goto err1;
      if ( (pos = intprs(ch,&(pfi->dnum[0])))==NULL) goto err1;
      if (pfi->dnum[0]<1) {
	snprintf(pout,255,"Warning: Invalid XDEF syntax in %s -- Changing size of X axis from %d to 1 \n",
		pfi->dnam,pfi->dnum[0]);
        gaprnt (1,pout);
	pfi->dnum[0] = 1;
      }
      if (*pos!=' ') goto err1;
      if ( (ch = nxtwrd(ch))==NULL) goto err2;
      if (cmpwrd("linear",ch)) {
        rc = deflin(ch, pfi, 0, 0);
        if (rc==-1) goto err8; 
        if (rc) goto err9;
        v2 = *(pfi->grvals[0]);
        v1 = *(pfi->grvals[0]+1) + v2;
        temp = v1+((gadouble)(pfi->dnum[0]))*v2;
        temp=temp-360.0;
        if (fabs(temp-v1)<0.01) pfi->wrap = 1;
      }
      else if (cmpwrd("levels",ch)) {
        rc = deflev (ch, rec, pfi, 0);
        if (rc==-1)  goto err8; 
        if (rc) goto err9;
      } else goto err2;
      flgs[0] = 0;

    } else if (cmpwrd("ydef",rec)) {
      if (pfi->type == 2) continue;
      if ( (ch = nxtwrd(rec)) == NULL) goto err1;
      if ( (pos = intprs(ch,&(pfi->dnum[1])))==NULL) goto err1;
      if (pfi->dnum[1]<1) {
	snprintf(pout,255,"Warning: Invalid YDEF syntax in %s -- Changing size of Y axis from %d to 1 \n",
		pfi->dnam,pfi->dnum[1]);
        gaprnt (1,pout);
	pfi->dnum[1] = 1;
      }
      if (*pos!=' ') goto err1;
      if ( (ch = nxtwrd(ch))==NULL) goto err2;
      if (cmpwrd("linear",ch)) {
        rc = deflin(ch, pfi, 1, 0);
        if (rc==-1) goto err8; 
        if (rc) goto err9;
      } else if (cmpwrd("levels",ch)) {
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
      } else if (cmpwrd("gaust62",ch)) {
        if ( (ch = nxtwrd(ch))==NULL) goto err3;
        if ( (pos = intprs(ch,&i))==NULL) goto err3;
        pfi->grvals[1] = gagst62(i,pfi->dnum[1]);
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
      if (pfi->dnum[2]<1) {
	snprintf(pout,255,"Warning: Invalid ZDEF syntax in %s -- Changing size of Z axis from %d to 1 \n",
		pfi->dnam,pfi->dnum[2]);
        gaprnt (1,pout);
	pfi->dnum[2] = 1;
      }
      if (*pos!=' ') goto err1;
      if ( (ch = nxtwrd(ch))==NULL) goto err2;
      if (cmpwrd("linear",ch)) {
        rc = deflin(ch, pfi, 2, 0);
        if (rc==-1) goto err8; 
        if (rc) goto err9;
      }
      else if (cmpwrd("levels",ch)) {
        rc = deflev (ch, rec, pfi, 2);
        if (rc==-1) goto err8; 
        if (rc) goto err9;
      } else goto err2;
      flgs[2] = 0;

    } else if (cmpwrd("tdef",rec)) {
      if ( (ch = nxtwrd(rec)) == NULL) goto err1;
      if ( (pos = intprs(ch,&(pfi->dnum[3])))==NULL) goto err1;
      if (pfi->dnum[3]<1) {
	snprintf(pout,255,"Warning: Invalid TDEF syntax in %s -- Changing size of T axis from %d to 1 \n",
		pfi->dnam,pfi->dnum[3]);
        gaprnt (1,pout);
	pfi->dnum[3] = 1;
      }
      if (*pos!=' ') goto err1;
      if ( (ch = nxtwrd(ch))==NULL) goto err2;
      if (cmpwrd("linear",ch)) {
        if ( (ch = nxtwrd(ch))==NULL) goto err3a_tdef;
        tdef.yr = -1000;
        tdef.mo = -1000;
        tdef.dy = -1000;
        if ( (pos = adtprs(ch,&tdef,&dt1))==NULL) goto err3b_tdef;
        if (*pos!=' ' || dt1.yr == -1000 || dt1.mo == -1000.0 ||
            dt1.dy == -1000) goto err3c_tdef;
        if ( (ch = nxtwrd(ch))==NULL) goto err4a_tdef;
        if ( (pos = rdtprs(ch,&dt2))==NULL) goto err4b_tdef;
        v1 = (dt2.yr * 12) + dt2.mo;
        v2 = (dt2.dy * 1440) + (dt2.hr * 60) + dt2.mn;
	/* check if 0 dt */
	if ( (v1 == 0) && (v2 == 0) ) goto err4c_tdef;  
	sz = sizeof(gadouble)*8;
        if ((vals = (gadouble *)galloc(sz,"tvals5")) == NULL) goto err8; 
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

    } else if (cmpwrd("edef",rec)) {
      if ((ch = nxtwrd(rec)) == NULL) goto err1;
      if ((pos = intprs(ch,&(pfi->dnum[4])))==NULL) goto err1;
      if (pfi->dnum[4]<1) {
	snprintf(pout,255,"Warning: Invalid EDEF syntax in %s -- Changing size of E axis from %d to 1 \n",
		pfi->dnam,pfi->dnum[4]);
        gaprnt (1,pout);
	pfi->dnum[4] = 1;
      }
      /* ensemble dimension is always linear -- set up linear scaling */
      sz = sizeof(gadouble)*6;
      if ((vals = (gadouble *)galloc(sz,"evals2")) == NULL) goto err8;
      ev1=ev2=1;
      *(vals+1) = ev1 - ev2;
      *(vals) = ev2;
      *(vals+2) = -999.9;
      pfi->grvals[4] = vals;
      *(vals+4) = -1.0 * ((ev1-ev2)/ev2);
      *(vals+3) = 1.0/ev2;
      *(vals+5) = -999.9;
      pfi->abvals[4] = vals+3;
      pfi->ab2gr[4] = liconv;
      pfi->gr2ab[4] = liconv;
      pfi->linear[4] = 1;
      size = pfi->dnum[4] * sizeof(struct gaens); 
      /* set up chain of gaens structures */
      sz = size;
      ens = (struct gaens *)galloc(sz,"ens4");
      if (ens==NULL) {
	gaprnt(0,"Open Error: memory allocation failed for ens\n");
	goto err8;
      }
      pfi->ens1 = ens;
      j = 0;
      ch = nxtwrd(ch);
      /* this is the pathway for keyword "names" followed by list of ensemble members */
      if ((ch!=NULL) && cmpwrd("names",ch)) {
	while (j<pfi->dnum[4]) {
	  if ((ch=nxtwrd(ch))==NULL) {
	    /* ensemble names are listed in more than one line */
	    if (fgets(rec,256,descr)==NULL) goto err7a;  
	    ch = rec;
	    while (*ch==' ' || *ch=='\t') ch++;        /* advance through white space */
	    if (*ch=='\0' || *ch=='\n') goto err7b;    /* nothing there */
	  }
	  /* get the ensemble name */
	  if ((getenm(ens, ch))!=0) goto err7d;
	  /* initialize remaining fields in ensemble structure */
	  for (jj=0;jj<4;jj++) ens->grbcode[jj]=-999;  
	  ens->length=0;
	  ens->gt=1;
	  ens->tinit.yr=0;
	  ens->tinit.mo=0;
	  ens->tinit.dy=0;
	  ens->tinit.hr=0;
	  ens->tinit.mn=0;
	  j++; ens++;
	}
      } 
      else {
	/* this is the pathway for separate lines 
	   containing name, length, initial time, and optional grib2 codes */
	while (j<pfi->dnum[4]) {
	  /* read the record and remove leading blanks */
	  fgets(rec,512,descr); 
	  reclen = strlen(rec);
	  jj = 0;
	  while (jj<reclen && rec[0]==' ') {
	    for (ii=0; ii<reclen; ii++) rec[ii] = rec[ii+1];
	    jj++;
	  }
	  /* replace newline with null at end of record */
	  for (ichar = strlen(rec) - 1 ;  ichar >= 0 ;  --ichar) {
	    if (rec[ichar] == '\n') {
	      rec[ichar] = '\0' ;
	      break ; 
	    }
	  }
	  /* Keep mixed case and lower case versions of rec handy */
	  strcpy (mrec,rec);
	  lowcas(rec);
	  /* Allow comments between EDEF and ENDEDEF */
	  if (!isalnum(*(mrec))) {
	    /* Parse comment if it contains attribute metadata  */
	    if ((strncmp("*:attr",mrec,6)==0) || (strncmp("@",mrec,1)==0)) {
	      if ((ddfattr(mrec,pfi)) == -1) goto retrn;
	      else continue;
	    }
	    else continue;
	  }
	  if (cmpwrd("endedef",rec)) {
	    gaprnt (0,"Open Error:  Unexpected ENDEDEF record\n");
	    snprintf(pout,255, "Was expecting %i records.  Found %i.\n", pfi->dnum[4], j);
	    gaprnt (2,pout);
	    goto err9;
	  }
	  /* get the ensemble name */
	  if ((getenm(ens, mrec))!=0) goto err7d;
	  /* get the length of the time axis */
	  if ( (ch=nxtwrd(rec))==NULL) goto err7e;
	  if ( (pos=intprs(ch,&(ens->length)))==NULL ) goto err7e;
	  /* get the initial time */
	  if ((ch = nxtwrd(ch))==NULL) goto err7e;
	  tdef.yr = -1000;
	  tdef.mo = -1000;
	  tdef.dy = -1000;
	  if ((pos = adtprs(ch,&tdef,&ens->tinit))==NULL) goto err7e;
	  if (ens->tinit.yr == -1000 || 
	      ens->tinit.mo == -1000 ||
	      ens->tinit.dy == -1000) goto err7e;
	  /* get the (optional) grib2 ensemble codes */
	  for (jj=0;jj<4;jj++) ens->grbcode[jj]=-999; 
	  if ((ch = nxtwrd(ch))!=NULL) {
	    jj=0;
	    while (1) {
	      if ((ch=intprs(ch,&(ens->grbcode[jj])))==NULL) goto err7c;
	      while (*ch==' ') ch++;
	      if (*ch!=',') break;
	      ch++;
	      while (*ch==' ') ch++;
	      if (*ch=='\0' || *ch=='\n') goto err7c;
	      jj++;
	      if (jj>3) goto err7c;
	    }
	  }
	  j++; ens++;
	}

	/* Get ENDEDEF statement and any additional comments */
	if (fgets(rec,512,descr)==NULL) {
	  gaprnt (0,"Open Error:  Missing ENDEDEF statement.\n");
	  goto retrn;
	}
	/* Remove any leading blanks from rec */
	reclen = strlen(rec);
	jj = 0;
	while (jj<reclen && rec[0]==' ') {
	  for (ii=0; ii<reclen; ii++) rec[ii] = rec[ii+1];
	  jj++;
	}
	/* replace newline with null at end of record */
	for (ichar = strlen(rec) - 1 ;  ichar >= 0 ;  --ichar) {
	  if (rec[ichar] == '\n') {
	    rec[ichar] = '\0' ;
	    break ; 
	  }
	}
	/* Keep mixed case and lower case versions handy */
	strcpy (mrec,rec);
	lowcas(rec);
	while (!cmpwrd("endedef",rec)) {
	  
	  /* see if it's an attribute comment */
	  if (!isalnum(*(mrec))) {
	    if ((strncmp("*:attr",mrec,6)==0) || (strncmp("@",mrec,1)==0)) {
	      if ((ddfattr(mrec,pfi)) == -1) goto retrn;
	    }
	  } else {
	    snprintf(pout,255,"Open Error:  Looking for \"ENDEDEF\", found \"%s\" instead.\n",mrec);
	    gaprnt (0,pout);
	    goto err9;
	  }
	  /* get a new record */
	  if (fgets(rec,512,descr)==NULL) {
	    gaprnt (0,"Open Error:  Missing ENDEDEF statement.\n");
	    goto retrn;
	  }
	  /* Remove any leading blanks from rec */
	  reclen = strlen(rec);
	  jj = 0;
	  while (jj<reclen && rec[0]==' ') {
	    for (ii=0; ii<reclen; ii++) rec[ii] = rec[ii+1];
	    jj++;
	  }
	  /* replace newline with null at end of record */
	  for (ichar = strlen(rec) - 1 ;  ichar >= 0 ;  --ichar) {
	    if (rec[ichar] == '\n') {
	      rec[ichar] = '\0' ;
	      break ; 
	    }
	  }
	  /* Keep mixed case and lower case versions handy */
	  strcpy (mrec,rec);
	  lowcas(rec);
	}
      }
	
    } else if (cmpwrd("vars",rec)) {
      if ( (ch = nxtwrd(rec)) == NULL) goto err5;
      if ( (pos = intprs(ch,&(pfi->vnum)))==NULL) goto err5;
      size = pfi->vnum * (sizeof(struct gavar) + 7 );
      sz = size;
      if ((pvar = (struct gavar *)galloc(sz,"pvar2")) == NULL) goto err8;
      pfi->pvar1 = pvar;
      i = 0;
      while (i<pfi->vnum) {
	/* initialize variables in the pvar structure */
	pvar->offset = 0; 
	pvar->recoff = 0;
	pvar->ncvid = -999;
	pvar->sdvid = -999;
	pvar->h5vid = -999;
	pvar->levels = 0;
	pvar->dfrm = 0;
	pvar->var_t = 0;
	pvar->scale = 1;
	pvar->add = 0;  
	pvar->undef= -9.99e8; 
        pvar->vecpair = -999;
        pvar->isu = 0;
	pvar->isdvar = 0;
	pvar->nvardims = 0; 
#if USEHDF5==1
	pvar->h5varflg=-999;
	pvar->dataspace=-999;
#endif

	/* get the complete variable declaration */
        if (fgets(rec,512,descr)==NULL) {
          gaprnt (0,"Open Error:  Unexpected EOF reading variables\n");
          snprintf(pout,255, "Was expecting %i records.  Found %i.\n", pfi->vnum, i);
          gaprnt (2,pout);
          goto retrn;
        }
	/* remove any leading blanks from rec */
	reclen = strlen(rec);
	jj = 0;
	while (jj<reclen && rec[0]==' ') {
	  for (ii=0; ii<reclen; ii++) rec[ii] = rec[ii+1];
	  jj++;
	}
	/* replace newline with null at end of record */
        for (ichar = strlen(rec) - 1 ;  ichar >= 0 ;  --ichar) {
	  if (rec[ichar] == '\n') {
	    rec[ichar] = '\0' ;
	    break ; 
	  }
        }
	/* Keep mixed case and lower case versions of rec handy */
        strcpy (mrec,rec);
        lowcas(rec);
	/* Allow comments between VARS and ENDVARS */
	/* For hdf5 datasets, the varnames may begin with a "/", so along with the 
	   test for non-alpha-numeric characters, which normally indicate a comment, 
	   check if first char is not slash, if so, then it's a comment, otherwise  
	   it's a legitimate hdf5 variable declaration. */
        if (!isalnum(*(mrec)) && (strncmp("/",mrec,1)!=0)) {
	  /* Parse comment if it contains attribute metadata  */
	  if ((strncmp("*:attr",mrec,6)==0) || (strncmp("@",mrec,1)==0)) {
	    if ((ddfattr(mrec,pfi)) == -1) goto retrn;
	    else continue;
	  }
  	  else continue;
	}

        if (cmpwrd("endvars",rec)) {
          gaprnt (0,"Open Error:  Unexpected ENDVARS record\n");
          snprintf(pout,255, "Was expecting %i records.  Found %i.\n", pfi->vnum, i);
          gaprnt (2,pout);
          goto err9;
        }
	
	/* get abbrv and full variable name if there */
        if ((getvnm(pvar, mrec))!=0) goto err6;

	/* parse the levels fields */
        if ( (ch=nxtwrd(rec))==NULL) goto err6;
	/* begin with 8th element of units aray for levels values */
	for (j=0;j<16;j++) pvar->units[j] = -999;
        j = 8;          
        while (1) {
	  if (j==8) {
	    /* first element is num levels */
	    if ((ch=intprs(ch,&(pvar->levels)))==NULL) goto err6;      
	  }
	  else {
	    /* remaining elements are grib2 level codes */
	    if ((ch=getdbl(ch,&(pvar->units[j-1])))==NULL) goto err6;  
	  }
	  /* advance through comma-delimited list of levels args */
          while (*ch==' ') ch++;
          if (*ch=='\0' || *ch=='\n') goto err6;
          if (*ch!=',') break;
          ch++;
	  while (*ch==',') { ch++; j++;}  /* advance past back to back commas */
          while (*ch==' ') ch++;
          if (*ch=='\0' || *ch=='\n') goto err6;
          j++;
          if (j>15) goto err6;
        }

	/* parse the units fields; begin with 0th element for variable units */
        j = 0;
	pvar->nvardims=0;
        while (1) {
          if (*ch=='x'||*ch=='y'||*ch=='z'||*ch=='t'||*ch=='e') { 
            if (*(ch+1)!=',' && *(ch+1)!=' ') goto err6;
            if (*ch=='x') { pvar->units[j] = -100; pvar->nvardims++; }
            if (*ch=='y') { pvar->units[j] = -101; pvar->nvardims++; }
            if (*ch=='z') { pvar->units[j] = -102; pvar->nvardims++; }
            if (*ch=='t') { pvar->units[j] = -103; pvar->nvardims++; }
            if (*ch=='e') { pvar->units[j] = -104; pvar->nvardims++; }
            ch++;
          } else {
            if ( (ch=getdbl(ch,&(pvar->units[j])))==NULL ) goto err6;
	    /* no negative array indices for ncflag files */
	    if ((pfi->ncflg) && (pvar->units[j] < 0))  goto err6;   
          }
          while (*ch==' ') ch++;
          if (*ch=='\0' || *ch=='\n') goto err6;
          if (*ch!=',') break;
          ch++;
          while (*ch==' ') ch++;
          if (*ch=='\0' || *ch=='\n') goto err6;
          j++;
          if (j>8) goto err6;
        }

	/* parse the variable description */
        getstr (pvar->varnm,mrec+(ch-rec),127);


	/* var_t is for data files with dimension sequence: X, Y, Z, T, V */
	if ((pvar->units[0]==-1) && 
	    (pvar->units[1]==20)) 
	  pvar->var_t = 1;

	/* non-float data types */
	if ((pvar->units[0]==-1) && 
	    (pvar->units[1]==40)) {

	  if (pvar->units[2]== 1) pvar->dfrm = 1;
	  if (pvar->units[2]== 2) { pvar->dfrm = 2;
	  if (pvar->units[3]==-1) pvar->dfrm = -2; }
	  if (pvar->units[2]== 4) pvar->dfrm = 4;
	}

        i++; pvar++;
      }

      /* Get ENDVARS statement and any additional comments */
      if (fgets(rec,512,descr)==NULL) {
        gaprnt (0,"Open Error:  Missing ENDVARS statement.\n");
        goto retrn;
      }
      /* Remove any leading blanks from rec */
      reclen = strlen(rec);
      jj = 0;
      while (jj<reclen && rec[0]==' ') {
	for (ii=0; ii<reclen; ii++) rec[ii] = rec[ii+1];
	jj++;
      }
      /* replace newline with null at end of record */
      for (ichar = strlen(rec) - 1 ;  ichar >= 0 ;  --ichar) {
	if (rec[ichar] == '\n') {
	  rec[ichar] = '\0' ;
	  break ; 
	}
      }
      /* Keep mixed case and lower case versions handy */
      strcpy (mrec,rec);
      lowcas(rec);
      while (!cmpwrd("endvars",rec)) {
	/* see if it's an attribute comment */
        if (!isalnum(*(mrec))) {
	  if ((strncmp("*:attr",mrec,6)==0) || (strncmp("@",mrec,1)==0)) {
	    if ((ddfattr(mrec,pfi)) == -1) goto retrn;
	  }
	}
        else {
	  snprintf(pout,255,"Open Error:  Looking for \"endvars\", found \"%s\" instead.\n",rec);
	  gaprnt (0,pout);
	  goto err9;
	}
	/* get a new record */
  	if (fgets(rec,512,descr)==NULL) {
	  gaprnt (0,"Open Error:  Missing ENDVARS statement.\n");
	  goto retrn;
	}
	/* Remove any leading blanks from new record */
	reclen = strlen(rec);
	jj = 0;
	while (jj<reclen && rec[0]==' ') {
	  for (ii=0; ii<reclen; ii++) rec[ii] = rec[ii+1];
	  jj++;
	}
	/* replace newline with null at end of record */
	for (ichar = strlen(rec) - 1 ;  ichar >= 0 ;  --ichar) {
	  if (rec[ichar] == '\n') {
	    rec[ichar] = '\0' ;
	    break ; 
	  }
	}
	/* Keep mixed case and lower case versions handy */
	strcpy (mrec,rec);
	lowcas(rec);
      }
      /* vars block parsed without error */
      flgs[6] = 0;

    } else {
      /* parse error of .ctl file */
      gaprnt (0,"Open Error:  Unknown keyword in description file\n");
      goto err9;
    }
  }

  err=0;
  for (i=0; i<7; i++) {
    if (flgs[i]) {
      snprintf(pout,255,"Open Error:  missing %s record \n",errs[i]);
      gaprnt (0,pout);
      err=1;
    }
  }

  if (err) goto retrn;


  /* Done scanning!
     Check if scanned stuff makes sense, and then set things up correctly */

  /* Set the default netcdf/hdf5 cache size to be big enough to contain 
     a global 2D grid of 8-byte data values times the global cache scale factor */
  //if (pfi->cachesize == (long)-1) {
  //  sf = sf();
  //  sf = sf * 8 * pfi->dnum[0] * pfi->dnum[1];
  //  pfi->cachesize = (long)floor(sf) ;
  //}
  /* set the netCDF-4 cache size */
  //sz = (size_t)pfi->cachesize;
  //set_nc_cache(sz);
  

  /* If no EDEF entry was found, set up the default values */
  if (pfi->ens1==NULL) {
      pfi->dnum[4]=1;
      /* set up linear scaling */
      sz = sizeof(gadouble)*6;
      if ((vals = (gadouble *)galloc(sz,"evals3")) == NULL) goto err8;
      v1=v2=1;
      *(vals+1) = v1 - v2;
      *(vals) = v2;
      *(vals+2) = -999.9;
      pfi->grvals[4] = vals;
      *(vals+4) = -1.0 * ( (v1-v2)/v2 );
      *(vals+3) = 1.0/v2;
      *(vals+5) = -999.9;
      pfi->abvals[4] = vals+3;
      pfi->ab2gr[4] = liconv;
      pfi->gr2ab[4] = liconv;
      pfi->linear[4] = 1;
      /* Allocate memory and initialize one ensemble structure */
      sz = sizeof(struct gaens);
      ens = (struct gaens *)galloc(sz,"ens5");
      if (ens==NULL) {
	gaprnt(0,"Open Error: memory allocation failed for default ens\n");
	goto err8;
      }
      pfi->ens1 = ens;
      snprintf(ens->name,15,"1");
      ens->length = pfi->dnum[3];
      ens->gt = 1;
      gr2t(pfi->grvals[3],1,&ens->tinit);
      for (j=0;j<4;j++) ens->grbcode[j]=-999;
  }
  else {
    ens=pfi->ens1;
    j=0;
    while (j<pfi->dnum[4]) {
      /* Copy length and time metadata to ensemble members that only have names */
      if (ens->length == 0) ens->length=pfi->dnum[3];
      if (ens->tinit.mo == 0) gr2t(pfi->grvals[3],1,&ens->tinit);
      /* Calculate the grid coordinate for the initial time */
      ens->gt = t2gr(pfi->abvals[3],&(ens->tinit));
      /* make sure time axis spans all ensemble members */
      if (ens->gt - 1 + ens->length > pfi->dnum[3]) {
	snprintf(pout,255,"Open Error:  ensemble %d (%s) extends beyond the time axis limits\n",j,ens->name);
	gaprnt(0,pout);
	goto retrn;
      }
      j++; ens++;
    }
  }

  /* Rearrange the pvar->units fields for GRIB1 level info so they match GRIB2 */
  if (pfi->idxflg == 1) {
    pvar=pfi->pvar1;
    for (j=1; j<=pfi->vnum; j++) {
      pvar->units[8]  = pvar->units[1];  
      pvar->units[9]  = pvar->units[2];  
      pvar->units[10] = pvar->units[3];  
      pvar->units[1]  = pvar->units[2] = pvar->units[3] = -999;
      pvar++;
    }
  }

  /* Handle the index file (Station or GRIB) */
  if (pfi->idxflg && mflag) {
    mfile = fopen (pfi->mnam, "rb");
    if (mfile==NULL) {
      snprintf(pout,255,"Open Error:  Can't open Station/Index map file %s \n",pfi->mnam);
      gaprnt (0,pout);
      goto retrn;
    }
    if (mflag==2) goto skipread;
    
    mflflg = 1;
    swpflg = 0; 
      
    /* GRIB (version 1 or 2) gridded data */
    if (pfi->type!=2) {
      /* GRIB version 1 */
      if (pfi->idxflg==1) {    
	/* allocate memory for index data */
	sz = sizeof(struct gaindx);
	if ((pindx = (struct gaindx *)galloc(sz,"pindx"))==NULL) 
	  goto err8;
	pfi->pindx = pindx;
	/*  check the gribmap version number */
	fseek(mfile,1,0);
	rc = fread(&vermap,sizeof(unsigned char),1,mfile);
	if (rc!=1) { 
	  gaprnt(0,"Error reading version number from GRIB1 index file\n"); 
	  goto retrn; 
	}

	/* gribmap version 2 or 3 */
	if ((vermap == 2) || (vermap == 3)) {        

	  /* read the header */
	  fseek(mfile,2,0);
	  rc = fread(urec,sizeof(unsigned char),4,mfile);
	  if (rc!=4) { 
	    gaprnt(0,"Error reading hinum from GRIB1 index file\n"); 
	    goto retrn; 
	  }
	  pindx->hinum=gagby(urec,0,4);
	  
	  rc = fread(urec,sizeof(unsigned char),4,mfile);
	  if (rc!=4) { 
	    gaprnt(0,"Error reading hfnum from GRIB1 index file\n"); 
	    goto retrn; 
	  }
	  pindx->hfnum=gagby(urec,0,4);
	  
	  rc = fread(urec,sizeof(unsigned char),4,mfile);
	  if (rc!=4) { 
	    gaprnt(0,"Error reading intnum from GRIB1 index file\n"); 
	    goto retrn; 
	  }
	  pindx->intnum=gagby(urec,0,4);
	  
	  rc = fread(urec,sizeof(unsigned char),4,mfile);
	  if (rc!=4) { 
	    gaprnt(0,"Error reading fltnum from GRIB1 index file\n"); 
	    goto retrn; 
	  }
	  pindx->fltnum=gagby(urec,0,4);
	  
	  if (vermap == 2) {
	    /* skip the begining time struct info */
	    /* this not written out in version 3 maps */
	    rc = fread(urec,sizeof(unsigned char),7,mfile);
	    if (rc!=7) { 
	      gaprnt(0,"Error reading time data from GRIB1 index file\n"); 
	      goto retrn; 
	    }
	  }

	  /* read the index data */
	  pindx->hipnt  = NULL;
	  pindx->hfpnt  = NULL;
	  pindx->intpnt = NULL;
	  pindx->fltpnt = NULL;
	  if (pindx->hinum>0) {
	    sz = sizeof(gaint)*pindx->hinum;
	    if ((pindx->hipnt = (gaint*)galloc(sz,"hipnt"))==NULL) 
	      goto err8;
	    for (i=0; i<pindx->hinum; i++) {
	      rc = fread(urec,sizeof(unsigned char),4,mfile);
	      if (rc!=4) { 
		snprintf(pout,255,"Error reading integer %d from header of GRIB1 index file\n",i); 
		gaprnt(0,pout); 
		goto retrn; 
	      }
	      idum = gagby(urec,0,4);
	      if (gagbb(urec,0,1)) idum = -idum;
	      *(pindx->hipnt+i) = idum;
	    }
	  }

	  if (pindx->hfnum>0) {
	    sz = sizeof(gafloat)*pindx->hfnum;
	    if ((pindx->hfpnt = (gafloat*)galloc(sz,"hfpnt"))==NULL) 
	      goto err8; 
	    rc = fread (pindx->hfpnt,sizeof(gafloat),pindx->hfnum,mfile);
	    if (rc!=pindx->hfnum) { 
	      gaprnt(0,"Error reading floats from header of GRIB1 index file\n"); 
	      goto retrn; 
	    }
	  }

	  if (pindx->intnum>0) {
	    sz = sizeof(gaint)*pindx->intnum;
	    if ((pindx->intpnt = (gaint*)galloc(sz,"intpnt"))==NULL) 
	      goto err8; 
	    for (i=0; i<pindx->intnum; i++) {
	      rc = fread(urec,sizeof(unsigned char),4,mfile);
	      if (rc!=4) { 
		snprintf(pout,255,"Error reading integer %d from GRIB1 index file\n",i); 
		gaprnt(0,pout); 
		goto retrn; 
	      }
	      idum = gagby(urec,0,4);
	      if (gagbb(urec,0,1)) idum = -gagbb(urec,1,31);
	      *(pindx->intpnt+i) = idum;
	    }
	  }

	  if (pindx->fltnum>0) {
	    sz = sizeof(gafloat)*pindx->fltnum;
	    if ((pindx->fltpnt = (gafloat *)galloc(sz,"fltpnt"))==NULL) 
	      goto err8; 
	    for (i=0; i<pindx->fltnum; i++) {
	      rc = fread(urec,sizeof(unsigned char),4,mfile);
	      if (rc!=4) { 
		snprintf(pout,255,"Error reading float %d from GRIB1 index file\n",i); 
		gaprnt(0,pout); 
		goto retrn; 
	      }
	      fdum = ibm2flt(urec);
	      *(pindx->fltpnt+i) = fdum;
	    }
	  }
	} 
	else {     
	  /* other gribmap versions */
	  fseek (mfile,0L,0);
	  rc = fread (pindx,sizeof(struct gaindx),1,mfile);
          if (rc!=1) {
	    gaprnt(0,"Error reading header from GRIB1 index file\n");
	    goto retrn;
          }
	  if (pindx->type>>24 > 0) swpflg=1;
	  if (swpflg) gabswp((gafloat *)pindx,5);
	  pindx->hipnt = NULL;
	  pindx->hfpnt = NULL;
	  pindx->intpnt = NULL;
	  pindx->fltpnt = NULL;
          if (pindx->type != 1 && pindx->type != 4 ) {
            snprintf(pout,100,"Invalid version number %i in GRIB index file\n",pindx->type); 
            gaprnt(0,pout); 
            goto retrn; 
          }
	  if (pindx->hinum>0) {
	    sz = sizeof(gaint)*pindx->hinum;
	    if ((pindx->hipnt = (gaint *)galloc(sz,"hipnt2")) == NULL) 
	      goto err8; 
	    rc = fread (pindx->hipnt,sizeof(gaint),pindx->hinum,mfile);
            if (rc!=pindx->hinum) {
	      gaprnt(0,"Error reading header ints from GRIB1 index file\n");
	      goto retrn;
            }
	    if (swpflg) gabswp((gafloat *)(pindx->hipnt),pindx->hinum);
	  }
	  if (pindx->hfnum>0) {
	    sz = sizeof(gafloat)*pindx->hfnum;
	    if ((pindx->hfpnt = (gafloat *)galloc(sz,"hfpnt2")) == NULL)
	      goto err8; 
	    rc = fread (pindx->hfpnt,sizeof(gafloat),pindx->hfnum,mfile);
            if (rc!=pindx->hfnum) {
	      gaprnt(0,"Error reading header floats from GRIB1 index file\n");
	      goto retrn;
            }
	    if (swpflg) gabswp(pindx->hfpnt,pindx->hfnum);
	  }
	  if (pindx->intnum>0) {
	    sz = sizeof(gaint)*pindx->intnum;
	    if ((pindx->intpnt = (gaint *)galloc(sz,"ipnt2")) == NULL) 
	      goto err8; 
	    rc = fread (pindx->intpnt,sizeof(gaint),pindx->intnum,mfile);
            if (rc!=pindx->intnum) {
	      gaprnt(0,"Error reading int array from GRIB1 index file\n");
	      goto retrn;
            }
	    if (swpflg) gabswp((gafloat *)(pindx->intpnt),pindx->intnum);
	  }
	  if (pindx->fltnum>0) {
	    sz = sizeof(gafloat)*pindx->fltnum;
	    if ((pindx->fltpnt = (gafloat *)galloc(sz,"fpnt2")) == NULL)
	      goto err8; 
	    rc = fread (pindx->fltpnt,sizeof(gafloat),pindx->fltnum,mfile);
            if (rc!=pindx->fltnum) {
	      gaprnt(0,"Error reading float array from GRIB1 index file\n");
	      goto retrn;
            }
	    if (swpflg) gabswp(pindx->fltpnt,pindx->fltnum);
	  }
          if (pindx->type == 4) {
            if (sizeof(off_t)!=8) goto err8a;
	    sz = sizeof(struct gaindxb);
	    if ((pindxb = (struct gaindxb *)galloc(sz,"pindxb"))==NULL) goto err8;
	    pfi->pindxb = pindxb;
            pindxb->bignum = *(pindx->hipnt + 4); 
            pindxb->bigpnt = NULL;
	    if (pindxb->bignum>0) {
	      sz = sizeof(off_t)*pindxb->bignum;
	      if ((pindxb->bigpnt = (off_t *)galloc(sz,"offpnt")) == NULL) goto err8; 
	      rc = fread (pindxb->bigpnt,sizeof(off_t),pindxb->bignum,mfile);
              if (rc!=pindxb->bignum) {
	        gaprnt(0,"Error reading off_t array from GRIB1 index file\n");
	        goto retrn;
              }
	      if (swpflg) gabswp8(pindxb->bigpnt,pindxb->bignum);
	    }
          }
	}
      }
#if GRIB2
      /* GRIB Version 2 */
      else if (pfi->idxflg==2) {   	 
	/* allocate memory for the grib2 index data */
	sz = sizeof(struct gag2indx);
	if ((g2indx = (struct gag2indx *)galloc(sz,"g2indx")) == NULL) 
	  goto err8;
	pfi->g2indx = g2indx;
        g2indx->g2intpnt = NULL;
        g2indx->g2bigpnt = NULL;
	/* get the grib2map version number */
	fseek(mfile,0L,SEEK_SET);
	rc = fread(&g2indx->version,sizeof(gaint),1,mfile);
	if (rc!=1) {
	  gaprnt(0,"Error reading version number from GRIB2 index file\n");
	  goto retrn;
	}
	/* check if we need to byte swap */
	if (g2indx->version>>24 > 0) swpflg=1;
	else swpflg=0;
	if (swpflg) gabswp(&g2indx->version,1);
	/* get the index values */
	if (g2indx->version == 1 || g2indx->version == 2) {     
          if (g2indx->version == 2 && sizeof(off_t)!=8) goto err8a;
	  rc = fread(&g2indx->g2intnum,sizeof(gaint),1,mfile);
	  if (rc!=1) {
	    gaprnt(0,"Error reading index values from GRIB2 index file\n");
	    goto retrn;
	  } 
	  if (swpflg) gabswp(&g2indx->g2intnum,1);
	  if (g2indx->g2intnum>0) {
	    sz = sizeof(gaint)*g2indx->g2intnum;
	    if ((g2indx->g2intpnt = (gaint *)galloc(sz,"g2intpnt")) == NULL) 
	      goto err8;
	    rc = fread(g2indx->g2intpnt,sizeof(gaint),g2indx->g2intnum,mfile);
	    if (rc!=g2indx->g2intnum) {
	      snprintf(pout,255,"Error reading GRIB2 index file, rc=%d\n",rc);
	      gaprnt(0,pout);
	      goto retrn;
	    }
	    if (swpflg) gabswp(g2indx->g2intpnt,g2indx->g2intnum);
            if (g2indx->version == 2) {
	      sz = sizeof(off_t)*g2indx->g2intnum;
	      if ((g2indx->g2bigpnt = (off_t *)galloc(sz,"g2bigpnt")) == NULL) 
	        goto err8;
	      rc = fread(g2indx->g2bigpnt,sizeof(off_t),g2indx->g2intnum,mfile);
	      if (rc!=g2indx->g2intnum) {
	        snprintf(pout,255,"Error reading GRIB2 index file, rc=%d\n",rc);
	        gaprnt(0,pout);
	        goto retrn;
	      }
	      if (swpflg) gabswp8(g2indx->g2bigpnt,g2indx->g2intnum);
            }
	  }
	}
	else {     
	  snprintf(pout,255,"Unknown GRIB2 index version number: %d\n",g2indx->version);
	  gaprnt(0,pout);
	  goto retrn;
	}
      }
#endif
    }  /* end of GRIB index handling */
    
    else {
      /* stnmap file processing */
      fread(rec,1,16,mfile);  /* minimum map file is 16 bytes */
      vermap=1;
      if (strncmp(rec,"GrADS_stnmapV002",16) == 0) {
	vermap=2;
      }
      if (vermap == 2) {
	fread(urec,sizeof(unsigned char),4,mfile);
	idum=gagby(urec,0,4);
	if (gagbb(urec,0,1)) idum=-idum;
	mcnt=idum;
	fread(urec,sizeof(unsigned char),4,mfile);
	idum=gagby(urec,0,4);
	if (gagbb(urec,0,1)) idum=-idum;
	maxlv=idum;
	sz = sizeof(gaint)*mcnt;
	if ((pfi->tstrt = (gaint *)galloc(sz,"tstrt")) == NULL) goto err8;
	if ((pfi->tcnt  = (gaint *)galloc(sz,"tcnt")) == NULL) goto err8;
	for(i=0;i<mcnt;i++) {
	  fread(urec,sizeof(unsigned char),4,mfile);
	  idum=gagby(urec,0,4);
	  if (gagbb(urec,0,1)) idum=-idum;
	  *(pfi->tstrt+i)=idum;
	}
	for(i=0;i<mcnt;i++) {
	  fread(urec,sizeof(unsigned char),4,mfile);
	  idum=gagby(urec,0,4);
	  if (gagbb(urec,0,1)) idum=-idum;
	  *(pfi->tcnt+i)=idum;
	}
      } 
      else if (vermap ==1) {
	/* reposition and read two local ints for version 1 map*/
	fseek (mfile,0L,0);
	fread (&mcnt,sizeof(gaint),1,mfile);
	fread (&maxlv,sizeof(gaint),1,mfile);
	if (maxlv>>24 > 0) swpflg = 1;
	if (swpflg) {
	  gabswp((gafloat *)(&mcnt),1);
	  gabswp((gafloat *)(&maxlv),1);
	}
	sz = sizeof(gaint)*mcnt;
	if ((pfi->tstrt = (gaint *)galloc(sz,"tstrt1")) == NULL) goto err8;
	if ((pfi->tcnt  = (gaint *)galloc(sz,"tcnt1")) == NULL) goto err8;
	fread (pfi->tstrt,sizeof(gaint),mcnt,mfile);
	fread (pfi->tcnt,sizeof(gaint),mcnt,mfile);
	if (swpflg) {
	  gabswp((gafloat *)pfi->tstrt,1);
	  gabswp((gafloat *)pfi->tcnt,1);
	}
	pfi->mtype = 1;
      }
    }

  skipread:
    fclose (mfile);
    mflflg = 0;
    
  } /* End of index file handling for grib and station data files */

  
#if GRIB2
  /* Check if we need to convert pressure values from Pascals to millibars (GRIB2 only) */
  /* Leave pressure values in Pascals for pre-processing of descriptor file (i.e. mflag==0 or 2) */
  if (pfi->pa2mb && mflag==1) {
    if (pfi->idxflg==2) {
      for (i=1; i<=pfi->dnum[2]; i++) {
	*(pfi->grvals[2]+i) = *(pfi->grvals[2]+i)/100;
      }
    }
    else {
      gaprnt(2,"Do not use Pascals as pressure units unless data format is GRIB2\n");
      goto err9;
    }
  }
#endif

  /* Parse the vector pairs */
  npairs = 0;
  if (vectorpairs) {
    vplist = vectorpairs; 
    sz = strlen(vplist)+1;
    if ((pair = (char *)galloc(sz,"pair")) == NULL) {
      gaprnt(0,"memory allocation error for list of vector pairs\n");
      goto err8;
    } 
    else {
      while (1) {
	/* copy the comma-delimited pair of variable names */
	getwrd(pair,vplist,strlen(vplist));
	i=0;
	while (1) {
	  if (*(pair+i)==',') {

	    /* get the two variable names that comprise the vector pair */
	    getstr(var1, pair, i);
	    getstr(var2, pair+(i+1), strlen(pair)-i+1);
	    npairs++;

	    /* loop through variable list */
	    foundvar1=foundvar2=0;
	    pvar = pfi->pvar1;
	    for (j=1; j<=pfi->vnum; j++) {
	      /* if variable names match, set flags */
	      if (cmpwrd(pvar->abbrv,var1)) foundvar1=j;
	      if (cmpwrd(pvar->abbrv,var2)) foundvar2=j;
	      pvar++;
	    }
	    if (foundvar1 && foundvar2) {
	      pvar = pfi->pvar1;
	      for (j=1; j<=pfi->vnum; j++) {
		/* if we've found both variables, set pvar->vecpair */
		if (cmpwrd(pvar->abbrv,var1)) {
		  pvar->vecpair=npairs;
		  pvar->isu=1;    /* trip flag for u-component */
		}
		if (cmpwrd(pvar->abbrv,var2)) {
		  pvar->vecpair=npairs;
		}
		pvar++;
	      }
	    }
	    else {
	      snprintf(pout,255,"Warning: VECTORPAIRS variables %s,%s were not found \n",var1,var2);
	      gaprnt(1,pout);
	    }
	    break;
	  }
	  i++; 
	}
	/* move pointer forward one word */
	if ((vplist = nxtwrd (vplist)) == NULL) break;
      }
    }
    gree(pair,"f174");
    gree(vectorpairs,"f175");
  }

  /* Find u,v variables -- vector pairs that havn't already been flagged */
  pvar=pfi->pvar1;
  for (j=1; j<=pfi->vnum; j++) {
    /* for GRIB2 data sets */
    if (pfi->idxflg == 2) {
      /* Look for a variable with units[0-2] == 0,2,2 or 0,2,3  that hasn't been handled yet */
      if ((pvar->vecpair<0) && 
	  ((pvar->units[0]==0 && 
	    pvar->units[1]==2 && 
	    pvar->units[2]==2) /* variabls is u */ ||   
	   (pvar->units[0]==0 && 
	    pvar->units[1]==2 && 
	    pvar->units[2]==3))) /* variabls is v */ {
	if (pvar->units[2]==2) 
	  rc = 3;
	else 
	  rc = 2; 
	/* Look for a matching variable with all units fields equal */
	pvar2 = pfi->pvar1;
	i = 0;
	while (i<pfi->vnum) {
	  if ((pvar2->vecpair<0) &&        
	      (pvar2->units[0]==pvar->units[0]) &&
	      (pvar2->units[1]==pvar->units[1]) &&
	      (pvar2->units[2]==rc) &&
	      (pvar2->units[8]==pvar->units[8]) &&
	      (pvar2->units[9]==pvar->units[9]) &&
	      (pvar2->units[10]==pvar->units[10])) break;
	  pvar2++; i++;
	}	
	if (i<pfi->vnum) { /* We've got a match! */
	  npairs++;
	  /* set the gavar parameters */
	  pvar->vecpair=npairs;
	  pvar2->vecpair=npairs;
	  if (pvar->units[2]==2) 
	    pvar->isu=1; 
	  else 
	    pvar2->isu=1;
	}      
      }
    }
    /* for GRIB1 and binary data sets */
    else {
      /* Look for a variable with units[0]==33,34 that hasn't been handled yet */
      if ((pvar->units[0]==33 || pvar->units[0]==34) && 
	  (pvar->vecpair<0)) {
	if (pvar->units[0]==33) 
	  rc = 34; 
	else 
	  rc = 33;
	/* Look for a matching variable with all units fields equal */
	pvar2 = pfi->pvar1;
	i = 0;
	while (i<pfi->vnum) {
	  if ((pvar2->vecpair<0) &&        
	      (pvar2->units[0]==rc) &&
	      (pvar2->units[8]==pvar->units[8]) &&
	      (pvar2->units[9]==pvar->units[9]) &&
	      (pvar2->units[10]==pvar->units[10])) break;
	  pvar2++; i++;
	}
	if (i<pfi->vnum) { /* We've got a match! */
	  npairs++;
	  /* set the gavar parameters */
	  pvar->vecpair=npairs;
	  pvar2->vecpair=npairs;
	  if (pvar->units[0]==33) 
	    pvar->isu=1;
	  else 
	    pvar2->isu=1;
	}
      }
    }
    pvar++;
  }
  if (err) goto retrn;  /* end of vector pair management */

  /* Chect time count in station index file and descriptor file */
  if (pfi->type>1 && mflag==1) {
    if (mcnt==-1) {
      gaprnt (0,"Open Error: missing STNMAP record\n");
      err=1;
    } else if (mcnt != pfi->dnum[3]) {
      gaprnt (0,"Open Error: Inconsistent time count\n");
      snprintf(pout,255,"  Count in station map file = %i\n",mcnt);
      gaprnt (0,pout);
      snprintf(pout,255,"  Count in descriptor file = %i\n",pfi->dnum[3]);
      gaprnt (0,pout);
      err=1;
    }
  }

  if (err) goto retrn;

  /* Make sure there are no conflicting options and data types */
  pvar=pfi->pvar1;
  for (j=1; j<=pfi->vnum; j++) {
    if (pvar->units[0]==-1 && pvar->units[1]==20) {
      if (pfi->tmplat) {
	gaprnt(0,"Open Error: Variables with transposed VAR-T dimensions cannot be templated together\n");
	err=1;
      }
      if (hdrb>0) {
	gaprnt(0,"Open Error: Variables with transposed VAR-T dimensions are incompatible with time headers\n");
	err=1;
      }
      if (trlb>0) {
	gaprnt(0,"Open Error: Variables with transposed VAR-T dimensions are incompatible with TRAILERBYTES\n");
	err=1;
      }
    }
    pvar++;
  }
  if (err) goto retrn;


  /* Figure out locations of variables within a time group */
  pvar = pfi->pvar1;

  /* Grid data */
  if (pfi->type==1) {
    pfi->gsiz = pfi->dnum[0] * pfi->dnum[1];
    if (pfi->ppflag) pfi->gsiz = pfi->ppisiz * pfi->ppjsiz;
    /* add the XY header to gsiz */
    if (pfi->xyhdr) {
      if (pvar->dfrm == 1) {
	pfi->xyhdr = pfi->xyhdr*4/1;          
      } 
      else if (pvar->dfrm ==  2 || pvar->dfrm == -2 ) {
	pfi->xyhdr = pfi->xyhdr*4/2;
      } 
      pfi->gsiz = pfi->gsiz + pfi->xyhdr;
    }

    /* adjust the size of hdrb and trlb for non-float data */
    if (pvar->dfrm == 1) {
      hdrb = hdrb*4/1;
      trlb = trlb*4/1;
    } 
    else if (pvar->dfrm == 2 || pvar->dfrm == -2 ) {
      hdrb = hdrb*4/2;
      trlb = trlb*4/2;
    } 
    
    if (pfi->seqflg) {
      /* pad the grid size with 2 4-byte chunks */
      if (pvar->dfrm == 1) {
	pfi->gsiz += 8;
      } 
      else if (pvar->dfrm == 2 || pvar->dfrm == -2 ) {
	pfi->gsiz += 4;
      } 
      else {
	pfi->gsiz += 2;             
      }
      /* pad the header with 2 4-byte chunks*/
      if (hdrb>0) {
	if (pvar->dfrm == 1) {
	  hdrb = hdrb + 8;
	} 
	else if (pvar->dfrm == 2 || pvar->dfrm == -2 ) {
	  hdrb = hdrb + 4;
	} 
	else {
	  hdrb += 2; 
	}
      }
      /* how far we have to go into the file before getting to 1st var */
      if (pvar->dfrm == 1) {
	pvar->offset = 4+hdrb;
	acum = 4+hdrb;
      } 
      else if (pvar->dfrm == 2 || pvar->dfrm == -2 ) {
	pvar->offset = 2+hdrb;
	acum = 2+hdrb;
      } 
      else {
	pvar->offset = 1+hdrb;
	acum = 1+hdrb;
      } 
    }
    else {
      /* how far we have to go into the file before getting to 1st var */
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
      if (pvar->var_t) {   
	acum = acum + levs*(pfi->gsiz)*(pfi->dnum[3]); 
      } else {                              
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

    /* last variable */
    acum = acum + (levs*pfi->gsiz);

    pfi->tsiz = acum;
    pfi->trecs = recacm;
    if (pfi->seqflg) pfi->tsiz-=1;
    pfi->tsiz += trlb;
    
  } 
  else {   /* non grid data */

    for (i=0; i<pfi->vnum; i++) {
      if (pvar->levels!=0) break;
      pvar->offset = i;
      pvar++;
    }
    for (j=i; j<pfi->vnum; j++) {
      if (pvar->levels==0) {
	if (!(pfi->bufrflg)) {   /* order of variables doesn't matter for BUFR data */
	  gaprnt (0,"Open Error: Variables out of order\n");
	  gaprnt (0,"  Non-vertical variables must go first\n");
	  goto retrn;
	}
      }
      pvar->offset = j-i;
      pvar++;
    }
    pfi->lvnum = pfi->vnum - i;
    pfi->ivnum = i;
  }

/* set the global calendar and check if we are trying to change with a new file...
   we do this here to set the calandar for templating */

  if (mfcmn.cal365<0) {
    mfcmn.cal365=pfi->calendar;
  } else {
    if (pfi->calendar != mfcmn.cal365) {
      gaprnt(0,"Attempt to change the global calendar...\n");
      if (mfcmn.cal365) {
	gaprnt(0,"The calendar is NOW 365 DAYS and you attempted to open a standard calendar file\n");
      } else {
	gaprnt(0,"The calendar is NOW STANDARD and you attempted to open a 365-day calendar file\n");
      }
      goto retrn;
    }
  }

  /* Allocate an I/O buffer.
     If we're just parsing the descriptor (mflag==0), no need to do this */
  if (mflag) {
    if (pfi->type > 1) {
      /* for station data, the buffer is the size of max levels  */
      if (pfi->bufrflg) maxlv=1;    /* maxlv not used for BUFR interface, set to 1 */
      size = maxlv * sizeof(gafloat);
      sz = size;
      pfi->sbuf = (gafloat *)galloc(sz,"sbuf1");
      if (pfi->sbuf==NULL) {
	gaprnt(0,"Open Error: memory allocation failed for sbuf\n");
	goto err8;
      }
      
    } else {
      /* for grids, the buffer is the size of one row */
      size = pfi->dnum[0] * sizeof(gadouble);
      sz = size;
      pfi->rbuf = (gadouble *)galloc(sz,"rbuf1");
      if (pfi->rbuf==NULL) {
	gaprnt(0,"Open Error: memory allocation failed for rbuf\n");
	goto err8;
      }
      /* pbuf is used for unpacking grib1 data */
      if (pfi->idxflg) {
	pfi->pbuf = (unsigned char *)galloc(sz,"pbuf1");
	if (pfi->pbuf==NULL)  {
	  gaprnt(0,"Open Error: memory allocation failed for pbuf\n");
	  goto err8;
	}
      }
    }
    pfi->ubuf = (char *)galloc(sz,"ubuf1");
    if (pfi->ubuf==NULL) {
      gaprnt(0,"Open Error: memory allocation failed for ubuf\n");
      goto err8;
    }
  }

  /* If a pre-projected grid, set up the interpolation constants. 
     If we're just checking the descriptor (mflag==2), no need to do this */
  if (pfi->ppflag && mflag!=2) {
    rc = gappcn(pfi,pdefop1,pdefop2);
    if (rc) goto retrn;
  }

  /* If the file name is a time series template, figure out
     which times go with which files, so we don't waste a lot
     of time later opening and closing files unnecessarily. */

  /* BUFR files are treated like templated files, so that the 
     data file isn't parsed until an I/O request is made */

  if (pfi->tmplat || pfi->bufrflg==1) {
    /* The fnums array is the size of the time axis 
       multiplied by the size of the ensemble axis. 
       It contains the t index which generates the filename 
       that contains the data for each timestep.
       If the ensemble has no data file for a given time, 
       the fnums value will be -1 */
    sz = sizeof(gaint)*pfi->dnum[3]*pfi->dnum[4];
    pfi->fnums = (gaint *)galloc(sz,"fnums1");   
    if (pfi->fnums==NULL) {
      gaprnt(0,"Open Error: memory allocation failed for fnums\n");
      goto err8;
    }
    /* get dt structure for t=1 */
    gr2t(pfi->grvals[3],1.0,&tdefi); 
    /* loop over ensembles */
    ens=pfi->ens1;
    e=1;
    while (e<=pfi->dnum[4]) {
      j = -1; 
      t=1;
      /* set fnums value to -1 for time steps before ensemble initial time */
      while (t<ens->gt) {
	pfi->fnums[(e-1)*pfi->dnum[3]+t-1] = j;                                                    
	t++;
      }
      j = ens->gt;
      /* get dt structure for ensemble initial time */
      gr2t(pfi->grvals[3],ens->gt,&tdefe);
      /* get filename for initial time of current ensemble member  */
      ch = gafndt(pfi->name,&tdefe,&tdefe,pfi->abvals[3],pfi->pchsub1,pfi->ens1,ens->gt,e,&flag);   
      if (ch==NULL) {
	snprintf(pout,255,"Open Error: couldn't determine data file name for e=%d t=%d\n",e,ens->gt);
	gaprnt(0,pout);
	goto err8;
      }
      /* set the pfi->tmplat flag to the flag returned by gafndt */
      if (flag==0) {
	gaprnt(1,"Warning: OPTIONS keyword \"template\" is used, but the \n");
	gaprnt(1,"   DSET entry contains no substitution templates.\n");
	pfi->tmplat = 1;
      } else {
	pfi->tmplat = flag; 
      }
      /* for non-indexed, non-netcdf/hdf, gridded data */
      if (pfi->type==1) {                /* gridded data   */
	if (pfi->ncflg==0) {             /* not netcdf/hdf */
	  if (pfi->idxflg==0) {          /* not indexed    */
	    if ((flag==1) && (pfi->dnum[4]>1)) {
	      gaprnt(0,"Open Error: If the data type is gridded binary, \n");
	      gaprnt(0,"  and the E dimension size is greater than 1 \n");
	      gaprnt(0,"  and templating in the T dimension is used,\n");
	      gaprnt(0,"  then templating in the E dimension must also be used.\n");
	      goto retrn;
	    }
	  }
	  else if (pfi->idxflg==1) {     /* GRIB1 */
	    if ((flag<2) && (pfi->dnum[4]>1)) {
	      gaprnt(0,"Open Error: If the data type is GRIB1 \n");
	      gaprnt(0,"  and the E dimension size is greater than 1 \n");
	      gaprnt(0,"  then templating in the E dimension must be used.\n");
	      goto retrn;
	    }
	  }
	}
      }
      pfi->fnums[(e-1)*pfi->dnum[3]+t-1] = j;                                                    
      /* loop over remaining valid times for this ensemble */
      for (t=ens->gt+1; t<ens->gt+ens->length; t++) {
	/* get filename for time index=t ens=e */
	gr2t(pfi->grvals[3],(gadouble)t,&tdef);
	pos = gafndt(pfi->name,&tdef,&tdefe,pfi->abvals[3],pfi->pchsub1,pfi->ens1,t,e,&flag);  
	if (pos==NULL) {
	  snprintf(pout,255,"Open Error: couldn't determine data file name for e=%d t=%d\n",e,t);
	  gaprnt(0,pout);
	  goto err8;
	}
	if (strcmp(ch,pos)!=0) {    /* filename has changed */
	  j = t;   
	  gree(ch,"f176");
	  ch = pos;
	}
	else {
	  gree(pos,"f176a");
	}
	pfi->fnums[(e-1)*pfi->dnum[3]+t-1] = j;                                                    
      }
      gree(ch,"f177");

      /* set fnums value to -1 for time steps after ensemble final time */
      j = -1;
      while (t<=pfi->dnum[3]) {
	pfi->fnums[(e-1)*pfi->dnum[3]+t-1] = j;                                                    
	t++;
      }
      e++; ens++;
    }
    pfi->fnumc = 0;
    pfi->fnume = 0;
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

 err3a_tdef:
  gaprnt (0,"Open Error:  Start Time missing in tdef card");
  gaprnt (0," starting value\n");
  goto err9;

 err3b_tdef:
  gaprnt (0,"Open Error:  Invalid start time in tdef card");
  gaprnt (0," starting value\n");
  goto err9;

 err3c_tdef:
  gaprnt (0,"Open Error:  Missing or invalid dimension");
  gaprnt (0," starting value\n");
  goto err9;

 err3:
  gaprnt (0,"Open Error:  Missing or invalid dimension");
  gaprnt (0," starting value\n");
  goto err9;

 err4a_tdef:
  gaprnt (0,"Open Error:  Time increment missing in tdef\n");
  gaprnt (0," use 1 for single time data\n");
  goto err9;

 err4b_tdef:
  gaprnt (0,"Open Error:  Invalid time increment in tdef\n");
  gaprnt (0," use 1 for single time data\n");
  goto err9;

 err4c_tdef:
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

 err6a:
  gaprnt (0,"Open Error:  Invalid x,y pair\n");
  goto err9;

 err7a: 
  gaprnt (0,"Open Error:  EOF occurred reading ensemble names\n");
  goto err9;

 err7b:
  gaprnt (0,"Open Error:  Blank record found in EDEF data\n");
  goto err9;

 err7c:
  gaprnt (0,"Open Error:  Invalid ensemble grib codes\n");
  goto err9;

 err7d:
  gaprnt (0,"Open Error:  Invalid ensemble name\n");
  goto err9;

 err7e:
  gaprnt (0,"Open Error:  Invalid ensemble record\n");
  goto err9;

 err8:
  gaprnt (0,"Open Error:  Memory allocation Error in gaddes.c\n");
  goto retrn;

 err8a:
  gaprnt (0,"Open Error:  Version 4 index file not compatible with size of off_t");
  goto retrn;

 err9:
  gaprnt (0,"  --> The invalid description file record is: \n");
  gaprnt (0,"  --> ");
  gaprnt (0,mrec);
  gaprnt (0,"\n");

 retrn:
  gaprnt (0,"  The data file was not opened. \n");
  fclose (descr);
  if (mflflg) fclose(mfile);
  if (pdfi) fclose(pdfi);
  return(1);

}

/* Process linear scaling args */

gaint deflin (char *ch, struct gafile *pfi, gaint dim, gaint flag) {
gadouble *vals,v1,v2;
size_t sz;

  sz = sizeof(gadouble)*6;
  vals = (gadouble *)galloc(sz,"vals1");
  if (vals==NULL) return (-1);

  if ((ch = nxtwrd(ch))==NULL) goto err1;
  if (getdbl(ch,&v1)==NULL) goto err1;
  if (flag) v2 = 1.0;
  else {
    if ((ch = nxtwrd(ch))==NULL) goto err2;
    if (getdbl(ch,&v2)==NULL) goto err2;
  }
  if (dim!=3 && v2<=0.0) goto err2;
  *(vals)   = v2;
  *(vals+1) = v1 - v2;
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
  gree(vals,"f178");
  return (1);

err2:
  gaprnt (0,"Open Error:  Missing or invalid dimension");
  gaprnt (0," increment value\n");
  gree(vals,"179");
  return (1);
}

/* Process levels values in def record */
/* Return codes:  -1 is memory allocation error, 1 is other error */

gaint deflev (char *ch, char *rec, struct gafile *pfi, gaint dim) {
gadouble *vvs,*vals,v1;
gaint i;
size_t sz;

  if (pfi->dnum[dim]==1) {
    i = deflin (ch, pfi, dim, 1);
    return (i);
  }

  sz = (pfi->dnum[dim]+5)*sizeof(gadouble);
  vals = (gadouble *)galloc(sz,"vals2");
  if (vals==NULL) return (-1);

  vvs = vals;
  *vvs = (gadouble)pfi->dnum[dim];
  vvs++;
  for (i=0; i<pfi->dnum[dim]; i++) {
    if ( (ch = nxtwrd(ch))==NULL) {
      if (fgets(rec,256,descr)==NULL) goto err2;
      ch = rec;
      while (*ch==' ' || *ch=='\t') ch++;
      if (*ch=='\0' || *ch=='\n') goto err3;
    }
    if (getdbl(ch,&v1)==NULL) goto err1;
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
  gree(vals,"f180");
  return (1);

err2:
  gaprnt (0,"Open Error:  Unexpected EOF reading descriptor file\n");
  gaprnt (0,"   EOF occurred reading LEVELS values\n");
  gree(vals,"f181");
  return (1);

err3:
  gaprnt (0,"Open Error:  Blank Record found in LEVELS data\n");
  gree(vals,"f182");
  return (1);
}


/* Process descriptor file attribute metadata. 
   Return -1 on error. */
gaint ddfattr (char *ch, struct gafile *pfi) {
  struct gaattr *newattrib,*attrib;
  
  /* check for presence of attribute metadata */
  if ((ch=nxtwrd(ch))==NULL ) { 
    gaprnt (2,"Warning: Missing all required attribute fields \n"); 
    return (-1);
  } 
  /* parse the attribute */
  newattrib = parseattr(ch);
  if (newattrib == NULL) return (-1);
  newattrib->fromddf = 1;

  /* hang the new attribute off the gafile structure */
  if (pfi->attr) {
    /* advance to end of chain */
    attrib=pfi->attr;
    while (attrib->next) attrib = attrib->next;
    /* add new link */
    attrib->next = newattrib;
  }
  else {
    /* add first link */
    pfi->attr = newattrib;
  } 
  return(0);
}


/* Parse attribute metadata  
   Return NULL for error, pointer to gaattr structure if successful */
struct gaattr *parseattr (char *ch) {

  gaint jj,len,nctype;
  char varname[512], attrtype[512], attrname[512], attrvalue[512];
  char *ptr,*cmd;
  void *value=NULL;
  gafloat *fvalues;
  gadouble dval,*dvalues;
  long *lvalues;
  short *svalues;
  gaint ival;
  struct gaattr *attrib;
  size_t sz;
  
  /* check for presence of attribute metadata */
  if ((cmd=nxtwrd(ch))==NULL ) {
    gaprnt (2,"Warning: Missing all required attribute fields \n"); 
    goto err;
  } 
  
  /* get the variable name */
  len = 0;
  while (*(ch+len)!=' ' && *(ch+len)!='\n' && *(ch+len)!='\t') len++;
  for (jj=0; jj<len; jj++) varname[jj] = *(ch+jj);
  varname[len] = '\0';
  
  /* get the attribute type */
  if ( (ch=nxtwrd(ch))==NULL ) {
    gaprnt (2,"Warning: Missing attribute type, name, and value \n"); 
    goto err;
  } 
  len = 0;
  while (*(ch+len)!=' ' && *(ch+len)!='\n' && *(ch+len)!='\t') len++;
  for (jj=0; jj<len; jj++) attrtype[jj] = *(ch+jj);
  attrtype[len] = '\0';
  
  /* get the attribute name */
  if ( (ch=nxtwrd(ch))==NULL ) {
    gaprnt (2,"Warning: Missing attribute name and value \n"); 
    goto err;
  }
  len = 0;
  while (*(ch+len)!=' ' && *(ch+len)!='\n' && *(ch+len)!='\t') len++;
  for (jj=0; jj<len; jj++) attrname[jj] = *(ch+jj);
  attrname[len] = '\0';
  
  /* Set attribute nctype if it matches OPeNDAP/NetCDF data types */
  if (!(strncmp(attrtype,"Byte",4)) || 
      !(strncmp(attrtype,"byte",4))) 
    nctype = 1;
  else if (!(strncmp(attrtype,"String",6)) || 
	   !(strncmp(attrtype,"Str",   3)) ||
	   !(strncmp(attrtype,"Url",   3)) ||
	   !(strncasecmp(attrtype,"char",4))) 
    nctype = 2; 
  else if (!(strncmp(attrtype,"Int16", 5)) || 
	   !(strncmp(attrtype,"UInt16",6)) || 
	   !(strncasecmp(attrtype,"short",5))) 
    nctype = 3;
  else if (!(strncmp(attrtype,"Int32", 5)) || 
	   !(strncmp(attrtype,"UInt32",6)) ||
	   !(strncasecmp(attrtype,"int",3)) || 
	   !(strncasecmp(attrtype,"long",4))) 
    nctype = 4;
  else if (!(strncmp(attrtype,"Float32",7)) ||
	   !(strncasecmp(attrtype,"float",5))) 
    nctype = 5;
  else if (!(strncmp(attrtype,"Float64",7)) ||
	   !(strncasecmp(attrtype,"double",6))) 
    nctype = 6;
  else 
    nctype = 0;
  
  /* get the attribute value */
  if ( (ch=nxtwrd(ch))==NULL ) {
    gaprnt (2,"Warning: Missing attribute value \n"); 
    goto err;
  } 
  getstr (attrvalue, ch, 512);
  ptr = ch;

  /* Get attribute length*/  
  if (nctype <= 2) {
    len=strlen(attrvalue);
  }
  else {
    /* find total number of comma-delimited numerals */
    jj = 0;          
    while (1) {
      if ((ptr=getdbl(ptr,&dval))==NULL) break;      
      jj++;  
      /* advance through comma-delimited list of levels args */
      while (*ptr==' ') ptr++;               /* advance past whitespace */
      if (*ptr=='\0' || *ptr=='\n') break;   
      if (*ptr!=',') break;
      ptr++;                                /* advance past comma */
      while (*ptr==' ') ptr++;               /* advance past whitespace */
      if (*ptr=='\0' || *ptr=='\n') break;
    }
    len = jj;
  }

  /* allocate space for the attribute value */
  if (nctype <= 2) {
    sz = (len+1) * sizeof(char);
    if ((value = (void *)galloc(sz,"valuec")) == NULL) {
       gaprnt (0,"Error: memory allocation failed for attribute metadata\n");
       goto err;
    }
    strcpy(value,attrvalue);
  } else if (nctype == 3) {
    sz = len * sizeof(short);
    if ((svalues = (short *)galloc(sz,"values")) == NULL) {
       gaprnt (0,"Error: memory allocation failed for attribute metadata\n");
       goto err;
    }
    value = (void*)svalues;
    ptr = &attrvalue[0];  
    for (jj=0;jj<len;jj++) {
      if ((ptr=intprs(ptr,&ival)) == NULL) {
       gaprnt (0,"Error: failed to parse int attribute metadata\n");
       goto err;
      }
      *svalues = (short)ival;
      svalues++;
      ptr++;    /* advance past the comma */
    }
  } else if (nctype == 4) {
    sz = len * sizeof(long);
    if ((lvalues = (long *)galloc(sz,"valuel")) == NULL) {
       gaprnt (0,"Error: memory allocation failed for attribute metadata\n");
       goto err;
    }
    value = (void*)lvalues;
    ptr = &attrvalue[0];  
    for (jj=0;jj<len;jj++) {
      if ((ptr=intprs(ptr,&ival)) == NULL) {
       gaprnt (0,"Error: failed to parse int attribute metadata\n");
       goto err;
      }
      *lvalues = (long)ival;
      lvalues++;
      ptr++;    /* advance past the comma */
    }
  } else if (nctype == 5) {
    sz = len * sizeof(gafloat);
    if ((fvalues = (gafloat *)galloc(sz,"valuef")) == NULL) {
       gaprnt (0,"Error: memory allocation failed for attribute metadata\n");
       goto err;
    }
    value = (void*)fvalues;
    ptr = &attrvalue[0];  
    for (jj=0;jj<len;jj++) {
      if ((ptr=getdbl(ptr,&dval)) == NULL) {
       gaprnt (0,"Error: failed to parse float attribute metadata\n");
       goto err;
      }
      *fvalues = (gafloat)dval;
      fvalues++;
      ptr++;    /* advance past the comma */
    }
  } else if (nctype == 6) {
    sz = len * sizeof(gadouble);
    if ((dvalues = (void *)galloc(sz,"valued")) == NULL) {
       gaprnt (0,"Error: memory allocation failed for attribute metadata\n");
       goto err;
    }
    value = (void*)dvalues;
    ptr = &attrvalue[0];  
    for (jj=0;jj<len;jj++) {
      if ((ptr=getdbl(ptr,&dval)) == NULL) {
       gaprnt (0,"Error: failed to parse double attribute metadata\n");
       goto err;
      }
      *dvalues = dval;
      dvalues++;
      ptr++;    /* advance past the comma */
    }
  } 
  else {
    snprintf(pout,255,"Error: attribute data type not handled: nctype = %d \n",nctype);
    gaprnt(0,pout);
    goto err;
  }
  
  /* Everything parsed and allocated OK, so allocate and populate a gaattr structure */
  sz = sizeof(struct gaattr);
  attrib = (struct gaattr *) galloc(sz,"attrib");
  if (attrib != NULL) {
    strcpy(attrib->varname,varname);
    strcpy(attrib->name,attrname);
    strcpy(attrib->type,attrtype);
    attrib->nctype = nctype;
    attrib->len = len;
    attrib->value = value;
    attrib->fromddf = 0;
    attrib->next = NULL;
  } else {
    gaprnt (0,"Error: memory allocation failed for attribute metadata\n");
    goto err;
  }
  /* return the pointer to the attribute structure */
  return attrib;

 err:
  if (value) gree(value,"f184");
  return(NULL);
}
 

/* Allocate and initialize a gafile structure */

struct gafile *getpfi (void) {
struct gafile *pfi;
gaint i;
size_t sz;

  sz = sizeof(struct gafile);
  pfi = (struct gafile *)galloc(sz,"pfi");
  if (pfi==NULL) return (NULL);

  pfi->type = 1;         /* Assume grid unless told otherwise */
  pfi->tlpflg = 0;       /* Assume file not circular */
  pfi->bswap = 0;        /* Assume no byte swapping needed */
  pfi->seqflg = 0;       /* Assume direct access */
  pfi->yrflg = 0;        /* Assume south to north */
  pfi->zrflg = 0;        /* Assume bottom to top */
  pfi->idxflg = 0;       /* Assume binary */
  pfi->ncflg = 0;        /* Assume not netcdf */
  pfi->bufrflg = 0;      /* Assume not bufr */
  pfi->ncid = -999;      /* No netcdf file open */
  pfi->sdid = -999;      /* No hdfsds file open */
  pfi->h5id = -999;      /* No hdf5 file open */
  pfi->fhdr = 0;         /* Assume no file header */
  pfi->xyhdr=0;          /* Assume no xyheader */
  pfi->fseq = -999;      /* No sequence number assigned */
  pfi->dhandle = -999;   /* Assume not a gadods stn data set */
  pfi->packflg = 0;      /* Assume data are not packed */
  pfi->undefattrflg = 0; /* Assume no undef attribute name given */
  pfi->pa2mb = 0;        /* Assume pressure values are given in mb */
  pfi->undef = -9.99e8; 
  pfi->ppisiz = 0;
  pfi->ppjsiz = 0;
  pfi->bufrinfo = NULL;
  pfi->bufrdset = NULL;
  pfi->attr = NULL;
  pfi->scattr = NULL;   
  pfi->ofattr = NULL;   
  pfi->undefattr = NULL;   
  pfi->tempname = NULL;
  pfi->mnam = NULL;
  pfi->infile = NULL;
  pfi->rbuf = NULL;
  pfi->sbuf = NULL;
  pfi->pbuf = NULL;
  pfi->bbuf = NULL;
  pfi->ubuf = NULL;
  pfi->tstrt = NULL;
  pfi->tcnt = NULL;
  pfi->mfile = NULL;
  pfi->vnum = 0;
  pfi->pvar1 = NULL;
  pfi->ens1 = NULL;
  pfi->pindx = NULL;
  pfi->pindxb = NULL;
  pfi->fnums = NULL;
  pfi->pchsub1 = NULL;
#if GRIB2
  pfi->g2indx = NULL;
#endif
  pfi->wrap = 0;        /* Assume no wrapping */
  for (i=0; i<5; i++) pfi->dimoff[i] = 0;
  pfi->title[0] = '\0';
  pfi->grvals[0] = NULL;
  pfi->grvals[1] = NULL;
  pfi->grvals[2] = NULL;
  pfi->grvals[3] = NULL;
  pfi->grvals[4] = NULL;
  pfi->grbgrd = -999;
  pfi->tmplat = 0;
  pfi->errcnt = 0;
  pfi->errflg = 0;
  pfi->ppflag = 0;  /* Assume lat-lon grid */
  pfi->ppwrot = 0;  /* Assume no wind rotataion */
  pfi->pdefgnrl = 0; 
  for (i=0; i<9; i++) pfi->ppi[i] = NULL;
  for (i=0; i<9; i++) pfi->ppf[i] = NULL;
  pfi->ppw = NULL;
  pfi->calendar=0;
  pfi->nsdfdims = 0; 
  for (i=0; i<100; i++) pfi->sdfdimids[i]=-1;
  for (i=0; i<100; i++) pfi->sdfdimsiz[i]=-1;
  for (i=0; i<100; i++) pfi->sdfdimnam[i][0]='\0';
  pfi->cachesize = -1;   /* if <0, a good default cache size has not been calcuated */
  return (pfi);
}

/* Free a gafile structure and associated storage.  If the flag is
   true, DO NOT free the storage related to scaling transforms,
   since someone, somewhere,  may still be pointing to that. */

void frepfi (struct gafile *pfi, gaint flag) {
struct gaattr *attrib, *nextattrib;
struct gaindx *pindx;
struct gaindxb *pindxb;
#if GRIB2
struct gag2indx *g2indx;
#endif
struct gachsub *pchsub,*pch2;
gaint i;

/* these are listed in the order in which they appear in the pfi declaration in grads.h */
  if (pfi->tempname) gree(pfi->tempname,"f56");
  if (pfi->mnam)   gree(pfi->mnam,"f57");
  if (pfi->rbuf)   gree(pfi->rbuf,"f58");
  if (pfi->sbuf)   gree(pfi->sbuf,"f58");
  if (pfi->pbuf)   gree(pfi->pbuf,"f59");
  if (pfi->bbuf)   gree(pfi->bbuf,"f60");
  if (pfi->ubuf)   gree(pfi->ubuf,"f61");
  if (pfi->tstrt)  gree(pfi->tstrt,"f62");
  if (pfi->tcnt)   gree(pfi->tcnt,"f63");
  if (pfi->pvar1)  gree(pfi->pvar1,"f64");
  if (pfi->ens1)   gree(pfi->ens1,"f65");
  for (i=0; i<9; i++) if (pfi->ppi[i]) gree(pfi->ppi[i],"f66");
  for (i=0; i<9; i++) if (pfi->ppf[i]) gree(pfi->ppf[i],"f67");
  if (pfi->ppw) gree(pfi->ppw,"f68");
  if (!flag) for (i=0; i<5; i++) {
    if (pfi->grvals[i]) gree(pfi->grvals[i],"f69");
  }
  if (pfi->pindx) {
    pindx = pfi->pindx;
    if (pindx->hipnt)  gree(pindx->hipnt,"f70");
    if (pindx->hfpnt)  gree(pindx->hfpnt,"f71");
    if (pindx->intpnt) gree(pindx->intpnt,"f72");
    if (pindx->fltpnt) gree(pindx->fltpnt,"f73");
    gree(pindx,"f74");
  }
  if (pfi->pindxb) {
    pindxb = pfi->pindxb;
    if (pindxb->bigpnt)  gree(pindxb->bigpnt,"b98");
    gree(pindxb,"b99");
  }
#if GRIB2
  if (pfi->g2indx) {
    g2indx = pfi->g2indx;
    if (g2indx->g2intpnt) gree(g2indx->g2intpnt,"f75");
    if (g2indx->g2bigpnt) gree(g2indx->g2bigpnt,"b75");
    gree(g2indx,"f76");
  }
#endif
  if (pfi->fnums)  gree(pfi->fnums,"f77");
  pchsub = pfi->pchsub1;
  while (pchsub) {
    if (pchsub->ch) gree(pchsub->ch,"f78");
    pch2 = pchsub->forw;
    gree(pchsub,"f79");
    pchsub = pch2;
  }
  if (pfi->scattr) gree(pfi->scattr,"f80");
  if (pfi->ofattr) gree(pfi->ofattr,"f81");
  if (pfi->undefattr) gree(pfi->undefattr,"f82");
  if (pfi->bufrinfo) gree(pfi->bufrinfo,"f83");
#ifndef STNDALN
  if (pfi->bufrdset) gabufr_close(pfi->bufrdset);
#endif
  while (pfi->attr != NULL) {
    /* point to first block in chain */
    attrib = pfi->attr;  
    if (attrib->next == NULL) {
      /* first block is only block */
      pfi->attr = NULL; 
    }
    else {
      /* move start of chain from 1st to 2nd block */
      nextattrib = attrib->next;
      pfi->attr = nextattrib;
    }
    /* release memory from 1st block */
    if (attrib->value) gree(attrib->value,"f85");
    gree(attrib,"f86");
  }
  gree(pfi,"f87");
}


/* Routine to calculate or input the interpolation constants needed for
   the implicit interpolation from pre-projected grids to lat-lon. */

gaint gappcn (struct gafile *pfi, gaint pdefop1, gaint pdefop2) {
gaint size,i,j,ii,jj;
gadouble lat,lon,rii,rjj;
gadouble *dx, *dy, *dw, dum;
gadouble pi;
gafloat *fvals=NULL;
gaint *ioff, rdw, rc, pnum, wflg;
size_t sz;

  dw=NULL;
  size = pfi->dnum[0]*pfi->dnum[1];

  /* Allocate space needed for the ppi and ppf grids */
  if (pfi->ppflag != 8) {
    sz = sizeof(gaint)*size;
    if ((pfi->ppi[0] = (gaint*)galloc(sz,"ppi0")) == NULL) goto merr;
    sz = sizeof(gadouble)*size;
    if ((pfi->ppf[0] = (gadouble*)galloc(sz,"ppf0")) == NULL) goto merr;
    sz = sizeof(gadouble)*size;
    if ((pfi->ppf[1] = (gadouble*)galloc(sz,"ppf1")) == NULL) goto merr;
    if (pfi->ppwrot) {
      sz = sizeof(gadouble)*size;
      if ((pfi->ppw  = (gadouble *)galloc(sz,"ppw")) == NULL) goto merr;
    }
  }

  /* pdef bilin */
  if (pfi->ppflag==7) {
    /* allocate memory to temporarily store array of floats to be read from pdef file */
    sz = sizeof(gafloat)*size;
    if ((fvals = (gafloat*)galloc(sz,"ppfvals")) == NULL) goto merr;

    if (pdefop1==2) {  /* sequential -- read the 4-byte header */
      rc = fread(&rdw, sizeof(gaint), 1, pdfi);
      if (rc!=1) goto merr2;
    }

    /* read the grid of pdef ivals into fvals array */
    rc = fread(fvals, sizeof(gafloat), size, pdfi); if (rc!=size) goto merr2; 
    /* byte swap if necessary */
    if ((pdefop2==2 && !BYTEORDER) || (pdefop2==3 &&  BYTEORDER)) gabswp (fvals,size);
    /* cast to doubles */
    for (i=0; i<size; i++) *(pfi->ppf[0]+i) = (gadouble)fvals[i];

    if (pdefop1==2) {  /* sequential -- read the 4-byte footer and next header */
      rc = fread(&rdw, sizeof(gaint), 1, pdfi); if (rc!=1) goto merr2;
      rc = fread(&rdw, sizeof(gaint), 1, pdfi); if (rc!=1) goto merr2;
    }
 
    /* read the grid of pdef jvals into fvals array */
    rc = fread(fvals, sizeof(gafloat), size, pdfi); if (rc!=size) goto merr2;
    /* byte swap if necessary */
    if ((pdefop2==2 && !BYTEORDER) || (pdefop2==3 &&  BYTEORDER)) gabswp (fvals,size);
    /* cast to doubles */
    for (i=0; i<size; i++) *(pfi->ppf[1]+i) = (gadouble)fvals[i];

    if (pdefop1==2) {  /* sequential -- read the 4-byte footer and next header */
      rc = fread(&rdw, sizeof(gaint), 1, pdfi); if (rc!=1) goto merr2;
      rc = fread(&rdw, sizeof(gaint), 1, pdfi); if (rc!=1) goto merr2;
    }

    /* read the grid of wind rotation vals */
    rc = fread(fvals, sizeof(gafloat), size, pdfi); if (rc!=size) goto merr2;
    /* byte swap if necessary */
    if ((pdefop2==2 && !BYTEORDER) || (pdefop2==3 &&  BYTEORDER)) gabswp (fvals,size);
    /* cast to doubles */
    for (i=0; i<size; i++) *(pfi->ppw+i) = (gadouble)fvals[i];

    /* Fill grids of file offsets and weights (dx,dy) for pdef grid interpolation */
    ioff = pfi->ppi[0];
    dx = pfi->ppf[0];
    dy = pfi->ppf[1];
    dw = pfi->ppw;
    wflg = 0;
    for (j=0; j<pfi->dnum[1]; j++) {
      for (i=0; i<pfi->dnum[0]; i++) {
        if (*dx < 0.0) *ioff = -1;
        else {
	  /* ii and jj are integer parts of i and j values read from pdef bilin file */
          ii = (gaint)(*dx);
          jj = (gaint)(*dy);
	  /* dx and dy are now the remainder after the integer part is subtracted out */
          *dx = *dx - (gadouble)ii;
          *dy = *dy - (gadouble)jj;
	  /* if ii and jj values are outside the native grid, they are not used */
          if (ii<1 || ii>pfi->ppisiz-1 || jj<1 || jj>pfi->ppjsiz-1) {
            *ioff = -1;
          } else {
	    /* ioff index values (pfi->ppi) start from 0 instead of 1 */
            *ioff = (jj-1)*pfi->ppisiz + ii - 1;
          }
        }
        if (fabs(*dw) > 0.00001) wflg = 1;
        ioff++; dx++; dy++, dw++;
      }
    }
    pfi->ppwrot = wflg;

  /* When pdef is a file, read in the offsets of the points to use and their weights, 
     as well as the array of wind rotation values to use */

  } else if (pfi->ppflag==8) {
    pnum = (gaint)(pfi->ppvals[0]+0.1);
    /* allocate memory for array of floats to be read from pdef file */
    sz = sizeof(gafloat)*size;
    if ((fvals = (gafloat*)galloc(sz,"ppfvals")) == NULL) goto merr;

    /* get weights and offsets from pdef file */
    for (i=0; i<pnum; i++) {
      /* allocate memory for the array of offsets */
      sz = sizeof(gaint)*size;
      if ((pfi->ppi[i] = (gaint*)galloc(sz,"ppi3")) == NULL) goto merr;
      /* sequential -- header */
      if (pdefop1==2) { rc = fread(&rdw, sizeof(gaint), 1, pdfi); if (rc!=1) goto merr2; }
      /* read the offsets */
      rc = fread(pfi->ppi[i], sizeof(gaint), size, pdfi); if (rc!=size) goto merr2;
      /* byte swap if necessary */
      if ((pdefop2==2 && !BYTEORDER) || (pdefop2==3 &&  BYTEORDER)) 
	gabswp((gafloat *)(pfi->ppi[i]),size);
      /* sequential -- footer */
      if (pdefop1==2) { rc = fread(&rdw, sizeof(gaint), 1, pdfi); if (rc!=1) goto merr2; }
 
      /* allocate memory for array of weights */
      sz = sizeof(gadouble)*size;
      if ((pfi->ppf[i] = (gadouble*)galloc(sz,"ppf2")) == NULL) goto merr;
      /* sequential -- header */
      if (pdefop1==2) { rc = fread(&rdw, sizeof(gaint), 1, pdfi); if (rc!=1) goto merr2; }
      /* read the floating-point weights */
      rc = fread(fvals, sizeof(gafloat), size, pdfi); if (rc!=size) goto merr2;
      /* byte swap if necessary */
      if ((pdefop2==2 && !BYTEORDER) || (pdefop2==3 &&  BYTEORDER)) gabswp(fvals,size);
      /* cast to doubles */
      for (j=0; j<size; j++) *(pfi->ppf[i]+j) = (gadouble)fvals[j];
      /* sequential -- footer */
      if (pdefop1==2) { rc = fread(&rdw, sizeof(gaint), 1, pdfi); if (rc!=1) goto merr2; }
    }

    /* allocate memory and read in the wind rotation values */
    sz = sizeof(gadouble)*size;
    if ((pfi->ppw = (gadouble *)galloc(sz,"ppw2")) == NULL) goto merr;
    /* sequential -- header */
    if (pdefop1==2) { rc = fread(&rdw, sizeof(gaint), 1, pdfi); if (rc!=1) goto merr2; }
    rc = fread(fvals, sizeof(gafloat), size, pdfi); if (rc!=size) goto merr2;
    /* byte swap if necessary */
    if ((pdefop2==2 && !BYTEORDER) || (pdefop2==3 &&  BYTEORDER)) gabswp(fvals,size);
    /* cast to doubles */
    for (i=0; i<size; i++) *(pfi->ppw+i) = (gadouble)fvals[i];

    /* set wind rotation flag */
    dw = pfi->ppw;
    wflg = 0;
    for (i=0; i<size; i++) {
      if (fabs(*dw) > 0.00001) wflg = 1;
      dw++;
    }
    pfi->ppwrot = wflg;
    
    /* If native data is grib, and the "pdef file" keyword is used,
       then the offsets in the file are assumed to be 0-based.
       The code in gaprow() expects 1-based offsets, so we add 1
       and check to make sure offsets don't exceed isize*jsize. */
    if (pfi->idxflg && pfi->type==1 && pfi->pdefgnrl==0) {
      for (i=0; i<pnum; i++) {
	for (j=0; j<size; j++) {
	  if (*(pfi->ppi[i]+j) == pfi->ppisiz * pfi->ppjsiz) {
	    gaprnt (0,"PDEF FILE Error: The offsets in the pdef file for native \n");
	    gaprnt (0,"  GRIB data must be 0-based (i.e., >= 0 and < isize*jsize). \n"); 
	    gaprnt (0,"  Use the PDEF GENERAL keyword for 1-based file offsets.\n"); 
	    goto err;
	  }
	  *(pfi->ppi[i]+j) = 1 + *(pfi->ppi[i]+j);
	}
      }
    }
    /* If native data is NOT grib, and the "pdef file" keyword is used,
       then the offsets in the file are assumed to be 1-based.
       The code in gaprow() expects 1-based offsets, so we just
       check to make sure offsets don't equal 0. */
    if (pfi->idxflg==0 && pfi->type==1 && pfi->pdefgnrl==0) {
      for (i=0; i<pnum; i++) {
	for (j=0; j<size; j++) {
	  if (*(pfi->ppi[i]+j) == 0) {
	    gaprnt (0,"PDEF FILE Error: The offsets in the pdef file \n");
	    gaprnt (0,"  must be 1-based (i.e., > 0 and <= isize*jsize). \n"); 
	    goto err;
	  }
	}
      }
    }
    /* The "pdef general" keyword means the offsets in the file are always 1-based.
       Check to make sure offsets don't equal 0. */
    if (pfi->pdefgnrl==1) {
      for (i=0; i<pnum; i++) {
	for (j=0; j<size; j++) {
	  if (*(pfi->ppi[i]+j) == 0) {
	    gaprnt (0,"PDEF GENERAL Error: The offsets in the pdef file \n");
	    gaprnt (0,"  must be 1-based (i.e., > 0 and <= isize*jsize). \n"); 
	    goto err;
	  }
	}
      }
    }

  } /* matches  else if (pfi->ppflag==8) */

  else {

    /* When a supported projection is specified, calculate 
       three constants at each lat-lon grid point: offset 
       of the ij gridpoint, and the delta x and delta y values. */
    
    pi = acos(-1.0);
    ioff = pfi->ppi[0];
    dx = (gadouble*)pfi->ppf[0];
    dy = (gadouble*)pfi->ppf[1];
    if (pfi->ppwrot) dw = (gadouble*)pfi->ppw;
    /* get i,j values in preprojected grid for each lat/lon point */
    for (j=0; j<pfi->dnum[1]; j++) {
      lat = pfi->gr2ab[1](pfi->grvals[1],(gadouble)(j+1));
      for (i=0; i<pfi->dnum[0]; i++) {
        lon = pfi->gr2ab[0](pfi->grvals[0],(gadouble)(i+1));
	if (pfi->ppflag==3) {             
	  if (pfi->ppwrot) {               /* PDEF lccr */
	    ll2lc (pfi->ppvals, lat, lon, &rii, &rjj, dw);   
	  } 
	  else {                           /* PDEF lcc */
	    ll2lc (pfi->ppvals, lat, lon, &rii, &rjj, &dum);
	  }
        } 
	else if (pfi->ppflag==4) {         /* PDEF eta.u */
          ll2eg (pfi->ppisiz,pfi->ppjsiz,pfi->ppvals, lon, lat, &rii, &rjj, dw);
        } 
	else if (pfi->ppflag==5) {         /* PDEF pse */
          ll2pse (pfi->ppisiz,pfi->ppjsiz,pfi->ppvals, lon, lat, &rii, &rjj);
        } 
	else if (pfi->ppflag==6) {         /* PDEF ops */  
          ll2ops (pfi->ppvals, lon, lat, &rii, &rjj);
        } 
	else if (pfi->ppflag==9) {         
	  if(pfi->ppwrot) {                /* PDEF rotllr */
	    ll2rotll (pfi->ppvals, lat, lon, &rii, &rjj, dw);
	  } else {                         /* PDEF rotll */
	    ll2rotll (pfi->ppvals, lat, lon, &rii, &rjj, &dum);
	  }
	}
	else {                             /* PDEF nps and sps */
	  w3fb04(lat, -1.0*lon, pfi->ppvals[3], -1.0*pfi->ppvals[2], &rii, &rjj);
          rii = rii + pfi->ppvals[0];  /* Normalize based on pole point */ 
          rjj = rjj + pfi->ppvals[1];
          *dw = (pfi->ppvals[2]-lon) * pi/180.0;  /* wind rotation amount */
          if (pfi->ppflag==2) *dw = pi - *dw;
        }
        ii = (gaint)rii;
        jj = (gaint)rjj;
        *dx = rii - (gadouble)ii;
        *dy = rjj - (gadouble)jj;
        if (ii<1 || ii>pfi->ppisiz-1 || 
	    jj<1 || jj>pfi->ppjsiz-1) {
          *ioff = -1;
        } else {
          *ioff = (jj-1)*pfi->ppisiz + ii - 1;
        }
	ioff++; dx++; dy++;
        if (pfi->ppwrot) dw++;
      }
    }
  }
  if (fvals!=NULL) gree(fvals,"f80g");
  return(0);

merr:
  gaprnt (0,"Open Error:  Memory allocation error in pdef handler\n");
  goto err;
merr2:
  gaprnt (0,"Open Error:  I/O Error on pdef file read\n");
  goto err;

err:
  if (pfi->ppi[0]!=NULL) gree(pfi->ppi[0],"f80a");
  if (pfi->ppf[0]!=NULL) gree(pfi->ppf[0],"f80c");
  if (pfi->ppf[1]!=NULL) gree(pfi->ppf[1],"f80d");
  if (pfi->ppwrot && pfi->ppw!=NULL) gree(pfi->ppw,"f80e");
  if (fvals!=NULL) gree(fvals,"f80f");
  return(1);

}

void w3fb04 (gadouble alat, gadouble along, gadouble xmeshl, gadouble orient,
     gadouble *xi, gadouble *xj) {

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

static gadouble d2r = 3.14159/180.0;
static gadouble earthr = 6371.2;

gadouble re,xlat,wlong,r;

      re   = (earthr * 1.86603) / xmeshl;
      xlat = alat * d2r;

      if (xmeshl>0.0) {
        wlong = (along + 180.0 - orient) * d2r;
        r     = (re * cos(xlat)) / (1.0 + sin(xlat));
        *xi    = r * sin(wlong);
        *xj    = r * cos(wlong);

      } else {

        re    = -re;
        xlat =  -xlat;
        wlong = (along - orient) * d2r;
        r     = (re * cos(xlat)) / (1.0+ sin(xlat));
        *xi   =  r * sin(wlong);
        *xj   = -r * cos(wlong);
      }
}

/* Lambert conformal conversion */

void ll2lc (gadouble *vals, gadouble grdlat, gadouble grdlon, gadouble *grdi, gadouble *grdj, gadouble *wrot) {

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
c  vals+0    latref: latitude at reference point (iref,jref)
c 		    
c  vals+1    lonref: longitude at reference point (iref,jref)
c 		    
c  vals+2    iref:   i-coordinate value of reference point
c 		    
c  vals+3    jref:   j-coordinate value of reference point
c 		    
c  vals+4    stdlt1: standard latitude of grid (S True lat)
c 		    
c  vals+5    stdlt2: second standard latitude of grid (only required
c                    if igrid = 2, lambert conformal) (N True lat)
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

  gadouble pi, pi2, pi4, d2r, r2d, radius, omega4;
  gadouble gcon,ogcon,H,deg,cn1,cn2,cn3,cn4,rih,xih,yih,rrih,check;
  gadouble alnfix,alon,x,y,windrot;
  gadouble latref,lonref,iref,jref,stdlt1,stdlt2,stdlon,delx,dely;

  pi = 4.0*atan(1.0);
  pi2 = pi/2.0;
  pi4 = pi/4.0;
  d2r = pi/180.0;
  r2d = 180.0/pi;
  radius = 6371229.0;
  omega4 = 4.0*pi/86400.0;

  latref = *(vals+0);    
  lonref = *(vals+1);    
  iref   = *(vals+2);    
  jref   = *(vals+3);    
  stdlt1 = *(vals+4);    
  stdlt2 = *(vals+5);    
  stdlon = *(vals+6);    
  delx   = *(vals+7);    
  dely   = *(vals+8);    
                    
/* case where standard lats are the same */
/* corrected by Dan Geiszler of NRL; fabs of the 
   lats was required for shem cases */

  if (stdlt1 == stdlt2) {
    gcon = sin(d2r*(fabs(stdlt1)));
  } else {
    gcon = (log(sin((90.0-fabs(stdlt1))*d2r))
	   -log(sin((90.0-fabs(stdlt2))*d2r)))
          /(log(tan((90.0-fabs(stdlt1))*0.5*d2r))
           -log(tan((90.0-fabs(stdlt2))*0.5*d2r)));
  }
  ogcon = 1.0/gcon;
  H = fabs(stdlt1)/(stdlt1);        /* 1 for NHem, -1 for SHem */
  cn1 = sin((90.0-fabs(stdlt1))*d2r);
  cn2 = radius*cn1*ogcon;
  deg = (90.0-fabs(stdlt1))*d2r*0.5;
  cn3 = tan(deg);
  deg = (90.0-fabs(latref))*d2r*0.5;
  cn4 = tan(deg);
  rih = cn2*pow((cn4/cn3),gcon);

  xih =  rih*sin((lonref-stdlon)*d2r*gcon);
  yih = -rih*cos((lonref-stdlon)*d2r*gcon)*H;
  deg = (90.0-grdlat*H)*0.5*d2r;
  cn4 = tan(deg);
  rrih = cn2*pow((cn4/cn3),gcon);
  check  = 180.0-stdlon;
  alnfix = stdlon+check;
  alon   = grdlon+check;

  while (alon<  0.0) alon = alon+360.0;
  while (alon>360.0) alon = alon-360.0;

  deg = (alon-alnfix)*gcon*d2r;
  x =  rrih*sin(deg);
  y = -rrih*cos(deg)*H;
  *grdi = iref + (x-xih)/delx;
  *grdj = jref + (y-yih)/dely;
  /* mf 20040630 -- use ftp://grads.iges.org/grads/src/grib212.f to calc rotation factor */
  windrot=gcon*(stdlon-grdlon)*d2r;
  *wrot=windrot;
}

/* NMC eta ll to xy map  */

void ll2eg (gaint im, gaint jm, gadouble *vals,  gadouble grdlon, gadouble grdlat,
	    gadouble *grdi, gadouble *grdj, gadouble *alpha) {

/*  Subroutine to convert from lat-lon to NMC eta i,j.

    Provided by Eric Rogers NMC; 
    Converted to C 3/29/95 by Mike Fiorino
    Modified 9/2004 by J.M.Adams to correct grdi/j and alpha calculations

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
c  vals+1    tph0d: latitude of the reference center point
c  vals+2    dlam:  dlon grid increment in deg
c  vals+3    dphi:  dlat grid increment in deg
c
c            grdlat: latitude of point (grdi,grdj)
c            grdlon: longitude of point (grdi,grdj)
c            grdi:   i-coordinate(s) that this routine will generate
c                    information for
c            grdj:   j-coordinate(s) that this routine will generate
c                    information for
*/

  gadouble pi,d2r,r2d, earthr;
  gadouble tlm0d,tph0d,dlam,dphi;
  gadouble phi,lam,lam0,phi0;
  gadouble x,y,z,xx,bigphi,biglam;
  gadouble dlmd,dphd,wbd,sbd;

  pi = 3.141592654;
  d2r = pi/180.0;
  r2d = 1.0/d2r;
  earthr = 6371.2;

  tlm0d = -*(vals+0); /* convert + W to + E, the grads standard for longitude */
  tph0d =  *(vals+1);
  dlam  = (*(vals+2))*0.5;
  dphi  = (*(vals+3))*0.5;

  /* convert to radians */
  phi   =  grdlat*d2r;          /* grid latitude */
  lam   = -grdlon*d2r;          /* grid longitude, convert +W to +E, the grads standard */
  phi0  = tph0d*d2r;            /* center latitude  */
  lam0  = tlm0d*d2r;            /* center longitude */

  /* Transform grid lat/lon */
  x = cos(phi0)*cos(phi)*cos(lam-lam0)+sin(phi0)*sin(phi);
  y = -cos(phi)*sin(lam-lam0);
  z = -sin(phi0)*cos(phi)*cos(lam-lam0)+cos(phi0)*sin(phi);
  biglam = atan2(y,x)/d2r;                  /* transformed lon in degrees */
  bigphi = atan2(z,sqrt(x*x+y*y))/d2r;      /* transformed lat in degrees */

  /* Convert transformed lat/lon -> i,j */
  dlmd  = (*(vals+2));
  dphd  = (*(vals+3));
  wbd = (-1)*0.5*(im-1)*dlmd;               /* western boundary of transformed grid */
  sbd = (-1)*0.5*(jm-1)*dphd;               /* southern boundary of transformed grid */
  *grdi = 1.0 + (biglam-wbd)/dlmd;
  *grdj = 1.0 + (bigphi-sbd)/dphd;

  /* params for wind rotation alpha, alpha>0 ==> counter clockwise rotation */
  xx=sin(phi0)*sin(biglam*d2r)/cos(phi);
  if (xx < -1.0) xx = -1.0;
  else if (xx > 1.0) xx = 1.0;
  *alpha = (-1)*asin(xx);

}

void ll2pse (gaint im, gaint jm, gadouble *vals, gadouble lon, gadouble lat,
	     gadouble *grdi, gadouble *grdj) {


  /* Convert from geodetic latitude and longitude to polar stereographic
     grid coordinates.  Follows mapll by V. J. Troisi.         */
  /* Conventions include that slat and lat must be absolute values */
  /* The hemispheres are controlled by the sgn parameter */
  /* Bob Grumbine 15 April 1994. */

  const gadouble rearth = 6378.273e3;
  const gadouble eccen2 = 0.006693883;
  const gadouble pi = 3.141592654;

  gadouble cdr, alat, along, e, e2;
  gadouble t, x, y, rho, sl, tc, mc;
  gadouble slat,slon,xorig,yorig,sgn,polei,polej,dx,dy;

  slat=*(vals+0);
  slon=*(vals+1);
  polei=*(vals+2);
  polej=*(vals+3);
  dx=*(vals+4)*1000;
  dy=*(vals+5)*1000;
  sgn=*(vals+6);

  xorig = -polei*dx;
  yorig = -polej*dy;

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

    return;
  }

}

void ll2ops(gadouble *vals, gadouble lni, gadouble lti, gadouble *grdi, gadouble *grdj) {

  const gadouble radius = 6371229.0 ;

  gadouble stdlat, stdlon, xref, yref, xiref, yjref, delx , dely;
  gadouble plt,pln;
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

  /* set flag for n/s hemisphere and convert longitude to <0 ; 360> gainterval */
  if (stdlat >= 0.0) {
    hsign= 1.0 ;
  } else {
    hsign=-1.0 ;
  }

  /* set flag for n/s hemisphere and convert longitude to <0 ; 360> interval */
  glor=lni ;
  if (glor <= 0.0) glor=360.0+glor ;
  rstdlon=stdlon;
  if (rstdlon < 0.0) rstdlon=360.0+stdlon;

  /* test for a n/s pole case */
  if (stdlat == 90.0) {
    plt=lti ;
    pln=fmod(glor+270.0,360.0) ;
    goto l2000;
  }

  if (stdlat == -90.0) {
    plt=-lti ;
    pln=fmod(glor+270.0,360.0) ;
    goto l2000;
  }

  /* test for longitude on 'greenwich or date line' */
  if (glor == rstdlon) {
    if (lti > stdlat) {
      plt=90.0-lti+stdlat;
      pln=90.0;
    } else {
      plt=90.0-stdlat+lti;
      pln=270.0;;
    }
    goto l2000;
  }

  if (fmod(glor+180.0,360.0) == rstdlon) {
    plt=stdlat-90.0+lti;
    if (plt < -90.0) {
      plt=-180.0-plt;
      pln=270.0;
    } else {
      pln= 90.0;
    }
    goto l2000 ;
  }

  /* determine longitude distance relative to rstdlon so it belongs to
     the absolute interval 0 - 180 */
  argu1 = glor-rstdlon;
  if (argu1 > 180.0) argu1 = argu1-360.0;
  if (argu1 < -180.0) argu1 = argu1+360.0;

  /* 1. get the help circle bb and angle alpha (legalize arguments) */

  c2=lti*pi180 ;
  c3=argu1*pi180 ;
  arg2a = cos(c2)*cos(c3) ;
  if ( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = max1(arg2a,-c1)  */
  if (  c1 < arg2a ) arg2a = c1 ; /* min1(arg2a, c1)         */
  bb = acos(arg2a) ;

  c4=hsign*lti*pi180 ;
  arg2a = sin(c4)/sin(bb) ;
  if ( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if (  c1 < arg2a ) arg2a = c1  ; /* arg2a = dmin1(arg2a, c1) */
  alpha = asin(arg2a) ;

  /* 2. get plt and pln (still legalizing arguments) */
  c5=stdlat*pi180 ;
  c6=hsign*stdlat*pi180 ;
  arg2a = cos(c5)*cos(bb) + sin(c6)*sin(c4) ;
  if ( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if (  c1 < arg2a ) arg2a = c1  ; /* arg2a = dmin1(arg2a, c1) */
  plt1   = asin(arg2a) ;

  arg2a = sin(bb)*cos(alpha)/cos(plt1) ;

  if ( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if (  c1 < arg2a ) arg2a =  c1 ; /* arg2a = dmin1(arg2a, c1) */
  pln1   = asin(arg2a) ;


  /* test for passage of the 90 degree longitude (duallity in pln)
     get plt for which pln=90 when lti is the latitude */
  arg2a = sin(c4)/sin(c6) ;
  if ( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if (  c1 < arg2a ) arg2a =  c1 ; /* arg2a = dmin1(arg2a, c1) */
  plt90 = asin(arg2a) ;

  /* get help arc bb and angle alpha */
  arg2a = cos(c5)*sin(plt90) ;
  if ( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if (  c1 < arg2a ) arg2a =  c1 ; /* arg2a = dmin1(arg2a, c1) */
  bb    = acos(arg2a) ;

  arg2a = sin(c4)/sin(bb) ;
  if ( -c1 > arg2a ) arg2a = -c1 ; /* arg2a = dmax1(arg2a,-c1) */
  if (  c1 < arg2a ) arg2a =  c1 ; /* arg2a = dmin1(arg2a, c1) */
  alpha = asin(arg2a) ;

  /* get glolim - it is nesc. to test for the existence of solution */
  argu2  = cos(c2)*cos(bb) / (1.-sin(c4)*sin(bb)*sin(alpha)) ;
  if ( fabs(argu2) > c1 ) {
    glolim = 999.0;
  } else {
    glolim = acos(argu2)/pi180;
  }

  /* modify (if nesc.) the pln solution */
  if ( ( fabs(argu1) > glolim && lti <= stdlat ) || ( lti > stdlat ) ) {
    pln1 = pi180*180.0 - pln1;
  }

  /* the solution is symmetric so the direction must be if'ed */
  if (argu1 < 0.0) {
    pln1 = -pln1;
  }

  /* convert the radians to degrees */
  plt = plt1/pi180 ;
  pln = pln1/pi180 ;

  /* to obtain a rotated value (ie so x-axis in pol.ste. points east) 
     add 270 to longitude */
  pln=fmod(pln+270.0,360.0) ;

 l2000:

/*
c     this program convert polar stereographic coordinates to x,y ditto
c     longitude:   0 - 360  ; positive to the east
c     latitude : -90 -  90  ; positive for northern hemisphere
c     it is assumed that the x-axis point towards the east and
c     corresponds to longitude = 0
c
c     tsp 20/06-89
c
c     constants and functions
*/
  facpla = radius*2.0/(1.0+sin(plt*pi180))*cos(plt*pi180);
  x = facpla*cos(pln*pi180) ;
  y = facpla*sin(pln*pi180)  ;

  *grdi=(x-xref)/delx + xiref;
  *grdj=(y-yref)/dely + yjref;

  return;

}


/* Projection definition for rotated lat/lon
 *
 * The transformation is done as described in the 
 * COSMO documentation, Part 1, chapter 3.3.
 * http://www.cosmo-model.org/public/documentation.htm
 */

void ll2rotll( gadouble *vals, gadouble grdlat, gadouble grdlon,
		        gadouble *grdi, gadouble *grdj,  gadouble *wrot ) {

  const gadouble pi = 4.0*atan( 1.0 );
  gadouble lon_pole;      /* longitude of the pole in radiants */
  gadouble lat_pole;      /* latitude of the pole in radiants */
  gadouble dlon;          /* longitude increment in radiants */
  gadouble dlat;          /* latitude increment in radiants */
  gadouble lon_ll_corner; /* longitude of the lower left corner in radiants */
  gadouble lat_ll_corner; /* latitude of the lower left corner in radiants */
  gadouble lon_rotated;   /* rotated longitude in radiants */
  gadouble lat_rotated;   /* rotated latitude in radiants */
  gadouble lon_RW;        /* real world longitude in radiants */
  gadouble lat_RW;        /* real world latitude in radiants */

  /* grab projection parameters from the pdef line */
  lon_pole      = *(vals+0)/180.0*pi;
  lat_pole      = *(vals+1)/180.0*pi;
  dlon          = *(vals+2)/180.0*pi;
  dlat          = *(vals+3)/180.0*pi;
  lon_ll_corner = *(vals+4)/180.0*pi;
  lat_ll_corner = *(vals+5)/180.0*pi;

  lat_RW = grdlat/180*pi;
  lon_RW = grdlon/180*pi;

  /* calculate rotated longitude and latitude */
  lat_rotated = asin(
                   sin( lat_RW )*sin( lat_pole )
                 + cos( lat_RW )*cos( lat_pole )
		  *cos( lon_RW - lon_pole )
               );
  lon_rotated = atan(
                   cos( lat_RW )*sin( lon_RW - lon_pole )
		/( cos( lat_RW )*sin( lat_pole )
		  *cos( lon_RW - lon_pole )
                 - sin( lat_RW )*cos( lat_pole ) )
               );

  /* calculate grid point number */
  *grdj = ( lat_rotated - lat_ll_corner )/dlat + 1;
  *grdi = ( lon_rotated - lon_ll_corner )/dlon + 1;

  /* calculate wind rotation angle */
  *wrot = -atan(
                cos( lat_pole )*sin( lon_pole - lon_RW )
                /(   cos( lat_RW )*sin( lat_pole )
                   - sin( lat_RW )*cos( lat_pole )*cos( lon_pole - lon_RW )
                )
              );
}

