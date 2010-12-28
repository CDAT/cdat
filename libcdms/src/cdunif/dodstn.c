/*  Copyright (C) 1988-2010 by Brian Doty and the 
    Institute of Global Environment and Society (IGES).  
    See file COPYRIGHT for more information.   */

/* Authored by Joe Wielgosz 
 * 
 * dodstn.c: interface to gadap library, for reading remote station data
 *
 * to do:
 * 
 *  - as in BUFR datafiles, any given coordinate may occur in
 *  either header or profile of DODS data. for example, in an EPIC
 *  time series, lat/lon/lev are in the header, time is in the
 *  profile. the loop that builds garpt structures needs to handle
 *  this.
 *
 *  - queries to the EPIC system do not send the time constraints;
 *  a function needs to be written to convert floating point grid
 *  time into epic integer-format absolute time, and print that
 *  out into the constraint string.
 *  
 *  the following features will reduce unnecessary use of server resorces, 
 *  by making it quicker and easier to figure out where the data is 
 *  located in a station dataset:
 * 
 *  - ideally there should be a way to request just coordinate data,
 *  without a data variable. 
 *
 * - dappfi should check for some kind of metadata fields that set
 *   lat/lon/lev to reasonable values if present. 
 *
 * - one could even go further, and have attributes for average
 *   number of stations, average profile length etc. in order to
 *   get a sense of how many data points are in the dataset.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"

/* If autoconfed, only include malloc.h when it's presen */
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#else /* undef HAVE_CONFIG_H */

#include <malloc.h>

#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "grads.h"
#include "gadap.h"

/* returns dimension index (0,1,2,3 or -1 for no match) associated
   with the varid, by searching the dapinf structure */
gaint doddim(gaint varid, struct gafile *pfi) {
  gaint i;
  for (i=0; i<5; i++) {
    if (varid == pfi->dapinf[i]) return i;
  }
  return -1;
}

/* gets the index of variable named either name1 or if not found, name2 */
gaint dodgvar(char *name1, char *name2, struct gafile *pfi) {
  gaint var = gadap_d_varindex(pfi->dhandle, name1);
  if (var < 0) var = gadap_d_varindex(pfi->dhandle, name2);
  if (var < 0) var = -999;
  return var;
}

/* True if this is an EPIC data set (has _id variable. Need better way)*/
gaint isepic(GADAP_DATASET handle) {
  return gadap_d_varindex(handle, "_id") >= 0;
}

/* Convert EPIC time format, which is a 64-bit double, into a dt structure */
struct dt epict2dt(gadouble);
struct dt epict2dt(gadouble val) {
  gadouble tim;
  long ltim;
  struct dt dtbase,dtval;
  
  dtbase.yr = 1970;
  dtbase.mo = 1;
  dtbase.dy = 1;
  dtbase.hr = 0;
  dtbase.mn = 0;
  
  tim = val;                        /* millisecs since 01-01-1970 */
  tim = (tim / 60 / 1000);          /* minutes since 01-01-1970 */
  ltim = tim;
  if (ltim < 0) ltim = (-1)*ltim;   /* absolute value of minutes since 01-01-1970 */

  dtval.yr = 0;
  dtval.mo = 0;
  dtval.mn = (ltim % 60);
  dtval.hr = (ltim / 60) % 24;
  dtval.dy = (ltim / 60 / 24);
  if (val >= 0.0) {
    timadd(&dtbase, &dtval);
  }
  else {
    timsub(&dtbase, &dtval);
  }
  return dtval;
}

/* handle EPIC time format, which is a 64-bit double */
gadouble epict2gr(gadouble val, struct gafile *pfi) {
  gadouble tim;
  long ltim;
  struct dt dtbase, dtval;
  
  dtbase.yr = 1970;
  dtbase.mo = 1;
  dtbase.dy = 1;
  dtbase.hr = 0;
  dtbase.mn = 0;
  
  tim = val; /* millisecs since 01-01-1970 */
  tim = (tim / 60 / 1000); /* minutes since 01-01-1970 */
  ltim = tim;
  
  dtval.yr = 0;
  dtval.mo = 0;
  dtval.mn = (ltim % 60);
  dtval.hr = (ltim / 60) % 24;
  dtval.dy = (ltim / 60 / 24);
  timadd(&dtbase, &dtval);

  return t2gr(pfi->abvals[3], &dtval);
  /* change this function to return a dtval structure instead of a grads grid value 
     so that this can be used when there is no grads grid structure defined */
}


/* builds URL for sending a query to epic, which uses generic DODS
 * constraint clauses (i.e. '&varname>value') instead of GDS functions
 * bounds() and stid()
 */
void epicqstr(char * buf, struct gastn * stn) {

  GADAP_DATASET handle;
  struct gafile *pfi;
  const char *name;
  gaint *dapinf;
  gaint i;
  char *next, *stid;

  pfi = stn->pfi;
  handle = pfi->dhandle;
  dapinf = pfi->dapinf;
  stid = stn->stid;
  next = buf;
  if (stn->sflag) {
    name = gadap_d_varname(handle, dapinf[4]);
    snprintf(next,8191,"&%s=",name);
    next += strlen(next);
    for (i=0; i<8; i++) {
      if (stid[i] == ' ') break;
      (*next) = stid[i];
      next++;
    }
    (*next) = '\0';
  } 
  else {
 

    /* could add code here to build time constraint on request */
    /* create a gradstime2epic time routine have to give epic server
       time constaints in its own units: msec since jan1970  */

    name = gadap_d_varname(handle, dapinf[0]);
    snprintf(next,8191,"&%s>=%f&%s<=%f", name, stn->dmin[0], name, stn->dmax[0]);
    next += strlen(next);

    name = gadap_d_varname(handle, dapinf[1]);
    snprintf(next,8191,"&%s>=%f&%s<=%f", name, stn->dmin[1], name, stn->dmax[1]);
    next += strlen(next);

  }

  if (stn->pvar->levels) {
    name = gadap_d_varname(handle, dapinf[2]);
    snprintf(next,8191,"&%s>=%f&%s<=%f", name, stn->dmax[2], name, stn->dmin[2]);
    next += strlen(next);
  }
}

/*  Open an OPeNDAP station data set and fill in the gafile
    info from metadata from the server */

gaint dappfi (char *url, struct gafile *pfi) {
  struct gavar *pvar;
  struct dt tdef,dt1,dt2;
  gaint i,j,isvert,nvars,nivars,lcnt,len,gotfill,tvar,tminid,tsizeid,tstepid,trngid;
  size_t sz;
  const char *name, *longname;
  const char *trngstr;
  GADAP_DATASET handle;
  GADAP_STATUS rc;
  gadouble v1=0,v2=0,*vals;
  gadouble fill,tmin,tmax;
  char *pos;

  rc = gadap_open(url, &handle);
  if (rc!=0) {
    gaprnt (0,"Open Error on OPeNDAP URL\n");
    return (99);
  }

  nvars = gadap_d_numvars(handle);
  if (nvars<1) {
    gaprnt (0,"Open error: OPeNDAP URL is not a station dataset\n");
    gadap_close(handle,1);
    return(99);
  }

  /* save handle, url, and dataset title */
  pfi->type = 2;
  pfi->dhandle = (gaint)handle; 
  len = 0;
  while (*(url+len) && len<4095) {
    pfi->name[len] = *(url+len);
    pfi->dnam[len] = *(url+len);
    len++;
  }
  pfi->name[len] = '\0';  
  pfi->dnam[len] = '\0';
  name = gadap_d_title(handle);
  if (name) {
    len = 0;
    while (*(name+len) && len<510) {
      pfi->title[len] = *(name+len);
      len++;
    }
    pfi->title[len] = '\0';
  } 
  else {
    /* empty string for title by default */
    pfi->title[0] = '\0';
  }
  
  /* search for coordinate variables */
  pfi->dapinf[0] = dodgvar("lon", "longitude", pfi);
  pfi->dapinf[1] = dodgvar("lat", "latitude", pfi);
  pfi->dapinf[2] = dodgvar("lev", "depth", pfi);
  pfi->dapinf[3] = dodgvar("time", "time", pfi);
  pfi->dapinf[4] = dodgvar("stid", "_id", pfi);
  tvar = pfi->dapinf[3];

  /* search for data variables */
  nivars = gadap_d_numlivars(handle);
  sz = nvars * (sizeof(struct gavar) + 7 );
  pvar = (struct gavar *)galloc(sz,"dapstnpvar");
  if (pvar==NULL) {
    gaprnt (0,"Memory allocation error in dappfi\n");
    gadap_close(handle,1);
    return(99);
  }
  pfi->pvar1 = pvar;
  lcnt = 0;
  gotfill = 0;
  /* set default values for file-wide undef and ulow/uhi */
  pfi->undef =  -9.99e8; 
  pfi->ulow = fabs(pfi->undef/EPSILON);
  pfi->uhi  = pfi->undef + pfi->ulow;
  pfi->ulow = pfi->undef - pfi->ulow;
  
  for (i=0; i<nvars; i++) {
    isvert = 0;
    if (i>=nivars) isvert = 1;
    name = longname = NULL;
    name = gadap_d_varname(handle,i);
    longname = gadap_d_attrstr(handle, i, gadap_d_attrindex(handle, i, "long_name"));
    if (!longname) longname = name;
    if (doddim(i, pfi) == -1) {
      if (!gotfill) {
	if (gadap_d_fill(handle,i,&fill) == GADAP_SUCCESS)  {
	  gotfill = 1;
	  /* Use the first missing value found in file (1st variable) as file-wide 
	     undef, provided that it is not a NaN. If it is, keep the default value. */
	  if (!isnan(fill)) {
	    pfi->undef = fill;
	    pfi->ulow = fabs(pfi->undef/EPSILON);
	    pfi->uhi  = pfi->undef + pfi->ulow;
	    pfi->ulow = pfi->undef - pfi->ulow;
	  } 
	}
      }
      pvar->offset = i;
      for (j=0;j<16;j++) pvar->units[j] = -999;
      pvar->units[0] = 99;
      pvar->levels = isvert;
      len = 0;
      while (*(name+len) && len < 16) {
        pvar->abbrv[len] = tolower(*(name+len));
        len++;
      }
      pvar->abbrv[len] = '\0';
      len = 0;
      while (*(longname+len) && len < 128) {
        pvar->varnm[len] = (*(longname+len));
        len++;
      }
      pvar->varnm[len] = '\0'; 
      pvar++;
      lcnt++;
    }
  }
  pfi->vnum = lcnt;
  pfi->ivnum = nivars-4; 
  pfi->lvnum = lcnt - pfi->ivnum;
  if (pfi->lvnum>0 && pfi->dapinf[2]<0) goto leverr;

  /* Parse tdef info provided by server */

  tminid  = gadap_d_attrindex(handle,tvar,"grads_size");
  tsizeid = gadap_d_attrindex(handle,tvar,"grads_min");
  tstepid = gadap_d_attrindex(handle,tvar,"grads_step");

  if ((tminid >= 0) && (tsizeid >= 0) && (tstepid >= 0)) {
    name = gadap_d_attrstr(handle,tvar,tminid);
    if ( (pos = intprs((char *)name,&(pfi->dnum[3])))==NULL) goto tdeferr;
    
    name = gadap_d_attrstr(handle,tvar,tsizeid);
    tdef.yr = -1000;
    tdef.mo = -1000;
    tdef.dy = -1000;
    if ( (pos = adtprs((char *)name,&tdef,&dt1))==NULL) goto tdeferr;
    if (dt1.yr == -1000 || dt1.mo == -1000.0 || dt1.dy == -1000) goto tdeferr;
    
    name = gadap_d_attrstr(handle,tvar,tstepid);
    if ((pos = rdtprs((char *)name,&dt2))==NULL) goto tdeferr;
    v1 = (gadouble)((dt2.yr * 12) + dt2.mo);
    v2 = (gadouble)((dt2.dy * 1440) + (dt2.hr * 60) + dt2.mn);
    if (dequal(v1,0.0,1e-08)==0 && dequal(v2,0.0,1e-08)==0) goto tdeferr;

  } 
  /* could add an else-if statment here to get attributes from epic
     server and populate time metadata */
  else if (isepic(handle)) {
    trngid = gadap_d_attrindex(handle,nvars,"time_range");
    trngstr = gadap_d_attrstr(handle,nvars,trngid);
    tmin = strtod(trngstr,&trngstr);
    tmax = strtod(trngstr,NULL);
    dt1 = epict2dt(tmax);
    dt1 = epict2dt(tmin);
  }
  else {
    /* If no tdef info, use default time grid - daily, starting at UNIX epoch */
    dt1.yr = 1970;
    dt1.mo = 1;
    dt1.dy = 1;
    dt1.hr = 0;
    dt1.mn = 0;
    v1 = 0;
    v2 = 1440;
  }
    
  /* The info we just collected gets hung off the pfi block
     as the time conversion constants */

  sz = sizeof(gadouble)*8;
  vals = (gadouble *)galloc(sz,"dapstnvals");
  if (vals==NULL) goto tdeferr;
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

  return (0);

tdeferr:
  gaprnt (0,"Invalid tdef info from server; error in dappfi\n");
  gadap_close(handle,1);
  return(99);
leverr:
  gaprnt (0,"Invalid lev info from server; error in dappfi\n");
  gadap_close(handle,1);
  return(99);
}



/* Obtain data to satisfy the request described in the gastn block  */
gaint dapget(struct gastn *stn) {
  struct gafile *pfi;
  struct gavar *pvar;
  struct garpt *rpt;
  struct dt dt;
  gadouble ulow,uhi;
  gaint *dapinf;
  gaint rptinfo[5];
  GADAP_DATASET handle;
  GADAP_STATUS rc;
  GADAP_STN_QUERY *query;
  GADAP_RPTCOL r_handle;
  gadouble lon,lat,lev,val,time;
  gaint nreps,nlevs,i,j,k,num, rptdatavar, gotepic;
  char tchmn[20],tchmx[20],stid[10];
  const char *stid2=NULL, *varnm;
  char extra[8192];

  pfi = stn->pfi;
  handle = pfi->dhandle;
  dapinf = pfi->dapinf;
  query = gadap_sq_new(handle);
  if (query==NULL) { 
    gaprnt (0,"Memory allocation error in dapget\n");
    return (99);
  }

  /* select variables to request */

  pvar = stn->pvar;
  for (i=0; i<5; i++) {
    if (i == 2 && pvar->levels == 0) continue;
    query->varflags[dapinf[i]] = 1;
  }
  query->varflags[pvar->offset] = 1;

  /* set query constraints */

  if (isepic(handle)) {
    gotepic = 1;
    epicqstr(extra, stn);
    query->extra = extra;
    query->usebounds = 0;
  } 
  else {
    gotepic = 0;
    query->minlon = stn->dmin[0];
    query->maxlon = stn->dmax[0];
    query->minlat = stn->dmin[1];
    query->maxlat = stn->dmax[1];
    query->minlev = stn->dmin[2];
    query->maxlev = stn->dmax[2];
    gr2t (stn->tvals, stn->tmin, &dt);
    gat2ch (&dt, 4, tchmn, 20);  
    query->mintime = tchmn;
    gr2t (stn->tvals, stn->tmax, &dt);
    gat2ch (&dt, 4, tchmx, 20);
    query->maxtime = tchmx;
    if (stn->sflag) {
      for (i=0; i<8; i++) stid[i] = stn->stid[i];
      i = 0;
      while (stid[i]!=' ' && i<8) i++;
      stid[i] = '\0';
      query->stid = stid;
    } else {
      query->stid = NULL;
    }
    query->usebounds = 1;
  }

  gaprnt(2, "gadap: requesting ");
  gaprnt(2, (char*)gadap_sq_url(query));
  gaprnt(2, "\n");

  rc = gadap_sq_send(query, &r_handle);
  if (rc) {
    gaprnt (0,"OPeNDAP data retrieval error\n");
    gadap_sq_free(query);
    return(99);
  }

  /* indices of coordinate variables in report will differ from
     indices in original dataset, since some vars are missing */
  for (i=0; i<5; i++) {
    if (i == 2 && pvar->levels == 0) continue;
    varnm = gadap_d_varname(handle, dapinf[i]);
    rptinfo[i] = gadap_r_varindex(r_handle, varnm);
  }
  varnm = gadap_d_varname(handle, pvar->offset);
  rptdatavar = gadap_r_varindex(r_handle, varnm);
  nreps = gadap_r_numrpts(r_handle);

  /* set uhi and ulow for fuzzy undef test */
  if (pfi->undef==0.0) {   
    ulow = 1e-5; 
  } 
  else {
    ulow = fabs(pfi->undef/EPSILON);   
  }
  uhi  = pfi->undef + ulow;
  ulow = pfi->undef - ulow;

  num = 0;
  for (i = 0; i < nreps; i++) {

    /* get "header" info (lat/lon/stid/time coordinates) */
    gadap_r_valdbl(r_handle, i, 0, rptinfo[0], 0, &lon); 
    if (isnan(lon)) continue;
    gadap_r_valdbl(r_handle, i, 0, rptinfo[1], 0, &lat); 
    if (isnan(lat)) continue;

    if (gotepic) {
      gadap_r_valdbl(r_handle, i, 0, rptinfo[4], 0, &val); 
      if (isnan(val)) continue;
      snprintf(stid,8,"%d", (gaint)val);
      
      gadap_r_valdbl(r_handle, i, 0, rptinfo[3], 0, &val); 
      if (isnan(val)) continue;
      time = epict2gr(val, pfi);

    } else {
      stid2 = gadap_r_valstr(r_handle, i, 0, rptinfo[4], 0);
      for (j = 0; j < 8; j++) {
	if (stid2[j] == '\0') break;
	stid[j] = stid2[j];
      }
      if (stid2) free(stid2);
      gadap_r_valdbl(r_handle, i, 0, rptinfo[3], 0, &time); 
    }

    /* pad station id with spaces */
    for (j = 0; j < 8; j++) {
      if (stid[j] == '\0') break;
    }
    while (j < 8) {
      stid[j] = ' ';
      j++;
    }
    stid[8] = '\0';

    /* get surface value or vertical profile */
    if (pvar->levels==0) {
      gadap_r_valdbl(r_handle, i, 0, rptdatavar, 0, &val);
      if (isnan(val) || isinf(val)) val=pfi->undef;
      rpt = gaarpt(stn);
      for (k=0; k<8; k++) rpt->stid[k] = *(stid+k);  
      rpt->lon = lon;
      rpt->lat = lat;
      rpt->tim = time;
      rpt->lev = -9.99e8;
      /* Test if value is within EPSILON of the missing data value.
	 If yes, set undef mask to 0. The undef mask is 1 for valid data */
      if (val >= ulow && val <= uhi) {
	rpt->umask = 0;
	rpt->val = pfi->undef;   
      }
      else {
	rpt->umask = 1;
	rpt->val = val;
      }
      num++;
    } else {
      nlevs = gadap_r_numlev(r_handle, i);
      for (j=0; j<nlevs; j++) {
	/* get the level and the data value */
	gadap_r_valdbl(r_handle, i, j, rptinfo[2], 0, &lev);
	if (isnan(lev) || isinf(lev))  continue;
        gadap_r_valdbl(r_handle, i, j, rptdatavar, 0, &val);
	if (isnan(val) || isinf(val)) val=pfi->undef;
        rpt = gaarpt(stn);
        for (k=0; k<8; k++) rpt->stid[k] = *(stid+k);  
        rpt->lon = lon;
        rpt->lat = lat;
        rpt->tim = time;
        rpt->lev = lev;
	/* Test if value is within EPSILON of the missing data value.
	   If yes, set undef mask to 0. The undef mask is 1 for valid data */
	if (val >= ulow && val <= uhi) {
	  rpt->umask = 0;
	  rpt->val = pfi->undef;   
	}
	else {
	  rpt->umask = 1;
	  rpt->val = val;
	}
        num++;
      }
    }
  }
  stn->rnum = num;
  gadap_r_free(r_handle);
  gadap_sq_free(query);
  return (0);
}

/*  Close gadap data set */

void dapclo (struct gafile *pfi) {
GADAP_DATASET handle;

  if (pfi->dhandle == -999) return;
  handle = pfi->dhandle;
  gadap_close(handle,1);
  pfi->dhandle = -999;
}
