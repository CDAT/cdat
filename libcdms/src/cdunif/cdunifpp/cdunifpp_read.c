/*
 *
 *    Copyright (C) 2004-2006 NERC DataGrid
 *    This software may be distributed under the terms of the
 *    CCLRC Licence for CCLRC Software
 * <CDATDIR>/External_License/CCLRC_CDAT_License.txt 
 *
 */
#ifdef HAVE_PP
#include "cdunifpp.h"

#define BREAKATFIRSTINVALID 1

/*---------------------------------------------------------
 * reads n words from ppfile, storing them at ptr
 *
 * Number of bytes read from file is n * (file wordsize).
 *
 * In the case where (file wordsize) != (compiled-in wordsize),
 * conversion may be done if "conv" is set to "convert_int" or
 * "convert_real", in which case bytes written at supplied pointer
 * will be n * (compiled-in wordsize).  If "conv" is set to 
 * "convert_none" then number of bytes written to pointer will be
 * number of bytes read.
 *
 * (Conversion simply means copying values between variables of different
 * word length.  convert_none is for use with packed data, to be unpacked
 * elsewhere.)
 *
 * returns number of file words read (i.e. n, unless there's a short read)
 */

size_t pp_read_words(void *ptr, size_t n, PPconvert conv, const PPfile *ppfile)
{
  size_t i,nread,nread1;
  void *tmp;

  Fint8 dummy; /* a variable of longest word length which might be used - we point ptr at it,
		* and use it as a place to store a variable of that length or shorter
		*/

  CKP(ppfile);
  CKP(ptr);
  
  if (ppfile->wordsize == wordsize || conv==convert_none) {
    nread = fread(ptr, ppfile->wordsize, n, ppfile->fh);

    /* NOTE: for 64-bit file packed with the CRAY32 method, the following call to swapbytes will 
     * transpose pairs of 32-bit data values.  It is responsibility of calling routine to deal
     * with this.
     */
    if (ppfile->swap)
      pp_swapbytes(ptr,ppfile->wordsize,nread);
  }
  else {

    tmp = &dummy;

    nread=0;

    /* read and convert a word at a time - save allocating extra memory */
    for (i=0; i<n; i++) {

      nread1 = fread(tmp, ppfile->wordsize, 1, ppfile->fh);
      if (nread1==0)
	break;

      nread += nread1;

      if (ppfile->swap)
	pp_swapbytes(tmp, ppfile->wordsize, nread1);

      /* gruesome switches on constants in the loop - hoping the optimiser will sort it out -
       * I'd rather not explicitly code copies of the loop inside the switch */

      switch(ppfile->wordsize) {
      case 4:
	switch(conv) {
	case convert_int:  ((Fint *)ptr)[i]  = *(Fint4 *)tmp;   break;
	case convert_real: ((Freal *)ptr)[i] = *(Freal4 *)tmp;	break;
	default: pp_switch_bug("pp_read_words"); ERR;
	}
	break;
      case 8:
	switch(conv) {
	case convert_int:  ((Fint *)ptr)[i]  = *(Fint8 *)tmp;  break;
	case convert_real: ((Freal *)ptr)[i] = *(Freal8 *)tmp; break;
	default: pp_switch_bug("pp_read_words"); ERR;
	}
	break;
      default: pp_switch_bug("pp_read_words"); ERR;
      }      
    }
  }
  return nread;

  ERRBLK("pp_read_words",0);
}

/*---------------------------------------------------------*/

int pp_swapbytes(void *ptr, int bytes, int nchunk)
{
  int i;
  char *p;
  char t;
  p = (char*) ptr;
  /* just deal with the 2 realistic cases; faster than
   * doing the general case with two sliding pointers
   */
  switch(bytes){
  case 4:
    for(i=0; i<nchunk; i++){
      t=p[3]; p[3]=p[0]; p[0]=t;
      t=p[2]; p[2]=p[1]; p[1]=t;
      p+=4;
    }
    break;
  case 8:
    for(i=0; i<nchunk; i++){
      t=p[7]; p[7]=p[0]; p[0]=t;
      t=p[6]; p[6]=p[1]; p[1]=t;
      t=p[5]; p[5]=p[2]; p[2]=t;
      t=p[4]; p[4]=p[3]; p[3]=t;	
      p+=8;
    }
    break;
  default:
    pp_switch_bug("pp_swap_bytes"); ERR;
  }
  return 0;

  ERRBLKI("pp_swapbytes");
}

/*---------------------------------------------------------*/



/*---------------------------------------------------------*/

/* pp_read_data_record:
 * reads in data for a record and does any necessary unpacking
 * (returns a pointer which should be freed with pp_free)
 */

void *pp_read_data_record(const PPrec *rec, const PPfile *ppfile, PPlist *heaplist)
{
  size_t bytes, packed_bytes, nread;
  FILE *fh;
  PPconvert conv;
  void *data, *data_expanded, *packed_data;
  CuType vartype;
  const PPhdr *hdrp;
  int pack;
  int nint;
  int ipt, npoint, npoint_used;
  Fint valid_landmask_value, *landmask_vals;
  Freal mdi;

  const void *fill_ptr;
  const char *srcptr;  /* points to compressed data; this could be of type Fint or Freal;
			* use char* instead of void* to allow ptr arithmetic
			*/
  char *destptr; /* uncompressed data */

  CKP(ppfile);
  CKP(rec);
  CKP(heaplist);
  fh = ppfile->fh;

  fseek(fh, rec->datapos, SEEK_SET);
  hdrp=&rec->hdr;

  pack = pp_get_var_packing(hdrp);
  vartype=pp_get_var_type(hdrp);

  if (pack==0) {
   /* unpacked data -- read and convert according to type */
    
    bytes = rec->datalen * wordsize;
    CKP(   data = pp_malloc(bytes,heaplist)   );
    
    if (vartype==inttype)
      conv = convert_int;
    else if (vartype==realtype)
      conv = convert_real;
    else {
      conv=convert_none;
      pp_switch_bug("pp_read_data_record"); ERR;
    }
    nread = pp_read_words(data, rec->datalen, conv, ppfile);
    ERRIF(nread != rec->datalen);
  }
  else {

    /* PACKING IN USE */

    /* first allocate array and read in packed data */

    packed_bytes = rec->disklen * ppfile->wordsize;
    CKP(   packed_data = pp_malloc(packed_bytes,heaplist)   );

    nread = pp_read_words(packed_data, rec->disklen, convert_none, ppfile);
    ERRIF(nread != rec->disklen);

    /* and allocate array for unpacked data*/
    bytes = rec->datalen * wordsize;
    CKP(   data = pp_malloc(bytes,heaplist)   );

    /* NOW UNPACK ACCORDING TO PACKING TYPE: */

    switch(pack) {

    case 1:

      /* WGDOS */

      /* for this case we will use unwgdos routine */

      /* unwgdos routine wants to know number of native integers in input.
       * input type might not be native int, so calculate:
       */
      nint = rec->disklen * ppfile->wordsize / sizeof(int);
      mdi = *(Freal*)pp_get_var_fill_value(hdrp);

      CKI(   pp_unwgdos_wrap(packed_data, nint, data, rec->datalen, mdi, heaplist)   );
      
      break;
      
    
    case 2:
      /* CRAY 32-bit method */
      
      if (vartype != realtype) {
	CuError(CU_EINTERN,"Cray 32-bit unpacking supported only for REAL type data");
	ERR;
      }
      
      /* 
       * in the event of a 64-bit file (which it probably is, else 32-bit packing is 
       * redundant), and if we're on a little-endian machine, the file was written on
       * a cray, so the 64-bit byte swapping (whether done by cdunifpp or previously)
       * will have had the side-effect of swapping pairs of 32-bit words and we need
       * to swap them back again.
       *
       * NB  LITTLE_ENDIAN_MACHINE  is defined (if at all) in cdunifpp.h
       */
      
#ifdef LITTLE_ENDIAN_MACHINE
      if (ppfile->wordsize == 8)
	pp_swap32couplets(packed_data,packed_bytes);
#endif
      
      for (ipt=0; ipt < rec->datalen ; ipt++)
	*(((Freal*) (data)) + ipt) = *(((Freal4*) (packed_data)) + ipt);
    
      break;
    
    case 3:
      CuError(CU_EINTERN,"GRIB unpacking not supported");
      ERR;
      /* break; */

    default:
      pp_switch_bug("pp_read_data_record"); ERR;
    }

    /* Okay - data unpacked - free up packed data */
    CKI(  pp_free(packed_data,heaplist)  );
  }


  /* if land or sea mask compression, then allocate another array, and
   * copy the relevant data across, filling the gaps with missing data
   */

  if (pp_get_var_compression(hdrp) == 2) {
    
    npoint = pp_genaxis_len(ppfile->landmask->xaxis) * pp_genaxis_len(ppfile->landmask->yaxis);
    bytes = npoint * wordsize;
    CKP(   data_expanded = pp_malloc(bytes,heaplist)   );
    
    switch ((hdrp->LBPACK/100)%10) {
    case 1:
      /* land mask compression */
      valid_landmask_value = 1;
      break;
    case 2:
      /* sea mask compression */
      valid_landmask_value = 0;
      break;
    default:
      pp_switch_bug("pp_read_data_record"); ERR;
    }

    landmask_vals = ppfile->landmask->data->values;
    srcptr = data;
    destptr = data_expanded;

    CKP(   fill_ptr = pp_get_var_fill_value(hdrp)   );

    npoint_used = 0;

    for (ipt = 0; ipt < npoint; ipt++) {
      if (landmask_vals[ipt] == valid_landmask_value) {

	if (npoint_used >= rec->datalen) {
	  CuError(CU_EINTERN,"Uncompressing tried to use more compressed data than available");
	  ERR;
	}

	memcpy(destptr,srcptr,wordsize);
	srcptr += wordsize;
	npoint_used++;
      }
      else {
	memcpy(destptr,fill_ptr,wordsize);
      }
      destptr += wordsize;
    }

    if (npoint_used != rec->datalen) {
      CuError(CU_EINTERN,"Uncompressing did not use all the compressed data");
      ERR;
    }


    CKI(  pp_free(data,heaplist)  );
    data = data_expanded;
  }

  return data;

  ERRBLKP("pp_read_data_record");
}


#ifdef LITTLE_ENDIAN_MACHINE
int pp_swap32couplets(char *p,int nbytes)
{
  int i;
  char a,b,c,d,e,f,g,h;
  for (i=0; i<nbytes; i+=8) {

    a = p[i+0];
    b = p[i+1];
    c = p[i+2];
    d = p[i+3];
    e = p[i+4];
    f = p[i+5];
    g = p[i+6];
    h = p[i+7];

    p[i+0] = e;
    p[i+1] = f;
    p[i+2] = g;
    p[i+3] = h;

    p[i+4] = a;
    p[i+5] = b;
    p[i+6] = c;
    p[i+7] = d;
  }
  return 0;
}
#endif

/*---------------------------------------------------------*/

/* pp_skip_fortran_record: skips a fortran record, and returns how big it was,
 *  or -1 for end of file, or -2 for any error which may imply corrupt file
 * (return value of 0 is a legitimate empty record).
 */
int pp_skip_fortran_record(const PPfile *ppfile)
{
  Fint recsize, recsize2;
  FILE *f;

  f=ppfile->fh;  
  if(   pp_read_words(&recsize,1,convert_int,ppfile)   !=1) return -1;
  CKI(   fseek(f,recsize,SEEK_CUR)   );
  ERRIF(   pp_read_words(&recsize2,1,convert_int,ppfile)   !=1);
  ERRIF(recsize != recsize2);
  return recsize;
  
  ERRBLK("pp_skip_fortran_record",2);
}


/* skip a single word */
int pp_skip_word(const PPfile *ppfile)
{
  CKI(   fseek(ppfile->fh, ppfile->wordsize, SEEK_CUR)   );
  return 0;

  ERRBLKI("pp_skip_word");    
}

void *pp_read_header(const PPfile *ppfile, PPlist *heaplist)
{
  void *hdr;
  /* reads a PP header -- file must be positioned at start of header,
   *  (after any fortran record length integer) */
  
  CKP(   hdr=pp_malloc(n_hdr * wordsize,heaplist)   );
  ERRIF(   pp_read_words(hdr,n_int_hdr,convert_int,ppfile)   !=n_int_hdr);

  ERRIF(   pp_read_words((char *)hdr + n_int_hdr * wordsize,
		      n_real_hdr,convert_real,ppfile)   !=n_real_hdr);
  
  return hdr; /* success */

  ERRBLKP("pp_read_header");    
}

/*---------------------------------------------------------*/

/* 
 *  The pp_read_all_headers routine takes a CuFile*, and does the following:
 *
 *  count the records (nrec), and allocate an array of nrec pointers to PPrec structures,
 *     
 *     populate:  file->internp->nrec (= nrec)
 *                file->internp->recs (pointer to recs array)
 * 
 *     populate the PPrec structures themselves
 *
 *  returns 0 on success, -1 on failure
 *
 *
 * (Incidentally: why are the pointers to records stored in an array, 
 * when virtually every other list used by cdunifpp is a linked list?
 * Answer: so we can use qsort, and also so we can access them directly by index.)
 */

int pp_read_all_headers(CuFile *file)
{
  FILE *fh;
  int rec, nrec, recsize, filerec, nlrec;
  void *hdr;
  PPfile *ppfile;
  PPrec **recs,*recp;
  PPlist *heaplist;

  Fint start_lookup, nlookup1, nlookup2, lbbegin, dataset_type, start_data;
  long hdr_start, hdr_size, lbbegin_offset, datapos;

  int *valid;
  PPhdr *hdrp;  
  int fieldsfile;


  ppfile=file->internp;
  fh=ppfile->fh;
  heaplist=ppfile->heaplist;

  switch(ppfile->type) {
  case pp_type:
    
    fseek(fh,0,SEEK_SET);
    /* count the PP records in the file */
    for (nrec=0; (recsize=pp_skip_fortran_record(ppfile)) != -1; nrec++) {
      ERRIF(recsize==-2);
      if (recsize != n_hdr * ppfile->wordsize) {
	CuError(CU_EOPEN,"Opening PP file %s: unsupported header length: %d words",
		file->controlpath, recsize / ppfile->wordsize);
	ERR;
      }
      ERRIF(   pp_skip_fortran_record(ppfile)   <0); /* skip the data record */
    }
    
    /* now rewind, and read in all the PP header data */
    fseek(fh,0,SEEK_SET);
    
    ppfile->nrec=nrec;
    CKP(   recs=pp_malloc(nrec*sizeof(PPrec*),heaplist)   );
    ppfile->recs = recs;
    
    for (rec=0; rec<nrec; rec++){
      
      /* fill in the record information - for each record, read the header
       * record into heap memory, copy out of it the elements we want to store
       * then free the heap memory.
       */
      
      /* just skip the fortran integers - we've already tested header length */
      CKI(   pp_skip_word(ppfile)   );
      CKP(   hdr=pp_read_header(ppfile,heaplist)   );
      CKI(   pp_skip_word(ppfile)   );
      
      CKP(   recp=pp_malloc(sizeof(PPrec),heaplist)   );
      recs[rec]=recp;
      hdrp=&recp->hdr;
      
      pp_store_header(hdrp,hdr);

      recp->recno = rec;
      
      /* skip data record but store length */
      recp->datapos = ftell(fh) + ppfile->wordsize;
      recp->disklen = pp_skip_fortran_record(ppfile) / ppfile->wordsize;
      
      /* work out datalen */
      pp_evaluate_lengths(hdrp, ppfile, &recp->datalen, NULL);      
      
      CKI(  pp_free(hdr,heaplist)  );
    }
    break;
  case um_type:

    /* pick out certain information from the fixed length header */
    
    CKI(   fseek(fh,4*ppfile->wordsize,SEEK_SET)  );
    ERRIF(   pp_read_words(&dataset_type, 1,  convert_int, ppfile)   !=1);

    CKI(   fseek(fh,149*ppfile->wordsize,SEEK_SET)  );
    ERRIF(   pp_read_words(&start_lookup, 1,  convert_int, ppfile)   !=1);
    ERRIF(   pp_read_words(&nlookup1, 1,  convert_int, ppfile)   !=1);
    ERRIF(   pp_read_words(&nlookup2, 1,  convert_int, ppfile)   !=1);
    
    CKI(   fseek(fh,159*ppfile->wordsize,SEEK_SET)  );
    ERRIF(   pp_read_words(&start_data, 1,  convert_int, ppfile)   !=1);

    fieldsfile = (dataset_type == 3);

    /* (first dim of lookup documented as being 64 or 128, so 
     * allow header longer than n_hdr (64) -- discarding excess -- but not shorter)
     */
    if (nlookup1 < n_hdr) {
      CuError(CU_EOPEN,"Opening UM file %s: unsupported header length: %d words",
		file->controlpath, nlookup1);
      ERR;
    }

    /* count the valid records in the file */
    /* loop over all records and pick out the valid ones - test for LBBEGIN != -99 */
    nrec=0;
    hdr_start = (start_lookup - 1) * ppfile->wordsize;
    hdr_size = nlookup1 * ppfile->wordsize;
    lbbegin_offset = 28 * ppfile->wordsize;

    CKP(   valid = pp_malloc(nlookup2 * sizeof(int),heaplist)   );

    for (filerec=0; filerec<nlookup2; filerec++) {
      valid[filerec]=0;
      CKI(   fseek(fh, hdr_start + filerec * hdr_size + lbbegin_offset, SEEK_SET)   );
      ERRIF(   pp_read_words(&lbbegin, 1,  convert_int, ppfile)   !=1);

      if (lbbegin != -99) {
	/* valid record */
	valid[filerec]=1;
	nrec++;
      } else {
#ifdef BREAKATFIRSTINVALID
        break;
#endif
	valid[filerec]=0;
      }
    }

    /* now read in all the PP header data */
    ppfile->nrec=nrec;
    CKP(   recs=pp_malloc(nrec*sizeof(PPrec*),heaplist)   );
    ppfile->recs = recs;


    rec=0;  /* valid record number, as opposed to  
	     * filerec which is total record number */

    datapos = (start_data-1) * ppfile->wordsize;
#ifdef BREAKATFIRSTINVALID
      nlrec=nrec;
#else
      nlrec=nlookup2;
#endif
    for (filerec=0; filerec<nlrec ; filerec++) {
      if (valid[filerec]) {

	/* seek to correct position, read in header into tmp dynamic array,
	 * store wanted elements in record structure, free tmp array
	 */
	CKI(   fseek(fh, hdr_start + filerec*hdr_size, SEEK_SET)   );	
	CKP(   hdr=pp_read_header(ppfile,heaplist)   );
	
	CKP(   recp=pp_malloc(sizeof(PPrec),heaplist)   );
	recs[rec]=recp;
	hdrp=&recp->hdr;
      	pp_store_header(hdrp,hdr);
	CKI(  pp_free(hdr,heaplist)  );

	/* work out datalen and disklen */
	pp_evaluate_lengths(hdrp, ppfile, &recp->datalen, &recp->disklen);
	/* use LBBEGIN if it is set - this will not work if LBBEGIN refers to 
	 * start record rather than start address
	 */
	if (hdrp->LBBEGIN != 0) {
	  recp->datapos=hdrp->LBBEGIN*ppfile->wordsize; 
	} else {
	  recp->datapos = datapos;
	}
	/* If LBNREC and LBBEGIN are both non-zero and it's not a FIELDSfile,
	 *   the file has well-formed records.  In that case, 
	 *   LBBEGIN should be correct, so do an assertion
	 */
	if (!fieldsfile && hdrp->LBNREC != 0 && hdrp->LBBEGIN != 0) {
	  if (recp->datapos != hdrp->LBBEGIN * ppfile->wordsize) {
	    
	    CuError(CU_EOPEN,"start of data record mismatch: %d %d",
		    recp->datapos, hdrp->LBBEGIN * ppfile->wordsize);
	    ERR;
	  }
	}

	datapos += recp->disklen * ppfile->wordsize;
	rec++;
      }
    }
    

    CKI(  pp_free(valid,heaplist)  );

    break;
  default:
    pp_switch_bug("pp_read_all_headers");
    ERR;
  }
  
  return 0;
  
  ERRBLKI("pp_read_all_headers");
}

int pp_store_header(PPhdr *hdrp, const void *hdr){
  const Fint *ihdr;
  const Freal *rhdr;
  
  ihdr = (Fint*) hdr;
  rhdr = (Freal*) (ihdr + 45);

#ifdef PP_STORE_LBYR
  hdrp->LBYR   =ihdr[ 0];     
#endif
#ifdef PP_STORE_LBMON
  hdrp->LBMON  =ihdr[ 1];    
#endif
#ifdef PP_STORE_LBDAT
  hdrp->LBDAT  =ihdr[ 2];    
#endif
#ifdef PP_STORE_LBHR
  hdrp->LBHR   =ihdr[ 3];     
#endif
#ifdef PP_STORE_LBMIN
  hdrp->LBMIN  =ihdr[ 4];    
#endif
#ifdef PP_STORE_LBDAY
  hdrp->LBDAY  =ihdr[ 5];    
#endif
#ifdef PP_STORE_LBYRD
  hdrp->LBYRD  =ihdr[ 6];    
#endif
#ifdef PP_STORE_LBMOND
  hdrp->LBMOND =ihdr[ 7];   
#endif
#ifdef PP_STORE_LBDATD
  hdrp->LBDATD =ihdr[ 8];   
#endif
#ifdef PP_STORE_LBHRD
  hdrp->LBHRD  =ihdr[ 9];    
#endif
#ifdef PP_STORE_LBMIND
  hdrp->LBMIND =ihdr[10];   
#endif
#ifdef PP_STORE_LBDAYD
  hdrp->LBDAYD =ihdr[11];   
#endif
#ifdef PP_STORE_LBTIM
  hdrp->LBTIM  =ihdr[12];    
#endif
#ifdef PP_STORE_LBFT
    hdrp->LBFT   =ihdr[13];     
#endif
#ifdef PP_STORE_LBLREC
    hdrp->LBLREC =ihdr[14];   
#endif
#ifdef PP_STORE_LBCODE
    hdrp->LBCODE =ihdr[15];   
#endif
#ifdef PP_STORE_LBHEM
    hdrp->LBHEM  =ihdr[16];    
#endif
#ifdef PP_STORE_LBROW
    hdrp->LBROW  =ihdr[17];    
#endif
#ifdef PP_STORE_LBNPT
    hdrp->LBNPT  =ihdr[18];    
#endif
#ifdef PP_STORE_LBEXT
    hdrp->LBEXT  =ihdr[19];    
#endif
#ifdef PP_STORE_LBPACK
    hdrp->LBPACK =ihdr[20];   
#endif
#ifdef PP_STORE_LBREL
    hdrp->LBREL  =ihdr[21];    
#endif
#ifdef PP_STORE_LBFC
    hdrp->LBFC   =ihdr[22];     
#endif
#ifdef PP_STORE_LBCFC
    hdrp->LBCFC  =ihdr[23];    
#endif
#ifdef PP_STORE_LBPROC
    hdrp->LBPROC =ihdr[24];   
#endif
#ifdef PP_STORE_LBVC
    hdrp->LBVC   =ihdr[25];     
#endif
#ifdef PP_STORE_LBRVC
    hdrp->LBRVC  =ihdr[26];    
#endif
#ifdef PP_STORE_LBEXP
    hdrp->LBEXP  =ihdr[27];    
#endif
#ifdef PP_STORE_LBBEGIN
    hdrp->LBBEGIN =ihdr[28];   
#endif
#ifdef PP_STORE_LBNREC
    hdrp->LBNREC =ihdr[29];   
#endif
#ifdef PP_STORE_LBPROJ
    hdrp->LBPROJ =ihdr[30];   
#endif
#ifdef PP_STORE_LBTYP
    hdrp->LBTYP  =ihdr[31];    
#endif
#ifdef PP_STORE_LBLEV
    hdrp->LBLEV  =ihdr[32];    
#endif
#ifdef PP_STORE_LBRSVD1
    hdrp->LBRSVD1=ihdr[33];  
#endif
#ifdef PP_STORE_LBRSVD2
    hdrp->LBRSVD2=ihdr[34];  
#endif
#ifdef PP_STORE_LBRSVD3
    hdrp->LBRSVD3=ihdr[35];  
#endif
#ifdef PP_STORE_LBRSVD4
    hdrp->LBRSVD4=ihdr[36];  
#endif
#ifdef PP_STORE_LBSRCE
    hdrp->LBSRCE =ihdr[37];   
#endif
#ifdef PP_STORE_LBUSER1
    hdrp->LBUSER1=ihdr[38];  
#endif
#ifdef PP_STORE_LBUSER2
    hdrp->LBUSER2=ihdr[39];  
#endif
#ifdef PP_STORE_LBUSER3
    hdrp->LBUSER3=ihdr[40];  
#endif
#ifdef PP_STORE_LBUSER4
    hdrp->LBUSER4=ihdr[41];  
#endif
#ifdef PP_STORE_LBUSER5
    hdrp->LBUSER5=ihdr[42];  
#endif
#ifdef PP_STORE_LBUSER6
    hdrp->LBUSER6=ihdr[43];  
#endif
#ifdef PP_STORE_LBUSER7
    hdrp->LBUSER7=ihdr[44];  
#endif

#ifdef PP_STORE_BULEV
    hdrp->BULEV=rhdr[ 0];     
#endif
#ifdef PP_STORE_BHULEV
    hdrp->BHULEV=rhdr[ 1];    
#endif
#ifdef PP_STORE_BRSVD3
    hdrp->BRSVD3=rhdr[ 2];    
#endif
#ifdef PP_STORE_BRSVD4
    hdrp->BRSVD4=rhdr[ 3];     
#endif
#ifdef PP_STORE_BDATUM
    hdrp->BDATUM=rhdr[ 4];    
#endif
#ifdef PP_STORE_BACC
    hdrp->BACC  =rhdr[ 5];    
#endif
#ifdef PP_STORE_BLEV
    hdrp->BLEV  =rhdr[ 6];    
#endif
#ifdef PP_STORE_BRLEV
    hdrp->BRLEV =rhdr[ 7];   
#endif
#ifdef PP_STORE_BHLEV
    hdrp->BHLEV =rhdr[ 8];   
#endif
#ifdef PP_STORE_BHRLEV
    hdrp->BHRLEV=rhdr[ 9];    
#endif
#ifdef PP_STORE_BPLAT
    hdrp->BPLAT =rhdr[10];   
#endif
#ifdef PP_STORE_BPLON
    hdrp->BPLON =rhdr[11];   
#endif
#ifdef PP_STORE_BGOR
    hdrp->BGOR  =rhdr[12];    
#endif
#ifdef PP_STORE_BZY
    hdrp->BZY   =rhdr[13];     
#endif
#ifdef PP_STORE_BDY
    hdrp->BDY   =rhdr[14];   
#endif
#ifdef PP_STORE_BZX
    hdrp->BZX   =rhdr[15];   
#endif
#ifdef PP_STORE_BDX
    hdrp->BDX   =rhdr[16];    
#endif
#ifdef PP_STORE_BMDI
    hdrp->BMDI  =rhdr[17];    
#endif
#ifdef PP_STORE_BMKS
    hdrp->BMKS  =rhdr[18];    
#endif

  return 0;
}

/*
 * pp_evaluate_lengths works out the data length and disk length, and outputs them using the supplied
 * pointers (unless these are NULL).
 *
 * NB as currently coded, pp_read_all_headers() does not use the calculated value of disk length in
 * the case of raw PP files (using instead the Fortran record length).  But to allow for future changes,
 * this routine has a stab at calculating it anyway.
 */

int pp_evaluate_lengths (const PPhdr *hdrp, const PPfile *ppfile, long *datalenp, long *disklenp) {

  long datalen;
  long disklen;

  if (hdrp->LBPACK != 0) {
    datalen=0;
    if (hdrp->LBROW > 0 && hdrp->LBNPT>0)
      datalen += hdrp->LBROW * hdrp->LBNPT;
    if (hdrp->LBEXT > 0)
      datalen += hdrp->LBEXT;
    if (datalen==0)
      datalen = hdrp->LBLREC;

    /* Input array size (packed field):
     *   First try LBNREC
     *   then if Cray 32-bit packing, know ratio of packed to unpacked lengths;
     *   else use LBLREC
     *
     * For raw PP files, first try LBLREC if it's non-zero, because values of 
     * LBNREC written by CONVPP may be wrong (particularly if CONVPP does unpacking)
     */
    switch(ppfile->type) {
    case um_type:
      disklen = 
	(hdrp->LBNREC != 0) ? hdrp->LBNREC :
	(hdrp->LBPACK%10 ==2) ? datalen * 4 / ppfile->wordsize :
	hdrp->LBLREC;
      break;
    case pp_type:
      disklen = 
	(hdrp->LBLREC != 0) ? hdrp->LBLREC :
	(hdrp->LBNREC != 0) ? hdrp->LBNREC :
	(hdrp->LBPACK%10 ==2) ? datalen * 4 / ppfile->wordsize :
	0;
      break;
    default:
      pp_switch_bug("pp_evaluate_lengths"); ERR;
    }
  }
  else {
    disklen=0; /* init to avoid compiler warnings */

    /* unpacked record */
    datalen = hdrp->LBLREC;

    /* UM file: try LBNREC first
     *
     * PP file: try LBLREC first, because convpp copies LBNREC direct from UM file
     *                            without setting it to value appropriate to PP file
     */
    switch(ppfile->type) {
    case pp_type:     
      disklen = (hdrp->LBLREC != 0) ? hdrp->LBLREC : hdrp->LBNREC;
      break;
    case um_type:
      disklen = (hdrp->LBNREC != 0) ? hdrp->LBNREC : hdrp->LBLREC;
      break;
    default:
      pp_switch_bug("pp_evaluate_lengths");  ERR;
    }
  }

  if (datalenp != NULL)
    *datalenp = datalen;

  if (disklenp != NULL)
    *disklenp = disklen;

  return 0;

  ERRBLKI("pp_evaluate_lengths");
}

PPdata *pp_read_extradata(const PPrec *rec, const PPfile *ppfile, PPlist *heaplist, const PPextravec extra) {

  const PPhdr *hdrp;
  FILE *fh;
  int pack, nread;
  long pos, epos;
  Fint ic, ia, ib;

  PPdata *data;

  CKP(ppfile);
  CKP(rec);
  CKP(heaplist);

  data=NULL;
  hdrp=&(rec->hdr);
  fh = ppfile->fh;
  pack = pp_get_var_packing(hdrp);

  if (pack==0) {
 
    pos=rec->datapos+(hdrp->LBROW*hdrp->LBNPT)*ppfile->wordsize;
    epos=pos+hdrp->LBEXT*ppfile->wordsize;
    
    fseek(fh, pos, SEEK_SET);
    ia=1;

    while (pos < epos && ia > 0) {
      fseek(fh, pos, SEEK_SET);
      nread=pp_read_words(&ic, 1, convert_int, ppfile);
      ERRIF(nread != 1);
      ia=ic/1000;
      ib=ic-ia*1000;
      if (ib == extra) {
	CKP(data=pp_data_new(realtype,ia,heaplist));
	if ((nread=pp_read_words(data->values,ia,convert_real, ppfile)) !=ia ) ERR;
	break;
      }
      pos+=ia;
    }
  } else {
    pp_error_mesg("pp_read_extra_dat","only do unpacked exta data");
    data=NULL;
  }
  return data;
    
  ERRBLKP("pp_read_extra_data");
}

int pp_extra_has_vector(const PPextravec extra, const PPrec *rec, const PPfile *ppfile) {
  /* JAK comment ERROR returns could be better ?*/
  const PPhdr *hdrp;
  FILE *fh;
  int pack;
  int nread;
  long pos, epos;
  char *errmess;
  Fint ic, ia, ib;

  errmess="";
  CKP(ppfile);
  CKP(rec);
  hdrp=&(rec->hdr);
  fh = ppfile->fh;
  pack = pp_get_var_packing(hdrp);

  if (pack==0) {

    pos=rec->datapos+(hdrp->LBROW*hdrp->LBNPT)*ppfile->wordsize;
    epos=pos+hdrp->LBEXT*ppfile->wordsize;
    ia=1;
    while (pos < epos && ia > 0) {
      fseek(fh, pos, SEEK_SET);
      if ((nread=pp_read_words(&ic, 1, convert_int, ppfile)) != 1) ERR;
      ia=ic/1000;
      ib=ic-ia*1000;
      if (ib == extra) {
	switch (extra) {
	case extra_x:
	  if (ia != hdrp->LBNPT) {
	    errmess="mismatch between pp header and extra data sections. Corrupt input file?";
	    ERR;
	  }
	  break;
	case extra_y:
	  if (ia != hdrp->LBROW) {
	    errmess="mismatch between pp header and extra data sections. Corrupt input file?";
	    ERR;
	  }
	  break;
	case extra_title:
	  ;
	default:
	  pp_switch_bug("pp_has_extra_vector");
	}
	return 1; /* found the axis we are after */
      }
      pos+=ia;
    }
    return 0;
  } else {
    errmess="only do unpacked exta data";
    ERR;
  }

 err:
  pp_error_mesg("pp_extra_has_vector",errmess);
  return 0;
}

#endif
