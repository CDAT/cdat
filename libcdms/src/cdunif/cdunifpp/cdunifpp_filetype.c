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

/* takes input filename and PPfile structure containing file handle 
 *
 * fills in the "type", "swap" and "wordsize" elements of the structure,
 * and then returns 0 on success, or -1 if file doesn't appear to be a
 * valid PP or UM ancillary file
 */

/* tests the contents, but first see if the filename has one of the specified
 * extensions, and if so then this forces the file type
 */

int pp_determine_file_type(PPfile *ppfile, const char *filename, int verbose)
{
  int status;

  status=pp_determine_file_type_by_name(ppfile,filename);

  if (status!=0)
   status=pp_determine_file_type_by_contents(ppfile);

  if (verbose && status==0)  
    printf("%s is a %s%d-bit %s-file\n",
	   filename,
	   ppfile->swap ? "byte-swapped ":"",
	   ppfile->wordsize * 8,
	   ppfile->type == um_type ? "UM" : "PP");

  return status;
}

int pp_determine_file_type_by_contents(PPfile *ppfile)
{
  FILE *fh;
  Fint4 data4[4],data4s[4];
  Fint8 data8[2],data8s[2];

  if(  ppfile==NULL || (fh=ppfile->fh)==NULL) return -1;
  
  /* read and store first two integers according to suppositions 
   * of 4- or 8- byte, and native or swapped byte ordering
   */
 
  fseek(fh,0,SEEK_SET);
  if(fread(data8,8,2,fh) != 2) return -1;

  memcpy(data4,data8,16);

  memcpy(data4s,data8,16);
  pp_swapbytes(data4s,4,4);

  memcpy(data8s,data8,16);
  pp_swapbytes(data8s,8,2);

  /* UM ancillary files: test second word (the submodel ID - is this 1, 2 or 4?)
   * (this should not give false +ve with PP file -- though takes thinking through
   * the various combinations of 32 / 64 bit to convince yourself of this)
   *
   * If this test fails, test for PP file.  Here test first word, which should be
   * record length (put there by fortran).  Because it's the first word, 64-bit 
   * little-endian could appear to be 32-bit little-endian, so a file is not 32-bit
   * PP if both the second and fourth words are zero.
   */

  if (pp_valid_um_word2(data4[1])) {
    ppfile->type = um_type;
    ppfile->swap=0;
    ppfile->wordsize=4;
  } else if (pp_valid_um_word2(data8[1])) {
    ppfile->type = um_type;
    ppfile->swap=0;
    ppfile->wordsize=8;
  } else if (pp_valid_um_word2(data4s[1])) {
    ppfile->type = um_type;
    ppfile->swap=1;
    ppfile->wordsize=4;
  } else if (pp_valid_um_word2(data8s[1])) {
    ppfile->type = um_type;
    ppfile->swap=1;
    ppfile->wordsize=8;
  } else if (pp_valid_pp_word1(data4[0],4) && !(data4[1]==0 && data4[3]==0)) {
    ppfile->type = pp_type;
    ppfile->swap=0;
    ppfile->wordsize=4;
  } else if (pp_valid_pp_word1(data8[0],8)) {
    ppfile->type = pp_type;
    ppfile->swap=0;
    ppfile->wordsize=8;
  } else if (pp_valid_pp_word1(data4s[0],4) && !(data4s[1]==0 && data4s[3]==0)) {
    ppfile->type = pp_type;
    ppfile->swap=1;
    ppfile->wordsize=4;
  } else if (pp_valid_pp_word1(data8s[0],8)) {
    ppfile->type = pp_type;
    ppfile->swap=1;
    ppfile->wordsize=8;
  } else {
    /* type not identified */
    return -1;
  }
  return 0;
}

/* values passed to pp_valid_um_word2 and pp_valid_pp_word1 could be 32 or 64-bit.
 * Declare as longer of these two (Fint8), and shorter will be accommodated also.
 */

int pp_valid_um_word2(Fint8 val)
{
  /* second word should be 1,2 or 4, reflecting model ID in fixed length header */
  return (val==1 || val==2 || val==4);
}

int pp_valid_pp_word1(Fint8 val, int wsize)
{
  /* first word should be integer from Fortan representing length of header record */
  return (val==64*wsize || val==128*wsize);
}


int pp_determine_file_type_by_name(PPfile *ppfile, const char *filename)
{
  int len;

  if (ppfile==NULL) return -1;

  len=strlen(filename);

  if (pp_string_ends_with(filename,".pp32")) {
    
    ppfile->type = pp_type;
    ppfile->swap=0;
    ppfile->wordsize=4;
    
  } else if (pp_string_ends_with(filename,".pp64")) {

    ppfile->type = pp_type;
    ppfile->swap=0;
    ppfile->wordsize=8;

  } else if (pp_string_ends_with(filename,".pp32s")) {

    ppfile->type = pp_type;
    ppfile->swap=1;
    ppfile->wordsize=4;

  } else if (pp_string_ends_with(filename,".pp64s")) {

    ppfile->type = pp_type;
    ppfile->swap=1;
    ppfile->wordsize=8;

  } else if (pp_string_ends_with(filename,".um32")) {

    ppfile->type = um_type;
    ppfile->swap=0;
    ppfile->wordsize=4;

  } else if (pp_string_ends_with(filename,".um64")) {

    ppfile->type = um_type;
    ppfile->swap=0;
    ppfile->wordsize=8;

  } else if (pp_string_ends_with(filename,".um32s")) {

    ppfile->type = um_type;
    ppfile->swap=1;
    ppfile->wordsize=4;
    
  } else if (pp_string_ends_with(filename,".um64s")) {
    
    ppfile->type = um_type;
    ppfile->swap=1;
    ppfile->wordsize=8;
    
  } else {
    /* type not identified */
    return -1;
  }  
  return  0;
}


int pp_is_ppum_file(const char *filename, FILE *fd)
{
  PPfile ppfile;

  ppfile.fh = fd;

  return (pp_determine_file_type(&ppfile, filename, 0) == 0);
}

/* case-insensitive test of ending part of string */

int pp_string_ends_with(const char *string, const char *ending) {

  int lstr, lend, i;
  const char *end;

  lstr=strlen(string);
  lend=strlen(ending);

  if (lend > lstr) return 0;

  end = string + lstr - lend;

  for (i=0 ; i<lend ; i++) {
    if (toupper(end[i]) != toupper(ending[i]))
      return 0;
  }

  return 1;
}

#endif
