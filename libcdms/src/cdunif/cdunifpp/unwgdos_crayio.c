#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <errno.h>
#include "crayio.h"

/*  Routines callable from C */

COSFILE *cos_open (char *fname, char *mode)
{
   COSFILE *file;
   FILE *fp;

   /* Allocate memory for COSFILE structure */

   if ((file = (COSFILE *) malloc(sizeof(COSFILE))) == NULL)
   {
       printf("Error unable to allocate memory in cos_open \n");
       return NULL;
   }

   /* Open file */

   if ((fp = fopen(fname, mode)) == NULL)
   {
       printf("Error opening file %s in cos_open \n", fname);
       return NULL;
   }

   /* Initialise COSFILE structure */

   file->fname = fname;
   file->fp = fp;
   file->fwi = 0;
   file->pri = 0;

   return file;
}

int cos_close (COSFILE *file)
{
   int iret;

   /* Close file */

   iret = fclose(file->fp);

   if (iret != 0)
      printf("Error closing file %s in cos_close \n", file->fname);

   /* Free memory for COSFILE structure */

   free(file);

   return iret;
}

int cos_rewind (COSFILE *file)
{
   int iret;

   /* Rewind file */

   iret = fseek(file->fp, 0L, SEEK_SET);

   if (iret != 0)
      printf("Error rewinding file %s in cos_rewind \n", file->fname);

   /* Reset forward index and previous record index */

   file->fwi = 0;
   file->pri = 0;

   return iret;
}

int cos_backspace (COSFILE *file)
{
   long inum, blknum;
   unsigned long m, pri, fwi, curpos, pos;
   unsigned char cw[CRAYWORD];

   curpos = ftell(file->fp);
   blknum = curpos/BLOCKSIZE;
   pri = file->pri;
   pos = (blknum-pri)*BLOCKSIZE;

   /* Seek to start of block containing start of record */

   if (fseek(file->fp, pos, SEEK_SET) != 0)
   {
      printf("Error backspacing file %s in cos_backspace \n", file->fname);
      return 1;
   }

   inum = fread(cw, 1, CRAYWORD, file->fp);
   m = cw[0] >> 4;
   if (m != 0)
   {
      printf("Error backspacing file %s in cos_backspace \n", file->fname);
      return 2;
   }
   fwi = ((unsigned long) (cw[CRAYWORD-2] & 1) << 8) | cw[CRAYWORD-1];
   pos += CRAYWORD*(fwi+1);
   m = 1;
   if (pri == 0)
   {
     if (pos+CRAYWORD == curpos) m = 0;
   }
   else
   {
     if (pos == (blknum-pri+1)*BLOCKSIZE) m = 0;
   }

   while ( m != 0 )
   {
      /* Seek to next record until start of current record or start of next 
         block is found */

      if (fseek(file->fp, pos, SEEK_SET) != 0)
      {
         printf("Error backspacing file %s in cos_backspace \n", file->fname);
         return 3;
      }

      inum = fread(cw, 1, CRAYWORD, file->fp);
      m = cw[0] >> 4;
      if (m == 0)
      {
         printf("Error backspacing file %s in cos_backspace \n", file->fname);
         return 4;
      }
      fwi = ((unsigned long) (cw[CRAYWORD-2] & 1) << 8) | cw[CRAYWORD-1];
      pos += CRAYWORD*(fwi+1);

      if (pri == 0)
      {
        if (pos+CRAYWORD == curpos) break;
      }
      else
      {
        if (pos == (blknum-pri+1)*BLOCKSIZE) break;
      }
   }

   /* Reset forward index and previous record index */

   file->fwi = fwi;
   file->pri = (cw[CRAYWORD-3] << 7) | (cw[CRAYWORD-2] >> 1);

   return 0;
}

int cos_read (COSFILE *file, void *data, int datasize, int *readsize)
{
   int iret, inum, full;
   long datalen, dataoffset, ilen;
   unsigned long m, fwi;
   unsigned char cw[CRAYWORD];
   char *cdata;

   cdata = (char *) data;

   iret = 0;
   fwi = file->fwi;
   ilen = 0;
   m = 0;
   if (datasize == 0)
      full = 1;
   else
      full = 0;
   dataoffset = 0;

   while ( m != CEOR && m != CEOF && m != CEOD )
   {
      ilen += CRAYWORD*fwi;

      if (ilen > datasize)
      {
         if (full == 0)
         {
            datalen=CRAYWORD*fwi - ilen + datasize;
            full = 1;
         }
         else
            datalen = 0;
      }
      else
         datalen=CRAYWORD*fwi;

      if (datalen > 0)
      {
         inum = fread(cdata+dataoffset, 1, datalen, file->fp);
         dataoffset += datalen;
         if (full == 1)
            inum = fseek(file->fp, ilen - datasize, SEEK_CUR);
      }
      else
         inum = fseek(file->fp, CRAYWORD*fwi, SEEK_CUR);

      inum = fread(cw, 1, CRAYWORD, file->fp);

      m = cw[0] >> 4;
      fwi = ((unsigned long) (cw[CRAYWORD-2] & 1) << 8) | cw[CRAYWORD-1];
   }

   if ( m == CEOF || m == CEOD )
      iret = -1;
   else if ( full == 1 )
      iret = -3;

   file->fwi = fwi;
   file->pri = (cw[CRAYWORD-3] << 7) | (cw[CRAYWORD-2] >> 1);
   *readsize = dataoffset;

   return iret;
}

/* 
 * buf is an array of n objects of size nbytes bytes.
 * reverse order of bytes in buf. buf will be changed by function.
 */

void swap_bytes(void *buf, int nbytes, int n)
{
   int i, j;
   unsigned char temp, *cbuf;

   cbuf = (unsigned char *) buf;

   for (i=0; i<n ; i++)
   {
      for (j=0; j<nbytes/2; j++)
      {
         temp = cbuf[j];
         cbuf[j] = cbuf[nbytes-j-1];
         cbuf[nbytes-j-1] = temp;
      }
      cbuf += nbytes;
   }

   return;
}

/* 
 * The following routine is adapted from code written originally
 * by Chuck D'Ambra (NCAR), and modified by Bob Drach
 *
 * Translate Cray 64-bit reals to IEEE 32-bit reals
 *
 * in: array of Cray 64-bit reals
 * out: array of result IEEE 32-bit reals
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 *  Cray 64 bit float representation:
 *      bits 0-47:   mantissa; bit 47 is 1 for normalized floating point
 *      bits 48-62:  exponent
 *      bit 63:      sign
 *
 *  IEEE 32 bit float representation:
 *      bits 0-22:   mantissa, leading 1 bit implicit for normalized reals
 *      bits 23-30:  exponent
 *      bit 31:      sign
 */

int c8_to_r4(void *in, void *out, int n)
{
#ifndef _CRAY
  int i, j, joff, nchar, shift;
  unsigned char *ucin, p[8], sign, *mp;
  unsigned long cexp, iman, round, mask;
  uint32 *uiout, sign32;
  long iexp;
  int err, k;

  ucin = (unsigned char *) in;
  uiout = (uint32 *) out;

  err=0;
  
  for (i = 0,joff=0; i < n; i++,ucin+=8,joff++)
  {
    memcpy(p, ucin, 8);
    sign = *(p) & CSIGNMASK;    /* sign bit */
    sign32 = (uint32) sign << 24;

    /*  If input is +/- 0 set output to be the same */

    if ((*(p) & ~CSIGNMASK)==0 && *(p+1)=='\0' && *(p+2)=='\0' && 
         *(p+3)=='\0' && *(p+4)=='\0' && *(p+5)=='\0' && *(p+6)=='\0' && 
         *(p+7)=='\0')
    {
        uiout[joff] = sign32;
        continue;
    }

    cexp = (*(p+1)) | ((unsigned long) (*p & 0x7f) << 8);  
                                           /* Cray unbiased exponent */
    iexp = cexp - CBIAS + I32BIAS - 1;     /* IEEE unbiased exponent */

    /* Check that Cray number is normalised */

    if ((*(p+2) & CSIGNMASK) != CSIGNMASK)
    {
       /* Normalise Cray number */

       nchar = 6;
       mp = p+2;

       for (j = 0; j < 6; j++)
       {
          if (mp[0] == '\0')
          {
             mp++;
             nchar--;
          }
          else
             break;
       }

       shift = 0;
       for (j=0; j<8; j++)
       {
          mask = CSIGNMASK >> j;
          if ((*mp & mask) == mask)
             break;
          shift += 1;
       }

       for (j = 0; j < nchar-1; j++)
          *(p+j+2) = (mp[j] << shift) | (mp[j+1] >> (8-shift));

       *(p+nchar+1) = mp[nchar-1] << shift;

       for (j = nchar; j < 6; j++)
          *(p+j+2) = '\0';

       cexp -= (6-nchar)*8 + shift;
    }

    if (cexp <= MAXEXP32 && cexp >= MINEXP32)  /* Normal numbers */
    {
       /* IEEE mantissa */
       iman = ((unsigned long) (*(p+2) & ~CSIGNMASK) << 16) | 
              ((unsigned long) *(p+3) << 8) | *(p+4);

       if ((*(p+5) & CSIGNMASK) == CSIGNMASK)
       {
          /*  Round up mantissa */
          if (((~iman) & 0x7fffff) == 0)
          {
             iman = 0;
             cexp += 1;
          }
          else
             iman += 1;
       }
    }
    else if (cexp < MINEXP32 && cexp >= MINSEXP32)  /* Subnormal numbers */
    {
       k = MINEXP32 - cexp -  1;
       iman = ((unsigned long) *(p+2) << 16) | 
              ((unsigned long) *(p+3) << 8) | *(p+4);
       mask = (unsigned long) 1 << k;
       round = iman & mask;
       iman = iman >> (k+1);
       if (round == mask) iman += 1;
    }

    /*  Pack input into 32 bit IEEE float representation */

    if (cexp <= MAXEXP32 && cexp >= MINSEXP32)
    {
       if (cexp >= MINEXP32)
          iexp = cexp - CBIAS + I32BIAS - 1; /* IEEE unbiased exponent */
       else
          iexp = 0;   /* IEEE unbiased exponent for subnormal numbers*/

       uiout[joff] = sign32 | (iexp << 23) | iman;
    }

    /*  If invalid Cray floats, set to NaN */

    else if (cexp < MINCEXP || cexp > MAXCEXP)
    {
        uiout[joff] = I32_NAN;
        err=1;
    }

    /*  Small Cray numbers, set to +/- 0 */

    else if (cexp < MINSEXP32)
    {
        uiout[joff] = sign32;
    }

    /*  Large Cray numbers, set to +/- INF */

    else if (cexp > MAXEXP32)
    {
        uiout[joff] =  sign32 | I32_INFP;
        err=1;
    }
  }

  return err;
#else
  int err, type, bitoff, stride, natlen, forlen;
  void *forn, *nat;

#ifdef _CRAYIEEE
  forn = in;
  nat = out;
  natlen = 32;
  forlen = 64;
#else
  forn = out;
  nat = in;
  natlen = 64;
  forlen = 32;
#endif
  type = 3;
  bitoff = 0;
  stride = 1;

  err = CRY2CRI(&type, &n, forn, &bitoff, nat, &stride, &natlen, &forlen);

  return err;
#endif
}

/* 
 * Translate Cray 64-bit integers to IEEE 32-bit integers
 *
 * in: array of Cray 64-bit integers
 * out: array of result IEEE 32-bit integers
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int c8_to_i4(void *in, void *out, int n)
{
#ifndef _CRAY
  unsigned char *pin, sign;
  int32 *pout;
  int err, i;

  pin = (unsigned char *) in;
  pout = (int32 *) out;

  err=0;

  for (i=0; i<n; i++, pin+=8, pout++)
  {
     sign = pin[0] & CSIGNMASK;	/* sign bit */

     if (sign == 0)
     {
        if ((pin[4] & CSIGNMASK) != 0 || pin[3] != 0 ||
             pin[2] != 0 || pin[1] != 0 || pin[0] != 0)
        {
           pout[0] = 0x7fffffff; /* largest positive 32 bit integer */
           err=1;
           continue;
        }
     }
     else if ((pin[4] & CSIGNMASK) != 0x80 || pin[3] != 0xff ||
               pin[2] != 0xff || pin[1] != 0xff || pin[0] != 0xff)
     {
        pout[0] = 0x80000000; /* largest negative 32 bit integer */
        err=1;
        continue;
     }

     pout[0] = ((int32) (sign | (pin[4] & (~CSIGNMASK))) << 24) |
                (int32) (pin[5] << 16) | ((int32) pin[6] << 8) | pin[7];
  }

  return err;
#else
  int err, type, bitoff, stride, natlen, forlen;
  void *forn, *nat;

#ifdef _CRAYIEEE
  forn = in;
  nat = out;
  natlen = 32;
  forlen = 64;
#else
  forn = out;
  nat = in;
  natlen = 64;
  forlen = 32;
#endif
  type = 2;
  bitoff = 0;
  stride = 1;

  err = CRY2CRI(&type, &n, forn, &bitoff, nat, &stride, &natlen, &forlen);

  return err;
#endif
}

/* 
 * Translate Cray 64-bit logicals to IEEE 32-bit logicals
 *
 * in: array of Cray 64-bit logicals
 * out: array of result IEEE 32-bit logicals
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int c8_to_l4(void *in, void *out, int n)
{
#ifndef _CRAY
  int64 *pin;
  int32 *pout;
  int err, i;

  pin = (int64 *) in;
  pout = (int32 *) out;

  err=0;

  for (i=0; i<n; i++)
     pout[i] = pin[i] & (int64) 1;

  return err;
#else
  int err, type, bitoff, stride, natlen, forlen;
  void *forn, *nat;

#ifdef _CRAYIEEE
  forn = in;
  nat = out;
  natlen = 32;
  forlen = 64;
#else
  forn = out;
  nat = in;
  natlen = 64;
  forlen = 32;
#endif
  type = 5;
  bitoff = 0;
  stride = 1;

  err = CRY2CRI(&type, &n, forn, &bitoff, nat, &stride, &natlen, &forlen);

  return err;
#endif
}

/* 
 * Translate Cray 64-bit reals to IEEE 64-bit reals
 *
 * in: array of Cray 64-bit reals
 * out: array of result IEEE 64-bit reals
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 *  Cray 64 bit float representation:
 *      bits 0-47:   mantissa; bit 47 is 1 for normalized floating point
 *      bits 48-62:  exponent
 *      bit 63:      sign
 *
 *  IEEE 64 bit float representation:
 *      bits 0-51:   mantissa, leading 1 bit implicit for normalized reals
 *      bits 52-62:  exponent
 *      bit 63:      sign
 */

int c8_to_r8(void *in, void *out, int n)
{
#ifndef _CRAY
  int i, j, joff, nchar, shift;
  unsigned char *ucin, p[8], sign, *mp;
  unsigned long cexp, mask;
  uint64 *uiout, sign64, iman, round, mask64;
  long iexp;
  int err, k;

  ucin = (unsigned char *) in;
  uiout = (uint64 *) out;

  err=0;
 
  for (i = 0,joff=0; i < n; i++,ucin+=8,joff++)
  {
    memcpy(p, ucin, 8);
    sign = *(p) & CSIGNMASK;    /* sign bit */
    sign64 = (uint64) sign << 56;

    /*  If input is +/- 0 set output to be the same */

    if ((*(p) & ~CSIGNMASK)==0 && *(p+1)=='\0' && *(p+2)=='\0' && 
         *(p+3)=='\0' && *(p+4)=='\0' && *(p+5)=='\0' && *(p+6)=='\0' && 
         *(p+7)=='\0')
    {
        uiout[joff] = sign64;
        continue;
    }

    cexp = (*(p+1)) | ((unsigned long) (*p & 0x7f) << 8);  
                                           /* Cray unbiased exponent */
    iexp = cexp - CBIAS + I64BIAS - 1;     /* IEEE unbiased exponent */

    /* Check that Cray number is normalised */

    if ((*(p+2) & CSIGNMASK) != CSIGNMASK)
    {
       /* Normalise Cray number */

       nchar = 6;
       mp = p+2;

       for (j = 0; j < 6; j++)
       {
          if (mp[0] == '\0')
          {
             mp++;
             nchar--;
          }
          else
             break;
       }

       shift = 0;
       for (j=0; j<8; j++)
       {
          mask = CSIGNMASK >> j;
          if ((*mp & mask) == mask)
             break;
          shift += 1;
       }

       for (j = 0; j < nchar-1; j++)
          *(p+j+2) = (mp[j] << shift) | (mp[j+1] >> (8-shift));

       *(p+nchar+1) = mp[nchar-1] << shift;

       for (j = nchar; j < 6; j++)
          *(p+j+2) = '\0';

       cexp -= (6-nchar)*8 + shift;
    }

    if (cexp <= MAXEXP64 && cexp >= MINEXP64)  /* Normal numbers */
    {
       /* IEEE mantissa */
       iman = ((uint64) (*(p+2) & ~CSIGNMASK) << 45) | ((uint64) *(p+3) << 37) |
              ((uint64) *(p+4) << 29) | ((uint64) *(p+5) << 21) | 
              ((uint64) *(p+6) << 13) | ((uint64) *(p+7) << 5);
    }
    else if (cexp < MINEXP64 && cexp >= MINSEXP64)  /* Subnormal numbers */
    {
       k = MINEXP64 - cexp -  1;
       iman = ((uint64) *(p+2) << 45) | ((uint64) *(p+3) << 37) | 
              ((uint64) *(p+4) << 29) | ((uint64) *(p+5) << 21) | 
              ((uint64) *(p+6) << 13) | ((uint64) *(p+7) << 5);
       mask64 = (uint64) 1 << k;
       round = iman & mask64;
       iman = iman >> (k+1);
       if (round == mask64) iman += 1;
    }

    /*  Pack input into 64 bit IEEE float representation */

    if (cexp <= MAXEXP64 && cexp >= MINSEXP64)
    {
       if (cexp >= MINEXP64)
          iexp = cexp - CBIAS + I64BIAS - 1; /* IEEE unbiased exponent */
       else
          iexp = 0;   /* IEEE unbiased exponent for subnormal numbers */

       uiout[joff] = sign64 | ((uint64) iexp << 52) | iman;
    }

    /*  If invalid Cray floats, set to NaN */

    else if (cexp < MINCEXP || cexp > MAXCEXP)
    {
        uiout[joff] = I64_NAN;
        err=1;
    }

    /*  Small Cray numbers, set to +/- 0 */

    else if (cexp < MINSEXP64)
    {
        uiout[joff] = sign64;
    }

    /*  Large Cray numbers, set to +/- INF */

    else if (cexp > MAXEXP64)
    {
        uiout[joff] =  sign64 | I64_INFP;
        err=1;
    }
  }

  return err;
#else
  int err, type, bitoff, stride, natlen, forlen;
  void *forn, *nat;

#ifdef _CRAYIEEE
  forn = in;
  nat = out;
#else
  forn = out;
  nat = in;
#endif
  type = 3;
  bitoff = 0;
  stride = 1;
  natlen = 64;
  forlen = 64;

  err = CRY2CRI(&type, &n, forn, &bitoff, nat, &stride, &natlen, &forlen);

  return err;
#endif
}

/* 
 * Translate Cray 64-bit integers to IEEE 64-bit integers
 *
 * in: array of Cray 64-bit integers
 * out: array of result IEEE 64-bit integers
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int c8_to_i8(void *in, void *out, int n)
{
  int64 *pin, *pout;
  int err;

  pin = (int64 *) in;
  pout = (int64 *) out;

  err=0;

  memcpy(pout, pin, 8*n);

#ifdef LITTLE__ENDIAN
  swap_bytes(pout, 8, n);
#endif

  return err;
}

/* 
 * Translate Cray 64-bit logicals to IEEE 64-bit logicals
 *
 * in: array of Cray 64-bit logicals
 * out: array of result IEEE 64-bit logicals
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int c8_to_l8(void *in, void *out, int n)
{
  int64 *pin, *pout;
  int err, i;

  pin = (int64 *) in;
  pout = (int64 *) out;

  err=0;

  for (i=0; i<n; i++)
     pout[i] = pin[i] & (int64) 1;

  return err;
}

/* 
 * Translate IEEE 64-bit reals to IEEE 32-bit reals
 *
 * in: array of IEEE 64-bit reals
 * out: array of result IEEE 32-bit reals
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int r8_to_r4(void *in, void *out, int n)
{
#ifndef _CRAY
  int i;
  float32 *r32;
  float64 *r64;
  int32 *i32;

  r64 = (float64 *) in;
  r32 = (float32 *) out;
  i32 = (int32 *) out;

  for (i=0; i<n; i++)
  {
     if (*r64 > (double) FLT_MAX)
        *i32 = (int32) I32_INFP;
     else if (*r64 < - (double) FLT_MAX)
        *i32 = (int32) I32_INFN;
     else
        *r32 = (float32) *r64;

     r32++;
     i32++;
     r64++;
  }

  return 0;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 3;
  bitoff = 0;
  stride = 1;
  intlen = 64;
  extlen = 32;

  err = CRI2IEG(&type, &n, out, &bitoff, in, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IEEE 64-bit integers to IEEE 32-bit integers
 *
 * in: array of IEEE 64-bit integers
 * out: array of result IEEE 32-bit integers
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int i8_to_i4(void *in, void *out, int n)
{
#ifndef _CRAY
  int i;
  int32 *i32;
  int64 *i64;

  i64 = (int64 *) in;
  i32 = (int32 *) out;

  for (i=0; i<n; i++)
     i32[i] = (int32) i64[i];

  return 0;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 2;
  bitoff = 0;
  stride = 1;
  intlen = 64;
  extlen = 32;

  err = CRI2IEG(&type, &n, out, &bitoff, in, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IEEE 32-bit reals to IEEE 64-bit reals
 *
 * in: array of IEEE 32-bit reals
 * out: array of result IEEE 64-bit reals
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int r4_to_r8(void *in, void *out, int n)
{
#ifndef _CRAY
  int i;
  float32 *r32;
  float64 *r64;

  r32 = (float32 *) in;
  r64 = (float64 *) out;

  for (i=0; i<n; i++)
     r64[i] = (float64) r32[i];

  return 0;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 3;
  bitoff = 0;
  stride = 1;
  intlen = 64;
  extlen = 32;

  err = IEG2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IEEE 32-bit integers to IEEE 64-bit integers
 *
 * in: array of IEEE 32-bit integers
 * out: array of result IEEE 64-bit integers
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int i4_to_i8(void *in, void *out, int n)
{
#ifndef _CRAY
  int i;
  int32 *i32;
  int64 *i64;

  i32 = (int32 *) in;
  i64 = (int64 *) out;

  for (i=0; i<n; i++)
     i64[i] = (int64) i32[i];

  return 0;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 2;
  bitoff = 0;
  stride = 1;
  intlen = 64;
  extlen = 32;

  err = IEG2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

#ifdef _CRAY
/* 
 * Translate IEEE 32-bit reals to Cray 64-bit reals
 *
 * in: array of IEEE 32-bit reals
 * out: array of result Cray 64-bit reals
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int r4_to_c8(void *in, void *out, int n)
{
  int type, bitoff, stride, natlen, forlen, err;
  void *forn, *nat;

#ifdef _CRAYIEEE
  forn = out;
  nat = in;
  natlen = 32;
  forlen = 64;
#else
  forn = in;
  nat = out;
  natlen = 64;
  forlen = 32;
#endif
  type = 3;
  bitoff = 0;
  stride = 1;

  err = CRI2CRY(&type, &n, forn, &bitoff, nat, &stride, &natlen, &forlen);

  return err;
}

/* 
 * Translate IEEE 32-bit integers to Cray 64-bit integers
 *
 * in: array of IEEE 64-bit integers
 * out: array of result Cray 32-bit integers
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int i4_to_c8(void *in, void *out, int n)
{
  int type, bitoff, stride, natlen, forlen, err;
  void *forn, *nat;

#ifdef _CRAYIEEE
  forn = out;
  nat = in;
  natlen = 32;
  forlen = 64;
#else
  forn = in;
  nat = out;
  natlen = 64;
  forlen = 32;
#endif
  type = 2;
  bitoff = 0;
  stride = 1;

  err = CRI2CRY(&type, &n, forn, &bitoff, nat, &stride, &natlen, &forlen);

  return err;
}

/* 
 * Translate IEEE 32-bit logicals to Cray 64-bit logicals
 *
 * in: array of IEEE 64-bit logicals
 * out: array of result Cray 32-bit logicals
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int l4_to_c8(void *in, void *out, int n)
{
  int type, bitoff, stride, natlen, forlen, err;
  void *forn, *nat;

#ifdef _CRAYIEEE
  forn = out;
  nat = in;
  natlen = 32;
  forlen = 64;
#else
  forn = in;
  nat = out;
  natlen = 64;
  forlen = 32;
#endif
  type = 5;
  bitoff = 0;
  stride = 1;

  err = CRI2CRY(&type, &n, forn, &bitoff, nat, &stride, &natlen, &forlen);

  return err;
}
/* 
 * Translate IEEE 64-bit reals to Cray 64-bit reals
 *
 * in: array of IEEE 64-bit reals
 * out: array of result Cray 64-bit reals
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int r8_to_c8(void *in, void *out, int n)
{
  int type, bitoff, stride, natlen, forlen, err;
  void *forn, *nat;

#ifdef _CRAYIEEE
  forn = out;
  nat = in;
#else
  forn = in;
  nat = out;
#endif
  type = 3;
  bitoff = 0;
  stride = 1;
  natlen = 64;
  forlen = 64;

  err = CRI2CRY(&type, &n, forn, &bitoff, nat, &stride, &natlen, &forlen);

  return err;
}

/* 
 * Translate IEEE 64-bit integers to Cray 64-bit integers
 *
 * in: array of IEEE 64-bit integers
 * out: array of result Cray 64-bit integers
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int i8_to_c8(void *in, void *out, int n)
{
  int type, bitoff, stride, natlen, forlen, err;
  void *forn, *nat;

#ifdef _CRAYIEEE
  forn = out;
  nat = in;
#else
  forn = in;
  nat = out;
#endif
  type = 2;
  bitoff = 0;
  stride = 1;
  natlen = 64;
  forlen = 64;

  err = CRI2CRY(&type, &n, forn, &bitoff, nat, &stride, &natlen, &forlen);

  return err;
}

/* 
 * Translate IEEE 64-bit logicals to Cray 64-bit logicals
 *
 * in: array of IEEE 64-bit logicals
 * out: array of result Cray 64-bit logicals
 * n: length of in, out 
 *
 * return: 0 if no errors, 1 if an error occurred
 *
 */

int l8_to_c8(void *in, void *out, int n)
{
  int type, bitoff, stride, natlen, forlen, err;
  void *forn, *nat;

#ifdef _CRAYIEEE
  forn = out;
  nat = in;
#else
  forn = in;
  nat = out;
#endif
  type = 5;
  bitoff = 0;
  stride = 1;
  natlen = 64;
  forlen = 64;

  err = CRI2CRY(&type, &n, forn, &bitoff, nat, &stride, &natlen, &forlen);

  return err;
}
#endif

/* 
 * Translate IBM 32-bit integers to IEEE 32-bit integers
 *
 */

int ibmi4_to_i4(void *in, void *out, int n)
{
#ifndef _CRAY
  int32 *pin;
  int32 *pout;
  int err, i;

  pin = (int32 *) in;
  pout = (int32 *) out;

  err=0;

  for (i=0; i<n; i++)
     pout[i] = pin[i];

#ifdef LITTLE__ENDIAN
  swap_bytes(pout, 4, n);
#endif

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 2;
  bitoff = 0;
  stride = 1;
  intlen = 32;
  extlen = 32;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 16-bit integers to IEEE 32-bit integers
 *
 */

int ibmi2_to_i4(void *in, void *out, int n, int offset)
{
#ifndef _CRAY
  int16 *pin;
  int32 *pout;
  int err, i;

  err=0;
  pin = (int16 *) in;
  pout = (int32 *) out;

  pin += offset;

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 2, n);
#endif

  for (i=0; i<n; i++)
     pout[i] = pin[i];

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 2, n);
#endif

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 2;
  bitoff = offset*16;
  stride = 1;
  intlen = 32;
  extlen = 16;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 32-bit integers to IEEE 64-bit integers
 *
 */

int ibmi4_to_i8(void *in, void *out, int n, int offset)
{
#ifndef _CRAY
  int32 *pin;
  int64 *pout;
  int err, i;

  err=0;
  pin = (int32 *) in;
  pout = (int64 *) out;

  pin += offset;

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 4, n);
#endif

  for (i=0; i<n; i++)
     pout[i] = pin[i];

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 4, n);
#endif

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 2;
  bitoff = offset*32;
  stride = 1;
  intlen = 64;
  extlen = 32;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 16-bit integers to IEEE 64-bit integers
 *
 */

int ibmi2_to_i8(void *in, void *out, int n, int offset)
{
#ifndef _CRAY
  int16 *pin;
  int64 *pout;
  int err, i;

  err=0;
  pin = (int16 *) in;
  pout = (int64 *) out;

  pin += offset;

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 2, n);
#endif

  for (i=0; i<n; i++)
     pout[i] = pin[i];

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 2, n);
#endif

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 2;
  bitoff = offset*16;
  stride = 1;
  intlen = 64;
  extlen = 16;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 32-bit reals to IEEE 32-bit reals
 *
 */

int ibmr4_to_r4(void *in, void *out, int n)
{
#ifndef _CRAY
  unsigned char *pin;
  float32 *pout;
  unsigned long man;
  int err, i, exp, sign;
  double d;
  uint32 i32;

  pin = (unsigned char *) in;
  pout = (float32 *) out;

  err=0;

  for (i=0; i<n; i++)
  {
     sign = pin[0] & 0x80;
     exp = pin[0] & 0x7f;
     man = ((unsigned long) pin[1] << 16) | 
           ((unsigned long) pin[2] << 8) | (unsigned long) pin[3];

     d = ldexp((double) man ,4*(exp-64-6));

     if (d > (double) FLT_MAX || errno == ERANGE)
     {
        i32 = (sign ? (int32) I32_INFN : (int32) I32_INFP);
        *pout = *(float32 *) &i32;
        err=1;
     }
     else if (d < (double) FLT_MIN) 
     {
        i32 = (sign ? (int32) I32_ZERON : (int32) I32_ZEROP);
        *pout = *(float32 *) &i32;
     }
     else
        *pout = (sign ? -d : d);

     pout++;
     pin += 4;     
  }

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 3;
  bitoff = 0;
  stride = 1;
  intlen = 32;
  extlen = 32;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 32-bit reals to IEEE 64-bit reals
 *
 */

int ibmr4_to_r8(void *in, void *out, int n, int offset)
{
#ifndef _CRAY
  unsigned char *pin;
  float64 *pout;
  unsigned long man;
  int err, i, exp, sign;
  double d;
  uint64 i64;

  pin = (unsigned char *) in;
  pout = (float64 *) out;

  for (i=0; i<4*offset; i++) pin++;

  err=0;

  for (i=0; i<n; i++)
  {
     sign = pin[0] & 0x80;
     exp = pin[0] & 0x7f;
     man = ((unsigned long) pin[1] << 16) | 
           ((unsigned long) pin[2] << 8) | (unsigned long) pin[3];

     d = ldexp((double) man ,4*(exp-64-6));

     if (d > (double) DBL_MAX || errno == ERANGE)
     {
        i64 = (sign ? (int64) I64_INFN : (int64) I64_INFP);
        *pout = *(float64 *) &i64;
        err=1;
     }
     else
        *pout = (sign ? -d : d);

     pout++;
     pin += 4;     
  }

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 3;
  bitoff = offset*32;
  stride = 1;
  intlen = 64;
  extlen = 32;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 64-bit reals to IEEE 64-bit reals
 *
 */

int ibmr8_to_r8(void *in, void *out, int n)
{
#ifndef _CRAY
  unsigned char *pin;
  float64 *pout;
  unsigned long man;
  int err, i, exp, sign;
  double d;
  uint64 i64;

  pin = (unsigned char *) in;
  pout = (float64 *) out;

  err=0;

  for (i=0; i<n; i++)
  {
     sign = pin[0] & 0x80;
     exp = pin[0] & 0x7f;
     man = ((unsigned long) pin[1] << 16) | 
           ((unsigned long) pin[2] << 8) | (unsigned long) pin[3];
     d = (double) man;
     d = 65536.0*d + (((unsigned long) pin[4] << 8) + (unsigned long) pin[5]);
     d = 65536.0*d + (((unsigned long) pin[6] << 8) + (unsigned long) pin[7]);

     d = ldexp(d ,4*(exp-64-14));

     if (d > (double) DBL_MAX || errno == ERANGE)
     {
        i64 = (sign ? (int64) I64_INFN : (int64) I64_INFP);
        *pout = *(float *) &i64;
        err=1;
     }
     else
        *pout = (sign ? -d : d);

     pout++;
     pin += 8;     
  }

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 3;
  bitoff = 0;
  stride = 1;
  intlen = 64;
  extlen = 64;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

#ifdef _CRAYPVP
/* 
 * Translate IBM 32-bit integers to CRAY 64-bit integers
 *
 */

int ibmi4_to_c8(void *in, void *out, int n, int offset)
{
  int err, type, bitoff;

  type = 1;
  bitoff = offset*32;

  err = IBM2CRAY(&type, &n, in, &bitoff, out);

  return err;
}

/* 
 * Translate IBM 16-bit integers to CRAY 64-bit integers
 *
 */

int ibmi2_to_c8(void *in, void *out, int n, int offset)
{
  int err, type, bitoff;

  type = 7;
  bitoff = offset*16;

  err = IBM2CRAY(&type, &n, in, &bitoff, out);

  return err;
}

/* 
 * Translate IBM 32-bit reals to CRAY 64-bit reals
 *
 */

int ibmr4_to_c8(void *in, void *out, int n, int offset)
{
  int err, type, bitoff;

  type = 2;
  bitoff = offset*32;

  err = IBM2CRAY(&type, &n, in, &bitoff, out);

  return err;
}

/* 
 * Translate IBM 64-bit reals to CRAY 64-bit reals
 *
 */

int ibmr8_to_c8(void *in, void *out, int n)
{
  int err, type, bitoff;

  type = 3;
  bitoff = 0;

  err = IBM2CRAY(&type, &n, in, &bitoff, out);

  return err;
}
#endif

/*  Routines callable from Fortran */

void cosopen (COSFILE **file, fpchar fname, fpchar mode, INTEGER *iret,
              long flen, long mlen)
{
   char *p, *cfname, *cmode;

#ifdef _CRAY
   flen = _fcdlen(fname);
   cfname = (char *) malloc(flen+1);
   strncpy(cfname, _fcdtocp(fname), flen);
   cfname[flen] = '\0';
   mlen = _fcdlen(mode);
   cmode = (char *) malloc(mlen+1);
   strncpy(cmode,  _fcdtocp(mode), mlen);
   cmode[mlen] = '\0';
#else
   cfname = (char *) malloc(flen+1);
   strncpy(cfname, fname, flen);
   cfname[flen] = '\0';
   cmode = (char *) malloc(mlen+1);
   strncpy(cmode, mode, mlen);
   cmode[mlen] = '\0';
#endif

   /* strip blanks */

   p = cfname;
   while(*p)
   {
      if (*p == ' ') *p = '\0';
      p++;
   }
   p = cmode;
   while(*p)
   {
      if (*p == ' ') *p = '\0';
      p++;
   }

   /* Open file */

   *file = cos_open(cfname, cmode);

   if (*file == NULL)
       *iret = -1;
   else
       *iret = 0;

   return;
}

void cosclose (COSFILE **file, INTEGER *iret)
{

   /* Close file */

   *iret = cos_close(*file);

   return;
}

void cosrewind (COSFILE **file, INTEGER *iret)
{

   /* Rewind file */

   *iret = cos_rewind(*file);

   return;
}

void cosbackspace (COSFILE **file, INTEGER *iret)
{

   /* Backspace file */

   *iret = cos_backspace(*file);

   return;
}

void cosread (COSFILE **file, void *data, INTEGER *datasize, INTEGER *readsize, 
              INTEGER *iret)
{

   /* Read file */

   int readsize1;

   *iret = cos_read(*file, data, *datasize, &readsize1);

   *readsize = readsize1;

   return;
}

void swapbytes(void *buf, INTEGER *nbytes, INTEGER *n)
{
   swap_bytes(buf, *nbytes, *n);

   return;
}

void c8tor4 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Cray 64 bit reals to Ieee 32 bit reals */

   *iret = c8_to_r4(in, out, *n);

   return;
}

void c8toi4 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Cray 64 bit integers to Ieee 32 bit integers */

   *iret = c8_to_i4(in, out, *n);

   return;
}

void c8tol4 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Cray 64 bit logicals to Ieee 32 bit logicals */

   *iret = c8_to_l4(in, out, *n);

   return;
}

void c8tor8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Cray 64 bit reals to Ieee 64 bit reals */

   *iret = c8_to_r8(in, out, *n);

   return;
}

void c8toi8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Cray 64 bit integers to Ieee 64 bit integers */

   *iret = c8_to_i8(in, out, *n);

   return;
}

void c8tol8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Cray 64 bit logicals to Ieee 64 bit logicals */

   *iret = c8_to_l8(in, out, *n);

   return;
}

void r8tor4 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Ieee 64 bit reals to Ieee 32 bit reals */

   *iret = r8_to_r4(in, out, *n);

   return;
}

void ii8toi4 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Ieee 64 bit integers to Ieee 32 bit integers */

   *iret = i8_to_i4(in, out, *n);

   return;
}

void r4tor8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Ieee 32 bit reals to Ieee 64 bit reals */

   *iret = r4_to_r8(in, out, *n);

   return;
}

void ii4toi8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Ieee 32 bit integers to Ieee 64 bit integers */

   *iret = i4_to_i8(in, out, *n);

   return;
}

#ifdef _CRAY
void r4toc8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Ieee 32 bit reals to Cray 64 bit reals */

   *iret = r4_to_c8(in, out, *n);

   return;
}

void i4toc8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Ieee 32 bit integers to Cray 64 bit integers */

   *iret = i4_to_c8(in, out, *n);

   return;
}

void l4toc8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Ieee 32 bit logicals to Cray 64 bit logicals */

   *iret = l4_to_c8(in, out, *n);

   return;
}

void r8toc8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Ieee 64 bit reals to Cray 64 bit reals */

   *iret = r8_to_c8(in, out, *n);

   return;
}

void i8toc8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Ieee 64 bit integers to Cray 64 bit integers */

   *iret = i8_to_c8(in, out, *n);

   return;
}

void l8toc8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert Ieee 64 bit logicals to Cray 64 logicals */

   *iret = l8_to_c8(in, out, *n);

   return;
}
#endif

void ibmi4toi4 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert IBM 32 bit integers to Ieee 32 bit integers */

   *iret = ibmi4_to_i4(in, out, *n);

   return;
}

void ibmi2toi4 (void *in, void *out, INTEGER *n, INTEGER *offset, INTEGER *iret)
{

   /* Convert IBM 16 bit integers to Ieee 32 bit integers */

   *iret = ibmi2_to_i4(in, out, *n, *offset);

   return;
}

void ibmr4tor4 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert IBM 32 bit floats to Ieee 32 bit floats */

   *iret = ibmr4_to_r4(in, out, *n);

   return;
}

void ibmi4toi8 (void *in, void *out, INTEGER *n, INTEGER *offset, INTEGER *iret)
{

   /* Convert IBM 32 bit integers to Ieee 64 bit integers */

   *iret = ibmi4_to_i8(in, out, *n, *offset);

   return;
}

void ibmi2toi8 (void *in, void *out, INTEGER *n, INTEGER *offset, INTEGER *iret)
{

   /* Convert IBM 16 bit integers to Ieee 64 bit integers */

   *iret = ibmi2_to_i8(in, out, *n, *offset);

   return;
}

void ibmr4tor8 (void *in, void *out, INTEGER *n, INTEGER *offset, INTEGER *iret)
{

   /* Convert IBM 32 bit floats to Ieee 64 bit floats */

   *iret = ibmr4_to_r8(in, out, *n, *offset);

   return;
}

void ibmr8tor8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert IBM 64 bit floats to Ieee 64 bit floats */

   *iret = ibmr8_to_r8(in, out, *n);

   return;
}

#ifdef _CRAYPVP
void ibmi4toc8 (void *in, void *out, INTEGER *n, INTEGER *offset, INTEGER *iret)
{

   /* Convert IBM 32 bit integers to CRAY 64 bit integers */

   *iret = ibmi4_to_c8(in, out, *n, *offset);

   return;
}

void ibmi2toc8 (void *in, void *out, INTEGER *n, INTEGER *offset, INTEGER *iret)
{

   /* Convert IBM 16 bit integers to CRAY 64 bit integers */

   *iret = ibmi2_to_c8(in, out, *n, *offset);

   return;
}

void ibmr4toc8 (void *in, void *out, INTEGER *n, INTEGER *offset, INTEGER *iret)
{

   /* Convert IBM 32 bit floats to CRAY 64 bit floats */

   *iret = ibmr4_to_c8(in, out, *n, *offset);

   return;
}

void ibmr8toc8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert IBM 64 bit floats to CRAY 64 bit floats */

   *iret = ibmr8_to_c8(in, out, *n);

   return;
}
#endif

#ifndef _CRAYPVP
/* 
 *
 * Translate 32-bit packed Cray reals to reals
 * c version of CRAY routine EXPAND21 written in CAL.
 *
 * n: length of in, out 
 * in: array of 32-bit packed Cray reals
 * out: array of result reals
 * nexp: size of exponent
 *
 *  32-bit packed Cray representation:
 *      bits 0-(30-nexp):   mantissa
 *      bits (31-nexp)-30:  exponent
 *      bit 31:             sign
 */

void expand21(INTEGER *n, void *in, void *out, INTEGER *nexp)
{
#if _FLT_TYPE == _IEEE8
   expand21_r8(n, in, out, nexp);
#else
   expand21_r4(n, in, out, nexp);
#endif
}

void expand21_r4(INTEGER *n, void *in, void *out, INTEGER *nexp)
{
  int i, bias;
  uint32 *lin, crayword[2];
  uint32 cexp, pexp, sign, pfrac;
  uint32 mask1, mask2, mask3, mask4;
  float32 *lout;

#ifdef LITTLE__ENDIAN
  swap_bytes(in, 4, *n);
#endif

  lin = (uint32 *) in;
  lout = (float32 *) out;
  bias = pow(2.0,(double) *nexp-1);
  mask1 = ~(~0 << *nexp);
  mask2 = ~(~0 << (31-*nexp));
  mask3 = ~(~0 << 16);
  mask4 = ~(~0 << (15-*nexp));

  for (i = 0; i < *n; i++, lin++, lout++)
  {
     sign = *lin & CSIGNMASK1;    /* sign bit */

     /* Calculate packed unbiased exponent and Cray unbiased exponent */

     pexp = (*lin >> (31-*nexp)) & mask1;
     cexp = pexp - bias + CBIAS;

     /* Calculate packed fraction */

     pfrac = *lin & mask2;

     /* Calculate 64 bit cray word stored as 2 32 bit integers */

     crayword[0] = sign | (cexp << 16) | ((pfrac >> (15-*nexp)) & mask3);
     crayword[1] = (pfrac & mask4) << (17+*nexp);

     /* Calculate 32 bit IEEE word */

#ifdef LITTLE__ENDIAN
     swap_bytes(crayword, 4, 2);
#endif
     c8_to_r4(crayword, lout, 1);
  }

#ifdef LITTLE__ENDIAN
  swap_bytes(in, 4, *n);
#endif
}

void expand21_r8(INTEGER *n, void *in, void *out, INTEGER *nexp)
{
  int i, bias;
  uint32 *lin, crayword[2];
  uint32 cexp, pexp, sign, pfrac;
  uint32 mask1, mask2, mask3, mask4;
  float64 *lout;

#ifdef LITTLE__ENDIAN
  swap_bytes(in, 4, *n);
#endif

  lin = (uint32 *) in;
  lout = (float64 *) out;
  bias = pow(2.0,(double) *nexp-1);
  mask1 = ~(~0 << *nexp);
  mask2 = ~(~0 << (31-*nexp));
  mask3 = ~(~0 << 16);
  mask4 = ~(~0 << (15-*nexp));

  for (i = 0; i < *n; i++, lin++, lout++)
  {
     sign = *lin & CSIGNMASK1;    /* sign bit */

     /* Calculate packed unbiased exponent and Cray unbiased exponent */

     pexp = (*lin >> (31-*nexp)) & mask1;
     cexp = pexp - bias + CBIAS;

     /* Calculate packed fraction */

     pfrac = *lin & mask2;

     /* Calculate 64 bit cray word stored as 2 32 bit integers */

     crayword[0] = sign | (cexp << 16) | ((pfrac >> (15-*nexp)) & mask3);
     crayword[1] = (pfrac & mask4) << (17+*nexp);

     /* Calculate 64 bit IEEE word */

#ifdef LITTLE__ENDIAN
     swap_bytes(crayword, 4, 2);
#endif
     c8_to_r8(crayword, lout, 1);
  }

#ifdef LITTLE__ENDIAN
  swap_bytes(in, 4, *n);
#endif
}
#endif
