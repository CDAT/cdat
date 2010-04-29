
/* unwgdos.c is unpack.c file from xconv but with GRIB stuff stripped out */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include "util.h"
#include "vardef.h"

#define TRUE 1
#define FALSE 0

int read_wgdos_header(int *, int, int *, int *, int *, int *);
int unwgdos(int *, int, REAL *, int, REAL);
#ifndef _CRAY
int xpnd(int, int32 *, REAL *, REAL, int, REAL, int, REAL);
int extrin(int32 *, int, int, int, int *, int);
#endif
int bit_test(void *, int);
void move_bits(void *, int, int, void *);

int read_wgdos_header(int *header, int ndata, 
                      int *len, int *isc, int *ix, int *iy)
{
   int nx, ny, n;
   int head[2], swap;

   /* Determine if data needs byte swapping */

   swap = -1;

#if _INT_SIZE == 8
   n = 2;
   head[0] = header[1];
#else
   n = 4;
   head[0] = header[2];
#endif
#if _INT_TYPE == _CRAY8
   ibmi2_to_c8(head, &nx, 1, 0);
   ibmi2_to_c8(head, &ny, 1, 1);
#elif _INT_TYPE == _IEEE8
   ibmi2_to_i8(head, &nx, 1, 0);
   ibmi2_to_i8(head, &ny, 1, 1);
#else
   ibmi2_to_i4(head, &nx, 1, 0);
   ibmi2_to_i4(head, &ny, 1, 1);
#endif
   if (nx*ny == ndata) swap = 0;

   if (swap == -1)
   {
      /* see if data is byte swapped with 4 byte words */

#if _INT_SIZE == 8
      head[0] = header[1];
#else
      head[0] = header[2];
#endif
      swap_bytes(head,4,1);
#if _INT_TYPE == _CRAY8
      ibmi2_to_c8(head, &nx, 1, 0);
      ibmi2_to_c8(head, &ny, 1, 1);
#elif _INT_TYPE == _IEEE8
      ibmi2_to_i8(head, &nx, 1, 0);
      ibmi2_to_i8(head, &ny, 1, 1);
#else
      ibmi2_to_i4(head, &nx, 1, 0);
      ibmi2_to_i4(head, &ny, 1, 1);
#endif
      if (nx*ny == ndata) swap = 4;
   }

   if (swap == -1)
   {
      /* see if data is byte swapped with 8 byte words */

#if _INT_SIZE == 8
      head[0] = header[1];
#else
      head[0] = header[2];
      head[1] = header[3];
#endif
      swap_bytes(head,8,1);
#if _INT_TYPE == _CRAY8
      ibmi2_to_c8(head, &nx, 1, 0);
      ibmi2_to_c8(head, &ny, 1, 1);
#elif _INT_TYPE == _IEEE8
      ibmi2_to_i8(head, &nx, 1, 0);
      ibmi2_to_i8(head, &ny, 1, 1);
#else
      ibmi2_to_i4(head, &nx, 1, 0);
      ibmi2_to_i4(head, &ny, 1, 1);
#endif
      if (nx*ny == ndata) swap = 8;
   }

   if (swap == -1)
   {
      printf("WGDOS data header record mismatch \n");
      return 1;
   }
   else if (swap > 0)
      swap_bytes(header,swap,n*sizeof(*header)/swap);

#if _INT_TYPE == _CRAY8
   ibmi4_to_c8(header, len, 1, 0);
   ibmi4_to_c8(header, isc, 1, 1);
   ibmi2_to_c8(header+1, ix, 1, 0);
   ibmi2_to_c8(header+1, iy, 1, 1);
#elif _INT_TYPE == _IEEE8
   ibmi4_to_i8(header, len, 1, 0);
   ibmi4_to_i8(header, isc, 1, 1);
   ibmi2_to_i8(header+1, ix, 1, 0);
   ibmi2_to_i8(header+1, iy, 1, 1);
#else
   ibmi4_to_i4(header, len, 1);
   ibmi4_to_i4(header+1, isc, 1);
   ibmi2_to_i4(header+2, ix, 1, 0);
   ibmi2_to_i4(header+2, iy, 1, 1);
#endif

   *len = 4*(*len);  /* Convert to bytes */

   return 0;
}

int unwgdos(int *datain, int nin, REAL *dataout, int nout, REAL mdi)
{
   int len, isc, ix, iy;
   REAL prec, base;
   int icx, j;
   int ibit, nop;
   int head[2], swap;
#ifdef _CRAY
   int idum1, idum2, idum3, idum4, idum5, false;
#else
   int32 *datain32;
#endif

   /* Determine if data needs byte swapping */

   swap = -1;

#if _INT_SIZE == 8
   head[0] = datain[1];
#else
   head[0] = datain[2];
#endif
#if _INT_TYPE == _CRAY8
   ibmi2_to_c8(head, &ix, 1, 0);
   ibmi2_to_c8(head, &iy, 1, 1);
#elif _INT_TYPE == _IEEE8
   ibmi2_to_i8(head, &ix, 1, 0);
   ibmi2_to_i8(head, &iy, 1, 1);
#else
   ibmi2_to_i4(head, &ix, 1, 0);
   ibmi2_to_i4(head, &iy, 1, 1);
#endif
   if (ix*iy == nout) swap = 0;

   if (swap == -1)
   {
      /* see if data is byte swapped with 4 byte words */

#if _INT_SIZE == 8
      head[0] = datain[1];
#else
      head[0] = datain[2];
#endif
      swap_bytes(head,4,1);
#if _INT_TYPE == _CRAY8
      ibmi2_to_c8(head, &ix, 1, 0);
      ibmi2_to_c8(head, &iy, 1, 1);
#elif _INT_TYPE == _IEEE8
      ibmi2_to_i8(head, &ix, 1, 0);
      ibmi2_to_i8(head, &iy, 1, 1);
#else
      ibmi2_to_i4(head, &ix, 1, 0);
      ibmi2_to_i4(head, &iy, 1, 1);
#endif
      if (ix*iy == nout) swap = 4;
   }

   if (swap == -1)
   {
      /* see if data is byte swapped with 8 byte words */

#if _INT_SIZE == 8
      head[0] = datain[1];
#else
      head[0] = datain[2];
      head[1] = datain[3];
#endif
      swap_bytes(head,8,1);
#if _INT_TYPE == _CRAY8
      ibmi2_to_c8(head, &ix, 1, 0);
      ibmi2_to_c8(head, &iy, 1, 1);
#elif _INT_TYPE == _IEEE8
      ibmi2_to_i8(head, &ix, 1, 0);
      ibmi2_to_i8(head, &iy, 1, 1);
#else
      ibmi2_to_i4(head, &ix, 1, 0);
      ibmi2_to_i4(head, &iy, 1, 1);
#endif
      if (ix*iy == nout) swap = 8;
   }

   if (swap == -1)
   {
      printf("WGDOS data header record mismatch \n");
      return 1;
   }
   else if (swap > 0)
      swap_bytes(datain,swap,nin*sizeof(*datain)/swap);

#ifdef _CRAY
   false = _btol(0);
   COEX(dataout, &nout, datain, &nin, 
        &idum1, &idum2, &idum3, &idum4, &false, &mdi, &idum5);
#else
   /* Below only works for 32 bit integers, therefore there must be a 
      32 bit integer type */

   datain32 = (int32 *) datain;

   /* Extract scale factor and number of columns and rows from header */

   ibmi4_to_i4(datain32, &len, 1);
   ibmi4_to_i4(datain32+1, &isc, 1);
   ibmi2_to_i4(datain32+2, &ix, 1, 0);
   ibmi2_to_i4(datain32+2, &iy, 1, 1);

   /* Expand compressed data */

   prec = pow(2.0, (double) isc);
   icx = 3;

   for (j=0; j<iy; j++)
   {
      /* Extract base, number of bits per value, number of 32 bit words used */

#if _FLT_TYPE == _IEEE8
      ibmr4_to_r8(datain32+icx, &base, 1, 0);
#else
      ibmr4_to_r4(datain32+icx, &base, 1);
#endif
      ibmi2_to_i4(datain32+icx+1, &ibit, 1, 0);
      ibmi2_to_i4(datain32+icx+1, &nop, 1, 1);

#ifdef LITTLE__ENDIAN
      swap_bytes(datain32+icx+2, 4, nop);
#endif
      xpnd(ix, datain32+icx+2, dataout, prec, ibit, base, nop, mdi);

      icx += nop + 2;
      dataout += ix;
   }
#endif

   return 0;
}

#ifndef _CRAY
int xpnd(int ix, int32 *icomp, REAL *field, REAL prec, 
         int ibit, REAL base, int nop, REAL mdi)
{
   int btmap, btmis, btmin, btzer;
   int jword, jbit, j, iscale;
   int *imap, *imis, *imin, *izer;

   btmap = FALSE;
   btmis = FALSE;
   btmin = FALSE;
   btzer = FALSE;

   /* check if bitmap used for zero values */

   if (ibit >= 128)
   {
      btzer = TRUE;
      btmap = TRUE;
      ibit -= 128;
   }

   /* check if bitmap used for minimum values */

   if (ibit >= 64)
   {
      btmin = TRUE;
      btmap = TRUE;
      ibit -= 64;
   }

   /* check if bitmap used for missing data values */

   if (ibit >= 32)
   {
      btmis = TRUE;
      btmap = TRUE;
      ibit -= 32;
   }

   if (ibit > 32)
   {
      printf("Number of bits used to pack wgdos data = %d must be <= 32 \n",
             ibit);
      return 1;
   }

   if (btmap)
   {
      if ( (imap = malloc (ix*sizeof(int))) == NULL )
      {
          printf("Error unable to allocate memory for imap in xpnd ix = %d \n",
                 ix);
          return 1;
      }

      for (j=0; j<ix; j++)
         imap[j] = 1;
   }

   /* Set start position in icomp */

   jword = 0;
   jbit = 31;

   /* Extract missing data value bitmap */

   if (btmis)
   {
      if ( (imis = malloc (ix*sizeof(int))) == NULL )
      {
          printf("Error unable to allocate memory for imis in xpnd ix = %d \n",
                 ix);
          return 1;
      }

      for (j=0; j<ix; j++)
      {
         if (bit_test(icomp+jword, jbit))
         {
            imis[j] = 1;
            imap[j] = 0;
         }
         else
            imis[j] = 0;

         if (jbit > 0)
            jbit--;
         else
         {
            jbit = 31;
            jword++;
         }
      }
   }

   /* Extract minimum value bitmap */

   if (btmin)
   {
      if ( (imin = malloc (ix*sizeof(int))) == NULL )
      {
          printf("Error unable to allocate memory for imin in xpnd ix = %d \n",
                 ix);
          return 1;
      }

      for (j=0; j<ix; j++)
      {
         if (bit_test(icomp+jword, jbit))
         {
            imin[j] = 1;
            imap[j] = 0;
         }
         else
            imin[j] = 0;

         if (jbit > 0)
            jbit--;
         else
         {
            jbit = 31;
            jword++;
         }
      }
   }

   /* Extract zero value bitmap */

   if (btzer)
   {
      if ( (izer = malloc (ix*sizeof(int))) == NULL )
      {
          printf("Error unable to allocate memory for izer in xpnd ix = %d \n",
                 ix);
          return 1;
      }

      for (j=0; j<ix; j++)
      {
         if (bit_test(icomp+jword, jbit))
            izer[j] = 0;
         else
         {
            izer[j] = 1;
            imap[j] = 0;
         }

         if (jbit > 0)
            jbit--;
         else
         {
            jbit = 31;
            jword++;
         }
      }
   }

   /* If bitmap used reset pointers to beginning of 32 bit boundary */

   if (btmap && jbit != 31)
   {
      jbit = 31;
      jword++;
   }

   if (ibit > 0)
   {
      /* Unpack scaled values */

      for (j=0; j<ix; j++)
      {
         if (btmap && imap[j] == 0) continue;

         extrin(icomp+jword, 4, jbit, ibit, &iscale, 0);
         field[j] = base + iscale*prec;

         jbit -= ibit;
         if (jbit < 0)
         {
            jword++;
            jbit += 32;
         }
      }

      /* If minimum value bitmap fill in field with base */

      if (btmin)
      {
         for (j=0; j<ix; j++) 
         {
            if (imin[j] == 1) field[j] = base;
         }
      }
   }
   else if (ibit == 0)
   {
      /* All points in row have same value */

      for (j=0; j<ix; j++) field[j] = base;
   }

   /* If missing data value bitmap fill in field with mdi */

   if (btmis)
   {
      for (j=0; j<ix; j++) 
      {
         if (imis[j] == 1) field[j] = mdi;
      }
   }

   /* If zero value bitmap fill in field with 0.0 */

   if (btzer)
   {
      for (j=0; j<ix; j++) 
      {
         if (izer[j] == 1) field[j] = 0.0;
      }
   }

   if (btmap) free(imap);
   if (btmis) free(imis);
   if (btmin) free(imin);
   if (btzer) free(izer);

   return 0;
}

int extrin(int32 *icomp, int iword, int istart, int nbit, int *inum, int isign)
{
   if (isign == 0)
   {
      move_bits(icomp, istart, nbit, inum);
   }
   else if (isign == 1)
   {
      /* move integer without sign bit */

      move_bits(icomp, istart-1, nbit-1, inum);

      /* move sign bit */

      *inum = (*icomp << (31-istart)) & (~0 << 31);

      /* set undefined if inum negative */

      if (*inum < 0) *inum = (~0 << (nbit-1)) | *inum;
   }
   else if (isign == 2)
   {
      /* move integer without sign bit */

      move_bits(icomp, istart-1, nbit-1, inum);

      if (bit_test(icomp, istart)) *inum = -*inum;
   }

   return 0;
}

int bit_test(void *iword, int ibit)
{
   unsigned char i;
   unsigned int ui;

   ui = *(unsigned int *) iword;

   i = (ui >> ibit) & ~(~0 << 1);

   if (i == 1)
      return TRUE;
   else
      return FALSE;
}

/*
 *  Move nbits from 32 bit word1 starting at start1 into 32 bit word2.
 *  0 =< nbits <= 32, bits can cross into word1+1.
 */

void move_bits(void *word1, int start1, int nbits, void *word2)
{
   uint32 *ui1, *ui2, temp1, temp2;

   ui1 = (uint32 *) word1;
   ui2 = (uint32 *) word2;

   if (start1+1-nbits >= 0)
   {
      /* move bits within one word */

      ui2[0] = (ui1[0] >> (start1+1-nbits)) & ~(~0 << nbits);
   }
   else
   {
      /* move bits within two words */

      temp1 = (ui1[0] << (nbits-start1-1)) & ~(~0 << nbits);
      temp2 = (ui1[1] >> (32+start1+1-nbits)) & ~(~0 << (nbits-start1-1));
      ui2[0] = temp1 | temp2;
   }
}
#endif
