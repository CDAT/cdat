/*
 *
 *    Copyright (C) 2004-2006 NERC DataGrid
 *    This software may be distributed under the terms of the
 *    CCLRC Licence for CCLRC Software
 * <CDATDIR>/External_License/CCLRC_CDAT_License.txt 
 *
 */
/* This is a wrapper function which interfaces between CDUNIFPP and
 * UNWGDOS.  (It should be the only function which needs both sets
 * of header files.)
 */

#ifdef HAVE_PP
#include "cdunifpp.h"
#include "crayio.h"

/* prototype unwgdos */
int unwgdos(int *, int, REAL *, int, REAL);

int pp_unwgdos_wrap(const void *packed_data, int nint, 
		    void *data, long datalen,
		    Freal mdi, PPlist *heaplist)
{
  int ipt;
  void *tmp;

      /* unwgdos routine as coded writes output which is native size of
       * fortran REAL.
       *
       * Don't want to muck around with unwgdos code, so in the event that
       * this cdunifpp has been compiled using Freal which is not the same
       * type as REAL, allocate another temporary array and copy across.
       */
    
      /* NB for output size use datalen, which does not include disk padding.
       * It is tested inside unwgdos that (nx * ny = output size), so it is
       * important to specify the exactly correct size.
       */
  
  if (wordsize == sizeof(REAL)) {
    CKI(   unwgdos(packed_data, nint, data, datalen, mdi)   );
  } else {
    CKP(  tmp=pp_malloc(datalen * sizeof(REAL), heaplist)  );
    CKI(   unwgdos(packed_data, nint, tmp, datalen, mdi)   );
    for (ipt=0; ipt<datalen; ipt++)
      *(((Freal*) (data)) + ipt) = *(((REAL*) (tmp)) + ipt);
    CKI(  pp_free(tmp,heaplist)  );
  }      

  return 0;

  ERRBLKI("pp_unwgdos_wrap");
}

#endif
