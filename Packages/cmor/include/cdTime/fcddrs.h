c -*-Mode: Fortran;-*-
c  Module:      fcddrs.h - Fortran include file for DRS wrapper routines
c 
c  Copyright:	1994, Regents of the University of California
c 		This software may not be distributed to others without
c 		permission of the author.
c 
c  Author:      Bob Drach, Lawrence Livermore National Laboratory
c               drach@llnl.gov
c 
c  Version:     fcddrs.h,v 1.3 1995/03/31 06:53:25 drach Exp
c 
c  Revision History:
c 
c  fcddrs.h,v
c Revision 1.3  1995/03/31  06:53:25  drach
c Ported to HP9000
c
c Revision 1.2  1995/02/15  20:55:29  drach
c - Added IDRS_VECTOR as synonym for IDRS_UNEQUALLY_SPACED
c
c Revision 1.1  1995/01/30  17:50:49  drach
c - Initial version
c
c 
c

      real CW_FLOAT_NULL
      integer CW_INT_NULL
      integer CW_C_MAJORITY, CW_FORTRAN_MAJORITY
      parameter(CW_FLOAT_NULL=1.0e20)
      parameter(CW_INT_NULL=0)
      parameter(CW_C_MAJORITY = 1, CW_FORTRAN_MAJORITY = 2)
      parameter(IDRS_VECTOR = 2)

c     Extended element types
      parameter(IDRS_I1 = 7
     $     ,IDRS_I2 = 8
     $     ,IDRS_IEEE_R8 = 9
     $     ,IDRS_CRAY_R16 = 10
     $     ,IDRS_IEEE_R16 = 11
     $     )

#if (defined(hpux) || defined(ibm)) || defined(__linux_absoft)

#ifndef NO_DECLARE

      integer cw_aslun_
      integer cw_cllun_
      integer cw_cluvdb_
      logical cw_drstest_
      integer cw_getdat_
      integer cw_getcdim_
      integer cw_getcdimD_
      integer cw_getedim_
      integer cw_getedimD_
      integer cw_getelemd_
      integer cw_getname_
      integer cw_getnd_
      integer cw_getrge2_
      integer cw_getrge2D_
      integer cw_getslab_
      integer cw_inqdict_
      integer cw_inqlun_
      integer cw_majority_
      integer cw_setdim_
      integer cw_seterr_
      integer cw_setname_
      integer cw_setvdim_
      integer cw_putdat_
      integer cw_putdic_
      integer cw_putvdim_
      integer cw_setdate_
      integer cw_setrep_

#endif

#ifndef _fcddrs_h
#define _fcddrs_h

#define cw_aslun cw_aslun_
#define cw_cllun cw_cllun_
#define cw_cluvdb cw_cluvdb_
#define cw_drstest cw_drstest_
#define cw_getdat cw_getdat_
#define cw_getcdim cw_getcdim_
#define cw_getcdimD cw_getcdimD_
#define cw_getedim cw_getedim_
#define cw_getedimD cw_getedimD_
#define cw_getelemd cw_getelemd_
#define cw_getname cw_getname_
#define cw_getnd cw_getnd_
#define cw_getrge2 cw_getrge2_
#define cw_getrge2D cw_getrge2D_
#define cw_getslab cw_getslab_
#define cw_inqdict cw_inqdict_
#define cw_inqlun cw_inqlun_
#define cw_majority cw_majority_
#define cw_setdim cw_setdim_
#define cw_seterr cw_seterr_
#define cw_setname cw_setname_
#define cw_setvdim cw_setvdim_
#define cw_putdat cw_putdat_
#define cw_putdic cw_putdic_
#define cw_putvdim cw_putvdim_
#define cw_setdate cw_setdate_
#define cw_setrep cw_setrep_
#endif

#else

#ifndef NO_DECLARE

      integer cw_aslun
      integer cw_cllun
      integer cw_cluvdb
      logical cw_drstest
      integer cw_getdat
      integer cw_getcdim
      integer cw_getcdimD
      integer cw_getedim
      integer cw_getedimD
      integer cw_getelemd
      integer cw_getname
      integer cw_getnd
      integer cw_getrge2
      integer cw_getrge2D
      integer cw_getslab
      integer cw_inqdict
      integer cw_inqlun
      integer cw_majority
      integer cw_setdim
      integer cw_seterr
      integer cw_setname
      integer cw_setvdim
      integer cw_putdat
      integer cw_putdic
      integer cw_putvdim
      integer cw_setdate
      integer cw_setrep

#endif

#endif

#ifdef CDCOMPAT
#if (defined(hpux) || defined(ibm)) || defined(__linux_absoft)

#ifndef _fcddrs_h
#define _fcddrs_h

#define aslun cw_aslun_
#define cllun cw_cllun_
#define cluvdb cw_cluvdb_
#define drstest cw_drstest_
#define getdat cw_getdat_
#define getcdim cw_getcdim_
#define getcdimD cw_getcdimD_
#define getedim cw_getedim_
#define getedimD cw_getedimD_
#define getelemd cw_getelemd_
#define getname cw_getname_
#define getnd cw_getnd_
#define getrge2 cw_getrge2_
#define getrge2D cw_getrge2D_
#define getslab cw_getslab_
#define inqdict cw_inqdict_
#define inqlun cw_inqlun_
#define majority cw_majority_
#define setdim cw_setdim_
#define seterr cw_seterr_
#define setname cw_setname_
#define setvdim cw_setvdim_
#define putdat cw_putdat_
#define putdic cw_putdic_
#define putvdim cw_putvdim_
#define setdate cw_setdate_
#define setrep cw_setrep_

#endif

#else

#ifndef _fcddrs_h
#define _fcddrs_h

#define aslun cw_aslun
#define cllun cw_cllun
#define cluvdb cw_cluvdb
#define drstest cw_drstest
#define getdat cw_getdat
#define getcdim cw_getcdim
#define getcdimD cw_getcdimD
#define getedim cw_getedim
#define getedimD cw_getedimD
#define getelemd cw_getelemd
#define getname cw_getname
#define getnd cw_getnd
#define getrge2 cw_getrge2
#define getrge2D cw_getrge2D
#define getslab cw_getslab
#define inqdict cw_inqdict
#define inqlun cw_inqlun
#define majority cw_majority
#define setdim cw_setdim
#define seterr cw_seterr
#define setname cw_setname
#define setvdim cw_setvdim
#define putdat cw_putdat
#define putdic cw_putdic
#define putvdim cw_putvdim
#define setdate cw_setdate
#define setrep cw_setrep

#endif

#endif

#endif
