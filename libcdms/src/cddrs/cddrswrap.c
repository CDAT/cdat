/* -*-Mode: C;-*-
 * Module:      DRS Fortran-to-C interface
 *
 * Copyright:	1994, Regents of the University of California
 *		This software may not be distributed to others without
 *		permission of the author.
 *
 * Author:      Bob Drach, Lawrence Livermore National Laboratory
 *              drach@llnl.gov
 *
 * Version:     $Id$
 *
 * Revision History:
 *
 * $Log: cddrswrap.c,v $
 * Revision 1.1.1.1  1997/12/09 18:57:40  drach
 * Copied from cirrus
 *
 * Revision 1.4  1996/09/09  18:17:33  drach
 * - (cddrs.c) alias 'comments' and 'source', 'title' and 'long_name'
 * - (cddrswrap.c) include math.h
 *
 * Revision 1.3  1996/04/04  18:21:27  drach
 * - Separate CRAY version of getrge2
 *
 * Revision 1.2  1995/02/15  20:56:28  drach
 * - Added cw_getname
 *
 * Revision 1.1  1995/01/30  17:58:58  drach
 * Initial version
 *
 *
 */

#include <math.h>
#include "cddrs.h"
#include "cfortran.h"
				/* Force an underscore to be appended on Absoft linux */
#ifdef __linux_absoft
#undef fcallsc
#define fcallsc(UN,LN) append_fcallsc(_,_,UN,LN)
#endif

FCALLSCFUN5(INT,cw_aslun,CW_ASLUN,cw_aslun,INT,STRING,INT,STRING,INT)

FCALLSCFUN1(INT,cw_cllun,CW_CLLUN,cw_cllun,INT)

FCALLSCFUN0(INT,cw_cluvdb,CW_CLUVDB,cw_cluvdb)

FCALLSCFUN1(LOGICAL,cw_drstest,CW_DRSTEST,cw_drstest,INT)

FCALLSCFUN9(INT,cw_getedim,CW_GETEDIM,cw_getedim,INT,PSTRING,PSTRING,PSTRING,PSTRING,PINT,PINT,PFLOAT,PFLOAT)

FCALLSCFUN3(INT,cw_getdat,CW_GETDAT,cw_getdat,INT,PVOID,INT)

FCALLSCFUN9(INT,cw_getcdim,CW_GETCDIM,cw_getcdim,INT,PSTRING,PSTRING,PSTRING,PSTRING,PINT,INT,FLOATV,PINT)

FCALLSCFUN2(INT,cw_getelemd,CW_GETELEMD,cw_getelemd,PINT,PINT)

FCALLSCFUN8(INT,cw_getname,CW_GETNAME,cw_getname,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PINT)

FCALLSCFUN1(INT,cw_getnd,CW_GETND,cw_getnd,PINT)

#ifdef CRAY
FCALLSCFUN8(INT,cw_getrge2,CW_GETRGE2,cw_getrge2,INT,INT,FLOAT,FLOAT,PINT,PINT,PFLOAT,PFLOAT)
#else
FCALLSCFUN8(INT,cw_getrge2,CW_GETRGE2,cw_getrge2,INT,INT,DOUBLE,DOUBLE,PINT,PINT,PFLOAT,PFLOAT)
#endif

FCALLSCFUN8(INT,cw_getslab,CW_GETSLAB,cw_getslab,INT,INT,INTV,FLOATV,FLOATV,FLOATV,PVOID,INTV)

FCALLSCFUN2(INT,cw_inqdict,CW_INQDICT,cw_inqdict,INT,INT)

FCALLSCFUN4(INT,cw_inqlun,CW_INQLUN,cw_inqlun,INT,PSTRING,PINT,PFLOAT)

FCALLSCFUN1(INT,cw_majority,CW_MAJORITY,cw_majority,INT)

					     /* Handle float/double mismatch;
					      * recognize fabs(x)>1.e20 as float null
					      */
int cwf_setdim(int n,char* dna,char* dun,int idim,float df,float dl){
	double dfx, dlx;

	dfx = (fabs((double)df) < (CW_FLOAT_NULL-CW_FLOAT_DELTA)) ? (double)df : CW_FLOAT_NULL;
	dlx = (fabs((double)dl) < (CW_FLOAT_NULL-CW_FLOAT_DELTA)) ? (double)dl : CW_FLOAT_NULL;
	return cw_setdim(n,dna,dun,idim,dfx,dlx);
}
FCALLSCFUN6(INT,cwf_setdim,CW_SETDIM,cw_setdim,INT,STRING,STRING,INT,FLOAT,FLOAT)

FCALLSCFUN2(INT,cw_seterr,CW_SETERR,cw_seterr,INT,INT)

FCALLSCFUN5(INT,cw_setname,CW_SETNAME,cw_setname,STRING,STRING,STRING,STRING,STRING)

FCALLSCFUN2(INT,cw_putdat,CW_PUTDAT,cw_putdat,INT,PVOID)

FCALLSCFUN2(INT,cw_putdic,CW_PUTDIC,cw_putdic,INT,INT)

FCALLSCFUN5(INT,cw_putvdim,CW_PUTVDIM,cw_putvdim,INT,INT,FLOATV,PINT,PINT)

FCALLSCFUN2(INT,cw_setdate,CW_SETDATE,cw_setdate,STRING,STRING)

FCALLSCFUN1(INT,cw_setrep,CW_SETREP,cw_setrep,INT)

					     /* Handle float/double mismatch;
					      * recognize fabs(x)>1.e20 as float null
					      */
int cwf_setvdim(int n,char* dso,char* dna,char* dti,char* dun,float df,float dl){
	double dfx, dlx;

	dfx = (fabs((double)df) < (CW_FLOAT_NULL-CW_FLOAT_DELTA)) ? (double)df : CW_FLOAT_NULL;
	dlx = (fabs((double)dl) < (CW_FLOAT_NULL-CW_FLOAT_DELTA)) ? (double)dl : CW_FLOAT_NULL;
	return cw_setvdim(n,dso,dna,dti,dun,dfx,dlx);
}
FCALLSCFUN7(INT,cwf_setvdim,CW_SETVDIM,cw_setvdim,INT,STRING,STRING,STRING,STRING,FLOAT,FLOAT)
