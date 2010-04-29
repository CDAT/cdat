/* -*-Mode: C;-*-
 * Module:      cddrsint.h - internal include file for DRS wrapper routines
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
 * $Log: cddrsint.h,v $
 * Revision 1.7  1995/06/09  22:44:16  drach
 * Added extensions for string length and dimension types
 *
 * Revision 1.6  1995/03/09  00:35:16  drach
 * Added netCDF, upgraded cureadarray with casting, user-specified indices
 *
 * Revision 1.5  1995/01/21  00:53:18  drach
 * - Recognize "" as a string null, since cfortran.h trims " "
 *
 * Revision 1.4  1995/01/13  01:03:57  drach
 * Added vdims fields:
 * - values (dimension cache), isCycle, and cycle
 * - Added cw_lookup_cycle, cw_dimget, cw_unif_to_drs_datatype
 *
 * Revision 1.3  1994/12/17  00:42:51  drach
 * - Define CW_IS_FLOAT_NULL
 *
 * Revision 1.2  1994/12/16  00:45:23  drach
 * - Added string match macro
 * - Added isset
 * - Added several function declarations
 *
 * Revision 1.1  1994/12/14  02:33:53  drach
 * - Added to CVS
 *
 *
 */

#ifndef __cddrsint_h
#define __cddrsint_h

#include <string.h>
#include <math.h>
#include "drscdf.h"
#include "cddrs.h"

					     /* Set and null-terminate a VDB string
					      * Note that either NULL, "" or " " are treated
					      * as null strings.
					      */
#define VDB_STRING_SET(d,s,n) {strncpy((d),((s && strcmp(s,"")) ? s : CW_STRING_NULL),(n));(d)[(n)-1]='\0';cw_strtrim((d));}
#define CW_STRING_MATCH(s,t) ((!strcmp((s),CW_STRING_NULL))||(!strcmp((t),CW_STRING_NULL))||(!strcmp((s),(t)))) 
#define CW_IS_FLOAT_NULL(x) (fabs((x)-CW_FLOAT_NULL)<=CW_FLOAT_DELTA)

typedef struct {
	int filelu;			     /* Dictionary file logical unit */
	char source[CU_MAX_NAME];	     /* DRS source string */
	char name[CU_MAX_NAME];		     /* DRS variable name */
	char title[CU_MAX_NAME];	     /* DRS variable title */
	char units[CU_MAX_NAME];	     /* DRS variable units */
	char date[CU_MAX_NAME];		     /* DRS date written */
	char time[CU_MAX_NAME];		     /* DRS time written */
	char type[CU_MAX_NAME];		     /* type string, e.g., 'R*4' */
	int ndims;			     /* number of dimensions */
} CwVar;

typedef struct {
	char source[CU_MAX_NAME];	     /* source string */
	char name[CU_MAX_NAME];		     /* DRS dimension name */
	char title[CU_MAX_NAME];	     /* DRS dimension title */
	char units[CU_MAX_NAME];	     /* DRS dimension units */
	int type;			     /* IDRS_EQUALLY_SPACED | IDRS_UNEQUALLY_SPACED | IDRS_IMPLICIT_VECTOR */
	double dfreq;			     /* first value of requested dimension range */
	double dlreq;			     /* first value of requested dimension range */
	double dfactual;		     /* first value of range as received */
	double dlactual;		     /* first value of range as received */
	int len;			     /* number of values in the dimension */
	int reqlen;			     /* number of values requested */
	int isset;			     /* True iff cw_setdim or cw_setvdim has been called */
					     /* since the last cw_cllun call.*/
	double* values;			     /* Actual values, or double** 0 if not cached */
	int isCycle;			     /* 1 iff dimension is cyclical */
	double cycle;			     /* dimension period, if isCycle */
} CwDim;

extern int cw_geterr PROTO((void));
extern void cw_error PROTO((char *fmt, ...));
extern void cw_lookup PROTO((double tab[], long n, double x, long *k));
extern int cw_lookup_cycle PROTO((double tab[], long n, double x, double cycle, CwRoundPolicy policy, double delta, long *k, long *icycle));
extern int cw_lookup_with_policy PROTO((double tab[], long n, double x, CwRoundPolicy policy, double delta, long *k));
extern int cw_dimget PROTO((int fileid, int dimid, double** values, long *dimlen));
extern int cw_dimmap PROTO((int fileid, int dimid, double df, double dl, CwRoundPolicy policy, double delta, int isCycle, double cycle, double **dp, long *idf, long *idl, double *xdf, double *xdl));
extern int cw_varid PROTO((int fileid, const char* source, const char* name, const char* title, const char* units));
extern int cw_string_attget PROTO((int fileid, int varid, const char* attname, char* value));
extern CuType cw_drs_to_unif_datatype PROTO((const char* drstype));
extern int cw_unif_to_drs_datatype PROTO((CuType dtype, char* drstype));
extern int cw_unif_to_drs_enumtype PROTO((CuType dtype, int* enumtype));
extern void cw_add_extension PROTO((const char* filename, const char* extension, char* result));
extern char* cw_strtrim PROTO((char* s));
extern int cw_dimension_varid PROTO((int fileid, int dimid, char* dname));

#endif
