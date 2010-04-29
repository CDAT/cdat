/* -*-Mode: C;-*-
 * Module:      CDMS Fortran time conversion and arithmetic routines
 *
 * Copyright:	1996, Regents of the University of California
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
 * $Log: fcdTimeConv.c,v $
 * Revision 1.2  1996/09/09  18:28:34  drach
 * - Cleaned up minor compilation warnings
 *
 * Revision 1.1  1996/04/04  19:02:28  drach
 * - Initial version
 *
 *
 */

#include "cdmsint.h"
#include "cfortran.h"

/* Map C comptime struct to Fortran args
 */

#ifdef CRAY
void cdfChar2Comp(int timetype, char* chartime, int* year, int* month, int* day, float* hour){
#else
void cdfChar2Comp(int timetype, char* chartime, int* year, int* month, int* day, double* hour){
#endif
	cdCompTime comptime;

	cdChar2Comp(timetype, chartime, &comptime);
	*year = comptime.year;
	*month = comptime.month;
	*day = comptime.day;
	*hour = comptime.hour;

	return;
}

#ifdef CRAY
void cdfComp2Char(int timetype, int year, int month, int day, float hour, char* time){
#else
void cdfComp2Char(int timetype, int year, int month, int day, double hour, char* time){
#endif
	cdCompTime comptime;

	comptime.year = year;
	comptime.month = month;
	comptime.day = day;
	comptime.hour = hour;
	cdComp2Char(timetype, comptime, time);

	return;
}

#ifdef CRAY
void cdfComp2Rel(int timetype, int year, int month, int day, double hour, char* relunits, float* reltime){
	double dreltime;
#else
void cdfComp2Rel(int timetype, int year, int month, int day, double hour, char* relunits, double* reltime){
#endif
	cdCompTime comptime;

	comptime.year = year;
	comptime.month = month;
	comptime.day = day;
	comptime.hour = hour;
#ifdef CRAY
	cdComp2Rel(timetype, comptime, relunits, &dreltime);
	*reltime=(float)dreltime;
#else
	cdComp2Rel(timetype, comptime, relunits, reltime);
#endif

	return;
}

#ifdef CRAY
void cdfRel2Comp(int timetype, char* relunits, float reltime, int* year, int* month, int* day, double* hour){
#else
void cdfRel2Comp(int timetype, char* relunits, double reltime, int* year, int* month, int* day, double* hour){
#endif
	cdCompTime comptime;

	cdRel2Comp(timetype, relunits, reltime, &comptime);
	
	*year = comptime.year;
	*month = comptime.month;
	*day = comptime.day;
	*hour = comptime.hour;

	return;
}

#ifdef CRAY
FCALLSCSUB6(cdfChar2Comp,FCDCHAR2COMP,fcdchar2comp,INT,STRING,PINT,PINT,PINT,PFLOAT)
FCALLSCSUB4(cdChar2Rel,FCDCHAR2REL,fcdchar2rel,INT,STRING,STRING,PFLOAT)
FCALLSCSUB6(cdfComp2Char,FCDCOMP2CHAR,fcdcomp2char,INT,INT,INT,INT,FLOAT,PSTRING)
FCALLSCSUB7(cdfComp2Rel,FCDCOMP2REL,fcdcomp2rel,INT,INT,INT,INT,FLOAT,STRING,PFLOAT)
FCALLSCSUB4(cdRel2Char,FCDREL2CHAR,fcdrel2char,INT,STRING,FLOAT,PSTRING)
FCALLSCSUB7(cdfRel2Comp,FCDREL2COMP,fcdrel2comp,INT,STRING,FLOAT,PINT,PINT,PINT,PFLOAT)
FCALLSCSUB5(cdRel2Rel,FCDREL2REL,fcdrel2rel,INT,STRING,FLOAT,STRING,PFLOAT)
#else
FCALLSCSUB6(cdfChar2Comp,FCDCHAR2COMP,fcdchar2comp,INT,STRING,PINT,PINT,PINT,PDOUBLE)
FCALLSCSUB4(cdChar2Rel,FCDCHAR2REL,fcdchar2rel,INT,STRING,STRING,PDOUBLE)
FCALLSCSUB6(cdfComp2Char,FCDCOMP2CHAR,fcdcomp2char,INT,INT,INT,INT,DOUBLE,PSTRING)
FCALLSCSUB7(cdfComp2Rel,FCDCOMP2REL,fcdcomp2rel,INT,INT,INT,INT,DOUBLE,STRING,PDOUBLE)
FCALLSCSUB4(cdRel2Char,FCDREL2CHAR,fcdrel2char,INT,STRING,DOUBLE,PSTRING)
FCALLSCSUB7(cdfRel2Comp,FCDREL2COMP,fcdrel2comp,INT,STRING,DOUBLE,PINT,PINT,PINT,PDOUBLE)
FCALLSCSUB5(cdRel2Rel,FCDREL2REL,fcdrel2rel,INT,STRING,DOUBLE,STRING,PDOUBLE)
#endif
