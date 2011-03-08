/* gasdf.h - header info for SDF functionality */
/* id: netcdf_io.h,v 1.2 1995/05/31 23:53:44 jac (Julia Collins) */
/* Revision 1.3 1997/07/21 09:51:00 hoop */
/* added gasdfparms struct */
/* Revision 1.2  1995/05/31  23:53:44  jac  */
/* add missing definition */
/*  */
/* Revision 1.1  1995/05/02  20:23:12  jac */
/* Initial revision */
/* */

#define CALENDAR "calendar"
#define CAL365 "365_day_year"
#define ALTCAL365 "noleap"

#define MISSING		-1

/* Missing data definitions  */
#define BFILL		FILL_BYTE
#define BMISS		-BFILL
#define SFILL		FILL_SHORT
#define SMISS		-SFILL
#define LFILL		FILL_LONG
#define LMISS		-LFILL
#define FFILL		FILL_FLOAT
#define FMISS		-FFILL		
#define DFILL		FILL_DOUBLE
#define DMISS		-DFILL

/* temporary flags to read time according to new or old standards */
#define CDC			0
#define COOP			1

/* default scale and offset values for unpacked files */
#define B_SCALE		(char) 	 1
#define S_SCALE		(short)  1
#define L_SCALE		(long) 	 1
#define F_SCALE		(float)  1.0
#define D_SCALE		(double) 1.0 
#define B_OFFSET	(char) 	 0
#define S_OFFSET	(short)  0
#define L_OFFSET	(long) 	 0
#define F_OFFSET	(float)  0.0
#define D_OFFSET	(double) 0.0 

/* Missing data definitions  */
#define BFILL		FILL_BYTE
#define BMISS		-BFILL
#define SFILL		FILL_SHORT
#define SMISS		-SFILL
#define LFILL		FILL_LONG
#define LMISS		-LFILL
#define FFILL		FILL_FLOAT
#define FMISS		-FFILL		
#define DFILL		FILL_DOUBLE
#define DMISS		-DFILL


