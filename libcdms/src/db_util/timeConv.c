/*
 * Module:      Basic time format conversion routines:
 *              Cdc2e, Cdc2h, Cde2c, Cde2h, Cdh2c, Cdh2e
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
 * $Log: timeConv.c,v $
 * Revision 1.2  1998/02/20 00:24:49  drach
 * - Added multi-variable file spanning in QL
 * - Accounted for goofy floor function on J90, in timeConv.c
 *
 * Revision 1.1.1.1  1997/12/09 18:57:40  drach
 * Copied from cirrus
 *
 * Revision 1.4  1996/02/21  23:56:50  drach
 * - Overlayed cdtime routines in cdTimeConv.c:
 * - Added seconds, julian calendar, changed include to cdmsint.h for old
 *   time routines in timeArith.c and timeConv.c
 *
 * Revision 1.3  1994/08/12  19:03:30  drach
 * Added CdDaysInMonth function so that CdSetTime can handle last-day-of-month flag.
 *
 * Revision 1.2  1994/07/20  00:01:17  drach
 * - Changed unsigned struct values to signed
 * - Added baseYear argument to Cdc2e
 *
 * Revision 1.1  1994/07/13  18:29:44  drach
 * Initial version
 *
 *
 */

#include <math.h>
#include <stdio.h>
#include <cdmsint.h>

#define ISLEAP(year,timeType)	(((timeType) & CdHasLeap) && (!((year) % 4) && (((timeType) & CdJulianType) || (((year) % 100) || !((year) % 400)))))

static int mon_day_cnt[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
static int days_sum[12] = {0,31,59,90,120,151,181,212,243,273,304,334};

/* Compute month and day from year and day-of-year.
 *
 *	Input:
 *		doy	     (int)  (day-of-year)
 *		date->year   (long)  (year since 0 BC)
 *              date->timeType (CdTimetype) (time type)
 *              date->baseYear   base year for relative times
 *	Output: 
 *		date->month  (short)  (month in year) 
 *		date->day    (short)  (day in month)
 *
 * 
 * Derived from NRL NEONS V3.6.
 */

void
CdMonthDay(int *doy, CdTime *date)
{
	int i;				/* month counter */
	int idoy;			/* day of year counter */
	long year;

	if ((idoy = *doy) < 1) {
		date->month = 0;
		date->day   = 0;
		return;
	}

	if(!(date->timeType & CdChronCal))   /* Ignore year for Clim calendar */
		year = 0;
	else if(!(date->timeType & CdBase1970))	/* year is offset from base for relative time */
		year = date->baseYear + date->year;
	else
		year = date->year;

	if (ISLEAP(year,date->timeType)) {
		mon_day_cnt[1] = 29;
	} else {
		mon_day_cnt[1] = 28;
	}
	date->month	= 0;
	for (i = 0; i < 12; i++) {
		(date->month)++;
		date->day	= idoy;
		if ((idoy -= ((date->timeType & Cd365) ? (mon_day_cnt[date->month-1]) : 30)) <= 0) {
			return;
		}
	}
	return;
}


/* Compute number of days in a month
 *
 *	Input:
 *		date->year       (long)  (year since 0 BC)
 *		date->month      (short)  (month in year) 
 *              date->timeType   (CdTimetype) (time type)
 *              date->baseYear   base year for relative times
 * 
 *	Output: 
 *		days    (short)  (number of days in month)
 *
 */

void
CdDaysInMonth(CdTime *date, int *days)
{
	long year;

	if(!(date->timeType & CdChronCal))   /* Ignore year for Clim calendar */
		year = 0;
	else if(!(date->timeType & CdBase1970))	/* year is offset from base for relative time */
		year = date->baseYear + date->year;
	else
		year = date->year;

	if (ISLEAP(year,date->timeType)) {
		mon_day_cnt[1] = 29;
	} else {
		mon_day_cnt[1] = 28;
	}

	*days = (date->timeType & Cd365) ? (mon_day_cnt[date->month-1]) : 30;

	return;
}

/* Compute day-of-year from year, month and day
 * 
 *	Input:
 *		date->year  (long)  (year since 0 BC)
 *		date->month (short)  (month in year)
 *		date->day   (short)  (day in month)
 *              date->baseYear   base year for relative times
 *	Output: doy         (int)  (day-of-year)
 * 
 * Derived from NRL NEONS V3.6
 */

void
CdDayOfYear(CdTime *date, int *doy)
{
	int leap_add = 0;		/* add 1 day if leap year */
	int month;			/* month */
	long year;

   	month	= date->month;
	if (month < 1 || month > 12) {
		cdError( "Day-of-year error; month: %d\n", month);
		month = 1;	
	}

	if(!(date->timeType & CdChronCal))   /* Ignore year for Clim calendar */
		year = 0;
	else if(!(date->timeType & CdBase1970))	/* year is offset from base for relative time */
		year = date->baseYear + date->year;
	else
		year = date->year;

	if (ISLEAP(year,date->timeType) && month > 2) leap_add = 1;
	*doy 	 = ((date->timeType & Cd365) ? (days_sum[month-1]) : 30*(month-1)) + date->day + leap_add;
	return;
}
/* Convert human time to epochal time (hours since 00 jan 1, 1970)
 * 
 * Input: htime = human time representation
 * 
 * Output: etime = epochal time representation
 * 
 * Derived from NRL Neons V3.6
 */
void
Cdh2e(CdTime *htime, double *etime)
{
	long 	ytemp, year;			/* temporary year holder */
	int	day_cnt;		/* count of days */
	int 	doy;			/* day of year */
	long    baseYear;		     /* base year for epochal time */
	int     daysInLeapYear;		     /* number of days in a leap year */
	int     daysInYear;		     /* days in non-leap year */
	extern void CdDayOfYear(CdTime *date, int *doy);

	CdDayOfYear(htime,&doy);
	
	day_cnt	= 0;

	baseYear = ((htime->timeType) & CdBase1970) ? 1970 : htime->baseYear;
	year = ((htime->timeType) & CdBase1970) ? htime->year : (htime->year + htime->baseYear);
	if(!((htime->timeType) & CdChronCal)) baseYear = year = 0;	/* set year and baseYear to 0 for Clim */
	daysInLeapYear = ((htime->timeType) & Cd365) ? 366 : 360;
	daysInYear = ((htime->timeType) & Cd365) ? 365 : 360;
	
	if (year > baseYear) {
		for (ytemp = year - 1; ytemp >= baseYear; ytemp--) {
			day_cnt += ISLEAP(ytemp,htime->timeType) ? daysInLeapYear : daysInYear;
		}
	} else if (year < baseYear) {
		for (ytemp = year; ytemp < baseYear; ytemp++) {
			day_cnt -= ISLEAP(ytemp,htime->timeType) ? daysInLeapYear : daysInYear;
		}
	}	
	*etime	= (double) (day_cnt + doy - 1) * 24. + htime->hour;
        return;
}
/* Convert epochal time (hours since 00 jan 1, 1970)
 *   to human time (structured)
 * 
 * Input: 
 *   etime = epochal time representation
 *   timeType = time type (e.g., CdChron, CdClim, etc.) as defined in cdms.h
 *   baseYear = base real, used for relative time types only
 * 
 * Output: htime = human (structured) time representation
 * 
 * Derived from NRL Neons V3.6
 */
void
Cde2h(double etime, CdTimeType timeType, long baseYear, CdTime *htime)
{
	long 	ytemp;			/* temporary year holder */
	int 	yr_day_cnt;		/* count of days in year */
	int 	doy;			/* day of year */
	int     daysInLeapYear;		     /* number of days in a leap year */
	int     daysInYear;		     /* days in non-leap year */
	extern void CdMonthDay(int *doy, CdTime *date);

	doy	= (long) floor(etime / 24.) + 1;
	htime->hour	= etime - (double) (doy - 1) * 24.;

					     /* Correct for goofy floor func on J90 */
	if(htime->hour >= 24.){
		doy += 1;
		htime->hour -= 24.;
	}

	htime->baseYear = (timeType & CdBase1970) ? 1970 : baseYear;
	if(!(timeType & CdChronCal)) htime->baseYear = 0; /* Set base year to 0 for Clim */
	daysInLeapYear = (timeType & Cd365) ? 366 : 360;
	daysInYear = (timeType & Cd365) ? 365 : 360;

	if (doy > 0) {
		for (ytemp = htime->baseYear; ; ytemp++) {
			yr_day_cnt = ISLEAP(ytemp,timeType) ? daysInLeapYear : daysInYear;
			if (doy <= yr_day_cnt) break;
			doy -= yr_day_cnt;
		}
	} else {
		for (ytemp = htime->baseYear-1; ; ytemp--) {
			yr_day_cnt = ISLEAP(ytemp,timeType) ? daysInLeapYear : daysInYear;
			doy += yr_day_cnt;
			if (doy > 0) break;
		}
	}
        htime->year = (timeType & CdBase1970) ? ytemp : (ytemp - htime->baseYear);
	if(!(timeType & CdChronCal)) htime->year = 0; /* Set year to 0 for Clim */
	htime->timeType = timeType;
	CdMonthDay(&doy,htime);

        return;
}
/* Convert character time to human time
 * 
 * Input:
 *   ctime    = character time
 *   timeType = time type (e.g. CdChron) as defined in cdms.h
 *   
 * Output:
 *   htime    = human (structured) time
 */
void
Cdc2h(char *ctime, CdTimeType timeType, CdTime *htime)
{
/* 	int iyear, imon, iday, ihour, imin; */
	int ihour, imin;
	double dsec;
/* 	long baseYear; */

	switch(timeType){
	  case CdChron: case CdChronNoLeap: case CdChron360:
		sscanf(ctime,"%ld/%hd/%hd %d:%d:%lf",&htime->year,&htime->month,
		       &htime->day,&ihour,&imin,&dsec);
		htime->hour = (double)ihour + (double)imin/60. + dsec/3600;
		htime->baseYear = 1970;
		htime->timeType = timeType;
		break;
	  case CdRel: case CdRelNoLeap:
		sscanf(ctime,"%ld+%ld/%hd/%hd %d:%d:%lf",&htime->baseYear,
		       &htime->year,&htime->month,&htime->day,&ihour,&imin,&dsec);
		htime->hour = (double)ihour + (double)imin/60. + dsec/3600;
		htime->timeType = timeType;
		break;
	  case CdClim:
		sscanf(ctime,"%hd/%hd %d:%d:%lf",&htime->month,&htime->day,
		       &ihour,&imin,&dsec);
		htime->hour = (double)ihour + (double)imin/60. + dsec/3600;
		htime->year = 0;
		htime->baseYear = 0;
		htime->timeType = timeType;
		break;
	  default:
		cdError("Invalid time type: %d\n",timeType);
	}
	return;
}
/* Convert human (structured) time to character time.
 * 
 * Input:
 *   htime = human time
 * 
 * Output:
 *   ctime = character time
 */
void
Cdh2c(CdTime *htime, char *ctime)
{

	int ihour, imin;
	double dmin, dsec;

	ihour = (int) htime->hour;
	dmin = (htime->hour - (double)ihour) * 60.0;
	imin = (int) dmin;
	dsec = (dmin - (double)imin) * 60.0;
		
	switch(htime->timeType){
	  case CdChron: case CdChronNoLeap: case CdChron360:
		sprintf(ctime,"%ld/%hd/%hd %d:%d:%.1f",htime->year,htime->month,
		       htime->day,ihour,imin,dsec);
		break;
	  case CdRel: case CdRelNoLeap:
		sprintf(ctime,"%ld+%ld/%hd/%hd %d:%d:%.1f",htime->baseYear,
		       htime->year,htime->month,htime->day,ihour,imin,dsec);
		break;
	  case CdClim:
		sprintf(ctime,"%hd/%hd %d:%d:%.1f",htime->month,htime->day,
		       ihour,imin,dsec);
		break;
	  default:
		cdError("Invalid time type: %d\n",htime->timeType);

	}
	return;
}
/* Convert character time to epochal time (hours since 00 jan 1, 1970)
 * 
 * Input:
 *   ctime    = character time
 *   timeType = time type (e.g. CdChron) as defined in cdms.h
 * 
 * Output:
 *   etime    = epochal time 
 */
void
Cdc2e(char *ctime, CdTimeType timeType, double *etime, long *baseYear)
{
	CdTime htime;
	extern void Cdc2h(char *ctime, CdTimeType timeType, CdTime *htime);
	extern void Cdh2e(CdTime *htime, double *etime);

	Cdc2h(ctime,timeType,&htime);
	Cdh2e(&htime,etime);
	*baseYear = htime.baseYear;
	return;
}
/* Convert epochal time (hours since 00 jan 1, 1970) to character time
 * 
 * Input:
 *   etime    = epochal time
 *   timeType = time type, (e.g., CdChron) as defined in cdms.h
 *   baseYear = base year, used for relative time only
 * 
 * Output:
 *   ctime    = character time
 */
void
Cde2c(double etime, CdTimeType timeType, long baseYear, char *ctime)
{
	CdTime htime;
	extern void Cde2h(double etime, CdTimeType timeType, long baseYear, CdTime *htime);
	extern void Cdh2c(CdTime *htime, char *ctime);

	Cde2h(etime,timeType,baseYear,&htime);
	Cdh2c(&htime,ctime);
	return;
}
