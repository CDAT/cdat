/*
 * Module:      Time arithmetic routines: CdAddDelTime, CdDivDeltime
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
 * $Log: timeArith.c,v $
 * Revision 1.1.1.1  1997/12/09 18:57:40  drach
 * Copied from cirrus
 *
 * Revision 1.2  1996/02/21  23:56:49  drach
 * - Overlayed cdtime routines in cdTimeConv.c:
 * - Added seconds, julian calendar, changed include to cdmsint.h for old
 *   time routines in timeArith.c and timeConv.c
 *
 * Revision 1.1  1994/07/19  23:54:13  drach
 * Initial version
 *
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <cdmsint.h>

/* Add 'nDel' times 'delTime' to epochal time 'begEtm',
 * return the result in epochal time 'endEtm'.
 */
void
CdAddDelTime(double begEtm, long nDel, CdDeltaTime delTime, CdTimeType timeType,
	     long baseYear, double *endEtm)
{
	double delHours;
	long delMonths, delYears;
	CdTime bhtime, ehtime;

	extern void Cde2h(double etime, CdTimeType timeType, long baseYear, CdTime *htime);
	extern void Cdh2e(CdTime *htime, double *etime);

	switch(delTime.units){
	  case CdYear:
		delMonths = 12;
		break;
	  case CdSeason:
		delMonths = 3;
		break;
	  case CdMonth:
		delMonths = 1;
		break;
	  case CdWeek:
		delHours = 168.0;
		break;
	  case CdDay:
		delHours = 24.0;
		break;
	  case CdHour:
		delHours = 1.0;
		break;
	  case CdMinute:
		delHours = 1./60.;
		break;
	  case CdSecond:
		delHours = 1./3600.;
		break;
	  default:
		cdError("Invalid delta time units: %d\n",delTime.units);
		return;
	}

	switch(delTime.units){
	  case CdYear: case CdSeason: case CdMonth:
		Cde2h(begEtm,timeType,baseYear,&bhtime);
		delMonths = delMonths * nDel * delTime.count + bhtime.month - 1;
		delYears = (delMonths >= 0 ? (delMonths/12) : (delMonths+1)/12 - 1);
		ehtime.year = bhtime.year + delYears;
		ehtime.month = delMonths - (12 * delYears) + 1;
		ehtime.day = 1;
		ehtime.hour = 0.0;
		ehtime.timeType = timeType;
		ehtime.baseYear = !(timeType & CdChronCal) ? 0 :
			(timeType & CdBase1970) ? 1970 : baseYear; /* base year is 0 for Clim, */
								   /* 1970 for Chron, */
								   /* or input base year for Rel */
		Cdh2e(&ehtime,endEtm);
		break;
	  case CdWeek: case CdDay: case CdHour: case CdMinute: case CdSecond:
		delHours *= (nDel * delTime.count);
		*endEtm = begEtm + delHours;
		break;
	}
	return;
}

/* Divide ('endEtm' - 'begEtm') by 'delTime',
 * return the integer portion of the result in 'nDel'.
 */
void
CdDivDelTime(double begEtm, double endEtm, CdDeltaTime delTime, CdTimeType timeType,
	     long baseYear, long *nDel)
{
	double delHours, frange;
	long delMonths, range;
	CdTime bhtime, ehtime;
	int hoursInYear;
	
	extern void Cde2h(double etime, CdTimeType timeType, long baseYear, CdTime *htime);

	switch(delTime.units){
	  case CdYear:
		delMonths = 12;
		break;
	  case CdSeason:
		delMonths = 3;
		break;
	  case CdMonth:
		delMonths = 1;
		break;
	  case CdWeek:
		delHours = 168.0;
		break;
	  case CdDay:
		delHours = 24.0;
		break;
	  case CdHour:
		delHours = 1.0;
		break;
	  case CdMinute:
		delHours = 1./60.;
		break;
	  case CdSecond:
		delHours = 1./3600.;
		break;
	  default:
		cdError("Invalid delta time units: %d\n",delTime.units);
		return;
	}

	switch(delTime.units){
	  case CdYear: case CdSeason: case CdMonth:
		delMonths *= delTime.count;
		Cde2h(begEtm,timeType,baseYear,&bhtime);
		Cde2h(endEtm,timeType,baseYear,&ehtime);
		if(timeType & CdChronCal){   /* Chron and Rel time */
			range = 12*(ehtime.year - bhtime.year)
				+ (ehtime.month - bhtime.month);
		}
		else{			     /* Clim time, ignore year */
			range = (ehtime.month - bhtime.month);
			if(range < 0) range += 12;
		}
		*nDel = abs(range)/delMonths;
		break;
	  case CdWeek: case CdDay: case CdHour: case CdMinute: case CdSecond:
		delHours *= (double)delTime.count;
		if(timeType & CdChronCal){   /* Chron and Rel time */
			frange = fabs(endEtm - begEtm);
		}
		else{			     /* Clim time, ignore year, but */
					     /* wraparound relative to hours-in-year*/
			frange = endEtm - begEtm;
			hoursInYear = (timeType & Cd365) ? 8760. : 8640.;
					     /* Normalize frange to interval [0,hoursInYear) */
			if(frange < 0.0 || frange >= hoursInYear)
				frange -= hoursInYear * floor(frange/hoursInYear);
		}
		*nDel = (frange + 1.e-10*delHours)/delHours;
		break;
	}
	return;
}
