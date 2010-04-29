/* -*-Mode: C;-*-
 * Module:      cdtime Python header file
 *
 * Copyright:	1997, Regents of the University of California
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
 * $Log: cdtimemodule.h,v $
 * Revision 1.2  1998/07/09 22:55:26  drach
 * - Added support for GDT absolute time (abstime)
 *
 *
 */



#ifndef Py_CDTIMEMODULE_H
#define Py_CDTIMEMODULE_H
#ifdef __cplusplus
extern "C" {
#endif

#include "Python.h"
#include "cdms.h"

/*****************************************************************************
 * Macros, typedefs and enums
 *****************************************************************************/

typedef enum PyCdtime_Units {
	PyCdtime_Seconds = 1, PyCdtime_Minutes, PyCdtime_Hours, PyCdtime_Days,
	PyCdtime_Weeks, PyCdtime_Months, PyCdtime_Seasons, PyCdtime_Years
} PyCdtime_Units;

#define onError(message) \
       { PyErr_SetString(PyCdtime_ErrorObject, message); return NULL; }

#define onSetError(message) \
       { PyErr_SetString(PyCdtime_ErrorObject, message); return -1; }

/*****************************************************************************
 * Cdtime globals
 *****************************************************************************/

extern PyObject *PyCdtime_ErrorObject;      /* cdtime.error */
extern PyObject *PyCdtime_ModuleDict;		     /* module dictionary */

/*****************************************************************************
 * Relative time struct
 *****************************************************************************/

typedef struct {                 /* reltime instance object */
    PyObject_HEAD                /* python header: ref-count + &typeobject */
    double value;
    char units[CD_MAX_RELUNITS+1];
} PyCdReltimeObject;

staticforward PyTypeObject RelTimeType;

#define is_reltimeobject(v)  ((v)->ob_type == &RelTimeType)
#define PyCdReltime_Check is_reltimeobject

/*****************************************************************************
 * Component time struct
 *****************************************************************************/

typedef struct {                 /* comptime instance object */
    PyObject_HEAD                /* python header: ref-count + &typeobject */
    long year;
    int month;
    int day;
    int hour;
    int minute;
    double second;
    double absvalue;			     /* abstime value */
    char absunits[CD_MAX_ABSUNITS];	     /* abstime units */
    double fraction;			     /* abstime fractional part */
} PyCdComptimeObject;

staticforward PyTypeObject CompTimeType;     /* shared type-descriptor */

#define is_comptimeobject(v)  ((v)->ob_type == &CompTimeType)
#define PyCdComptime_Check is_comptimeobject

#ifdef __cplusplus
}
#endif
#endif /* Py_CDTIMEMODULE_H */
