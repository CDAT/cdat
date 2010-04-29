/*
 * This file is the external interface for the C version of the GENERALIZED
 * DRAWING PRIMITIVE function.  It is separated from the file "gdp.c" in
 * order to make this function callable from both the Fortran and C XGKS
 * interfaces: the standard specifies that the name of the function is ggdp()
 * and, on some systems, Fortran function names are the same as C function
 * names (notably under NeXTOS).  Hence, we have both the Fortran and C ggdp()
 * functions call the one in "gdp.c".
 */

/* LINTLIBRARY */

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include "gks_implem.h"


/*
 * ggdp(npoints,points,function,data) - GENERALIZED DRAWING PRIMITIVE
 *
 * Gint    npoints  - number of points
 * Gpoint  *points  - points array
 * Gint    function - GDP identifier
 * Ggdprec *data    - GDP data record pointer
 *
 * errors 5,100,102,103,104,105
 *
 * See also: ANSI standard p.86
 */
ggdp(npoints, points, function, data)
    Gint            npoints;		/* number of points        */
    Gpoint         *points;		/* points array            */
    Gint            function;		/* GDP identifier          */
    Ggdprec        *data;		/* GDP data record pointer */
{
    return g_gdp(npoints, points, function, data);
}
