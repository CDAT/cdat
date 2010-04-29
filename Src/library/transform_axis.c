#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Xlib.h>
#endif
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "array.h"
#include "list.h"
#include "picture.h"
#include "graph.h"
#include "project.h"
#include "display.h"
#include "workstations.h"

extern FILE *fpin,*fpout,*fperr;

int trans_coord(pa_xf, pa_xl, axis_type, did_save, which_axis)
float	*pa_xf;
float	*pa_xl;
char	*axis_type;
int	*did_save;
char	*which_axis;
{
        int 		cannot_draw=0;

/*                              Save the existing coordinate axis.         */
	if (did_save != NULL) *did_save = 0;

/*			Reset the standard error flag to zero.	   	*/
	errno = 0;

/*                              Tranform the coordinate axis.         */
        if (strcmp("log10",axis_type) == 0) {
             if (did_save != NULL) *did_save = 1;
             *pa_xf = (float) log10((double) *pa_xf);
             *pa_xl = (float) log10((double) *pa_xl);
             if (errno != 0) cannot_draw = 1;
        } else if (strcmp("ln",axis_type) == 0) {
             if (did_save != NULL) *did_save = 1;
             *pa_xf = (float) log((double) *pa_xf);
             *pa_xl = (float) log((double) *pa_xl);
             if (errno != 0) cannot_draw = 1;
        } else if (strcmp("exp",axis_type) == 0) {
             if (did_save != NULL) *did_save = 1;
             *pa_xf = (float) exp((double) *pa_xf);
             *pa_xl = (float) exp((double) *pa_xl);
             if (errno != 0) cannot_draw = 1;
        } else if (strcmp("area_wt",axis_type) == 0) {
             if (did_save != NULL) *did_save = 1;
             *pa_xf = (float) sin((double) (*pa_xf*3.1415926536/180.0));
             *pa_xl = (float) sin((double) (*pa_xl*3.1415926536/180.0));
             if (errno != 0) cannot_draw = 1;
	}

        if (cannot_draw) {
           err_warn(1,fperr, "Error - in transforming %s-axis to %s.\n",
                     which_axis, axis_type);
	   return 0;
	}

       return 1;
}

int trans_axis(save_xv, pa_xv, pa_xs, pa_xf, pa_xl, axis_type, did_save, 
		which_axis)
float	*save_xv;
float	*pa_xv;
int	pa_xs;
float	*pa_xf;
float	*pa_xl;
char	*axis_type;
int	*did_save;
char	*which_axis;
{
   	int		l;
        int 		cannot_draw=0;
	char		buf[1024];

/*                      Save the existing coordinate axis.         	*/
	if (did_save != NULL) *did_save = 0;

/*			Reset the standard error flag to zero.	   	*/
	errno = 0;

/*                      Tranform the coordinate axis.         		*/

        if (strcmp("log10",axis_type) == 0) {
             for (l = 0; l < pa_xs; ++l)
                {
                 save_xv[l] = pa_xv[l];
                 pa_xv[l] = (float) log10((double) pa_xv[l]);
                 if (did_save != NULL) *did_save = 1;
                }
             if (pa_xf != NULL) *pa_xf = (float) log10((double) *pa_xf);
             if (pa_xl != NULL) *pa_xl = (float) log10((double) *pa_xl);
             if (errno != 0) cannot_draw = 1;
        } else if (strcmp("ln",axis_type) == 0) {
             for (l = 0; l < pa_xs; ++l)
                {
                 save_xv[l] = pa_xv[l];
                 pa_xv[l] = (float) log((double) pa_xv[l]);
                 if (did_save != NULL) *did_save = 1;
                }
             if (pa_xf != NULL) *pa_xf = (float) log((double) *pa_xf);
             if (pa_xl != NULL) *pa_xl = (float) log((double) *pa_xl);
             if (errno != 0) cannot_draw = 1;
        } else if (strcmp("exp",axis_type) == 0) {
             for (l = 0; l < pa_xs; ++l)
                {
                 save_xv[l] = pa_xv[l];
                 pa_xv[l] = (float) exp((double) pa_xv[l]);
                 if (did_save != NULL) *did_save = 1;
                }
             if (pa_xf != NULL) *pa_xf = (float) exp((double) *pa_xf);
             if (pa_xl != NULL) *pa_xl = (float) exp((double) *pa_xl);
             if (errno != 0) cannot_draw = 1;
        } else if (strcmp("area_wt",axis_type) == 0) {
             for (l = 0; l < pa_xs; ++l)
                {
                 save_xv[l] = pa_xv[l];
                 pa_xv[l] = (float) sin((double) (pa_xv[l]*3.141592653589793/180.0));
                 if (did_save != NULL) *did_save = 1;
                }
             if (pa_xf != NULL) *pa_xf = (float) sin((double) (*pa_xf*3.141592653589793/180.0));
             if (pa_xl != NULL) *pa_xl = (float) sin((double) (*pa_xl*3.141592653589793/180.0));
             if (errno != 0) cannot_draw = 1;
        }

        if (cannot_draw) {
           err_warn(1,fperr, "Error - in transforming %s-axis to %s.\n",
                     which_axis, axis_type);
	   return 0;
	}

       return 1;
}

int trans_continent(npts, pts, xaxis_type, yaxis_type)
int	npts;
Gpoint  pts[];
char	*xaxis_type;
char	*yaxis_type;
{
   	int		l;
        int 		cannot_draw=0;

	if ((xaxis_type == NULL) || (xaxis_type == NULL)) return 1;
/*                              Tranform the coordinate x-axis.         */

/*			Reset the standard error flag to zero.	   	*/
	errno = 0;

        if (strcmp("log10",xaxis_type) == 0) {
             for (l = 0; l < npts; ++l)
                {
                 pts[l].x = (float) log10((double) pts[l].x);
                 if (errno != 0) cannot_draw = 1;
                }
        } else if (strcmp("ln",xaxis_type) == 0) {
             for (l = 0; l < npts; ++l)
                {
                 pts[l].x = (float) log((double) pts[l].x);
                 if (errno != 0) cannot_draw = 1;
                }
        } else if (strcmp("exp",xaxis_type) == 0) {
             for (l = 0; l < npts; ++l)
                {
                 pts[l].x = (float) exp((double) pts[l].x);
                 if (errno != 0) cannot_draw = 1;
                }
             if (errno != 0) cannot_draw = 1;
        } else if (strcmp("area_wt",xaxis_type) == 0) {
             for (l = 0; l < npts; ++l)
                {
                 pts[l].x = (float) sin((double) (pts[l].x*3.141592653589793/180.0));
                 if (errno != 0) cannot_draw = 1;
                }
             if (errno != 0) cannot_draw = 1;
        }

        if (cannot_draw) {
           err_warn(1,fperr, "Error - in transforming the x-axis to (%s).\n",
                     xaxis_type);
	   return 0;
	}

/*                              Tranform the coordinate y-axis.         */

        if (strcmp("log10",yaxis_type) == 0) {
             for (l = 0; l < npts; ++l)
                {
                 pts[l].y = (float) log10((double) pts[l].y);
                 if (errno != 0) cannot_draw = 1;
                }
        } else if (strcmp("ln",yaxis_type) == 0) {
             for (l = 0; l < npts; ++l)
                {
                 pts[l].y = (float) log((double) pts[l].y);
                 if (errno != 0) cannot_draw = 1;
                }
        } else if (strcmp("exp",yaxis_type) == 0) {
             for (l = 0; l < npts; ++l)
                {
                 pts[l].y = (float) exp((double) pts[l].y);
                 if (errno != 0) cannot_draw = 1;
                }
        } else if (strcmp("area_wt",yaxis_type) == 0) {
             for (l = 0; l < npts; ++l)
                {
                 pts[l].y = (float) sin((double) (pts[l].y*3.141592653589793/180.0));
                 if (errno != 0) cannot_draw = 1;
                }
        }

        if (cannot_draw) {
           err_warn(1,fperr, "Error - in transforming the x-axis to (%s).\n",
                     xaxis_type);
           return 0;
        }

       return 1;
}
