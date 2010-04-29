/* This file contains the routines that draws the VCS markers. */
#include "gks.h"
#include "gksshort.h"
#include <math.h>
#include "vcs_marker.h"

/* Get the VCS Marker attribute settings */
extern struct vcs_marker Vma_tab;

extern FILE *fperr;/* input, output, and error for scripts */

vgpm (npts, pe)
int	npts;
Gpoint 	*pe;
{
	int		i, j, k, mtype;
	int 		opwk, *pwk;
	float   	x, y, s;
	float		PI_V=3.14159265358979323846;
        float	   	add_angle, angle1, angle2;
    	Gpoint  	xseg[1000], plus1[2], star1[3], cros1[2];
    	Gpoint  	plus2[2], star2[3], cros2[2];
    	Gpoint  	star3[3];
	Gintlist 	wsid;

	/* Get the open workstation *
        opwk=0;
        wsid.number = 0;
        wsid.integers = NULL;
        gqopwk(&wsid);
        for (i=0,pwk=wsid.integers;i<wsid.number;i++,pwk++) {
           if (*pwk != 7) {opwk=*pwk; break;}
          }
        if (opwk == 0) {
           err_warn(0,fperr,"Error - no workstations open.\n");
          }
        if (wsid.number > 0 && wsid.integers != NULL)
            free((char *)wsid.integers);*/

	/* Save the line attributes */

	/* Set the line attribute settings */
	gsplci(Vma_tab.colour);	/* set line color */
	gsln(1); 		/* set line type */
	gslwsc(1.); 		/* set line width */

	/* Set the fill color attribute settings */
	gsfaci(Vma_tab.color);

	/* Set the fill area type and interior style */
	mtype = Vma_tab.type;
	if ((Vma_tab.type >= 12) && (Vma_tab.type<18)) {
	   gsfais(GSOLID);
	   mtype -= 6;
	}

	/* Draw the marker */
	s = Vma_tab.size/1000;
        switch (mtype) {

           case GMK_POINT:
	       gsfais(GSOLID);
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
		   add_angle = PI_V/24;
		   angle1 = 0; angle2 = add_angle;
		   j = 0;
                   while (angle2 <= (2*PI_V) ) {
                       xseg[j].x = x + s*cos(angle1);
                       xseg[j].y = y + s*sin(angle1);
                       xseg[j+1].x = x + s*cos(angle2);
                       xseg[j+1].y = y + s*sin(angle2);
		       ++j;
		       angle1 += add_angle;
		       angle2 += add_angle;
                   }
                   gfa(48, xseg);
                   /*gflush(opwk, GNCLASS, 0);*/
                   pe++;
               }
               break;
       
           case GMK_PLUS:
               plus1[0].x = -s;
               plus1[0].y = 0;
               plus1[1].x = s;
               plus1[1].y = 0;
               plus2[0].x = 0;
               plus2[0].y = s;
               plus2[1].x = 0;
               plus2[1].y = -s;
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
                   xseg[0].x = x + plus1[0].x;
                   xseg[0].y = y + plus1[0].y;
                   xseg[1].x = x + plus1[1].x;
                   xseg[1].y = y + plus1[1].y;
                   gpl(2, xseg);
                   xseg[0].x = x + plus2[0].x;
                   xseg[0].y = y + plus2[0].y;
                   xseg[1].x = x + plus2[1].x;
                   xseg[1].y = y + plus2[1].y;
                   gpl(2, xseg);
                   /*gflush(opwk, GNCLASS, 0);*/
                   pe++;
               }
               break;
       
           case GMK_STAR:
               star1[0].x = -s;
               star1[0].y = 0;
               star1[1].x = s;
               star1[1].y = 0;
               star2[0].x = s * 0.5;
               star2[0].y = s * 0.866;
               star2[1].x = -s * 0.5;
               star2[1].y = -s * 0.866;
               star3[0].x = s * 0.5;
               star3[0].y = -s * 0.866;
               star3[1].x = -s * 0.5;
               star3[1].y = s * 0.866;
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
                   xseg[0].x = x + star1[0].x;
                   xseg[0].y = y + star1[0].y;
                   xseg[1].x = x + star1[1].x;
                   xseg[1].y = y + star1[1].y;
                   gpl(2, xseg);
                   xseg[0].x = x + star2[0].x;
                   xseg[0].y = y + star2[0].y;
                   xseg[1].x = x + star2[1].x;
                   xseg[1].y = y + star2[1].y;
                   gpl(2, xseg);
                   xseg[0].x = x + star3[0].x;
                   xseg[0].y = y + star3[0].y;
                   xseg[1].x = x + star3[1].x;
                   xseg[1].y = y + star3[1].y;
                   gpl(2, xseg);
                   /*gflush(opwk, GNCLASS, 0);*/
                   pe++;
               }
               break;

           case GMK_O:
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
                   add_angle = PI_V/24;
                   angle1 = 0; angle2 = add_angle;
                   j = 0;
                   while (angle2 <= (2*PI_V+add_angle) ) {
                       xseg[j].x = x + s*cos(angle1);
                       xseg[j].y = y + s*sin(angle1);
                       xseg[j+1].x = x + s*cos(angle2);
                       xseg[j+1].y = y + s*sin(angle2);
                       ++j;
                       angle1 += add_angle;
                       angle2 += add_angle;
                   }
                   gpl(49, xseg);
                   pe++;
               }
               break;

           case GMK_X:
               cros1[0].x = 0.5 * s;
               cros1[0].y = 0.866 * s;
               cros1[1].x = -0.5 * s;
               cros1[1].y = -0.866 * s;
               cros2[0].x = 0.5 * s;
               cros2[0].y = -0.866 * s;
               cros2[1].x = -0.5 * s;
               cros2[1].y = 0.866 * s;
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
                   xseg[0].x = x + cros1[0].x;
                   xseg[0].y = y + cros1[0].y;
                   xseg[1].x = x + cros1[1].x;
                   xseg[1].y = y + cros1[1].y;
                   gpl(2, xseg);
                   xseg[0].x = x + cros2[0].x;
                   xseg[0].y = y + cros2[0].y;
                   xseg[1].x = x + cros2[1].x;
                   xseg[1].y = y + cros2[1].y;
                   gpl(2, xseg);
                   pe++;
               }
               break;

           case GMK_DIAMOND:
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
		   add_angle = PI_V/2;
		   angle1 = 0; angle2 = add_angle;
		   j = 0;
                   while (angle2 <= (2*PI_V + add_angle) ) {
                       xseg[j].x = x + s*cos(angle1);
                       xseg[j].y = y + s*sin(angle1);
                       xseg[j+1].x = x + s*cos(angle2);
                       xseg[j+1].y = y + s*sin(angle2);
		       ++j;
		       angle1 += add_angle;
		       angle2 += add_angle;
                   }
		   if (Vma_tab.type<12) gpl(5,xseg);
		   else gfa(4, xseg);
                   pe++;
               }
               break;

           case GMK_TRIANGLEUP:
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
                   add_angle = (2 * PI_V / 3);
                   angle1 = (7 * PI_V / 6); angle2 = PI_V / 2;
                   j = 0;
                   while (j < 4 ) {
                       xseg[j].x = x + s*cos(angle1);
                       xseg[j].y = y + s*sin(angle1);
                       xseg[j+1].x = x + s*cos(angle2);
                       xseg[j+1].y = y + s*sin(angle2);
                       ++j;
                       angle1 += add_angle;
                       angle2 += add_angle;
                   }
 		   if (Vma_tab.type<12) gpl(4,xseg);
		   else gfa(3, xseg);
                   pe++;
               }
               break;

           case GMK_TRIANGLEDOWN:
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
                   add_angle = (2 * PI_V / 3);
                   angle1 = (PI_V / 6); angle2 = (5 * PI_V / 6);
                   j = 0;
                   while (j < 4 ) {
                       xseg[j].x = x + s*cos(angle1);
                       xseg[j].y = y + s*sin(angle1);
                       xseg[j+1].x = x + s*cos(angle2);
                       xseg[j+1].y = y + s*sin(angle2);
                       ++j;
                       angle1 += add_angle;
                       angle2 += add_angle;
                   }
 		   if (Vma_tab.type<12) gpl(4,xseg);
		   else gfa(3, xseg);
                   pe++;
               }
               break;

           case GMK_TRIANGLELEFT:
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
                   add_angle = (2 * PI_V / 3);
                   angle1 = (5 * PI_V / 3); angle2 = (PI_V / 3);
                   j = 0;
                   while (j < 4 ) {
                       xseg[j].x = x + s*cos(angle1);
                       xseg[j].y = y + s*sin(angle1);
                       xseg[j+1].x = x + s*cos(angle2);
                       xseg[j+1].y = y + s*sin(angle2);
                       ++j;
                       angle1 += add_angle;
                       angle2 += add_angle;
                   }
  		   if (Vma_tab.type<12) gpl(4,xseg);
		   else gfa(3, xseg);
                   pe++;
               }
               break;

           case GMK_TRIANGLERIGHT:
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
                   add_angle = (2 * PI_V / 3);
                   angle1 = (2 * PI_V / 3); angle2 = (4 * PI_V / 3);
                   j = 0;
                   while (j < 4 ) {
                       xseg[j].x = x + s*cos(angle1);
                       xseg[j].y = y + s*sin(angle1);
                       xseg[j+1].x = x + s*cos(angle2);
                       xseg[j+1].y = y + s*sin(angle2);
                       ++j;
                       angle1 += add_angle;
                       angle2 += add_angle;
                   }
   		   if (Vma_tab.type<12) gpl(4,xseg);
		   else gfa(3, xseg);
                   pe++;
               }
               break;

           case GMK_SQUARE:
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
                   add_angle = (PI_V / 2);
                   angle1 = (PI_V / 4); angle2 = (3 * PI_V / 4);
                   j = 0;
                   while (j < 5 ) {
                       xseg[j].x = x + s*cos(angle1);
                       xseg[j].y = y + s*sin(angle1);
                       xseg[j+1].x = x + s*cos(angle2);
                       xseg[j+1].y = y + s*sin(angle2);
                       ++j;
                       angle1 += add_angle;
                       angle2 += add_angle;
                   }
   		   if (Vma_tab.type<12) gpl(5,xseg);
		   else gfa(4, xseg);
                   pe++;
               }
               break;

	case GMK_HURRICANE: /* hurricane */
	  gsfais(GSOLID);
	  for (i=0;i<npts;i++) {
                   x = pe->x;
                   y = pe->y;
                   add_angle = PI_V/180;
                   angle1 = 0; 
                   j = 0;
                   while (angle1 <= (2*PI_V) ) {
                       xseg[j].x = x + s*cos(angle1);
                       xseg[j].y = y + s*sin(angle1);
                       ++j;
                       angle1 += add_angle;
                   }
                   angle1 = 2*PI_V+add_angle; 
                   while (angle1 >= 0 ) {
                       xseg[j].x = x + s*.58*cos(angle1);
                       xseg[j].y = y + s*.58*sin(angle1);
                       ++j;
                       angle1 -= add_angle;
                   }
                  gfa(j, xseg);
		  j=0;
		  angle1 = .6*PI_V; 
		  angle2 = .88*PI_V;
		  while (angle1 <= angle2 ) {
		    xseg[j].x = x + 2*s + s*2*cos(angle1);
		    xseg[j].y = y + s*2*sin(angle1);
		    ++j;
                       angle1 += add_angle;
                   }
		  angle1 = .79*PI_V; 
		  angle2 = .6*PI_V;
		  while (angle1 >= angle2 ) {
		    xseg[j].x = x + 2.25*s + s*4*cos(angle1);
		    xseg[j].y = y - 2*s + s*4*sin(angle1);
		    ++j;
		    angle1 -= add_angle;
                   }
		  gfa(j-1,xseg);

		  j=0;
		  angle1 = 1.6*PI_V; 
		  angle2 = 1.9*PI_V;
		  while (angle1 <= angle2 ) {
		    xseg[j].x = x - 2*s + s*2*cos(angle1);
		    xseg[j].y = y + s*2*sin(angle1);
		    ++j;
                       angle1 += add_angle;
                   }
		  angle1 = 1.8*PI_V; 
		  angle2 = 1.6*PI_V;
		  while (angle1 >= angle2 ) {
		    xseg[j].x = x - 2.27*s + s*4*cos(angle1);
		    xseg[j].y = y + 2*s + s*4*sin(angle1);
		    ++j;
		    angle1 -= add_angle;
                   }
		  gfa(j-1,xseg);
		  pe++;
	  }
           case GMK_NONE:
               break;

           default:
               break;
        }
	/* Set the Line attributes back */
}
