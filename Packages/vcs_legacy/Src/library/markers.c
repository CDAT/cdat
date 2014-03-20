/* This file contains the routines that draws the VCS markers. */
#include "gks.h"
#include "gksshort.h"
#include <math.h>
#include "vcs_legacy_marker.h"

/* Get the VCS Marker attribute settings */
    extern struct vcs_legacy_marker Vma_tab;

    extern FILE *fperr;/* input, output, and error for scripts */

    vgpm (npts, pe)
    int	npts;
    Gpoint 	*pe;
    {
            int		i, j, k,k1,n, mtype;
            int 		opwk, *pwk;
            float   	x, y, s,tempx,tempy,startx,starty;
            float		PI_V=3.14159265358979323846;
            float	   	add_angle, angle1, angle2;//,myx[30],myy[30];
            Gpoint  	xseg[8000],xseg1[100],xseg2[100], xseg3[100],xseg4[100],xseg5[100],xseg6[100],plus1[2], star1[3], cros1[2];
            Gpoint  	plus2[2], star2[3], cros2[2];
            Gpoint  	star3[3];
            Gintlist 	wsid;

            float	        x1,y1,xcent,ycent,p,r;
            
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
	gsfaci(Vma_tab.colour);

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
		   add_angle = PI_V/1000;
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
                   gfa(j, xseg);
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
                   add_angle = PI_V/1000;
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
                   gpl(j, xseg);
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
	  for (i=0;i<npts;i++) {
                   x = pe->x;
                   y = pe->y;
                   add_angle = PI_V/360.;
                   angle1 = 0; 
                   j = 0;
                   while (angle1 <= (20*PI_V) ) {
                       xseg[j].x = x + s*cos(angle1);
                       xseg[j].y = y + s*sin(angle1);
                       j++;
                       angle1 += add_angle;
                   }
                   angle1 = 2*PI_V+add_angle; 
                   while (angle1 >= 0 ) {
                       xseg[j].x = x + s*.58*cos(angle1);
                       xseg[j].y = y + s*.58*sin(angle1);
                       j++;
                       angle1 -= add_angle;
                   }
                  gsfais(GSOLID);
                  gfa(j-1, xseg);
		  j=0;
		  angle1 = .6*PI_V; 
		  angle2 = .88*PI_V;
		  while (angle1 <= angle2 ) {
		    xseg[j].x = x + 2*s + s*2*cos(angle1);
		    xseg[j].y = y + s*2*sin(angle1);
		    j++;
                       angle1 += add_angle;
                   }
		  angle1 = .79*PI_V; 
		  angle2 = .6*PI_V;
		  while (angle1 >= angle2 ) {
		    xseg[j].x = x + 2.25*s + s*4*cos(angle1);
		    xseg[j].y = y - 2*s + s*4*sin(angle1);
		    j++;
		    angle1 -= add_angle;
                   }
                  gsfais(GSOLID);
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
		    j++;
		    angle1 -= add_angle;
                   }
                  gsfais(GSOLID);
		  gfa(j-1,xseg);
		  pe++;
	  	}
		break;

           case GMK_w00:
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
                   angle1 = 0; 
                   j = 0;
                   s = Vma_tab.size/1000;
		   n = 20; add_angle = PI_V/n ; 
		   for(k = 0; k <= 2*n; k++){
		   	angle1 = k*add_angle;
		 	xseg[j].x = x + s*cos(angle1);
		 	xseg[j].y = y + s*sin(angle1);
		 	j++;
		   }
                   gpl(41, xseg);
                   pe++;
               }
               break;




	   case GMK_w01:
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
		   n=20;
                   add_angle = PI_V/n;
                   angle1 = 0; 
                   j = 0,k = 0;
		   s = Vma_tab.size/1000;
		   for(k = 0; k <= 2*n; k++){  
			angle1= k*add_angle;
			if(k==31){     
				xseg[j].x = tempx; 
				xseg[j].y = tempy-(Vma_tab.size*0.001);
				j++;
				xseg[j].x = tempx;
				xseg[j].y = tempy;
				j++;
				}
			tempx = xseg[j].x = x + s*cos(angle1);  
			tempy = xseg[j].y = y + s*sin(angle1); 
    			j++;
 			}
		   gpl(43,xseg);
                   pe++;
               }
               break;

	   case GMK_w02:
               for (i = 0; i < npts; i++) {
                   x = pe->x;
                   y = pe->y;
		   n=20;
                   add_angle = PI_V/n;
                   angle1 = 0; 
                   j = 0,k = 0;
		   s = Vma_tab.size/1000;
		   for(k = 0; k <= 2*n; k++){   
			angle1 = k*add_angle;
			tempx = xseg[j].x = x + s*cos(angle1);  
			tempy = xseg[j].y = y + s*sin(angle1); 
	    		j++;
				if(k==0 || k==20){ 
					if(k==0){  
						xseg[j].x = tempx + (Vma_tab.size*0.001); }
		                        if(k==20) {
						xseg[j].x = tempx- (Vma_tab.size*0.001); }
					xseg[j].y = tempy;
					j++;
					xseg[j].x = tempx;
					xseg[j].y = tempy;
					j++;
					}
				
				
 				}
		  gpl(45,xseg);
                  pe++;
               }
               break;

	case GMK_w03:
             for (i = 0; i < npts; i++) {
             	   x = pe->x;
              	   y = pe->y;
		   n=20;
                   add_angle = PI_V/n;
                   angle1 = 0; 
                   j = 0,k = 0;
		   s = Vma_tab.size/1000;


		   for(k = 0; k <= 2*n; k++){ 
				angle1 = k*add_angle;
				tempx = xseg[j].x = x + s*cos(angle1);
				tempy = xseg[j].y = y + s*sin(angle1); 
    				j++;			
				if(k==10 ){    
					xseg[j].x = tempx; 
					xseg[j].y = tempy + (Vma_tab.size*0.001);
					j++;
					xseg[j].x = tempx;
					xseg[j].y = tempy;
					j++;
				        }
		}
		gpl(43,xseg);
                pe++;
		}
              	break;

	case GMK_w04:
               for (i = 0; i < npts; i++) {
             	 	startx=   x = pe->x;
              		starty=   y = pe->y;
                  	 add_angle = PI_V/24;
                  	 angle1 = 0; angle2 = add_angle;
                 	 j = 0,k=0;
		  	 s = Vma_tab.size/1000;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			xseg[j].x = x;
			xseg[j].y = y-(Vma_tab.size*0.001); 
			j++;
			for (k = 0; k <= 17; k+= 1){
				xseg[j].x = x + k*Vma_tab.size*0.0001; 
				xseg[j].y = y + 1*sin(k)*Vma_tab.size*0.0001; 
				j++;
				}
				gpl(19,xseg);
				pe++;
              		 }
 			break;  

		
	case GMK_w05:
               for (i = 0; i < npts; i++) {
               		x = pe->x;
               		y = pe->y;
                   	add_angle = PI_V/24;
                   	angle1 = 0; 
                   	j = 0,k = 0;
		   	s = Vma_tab.size/1000;
			// for w05
		 	for ( k=0; k <= 6; k += 1){
				xseg[j].x = x + k*Vma_tab.size*0.0001; 
				xseg[j].y = y + (sin(k))*Vma_tab.size*0.0001; 
				j++;
				}
		
			for (k = 6; k >= 0; k -= 1){
				xseg[j].x = x + k*Vma_tab.size*0.0001;
				xseg[j].y = y + (1*sin(-1*k))*Vma_tab.size*0.0001; 
				j++;
				}
		

			gpl(14,xseg);
			pe++;
		        }
               	        break;





//w06
	case GMK_w06:
             for (i = 0; i < npts; i++) {
             	       x = pe->x;
                       y = pe->y;
                       add_angle = PI_V/24;
                       angle1 = 0; 
                       j = 0,k=0;
		       s = Vma_tab.size/1000;
		       n=20;
		       add_angle=PI_V/n ; 
		       for( k = 0; k < n + (n/2); k++) {
		 		angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
		 		xseg[j].y = y + s*sin(angle1);
		 		j++;
				}
			y = y - Vma_tab.size*0.002;
			add_angle = PI_V/n ; 
			for( k = n/2; k >-(n); k--) {
				 angle1 = k*add_angle;
				 xseg[j].x = x + s*cos(angle1);
				 xseg[j].y = y + s*sin(angle1);
				 j++;
				 }
		gpl(60,xseg);
		pe++;
		}
		break;
		
     



// w07
	case GMK_w07:
               for (i = 0; i < npts; i++) {
               		x = pe->x;
               		y = pe->y;
               		angle1 = 0; 
                   	j = 0,k=0;
		   	s = Vma_tab.size/1000;

            		// code for "S" in symbol w07
        		n=20;
        		add_angle=PI_V/n ;
        		for( k = 0; k <n + (n/2); k++) {
				 angle1 = k*add_angle;
				 xseg[j].x = x + s*cos(angle1);
				 xseg[j].y = y + s*sin(angle1);
				 j++;
				}

			y = y - Vma_tab.size*0.002;
			add_angle = PI_V/n ;
			for( k = n/2; k >-(n); k--){
				 angle1 = k*add_angle;
				 xseg[j].x = x + s*cos(angle1);
				 xseg[j].y = y + s*sin(angle1);
				 j++;
				}

        		gpl(60,xseg);


        
        		// code for "|" i.e cross vertical arrow in symbol w07
          
                	x = pe->x;
               		y = pe->y;                             
                   	j = 0;
               		tempx = xseg1[j].x = x;
			tempy = xseg1[j].y = y + Vma_tab.size*0.002;
			j++;
       
    			// code for up arrow

			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
        		xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;

			xseg1[j].x = tempx;
			xseg1[j].y = tempy;
			j++;
		
			xseg1[j].x = xseg1[j-1].x + Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;

			//code for vetical line

			xseg1[j].x = tempx;
			xseg1[j].y = tempy;
			j++;
			xseg1[j].x = tempx;
			xseg1[j].y = tempy - (Vma_tab.size*0.006);
			j++;
			gpl(6,xseg1);
			       
        	pe++;
                }
                break;



	case GMK_w08:
               for (i = 0; i < npts; i++) {
               		x = pe->x;
               		y = pe->y;
               		add_angle = PI_V/24;
                   	angle1 = 0; angle2 = add_angle;
                   	j = 0,k = 0;
		  	s = Vma_tab.size/1000;
			//w08

			// code for first ring
			n = 20;
			add_angle=PI_V/n ; 
			for(k = n/2; k < (2*n)-2; k++) {
				 angle1 = k*add_angle;
				 xseg[j].x = x + s*cos(angle1);
				 xseg[j].y = y + s*sin(angle1);
				 if(k== (2*n-(n/2))){
					tempx = xseg[j].x;
					tempy = xseg[j].y;
					}
				j++;
				}
				gpl(28,xseg);

			//code for second ring		
			n = 20;
			x = tempx;
			y = tempy;
			j = 0;
			for(k = 3; k<(2*n-(n/8)); k++){
				 angle2 = k*add_angle;
				 xseg1[j].x = x + s*cos(angle2);
				 xseg1[j].y = y + s*sin(angle2);
				 if ( k== (2*n-(n/2))){
					startx = xseg1[j].x;
					starty = xseg1[j].y;
					}
				j++;
				}
			        gpl(35,xseg1);

			//code for third ring
			n = 20;
			x = startx;
			y = starty;
			j = 0;
			for(k = 3; k<(2*n-(n/4)); k++) {
				 angle2 = k*add_angle;
				 xseg2[j].x = x + s*cos(angle2);
				 xseg2[j].y = y + s*sin(angle2);
				 j++;
				 }
				 gpl(32,xseg2);

		
		pe++;
		}
		break;




	case GMK_w09:
               for (i = 0; i < npts; i++) {
                	x = pe->x;
                	y = pe->y;
                   	add_angle = PI_V/24;
                   	angle1 = 0; angle2 = add_angle;
                   	j = 0,k = 0;
		
		
				//w09
				//code for "S"
				n = 20;
				add_angle = PI_V/n ;
				for(k = 0; k<n+(n/2); k++) {
					 angle1 = k*add_angle;
					 xseg[j].x =x + s*cos(angle1);
					 xseg[j].y =y + s*sin(angle1);
					 j++;
					 }
					 tempx = xseg[j-1].x;
					 tempy = xseg[j-1].y;
					 y = y - Vma_tab.size*0.002;
					 add_angle = PI_V/n ;
					 for( k = n/2; k >-(n); k--) {
						 angle1 = k*add_angle;
						 xseg[j].x = x + s*cos(angle1);
						 xseg[j].y = y + s*sin(angle1);
						 j++;
						 }
	 				 gpl(60,xseg);
                                      
				//code for horizontal right hand arrow mark         
 
     				{         
	                        x = pe->x;
				y = pe->y;
				j = 0;
				startx = xseg1[j].x = tempx - Vma_tab.size*0.002;
				starty = xseg1[j].y = tempy;
				j++;
				xseg1[j].x = tempx + Vma_tab.size*0.002;
				xseg1[j].y = tempy;
				j++;
			       
				xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
				xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
				j++;


				xseg1[j].x = xseg1[j-2].x;
				xseg1[j].y = xseg1[j-2].y;
				j++;
				xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
				xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0006;
				j++;
			  	gpl(5,xseg1);
				}

			       //code for right bracket in w09
			       {
			        x = tempx;
			        y = tempy;
				add_angle = PI_V/24;
				angle1 = 0; angle2 = add_angle;
				j = 0;
				
                                //code for right down bracket
				for (k = 0; k < 8; k++) {
				       xseg[j].x = x + Vma_tab.size*0.0025*cos(angle1);
				       xseg[j].y = y - Vma_tab.size*0.0025*sin(angle1);
				       ++j;
				       angle1 += add_angle;
				      }
				      gpl(8, xseg);
				
                                //code for right up bracket
				angle1 = 0;
				j=0;
	                        for (k = 0; k < 8; k++) {
				       xseg2[j].x = x + Vma_tab.size*0.0025*cos(angle1);
				       xseg2[j].y = y + Vma_tab.size*0.0025*sin(angle1);
				       ++j;
				       angle1 += add_angle;
				      }  
				     gpl(8, xseg2);
				  
                                 }

			         //code for left bracket in w09
			         {
				 x =  tempx;
				 y = tempy ;
				 j = 0;
				 n = 20;
				 add_angle = (PI_V/n) ;
				 angle2 = 0;
				 
				//code for left down bracket
				for(k = 0; k < 7; k++) {
				 	 xseg3[j].x = x - Vma_tab.size*0.0025*cos(angle2);
				         xseg3[j].y = y + Vma_tab.size*0.0025*sin(angle2);
				 	 ++j;
					 angle2 += add_angle;
					 }             
				         gpl(7,xseg3);

				 //code for left down bracket
				 j = 0;
				 angle2 = 0;
				 add_angle = (PI_V/n) ;
				 for(k = 0; k < 7; k++) {
			       		 xseg4[j].x = x - Vma_tab.size*0.0025*cos(angle2);
				         xseg4[j].y = y - Vma_tab.size*0.0025*sin(angle2);
				         ++j;
				         angle2 += add_angle;
				         }             
				         gpl(7,xseg4);
						
				 }      
               
				

			pe++;
			}
               		break;

		
	case GMK_w10:
               for (i = 0; i < npts; i++) {
		        x = pe->x;
		        y = pe->y;
		        add_angle = PI_V/24;
		        angle1 = 0; angle2 = add_angle;
		        j = 0, k = 0;
			s = Vma_tab.size/1000;
			//w10

			{
		        j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y - Vma_tab.size*0.0005;
			j++;
			xseg[j].x = x + Vma_tab.size*0.0025;
			xseg[j].y = tempy;
			j++;
		        gpl(2,xseg);
			}
			{
	 		k=0;
		        xseg1[k].x = x;
			xseg1[k].y = y;
			k++;
			xseg1[k].x = x + Vma_tab.size*0.0025;
			xseg1[k].y = y;
			k++;
		        gpl(2,xseg1);
			}
		pe++;
		}
		break;



	case GMK_w11:
               for (i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0,k=0;
			s = Vma_tab.size/1000;
			//w11

			// code for upper first part of line
			{
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y - Vma_tab.size*0.0005;
			j++;
			xseg[j].x = x + Vma_tab.size*0.0010;
			xseg[j].y = tempy;
			j++;
			gpl(2,xseg);
			}
			
			//code for upper second part of line
			{
			k = 0;
			tempx = xseg1[k].x = x + Vma_tab.size*0.0015 ;
			tempy = xseg1[k].y = y - Vma_tab.size*0.0005;
			k++;
			xseg1[k].x = tempx + Vma_tab.size*0.0010;
			xseg1[k].y = tempy;
			k++;
			gpl(2,xseg1);
			}
			
			//code for lower first part of line
			{
			j = 0;
			xseg2[j].x = x;
			tempy = xseg2[j].y = y;
			j++;
			xseg2[j].x = x + Vma_tab.size*0.0010;
			xseg2[j].y = tempy;
			j++;
			gpl(2,xseg2);
			}
			
			//code for lower second part of line
			{
			k = 0;
			tempx = xseg3[k].x = x + Vma_tab.size*0.0015;
			xseg3[k].y = y;
			k++;
			xseg3[k].x = tempx + Vma_tab.size*0.0010;
			xseg3[k].y = y;
			k++;
			gpl(2,xseg3);
			}

		pe++;
		}
		break;




	case GMK_w12:
               for (i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0,k=0;
			s = Vma_tab.size/1000;
			//w12

			// code for upper first part of line
			{
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y + Vma_tab.size*0.0005;
			j++;
			xseg[j].x = x + Vma_tab.size*0.0010;
			xseg[j].y = tempy;
			j++;
			gpl(2,xseg);
			}
			
			//code for upper second part of line
			{
			k = 0;
			tempx = xseg1[k].x = x + Vma_tab.size*0.0015 ;
			tempy = xseg1[k].y = y + Vma_tab.size*0.0005;
			k++;
			xseg1[k].x = tempx + Vma_tab.size*0.0010;
			xseg1[k].y = tempy;
			k++;
			gpl(2,xseg1);
			}
			
			//code for lower line
			{
			j = 0;
			xseg2[j].x = x;
			tempy = xseg2[j].y = y;
			j++;
			xseg2[j].x = x + 2*Vma_tab.size*0.0010 + Vma_tab.size*0.0005;
			xseg2[j].y = tempy;
			j++;
			gpl(2,xseg2);
			}
		pe++;
		}
		break;



	//w13
	case GMK_w13:
		for (i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0,k = 0;
			s = Vma_tab.size/1000;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			
			// code for left down line
			xseg[j].x = x - Vma_tab.size*0.0010;
			xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			
			// code for right down line
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;
			
			// code for arrow
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y  - Vma_tab.size*0.0020;
			j++;

			xseg[j].x = xseg[j-1].x;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
			j++;


			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			
			xseg[j].x = xseg[j-3].x-Vma_tab.size*0.0004;
			xseg[j].y = xseg[j-3].y;
			j++;
			
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			gpl(8,xseg);
		pe++;
		}
		break;





	//w14
	case GMK_w14:
		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/1000;
			n=20;
			add_angle=PI_V/n ; 
			// start of solid circle
			gsfais(GSOLID);
			for( k = 0; k <= 2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				if( k == 10) {
					tempx = xseg[j].x;
					tempy = xseg[j].y + 0.01;
					}
				}
			gfa(41, xseg);
			// end of solid circle


			//code for right bracket in w09

			add_angle = PI_V/n;
			angle1 = 0; angle2 = 0;
			j = 0;

			for ( k = 0; k < 12; k++) {
				angle2 += add_angle;
				if ( k >= 4) {
				xseg1[j].x = x - Vma_tab.size*0.0015*cos(angle2);
				xseg1[j].y = y - Vma_tab.size*0.0015*sin(angle2);
				j++;
				}
			}
			gpl(8, xseg1);
			
			angle2 = 0;
			j=0;
			for ( k = 0; k < 8; k++) {
			angle2 += add_angle;
				if ( k >= 4){
				xseg2[j].x = x +Vma_tab.size*0.0015*cos(angle2);
				xseg2[j].y = y - Vma_tab.size*0.0015*sin(angle2);
				j++;
				}
			}
			gpl(4, xseg2);

                pe++;
                }
                break;



	//w15

	case GMK_w15:


		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n=20;
			add_angle=PI_V/n ; 
			
			// start of solid circle
			gsfais(GSOLID);
			for( k = 0; k <= 2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				if(k == 10) {
				tempx = xseg[j].x;
				tempy = xseg[j].y + 0.01;
				}
			}
			gfa(41, xseg);

			//right side open bracket 
			add_angle = PI_V/n;
			angle1 = 0; 
			angle2 = 0;
			j = 0;
			x=x+Vma_tab.size*0.0025;
			
			for ( k = 0; k <(n+5); k++) {
				angle2 += add_angle;
				if ( k >= 15) {
				xseg1[j].x = x + Vma_tab.size*0.0015*cos(angle2);
				xseg1[j].y = y + Vma_tab.size*0.0015*sin(angle2);
				j++;
				}
			}
			gpl(10, xseg1);
			
			//left side open bracket 	 
			angle2 = 0;
			j = 0;
			x = x - Vma_tab.size*0.0050;
			for ( k = 0; k <n-5; k++) {
				angle2 += add_angle;
				if ( k >= 5){
				xseg2[j].x = x + Vma_tab.size*0.0015*sin(angle2);
				xseg2[j].y = y + Vma_tab.size*0.0015*cos(angle2);
				j++;
				}
			}
			gpl(10, xseg2);


		pe++;
		}
		break;





	//w16
	case GMK_w16:


		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n=20;
			add_angle=PI_V/n ; 
			
			// start of solid circle
			gsfais(GSOLID);
			for( k = 0; k<=2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				if( k== 10) {
				tempx = xseg[j].x;
				tempy = xseg[j].y + 0.01;
				}
			}
			gfa(41, xseg);

			//right side open bracket 
			add_angle = PI_V/n;
			angle1 = 0; angle2 = 0;
			j = 0;
			for ( k = 0; k <n-5; k++) {
				angle2 += add_angle;
				if ( k >= 5) {
				xseg2[j].x = x + Vma_tab.size*0.0015*sin(angle2);
				xseg2[j].y = y + Vma_tab.size*0.0015*cos(angle2);
				j++;
				}
			}
			gpl(10, xseg2);

			//left side open bracket 	 
			angle2 = 0;
			j = 0;
			for ( k = 0; k <(n+5); k++) {
				angle2 += add_angle;
				if ( k >= 15) {
				xseg1[j].x = x + Vma_tab.size*0.0015*cos(angle2);
				xseg1[j].y = y + Vma_tab.size*0.0015*sin(angle2);
				j++;
				}
			}
			gpl(10, xseg1);

		pe++;
		}
		break;





	//w17
	case GMK_w17:

	// its part of w13 and other few lines
	for ( i = 0; i < npts; i++) {
		startx =   x = pe->x;
		starty =   y = pe->y;
		add_angle = PI_V/24;
		angle1 = 0; angle2 = add_angle;
		j = 0,k = 0;
		s = Vma_tab.size/1000;

		//code for w13
		j = 0;
		xseg[j].x = x;
		tempy = xseg[j].y = y;
		j++;
		
		// code for left down line
		tempx = xseg[j].x = x - Vma_tab.size*0.0010;
		tempy = xseg[j].y = y - Vma_tab.size*0.0010;
		j++;
		
		// code for right down line
		xseg[j].x = x + Vma_tab.size*0.0001;
		xseg[j].y = y -  Vma_tab.size*0.0020;
		j++;
		
		// code for arrow
		xseg[j].x = x + Vma_tab.size*0.0001;
		xseg[j].y = y -  Vma_tab.size*0.0020;
		j++;

		xseg[j].x = xseg[j-1].x;
		xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
		j++;

		xseg[j].x = xseg[j-2].x;
		xseg[j].y = xseg[j-2].y;
		j++;
		
		xseg[j].x = xseg[j-3].x - Vma_tab.size*0.0004;
		xseg[j].y = xseg[j-3].y;
		j++;
		
		xseg[j].x = x + Vma_tab.size*0.0001;
		xseg[j].y = y -  Vma_tab.size*0.0020;
		j++;

		gpl(8,xseg);

		// code for top horizontal line
		j = 0;
		xseg1[j].x = x; 
		xseg1[j].y = y;
		j++;
		
		xseg1[j].x = x - Vma_tab.size*0.0024;
		xseg1[j].y = y;
		j++;
		
		// code for vertical line
		xseg1[j].x = x - Vma_tab.size*0.002;
		xseg1[j].y = y;
		j++;
		xseg1[j].x = x - Vma_tab.size*0.002;
		xseg1[j].y = y - Vma_tab.size*0.002;
		j++;
		gpl(4,xseg1);



		//right side close bracket 
		x = tempx + Vma_tab.size*0.0015;
		y = tempy;  //x,y adjustment to draw bracket
		n = 20;
		add_angle = PI_V/n;
		angle1 = 0; angle2 = 0;
		j = 0;

		for ( k = 0; k < n-4; k++) {
			angle2 += add_angle;
			if ( k >= 3) {
			xseg2[j].x = x + Vma_tab.size*0.0015*sin(angle2);
			xseg2[j].y = y + Vma_tab.size*0.0015*cos(angle2);
			j++;
			}
		}
		gpl(13, xseg2);

		//left side open bracket 	 
		angle2 = 0;
		j = 0;
		x = tempx - Vma_tab.size*0.0015;
		y = tempy; //x,y adjustment to draw bracket
		for ( k = 0; k <(n+6); k++) {
			angle2 += add_angle;
			if ( k >= 13) {
			xseg1[j].x = x +Vma_tab.size*0.0015*cos(angle2);
			xseg1[j].y = y + Vma_tab.size*0.0015*sin(angle2);
			j++;
			}
		}
		gpl(13, xseg1);
		
	pe++;
	}

	break;



	//w18
	case GMK_w18:


		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			j = 0,k = 0;
			
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.001;
			j++;
			
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.0015;
			j++;

			xseg[j].x = x;
			xseg[j].y = y; j++;

			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.0015;
			j++;

			xseg[j].x = x ;
			xseg[j].y = y + Vma_tab.size*0.001;
			j++;
			gpl(5, xseg);


		pe++;
		}
		break;




	//w19
	case GMK_w19:


		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			
			j = 0,k = 0;
			s = Vma_tab.size/1000;
			
			// code for left square open bracket
			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.001;
			j++;	

			tempx = xseg[j].x = x + Vma_tab.size*0.001;
			tempy = xseg[j].y = y + Vma_tab.size*0.005;
			j++;

			xseg[j].x = tempx - Vma_tab.size*0.001;
			xseg[j].y = tempy + Vma_tab.size*0.001;
			j++;

			gpl(4, xseg);

			x = x + Vma_tab.size*0.004; // gap between two vetical lines
			j = 0;
			
			// code for right square open bracket
			xseg1[j].x = x;
			xseg1[j].y=y;
			j++;

			xseg1[j].x = x - Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.001;
			j++;

			startx = xseg1[j].x = x - Vma_tab.size*0.001;
			starty = xseg1[j].y = y + Vma_tab.size*0.005;
			j++;

			xseg1[j].x = startx + Vma_tab.size*0.001;
			xseg1[j].y = starty + Vma_tab.size*0.001;
			j++;
			
			gpl(4, xseg1);


		pe++;
		}
		break;



	//w20
	case GMK_w20:
		
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n=20;
			add_angle=PI_V/n ; 
			
			// start of solid circle 
			gsfais(GSOLID);
			for( k = 0; k <=2*n; k++) {
				
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)) {
					tempx=x;
					tempy=y;
					}

				}
				gfa(41, xseg);
				
			//start of solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size/3000;
			j = 0;
			
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--) {	
				 angle1 = k*add_angle;
				 xseg1[j].x = x + s*cos(angle1);
				 xseg1[j].y = y + s*sin(angle1);
				 j++;

			 }
			s = Vma_tab.size/1500;
			for( k =-(n/2); k < n/4; k++) {
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size/1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size/2500*sin(angle1);
				j++;
			 }
					
			gfa(35, xseg1);

			// code for right side line
			y = tempy - Vma_tab.size*0.0015;

			x = tempx + Vma_tab.size*0.0005;

			j = 0;
			xseg1[j].x = x; 
			xseg1[j].y = y;
			j++;
			 
			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y + Vma_tab.size*0.003; 
			j++;

			xseg1[j].x = x ;
			xseg1[j].y = y + Vma_tab.size*0.003; 
			j++;
			gpl(4, xseg1);

		pe++;
		}
		break;





	//w21
	case GMK_w21:


		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			
			angle1 = 0; 
			j = 0,k = 0;
			s = Vma_tab.size/2000;

			y = y + Vma_tab.size*0.001; // setting centered circle position

			// start of solid circle
			gsfais(GSOLID);                                                   
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				if( k == 10) {
					tempx = xseg[j].x;
					tempy = xseg[j].y + 0.01;
					}
				}
				gfa(41, xseg);

			// end of solid circle

			// code for right side line
			y = y - Vma_tab.size*0.0015;

			x = x + Vma_tab.size*0.0005;

			j = 0;
			xseg1[j].x = x ;
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.003; 
			j++;

			xseg1[j].x = x;
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;
			gpl(4, xseg1);

		pe++;
		}
		break;





	//w22
	case GMK_w22:

		// start of  star
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
	
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			j = 0,k = 0;
			y = y + Vma_tab.size*0.001; // setting centered star position
			
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

			// end of  star

			// code for right side line
			y = y - Vma_tab.size*0.0015;
			x = x + Vma_tab.size*0.001;

			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
	
			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;

			xseg1[j].x = x;
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;
			gpl(4, xseg1);

		pe++;
		}
		break;





	//w23
	case GMK_w23:


		// start of  star
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
			j = 0,k=0;
			y=y+Vma_tab.size*0.001; // setting star position
			
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

			// end of  star

			// start of solid circle

			y = y + Vma_tab.size*0.003; // setting circle position
			gsfais(GSOLID); 
			s = Vma_tab.size/1500;                                                 
			n=20;
			add_angle=PI_V/n ; 
			for( k=0; k <=2*n; k++) {
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				if( k == 10) {
					tempx = xseg2[j].x;
					tempy = xseg2[j].y + 0.01;
					}
			}
			gfa(41, xseg2);

			// end of solid circle


			// code for right side line
			y = y - Vma_tab.size*0.0045;
			x = x + Vma_tab.size*0.001;
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y ;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.006; 
			j++;

			xseg1[j].x = x;
			xseg1[j].y = y + Vma_tab.size*0.006; 
			j++;
			
			gpl(4, xseg1);

		pe++;
		}
		break;




	//w24
	case GMK_w24:


		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			angle1 = 0; 
			j = 0,k = 0;
			s = Vma_tab.size/1000;
			n=20;
			add_angle=PI_V/n ; 
			for( k = 0; k <n + (n/2); k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*sin(angle1);
				xseg[j].y = y + s*cos(angle1);
				j++;
				}

			x = x - Vma_tab.size*0.002;
			y = y;
			add_angle = PI_V/n ; 
			for( k = n/2; k >-(n); k--) {
				angle1= k*add_angle;
				xseg[j].x = x + s*sin(angle1);
				xseg[j].y = y + s*cos(angle1);
				j++;
				} 
			gpl(60,xseg);

			// code for right side line

			x = x + Vma_tab.size*0.003;
			y = y - Vma_tab.size*0.0015;

			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;

			xseg1[j].x = x;
			xseg1[j].y = y + Vma_tab.size*0.003; 
			j++;
			
			gpl(4, xseg1);


		pe++;
		}
		break;




	//w25
	case GMK_w25:


		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			
			angle1 = 0; 
			j = 0,k = 0;
			s = Vma_tab.size/1000;


			// code for invert triangle
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.001;
			j++;
			
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.001;
			j++;

			xseg[j].x = x;
			xseg[j].y = y;
			j++;

			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.001;
			j++;

			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y +Vma_tab.size*0.001;
			j++;
			
			gpl(5, xseg);


			y = y + Vma_tab.size*0.003; // setting circle position
			gsfais(GSOLID);                                                   
			n=20; 
			j = 0;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				if(k == 10) {
					tempx = xseg2[j].x;
					tempy = xseg2[j].y + 0.01;
					}
				}
			gfa(41, xseg2);

			// end of solid circle


			// code for right side line
			y = y - Vma_tab.size*0.004;
			x = x + Vma_tab.size*0.001;

			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
	
			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.006; 
			j++;

			xseg1[j].x = x;
			xseg1[j].y = y + Vma_tab.size*0.006; 
			j++;
			
			gpl(4, xseg1);



		pe++;
		}
		break;





	//w26
	case GMK_w26:

		// star initialization 
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

		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			j = 0,k = 0;
			s = Vma_tab.size/1000;


			// code for invert triangle
			xseg2[j].x = x;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg2[j].x = x - Vma_tab.size*0.001;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;

			xseg2[j].x = x;
			xseg2[j].y = y;
			j++;

			xseg2[j].x = x + Vma_tab.size*0.001;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;

			xseg2[j].x = x - Vma_tab.size*0.001;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;
			
			gpl(5, xseg2);

			y = y + Vma_tab.size*0.002; // setting star position
			// start of  star

			j = 0,k = 0;
			y = y + Vma_tab.size*0.001; // setting star position
			
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

			// end of  star


			// code for right side line
			y = y - Vma_tab.size*0.004;
			x = x + Vma_tab.size*0.001;

			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y + Vma_tab.size*0.006; 
			j++;

			xseg1[j].x = x;
			xseg1[j].y = y + Vma_tab.size*0.006; 
			j++;
			gpl(4, xseg1);



		pe++;
		}
		break;



	//w27
	case GMK_w27:


		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0,k = 0;
			s = Vma_tab.size/1000;


			// code for invert triangle
			xseg2[j].x = x;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg2[j].x = x - Vma_tab.size*0.001;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;

			xseg2[j].x = x ; 
			xseg2[j].y = y;
			j++;

			xseg2[j].x = x + Vma_tab.size*0.001;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;

			xseg2[j].x = x - Vma_tab.size*0.001;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;
			gpl(5, xseg2);


			y = y + Vma_tab.size*0.002; // setting regular triangle [top] position
			j=0;
			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y; 
			j++;

			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.001;
			j++;

			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y;
			j++;

			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			gpl(5, xseg);


			// code for right side line
			y = y - Vma_tab.size*0.003;

			x = x + Vma_tab.size*0.0012;


			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y + Vma_tab.size*0.005;
			j++;

			xseg1[j].x = x;
			xseg1[j].y = y + Vma_tab.size*0.005; 
			j++;
			gpl(4, xseg1);



		pe++;
		}
		break;




	//w28
	case GMK_w28:


		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			
			j = 0,k = 0;
			s = Vma_tab.size/1000;

			// first horizontal line

			{
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y - Vma_tab.size*0.001;
			j++;
			xseg[j].x = x + Vma_tab.size*0.004;
			xseg[j].y = tempy;
			j++;
			gpl(2,xseg);
			}


			// second horizontal line
			{
			j = 0;
			xseg2[j].x = x;
			tempy = xseg2[j].y = y;
			j++;
			xseg2[j].x = x + Vma_tab.size*0.004;
			xseg2[j].y = tempy;
			j++;
			gpl(2,xseg2);
			}


			// third horizontal line
			{
			j = 0;
			xseg3[j].x = x;
			tempy = xseg3[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg3[j].x = x + Vma_tab.size*0.004;
			xseg3[j].y = tempy;
			j++;
			gpl(2,xseg3);
			}

			// code for right side line
			y = y - Vma_tab.size*0.0015;
			x = x + Vma_tab.size*0.004;

			j = 0;
			xseg1[j].x = x ;
			xseg1[j].y = y;
			j++;
	
			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;

			xseg1[j].x = x ;
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;

			gpl(4, xseg1);



		pe++;
		}
		break;






	//w29
	case GMK_w29:


		// code of w17
		// its part of w13 and other few lines
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			j = 0,k = 0;
			s = Vma_tab.size/1000;

			//code for w13
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			// code for left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.0010;
			tempy = xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			// code for right down line
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;
			// code for arrow
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			xseg[j].x = xseg[j-1].x;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
			j++;


			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			
			xseg[j].x = xseg[j-3].x - Vma_tab.size*0.0004;
			xseg[j].y = xseg[j-3].y;
			j++;
			
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			gpl(8,xseg);

			// code for top horizontal line
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			
			xseg1[j].x = x - Vma_tab.size*0.0024;
			xseg1[j].y = y;
			j++;
			
			// code for vertical line
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;
			
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.002;
			j++;
			
			gpl(4,xseg1);



			// code for right side line
			y = y - Vma_tab.size*0.0025;
			x = x + Vma_tab.size*0.0005;

			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.003; 
			j++;

			xseg1[j].x = x ; 
			xseg1[j].y = y + Vma_tab.size*0.003; 
			j++;
			
			gpl(4, xseg1);


		pe++;
		}

		break;





	//w30

	case GMK_w30:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			angle1 = 0;
			j = 0,k = 0;

			// code of w09
			//code for "S"
			n = 20;
			add_angle = PI_V/n ;
			for( k = 0;k <n+(n/2); k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			tempx = xseg[j-1].x;
			tempy = xseg[j-1].y;
			y = y - Vma_tab.size*0.002;
			add_angle = PI_V/n ;
			for( k = n/2; k >-(n); k--) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}

			gpl(60,xseg);



			//code for horizontal right hand arrow mark         

			{         

			x = pe->x;
			y = pe->y;

			j = 0;
			startx = xseg1[j].x = tempx - Vma_tab.size*0.002;
			starty = xseg1[j].y = tempy; 
			j++;
			xseg1[j].x = tempx + Vma_tab.size*0.002;
			xseg1[j].y = tempy;
			j++;

			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;

			xseg1[j].x = xseg1[j-2].x;
			xseg1[j].y = xseg1[j-2].y;
			j++;
			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0006;
			j++;


			gpl(5,xseg1);

			}


			// code for right side line
			y = y - Vma_tab.size*0.002;
			x = x + Vma_tab.size*0.001;

			j = 0;
			xseg2[j].x = x + Vma_tab.size*0.003; 
			xseg2[j].y = y - Vma_tab.size*0.001;
			j++;
			xseg2[j].x = x + Vma_tab.size*0.003;
			xseg2[j].y = y + Vma_tab.size*0.003;
			j++;

			gpl(2, xseg2);


		pe++;
		}

		break;



	//w31
	case GMK_w31:

		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			
			angle1 = 0; 
			j = 0, k = 0;


			// code of w09
			//code for "S"
			n = 20;
			add_angle = PI_V/n ;
			for( k = 0; k <n+(n/2); k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			tempx = xseg[j-1].x;
			tempy = xseg[j-1].y;
			y = y - Vma_tab.size*0.002;
			add_angle = PI_V/n ;
			for( k = n/2;k >-(n); k--) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}

			gpl(60,xseg);


			//code for horizontal right hand arrow mark         
			{         

			x = pe->x;
			y = pe->y;

			j = 0;
			startx = xseg1[j].x = tempx - Vma_tab.size*0.002;
			starty = xseg1[j].y = tempy;
			j++;
			
			xseg1[j].x = tempx + Vma_tab.size*0.002;
			xseg1[j].y = tempy;
			j++;

			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;

			xseg1[j].x = xseg1[j-2].x;
			xseg1[j].y = xseg1[j-2].y;
			j++;
			
			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0006;
			j++;


			gpl(5,xseg1);

			}


		pe++;
		}

		break;




	//w32
	case GMK_w32:

		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			
			angle1 = 0; 
			j = 0, k = 0;


			// code of w09
			//code for "S"
			n = 20;
			add_angle = PI_V/n ;
			for( k = 0; k <n+(n/2); k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			tempx = xseg[j-1].x;
			tempy = xseg[j-1].y;
			y = y - Vma_tab.size*0.002;
			add_angle = PI_V/n ;
			for( k =n/2; k >-(n); k--) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}

			gpl(60,xseg);

			//code for horizontal right hand arrow mark         
			{         

			x = pe->x;
			y = pe->y;

			j = 0;
			startx = xseg1[j].x = tempx - Vma_tab.size*0.002;
			starty = xseg1[j].y = tempy;
			j++;
			
			xseg1[j].x = tempx + Vma_tab.size*0.002;
			xseg1[j].y = tempy;
			j++;

			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;


			xseg1[j].x = xseg1[j-2].x;
			xseg1[j].y = xseg1[j-2].y;
			j++;
			
			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0006;
			j++;


			gpl(5,xseg1);

			}

			// code for right side line
			y = y - Vma_tab.size*0.002;
			j = 0;
			xseg2[j].x = x - Vma_tab.size*0.003; 
			xseg2[j].y = y - Vma_tab.size*0.001; 
			j++;
			
			xseg2[j].x = x - Vma_tab.size*0.003; 
			xseg2[j].y = y + Vma_tab.size*0.003; 
			j++;

			gpl(2, xseg2);


		pe++;
		}

		break;




	//w33
	case GMK_w33:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			angle1 = 0; 
			j = 0, k = 0;


			// code of w09
			//code for "S"
			n = 20;
			add_angle = PI_V/n ;
			for( k = 0; k <n+(n/2); k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			tempx = xseg[j-1].x;
			tempy = xseg[j-1].y;
			y = y - Vma_tab.size*0.002;
			add_angle = PI_V/n ;
			for( k = n/2; k >-(n); k--) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}

			gpl(60,xseg);



			//code for horizontal right hand arrow mark         

			{         

			x = pe->x;
			y = pe->y;

			j = 0;
			startx = xseg1[j].x = tempx - Vma_tab.size*0.002; 
			starty = xseg1[j].y = tempy - Vma_tab.size*0.0002;
			j++;
			
			xseg1[j].x = tempx + Vma_tab.size*0.002;
			xseg1[j].y = tempy - Vma_tab.size*0.0002;
			j++;

			k = 0;
			xseg2[k].x = tempx - Vma_tab.size*0.002; 
			xseg2[k].y = tempy + Vma_tab.size*0.0002;
			k++;
			
			xseg2[k].x = tempx + Vma_tab.size*0.002;
			xseg2[k].y = tempy + Vma_tab.size*0.0002;
			k++;
			
			gpl(2,xseg2);

			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;


			xseg1[j].x = xseg1[j-2].x + Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-2].y + Vma_tab.size*0.0003;
			j++;

			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y +Vma_tab.size*0.0008;
			j++;

			xseg1[j].x = xseg2[k-1].x;
			xseg1[j].y = xseg2[k-1].y;
			j++;


			gpl(6,xseg1);

			}


			// code for right side line
			y = y - Vma_tab.size*0.002;

			j = 0;
			xseg2[j].x = x + Vma_tab.size*0.0035;
			xseg2[j].y = y - Vma_tab.size*0.001;
			j++;
			xseg2[j].x = x + Vma_tab.size*0.0035; 
			xseg2[j].y = y + Vma_tab.size*0.003;
			j++;

			gpl(2, xseg2);



		pe++;
		}

		break;




	//w34
	case GMK_w34:

		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			
			angle1 = 0;
			j = 0, k = 0;


			// code of w09
			//code for "S"
			n = 20;
			add_angle = PI_V/n ;
			for( k = 0; k <n+(n/2); k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			tempx = xseg[j-1].x;
			tempy = xseg[j-1].y;
			y = y - Vma_tab.size*0.002;
			add_angle = PI_V/n ;
			for( k = n/2; k >-(n); k--) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}

			gpl(60,xseg);


			//code for horizontal right hand arrow mark         

			{         

			x = pe->x;
			y = pe->y;
			     
			j = 0;
			startx = xseg1[j].x = tempx - Vma_tab.size*0.002;
			starty = xseg1[j].y = tempy - Vma_tab.size*0.0002;
			j++;
			
			xseg1[j].x = tempx + Vma_tab.size*0.002;
			xseg1[j].y = tempy - Vma_tab.size*0.0002;
			j++;

			k = 0;
			xseg2[k].x = tempx - Vma_tab.size*0.002; 
			xseg2[k].y = tempy + Vma_tab.size*0.0002;
			k++;
			
			xseg2[k].x = tempx + Vma_tab.size*0.002;
			xseg2[k].y = tempy + Vma_tab.size*0.0002;
			k++;
			
			gpl(2,xseg2);


			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;


			xseg1[j].x = xseg1[j-2].x + Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-2].y + Vma_tab.size*0.0003;
			j++;

			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0008;
			j++;

			xseg1[j].x = xseg2[k-1].x;
			xseg1[j].y = xseg2[k-1].y;
			j++;


			gpl(6,xseg1);

			}


		pe++;
		}

		break;





	//w35

	case GMK_w35:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			angle1 = 0; 
			j = 0, k = 0;


			// code of w09
			//code for "S"
			n = 20;
			add_angle = PI_V/n ;
			for( k = 0; k <n+(n/2); k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			tempx = xseg[j-1].x;
			tempy = xseg[j-1].y;
			y = y - Vma_tab.size*0.002;
			add_angle = PI_V/n ;
			for( k = n/2; k >-(n); k--) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}

			gpl(60,xseg);



			//code for horizontal right hand arrow mark         

			{         

			x = pe->x;
			y = pe->y;
			     
			j = 0;
			startx = xseg1[j].x = tempx - Vma_tab.size*0.002;
			starty = xseg1[j].y = tempy - Vma_tab.size*0.0002;
			j++;
			
			xseg1[j].x = tempx + Vma_tab.size*0.002;
			xseg1[j].y = tempy - Vma_tab.size*0.0002;
			j++;

			k = 0;
			xseg2[k].x = tempx - Vma_tab.size*0.002;
			xseg2[k].y = tempy + Vma_tab.size*0.0002;
			k++;
			
			xseg2[k].x = tempx + Vma_tab.size*0.002;
			xseg2[k].y = tempy + Vma_tab.size*0.0002;
			k++;
			
			gpl(2,xseg2);


			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;


			xseg1[j].x = xseg1[j-2].x + Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-2].y + Vma_tab.size*0.0003;
			j++;

			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0008;
			j++;

			xseg1[j].x = xseg2[k-1].x;
			xseg1[j].y = xseg2[k-1].y;
			j++;


			gpl(6,xseg1);

			}


			// code for left side line

			y = y - Vma_tab.size*0.002;

			j = 0;
			xseg2[j].x = x - Vma_tab.size*0.003; 
			xseg2[j].y = y - Vma_tab.size*0.001;
			j++;

			xseg2[j].x = x - Vma_tab.size*0.003; 
			xseg2[j].y = y + Vma_tab.size*0.003;
			j++;

			gpl(2, xseg2);

		pe++;
		}

		break;




	//w36
	case GMK_w36:

		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			
			j = 0, k = 0;

			//code for horizontal right hand arrow mark         

			{         

			startx = xseg1[j].x = x - Vma_tab.size*0.002;
			starty = xseg1[j].y = y;
			j++;
			
			xseg1[j].x = x + Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;


			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;

			xseg1[j].x = xseg1[j-2].x;
			xseg1[j].y = xseg1[j-2].y;
			j++;
			
			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0006;
			j++;

			gpl(5,xseg1);

			}


			//code for vertical down hand arrow mark         

			{   j = 0;      

			xseg[j].x = x ;
			starty = xseg[j].y = y + Vma_tab.size*0.0015;
			j++;
			
			xseg[j].x = x ;
			xseg[j].y = y - Vma_tab.size*0.0015;
			j++;

			xseg[j].x = xseg[j-1].x - Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0006;
			j++;


			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			
			xseg[j].x = xseg[j-3].x + Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-3].y + Vma_tab.size*0.0006;
			j++; 
			
			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++; 

			gpl(6,xseg);

			}

		pe++;
		}

		break;




	//w37
	case GMK_w37:

		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			
			j = 0, k = 0;

			//code for horizontal right hand arrow mark         

			{         

			x = pe->x;
			y = pe->y;
			     
			j = 0;
			startx = xseg1[j].x = x - Vma_tab.size*0.002; 
			starty = xseg1[j].y = y - Vma_tab.size*0.0002;
			j++;
			
			xseg1[j].x = x + Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.0002;
			j++;

			k = 0;
			xseg2[k].x = x - Vma_tab.size*0.002;
			xseg2[k].y = y + Vma_tab.size*0.0002;
			k++;
			
			xseg2[k].x = x + Vma_tab.size*0.002;
			xseg2[k].y = y + Vma_tab.size*0.0002;
			k++;
			
			gpl(2,xseg2);


			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;


			xseg1[j].x = xseg1[j-2].x + Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-2].y + Vma_tab.size*0.0003;
			j++;

			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0008;
			j++;

			xseg1[j].x = xseg2[k-1].x;
			xseg1[j].y = xseg2[k-1].y;
			j++;


			gpl(6,xseg1);

			}

			//code for vertical down hand arrow mark         

			{  j = 0;      

			xseg[j].x = x ;
			starty = xseg[j].y = y + Vma_tab.size*0.0015;
			j++;
			xseg[j].x = x ;
			xseg[j].y = y - Vma_tab.size*0.002;
			j++;

			xseg[j].x = xseg[j-1].x - Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0006;
			j++;


			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			
			xseg[j].x = xseg[j-3].x + Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-3].y + Vma_tab.size*0.0006;
			j++; 

			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++; 


			gpl(6,xseg);

			}


		pe++;
		}

		break;




	//w38
	case GMK_w38:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			j = 0,k = 0;

			//code for horizontal right hand arrow mark         

			{         

			startx = xseg1[j].x = x - Vma_tab.size*0.002;
			starty = xseg1[j].y = y;
			j++;
			
			xseg1[j].x = x + Vma_tab.size*0.002;
			xseg1[j].y = y ;
			j++;


			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;


			xseg1[j].x = xseg1[j-2].x;
			xseg1[j].y = xseg1[j-2].y;
			j++;
			
			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0006;
			j++;


			gpl(5,xseg1);

			}



			//code for vertical up hand arrow mark         

			{   j = 0;      

			xseg[j].x = x;
			starty = xseg[j].y = y - Vma_tab.size*0.0015;
			j++;
			
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.0015;
			j++;

			xseg[j].x = xseg[j-1].x - Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-1].y - Vma_tab.size*0.0006;
			j++;


			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			
			xseg[j].x = xseg[j-3].x + Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-3].y - Vma_tab.size*0.0006;
			j++; 
			
			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++; 

			gpl(6,xseg);

			}

		pe++;
		}

		break;





	//w39
	case GMK_w39:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			j = 0, k = 0;

			//code for horizontal right hand arrow mark         

			{         

			x = pe->x;
			y = pe->y;

			j = 0;
			startx = xseg1[j].x = x - Vma_tab.size*0.002;
			starty = xseg1[j].y = y - Vma_tab.size*0.0002;
			j++;
			
			xseg1[j].x = x + Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.0002;
			j++;

			k = 0;
			xseg2[k].x = x - Vma_tab.size*0.002;
			xseg2[k].y = y + Vma_tab.size*0.0002;
			k++;
			
			xseg2[k].x = x + Vma_tab.size*0.002; 
			xseg2[k].y = y + Vma_tab.size*0.0002;
			k++;
			
			gpl(2,xseg2);


			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;

			xseg1[j].x = xseg1[j-2].x + Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-2].y + Vma_tab.size*0.0003;
			j++;

			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0008;
			j++;

			xseg1[j].x = xseg2[k-1].x;
			xseg1[j].y = xseg2[k-1].y;
			j++;


			gpl(6,xseg1);

			}

			//code for vertical up hand arrow mark         

			{   j = 0;      

			xseg[j].x = x;
			starty = xseg[j].y = y - Vma_tab.size*0.0015;
			j++;
			
			xseg[j].x = x ;
			xseg[j].y = y + Vma_tab.size*0.0015;
			j++;

			xseg[j].x = xseg[j-1].x - Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-1].y - Vma_tab.size*0.0006;
			j++;

			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			
			xseg[j].x = xseg[j-3].x + Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-3].y - Vma_tab.size*0.0006;
			j++; 
			
			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++; 

			gpl(6,xseg);

			}


		pe++;
		}

		break;




	//w40
	case GMK_w40:

		for ( i = 0; i < npts; i++) {
			startx =   x = pe->x;
			starty =   y = pe->y;
			
			j = 0,k = 0;
			s = Vma_tab.size/1000;

			//first line
			{
			j = 0;
			xseg5[j].x = x;
			tempy = xseg5[j].y = y - Vma_tab.size*0.0005;
			j++;
			xseg5[j].x = x + Vma_tab.size*0.0025;
			xseg5[j].y = tempy;
			j++;
			gpl(2,xseg5);
			}
			
			//second line
			{
			k = 0;
			xseg1[k].x = x;
			xseg1[k].y = y;
			k++;
			xseg1[k].x = x + Vma_tab.size*0.0025;
			xseg1[k].y = y;
			k++;
			gpl(2,xseg1);
			}
			
			//third line
			{
			k = 0;
			xseg6[k].x = x;
			xseg6[k].y = y + Vma_tab.size*0.0005;
			k++;
			xseg6[k].x = x + Vma_tab.size*0.0025;
			xseg6[k].y = y + Vma_tab.size*0.0005;
			k++;
			gpl(2,xseg6);
			}


			//code for right bracket in w09
			{
			x = x + Vma_tab.size*0.0025;

			add_angle = PI_V/24;
			angle1 = 0; 
			j = 0;
			//code for right down bracket

			for ( k = 0; k <8; k++) {
				xseg[j].x = x + Vma_tab.size*0.001*cos(angle1);
				xseg[j].y = y - Vma_tab.size*0.001*sin(angle1);
				++j;
				angle1 += add_angle;
				}
				gpl(8, xseg);
				
			//code for right up bracket
			angle1 = 0;
			j = 0;
			for ( k = 0; k <8; k++) {
				xseg2[j].x = x +Vma_tab.size*0.001*cos(angle1);
				xseg2[j].y = y + Vma_tab.size*0.001*sin(angle1);
				++j;
				angle1 += add_angle;
				}  
				gpl(8, xseg2);

			}

			
			//code for left bracket in w09
			{
			x =  x - Vma_tab.size*0.0025;
			j = 0;
			n = 20;
			add_angle = (PI_V/n) ;
			angle2 = 0;
				
			//code for left down bracket
			for( k = 0; k <7; k++){
				xseg3[j].x = x - Vma_tab.size*0.001*cos(angle2);
				xseg3[j].y = y + Vma_tab.size*0.001*sin(angle2);
				++j;
				angle2 += add_angle;
				}             
				gpl(7,xseg3);
				
			//code for left down bracket
			j = 0; angle2 = 0;

			add_angle = (PI_V/n) ;
			for( k = 0; k <7; k++){
				xseg4[j].x = x - Vma_tab.size*0.001*cos(angle2);
				xseg4[j].y = y - Vma_tab.size*0.001*sin(angle2);
				++j;
				angle2 += add_angle;
				}             
				gpl(7,xseg4);

			}


		pe++;
		}
		break;




	//w41

	case GMK_w41:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			j = 0,k = 0;
			s = Vma_tab.size/1000;

			//first line - first half
			{
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y + Vma_tab.size*0.0005;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = tempy;
			j++;
			gpl(2,xseg);
			}

			//first line - second half
			{
			j = 0;
			tempx = xseg3[j].x = x + Vma_tab.size*0.0015;
			tempy = xseg3[j].y = y + Vma_tab.size*0.0005;
			j++;
			xseg3[j].x = tempx + Vma_tab.size*0.001;
			xseg3[j].y = tempy;
			j++;
			gpl(2,xseg3);
			}
			
			//middle line
			{
			k = 0;
			xseg1[k].x = x;
			xseg1[k].y = y;
			k++;
			xseg1[k].x = x + Vma_tab.size*0.0025;
			xseg1[k].y = y;
			k++;
			gpl(2,xseg1);
			}
			
			// third line - first half
			{
			k = 0;
			xseg2[k].x = x;
			xseg2[k].y = y - Vma_tab.size*0.0005;
			k++;
			xseg2[k].x = x + Vma_tab.size*0.001;
			xseg2[k].y = y - Vma_tab.size*0.0005;
			k++;
			gpl(2,xseg2);
			}

			// third line - second half
			{
			k = 0;
			tempx = xseg4[k].x = x + Vma_tab.size*0.0015;
			xseg4[k].y = y - Vma_tab.size*0.0005;
			k++;
			xseg4[k].x = tempx + Vma_tab.size*0.001;
			xseg4[k].y = y - Vma_tab.size*0.0005;
			k++;
			gpl(2,xseg4);
			}


		pe++;
		}
		break;







	//w42
	case GMK_w42:

		for  ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
		
			j = 0,k = 0;
			s = Vma_tab.size/1000;

			//first line - first half
			{
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y + Vma_tab.size*0.0005;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = tempy;
			j++;
			gpl(2,xseg);
			}

			//first line - second half
			{
			j = 0;
			tempx = xseg3[j].x = x + Vma_tab.size*0.0015;
			tempy = xseg3[j].y = y+ Vma_tab.size*0.0005;
			j++;
			xseg3[j].x = tempx + Vma_tab.size*0.001;
			xseg3[j].y = tempy;
			j++;
			gpl(2,xseg3);
			}
			
			//middle line
			{
			k = 0;
			xseg1[k].x = x;
			xseg1[k].y = y;
			k++;
			xseg1[k].x = x +Vma_tab.size*0.0025;
			xseg1[k].y = y;
			k++;
			gpl(2,xseg1);
			}
			
			// third line
			{
			k = 0;
			xseg2[k].x = x;
			xseg2[k].y = y -Vma_tab.size*0.0005;
			k++;
			xseg2[k].x = x +Vma_tab.size*0.0025;
			xseg2[k].y = y - Vma_tab.size*0.0005;
			k++;
			gpl(2,xseg2);
			}

			// code for right side line
			j = 0;
			xseg4[j].x = x + Vma_tab.size*0.003;
			xseg4[j].y = y -Vma_tab.size*0.0008;
			j++;
			xseg4[j].x = x + Vma_tab.size*0.003;
			xseg4[j].y = y + Vma_tab.size*0.0008;
			j++;

			gpl(2, xseg4);

		pe++;
		}
		break;





	//w43
	case GMK_w43:

		for (i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			j = 0,k = 0;
			s = Vma_tab.size/1000;

			//first line
			{
			j = 0;
			xseg5[j].x = x;
			tempy = xseg5[j].y = y - Vma_tab.size*0.0005;
			j++;
			xseg5[j].x = x + Vma_tab.size*0.0025;
			xseg5[j].y = tempy;
			j++;
			gpl(2,xseg5);
			}
			
			//second line
			{
			k = 0;
			xseg1[k].x =  x;
			xseg1[k].y = y;
			k++;
			xseg1[k].x = x + Vma_tab.size*0.0025;
			xseg1[k].y = y;
			k++;
			gpl(2,xseg1);
			}
			
			//third line
			{
			k = 0;
			xseg6[k].x = x;
			xseg6[k].y = y + Vma_tab.size*0.0005;
			k++;
			xseg6[k].x = x + Vma_tab.size*0.0025;
			xseg6[k].y = y + Vma_tab.size*0.0005;
			k++;
			gpl(2,xseg6);
			}

			// code for right side line
			j = 0;
			xseg4[j].x = x + Vma_tab.size*0.003;
			xseg4[j].y = y - Vma_tab.size*0.0008;
			j++;
			xseg4[j].x = x + Vma_tab.size*0.003;
			xseg4[j].y = y + Vma_tab.size*0.0008;
			j++;

			gpl(2, xseg4);

		pe++;
		}
		break;

   



	//w44
	case GMK_w44:

		for (i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;

			j = 0,k = 0;
			s = Vma_tab.size/1000;

			//first line - first half
			{
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y + Vma_tab.size*0.0005;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = tempy;
			j++;
			gpl(2,xseg);
			}

			//first line - second half
			{
			j = 0;
			tempx = xseg3[j].x = x + Vma_tab.size*0.0015;
			tempy = xseg3[j].y = y + Vma_tab.size*0.0005;
			j++;
			xseg3[j].x = tempx + Vma_tab.size*0.001;
			xseg3[j].y = tempy;
			j++;
			gpl(2,xseg3);
			}
			
			//middle line
			{
			k = 0;
			xseg1[k].x = x;
			xseg1[k].y = y;
			k++;
			xseg1[k].x = x + Vma_tab.size*0.0025;
			xseg1[k].y = y;
			k++;
			gpl(2,xseg1);
			}
			
			// third line
			{
			k = 0;
			xseg2[k].x = x;
			xseg2[k].y = y - Vma_tab.size*0.0005;
			k++;
			xseg2[k].x = x + Vma_tab.size*0.0025;
			xseg2[k].y = y - Vma_tab.size*0.0005;
			k++;
			gpl(2,xseg2);
			}
			
		pe++;
		}
		break;




	//w45
	case GMK_w45:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			j = 0,k = 0;
			s = Vma_tab.size/1000;

			//first line
			{
			j = 0;
			xseg5[j].x = x;
			tempy = xseg5[j].y = y - Vma_tab.size*0.0005;
			j++;
			xseg5[j].x = x + Vma_tab.size*0.0025;
			xseg5[j].y = tempy;
			j++;
			gpl(2,xseg5);
			}
			
			//second line
			{
			k = 0;
			xseg1[k].x = x;
			xseg1[k].y = y;
			k++;
			xseg1[k].x = x + Vma_tab.size*0.0025;
			xseg1[k].y = y;
			k++;
			gpl(2,xseg1);
			}
			
			//third line
			{
			k = 0;
			xseg6[k].x = x;
			xseg6[k].y = y + Vma_tab.size*0.0005;
			k++;
			xseg6[k].x = x + Vma_tab.size*0.0025;
			xseg6[k].y = y + Vma_tab.size*0.0005;
			k++;
			gpl(2,xseg6);
			}


		pe++;
		}
		break;




	//w46
	case GMK_w46:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			j = 0,k = 0;
			s = Vma_tab.size/1000;

			//first line - first half
			{
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y + Vma_tab.size*0.0005;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = tempy;
			j++;
			gpl(2,xseg);
			}

			//first line - second half
			{
			j = 0;
			tempx = xseg3[j].x = x + Vma_tab.size*0.0015;
			tempy = xseg3[j].y = y + Vma_tab.size*0.0005;
			j++;
			xseg3[j].x = tempx + Vma_tab.size*0.001;
			xseg3[j].y = tempy;
			j++;
			gpl(2,xseg3);
			}
			
			//middle line
			{
			k = 0;
			xseg1[k].x = x;
			xseg1[k].y = y;
			k++;
			xseg1[k].x = x + Vma_tab.size*0.0025;
			xseg1[k].y = y;
			k++;
			gpl(2,xseg1);
			}
			
			// third line
			{
			k = 0;
			xseg2[k].x = x;
			xseg2[k].y = y - Vma_tab.size*0.0005;
			k++;
			xseg2[k].x = x + Vma_tab.size*0.0025;
			xseg2[k].y = y - Vma_tab.size*0.0005;
			k++;
			gpl(2,xseg2);
			}

			// code for left side line
			j = 0;
			xseg4[j].x = x - Vma_tab.size*0.0005;
			xseg4[j].y = y - Vma_tab.size*0.0008;
			j++;
			xseg4[j].x = x - Vma_tab.size*0.0005;
			xseg4[j].y = y + Vma_tab.size*0.0008; 
			j++;
			gpl(2, xseg4);

		pe++;
		}
		break;




	//w47
	case GMK_w47:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			j = 0,k = 0;
			s = Vma_tab.size/1000;

			//first line
			{
			j = 0;
			xseg5[j].x = x;
			tempy = xseg5[j].y = y - Vma_tab.size*0.0005;
			j++;
			xseg5[j].x = x + Vma_tab.size*0.0025;
			xseg5[j].y = tempy;
			j++;
			gpl(2,xseg5);
			}
			
			//second line
			{
			k = 0;
			xseg1[k].x = x;
			xseg1[k].y = y;
			k++;
			xseg1[k].x = x + Vma_tab.size*0.0025;
			xseg1[k].y = y;
			k++;
			gpl(2,xseg1);
			}
			
			//third line
			{
			k = 0;
			xseg6[k].x = x;
			xseg6[k].y = y + Vma_tab.size*0.0005;
			k++;
			xseg6[k].x = x + Vma_tab.size*0.0025;
			xseg6[k].y = y + Vma_tab.size*0.0005;
			k++;
			gpl(2,xseg6);
			}

			// code for left side line
			j = 0;
			xseg4[j].x = x - Vma_tab.size*0.0005;
			xseg4[j].y = y - Vma_tab.size*0.0008;
			j++;
			xseg4[j].x = x - Vma_tab.size*0.0005; 
			xseg4[j].y = y + Vma_tab.size*0.0008;
			j++;

			gpl(2, xseg4);

		pe++;
		}
		break;



	//w48
	case GMK_w48:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			//first base horizontal line
			{
			 j = 0;
			xseg5[j].x = x;
			tempy = xseg5[j].y = y - Vma_tab.size*0.0007;
			j++;
			xseg5[j].x = x + Vma_tab.size*0.0028;
			xseg5[j].y = tempy;
			j++;
			gpl(2,xseg5);
			}

			// solid invert triangle
			{
			gsfais(GSOLID);
			k = 0;
			xseg2[k].x = x + Vma_tab.size*0.0015;
			xseg2[k].y = tempy;
			k++;
			xseg2[k].x = x + Vma_tab.size*0.001;
			xseg2[k].y = y;
			k++;
			xseg2[k].x = x + Vma_tab.size*0.002;
			xseg2[k].y = y;
			k++;
			xseg2[k].x = x + Vma_tab.size*0.0015;
			xseg2[k].y = tempy;
			k++;
			gfa(4,xseg2);
			}


			//second line
			{
			k = 0;
			xseg1[k].x = x;
			xseg1[k].y = y;
			k++;
			xseg1[k].x = x + Vma_tab.size*0.0028;
			xseg1[k].y = y;
			k++;
			gpl(2,xseg1);
			}

			//connection between second and third line
			{
			//left line
			xseg2[0].x = x + Vma_tab.size*0.001;
			xseg2[0].y = y;
			xseg2[1].x = x + Vma_tab.size*0.0005;
			xseg2[1].y = y + Vma_tab.size*0.0008;
			xseg2[2].x = x;
			xseg2[2].y = y + Vma_tab.size*0.0008;
			gpl(3,xseg2);

			//right line
			xseg4[0].x = x + Vma_tab.size*0.002;
			xseg4[0].y = y;
			xseg4[1].x = x + Vma_tab.size*0.0024;
			xseg4[1].y = y + Vma_tab.size*0.0008;
			xseg4[2].x = x + Vma_tab.size*0.0029;
			xseg4[2].y = y + Vma_tab.size*0.0008;
			gpl(3,xseg4);

			}

						
		pe++;
		}
		break;






	//w49
	case GMK_w49:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			j = 0,k=0;
			s = Vma_tab.size/1000;

			//first base horizontal line
			{
			j = 0;
			xseg5[j].x = x;
			tempy = xseg5[j].y = y - Vma_tab.size*0.0007;
			j++;
			xseg5[j].x = x + Vma_tab.size*0.0028;
			xseg5[j].y = tempy;
			j++;
			gpl(2,xseg5);
			}

			// solid invert triangle
			{
			gsfais(GSOLID);
			k = 0;
			xseg2[k].x = x + Vma_tab.size*0.0015;
			xseg2[k].y = tempy;
			k++;
			xseg2[k].x = x + Vma_tab.size*0.001;
			xseg2[k].y = y;
			k++;
			xseg2[k].x = x + Vma_tab.size*0.002;
			xseg2[k].y = y;
			k++;
			xseg2[k].x = x + Vma_tab.size*0.0015;
			xseg2[k].y = tempy;
			k++;
			gfa(4,xseg2);
			}


			//second line
			{
			k = 0;
			xseg1[k].x = x;
			xseg1[k].y = y;
			k++;
			xseg1[k].x = x + Vma_tab.size*0.0028;
			xseg1[k].y = y;
			k++;
			gpl(2,xseg1);
			}

			//connection between second and third line
			{
			//left line
			xseg2[0].x = x + Vma_tab.size*0.001;
			xseg2[0].y = y;
			xseg2[1].x = x + Vma_tab.size*0.0005;
			xseg2[1].y = y + Vma_tab.size*0.0008;
			gpl(2,xseg2);

			//right line
			xseg4[0].x = x + Vma_tab.size*0.002;
			xseg4[0].y = y;
			xseg4[1].x = x + Vma_tab.size*0.0024;
			xseg4[1].y = y + Vma_tab.size*0.0008;
			gpl(2,xseg4);

			}

			//third line
			{
			k = 0;
			xseg6[k].x = x;
			xseg6[k].y = y + Vma_tab.size*0.0008;
			k++;
			xseg6[k].x = x + Vma_tab.size*0.0028;
			xseg6[k].y = y + Vma_tab.size*0.0008;
			k++;
			gpl(2,xseg6);
			}
		
		pe++;
		}
		break;





	//w50
	case GMK_w50:

		// start of solid circle 
		gsfais(GSOLID);
		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				if ( k == n+(n/2)) {
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg);
				
			//start of solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size/3000; 
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--){	
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k = -(n/2); k <n/4; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size / 1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size / 2500*sin(angle1);
				j++;
				}
			
		gfa(35, xseg1);
		pe++;
		}
		break;



	//w51
	case GMK_w51:


		// start of solid left circle 
		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <= 2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg);
				
			//start of left solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--) {	
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k = -(n/2); k <n/4; k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + Vma_tab.size / 1500*cos(angle1);
				xseg[j].y = y + Vma_tab.size / 2500*sin(angle1);
				j++;
				}				
			gfa(35, xseg);

			// start of solid right circle 
			x = x +  Vma_tab.size*0.0015;
			y = tempy;//position of right circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/1800;
			n =20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++) {
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg1);

			//start of right solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--) {	
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k = -(n/2); k <n/4; k++) {
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size / 1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size / 2500*sin(angle1);
				j++;
				}
			gfa(35, xseg1);


		pe++;
		}
		break;




	//w52
	case GMK_w52:


		// start of solid left circle 
		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n =20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)) {
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg);

			//start of left solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--) {	
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size/1500;
			for( k = -(n/2); k <n/4; k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + Vma_tab.size / 1500*cos(angle1);
				xseg[j].y = y + Vma_tab.size / 2500*sin(angle1);
				j++;
				}
			gfa(35, xseg);

			// start of solid right circle 
			y = tempy + Vma_tab.size*0.0018;//position of right circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)) {
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg1);

			//start of right solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0; 
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--) {	
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k = -(n/2); k <n/4; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size / 1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size / 2500*sin(angle1);
				j++;
				}
			gfa(35, xseg1);

		pe++;
		}
		break;






	//w53
	case GMK_w53:


		// start of solid left circle 
		gsfais(GSOLID);
			for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				if(k == n+(n/2)) {
					tempx = x ;
					tempy = y ;
					}
				}
				gfa(41, xseg);
	
			//start of left solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005; 
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k > -(n/2); k--) {	
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k =-(n/2); k < n/4; k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + Vma_tab.size / 1500*cos(angle1);
				xseg[j].y = y + Vma_tab.size / 2500*sin(angle1);
				j++;
				}
			gfa(35, xseg);

			// start of solid right circle 
			x = x + Vma_tab.size*0.0015;
			y = tempy;//position of right circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n =20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++) {
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)) {
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg1);

			//start of right solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005; 
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--) {	
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k = -(n/2); k <n/4; k++) {
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size / 1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size / 2500*sin(angle1);
				j++;
				}
			gfa(35, xseg1);

			// start of solid top circle 
			x = tempx - Vma_tab.size*0.0008;//position of top circle
			y = tempy + Vma_tab.size*0.0015;
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0 ; k <=2*n; k++) {
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				if(k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg2);

			//start of top solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005; 
			s = Vma_tab.size / 3000;
			j = 0; 
				
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--) {	
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k = -(n/2); k <n/4; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + Vma_tab.size / 1500*cos(angle1);
				xseg2[j].y = y + Vma_tab.size / 2500*sin(angle1);
				j++;
				}
			gfa(35, xseg2);

		pe++;
		}
		break;



	//w54
		case GMK_w54:


		// start of solid first circle 
		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			y = y - Vma_tab.size*0.0018; //position of first circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++) {  
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg);

			//start of first solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--) {	
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size/1500;
			for( k =-(n/2); k <n/4; k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + Vma_tab.size / 1500*cos(angle1);
				xseg[j].y = y + Vma_tab.size / 2500*sin(angle1);
				j++;
				}
			gfa(35, xseg);

			// start of solid middle circle 
			y = pe->y;//position of middle circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++) {
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg1);

			//start of middle solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--){	
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for(k = -(n/2); k <n/4; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size / 1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size / 2500*sin(angle1);
				j++;
				}
			gfa(35, xseg1);


			// start of solid last circle 
			y = tempy + Vma_tab.size*0.0018;//position of last circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg2);

			//start of last solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--){	
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size/1500;
			for( k = -(n/2); k < n/4; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + Vma_tab.size/1500*cos(angle1);
				xseg2[j].y = y + Vma_tab.size/2500*sin(angle1);
				j++;
				}
			gfa(35, xseg2);

		pe++;
		}
		break;





	//w55
	case GMK_w55:


		// start of solid left circle 
		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0 ; k <=2*n; k++){

				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)) {
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg);

			//start of left solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--) {	
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k = -(n/2); k <n/4; k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + Vma_tab.size / 1500*cos(angle1);
				xseg[j].y = y + Vma_tab.size / 2500*sin(angle1);
				j++;
				}
			gfa(35, xseg);

			// start of solid right circle 
			x = tempx + Vma_tab.size*0.0015;
			y = tempy;//position of right circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <= 2*n; k++){

				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				if(k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg1);
				
			//start of right solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--) {	
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k =-(n/2); k <n/4; k++) {
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size/1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size/2500*sin(angle1);
				j++;
				}
			gfa(35, xseg1);


			// start of solid top circle 
			x = tempx -  Vma_tab.size*0.0008;//position of top circle
			y = tempy + Vma_tab.size*0.0015;
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n=20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)) {
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg2);

			//start of top solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--) {	
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k =-(n/2); k <n/4; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + Vma_tab.size/1500*cos(angle1);
				xseg2[j].y = y + Vma_tab.size/2500*sin(angle1);
				j++;
				}
			gfa(35, xseg2);

			// start of solid bottom circle 
			x = tempx;//position of bottom circle
			y = tempy - Vma_tab.size*0.003;
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle=PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg3[j].x = x + s*cos(angle1);
				xseg3[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)) {
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg3);

			//start of bottom solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n /2; k >-(n/2); k--){	
				angle1 = k*add_angle;
				xseg3[j].x = x + s*cos(angle1);
				xseg3[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k =-(n/2); k <n/4; k++){
				angle1 = k*add_angle;
				xseg3[j].x = x + Vma_tab.size/1500*cos(angle1);
				xseg3[j].y = y + Vma_tab.size/2500*sin(angle1);
				j++;
				}
			gfa(35, xseg3);


		pe++;
		}
		break;





//w56
case GMK_w56:


		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			
			angle1 = 0; 
			j = 0,k = 0;
			s = Vma_tab.size/1000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k < n+(n/2); k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*sin(angle1);
				xseg[j].y = y + s*cos(angle1);
				j++;
				}

			x = x - Vma_tab.size*0.002;
			y = y;
			add_angle = PI_V/n ; 
			for( k = n/2; k >-(n); k--){
				angle1 = k*add_angle;
				xseg[j].x = x + s*sin(angle1);
				xseg[j].y = y + s*cos(angle1);
				j++;
				} 
			gpl(60,xseg);

			//solid circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2200;
			n=20;
			add_angle=PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg1);

			//start of first solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--){	
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k =-(n/2); k < n/4; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size/1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size/2500*sin(angle1);
				j++;
				}
			gfa(35, xseg1);

		pe++;
		}
		break;



	//w57
	case GMK_w57:


		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0;
			angle2 = add_angle;
			j = 0,k = 0;
			s = Vma_tab.size/1000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0;k < n+(n/2); k++) {
				angle1 = k*add_angle;
				xseg[j].x = x + s*sin(angle1);
				xseg[j].y = y + s*cos(angle1);
				j++;
				}

			x = x - Vma_tab.size*0.002;
			y = y;
			add_angle = PI_V/n ; 
			for( k = n/2; k >-(n); k--){
				angle1 = k*add_angle;
				xseg[j].x = x + s*sin(angle1);
				xseg[j].y = y + s*cos(angle1);
				j++;
				} 
			gpl(60,xseg);

			//solid left circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2200;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg1);
			
			//start of left solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--){	
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size / 1500;
			for( k =-(n/2); k <n/4; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size/1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size/2500*sin(angle1);
				j++;
				}

			gfa(35, xseg1);

			// solid second circle
			x = x + Vma_tab.size*0.002;
			y = tempy;//position of second circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 2200;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg2);
			
			//start of left solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--){	
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size/1500;
			for( k = -(n/2); k < n/4; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + Vma_tab.size/1500*cos(angle1);
				xseg2[j].y = y + Vma_tab.size/2500*sin(angle1);
				j++;
				}
			gfa(35, xseg2);

		pe++;
		}
		break;


		
	//w58
	case GMK_w58:

		// start of solid bottom circle 
		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			// start of solid top circle 
			y = y + Vma_tab.size*0.0015;//position of top circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);


			// start comma
			//solid circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n = 20;
			add_angle = PI_V/n ;
			x = pe->x; 
			y = pe->y;
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				if(k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg1);
			
			//start of first solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--){	
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size/1500;
			for( k =-(n/2); k <n/4; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size/1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size/2500*sin(angle1);
				j++;
				}
		gfa(35, xseg1);
		pe++;
		}
		break;




	//w59
	case GMK_w59:

		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			// start top comma
			//solid circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n =20;
			add_angle = PI_V/n ;
			y = y + Vma_tab.size*0.0035;//position of top comma;
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg1);

			//start of first solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k = n/2; k >-(n/2); k--){	
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size/1500;
			for( k =-(n/2); k <n/4; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size/1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size/2500*sin(angle1);
				j++;
				}
			gfa(35, xseg1);

			// start of solid middle circle 
			y = pe->y;
			y = y + Vma_tab.size*0.0015;//position of middle circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 1800;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);

			// start bottom comma
			//solid circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/1800;
			n = 20;
			add_angle = PI_V/n ;
			x = pe->x; 
			y = pe->y;
			for( k = 0 ; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				if( k == n+(n/2)){
					tempx = x;
					tempy = y;
					}
				}
			gfa(41, xseg1);

			//start of first solid comma symbol
			x = tempx - Vma_tab.size*0.0001;
			y = tempy - Vma_tab.size*0.0005;
			s = Vma_tab.size / 3000;
			j = 0;
			gsfais(GSOLID);
			for( k =n/2; k >-(n/2); k--){	
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			s = Vma_tab.size/1500;
			for( k =-(n/2); k <n/4; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + Vma_tab.size/1500*cos(angle1);
				xseg1[j].y = y + Vma_tab.size/2500*sin(angle1);
				j++;
				}
			gfa(35, xseg1);

		pe++;
		}
		break;




	//w60
	case GMK_w60:


		// start of solid circle 
		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg);
			pe++;
		}
		break;




	//w61
	case GMK_w61:


		// start of solid left circle 
		gsfais(GSOLID);
		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg);

			// start of solid right circle 
			x = x + Vma_tab.size*0.0015;//position of right circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);

		pe++;
		}
		break;





	//w62
	case GMK_w62:


		// start of solid bottom circle 
		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0 ; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg);

			// start of solid top circle 
			y = y + Vma_tab.size*0.0015;//position of top circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++) {
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);

		pe++;
		}
		break;






	//w63
		case GMK_w63:


		// start of solid left circle 
		gsfais(GSOLID);
		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size / 2000;
			n=20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg);

			// start of solid right circle 
			x = x + Vma_tab.size*0.0015;//position of right circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);

			// start of solid top circle 
			x = x - Vma_tab.size*0.0008;//position of top circle
			y = y + Vma_tab.size*0.001;
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg2);
				
		pe++;
		}
		break;




	//w65
	case GMK_w65:


		// start of solid left circle 
		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg);

			// start of solid right circle 
			x = x + Vma_tab.size*0.0015;//position of right circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);


			// start of solid top circle 
			x = x - Vma_tab.size*0.0008;//position of top circle
			y = y + Vma_tab.size*0.001;
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg2);
			// start of solid bottom circle 

			//position of bottom circle
			y = y - Vma_tab.size*0.002;
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <= 2*n; k++){
				angle1 = k*add_angle;
				xseg3[j].x = x + s*cos(angle1);
				xseg3[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg3);


		pe++;
		}
		break;




	//w64
	case GMK_w64:


		// start of solid first circle 
		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;
			y=y- Vma_tab.size*0.0012; //position of first circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n=20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg);

			// start of solid middle circle 
			y = pe->y;//position of middle circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);


			// start of solid last circle 
			y = y + Vma_tab.size*0.0012;//position of last circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg2);

		pe++;
		}
		break;





	//w66
	case GMK_w66:


		for (i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; 
			j = 0,k=0;
			s = Vma_tab.size/1000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <n+(n/2); k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*sin(angle1);
				xseg[j].y = y + s*cos(angle1);
				j++;
				}
			x = x - Vma_tab.size*0.002;
			y = y;
			add_angle = PI_V/n ; 
			for( k = n/2; k >-(n); k--){
				angle1 = k*add_angle;
				xseg[j].x = x + s*sin(angle1);
				xseg[j].y = y + s*cos(angle1);
				j++;
				} 
			gpl(60,xseg);

			//solid circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);

		pe++;
		}
		break;




	//w67
	case GMK_w67:


		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; 
			j = 0,k=0;
			s = Vma_tab.size/1000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <n+(n/2); k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*sin(angle1);
				xseg[j].y = y + s*cos(angle1);
				j++;
				}

			x = x - Vma_tab.size*0.002;
			y = y;
			add_angle = PI_V/n ; 
			for( k = n/2; k >-(n); k--){
				angle1 = k*add_angle;
				xseg[j].x = x + s*sin(angle1);
				xseg[j].y = y + s*cos(angle1);
				j++;
				} 
			gpl(60,xseg);

			//solid left circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);

			// solid second circle
			x = x + Vma_tab.size*0.002;//position of second circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/2000;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg2);

		pe++;
		}
		break;




	//w68
	case GMK_w68:


		// star initialization 
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



		// start of solid bottom circle 
		gsfais(GSOLID);
		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			// start of  star
			j = 0,k = 0;

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

			// end of  star


			// start of solid top circle 
			y = y+ Vma_tab.size*0.0022;//position of top circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/1500;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);

		pe++;
		}
		break;







	//w69
	case GMK_w69:


		// star initialization 
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



		// start of solid bottom circle 
		gsfais(GSOLID);
		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;


			// start of bottom star
			j = 0,k = 0;

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

			// end of  bottom star



			// start of solid middle circle 
			y = y + Vma_tab.size*0.0022;//position of top circle
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/1500;
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);


			// start of  top star
			y = y + Vma_tab.size*0.0022;//position of top star
			j = 0,k = 0;

			xseg2[0].x = x + star1[0].x;
			xseg2[0].y = y + star1[0].y;
			xseg2[1].x = x + star1[1].x;
			xseg2[1].y = y + star1[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star2[0].x;
			xseg2[0].y = y + star2[0].y;
			xseg2[1].x = x + star2[1].x;
			xseg2[1].y = y + star2[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star3[0].x;
			xseg2[0].y = y + star3[0].y;
			xseg2[1].x = x + star3[1].x;
			xseg2[1].y = y + star3[1].y;
			gpl(2, xseg2);
			// end of  top star

		pe++;
		}
		break;





	//w70
	case GMK_w70:


		// star initialization 
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


		// start of solid bottom circle 
		gsfais(GSOLID);
		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			// start of  star
			j = 0, k = 0;

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

			// end of   star

		pe++;
		}
		break;




	//w71
	case GMK_w71:


		// star initialization 
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


		gsfais(GSOLID);
		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			// start of left star
			j = 0, k = 0;

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

			// end of  left star


			// start of  right star
			x = x + Vma_tab.size*0.0025;//position of right star
			j = 0, k = 0;

			xseg2[0].x = x + star1[0].x;
			xseg2[0].y = y + star1[0].y;
			xseg2[1].x = x + star1[1].x;
			xseg2[1].y = y + star1[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star2[0].x;
			xseg2[0].y = y + star2[0].y;
			xseg2[1].x = x + star2[1].x;
			xseg2[1].y = y + star2[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star3[0].x;
			xseg2[0].y = y + star3[0].y;
			xseg2[1].x = x + star3[1].x;
			xseg2[1].y = y + star3[1].y;
			gpl(2, xseg2);

			// end of  right star


		pe++;
		}
		break;








	//w72
	case GMK_w72:


		// star initialization 
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

		gsfais(GSOLID);
		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			// start of bottom star
			j = 0, k = 0;

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

			// end of  bottom star


			// start of  top star
			y = y + Vma_tab.size*0.002;//position of right star
			j = 0, k = 0;

			xseg2[0].x = x + star1[0].x;
			xseg2[0].y = y + star1[0].y;
			xseg2[1].x = x + star1[1].x;
			xseg2[1].y = y + star1[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star2[0].x;
			xseg2[0].y = y + star2[0].y;
			xseg2[1].x = x + star2[1].x;
			xseg2[1].y = y + star2[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star3[0].x;
			xseg2[0].y = y + star3[0].y;
			xseg2[1].x = x + star3[1].x;
			xseg2[1].y = y + star3[1].y;
			gpl(2, xseg2);

			// end of  top star


		pe++;
		}
		break;






	//w73
	case GMK_w73:


		// star initialization 
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


		gsfais(GSOLID);
		for (i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;


			// start of left star
			j = 0, k = 0;

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

			// end of  left star


			// start of  right star
			x = x + Vma_tab.size*0.0025;//position of right star
			j = 0, k = 0;

			xseg2[0].x = x + star1[0].x;
			xseg2[0].y = y + star1[0].y;
			xseg2[1].x = x + star1[1].x;
			xseg2[1].y = y + star1[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star2[0].x;
			xseg2[0].y = y + star2[0].y;
			xseg2[1].x = x + star2[1].x;
			xseg2[1].y = y + star2[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star3[0].x;
			xseg2[0].y = y + star3[0].y;
			xseg2[1].x = x + star3[1].x;
			xseg2[1].y = y + star3[1].y;
			gpl(2, xseg2);

			// end of  right star


			// start of  top star
			x = x - Vma_tab.size*0.0012;
			y = y + Vma_tab.size*0.0025;//position of top star
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  top star


		pe++;
		}
		break;





	//w74
	case GMK_w74:


		// star initialization 
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




		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			// start of bottom star
			j = 0, k = 0;

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

			// end of  bottom star


			// start of middle star
			y = y + Vma_tab.size*0.002;//position of middle star
			j = 0, k = 0;

			xseg2[0].x = x + star1[0].x;
			xseg2[0].y = y + star1[0].y;
			xseg2[1].x = x + star1[1].x;
			xseg2[1].y = y + star1[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star2[0].x;
			xseg2[0].y = y + star2[0].y;
			xseg2[1].x = x + star2[1].x;
			xseg2[1].y = y + star2[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star3[0].x;
			xseg2[0].y = y + star3[0].y;
			xseg2[1].x = x + star3[1].x;
			xseg2[1].y = y + star3[1].y;
			gpl(2, xseg2);

			// end of  middle star


			// start of  top star
			y = y + Vma_tab.size*0.002;//position of top star
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  top star

		pe++;
		}
		break;




	//w75
	case GMK_w75:


		// star initialization 
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


		gsfais(GSOLID);
		for ( i = 0; i < npts; i++) {
			x = pe->x;
			y = pe->y;

			// start of left star
			j = 0, k = 0;

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

			// end of  left star

			// start of  right star
			x = x + Vma_tab.size*0.0025;//position of right star
			j = 0, k = 0;

			xseg2[0].x = x + star1[0].x;
			xseg2[0].y = y + star1[0].y;
			xseg2[1].x = x + star1[1].x;
			xseg2[1].y = y + star1[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star2[0].x;
			xseg2[0].y = y + star2[0].y;
			xseg2[1].x = x + star2[1].x;
			xseg2[1].y = y + star2[1].y;
			gpl(2, xseg2);
			xseg2[0].x = x + star3[0].x;
			xseg2[0].y = y + star3[0].y;
			xseg2[1].x = x + star3[1].x;
			xseg2[1].y = y + star3[1].y;
			gpl(2, xseg2);

			// end of  right star


			// start of  top star
			x = x - Vma_tab.size*0.0012;
			y = y + Vma_tab.size*0.0025;//position of top star
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  top star


			// start of bottom star

			y = y - Vma_tab.size*0.005;//position of bottom star
			j = 0, k = 0;

			xseg4[0].x = x + star1[0].x;
			xseg4[0].y = y + star1[0].y;
			xseg4[1].x = x + star1[1].x;
			xseg4[1].y = y + star1[1].y;
			gpl(2, xseg4);
			xseg4[0].x = x + star2[0].x;
			xseg4[0].y = y + star2[0].y;
			xseg4[1].x = x + star2[1].x;
			xseg4[1].y = y + star2[1].y;
			gpl(2, xseg4);
			xseg4[0].x = x + star3[0].x;
			xseg4[0].y = y + star3[0].y;
			xseg4[1].x = x + star3[1].x;
			xseg4[1].y = y + star3[1].y;
			gpl(2, xseg4);

			// end of  bottom star


		pe++;
		}
		break;





	//w76
	case GMK_w76:

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0, k = 0;

			//code for horizontal line right hand arrow mark         

			{         

			startx = xseg1[j].x = x - Vma_tab.size*0.002;
			starty = xseg1[j].y = y;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.002;
			xseg1[j].y = y ;
			j++;
			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0006;
			j++;
			xseg1[j].x = xseg1[j-2].x;
			xseg1[j].y = xseg1[j-2].y;
			j++;
			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0006;
			j++;
				  
			gpl(5,xseg1);
			
			//left side arrow mark	
			j = 0;
			xseg[j].x = startx;
			xseg[j].y = starty;
			j++;
			xseg[j].x = xseg[j-1].x + Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-1].y - Vma_tab.size*0.0006;
			j++;
			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			xseg[j].x = xseg[j-3].x + Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-3].y + Vma_tab.size*0.0006;
			j++;
			gpl(4,xseg);	

			}



		pe++;
		}
		break;





	//w77
	case GMK_w77:


		for (i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			// setting regular triangle position
			j = 0;
			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y;
			j++;
			xseg[j].x = x ;
			xseg[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y;
			j++;
			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			gpl(5, xseg);

			// horizontal line
			j = 0;
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y + Vma_tab.size*0.0005;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.002;
			xseg1[j].y = y + Vma_tab.size*0.0005;
			j++;
			gpl(2, xseg1);
		pe++;
		}
		break;







	//w78
	case GMK_w78:

		for (i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0, k = 0;

			//code for cross mark

			//right cross line
			xseg2[j].x = x; 
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x + Vma_tab.size*0.002; 
			xseg2[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(2,xseg2);

			//left cross line	
			j = 0;
			xseg3[j].x = x;
			xseg3[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg3[j].x = x + Vma_tab.size*0.002;
			xseg3[j].y = y;
			j++;
			gpl(2,xseg3);

			//code for horizontal line right hand arrow mark         
			{         
			j = 0;
			xseg1[j].x = x - Vma_tab.size*0.002; 
			xseg1[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0002;
			j++;
			xseg1[j].x = xseg1[j-2].x;
			xseg1[j].y = xseg1[j-2].y;
			j++;
			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0006;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0002;
			j++;
			gpl(5,xseg1);
			
			//left side arrow mark	
			j = 0;
			xseg[j].x = x + Vma_tab.size*0.004;
			xseg[j].y = y + Vma_tab.size*0.001 ;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg[j].x = xseg[j-1].x + Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-1].y - Vma_tab.size*0.0002;
			j++;
			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			xseg[j].x = xseg[j-3].x + Vma_tab.size*0.0006;
			xseg[j].y = xseg[j-3].y + Vma_tab.size*0.0002;
			j++;
			gpl(5,xseg);	

			}


		pe++;
		}
		break;




	//w79

	case GMK_w79:


		gsfais(GSOLID);
		for (i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; 
			j = 0,k=0;
			s = Vma_tab.size/1000;
			// start of solid circle 		
			x = x;
			y = y + Vma_tab.size*0.0004;
			angle1 = 0; 
			j = 0;
			s = Vma_tab.size/8000;//set size of circle
			n = 20;
			add_angle = PI_V/n ; 
			for( k = 0 ; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);	

			// setting regular triangle position
			j = 0;
			y = pe->y;
			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y;
			j++;
			xseg[j].x = x ;
			xseg[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y; 
			j++;
			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			gpl(5, xseg);


		pe++;
		}
		break;





	//w80
	case GMK_w80:


	for ( i = 0; i < npts; i++) {
		startx=   x = pe->x;
		starty=   y = pe->y;
		add_angle = PI_V/24;
		angle1 = 0; angle2 = add_angle;
		j = 0,k = 0;

		// code for invert triangle
		xseg[j].x = x; 
		xseg[j].y = y + Vma_tab.size*0.002;
		j++;
		xseg[j].x = x - Vma_tab.size*0.001;
		xseg[j].y = y + Vma_tab.size*0.002;
		j++;
		xseg[j].x = x ;
		xseg[j].y = y;
		j++;
		xseg[j].x = x + Vma_tab.size*0.001;
		xseg[j].y = y + Vma_tab.size*0.002;
		j++;
		xseg[j].x = x - Vma_tab.size*0.001;
		xseg[j].y = y + Vma_tab.size*0.002;
		j++;
		gpl(5, xseg);

		y = y + Vma_tab.size*0.003; // setting circle position
		gsfais(GSOLID);                                                   
		n = 20; 
		j = 0; 
		s = Vma_tab.size/2000;
		add_angle = PI_V/n ; 
		for( k = 0; k <=2*n; k++){
			angle1 = k*add_angle;
			xseg2[j].x = x + s*cos(angle1);
			xseg2[j].y = y + s*sin(angle1);
			j++;
			}
		gfa(41, xseg2);

		// end of solid circle

	pe++;
	}
	break;






	//w82
	case GMK_w82:


		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; 
			j = 0, k = 0;

			// code for invert triangle
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(5, xseg);


			y = y + Vma_tab.size*0.0028; // setting circle position
			gsfais(GSOLID);                                                   
			n = 20;
			j = 0;
			s = Vma_tab.size/2000;
			add_angle=PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg2);

			// end of solid circle

			// top most circle
			y = y + Vma_tab.size*0.0012; // setting top most circle position
			gsfais(GSOLID);                                                   
			n = 20;
			j = 0;
			s = Vma_tab.size/2000;
			add_angle=PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);

			// end of solid circle



		pe++;
		}
		break;





	//w81
	case GMK_w81:


		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; 
			j = 0, k = 0;

			// code for invert triangle
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x ;
			xseg[j].y = y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(5, xseg);

			// code for horizontal line inside triangle
			j = 0;
			xseg1[j].x = x - Vma_tab.size*0.0007;
			xseg1[j].y = y + Vma_tab.size*0.0014;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.0007;
			xseg1[j].y = y + Vma_tab.size*0.0014;
			j++;
			gpl(2, xseg1);

			y = y + Vma_tab.size*0.003; // setting circle position
			gsfais(GSOLID);                                                   
			n = 20;
			j = 0;
			s = Vma_tab.size/2000;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg2);

			// end of solid circle

		pe++;
		}
		break;





	//w83
	case GMK_w83:



		// star initialization 
		s = Vma_tab.size/2000;//size of star
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




		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; 
			j = 0, k = 0;

			// code for invert triangle
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(5, xseg);

			// start of  top star

			y = y + Vma_tab.size*0.0028;//setting star position
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  top star	

			// top most circle
			y = y+ Vma_tab.size*0.0012; // setting top most circle position
			gsfais(GSOLID);                                                   
			n = 20;
			j = 0;
			s = Vma_tab.size/2000;
			add_angle = PI_V/n ; 
			for( k =0 ; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg1[j].x = x + s*cos(angle1);
				xseg1[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg1);

			// end of solid circle

		pe++;
		}
		break;





	//w84
	case GMK_w84:

		// star initialization 
		s = Vma_tab.size/2000;//size of star
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

		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; 
			j = 0, k = 0;

			// code for invert triangle
			xseg[j].x = x ;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x ;
			xseg[j].y = y ;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(5, xseg);

			// code for horizontal line inside triangle
			j = 0;
			xseg1[j].x = x - Vma_tab.size*0.0007;
			xseg1[j].y = y+ Vma_tab.size*0.0014;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.0007;
			xseg1[j].y = y +Vma_tab.size*0.0014;
			j++;
			gpl(2, xseg1);


			// start of  top star
			y = y + Vma_tab.size*0.0028;//setting star position
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  top star	


			// top most circle
			y = y + Vma_tab.size*0.0012; // setting top most circle position
			gsfais(GSOLID);                                                   
			n = 20;
			j = 0;
			s = Vma_tab.size/2000;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg2);

			// end of solid circle


		pe++;
		}
		break;




	//w85
	case GMK_w85:



		// star initialization 
		s = Vma_tab.size/2000;//size of star
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




		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			j = 0, k = 0;

			// code for invert triangle
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x ;
			xseg[j].y = y ;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(5, xseg);


			// start of  top star
			y = y + Vma_tab.size*0.0028;//setting star position
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  top star	

		pe++;
		}
		break;



	//w86
	case GMK_w86:


		// star initialization 
		s = Vma_tab.size/2000;//size of star
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




		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			j = 0, k = 0;

			// code for invert triangle
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x ;
			xseg[j].y = y ;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(5, xseg);

			// code for horizontal line inside triangle
			j = 0;
			xseg1[j].x = x - Vma_tab.size*0.0007;
			xseg1[j].y = y + Vma_tab.size*0.0014;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.0007;
			xseg1[j].y = y + Vma_tab.size*0.0014;
			j++;
			gpl(2, xseg1);


			// start of  top star
			y = y + Vma_tab.size*0.0028;//setting star position
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  top star	

		pe++;
		}
		break;





	//w87
	case GMK_w87:


		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			// code for invert triangle
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x ; 
			xseg[j].y = y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(5, xseg);


			y = y + Vma_tab.size*0.0025; // setting regular triangle [top] position
			j = 0;
			xseg2[j].x = x;
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x - Vma_tab.size*0.001;
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg2[j].x = x + Vma_tab.size*0.001;
			xseg2[j].y = y ;
			j++;
			xseg2[j].x = x;
			xseg2[j].y = y;
			j++;
			gpl(5, xseg2);
			
		pe++;
		}
		break;





	//w88
	case GMK_w88:


		for (i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			// code for invert triangle
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(5, xseg);
			
			// code for horizontal line inside triangle
			j = 0;
			xseg1[j].x = x - Vma_tab.size*0.0007;
			xseg1[j].y = y + Vma_tab.size*0.0014;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.0007;
			xseg1[j].y = y + Vma_tab.size*0.0014;
			j++;
			gpl(2, xseg1);

			y = y + Vma_tab.size*0.0025; // setting regular triangle [top] position
			j = 0;
			xseg2[j].x = x;
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x - Vma_tab.size*0.001;
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg2[j].x = x + Vma_tab.size*0.001;
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x;
			xseg2[j].y = y;
			j++;
			gpl(5, xseg2);
		
		pe++;
		}
		break;





	//w89
	case GMK_w89:


		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			// code for invert triangle
			
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(5, xseg);

			gsfais(GSOLID); 
			y = y + Vma_tab.size*0.0025; // setting regular triangle [top] position
			j = 0;
			xseg2[j].x = x;
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x - Vma_tab.size*0.001;
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg2[j].x = x + Vma_tab.size*0.001;
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x;
			xseg2[j].y = y;
			j++;
			gfa(5, xseg2);
			
		pe++;
		}
		break;




	//w90
	case GMK_w90:


		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			// code for invert triangle
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(5, xseg);
			
			// code for horizontal line inside triangle
			j = 0;
			xseg1[j].x = x - Vma_tab.size*0.0007;
			xseg1[j].y = y + Vma_tab.size*0.0014;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.0007;
			xseg1[j].y = y + Vma_tab.size*0.0014;
			j++;
			gpl(2, xseg1);

			gsfais(GSOLID); 
			y = y + Vma_tab.size*0.0025; // setting regular triangle [top] position
			j = 0;
			xseg2[j].x = x;
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x - Vma_tab.size*0.001;
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x;
			xseg2[j].y = y + Vma_tab.size*0.001;
			j++;
			xseg2[j].x = x + Vma_tab.size*0.001;
			xseg2[j].y = y;
			j++;
			xseg2[j].x = x;
			xseg2[j].y = y;
			j++;
			gfa(5, xseg2);
	
		pe++;
		}
		break;





	//w91
	case GMK_w91:


		// code of w17
		// its part of w13 and other few lines
		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; 
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			//code for w13
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			
			// code for left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.0010;
			tempy = xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			
			// code for right down line
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;
			
			// code for arrow
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;
			xseg[j].x = xseg[j-1].x;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
			j++;
			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			xseg[j].x = xseg[j-3].x - Vma_tab.size*0.0004;
			xseg[j].y = xseg[j-3].y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			gpl(8,xseg);

			// code for top horizontal line
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.0024;
			xseg1[j].y = y;
			j++;
			
			// code for vertical line
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.002;
			j++;
			gpl(4,xseg1);

			// code for right side line
			y = y - Vma_tab.size*0.0025;
			x = x + Vma_tab.size*0.0005;
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.001; 
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;
			xseg1[j].x = x;
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;
			gpl(4, xseg1);

			//code for solid circle
			x = x + Vma_tab.size*0.002;
			y = y + Vma_tab.size*0.0015; // setting circle position
			gsfais(GSOLID);                                                   
			n = 20;
			j =0 ;
			s = Vma_tab.size/2000;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg2);


		pe++;
		}

		break;





	//w92
	case GMK_w92:


		// code of w17
		// its part of w13 and other few lines
		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; 
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			//code for w13
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			
			// code for left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.0010;
			tempy = xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			
			// code for right down line
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;
			
			// code for arrow
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			xseg[j].x = xseg[j-1].x;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
			j++;

			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			
			xseg[j].x = xseg[j-3].x - Vma_tab.size*0.0004;
			xseg[j].y = xseg[j-3].y;
			j++;
			
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			gpl(8,xseg);

			// code for top horizontal line
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.0024;
			xseg1[j].y = y;
			j++;
			
			// code for vertical line
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.002;
			j++;
			gpl(4,xseg1);

			// code for right side line
			y = y - Vma_tab.size*0.0025;
			x = x + Vma_tab.size*0.0005;

			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;

			xseg1[j].x = x; 
			xseg1[j].y = y + Vma_tab.size*0.003; 
			j++;
			gpl(4, xseg1);

			//code for solid top circle
			x = x + Vma_tab.size*0.002;
			y = y+ Vma_tab.size*0.0022; // setting top circle position
			gsfais(GSOLID);                                                   
			n = 20;
			j = 0; 
			s = Vma_tab.size/2000;
			add_angle=PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg2);

			//end code for solid top circle


			//code for solid bottom circle

			y = y - Vma_tab.size*0.0012; // setting bottom circle position
			gsfais(GSOLID);                                                   
			n = 20;
			j = 0; 
			s = Vma_tab.size/2000;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg3[j].x = x + s*cos(angle1);
				xseg3[j].y = y+ s*sin(angle1);
				j++;
				}
			gfa(41, xseg3);

			//end for solid bottom circle

		pe++;
		}

		break;




	//w93
	case GMK_w93:


		// star initialization 
		s = Vma_tab.size/2000;//size of star
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


		// code of w17
		// its part of w13 and other few lines
		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			//code for w13
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			
			// code for left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.0010;
			tempy = xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			
			// code for right down line
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;
			
			// code for arrow
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			xseg[j].x = xseg[j-1].x;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;j++;


			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			xseg[j].x = xseg[j-3].x - Vma_tab.size*0.0004;
			xseg[j].y = xseg[j-3].y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			gpl(8,xseg);

			// code for top horizontal line
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.0024;
			xseg1[j].y = y;
			j++;
			
			// code for vertical line
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.002;
			j++;
			gpl(4,xseg1);


			// code for right side line
			y = y - Vma_tab.size*0.0025;
			x = x + Vma_tab.size*0.0005;
			j = 0 ;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y;
			j++;

			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;

			xseg1[j].x = x;
			xseg1[j].y = y + Vma_tab.size*0.003; 
			j++;
			gpl(4, xseg1);


			// start of  top star
			x = x + Vma_tab.size*0.002;//setting star position
			y = y + Vma_tab.size*0.0022; 
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  top star


			// code for cross line

			xseg2[0].x = x + Vma_tab.size*0.0015;
			xseg2[0].y = y; 
			xseg2[1].x = x - Vma_tab.size*0.0005;
			xseg2[1].y = y - Vma_tab.size*0.001; 
			gpl(2, xseg2);

			// code for triangle
			x = x + Vma_tab.size*0.0005;
			y = y - Vma_tab.size*0.002; // setting regular triangle [top] position
			j = 0;
			xseg4[j].x = x;
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x - Vma_tab.size*0.0008;
			xseg4[j].y = y; 
			j++;
			xseg4[j].x = x;
			xseg4[j].y = y + Vma_tab.size*0.0008;
			j++;
			xseg4[j].x = x + Vma_tab.size*0.0008;
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x;
			xseg4[j].y = y;
			j++;
			gpl(5, xseg4);


		pe++;
		}

		break;






	//w94
	case GMK_w94:


		// star initialization 
		s = Vma_tab.size/2000;//size of star
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


		// code of w17
		// its part of w13 and other few lines
		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			//code for w13
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			
			// code for left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.0010;
			tempy = xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			
			// code for right down line
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;
			
			// code for arrow
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			xseg[j].x = xseg[j-1].x;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
			j++;


			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			xseg[j].x = xseg[j-3].x - Vma_tab.size*0.0004;
			xseg[j].y = xseg[j-3].y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			gpl(8,xseg);

			// code for top horizontal line
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.0024;
			xseg1[j].y = y;
			j++;
			
			// code for vertical line
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.002;
			j++;
			gpl(4,xseg1);

			// code for right side line
			y = y- Vma_tab.size*0.0025;
			x = x + Vma_tab.size*0.0005;
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x + Vma_tab.size*0.001;
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;
			xseg1[j].x = x;
			xseg1[j].y = y + Vma_tab.size*0.003;
			j++;
			gpl(4, xseg1);


			// start of  top star
			x = x + Vma_tab.size*0.002;//setting star position
			y = y + Vma_tab.size*0.0032; 
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  top star

			// start of  bottom star
			//setting bottom position
			y = y - Vma_tab.size*0.001; 
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  bottom star


			// code for cross line

			xseg2[0].x = x + Vma_tab.size*0.0015;
			xseg2[0].y = y; 
			xseg2[1].x = x - Vma_tab.size*0.0005;
			xseg2[1].y = y - Vma_tab.size*0.001; 
			gpl(2, xseg2);

			// code for top triangle
			x = x + Vma_tab.size*0.0005;
			y = y - Vma_tab.size*0.0015; // setting regular triangle [top] position
			j = 0;
			xseg4[j].x = x; 
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x - Vma_tab.size*0.0005;
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x;
			xseg4[j].y = y + Vma_tab.size*0.0008;
			j++;
			xseg4[j].x = x + Vma_tab.size*0.0008;
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x;
			xseg4[j].y = y;
			j++;
			gpl(5, xseg4);

			// code for bottom triangle
			y = y - Vma_tab.size*0.001; // setting regular triangle [bottom] position
			j = 0;
			xseg4[j].x = x; 
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x - Vma_tab.size*0.0008;
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x ;
			xseg4[j].y = y + Vma_tab.size*0.0008;
			j++;
			xseg4[j].x = x + Vma_tab.size*0.0008;
			xseg4[j].y = y ;
			j++;
			xseg4[j].x = x;
			xseg4[j].y = y;
			j++;
			gpl(5, xseg4);

		pe++;
		}

		break;




	//w95
	case GMK_w95:


		// star initialization 
		s = Vma_tab.size/2000;//size of star
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


		// code of w17
		// its part of w13 and other few lines
		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0,k = 0;
			s = Vma_tab.size/1000;

			//code for w13
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			
			// code for left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.0010;
			tempy = xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			
			// code for right down line
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;
			
			// code for arrow
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			xseg[j].x = xseg[j-1].x;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
			j++;

			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			xseg[j].x = xseg[j-3].x - Vma_tab.size*0.0004;
			xseg[j].y = xseg[j-3].y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			gpl(8,xseg);

			// code for top horizontal line
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.0024;
			xseg1[j].y = y;
			j++;
			
			// code for vertical line
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.002;
			j++;
			gpl(4,xseg1);


			//code for solid circle
			x = x - Vma_tab.size*0.002;
			y = y + Vma_tab.size*0.0008; // setting circle position
			gsfais(GSOLID);                                                   
			n = 20;
			j = 0;
			s = Vma_tab.size/2000;
			add_angle=PI_V/n ; 
			for( k = 0 ; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg2);

			// code for cross line
			xseg3[0].x = x + Vma_tab.size*0.0014;
			xseg3[0].y = y + Vma_tab.size*0.0005; 
			xseg3[1].x = x + Vma_tab.size*0.0004;
			xseg3[1].y = y - Vma_tab.size*0.0005; 
			gpl(2, xseg3);

			// start of star
			x = x + Vma_tab.size*0.002; 
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  bottom star


		pe++;
		}
		break;





	//w96
	case GMK_w96:


		// code of w17
		// its part of w13 and other few lines
		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			//code for w13
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			
			// code for left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.0010;
			tempy = xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			
			// code for right down line
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;
			
			// code for arrow
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y- Vma_tab.size*0.0020;
			j++;

			xseg[j].x = xseg[j-1].x;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
			j++;

			xseg[j].x=xseg[j-2].x;
			xseg[j].y=xseg[j-2].y;j++;
			xseg[j].x=xseg[j-3].x-Vma_tab.size*0.0004;
			xseg[j].y=xseg[j-3].y;
			j++;
			xseg[j].x=x+Vma_tab.size*0.0001;
			xseg[j].y=y-Vma_tab.size*0.0020;
			j++;

			gpl(8,xseg);



			// code for top horizontal line
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.0024;
			xseg1[j].y = y;
			j++;
			
			// code for vertical line
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.002;
			j++;
			gpl(4,xseg1);

			// code for triangle
			x = x - Vma_tab.size*0.0012;
			y = y + Vma_tab.size*0.0008; // setting regular triangle [top] position

			j = 0;
			xseg4[j].x = x;
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x - Vma_tab.size*0.0005;
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x;
			xseg4[j].y = y + Vma_tab.size*0.0005;
			j++;
			xseg4[j].x = x + Vma_tab.size*0.0005;
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x;
			xseg4[j].y = y;
			j++;
			gpl(5, xseg4);



		pe++;
		}

		break;






	//w97
	case GMK_w97:



		// star initialization 
		s = Vma_tab.size/2000;//size of star
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


		// code of w17
		// its part of w13 and other few lines
		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0,k=0;
			s = Vma_tab.size/1000;

			//code for w13
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			
			// code for left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.0010;
			tempy = xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			
			// code for right down line
			xseg[j].x = x;
			xseg[j].y = y - Vma_tab.size*0.0015;
			j++;
			
			// code for again pending left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.001;
			tempy = xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			// code for arrow
			xseg[j].x = xseg[j-1].x + Vma_tab.size*0.0001;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
			j++;
			gpl(5,xseg);

			xseg[0].x = tempx;
			xseg[0].y = tempy;
			j++;
			xseg[1].x = tempx + Vma_tab.size*0.0004;
			xseg[1].y = tempy;
			Vma_tab.size*0.0001;
			j++;
			gpl(2,xseg);

			// code for top horizontal line
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.0024;
			xseg1[j].y = y;
			j++;
			
			// code for vertical line
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.002;
			j++;
			gpl(4,xseg1);


			//code for solid circle
			x = x - Vma_tab.size*0.002;
			y = y + Vma_tab.size*0.0008; // setting circle position
			gsfais(GSOLID);                                                   
			n = 20;
			j = 0;
			s = Vma_tab.size/2000;
			add_angle = PI_V/n ; 
			for( k = 0; k <=2*n; k++){
				angle1 = k*add_angle;
				xseg2[j].x = x + s*cos(angle1);
				xseg2[j].y = y + s*sin(angle1);
				j++;
				}
			gfa(41, xseg2);

			// code for cross line
			xseg3[0].x = x + Vma_tab.size*0.0014;
			xseg3[0].y = y + Vma_tab.size*0.0005; 
			xseg3[1].x = x + Vma_tab.size*0.0004;
			xseg3[1].y = y - Vma_tab.size*0.0005; 
			gpl(2, xseg3);


			// start of star
			x = x + Vma_tab.size*0.002; 
			j = 0, k = 0;

			xseg3[0].x = x + star1[0].x;
			xseg3[0].y = y + star1[0].y;
			xseg3[1].x = x + star1[1].x;
			xseg3[1].y = y + star1[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star2[0].x;
			xseg3[0].y = y + star2[0].y;
			xseg3[1].x = x + star2[1].x;
			xseg3[1].y = y + star2[1].y;
			gpl(2, xseg3);
			xseg3[0].x = x + star3[0].x;
			xseg3[0].y = y + star3[0].y;
			xseg3[1].x = x + star3[1].x;
			xseg3[1].y = y + star3[1].y;
			gpl(2, xseg3);

			// end of  bottom star


		pe++;
		}
		break;



	//w98
	case GMK_w98:



		// star initialization 
		s = Vma_tab.size/2000;//size of star
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


		// code of w17
		// its part of w13 and other few lines
		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0, k = 0;

			//code for w13
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			
			// code for left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.0010;
			tempy = xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			
			// code for right down line
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;
			
			// code for arrow
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y -  Vma_tab.size*0.0020;
			j++;

			xseg[j].x = xseg[j-1].x;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
			j++;

			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			xseg[j].x = xseg[j-3].x - Vma_tab.size*0.0004;
			xseg[j].y = xseg[j-3].y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			gpl(8,xseg);



			// code for top horizontal line
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.0024;
			xseg1[j].y = y;
			j++;
			
			// code for vertical line
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.002;
			j++;
			gpl(4,xseg1);


			// code for w31
			x = x - Vma_tab.size*0.0012;
			y = y + Vma_tab.size*0.002; // setting S position


			// code of w09
			//code for "S"
			n = 20;
			s = Vma_tab.size/2000;j=0;
			add_angle = PI_V/n ;
			for( k = 0; k <n+(n/2); k++){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}
			tempx = xseg[j-1].x;
			tempy = xseg[j-1].y;
			y = y - Vma_tab.size*0.001;
			add_angle = PI_V/n ;
			for( k = n/2; k >-(n); k--){
				angle1 = k*add_angle;
				xseg[j].x = x + s*cos(angle1);
				xseg[j].y = y + s*sin(angle1);
				j++;
				}

			gpl(60,xseg);


			//code for horizontal right hand arrow mark         
			{         
			// x = pe->x;
			y = y + Vma_tab.size*0.0025;
			j = 0;
			startx = xseg1[j].x = tempx - Vma_tab.size*0.001;
			starty = xseg1[j].y = tempy;
			j++;
			xseg1[j].x = tempx + Vma_tab.size*0.001;
			xseg1[j].y = tempy;
			j++;
			xseg1[j].x = xseg1[j-1].x - Vma_tab.size*0.0003;
			xseg1[j].y = xseg1[j-1].y - Vma_tab.size*0.0003;
			j++;
			xseg1[j].x = xseg1[j-2].x;
			xseg1[j].y = xseg1[j-2].y;
			j++;
			xseg1[j].x = xseg1[j-3].x - Vma_tab.size*0.0003;
			xseg1[j].y = xseg1[j-3].y + Vma_tab.size*0.0003;
			j++;

			gpl(5,xseg1);

			}


		pe++;
		}

		break;





	//w99
	case GMK_w99:


		// code of w17
		// its part of w13 and other few lines
		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			//code for w13
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			
			// code for left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.0010;
			tempy = xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			
			// code for right down line
			xseg[j].x = x;
			xseg[j].y = y - Vma_tab.size*0.0015;
			j++;
			
			// code for again pending left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.001;
			tempy = xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			// code for arrow
			xseg[j].x = xseg[j-1].x + Vma_tab.size*0.0001;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
			j++;
			gpl(5,xseg);

			xseg[0].x = tempx;
			xseg[0].y = tempy;
			j++;
			xseg[1].x =tempx +Vma_tab.size*0.0004;
			xseg[1].y =tempy;Vma_tab.size*0.0001;
			j++;
			gpl(2,xseg);



			// code for top horizontal line
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.0024;
			xseg1[j].y = y;
			j++;
			
			// code for vertical line
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.002;
			j++;
			gpl(4,xseg1);


			// code for triangle
			x = x - Vma_tab.size*0.0012;
			y = y + Vma_tab.size*0.0008; // setting regular triangle [top] position

			j = 0;
			xseg4[j].x = x;
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x - Vma_tab.size*0.0005;
			xseg4[j].y = y;
			j++;

			xseg4[j].x = x;
			xseg4[j].y = y + Vma_tab.size*0.0005;
			j++;
			xseg4[j].x = x + Vma_tab.size*0.0005;
			xseg4[j].y = y;
			j++;
			xseg4[j].x = x;
			xseg4[j].y = y;
			j++;
			gpl(5, xseg4);


		pe++;
		}
		break;




	//w200
	case GMK_w200:

		// its part of w13 and other few lines
		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0,k = 0;
			s = Vma_tab.size/1000;

			//code for w13
			j = 0;
			xseg[j].x = x;
			tempy = xseg[j].y = y;
			j++;
			
			// code for left down line
			tempx = xseg[j].x = x - Vma_tab.size*0.0010;
			tempy = xseg[j].y = y - Vma_tab.size*0.0010;
			j++;
			
			// code for right down line
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;
			
			// code for arrow
			xseg[j].x = x + Vma_tab.size*0.0001;
			xseg[j].y = y - Vma_tab.size*0.0020;
			j++;

			xseg[j].x = xseg[j-1].x;
			xseg[j].y = xseg[j-1].y + Vma_tab.size*0.0003;
			j++;

			xseg[j].x = xseg[j-2].x;
			xseg[j].y = xseg[j-2].y;
			j++;
			xseg[j].x = xseg[j-3].x - Vma_tab.size*0.0004;
			xseg[j].y = xseg[j-3].y;
			j++;
			xseg[j].x=x+Vma_tab.size*0.0001;
			xseg[j].y=y-Vma_tab.size*0.0020;
			j++;

			gpl(8,xseg);

			// code for top horizontal line
			j = 0;
			xseg1[j].x = x;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.0024;
			xseg1[j].y = y;
			j++;
			
			// code for vertical line
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y;
			j++;
			xseg1[j].x = x - Vma_tab.size*0.002;
			xseg1[j].y = y - Vma_tab.size*0.002;
			j++;
			gpl(4,xseg1);


		pe++;
		}

		break;


		
		
		
	//w201
	case GMK_w201:


		for ( i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0, k = 0;
			s = Vma_tab.size/1000;

			// code for invert triangle
			xseg[j].x = x;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y +Vma_tab.size*0.002;
			j++;
 			xseg[j].x = x;
			xseg[j].y = y;
			j++;
			xseg[j].x = x + Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			xseg[j].x = x - Vma_tab.size*0.001;
			xseg[j].y = y + Vma_tab.size*0.002;
			j++;
			gpl(5, xseg);

		pe++;
		}
		break;




	//w202
	case GMK_w202:

		// slash 

		for (i = 0; i < npts; i++) {
			startx=   x = pe->x;
			starty=   y = pe->y;
			add_angle = PI_V/24;
			angle1 = 0; angle2 = add_angle;
			j = 0, k = 0;
			s = Vma_tab.size/1000;
			j = 0;
			xseg[0].x = x + Vma_tab.size*0.0005;
			xseg[0].y = y + Vma_tab.size*0.001;
			xseg[1].x = x - Vma_tab.size*0.0005;
			xseg[1].y = y - Vma_tab.size*0.001;
			gpl(2, xseg);
		pe++;
		}

		break;




           case GMK_NONE:
               break;




           default:
               break;
        }
	/* Set the Line attributes back */
}
		
