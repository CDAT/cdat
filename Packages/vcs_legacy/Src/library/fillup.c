#define NMAX 1000

#include "gks.h"
#include "gksshort.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <errno.h>

    extern FILE *fpin,*fpout,*fperr;
    extern int IN[14][2][2],JN[14][2][2];

/*		Table of increments in i and j for isoline following.	*/

/*  int IN[14][2][2] = {
			0,0,0,1, 0,1,1,1, 0,0,1,1, 1,1,0,1, 0,0,0,1,
			0,1,0,1, 0,0,0,1, 0,1,0,0, 0,1,0,1, 0,1,0,0,
			0,1,1,1, 1,1,0,0, 1,1,0,1, 0,1,0,0
		       };
    int JN[14][2][2] = {
			0,1,0,0, 0,0,0,1, 0,1,0,1, 0,1,1,1, 0,1,1,1,
			0,0,1,1, 0,1,1,1, 1,1,0,1, 1,1,0,0, 0,0,0,1,
			1,1,0,1, 0,1,0,1, 0,1,0,0, 0,0,0,1
		       };						*/
/*
        COMPUTE THE INDEX FOR TYPE OF LINE THROUGH THE BOX
        "o" marks the start of the line. Higher values are always
        to the right of the line.

        1        2        3        4        5        6        7
        ......   ......   ......   ....\.   ./....   ..|...   ./....
        o    .   .    /   o-----   .    o   o    o   . |  .   o    .
        .\....   ....o.   ......   ......   ..../.   ..o...   ......

        8        9        10       11       12       13       14
        .o....   ..o...   ....o.   ....o.   ......   ......   ......
        /    .   . |  .   \    \   .    \   -----o   .    o   \    .
        ......   ..|...   .o....   ......   ......   ..../.   .o....


        15 is all greater than C.
        0 is all less than C.
									*/
    extern float *A;	/* Array of values.				*/
    extern short *mask;	/* Dual valued mask for (min, max) values.	*/
    extern int IM;	/* First dimension size for A and mask.		*/
    extern int JM;	/* Second dimension size for A and mask.	*/
    extern float *xv;	/* Vector of values for first dimension.	*/
    extern float *yv;	/* Vector of values for second dimension.	*/
    extern Gpoint xyu[NMAX+12];/* Vector of upper line values.		*/
    extern Gpoint xyl[NMAX+12];/* Vector of lower line values.		*/
    extern int nxyu;	/* Number of values in xyu.			*/
    extern int nxyl;	/* Number of values in xyl.			*/

/*	Fill a 01 crossing box.						*/

    int fill01 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int m,n;
       int ixj;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 1) return -1;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;

       line_it(m,c1,i0,j0,xyu,&nxyu);

       if (n > 0 && n < 15)
	 {
	  if (n != m) return -1;
	  line_it(n,c2,i0,j0,xyl,&nxyl);
	 }
       else
	 {
	  xyl[nxyl].x=xv[i0];
	  xyl[nxyl].y=yv[j0];
	 }

       return 1;
      }

/*	Fill a 02 crossing box.						*/

    int fill02 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 2) return -2;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;

       line_it(m,c1,i0,j0,xyu,&nxyu);

       if (n > 0 && n < 15)
	 {
	  if (n != m) return -2;
	  line_it(n,c2,i0,j0,xyl,&nxyl);
	 }
       else
	 {
	  xyl[nxyl].x=xv[i0+1];
	  xyl[nxyl].y=yv[j0];
	 }

       i=i0+1;
       j=j0;
       if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	  return 1;

       switch(m)
	 {
	  case 1:
	    c=fill01(c1,c2,i,j);
	    break;
	  case 3:
	    c=fill03(c1,c2,i,j);
	    break;
	  case 5:
 	    c=fill05(c1,c2,i,j);
	    break;
	  case 7:
 	    c=fill07(c1,c2,i,j);
	    break;
	  default:
	    err_warn(1,fperr,"Error - (d2) tracing isofill line.\n");
	    return -2;
	 }

       return c;
      }

/*	Fill a 03 crossing box.						*/

    int fill03 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 3) return -3;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;

       line_it(m,c1,i0,j0,xyu,&nxyu);

       xyl[nxyl].x=xv[i0];
       xyl[nxyl].y=yv[j0];
       if (n > 0 && n < 15)
	 {
	  if (n != m && n != 1 && n != 2) return -3;
	  if (n == 2 && nxyl == 0) nxyl++;
	  line_it(n,c2,i0,j0,xyl,&nxyl);
	  if (n == 1)
	    {
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	    }
	 }
       else
	 {
	  if (nxyl == 0) nxyl++;
          xyl[nxyl].x=xv[i0+1];
          xyl[nxyl].y=yv[j0];
	 }

       i=i0+1;
       j=j0;
       if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	  return 1;

       switch(m)
	 {
	  case 1:
	    c=fill01(c1,c2,i,j);
	    break;
	  case 3:
	    c=fill03(c1,c2,i,j);
	    break;
	  case 5:
 	    c=fill05(c1,c2,i,j);
	    break;
	  case 7:
 	    c=fill07(c1,c2,i,j);
	    break;
	  default:
	    err_warn(1,fperr,"Error - (d3) tracing isofill line.\n");
	    return -3;
	 }

       return c;
      }

/*	Fill a 04 crossing box.						*/

    int fill04 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 4) return -4;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;

       line_it(15-m,c1,i0,j0,xyl,&nxyl);

       if (n > 0 && n < 15)
	 {
	  if (n != m) return -4;
	  line_it(15-n,c2,i0,j0,xyu,&nxyu);
	 }
       else
	 {
	  xyu[nxyu].x=xv[i0+1];
	  xyu[nxyu].y=yv[j0+1];
	 }

       i=i0+1;
       j=j0;
       if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	  return 1;

       switch(m)
	 {
	  case 8:
	    c=fill08(c1,c2,i,j);
	    break;
	  case 10:
	    c=fill10(c1,c2,i,j);
	    break;
	  case 12:
 	    c=fill12(c1,c2,i,j);
	    break;
	  case 14:
 	    c=fill14(c1,c2,i,j);
	    break;
	  default:
	    err_warn(1,fperr,"Error - (d4) tracing isofill line.\n");
	    return -4;
	 }

       return c;
      }


/*	Fill a 05 crossing box.						*/

    int fill05 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 5) return -5;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;

       line_it(7,c1,i0,j0,xyu,&nxyu);

       if (n > 0 && n < 15)
	 {
	  if (n == m)
	    {
	     line_it(7,c2,i0,j0,xyl,&nxyl);
	     mask[ixj*2+1]=13;
	     mask[ixj*2]=13;
	     return 1;
	    }
	  else if (n == 1)
	    {
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     nxyl++;
	     line_it(2,c1,i0,j0,xyl,&nxyl);
	    }
	  else if (n == 4)
	    {
	     nxyu++;
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     if (nxyl == 0)
	       {
	        xyl[nxyl].x=xv[i0];
	        xyl[nxyl].y=yv[j0];
	        nxyl++;
	       }
	     line_it(2,c1,i0,j0,xyl,&nxyl);
	    }
	  else
	    return -5;
	 }
       else
	 {
	  if (nxyl == 0)
	    {
	     xyl[nxyl].x=xv[i0];
	     xyl[nxyl].y=yv[j0];
	     nxyl++;
	    }
	  nxyu++;
	  xyu[nxyu].x=xv[i0+1];
	  xyu[nxyu].y=yv[j0+1];
	  line_it(2,c1,i0,j0,xyl,&nxyl);
	 }

       i = i0 +1;
       j = j0;
       if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
          return 1;

       switch(m)
	 {
	   case 8:
		c = fill08(c1,c2,i,j);
		break;
	   case 10:
		c = fill10(c1,c2,i,j);
		break;
	   case 12:
		c = fill12(c1,c2,i,j);
		break;
	   case 14:
		c = fill14(c1,c2,i,j);
		break;
	   default:
                err_warn(1,fperr,"Error - (d5) tracing isofill line.\n");
                 return -5;
		break;
	 }

       return c;
      }


/*	Fill a 06 crossing box.						*/

    int fill06 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 6) return -6;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;

       line_it(m,c1,i0,j0,xyu,&nxyu);

       if (n > 0 && n < 15)
	 {
	  if (n == m)
	    {
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     return 1;
	    }
	  else if (n == 2)
	    {
	     line_it(2,c2,i0,j0,xyl,&nxyl);
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     i=i0+1;
	     j=j0;
	     if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	       return 1;

	     switch(n=mask[(ixj+1)*2+1])
	      {
	       case 1:
	         c=filu01(c1,c2,i,j);
	         break;
	       case 3:
	         c=filu03(c1,c2,i,j);
	         break;
	       case 5:
	         c=filu05(c1,c2,i,j);
	         break;
	       case 7:
	         c=filu07(c1,c2,i,j);
	         break;
	       default:
	         err_warn(1,fperr,"Error - (d6) tracing isofill line.\n");
	         return -6;
	      }
	     return c;
	    }
	  else if (n == 4)
	    {
	     nxyu++;
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	     i=i0+1;
	     j=j0;
	     if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	       return 1;

	     switch(n=mask[(ixj+1)*2+1])
	      {
	       case 8:
	         c=filu08(c1,c2,i,j);
	         break;
	       case 10:
	         c=filu10(c1,c2,i,j);
	         break;
	       case 12:
	         c=filu12(c1,c2,i,j);
	         break;
	       case 14:
	         c=filu14(c1,c2,i,j);
	         break;
	       default:
	         err_warn(1,fperr,"Error - (d6) tracing isofill line.\n");
	         return -6;
	      }
	     return c;
	    }
	 }
       nxyu++;
       xyu[nxyu].x=xv[i0+1];
       xyu[nxyu].y=yv[j0+1];
       xyl[nxyl].x=xv[i0+1];
       xyl[nxyl].y=yv[j0];
       for (i=i0+1,j=j0,ixj=i+j*IM;
		i<IM-1 && (m=mask[ixj*2])==15 && (n=mask[ixj*2+1])==15;
		i++,ixj++)
	 {
	  mask[ixj*2]+=16;
	  xyu[nxyu].x=xv[i+1];
	  xyu[nxyu].y=yv[j+1];

	  xyl[nxyl].x=xv[i+1];
	  xyl[nxyl].y=yv[j];
	 }
       if (i<IM-1 && m > 0 && m < 15)
	 {
	  switch(m)
	    {
	     case 9:
	       c=fill09(c1,c2,i,j);
	       break;
	     case 11:
	       c=fill11(c1,c2,i,j);
	       break;
	     case 13:
	       c=fill13(c1,c2,i,j);
	       break;
	     default:
	       err_warn(1,fperr,"Error - (d6) tracing isofill line.\n");
	       return -6;
	    }
	  return c;
	 }
       else if (i<IM-1 && m == 15 && n > 0 && n < 15)
	 {
	  switch(n)
	    {
	     case 2:
	       c=filu02(c1,c2,i,j);
	       break;
	     case 4:
	       c=filu04(c1,c2,i,j);
	       break;
	     case 6:
	       c=filu06(c1,c2,i,j);
	       break;
	     default:
	       err_warn(1,fperr,"Error - (d6) tracing isofill line.\n");
	       return -6;
	    }
	  return c;
	 }

       return 1;
      }

/*	Fill a 07 crossing box.						*/

    int fill07 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 7) return -7;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;

       line_it(m,c1,i0,j0,xyu,&nxyu);

       if (n > 0 && n < 15)
	 {
	  if (n == m)
	    {
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     return 1;
	    }
	  else if (n == 1)
	    {
	     line_it(1,c2,i0,j0,xyl,&nxyl);
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	    }
	  else if (n == 2)
	    {
	     if (nxyl == 0)
	       {
	        xyl[nxyl].x=xv[i0];
	        xyl[nxyl].y=yv[j0];
	        nxyl++;
	       }
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     i=i0+1;
	     j=j0;
	     if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	       return 1;

	     switch(n=mask[(ixj+1)*2+1])
	      {
	       case 1:
	         c=filu01(c1,c2,i,j);
	         break;
	       case 3:
	         c=filu03(c1,c2,i,j);
	         break;
	       case 5:
	         c=filu05(c1,c2,i,j);
	         break;
	       case 7:
	         c=filu07(c1,c2,i,j);
	         break;
	       default:
	         err_warn(1,fperr,"Error - (d7) tracing isofill line.\n");
	         return -7;
	      }
	     return c;
	    }
	  else if (n == 3)
	    {
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     i=i0+1;
	     j=j0;
	     ixj=i+j*IM;
	     if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[ixj*2])<=0||m>16)
	       return 1;

	     switch(n=mask[ixj*2+1])
	      {
	       case 1:
	         c=filu01(c1,c2,i,j);
	         break;
	       case 3:
	         c=filu03(c1,c2,i,j);
	         break;
	       case 5:
	         c=filu05(c1,c2,i,j);
	         break;
	       case 7:
	         c=filu07(c1,c2,i,j);
	         break;
	       default:
	         err_warn(1,fperr,"Error - (d7) tracing isofill line.\n");
	         return -7;
	      }
	     return c;
	    }
	  else if (n == 4)
	    {
	     nxyu++;
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     if (nxyl == 0)
	       {
	        xyl[nxyl].x=xv[i0];
	        xyl[nxyl].y=yv[j0];
	        nxyl++;
	       }
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	     i=i0+1;
	     j=j0;
	     if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	       return 1;

	     switch(n=mask[(ixj+1)*2+1])
	      {
	       case 8:
	         c=filu08(c1,c2,i,j);
	         break;
	       case 10:
	         c=filu10(c1,c2,i,j);
	         break;
	       case 12:
	         c=filu12(c1,c2,i,j);
	         break;
	       case 14:
	         c=filu14(c1,c2,i,j);
	         break;
	       default:
	         err_warn(1,fperr,"Error - (d7) tracing isofill line.\n");
	         return -7;
	      }
	     return c;
	    }
	  else if (n == 5)
	    {
/*			Because of this it is necessary for the driver
			to check each point, even the first one, again
			after plotting.					*/
	     line_it(7,c2,i0,j0,xyl,&nxyl);
	     mask[ixj*2]=15;
	     mask[ixj*2+1]=13;
	     return 1;
	    }
	  else if (n == 6)
	    {
	     if (nxyl == 0)
	       {
		xyl[nxyl].x=xv[i0];
		xyl[nxyl].y=yv[j0];
		nxyl++;
	       }
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     return 1;
	    }
	  else return -7;
	 }
       nxyu++;
       xyu[nxyu].x=xv[i0+1];
       xyu[nxyu].y=yv[j0+1];
       if (nxyl == 0)
	 {
	  xyl[nxyl].x=xv[i0];
	  xyl[nxyl].y=yv[j0];
	  nxyl++;
	 }
       xyl[nxyl].x=xv[i0+1];
       xyl[nxyl].y=yv[j0];
       for (i=i0+1,j=j0,ixj=i+j*IM;
		i<IM-1 && (m=mask[ixj*2])==15 && (n=mask[ixj*2+1])==15;
		i++,ixj++)
	 {
	  mask[ixj*2]+=16;
	  xyu[nxyu].x=xv[i+1];
	  xyu[nxyu].y=yv[j+1];

	  xyl[nxyl].x=xv[i+1];
	  xyl[nxyl].y=yv[j];
	 }
       if (i<IM-1 && m > 0 && m < 15)
	 {
	  switch(m)
	    {
	     case 9:
	       c=fill09(c1,c2,i,j);
	       break;
	     case 11:
	       c=fill11(c1,c2,i,j);
	       break;
	     case 13:
	       c=fill13(c1,c2,i,j);
	       break;
	     default:
	       err_warn(1,fperr,"Error - (d7) tracing isofill line.\n");
	       return -7;
	    }
	  return c;
	 }
       else if (i<IM-1 && m == 15 && n > 0 && n < 15)
	 {
	  switch(n)
	    {
	     case 2:
	       c=filu02(c1,c2,i,j);
	       break;
	     case 4:
	       c=filu04(c1,c2,i,j);
	       break;
	     case 6:
	       c=filu06(c1,c2,i,j);
	       break;
	     default:
	       err_warn(1,fperr,"Error - (d7) tracing isofill line.\n");
	       return -7;
	    }
	  return c;
	 }

       return 1;
      }


/*	Fill a 08 crossing box.						*/

    int fill08 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int m,n;
       int ixj;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 8) return -8;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;

       line_it(15-m,c1,i0,j0,xyl,&nxyl);

       if (n > 0 && n < 15)
	 {
	  if (n != m) return -8;
	  line_it(15-n,c2,i0,j0,xyu,&nxyu);
	 }
       else
	 {
	  xyu[nxyu].x=xv[i0];
	  xyu[nxyu].y=yv[j0+1];
	 }

       return 1;
      }

/*	Fill a 09 crossing box.						*/

    int fill09 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int m,n;
       int ixj;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 9) return -9;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;


       if (n > 0 && n < 15)
	 {
	  if (n == m)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	    }
	  else if (n == 1)
	    {
	     xyu[nxyu].x=xv[i0];
	     xyu[nxyu].y=yv[j0+1];
	     nxyu++;
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	    }
	  else if (n == 8)
	    {
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     if (nxyl == 0)
	       {
		xyl[nxyl].x=xv[i0];
		xyl[nxyl].y=yv[j0];
	        nxyl++;
	       }
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	    }
	  else return -9;
	 }
       else
	 {
	  if (nxyu == 0)
	    {
	     xyu[nxyu].x=xv[i0];
	     xyu[nxyu].y=yv[j0+1];
	    }
	  nxyu++;
	  line_it(m,c1,i0,j0,xyu,&nxyu);
	  if (nxyl == 0)
	    {
	     xyl[nxyl].x=xv[i0];
	     xyl[nxyl].y=yv[j0];
	    }
	 }

       return 1;
      }

/*	Fill a 10 crossing box.						*/

    int fill10 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 10) return -10;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;

       line_it(1,c1,i0,j0,xyl,&nxyl);

       if (n > 0 && n < 15)
	 {
	  if (n == m)
	    {
	     line_it(1,c2,i0,j0,xyu,&nxyu);
	     mask[ixj*2]=11;
	     mask[ixj*2+1]=11;
	     return 1;
	    }
	  else if (n == 2)
	    {
	     nxyl++;
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     if (nxyu == 0)
	       {
		xyu[nxyu].x=xv[i0];
		xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     line_it(11,c1,i0,j0,xyu,&nxyu);
	    }
	  else if (n == 8)
	    {
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     nxyu++;
	     line_it(11,c1,i0,j0,xyu,&nxyu);
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	    }
	  else
	    return -10;
	 }
       else
	 {
	  if (nxyu == 0)
	    {
	     xyu[nxyu].x=xv[i0];
	     xyu[nxyu].y=yv[j0+1];
	     nxyu++;
	    }
	  line_it(11,c1,i0,j0,xyu,&nxyu);
	  nxyl++;
	  xyl[nxyl].x=xv[i0+1];
	  xyl[nxyl].y=yv[j0];
	 }

       i=i0+1;
       j=j0;
       if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	  return 1;

       switch(m)
	 {
	  case 1:
	    c=fill01(c1,c2,i,j);
	    break;
	  case 3:
	    c=fill03(c1,c2,i,j);
	    break;
	  case 5:
 	    c=fill05(c1,c2,i,j);
	    break;
	  case 7:
 	    c=fill07(c1,c2,i,j);
	    break;
	  default:
	    err_warn(1,fperr,"Error - (d10) tracing isofill line.\n");
	    return -10;
	 }

       return c;
      }

/*	Fill an 11 crossing box.					*/

    int fill11 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 11) return -11;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;


       if (n > 0 && n < 15)
	 {
	  if (n == m)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	    }
	  else if (n == 1)
	    {
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     line_it(m,c1,i0,j0,xyu,&nxyu);

	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	    }
	  else if (n == 2)
	    {
	     if (nxyl == 0)
	       {
		xyl[nxyl].x=xv[i0];
		xyl[nxyl].y=yv[j0];
		nxyl++;
	       }
	     line_it(2,c2,i0,j0,xyl,&nxyl);
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	    }
	  else if (n == 3)
	    {
	     line_it(3,c2,i0,j0,xyl,&nxyl);
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     
	    }
	  else if (n == 8)
	    {
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     nxyu++;
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     if (nxyl == 0)
	       {
	        xyl[nxyl].x=xv[i0];
	        xyl[nxyl].y=yv[j0];
		nxyl++;
	       }
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	    }
	  else if (n == 9)
	    {
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	    }
	  else if (n == 10)
	    {
	     line_it(1,c2,i0,j0,xyu,&nxyu);
	     if (nxyl == 0)
	       {
		xyl[nxyl].x=xv[i0];
		xyl[nxyl].y=yv[j0];
	       }
	     mask[ixj*2+1]=11;
	     mask[ixj*2]=11;
	     return 1;
	    }
	  else if (n == 11)
	    {
	     line_it(11,c1,i0,j0,xyu,&nxyu);
	     line_it(11,c2,i0,j0,xyl,&nxyl);
	    }
	  else return -11;
	 }
       else
	 {
	  if (nxyu == 0)
	    {
	     xyu[nxyu].x=xv[i0];
	     xyu[nxyu].y=yv[j0+1];
	    }
	  nxyu++;
	  line_it(m,c1,i0,j0,xyu,&nxyu);
	  if (nxyl == 0)
	    {
	     xyl[nxyl].x=xv[i0];
	     xyl[nxyl].y=yv[j0];
	    }
	  nxyl++;
	  xyl[nxyl].x=xv[i0+1];
	  xyl[nxyl].y=yv[j0];
	 }
       i=i0+1;
       j=j0;
       if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	       return 1;

       switch(m)
         {
          case 1:
            c=fill01(c1,c2,i,j);
            break;
          case 3:
            c=fill03(c1,c2,i,j);
            break;
          case 5:
            c=fill05(c1,c2,i,j);
            break;
          case 7:
            c=fill07(c1,c2,i,j);
            break;
          default:
            err_warn(1,fperr,"Error - (d11) tracing isofill line.\n");
            return -11;
         }
       return c;
      }


/*	Fill a 12 crossing box.						*/

    int fill12 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 12) return -12;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;

       line_it(15-m,c1,i0,j0,xyl,&nxyl);

       xyu[nxyu].x=xv[i0];
       xyu[nxyu].y=yv[j0+1];
       if (n > 0 && n < 15)
	 {
	  if (n != m && n != 4 && n != 8) return -12;
	  if (n == 4) nxyu++;
	  line_it(15-n,c2,i0,j0,xyu,&nxyu);
	  if (n == 8)
	    {
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	    }
	 }
       else
	 {
	  if (nxyu ==0) nxyu++;
          xyu[nxyu].x=xv[i0+1];
          xyu[nxyu].y=yv[j0+1];
	 }

       i=i0+1;
       j=j0;
       if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	  return 1;

       switch(m)
	 {
	  case 8:
	    c=fill08(c1,c2,i,j);
	    break;
	  case 10:
	    c=fill10(c1,c2,i,j);
	    break;
	  case 12:
 	    c=fill12(c1,c2,i,j);
	    break;
	  case 14:
 	    c=fill14(c1,c2,i,j);
	    break;
	  default:
	    err_warn(1,fperr,"Error - (d12) tracing isofill line.\n");
	    return -12;
	 }

       return c;
      }

/*	Fill an 13 crossing box.					*/

    int fill13 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 13) return -13;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;


       if (n > 0 && n < 15)
	 {
	  if (n == m)
	    {
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	    }
	  else if (n == 1)
	    {
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     nxyl++;
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	    }
	  else if (n == 4)
	    {
	     if (nxyl == 0)
	       {
		xyl[nxyl].x=xv[i0];
		xyl[nxyl].y=yv[j0];
		nxyl++;
	       }
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	    }
	  else if (n == 5)
	    {
	     line_it(7,c2,i0,j0,xyl,&nxyl);
	     if (nxyu == 0)
	       {
		xyu[nxyu].x=xv[i0+1];
		xyu[nxyu].y=yv[j0+1];
	       }
	     mask[ixj*2+1]=13;
	     return 1;
	    }
	  else if (n == 8)
	    {
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     if (nxyl == 0)
	       {
	        xyl[nxyl].x=xv[i0];
	        xyl[nxyl].y=yv[j0];
		nxyl++;
	       }
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	    }
	  else if (n == 9)
	    {
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	    }
	  else if (n == 12)
	    {
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     if (nxyl == 0)
	       {
	        xyl[nxyl].x=xv[i0];
	        xyl[nxyl].y=yv[j0];
	       }
	     nxyl++;
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	    }
	  else return -13;
	 }
       else
	 {
	  if (nxyu == 0)
	    {
	     xyu[nxyu].x=xv[i0];
	     xyu[nxyu].y=yv[j0+1];
	     nxyu++;
	    }
	  xyu[nxyu].x=xv[i0+1];
	  xyu[nxyu].y=yv[j0+1];
	  if (nxyl == 0)
	    {
	     xyl[nxyl].x=xv[i0];
	     xyl[nxyl].y=yv[j0];
	     nxyl++;
	    }
	  line_it(15-m,c1,i0,j0,xyl,&nxyl);
	 }
       i=i0+1;
       j=j0;
       if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	       return 1;

       switch(m)
         {
          case 8:
            c=fill08(c1,c2,i,j);
            break;
          case 10:
            c=fill10(c1,c2,i,j);
            break;
          case 12:
            c=fill12(c1,c2,i,j);
            break;
          case 14:
            c=fill14(c1,c2,i,j);
            break;
          default:
            err_warn(1,fperr,"Error - (d13) tracing isofill line.\n");
            return -13;
         }
       return c;
      }


/*	Fill a 14 crossing box.						*/

    int fill14 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 14) return -14;
       n=mask[ixj*2+1];
       mask[ixj*2]+=16;

       line_it(15-m,c1,i0,j0,xyl,&nxyl);

       if (n > 0 && n < 15)
	 {
	  if (n == m)
	    {
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     return 1;
	    }
	  else if (n == 2)
	    {
	     nxyl++;
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     i=i0+1;
	     j=j0;
	     if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	       return 1;

	     switch(n=mask[(ixj+1)*2+1])
	      {
	       case 1:
	         c=filu01(c1,c2,i,j);
	         break;
	       case 3:
	         c=filu03(c1,c2,i,j);
	         break;
	       case 5:
	         c=filu05(c1,c2,i,j);
	         break;
	       case 7:
	         c=filu07(c1,c2,i,j);
	         break;
	       default:
	         err_warn(1,fperr,"Error - (d14) tracing isofill line.\n");
	         return -14;
	      }
	     return c;
	    }
	  else if (n == 4)
	    {
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	     i=i0+1;
	     j=j0;
	     if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	       return 1;

	     switch(n=mask[(ixj+1)*2+1])
	      {
	       case 8:
	         c=filu08(c1,c2,i,j);
	         break;
	       case 10:
	         c=filu10(c1,c2,i,j);
	         break;
	       case 12:
	         c=filu12(c1,c2,i,j);
	         break;
	       case 14:
	         c=filu14(c1,c2,i,j);
	         break;
	       default:
	         err_warn(1,fperr,"Error - (d14) tracing isofill line.\n");
	         return -14;
	      }
	     return c;
	    }
	  else if (n == 6)
	    {
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     return 1;
	    }
	  else if (n == 8)
	    {
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	    }
	  else if (n == 10)
	    {
/*			Because of this it is necessary for the driver
			to check each point, even the first one, again
			after plotting.					*/
	     line_it(1,c2,i0,j0,xyu,&nxyu);
	     mask[ixj*2]=15;
	     mask[ixj*2+1]=11;
	     return 1;
	    }
	  else if (n == 12)
	    {
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     i=i0+1;
	     j=j0;
	     if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[(ixj+1)*2])<=0||m>16)
	       return 1;

	     switch(n=mask[(ixj+1)*2+1])
	      {
	       case 8:
	         c=filu08(c1,c2,i,j);
	         break;
	       case 10:
	         c=filu10(c1,c2,i,j);
	         break;
	       case 12:
	         c=filu12(c1,c2,i,j);
	         break;
	       case 14:
	         c=filu14(c1,c2,i,j);
	         break;
	       default:
	         err_warn(1,fperr,"Error - (d14) tracing isofill line.\n");
	         return -14;
	      }
	     return c;
	    }
	  else return -14;
	 }
       nxyl++;
       xyl[nxyl].x=xv[i0+1];
       xyl[nxyl].y=yv[j0];
       if (nxyu == 0)
	 {
	  xyu[nxyu].x=xv[i0];
	  xyu[nxyu].y=yv[j0+1];
	 }
       nxyu++;
       xyu[nxyu].x=xv[i0+1];
       xyu[nxyu].y=yv[j0+1];
       for (i=i0+1,j=j0,ixj=i+j*IM;
		i<IM-1 && (m=mask[ixj*2])==15 && (n=mask[ixj*2+1])==15;
		i++,ixj++)
	 {
	  mask[ixj*2]+=16;
	  xyu[nxyu].x=xv[i+1];
	  xyu[nxyu].y=yv[j+1];

	  xyl[nxyl].x=xv[i+1];
	  xyl[nxyl].y=yv[j];
	 }
       if (i<IM-1 && m > 0 && m < 15)
	 {
	  switch(m)
	    {
	     case 9:
	       c=fill09(c1,c2,i,j);
	       break;
	     case 11:
	       c=fill11(c1,c2,i,j);
	       break;
	     case 13:
	       c=fill13(c1,c2,i,j);
	       break;
	     default:
	       err_warn(1,fperr,"Error - (d14) tracing isofill line.\n");
	       return -14;
	    }
	  return c;
	 }
       else if (i<IM-1 && m == 15 && n > 0 && n < 15)
	 {
	  switch(n)
	    {
	     case 2:
	       c=filu02(c1,c2,i,j);
	       break;
	     case 4:
	       c=filu04(c1,c2,i,j);
	       break;
	     case 6:
	       c=filu06(c1,c2,i,j);
	       break;
	     default:
	       err_warn(1,fperr,"Error - (d14) tracing isofill line.\n");
	       return -14;
	    }
	  return c;
	 }

       return 1;
      }

/*	Fill a 15 crossing box.						*/

    int fill15 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       if (m != 15) return -15;
       n=mask[ixj*2+1];

       if (n > 0 && n < 15)
	 {
	  switch(n)
	    {
	     case 1:
	       c=filu01(c1,c2,i0,j0);
	       break;
	     case 2:
	       c=filu02(c1,c2,i0,j0);
	       break;
	     case 3:
	       c=filu03(c1,c2,i0,j0);
	       break;
	     case 4:
	       c=filu04(c1,c2,i0,j0);
	       break;
	     case 5:
	       c=filu05(c1,c2,i0,j0);
	       break;
	     case 6:
	       c=filu06(c1,c2,i0,j0);
	       break;
	     case 7:
	       c=filu07(c1,c2,i0,j0);
	       break;
	     case 8:
	       c=filu08(c1,c2,i0,j0);
	       break;
	     case 9:
	       c=filu09(c1,c2,i0,j0);
	       break;
	     case 10:
	       c=filu10(c1,c2,i0,j0);
	       break;
	     case 11:
	       c=filu11(c1,c2,i0,j0);
	       break;
	     case 12:
	       c=filu12(c1,c2,i0,j0);
	       break;
	     case 13:
	       c=filu13(c1,c2,i0,j0);
	       break;
	     case 14:
	       c=filu14(c1,c2,i0,j0);
	       break;
	    }
/*	 mask[ixj*2]+=16;				*/
	 return c;
	}
      else
	{
	 if (nxyu == 0)
	   {
	    xyu[nxyu].x=xv[i0];
	    xyu[nxyu].y=yv[j0+1];
	    nxyu++;
	   }
	 xyu[nxyu].x=xv[i0+1];
	 xyu[nxyu].y=yv[j0+1];
	 if (nxyl == 0)
	   {
	    xyl[nxyl].x=xv[i0];
	    xyl[nxyl].y=yv[j0];
	    nxyl++;
	   }
	 xyl[nxyl].x=xv[i0+1];
	 xyl[nxyl].y=yv[j0];
	}
       mask[ixj*2]+=16;
       for (i=i0+1,j=j0,ixj=i+j*IM;
		i<IM-1 && (m=mask[ixj*2])==15 && (n=mask[ixj*2+1])==15;
		i++,ixj++)
	 {
	  mask[ixj*2]+=16;
	  xyu[nxyu].x=xv[i+1];
	  xyu[nxyu].y=yv[j+1];

	  xyl[nxyl].x=xv[i+1];
	  xyl[nxyl].y=yv[j];
	 }
       if (i<IM-1 && m > 0 && m < 15)
	 {
	  switch(m)
	    {
	     case 9:
	       c=fill09(c1,c2,i,j);
	       break;
	     case 11:
	       c=fill11(c1,c2,i,j);
	       break;
	     case 13:
	       c=fill13(c1,c2,i,j);
	       break;
	     default:
	       err_warn(1,fperr,"Error - (d15) tracing isofill line.\n");
	       return -15;
	    }
	  return c;
	 }
       else if (i<IM-1 && m == 15 && n > 0 && n < 15)
	 {
	  switch(n)
	    {
	     case 2:
	       c=filu02(c1,c2,i,j);
	       break;
	     case 4:
	       c=filu04(c1,c2,i,j);
	       break;
	     case 6:
	       c=filu06(c1,c2,i,j);
	       break;
	     default:
	       err_warn(1,fperr,"Error - (d15) tracing isofill line.\n");
	       return -15;
	    }
	  return c;
	 }

       return 1;
      }


/*		Line for a grid box crossing.				*/

    int line_it(m,C,i,j,xy,n)
      int m;		/*  Crossing index.				*/
      float C;		/*  Level of crossing.				*/
      int i;		/*  First array index.				*/
      int j;		/*  Second array index.				*/
      Gpoint xy[];	/*  Returned x and y coordinates of points.	*/
      int *n;		/*  Given and returned index into xy.		*/
      {
       int ixj;
       int ix0,ix1,jy0,jy1;
       float a0,a1;
       float coef;

       ixj=i+j*IM;
/*				Entry point.				*/
       ix0=((IN[m-1][0][0] > IN[m-1][0][1]) ? IN[m-1][0][1]:IN[m-1][0][0]);
       ix1=((IN[m-1][0][0] <= IN[m-1][0][1]) ? IN[m-1][0][1]:IN[m-1][0][0]);
       jy0=((JN[m-1][0][0] > JN[m-1][0][1]) ? JN[m-1][0][1]:JN[m-1][0][0]);
       jy1=((JN[m-1][0][0] <= JN[m-1][0][1]) ? JN[m-1][0][1]:JN[m-1][0][0]);
       a0=A[ixj+ix0+jy0*IM];
       a1=A[ixj+ix1+jy1*IM];
       coef=(C-a0)/(a1-a0);
       xy[*n].x=(xv[i+ix1]-xv[i+ix0])*coef+xv[i+ix0];
       xy[*n].y=(yv[j+jy1]-yv[j+jy0])*coef+yv[j+jy0];

       (*n)++;
/*				Exit point.				*/
       ix0=((IN[m-1][1][0] > IN[m-1][1][1]) ? IN[m-1][1][1]:IN[m-1][1][0]);
       ix1=((IN[m-1][1][0] <= IN[m-1][1][1]) ? IN[m-1][1][1]:IN[m-1][1][0]);
       jy0=((JN[m-1][1][0]  > JN[m-1][1][1]) ? JN[m-1][1][1]:JN[m-1][1][0]);
       jy1=((JN[m-1][1][0] <= JN[m-1][1][1]) ? JN[m-1][1][1]:JN[m-1][1][0]);
       a0=A[ixj+ix0+jy0*IM];
       a1=A[ixj+ix1+jy1*IM];
       coef=(C-a0)/(a1-a0);
       xy[*n].x=(xv[i+ix1]-xv[i+ix0])*coef+xv[i+ix0];
       xy[*n].y=(yv[j+jy1]-yv[j+jy0])*coef+yv[j+jy0];
       return 1;
      }
