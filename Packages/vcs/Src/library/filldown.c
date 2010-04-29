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

/*	Fill a 14 crossing box.						*/

    int filu14 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int m,n;
       int ixj;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 14) return -14;
       mask[ixj*2]+=16;

       line_it(15-n,c2,i0,j0,xyu,&nxyu);

       if (m > 0 && m < 15)
	 {
	  if (n != m) return -14;
	  line_it(15-m,c1,i0,j0,xyl,&nxyl);
	 }
       else if (nxyl == 0)
	 {
	  xyl[nxyl].x=xv[i0];
	  xyl[nxyl].y=yv[j0];
	 }

       return 1;
      }

/*	Fill a 13 crossing box.						*/

    int filu13 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 13) return -13;
       mask[ixj*2]+=16;

       line_it(15-n,c2,i0,j0,xyu,&nxyu);

       if (m > 0 && m < 15)
	 {
	  if (n != m) return -13;
	  line_it(15-m,c1,i0,j0,xyl,&nxyl);
	 }
       else
	 {
	  xyl[nxyl].x=xv[i0+1];
	  xyl[nxyl].y=yv[j0];
	 }

       i=i0+1;
       j=j0;
       ixj=i+j*IM;
       if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[ixj*2])<=0||m>16)
	  return 1;

       switch(n=mask[ixj*2+1])
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
	    err_warn(1,fperr,"Error - (u13) tracing isofill line.\n");
	    return -13;
	 }

       return c;
      }

/*	Fill a 12 crossing box.						*/

    int filu12 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 12) return -12;
       mask[ixj*2]+=16;

       line_it(15-n,c2,i0,j0,xyu,&nxyu);

       xyl[nxyl].x=xv[i0];
       xyl[nxyl].y=yv[j0];
       if (m > 0 && m < 15)
	 {
	  if (n != m && m != 13 && m != 14) return -12;
	  if (m == 13 && nxyl == 0) nxyl++;
	  line_it(15-m,c1,i0,j0,xyl,&nxyl);
	  if (m == 14)
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
       ixj=i+j*IM;
       if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[ixj*2])<=0||m>16)
	  return 1;

       switch(n=mask[ixj*2+1])
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
	    err_warn(1,fperr,"Error - (u12) tracing isofill line.\n");
	    return -12;
	 }

       return c;
      }

/*	Fill a 11 crossing box.						*/

    int filu11 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 11) return -11;
       mask[ixj*2]+=16;

       line_it(n,c2,i0,j0,xyl,&nxyl);

       if (m > 0 && m < 15)
	 {
	  if (n != m) return -11;
	  line_it(m,c1,i0,j0,xyu,&nxyu);
	 }
       else if (nxyu == 0)
	 {
	  xyu[nxyu].x=xv[i0+1];
	  xyu[nxyu].y=yv[j0+1];
	 }

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
	    err_warn(1,fperr,"Error - (u11) tracing isofill line.\n");
	    return -11;
	 }

       return c;
      }


/*	Fill a 10 crossing box.						*/

    int filu10 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int m,n;
       int ixj;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 10) return -10;
       mask[ixj*2]+=16;

       line_it(1,c2,i0,j0,xyu,&nxyu);

       if (m > 0 && m < 15)
	 {
	  if (n == m)
	    {
	     line_it(1,c1,i0,j0,xyl,&nxyl);
	     mask[ixj*2]=11;
	     mask[ixj*2+1]=11;
	     return 1;
	    }
	  else if (m == 14)
	    {
	     line_it(1,c1,i0,j0,xyl,&nxyl);
	     mask[ixj*2]=15;
	     mask[ixj*2+1]=11;
	     return 1;
	    }
	  else if (m == 11)
	    {
	     if (nxyl == 0)
	       {
	        xyl[nxyl].x=xv[i0];
	        xyl[nxyl].y=yv[j0];
	       }
	     mask[ixj*2]=11;
	     mask[ixj*2+1]=11;
	     return 1;
	    }
	  else
	    return -10;
	 }
       else
	 {
	  if (nxyl == 0)
	    {
	     xyl[nxyl].x=xv[i0];
	     xyl[nxyl].y=yv[j0];
	    }
	  mask[ixj*2]=15;
	  mask[ixj*2+1]=11;
	  return 1;
	 }
      }


/*	Fill a 09 crossing box.						*/

    int filu09 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 9) return -9;
       mask[ixj*2]+=16;

       line_it(15-n,c2,i0,j0,xyu,&nxyu);

       if (m > 0 && m < 15)
	 {
	  if (n == m)
	    {
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	     return 1;
	    }
	  else if (m == 11)
	    {
	     nxyu++;
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
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
	         err_warn(1,fperr,"Error - (u9) tracing isofill line.\n");
	         return -9;
	      }
	     return c;
	    }
	  else if (m == 13)
	    {
	     line_it(2,c1,i0,j0,xyl,&nxyl);
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
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
	         err_warn(1,fperr,"Error - (u9) tracing isofill line.\n");
	         return -9;
	      }
	     return c;
	    }
	  else return -9;
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
	       err_warn(1,fperr,"Error - (u9) tracing isofill line.\n");
	       return -9;
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
	       err_warn(1,fperr,"Error - (u9) tracing isofill line.\n");
	       return -9;
	    }
	  return c;
	 }

       return 1;
      }

/*	Fill a 08 crossing box.						*/

    int filu08 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 8) return -8;
       mask[ixj*2]+=16;

       line_it(15-n,c2,i0,j0,xyu,&nxyu);

       if (m > 0 && m < 15)
	 {
	  if (n == m)
	    {
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	     return 1;
	    }
	  else if (m == 14)
	    {
	     line_it(1,c1,i0,j0,xyl,&nxyl);
	    }
	  else if (m == 13)
	    {
	     if (nxyl == 0)
	       {
	        xyl[nxyl].x=xv[i0];
	        xyl[nxyl].y=yv[j0];
	        nxyl++;
	       }
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     i=i0+1;
	     j=j0;
	     ixj=i+j*IM;
	     if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[ixj*2])<=0||m>16)
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
	         err_warn(1,fperr,"Error - (u8) tracing isofill line.\n");
	         return -8;
	      }
	     return c;
	    }
	  else if (m == 12)
	    {
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     i=i0+1;
	     j=j0;
	     ixj=i+j*IM;
	     if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[ixj*2])<=0||m>16)
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
	         err_warn(1,fperr,"Error - (u8) tracing isofill line.\n");
	         return -8;
	      }
	     return c;
	    }
	  else if (m == 11)
	    {
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
	         err_warn(1,fperr,"Error - (u8) tracing isofill line.\n");
	         return -8;
	      }
	     return c;
	    }
	  else if (m == 10)
	    {
/*			Because of this it is necessary for the driver
			to check each point, even the first one, again
			after plotting.					*/
	     line_it(1,c1,i0,j0,xyl,&nxyl);
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	     nxyu++;
	     line_it(11,c1,i0,j0,xyu,&nxyu);
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
	         err_warn(1,fperr,"Error - (u8) tracing isofill line.\n");
	         return -8;
	      }
	     return c;


	    }
	  else if (m == 9)
	    {
	     if (nxyl == 0)
	       {
		xyl[nxyl].x=xv[i0];
		xyl[nxyl].y=yv[j0];
		nxyl++;
	       }
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	     return 1;
	    }
	  else return -8;
	 }
       nxyu++;
       xyu[nxyu].x=xv[i0+1];
       xyu[nxyu].y=yv[j0+1];
       if (nxyl == 0)
	 {
	  xyl[nxyl].x=xv[i0];
	  xyl[nxyl].y=yv[j0];
	 }
       nxyl++;
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
	       err_warn(1,fperr,"Error - (u8) tracing isofill line.\n");
	       return -8;
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
	       err_warn(1,fperr,"Error - (u8) tracing isofill line.\n");
	       return -8;
	    }
	  return c;
	 }

       return 1;
      }


/*	Fill a 07 crossing box.						*/

    int filu07 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int m,n;
       int ixj;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 7) return -7;
       mask[ixj*2]+=16;

       line_it(n,c2,i0,j0,xyl,&nxyl);

       if (m > 0 && m < 15)
	 {
	  if (n != m) return -7;
	  line_it(m,c1,i0,j0,xyu,&nxyu);
	 }
       else
	 {
	  xyu[nxyu].x=xv[i0];
	  xyu[nxyu].y=yv[j0+1];
	 }

       return 1;
      }

/*	Fill a 06 crossing box.						*/

    int filu06 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int m,n;
       int ixj;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 6) return -6;
       mask[ixj*2]+=16;

       if (m > 0 && m < 15)
	 {
	  if (n == m)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     return 1;
	    }
	  else if (m == 14)
	    {
	     xyu[nxyu].x=xv[i0];
	     xyu[nxyu].y=yv[j0+1];
	     nxyu++;
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	     return 1;
	    }
	  else if (m == 7)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     if (nxyl == 0)
	       {
		xyl[nxyl].x=xv[i0];
		xyl[nxyl].y=yv[j0];
	        nxyl++;
	       }
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	    }
	  else return -6;
	 }
       else
	 {
	  if (nxyu == 0)
	    {
	     xyu[nxyu].x=xv[i0];
	     xyu[nxyu].y=yv[j0+1];
	     nxyu++;
	    }
	  line_it(15-n,c2,i0,j0,xyu,&nxyu);
	  if (nxyl == 0)
	    {
	     xyl[nxyl].x=xv[i0];
	     xyl[nxyl].y=yv[j0];
	    }
	 }

       return 1;
      }

/*	Fill a 05 crossing box.						*/

    int filu05 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int m,n;
       int ixj;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 5) return -5;
       mask[ixj*2+1]=13;

       line_it(7,c2,i0,j0,xyl,&nxyl);

       if (m > 0 && m < 15)
	 {
	  if (n == m)
	    {
	     line_it(7,c1,i0,j0,xyu,&nxyu);
	     mask[ixj*2]=13;
	     return 1;
	    }
	  else if (m == 7)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     mask[ixj*2]=15;
	     return 1;
	    }
	  else if (m == 13)
	    {
/*	     line_it(m,c1,i0,j0,xyu,&nxyu);		*/
	     if (nxyu == 0)
	       {
		xyu[nxyu].x=xv[i0];
		xyu[nxyu].y=yv[j0+1];
	       }
	     return 1;
	    }
	  else
	    return -5;
	 }
       else if (m == 15 || m == 31)
	 {
	  if (nxyu == 0)
	    {
	     xyu[nxyu].x=xv[i0];
	     xyu[nxyu].y=yv[j0+1];
	    }
	  return 1;
	 }
       else
	 return -5;
      }

/*	Fill a 04 crossing box.					*/

    int filu04 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 4) return -4;
       mask[ixj*2]+=16;


       if (m > 0 && m < 15)
	 {
	  if (n == m)
	    {
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	    }
	  else if (m == 14)
	    {
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);

	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	    }
	  else if (m == 13)
	    {
	     if (nxyl == 0)
	       {
		xyl[nxyl].x=xv[i0];
		xyl[nxyl].y=yv[j0];
		nxyl++;
	       }
	     line_it(2,c1,i0,j0,xyl,&nxyl);
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	    }
	  else if (m == 7)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
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
	    }
	  else if (m == 6)
	    {
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	    }
	  else if (m == 5)
	    {
	     line_it(7,c1,i0,j0,xyu,&nxyu);
	     line_it(15-n,c2,i0,j0,xyu,&nxyu);
	     if (nxyl == 0)
	       {
		xyl[nxyl].x=xv[i0];
		xyl[nxyl].y=yv[j0];
		nxyl++;
	       }
	     line_it(2,c1,i0,j0,xyl,&nxyl);
	    }
	  else return -4;
	 }
       else
	 {
	  if (nxyu == 0)
	    {
	     xyu[nxyu].x=xv[i0];
	     xyu[nxyu].y=yv[j0+1];
	     nxyu++;
	    }
	  line_it(15-n,c2,i0,j0,xyu,&nxyu);
	  if (nxyl == 0)
	    {
	     xyl[nxyl].x=xv[i0];
	     xyl[nxyl].y=yv[j0];
	     nxyl++;
	    }
	  xyl[nxyl].x=xv[i0+1];
	  xyl[nxyl].y=yv[j0];
	 }
       i=i0+1;
       j=j0;
       ixj=i+j*IM;
       if (nxyu>=NMAX||nxyl>=NMAX||i>IM-2||(m=mask[ixj*2])<=0||m>16)
	       return 1;

       switch(n=mask[ixj*2+1])
         {
          case 14:
            c=filu14(c1,c2,i,j);
            break;
          case 12:
            c=filu12(c1,c2,i,j);
            break;
          case 10:
            c=filu10(c1,c2,i,j);
            break;
          case 8:
            c=filu08(c1,c2,i,j);
            break;
          default:
            err_warn(1,fperr,"Error - (u4) tracing isofill line.\n");
            return -4;
         }
       return c;
      }


/*	Fill a 03 crossing box.						*/

    int filu03 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 3) return -3;
       mask[ixj*2]+=16;

       line_it(n,c2,i0,j0,xyl,&nxyl);

       xyu[nxyu].x=xv[i0];
       xyu[nxyu].y=yv[j0+1];
       if (m > 0 && m < 15)
	 {
	  if (n != m && m != 11 && m != 7) return -3;
	  if (m == 11) nxyu++;
	  line_it(m,c1,i0,j0,xyu,&nxyu);
	  if (m == 7)
	    {
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	    }
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
	 }

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
	    err_warn(1,fperr,"Error - (u3) tracing isofill line.\n");
	    return -3;
	 }

       return c;
      }

/*	Fill a 02 crossing box.					*/

    int filu02 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 2) return -2;
       mask[ixj*2]+=16;


       if (m > 0 && m < 15)
	 {
	  if (n == m)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	    }
	  else if (m == 14)
	    {
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	    }
	  else if (m == 11)
	    {
	     if (nxyl == 0)
	       {
		xyl[nxyl].x=xv[i0];
		xyl[nxyl].y=yv[j0];
		nxyl++;
	       }
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	    }
	  else if (m == 10)
	    {
	     line_it(1,c1,i0,j0,xyl,&nxyl);
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	     if (nxyu == 0)
	       {
		xyu[nxyu].x=xv[i0];
		xyu[nxyu].y=yv[j0+1];
		nxyu++;
	       }
	     line_it(11,c1,i0,j0,xyu,&nxyu);
	    }
	  else if (m == 7)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     if (nxyl == 0)
	       {
	        xyl[nxyl].x=xv[i0];
	        xyl[nxyl].y=yv[j0];
		nxyl++;
	       }
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	    }
	  else if (m == 6)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	    }
	  else if (m == 3)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     if (nxyl == 0)
	       {
	        xyl[nxyl].x=xv[i0];
	        xyl[nxyl].y=yv[j0];
	        nxyl++;
	       }
	     line_it(n,c2,i0,j0,xyl,&nxyl);
	    }
	  else return -2;
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
	  line_it(n,c2,i0,j0,xyl,&nxyl);
	 }
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
            err_warn(1,fperr,"Error - (u2) tracing isofill line.\n");
            return -2;
         }
       return c;
      }


/*	Fill a 01 crossing box.						*/

    int filu01 (c1,c2,i0,j0)

      float c1;		/*  Min level.					*/
      float c2;		/*  Max level.					*/
      int i0;		/*  First dimension index.			*/
      int j0;		/*  Second dimension index.			*/

      {
       int i,j,m,n;
       int ixj,c;

       ixj=i0+j0*IM;
       m=mask[ixj*2];
       n=mask[ixj*2+1];
       if (n != 1) return -1;
       mask[ixj*2]+=16;

       line_it(n,c2,i0,j0,xyl,&nxyl);

       if (m > 0 && m < 15)
	 {
	  if (n == m)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     return 1;
	    }
	  else if (m == 13)
	    {
	     nxyl++;
	     line_it(15-m,c1,i0,j0,xyl,&nxyl);
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
	         err_warn(1,fperr,"Error - (u1) tracing isofill line.\n");
	         return -1;
	      }
	     return c;
	    }
	  else if (m == 11)
	    {
	     if (nxyu == 0)
	       {
	        xyu[nxyu].x=xv[i0];
	        xyu[nxyu].y=yv[j0+1];
	        nxyu++;
	       }
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
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
	         err_warn(1,fperr,"Error - tracing isofill line.\n");
	         return -1;
	      }
	     return c;
	    }
	  else if (m == 7)
	    {
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     nxyu++;
	    }
	  else if (m == 9)
	    {
	     if (nxyu == 0)
                {
                   xyu[nxyu].x=xv[i0];
                   xyu[nxyu].y=yv[j0+1];
                   nxyu++;
                }
	     line_it(m,c1,i0,j0,xyu,&nxyu);
	     return 1;
	    }
	  else if (m == 5)
	    {
/*			Because of this it is necessary for the driver
			to check each point, even the first one, again
			after plotting.					*/
	     nxyl++;
	     line_it(2,c1,i0,j0,xyl,&nxyl);
	     line_it(7,c1,i0,j0,xyu,&nxyu);
	     nxyu++;
	     xyu[nxyu].x=xv[i0+1];
	     xyu[nxyu].y=yv[j0+1];
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
	         err_warn(1,fperr,"Error - (u1) tracing isofill line.\n");
	         return -1;
	      }
	     return c;
	    }
	  else if (m == 3)
	    {
	     nxyl++;
	     xyl[nxyl].x=xv[i0+1];
	     xyl[nxyl].y=yv[j0];
	     line_it(m,c1,i0,j0,xyu,&nxyu);
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
	         err_warn(1,fperr,"Error - (u1) tracing isofill line.\n");
	         return -1;
	      }
	     return c;
	    }
	  else return -1;
	 }
       nxyl++;
       xyl[nxyl].x=xv[i0+1];
       xyl[nxyl].y=yv[j0];
       if (nxyu == 0)
	 {
	  xyu[nxyu].x=xv[i0];
	  xyu[nxyu].y=yv[j0+1];
	  nxyu++;
	 }
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
	       err_warn(1,fperr,"Error - (u1) tracing isofill line.\n");
	       return -1;
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
	       err_warn(1,fperr,"Error - (u1) tracing isofill line.\n");
	       return -1;
	    }
	  return c;
	 }

       return 1;
      }

