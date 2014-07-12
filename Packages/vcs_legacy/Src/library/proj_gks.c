#include "gks.h"
#include "gksshort.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "project.h"
#include "picture.h"
#include "vcs_legacy_marker.h"

#define V(C,z) (C.c0 + z * (C.c1 + z * (C.c2 + z * C.c3)))
#define DV(C,z) (C.c1 + z * (C.c2 + C.c2 + z * 3. * C.c3))
/* note: following terms based upon 5 deg. intervals in degrees. */
static struct COEFS {
	float c0, c1, c2, c3;
} X[] = {
1,	-5.67239e-12,	-7.15511e-05,	3.11028e-06,
0.9986,	-0.000482241,	-2.4897e-05,	-1.33094e-06,
0.9954,	-0.000831031,	-4.4861e-05,	-9.86588e-07,
0.99,	-0.00135363,	-5.96598e-05,	3.67749e-06,
0.9822,	-0.00167442,	-4.4975e-06,	-5.72394e-06,
0.973,	-0.00214869,	-9.03565e-05,	1.88767e-08,
0.96,	-0.00305084,	-9.00732e-05,	1.64869e-06,
0.9427,	-0.00382792,	-6.53428e-05,	-2.61493e-06,
0.9216,	-0.00467747,	-0.000104566,	4.8122e-06,
0.8962,	-0.00536222,	-3.23834e-05,	-5.43445e-06,
0.8679,	-0.00609364,	-0.0001139,	3.32521e-06,
0.835,	-0.00698325,	-6.40219e-05,	9.34582e-07,
0.7986,	-0.00755337,	-5.00038e-05,	9.35532e-07,
0.7597,	-0.00798325,	-3.59716e-05,	-2.27604e-06,
0.7186,	-0.00851366,	-7.0112e-05,	-8.63072e-06,
0.6732,	-0.00986209,	-0.000199572,	1.91978e-05,
0.6213,	-0.010418,	8.83948e-05,	6.24031e-06,
0.5722,	-0.00906601,	0.000181999,	6.24033e-06,
0.5322, 0.,0.,0.  },
Y[] = {
0,	0.0124,	3.72529e-10,	1.15484e-09,
0.062,	0.0124001,	1.76951e-08,	-5.92321e-09,
0.124,	0.0123998,	-7.09668e-08,	2.25753e-08,
0.186,	0.0124008,	2.66917e-07,	-8.44523e-08,
0.248,	0.0123971,	-9.99682e-07,	3.15569e-07,
0.31,	0.0124108,	3.73349e-06,	-1.1779e-06,
0.372,	0.0123598,	-1.3935e-05,	4.39588e-06,
0.434,	0.0125501,	5.20034e-05,	-1.00051e-05,
0.4968,	0.0123198,	-9.80735e-05,	9.22397e-06,
0.5571,	0.0120308,	4.02857e-05,	-5.2901e-06,
0.6176,	0.0120369,	-3.90662e-05,	7.36117e-07,
0.6769,	0.0117015,	-2.80246e-05,	-8.54283e-07,
0.7346,	0.0113572,	-4.08389e-05,	-5.18524e-07,
0.7903,	0.0109099,	-4.86169e-05,	-1.0718e-06,
0.8435,	0.0103433,	-6.46934e-05,	5.36384e-09,
0.8936,	0.00969679,	-6.46129e-05,	-8.54894e-06,
0.9394,	0.00840949,	-0.000192847,	-4.21023e-06,
0.9761,	0.00616525,	-0.000256001,	-4.21021e-06,
1., 0.,0.,0 };
#define FXC	0.8487
#define FYC	1.3523
#define C1	11.45915590261646417544
#define RC1	0.08726646259971647884
#define NODES	18
#define ONEEPS	1.000001
#define EPS	1e-8
#define HALFPI		1.5707963267948966
#define FORTPI		0.78539816339744833
#define PI		3.14159265358979323846
#define TWOPI		6.2831853071795864769
#define RAD_TO_DEG	57.29577951308232
#define DEG_TO_RAD	.0174532925199432958

    extern struct project_attr p_PRJ;

    extern struct vcs_legacy_marker Vma_tab;

    extern char PRJ_names[PRJ_TYPES][17];

    extern FILE *fpin,*fpout,*fperr;

    extern struct projection_attr p_PRJ_list;
    extern struct project_attr p_PRJ;

int robinson(
	int num,		/* Number of points to project.		*/
	Gpoint lam_phi[],
	Gpoint x_y[]
	);

int mollweide(
	int num,		/* Number of points to project.		*/
	Gpoint lam_phi[],
	Gpoint x_y[]
	);

int polar(
	int num,		/* Number of points to project.		*/
	Gpoint lam_phi[],
	Gpoint x_y[]
	);


double deg2DMS(float in)
{
  /* convert degreees in DDDMMMSSS.ss format */
  int itmp;
  double out;
  float ftmp,ftmp2;


  itmp=(int)in;
  out=(float)itmp;
  ftmp=in-out; /* fractional part of in*/
  out=out*1000000.;
  itmp=ftmp*60.;
  out=out+(double)itmp*1000.;
  ftmp=ftmp-(double)itmp/60.; /* fract miuns the minutes */
  itmp=ftmp*3600.;
  out=out+(double)itmp;
  ftmp=ftmp-(double)itmp/3600.; /* frac minus minutes and seconds */
  out=out+ftmp;
  return out;
}


float reset_lon(float lon)
{
  while (lon>180.) lon=lon-360;;
  while (lon<-180.) lon=lon+360.;
  return lon;
}

void setProj(char *proj)
{
  struct project_attr *pj;
  struct projection_attr *prj;
  int i;
  float tmp;
  pj=&p_PRJ;
  
  for (prj=&p_PRJ_list;prj!=NULL;prj=prj->next)
    {
      if (strcmp(prj->name,proj)==0)
	{
	  for (i=0;i<15;i++) pj->parm[i]=prj->parm[i];
	  pj->proj_type=prj->proj_type;
/* 	  printf("projection type is: %d\n",pj->proj_type); */
	  /* Now projection specific stuff !*/
	  if (((pj->proj_type>=3)&&(pj->proj_type<=19))
	      ||((pj->proj_type>=27)&&(pj->proj_type<=30))
	      ||(pj->proj_type==25)
	      ||(pj->proj_type==21)
	      ||(pj->proj_type==22))
	    {
/* 	      printf("in test ! param4 is %f\n",pj->parm[4]); */
	      if (pj->parm[4]>9.e19)
		{
		  tmp=reset_lon((pj->X1+pj->X2)/2.);
/* 		  printf("param 4 reset to %f\n",tmp); */
		  pj->parm[4]=deg2DMS(tmp);
		};
	    }
	  if (((pj->proj_type>=3)&&(pj->proj_type<=15))
	      ||(pj->proj_type==17)
	      ||(pj->proj_type==19)
	      ||(pj->proj_type==20))
	    {
	      if (pj->parm[5]>9.e19)
		{
		  tmp=(pj->Y1+pj->Y2)/2.;
/* 		  printf("param 5 reset to %f\n",tmp); */
		  pj->parm[5]=deg2DMS(tmp);
		}
	    }
	  if ((pj->proj_type==3)||(pj->proj_type==4))
	    {
	      if (pj->parm[2]>9.e19)
		{
/* 		  printf("param 2 reset to %f\n",pj->Y1); */
		  pj->parm[2]=deg2DMS(pj->Y1);
		}
	      if (pj->parm[3]>9.e19)
		{
/* 		  printf("param 3 reset to %f\n",pj->Y2); */
		  pj->parm[3]=deg2DMS(pj->Y2);
		}
	    }
	  if (pj->proj_type==8)
	    {
	      if (pj->parm[8]==1)
		{
		  if (pj->parm[2]>9.e19)
		    {
		      pj->parm[2]=deg2DMS(pj->Y1);
		    }
		  if (pj->parm[3]>9.e19)
		    {
		      pj->parm[3]=deg2DMS(pj->Y2);
		    }
		}
	      else if ((pj->parm[8]==0)||(pj->parm[8]>9.E19))
		{
		  if (pj->parm[2]>9.e19)
		    {
		      tmp=(pj->Y1+pj->Y2)/2.;
		      pj->parm[2]=deg2DMS(tmp);
		    }
		}
	    }
	  else if ((pj->proj_type==9)||(pj->proj_type==20))
	    {
	      pj->parm[2]=1.;
	    }
	  
	  /* Now everything that has not been set is set to default=0*/
	  for (i=0;i<15;i++) {
	    if (pj->parm[i]>9.e19) pj->parm[i]=0.;
/* 	    printf("Paramter %d, is: %f\n",i,pj->parm[i]); */
	  }
	}
    }
}


/*		Plot lines after projecting into viewing space.		*/

int proj_pl (n,pxy)
     int n;
     Gpoint pxy[];
{
  int i,j,m,k,ierr,nsubpoint=250;
  Gpoint *pnt;
  double tmp1,tmp2;
  struct project_attr *pj;
  double dx,dy,ddx,ddy,dpx,dpy,x0,y0,r,dr,pol,otmp1,otmp2;
  double Precis = 250.0;
  
  if (n <= 1) return 1;
/*   printf("alloc: %d\n",i); */

  
  pj=&p_PRJ;
  if (n <= 1)
    return 1;
  
  if (n <= 1) return 1;

  if (n >= 10000)
    {
      err_warn(1,fperr,"Error - too many points for fill area.\n");
      printf("too many\n");
      return 0;
    }
  m=1;
  if (pj->proj_type>0)
    {

      i=(n-1)*(nsubpoint+1);
      if ((pnt=(Gpoint *)malloc(i*sizeof(Gpoint)))==NULL) 
	{
	  err_warn(1,fperr,"Error: Memory overflow in proj_gks pnt malloc.\n");
	  printf("mem oveflo\n");
	  return 0;
	}
      k=0;
      dr=1.E-10;
/*       dr=10; */
      for (i=0; i < n-1; i++)
	{
	  m=0;
	  dx=(double)(pxy[i+1].x-pxy[i].x);
	  dy=(double)(pxy[i+1].y-pxy[i].y);
	  m=nsubpoint;
	  dx=dx/(double)m;
	  dy=dy/(double)m;
	  for (j=0;j<m+1;j++)
	    {
	      ddx=(double)pxy[i].x+dx*(double)j;
	      ddy=(double)pxy[i].y+dy*(double)j;
/* 	      pnt[k].x=ddx; */
/* 	      pnt[k].y=ddy; */
	      otmp1=1.e20; otmp2=1.e20;
	      ierr=gctp_conv(ddx,ddy,&otmp1,&otmp2,0);
	      if (ierr==0) 
		{
		  pnt[k].x=pj->cX+pj->sX*otmp1;
		  pnt[k].y=pj->cY+pj->sY*otmp2;
		  k=k+1;
		}
	    }
	}
      gpl(k,&pnt[0]);
    }
  else
	 {
	   dx=(fabs(pj->x2_NDC-pj->x1_NDC)/fabs(pj->X2-pj->X1))*Precis;
	   dy=(fabs(pj->y2_NDC-pj->y1_NDC)/fabs(pj->Y2-pj->Y1))*Precis;
	   if ((pnt=(Gpoint *)malloc(201*sizeof(Gpoint)))==NULL) 
	     {
	       err_warn(1,fperr,"Error: Memory overflow in proj_gks pnt malloc.\n");
	       return 0;
	     }
	   switch (pj->projno)
	     {
	     case 0:
	       /*		It's a linear projection.				*/
	       for (i=0,j=0; i < n; i++)
		 {
		   pnt[j].x=pj->cX+pj->sX*pxy[i].x;
		   pnt[j].y=pj->cY+pj->sY*pxy[i].y;

		   if (++j == 200)
		     {
		       gpl(200,&pnt[0]);
		       pnt[0].x=pnt[j-1].x;
		       pnt[0].y=pnt[j-1].y;
		       j=1;
		     }
		    }
	       if (j > 1) gpl(j,&pnt[0]);
	       break;
	       
	     case -1:
	       /*		It's a robinson projection.				*/
	       pnt[0].x=pxy[0].x;
	       pnt[0].y=pxy[0].y;
	       for (i=0,j=0; i < n-1; i++)
		 {
		   dpx=pxy[i+1].x-pxy[i].x;
		   dpy=pxy[i+1].y-pxy[i].y;
		   ddx=fabs(dpx)*dx;
		   ddy=fabs(dpy)*dy;
		   /*		     m=sqrt(ddy*ddy+ddx*ddx);		*/
		   m=ddy;
		   if (m > 1)
		     {
		       dpx/=m;
		       dpy/=m;
		       for (k=1;k < m;k++)
			 {
			   if (++j == 200)
			     {
			       x0=pnt[j-1].x;
			       y0=pnt[j-1].y;
			       robinson(200,&pnt[0],&pnt[0]);
			       gpl(200,&pnt[0]);
			       pnt[0].x=x0;
			       pnt[0].y=y0;
			       j=1;
			     }
			   pnt[j].x=pnt[j-1].x+dpx;
			   pnt[j].y=pnt[j-1].y+dpy;
			 }
		     }
		   if (++j == 200)
		     {
		       x0=pnt[j-1].x;
			y0=pnt[j-1].y;
			robinson(200,&pnt[0],&pnt[0]);
			gpl(200,&pnt[0]);
			pnt[0].x=x0;
			pnt[0].y=y0;
			j=1;
		     }
		   pnt[j].x=pxy[i+1].x;
		   pnt[j].y=pxy[i+1].y;
		 }
	       if (j > 0)
		 {
		   robinson(j+1,&pnt[0],&pnt[0]);
		   gpl(j+1,&pnt[0]);
		 }
	       break;
	     case -2:
	       /*		It's a mollweide projection.				*/
	       pnt[0].x=pxy[0].x;
	       pnt[0].y=pxy[0].y;
	       for (i=0,j=0; i < n-1; i++)
		 {
		   dpx=pxy[i+1].x-pxy[i].x;
		   dpy=pxy[i+1].y-pxy[i].y;
		   ddx=fabs(dpx)*dx;
		   ddy=fabs(dpy)*dy;
		   /*		     m=sqrt(ddy*ddy+ddx*ddx);		*/
		   m=ddy;
		   if (m > 1)
		     {
		       dpx/=m;
		       dpy/=m;
		       for (k=1;k < m;k++)
			 {
			   if (++j == 200)
			     {
			       x0=pnt[j-1].x;
			       y0=pnt[j-1].y;
			       mollweide(200,&pnt[0],&pnt[0]);
			       gpl(200,&pnt[0]);
			       pnt[0].x=x0;
			       pnt[0].y=y0;
			       j=1;
			     }
			   pnt[j].x=pnt[j-1].x+dpx;
			   pnt[j].y=pnt[j-1].y+dpy;
			 }
		     }
		   if (++j == 200)
		     {
		       x0=pnt[j-1].x;
		       y0=pnt[j-1].y;
		       mollweide(200,&pnt[0],&pnt[0]);
		       gpl(200,&pnt[0]);
		       pnt[0].x=x0;
		       pnt[0].y=y0;
		       j=1;
		     }
		   pnt[j].x=pxy[i+1].x;
		   pnt[j].y=pxy[i+1].y;
		 }
	       if (j > 0)
		 {
		   mollweide(j+1,&pnt[0],&pnt[0]);
		   gpl(j+1,&pnt[0]);
		 }
	       break;
	     case -3:
	       
	       /*		It's a polar projection.				*/
	       pnt[0].x=pxy[0].x;
	       pnt[0].y=pxy[0].y;
	       pol = (pj->Y1 < pj->Y2) ? -90 : 90;
	       
	       /*		   dy=(fabs(pj->y2_NDC-pj->y1_NDC)/fabs(pj->Y2-pj->Y1))*Precis;
				   r=(   fabs(pj->y2_NDC-pj->y1_NDC)/
				   (2*fabs(pj->Y2-pj->Y1))	 )  *  Precis  */
	       
	       r=dy;
	       for (i=0,j=0; i < n-1; i++)
		 {
		   dr=r*fabs(0.5*(pxy[i].y+pxy[i+1].y)-pol)*DEG_TO_RAD;
		   dpx=pxy[i+1].x-pxy[i].x;
		   dpy=pxy[i+1].y-pxy[i].y;
		   ddx=dr*fabs(dpx);
		   m=ddx;
		   if (m > 1)
		     {
		       dpx/=m;
		       dpy/=m;
		       for (k=1;k < m;k++)
			 {
			   if (++j == 200)
			     {
			       x0=pnt[j-1].x;
			       y0=pnt[j-1].y;
			       polar(200,&pnt[0],&pnt[0]);
			       gpl(200,&pnt[0]);
			       pnt[0].x=x0;
			       pnt[0].y=y0;
			       j=1;
			     }
			   pnt[j].x=pnt[j-1].x+dpx;
			   pnt[j].y=pnt[j-1].y+dpy;
			 }
		     }
		   if (++j == 200)
		     {
		       x0=pnt[j-1].x;
		       y0=pnt[j-1].y;
		       polar(200,&pnt[0],&pnt[0]);
		       gpl(200,&pnt[0]);
		       pnt[0].x=x0;
		       pnt[0].y=y0;
		       j=1;
		     }
		   pnt[j].x=pxy[i+1].x;
		   pnt[j].y=pxy[i+1].y;
		 }
	       if (j > 0)
		 {
		   polar(j+1,&pnt[0],&pnt[0]);
		   gpl(j+1,&pnt[0]);
		 }
	       break;
	     default:
	       err_warn(1,fperr,"Error - projection not available.\n");
	       free(pnt);
	       return 1;
	     }
	 }
       free(pnt);
       return 0;
      }

/*		Plot text after projecting into viewing space.	*/
/* Added by C.Doutriaux for projections and primitives */
int proj_gtx (pxy,cpts)
     Gpoint pxy;
     unsigned char *cpts;
{
  int i,j,ierr,m;
  double dx,dy,otmp1,otmp2;
  Gpoint pnt;
  
  struct project_attr *pj;
  
  pj=&p_PRJ;
  
/*   if (n < 1) return 1; */
  
  pj=&p_PRJ;
  if (pj->proj_type>0)
    {
      dx=(double)pxy.x;
      dy=(double)pxy.y;
      otmp1=1.e20; otmp2=1.e20;
      ierr=gctp_conv(dx,dy,&otmp1,&otmp2,0);
      if (ierr==0) 
	{
	  pnt.x=pj->cX+pj->sX*otmp1;
	  pnt.y=pj->cY+pj->sY*otmp2;
	}
      /* 	  printf("used: %d\n",k); */
      gtx(&pnt,cpts);
    }
  else
    {
      switch (pj->projno)
	{
	case 0:
	  
	  /*		It's a linear projection.				*/
	  pnt.x=pj->cX+pj->sX*pxy.x;
	  pnt.y=pj->cY+pj->sY*pxy.y;
	  gtx(&pnt,cpts);
	  break;
	case -1:
	  /*		It's a robinson projection.				*/
	  
	  pnt.x=pxy.x;
	  pnt.y=pxy.y;
	  robinson(1,&pnt,&pnt);
	  gtx(&pnt,cpts);
	  
	  break;
	case -2:
	  /*		It's a mollweide projection.				*/
	  pnt.x=pxy.x;
	  pnt.y=pxy.y;
	  mollweide(1,&pnt,&pnt);
	  gtx(&pnt,cpts);
	  break;
	case -3:
	  /*		It's a polar projection.				*/
	  pnt.x=pxy.x;
	  pnt.y=pxy.y;
	  polar(1,&pnt,&pnt);
	  gtx(&pnt,cpts);
	  break;
	default:
	  err_warn(1,fperr,"Error - projection not available.\n");
	  return 1;
	}
    }
  return 0;
}
/*		Plot markers after projecting into viewing space.	*/

int proj_pm (n,pxy)
     int n;
     Gpoint pxy[];
{
  int i,j,ierr,m;
  double dx,dy,otmp1,otmp2;
  Gpoint *pnt;
  
  struct project_attr *pj;
  
  pj=&p_PRJ;
  
  if (n < 1) return 1;
  
  pj=&p_PRJ;
  if (pj->proj_type>0)
    {
      if ((pnt=(Gpoint *)malloc(n*sizeof(Gpoint)))==NULL)
	{
	  err_warn(1,fperr,"Error: Memory overflow in proj_gks pnt malloc.\n");
	  return 0;
	}
      for (i=0; i < n; i++)
	{
	  dx=(double)pxy[i].x;
	  dy=(double)pxy[i].y;
	  otmp1=1.e20; otmp2=1.e20;
	  ierr=gctp_conv(dx,dy,&otmp1,&otmp2,0);
	  if (ierr==0) 
	    {
	      pnt[i].x=pj->cX+pj->sX*otmp1;
	      pnt[i].y=pj->cY+pj->sY*otmp2;
	    }
	}
      /* 	  printf("used: %d\n",k); */
      vgpm(n,&pnt[0]);
    }
  else
    {
      if ((pnt=(Gpoint *)malloc(201*sizeof(Gpoint)))==NULL) 
	{
	  err_warn(1,fperr,"Error: Memory overflow in proj_gks pnt malloc.\n");
	  return 0;
	}
      switch (pj->projno)
	{
	case 0:
	  
	  /*		It's a linear projection.				*/
	  for (i=0,j=0; i < n; i++)
	    {
	      pnt[j].x=pj->cX+pj->sX*pxy[i].x;
	      pnt[j].y=pj->cY+pj->sY*pxy[i].y;
	      
	      if (++j == 200)
		{
		  vgpm(j,pnt);
		  j=0;
		}
	    }
	  if (j >= 1) {
		 vgpm(j,pnt);
	  }
	  break;
	case -1:
	  /*		It's a robinson projection.				*/
	  
	  for (i=0,j=0; i < n; i++)
	    {
	      pnt[j].x=pxy[i].x;
	      pnt[j].y=pxy[i].y;
	      if (++j == 200)
		{
		  robinson(j,&pnt[0],&pnt[0]);
		  vgpm(j,pnt);
		  j=0;
		}
	    }
	  if (j > 0)
	    {
	      robinson(j,&pnt[0],&pnt[0]);
	      vgpm(j,pnt);
	    }
	  break;
	case -2:
	  /*		It's a mollweide projection.				*/
	  for (i=0,j=0; i < n-1; i++)
	    {
	      pnt[j].x=pxy[i].x;
	      pnt[j].y=pxy[i].y;
	      if (++j == 200)
		{
		  mollweide(j,&pnt[0],&pnt[0]);
		  vgpm(j,pnt);
		  j=0;
		}
	    }
	  if (j > 0)
	    {
	      mollweide(j,&pnt[0],&pnt[0]);
	      vgpm(j,pnt);
	    }
	  break;
	case -3:
	  /*		It's a polar projection.				*/
	  for (i=0,j=0; i < n-1; i++)
	    {
	      pnt[j].x=pxy[i].x;
	      pnt[j].y=pxy[i].y;
	      if (++j == 200)
		{
		  polar(j,&pnt[0],&pnt[0]);
		  vgpm(j,pnt);
		  j=0;
		}
	    }		     
	  if (j > 0)
	    {
	      polar(j,&pnt[0],&pnt[0]);
	      vgpm(j,pnt);
	    }
	  break;
	default:
	  err_warn(1,fperr,"Error - projection not available.\n");
	  return 1;
	}
    }
  return 0;
}
/*		Plot markers after projecting into viewing space.	*/

int proj_pm_dean (n,pxy)
     int n;
     Gpoint pxy[];
{
  int i,j,k,m;
  Gpoint pnt[210];
  
  struct project_attr *pj;
  
  pj=&p_PRJ;
  
  if (n < 1) return 1;
  
  switch (pj->projno)
    {
    case 0:
      
      /*		It's a linear projection.				*/
      
      for (i=0,j=0; i < n; i++)
	{
	  pnt[j].x=pj->cX+pj->sX*pxy[i].x;
	  pnt[j].y=pj->cY+pj->sY*pxy[i].y;
	  
	  if (++j == 200)
	    {
	      vgpm(j,pnt);
	      j=0;
	    }
	}
      if (j > 1) vgpm(j,pnt);
      
      break;
    case -1:
      /*		It's a robinson projection.				*/
      
		  for (i=0,j=0; i < n; i++)
		    {
		      pnt[j].x=pxy[i].x;
		      pnt[j].y=pxy[i].y;
		      if (++j == 200)
			{
			  robinson(j,&pnt[0],&pnt[0]);
			  vgpm(j,pnt);
			  j=0;
			}
		    }
		  if (j > 0)
		    {
		      robinson(j,&pnt[0],&pnt[0]);
		      vgpm(j,pnt);
		    }
		  break;
    case -2:
      /*		It's a mollweide projection.				*/
      for (i=0,j=0; i < n-1; i++)
	{
	  pnt[j].x=pxy[i].x;
	  pnt[j].y=pxy[i].y;
	  if (++j == 200)
	    {
	      mollweide(j,&pnt[0],&pnt[0]);
	      vgpm(j,pnt);
	      j=0;
	    }
	}
      if (j > 0)
	{
	  mollweide(j,&pnt[0],&pnt[0]);
	  vgpm(j,pnt);
	}
      break;
    case -3:
      /*		It's a polar projection.				*/
      for (i=0,j=0; i < n-1; i++)
	{
	  pnt[j].x=pxy[i].x;
	  pnt[j].y=pxy[i].y;
	  if (++j == 200)
	    {
	      polar(j,&pnt[0],&pnt[0]);
	      vgpm(j,pnt);
		        j=0;
	    }
	}		     
      if (j > 0)
	{
	  polar(j,&pnt[0],&pnt[0]);
	  vgpm(j,pnt);
	}
      break;
    default:
      err_warn(1,fperr,"Error - projection not available.\n");
      return 1;
    }
  return 0;
}

/* Invert function to go from point value to world coordiantes value */
Gpoint invert_proj_convert (Gpoint pxy)
{
  int i,j,ierr,m;
  double dx,dy,otmp1,otmp2;
  Gpoint pnt;
  
  struct project_attr *pj;
  
  pj=&p_PRJ;
  ierr=0;
  /*		It's a linear projection. */
  if (pj->proj_type==0)
    {
      pnt.x=(pxy.x-pj->cX)/pj->sX;
      pnt.y=(pxy.y-pj->cY)/pj->sY;
    }
  else if (pj->proj_type>0)
    {
      dx=(pxy.x-pj->cX)/pj->sX;
      dy=(pxy.y-pj->cY)/pj->sY;
      otmp1=1.e20; otmp2=1.e20;
      ierr=gctp_conv(dx,dy,&otmp1,&otmp2,1);
      pnt.x=otmp1;
      pnt.y=otmp2;
    }
/*   printf("out: %f, %f\n",pnt.x,pnt.y);    */
  if ((ierr!=0) || (pj->proj_type<0))
    {
      pnt.x=pnt.y=1.e20;
    }
  return pnt;
}

/* Function to get value for a single point */
Gpoint proj_convert (pxy)
     Gpoint pxy;
{
  int i,j,ierr,m;
  double dx,dy,otmp1,otmp2;
  Gpoint pnt;
  
  struct project_attr *pj;
  
  pj=&p_PRJ;
  if (pj->proj_type>0)
    {
      dx=(double)pxy.x;
      dy=(double)pxy.y;
      otmp1=1.e20; otmp2=1.e20;
      ierr=gctp_conv(dx,dy,&otmp1,&otmp2,0);
      if (ierr==0) 
	{
	  pnt.x=pj->cX+pj->sX*otmp1;
	  pnt.y=pj->cY+pj->sY*otmp2;
	}
    }
  else
    {
      pnt.x=pxy.x;
      pnt.y=pxy.y;
      switch (pj->projno)
	{
	case 0:
	  
	  /*		It's a linear projection.				*/
	  pnt.x=pj->cX+pj->sX*pxy.x;
	  pnt.y=pj->cY+pj->sY*pxy.y;
	      
	  break;
	case -1:
	  /*		It's a robinson projection.				*/
	  robinson(1,&pnt,&pnt);
	  break;
	case -2:
	  /*		It's a mollweide projection.				*/
	  mollweide(1,&pnt,&pnt);
	  break;
	case -3:
	  /*		It's a polar projection.				*/
	  polar(1,&pnt,&pnt);
	  break;
	default:
	  err_warn(1,fperr,"Error - projection not available.\n");
	  return pnt;
	}
    }
  return pnt;
}


/* Function that converts x/y with gctp if rever tis 1 then does inverse projection*/
int gctp_conv( double xin, double yin,double *xout, double *yout,int revert)
{ 
  char efile[200],pfile[200];
  char fn27[200],fn83[200];
  double incoor[2];
  double outcoor[2];
  long insys,inzone,inspheroid,ipr,jpr,inunit,iflg;
  long outsys,outzone,outspheroid,outunit;
  double inparm[15],outparm[15];
  struct project_attr *pj;
  int j;
  double pi=3.1415926535897931;

  pj=&p_PRJ;
  /* Now a simple test of gctp do nothing goes from geo to geo ! */
  strcpy(fn27,"/work/proj/Packages/vcs_legacy/Src/library/projections/nad1927.dat");
  strcpy(fn83,"/work/proj/Packages/vcs_legacy/Src/library/projections/nad1983.dat");
  strcpy(efile,"gctp_err_msg.txt");
  strcpy(pfile,"gctp_proj_err_msg.txt");
  insys=0;
  inzone=62;
  /* since it is geo as input, everything is intialized to 0*/
  for (j=0;j<15;j++) 
    {
      inparm[j]=0.;
      outparm[j]=pj->parm[j];
    }
  outsys=pj->proj_type;
  inunit=0.; /* radians */
  inspheroid=0; /* clark 1866*/
  ipr=3; /* send error to terminal if 0, file if 1, both if 2 nowhere if else */
  jpr=3; /* proj error to terminal if 0, file if 1, both if 2 nowhere if else */
  
  outunit=2;
  outzone=62;
  outspheroid=0; /* clark 1866*/
  iflg=0;
  /* Creates INCOOR array for GCTP method (converts to radians)*/
  incoor[0]=(double )xin;
  incoor[1]=(double )yin;
    
/*   printf("All right requesting output of type: %d\n",outsys); */
/*   for (j=0;j<15;j++) printf("Parameter %d, value %f\n",j,outparm[j]); */
  /* mapping*/
  if (revert!=1)
    {
      incoor[0]=incoor[0]/180.*pi;
      incoor[1]=incoor[1]/180.*pi;
      gctp(incoor,&insys,&inzone,\
	   inparm,&inunit,&inspheroid,\
	   &ipr,efile,&jpr,pfile,\
	   outcoor,&outsys,&outzone,\
	   outparm,&outunit,&outspheroid,\
	   fn27, fn83, &iflg);
    }
  else
    {
/*       printf("I got in: %f,%f\n",incoor[0],incoor[1]); */
      gctp(incoor,&outsys,&outzone,\
	   outparm,&outunit,&outspheroid,\
	   &ipr,efile,&jpr,pfile,\
	   outcoor,&insys,&inzone,\
	   inparm,&inunit,&inspheroid,\
	   fn27, fn83, &iflg);
/*       printf("I got out: %f,%f\n",outcoor[0],outcoor[1]); */
      outcoor[0]=outcoor[0]*180./pi;
      outcoor[1]=outcoor[1]*180./pi;
/*       printf("I got out: %f,%f\n",outcoor[0],outcoor[1]); */
    }
  *xout=outcoor[0];
  *yout=outcoor[1];

  return iflg;
    }


/*		Fill area after projecting into viewing space.		*/

int proj_gfa (n,pxy)
     int n;
     Gpoint pxy[];
{
  int i,j,m,k,ierr,nsubpoint=250;
  Gpoint *pnt;
/*   Gpoint *pxy2; */
  double dx,dy,ddx,ddy,dpx,dpy,x0,y0,r,dr,pol;
  double px1,py1,px2,py2,pnx1,pny1,pnx2,pny2;
  double Precis = 250.0;
  struct project_attr *pj;
  double otmp1,otmp2;  
  float XS[4],YS[4];
  float low,high;

  extern int isInsidePolygon(float X, float Y, int n,float *XS,float *YS);
    
  low=1.;
  high=1.;

  low=.9;
  high=1.1;
  pj=&p_PRJ;
  pnt=NULL;
  if (n <= 1)
    return 1;
  
  if (n <= 1) return 1;

  if (n >= 10000)
    {
      err_warn(1,fperr,"Error - too many points for fill area.\n");
      return 0;
    }
  m=1;
  if (pj->proj_type>0)
    {
      /* Test if polygon is inside proj domain... */
      /* This is needed for fillarea continents stufff */
      if (pj->X1<0)
	{
	  XS[0]=pj->X1*high;
	  XS[1]=pj->X1*high;
	}
      else
	{
	  XS[0]=pj->X1*low;
	  XS[1]=pj->X1*low;
	}
      if (pj->X2<0)
	{
	  XS[2]=pj->X2*low;
	  XS[3]=pj->X2*low;
	}
      else
	{
	  XS[2]=pj->X2*high;
	  XS[3]=pj->X2*high;
	}

      if (pj->Y1<0)
	{
	  YS[0]=pj->Y1*high;
	  YS[3]=pj->Y1*high;
	}
      else
	{
	  YS[0]=pj->Y1*low;
	  YS[3]=pj->Y1*low;
	}
      if (pj->Y2<0)
	{
	  YS[1]=pj->Y2*low;
	  YS[2]=pj->Y2*low;
	}
      else
	{
	  YS[1]=pj->Y2*high;
	  YS[2]=pj->Y2*high;
	}
      ierr=0;
      for (i=0; i<n ; i++)
	{
	  ierr=ierr+isInsidePolygon((float)pxy[i].x,(float)pxy[i].y,4,XS,YS);
	}
      if (ierr>0)
	{
	  i=(n)*(nsubpoint+1);
	  /*       printf("allocated:%d\n",i); */
	  if ((pnt=(Gpoint *)malloc(i*sizeof(Gpoint)))==NULL)
	    {
	      err_warn(1,fperr,"Error: Memory overflow in proj_gks pnt malloc.\n");
	      return 0;
	    }
	  /*      if ((pxy2=(Gpoint *)malloc((n+1)*sizeof(Gpoint)))==NULL)  */
	  /* 	{ */
	  /* 	  err_warn(1,fperr,"Error: Memory overflow in proj_gks pxy2 malloc.\n"); */
	  /* 	  return 0; */
	  /* 	} */
	  /*      for (i=0;i<n;i++) */
	  /*        { */
	  /* 	 pxy2[i].x=pxy[i].x; */
	  /* 	 pxy2[i].y=pxy[i].y; */
	  /*        } */
	  /*      pxy2[n].x=pxy[0].x; */
	  /*      pxy2[n].y=pxy[0].y; */
	  /*      n=n+1; */
	  k=0;
	  dr=1.E-10;
	  /* 	  for (i=0; i < n; i++) printf("Coords: (%f, %f)\n",pxy2[i].x,pxy2[i].y); */
	  for (i=0; i < n; i++)
	    {
	      m=0.;
	      if (i<n-1){
		dx=(double)(pxy[i+1].x-pxy[i].x);
		dy=(double)(pxy[i+1].y-pxy[i].y);
	      }
	      else {
		dx=(double)(pxy[0].x-pxy[i].x);
		dy=(double)(pxy[0].y-pxy[i].y);
	      }
	      m=nsubpoint;
	      dx=dx/(double)m;
	      dy=dy/(double)m;
	      /* 	      printf("%i -- %f, %f, %i\n",i,dx,dy,m); */
	      for (j=0;j<m+1;j++)
		{
		  /* 		  printf("m is: %i\n",m); */
		  ddx=(double)pxy[i].x+dx*(double)j;
		  ddy=(double)pxy[i].y+dy*(double)j;
		  /* 	     pnt2[k].x=ddx; */
		  /* 	     /\* 		  ddy=(double)pxy2[i].y; *\/ */
		  /* /\* 	     pnt2[k].y=ddy; *\/ */
		  otmp1=1.e20; otmp2=1.e20;
		  ierr=gctp_conv(ddx,ddy,&otmp1,&otmp2,0);
		  if (ierr==0)
		    {
		      /* 		    printf("Returned: %i, %f, %f\n",ierr, otmp1,otmp2); */
		      pnt[k].x=pj->cX+pj->sX*otmp1;
		      pnt[k].y=pj->cY+pj->sY*otmp2;
		      k=k+1;
		    }
		}
	    }
	  /*      else */
	  /* 	{ */
	  /* 	  k=n; */
	  /* 	  for (i=0; i < n; i++) */
	  /* 	    { */
	  /* 	      pnt2[i].x=(double)pxy[i].x; */
	  /* 	      pnt2[i].y=(double)pxy[i].y; */
	  /* 	      gctp_conv((double)pxy[i].x,(double)pxy[i].y,&otmp1,&otmp2,0); */
	  /* 	      pnt[i].x=pj->cX+pj->sX*otmp1; */
	  /* 	      pnt[i].y=pj->cY+pj->sY*otmp2; */
	  /* 	    } */
	  /* 	} */
	  /* /\*       printf("Total points: %i\n",k); *\/ */
	  /*       for (i=0; i < k; i++) printf("FCoords: (%f, %f)\n",pnt2[i].x,pnt2[i].y); */
	  /*      printf("used %d\n",k); */
	  gfa(k,&pnt[0]);
	  /*       free(pxy2); */
	}
    }
  else
    {
      pnx1 = pj->x1_NDC; pnx2 = pj->x2_NDC;
      pny1 = pj->y1_NDC; pny2 = pj->y2_NDC;
      px1 = pj->X1; px2 = pj->X2;
      py1 = pj->Y1; py2 = pj->Y2;
      dx=(fabs(pnx2-pnx1)/fabs(px2-px1))*Precis;
      dy=(fabs(pny2-pny1)/fabs(py2-py1))*Precis;
  
      if ((pnt=(Gpoint *)malloc(10000*sizeof(Gpoint)))==NULL)
	{
	  err_warn(1,fperr,"Error: Memory overflow in proj_gks pnt malloc.\n");
	  return 0;
	}
      switch (pj->projno)
	{
	case 0:
	  /*		It's a linear projection.				*/
	  for (i=0; i < n; i++)
	    {
	      pnt[i].x=pj->cX+pj->sX*pxy[i].x;
	      pnt[i].y=pj->cY+pj->sY*pxy[i].y;
	    }
	  gfa(n,&pnt[0]);
	  break;
	  
	case -1:
	  /*		It's a robinson projection.				*/
	  pnt[0].x=pxy[0].x;
	  pnt[0].y=pxy[0].y;
	  for (i=0,j=0; i < n-1; i++)
	    {
	      dpx=pxy[i+1].x-pxy[i].x;
	      dpy=pxy[i+1].y-pxy[i].y;
	      ddx=fabs(dpx)*dx;
	      ddy=fabs(dpy)*dy;
	      /*		     m=sqrt(ddy*ddy+ddx*ddx);		*/
	      m=ddy;
	      if (m > 1)
		{
		  dpx/=m;
		  dpy/=m;
		  for (k=1;k < m;k++)
		    {
		      if (++j == 10000)
			{
			  err_warn(1,fperr,
				   "Error - too many points for fill.\n");
			  free(pnt);
			  return 0;
			}
		      pnt[j].x=pnt[j-1].x+dpx;
		      pnt[j].y=pnt[j-1].y+dpy;
		    }
		}
	      if (++j == 10000)
		{
		  err_warn(1,fperr,"Error - too many points for fill.\n");
		  free(pnt);
		  return 0;
		}
	      pnt[j].x=pxy[i+1].x;
	      pnt[j].y=pxy[i+1].y;
	    }
	  /*			Take care of closure.				*/
	  
	  dpx=pxy[0].x-pxy[n-1].x;
	  dpy=pxy[0].y-pxy[n-1].y;
	  ddx=fabs(dpx)*dx;
	  ddy=fabs(dpy)*dy;
	  /*		  m=sqrt(ddy*ddy+ddx*ddx);		*/
	  m=ddy;
	  if (m > 1)
	    {
	      dpx/=m;
	      dpy/=m;
	      for (k=1;k < m;k++)
		{
		  if (++j == 10000)
		    {
		      err_warn(1,fperr,
			       "Error - too many points for fill.\n");
		      free(pnt);
		      return 0;
		    }
		  pnt[j].x=pnt[j-1].x+dpx;
		  pnt[j].y=pnt[j-1].y+dpy;
		}
	    }
	  if (j > 0)
	    {
	      robinson(j+1,&pnt[0],&pnt[0]);
	      gfa(j+1,&pnt[0]);
	    }
	  break;
	case -2:
	  /*		It's a mollweide projection.				*/
	  pnt[0].x=pxy[0].x;
	  pnt[0].y=pxy[0].y;
	  for (i=0,j=0; i < n-1; i++)
	    {
	      dpx=pxy[i+1].x-pxy[i].x;
	      dpy=pxy[i+1].y-pxy[i].y;
	      ddx=fabs(dpx)*dx;
	      ddy=fabs(dpy)*dy;
	      /*		     m=sqrt(ddy*ddy+ddx*ddx);		*/
	      m=ddy;
	      if (m > 1)
		{
		  dpx/=m;
		  dpy/=m;
		  for (k=1;k < m;k++)
		    {
		      if (++j == 10000)
			{
			  err_warn(1,fperr,
				   "Error - too many points for fill.\n");
			  free(pnt);
			  return 0;
			}
		      pnt[j].x=pnt[j-1].x+dpx;
		      pnt[j].y=pnt[j-1].y+dpy;
		    }
		}
	      if (++j == 10000)
		{
		  err_warn(1,fperr,"Error - too many points for fill.\n");
		  free(pnt);
		  return 0;
		}
	      pnt[j].x=pxy[i+1].x;
	      pnt[j].y=pxy[i+1].y;
	    }
	  dpx=pxy[0].x-pxy[n-1].x;
	  dpy=pxy[0].y-pxy[n-1].y;
	  ddx=fabs(dpx)*dx;
	  ddy=fabs(dpy)*dy;
	  /*		  m=sqrt(ddy*ddy+ddx*ddx);		*/
	  m=ddy;
	  if (m > 1)
	    {
	      dpx/=m;
	      dpy/=m;
	      for (k=1;k < m;k++)
		{
		  if (++j == 10000)
		    {
		      err_warn(1,fperr,
			       "Error - too many points for fill.\n");
		      free(pnt);
		      return 0;
		    }
		  pnt[j].x=pnt[j-1].x+dpx;
		  pnt[j].y=pnt[j-1].y+dpy;
		}
	    }
	  if (j > 0)
	    {
	      mollweide(j+1,&pnt[0],&pnt[0]);
	      gfa(j+1,&pnt[0]);
	    }
	  break;
	case -3:
	  /*		It's a polar projection.				*/
/* 	  printf("OK DOING POLAR CRAP !\n"); */
	  pnt[0].x=pxy[0].x;
	  pnt[0].y=pxy[0].y;
	  x0 = pnt[0].x;
	  y0 = pnt[0].y;
	  pol = (pj->Y1 < pj->Y2) ? -90.0 : 90.0;
	  
	  /*		   dy=(fabs(pj->y2_NDC-pj->y1_NDC)/fabs(pj->Y2-pj->Y1))*Precis;
			   r=(   fabs(pj->y2_NDC-pj->y1_NDC)/
			   (2*fabs(pj->Y2-pj->Y1))	 )  *  Precis  */
	  
	  r=dy;
	  
	  for (i=0,j=0; i < n-1; i++)
	    {
	      px1 = pxy[i].x; px2 = pxy[i+1].x;
	      py1 = pxy[i].y; py2 = pxy[i+1].y;
	      dr=r*fabs(0.5*(py1+py2)-pol)*DEG_TO_RAD;
	      dpx=px2-px1;
	      dpy=py2-py1;
	      ddx=dr*fabs(dpx);
	      m=ddx;
	      if (m > 1)
		{
		  dpx/=m;
		  dpy/=m;
		  for (k=1;k < m;k++)
		    {
		      if (++j == 10000)
			{
			  err_warn(1,fperr,
				   "Error - too many points for fill.\n");
			  free(pnt);
			  return 0;
			}
		      x0 = x0 + dpx;
		      y0 = y0 + dpy;
		      pnt[j].x = x0;
		      pnt[j].y = y0;
		      /*pnt[j].x=pnt[j-1].x+dpx;
			pnt[j].y=pnt[j-1].y+dpy;*/
		    }
		}
	      if (++j == 10000)
		{
		  err_warn(1,fperr,"Error - too many points for fill.\n");
		  free(pnt);
		  return 0;
		}
	      pnt[j].x=pxy[i+1].x;
	      pnt[j].y=pxy[i+1].y;
	      x0 = pnt[j].x;
	      y0 = pnt[j].y;
	    }
	  px1 = pxy[n-1].x; px2 = pxy[0].x;
	  py1 = pxy[n-1].y; py2 = pxy[0].y;
	  dr=r*fabs(0.5*(py1+py2)-pol)*DEG_TO_RAD;
	  dpx=px2-px1;
	  dpy=py2-py1;
	  ddx=dr*fabs(dpx);
	  m=ddx;
	  if (m > 1)
	    {
	      dpx/=m;
	      dpy/=m;
	      for (k=1;k < m;k++)
		{
		  if (++j == 10000)
		    {
		      err_warn(1,fperr,
			       "Error - too many points for fill.\n");
		      free(pnt);
		      return 0;
		    }
		  x0 = x0 + dpx;
		  y0 = y0 + dpy;
		  pnt[j].x = x0;
		  pnt[j].y = y0;
		  /*pnt[j].x=pnt[j-1].x+dpx;
		    pnt[j].y=pnt[j-1].y+dpy;*/
		}
	    }
	  if (j > 0)
	    {
	      polar(j+1,&pnt[0],&pnt[0]);
	      gfa(j+1,&pnt[0]);
	    }
	  break;
	default:
	  err_warn(1,fperr,"Error - projection not available.\n");
	  free(pnt);
	  return 1;
	}
    }
  if (pnt!=NULL) free(pnt);
  return 0;
}


/*		Set up the projection parameters using
			template NDC x1, y1, x2, y2
			graphics WC  x1, y1, x2, y2
			data WC      x1, y1, x2, y2

		The data dimensions determine the direction.		*/

int set_projection(char *proj,struct pe_dsp pP_dsp,float pG_dsp[],
		   float pA_dsp[])
{
  int ierr;
  float Xmin,Xmax,Ymin,Ymax,X,Y,dX,dY;
  double tmp1,tmp2;
  struct project_attr *pj;
  struct projection_attr *prj;
  
  
  pj=&p_PRJ;
  prj=&p_PRJ_list;
  strcpy (pj->name,proj);  /* Save the projection name.	*/
  
  /*			Find the projection name and set an index.	*/
  while ((prj != NULL) &&
	 (strcmp(prj->name, pj->name) != 0)){
    prj = prj->next;}
  if (prj == NULL) 
    {
  
      pj->projno=0;
      err_warn(1,fperr,
	       "Warning - projection (%s) unavailable - (linear) used.\n",
	       proj);
    }
  else {
    pj->projno=prj->proj_type;
  }

  
  /*			Save NDC space limits.				*/
  
  pj->x1_NDC=pP_dsp.x1;
  pj->y1_NDC=pP_dsp.y1;
  pj->x2_NDC=pP_dsp.x2;
  pj->y2_NDC=pP_dsp.y2;
/*   printf("OK Viewport in is: %f,%f,%f,%f\n",pj->x1_NDC,pj->x2_NDC,pj->y1_NDC,pj->y2_NDC); */
  
  /*		Set the limits of the display (user coordinate) range
		limits for the projection.
		The data dimensions determine direction.		*/
  
  if (fabs(pG_dsp[1]) < 9.e19 && fabs(pG_dsp[3]) < 9.e19)
    {

/*        Following commented out by C.Doutriaux, to allow going from max to min  */
/*       if ( (pA_dsp[3]-pA_dsp[1])*(pG_dsp[3]-pG_dsp[1]) >= 0.0) */
/* 	{ */
	  pj->Y1=pG_dsp[1];
	  pj->Y2=pG_dsp[3];
/* 	} */
/*       else  */
/* 	{ */
/* 	  pj->Y1=pG_dsp[3]; */
/* 	  pj->Y2=pG_dsp[1]; */
/* 	} */
    }
  else
    {
      pj->Y1=pA_dsp[1];
      pj->Y2=pA_dsp[3];	   
    }
  
  
  if (fabs(pG_dsp[0]) < 9.e19 && fabs(pG_dsp[2]) < 9.e19)
    {
/*        Following commented out by C.Doutriaux, to allow going from max to min  */
/*       if ( (pA_dsp[2]-pA_dsp[0])*(pG_dsp[2]-pG_dsp[0]) >= 0.0) */
/* 	{ */
	  pj->X1=pG_dsp[0];
	  pj->X2=pG_dsp[2];
/* 	} */
/*       else  */
/* 	{ */
/* 	  pj->X1=pG_dsp[2]; */
/* 	  pj->X2=pG_dsp[0]; */
/* 	} */
    }
  else
    {
      pj->X1=pA_dsp[0];
      pj->X2=pA_dsp[2];
      
    }

setProj(proj);
/* pj=&p_PRJ; */
  
  
  /*		Check the display region user coordinates and set
		constant values for the projection.			*/
  
  pj->sX = 0.0;
  pj->sY = 0.0;
  pj->cX = 0.0;
  pj->cY = 0.0;
  
  /*			Check NDC space is greater than zero.		*/
  
  if (pP_dsp.x2-pP_dsp.x1 < .001 || pP_dsp.y2-pP_dsp.y1 < .001)
    {
      err_warn(1,fperr,"Error - projection display space (NDC) is nil.\n");
      return 0;
    }
  if (pj->proj_type<=0) /* if linear projection nothing special to do */
    {
      pj->sX=(pj->x2_NDC-pj->x1_NDC)/(pj->X2-pj->X1);
      pj->cX=pj->x1_NDC-pj->X1*pj->sX;
      pj->sY=(pj->y2_NDC-pj->y1_NDC)/(pj->Y2-pj->Y1);
      pj->cY=pj->y1_NDC-pj->Y1*pj->sY;
/*       printf("The coeff decided are: %f,%f,%f,%f,\n",pj->sX,pj->cX,pj->sY,pj->cY); */
/*       printf("The XY were: %f,%f,%f,%f,\n",pj->X1,pj->X2,pj->Y1,pj->Y2); */
    }
  else/* gctp type , need to recompute cx,etc...*/
    {
      Xmin=Ymin=1.E30;
      Xmax=Ymax=-1.E30;
      dX=(pj->X2-pj->X1)/100.;
      dY=(pj->Y2-pj->Y1)/100.;
      /* Computes 100 points along max/min lat/lon*/
      /* and determine max min values */
      for (X=pj->X1;X<pj->X2;X=X+dX)
	{
	  for (Y=pj->Y1;Y<pj->Y2;Y=Y+dY)
	    {
	      ierr=gctp_conv((double)X,(double)Y,&tmp1,&tmp2,0);
	      if (ierr==0)
		{
		  if (tmp1<Xmin) Xmin=tmp1;
		  if (tmp2<Ymin) Ymin=tmp2;
		  if (tmp1>Xmax) Xmax=tmp1;
		  if (tmp2>Ymax) Ymax=tmp2;
		}
	    }
	}
/*       /\* Computes 10 points along max/min lon*\/ */
/*       /\* and determine max min values *\/ */
/*       for (Y=pj->Y1;Y<pj->Y2;Y=Y+dY) */
/* 	{ */
/* 	  gctp_conv((double)pj->X1,(double)Y,&tmp1,&tmp2,0); */
/* 	  if (tmp1<Xmin) Xmin=tmp1; */
/* 	  if (tmp2<Ymin) Ymin=tmp2; */
/* 	  if (tmp1>Xmax) Xmax=tmp1; */
/* 	  if (tmp2>Ymax) Ymax=tmp2; */
/* 	  gctp_conv((double)pj->X2,(double)Y,&tmp1,&tmp2,0); */
/* 	  if (tmp1<Xmin) Xmin=tmp1; */
/* 	  if (tmp2<Ymin) Ymin=tmp2; */
/* 	  if (tmp1>Xmax) Xmax=tmp1; */
/* 	  if (tmp2>Ymax) Ymax=tmp2; */
/* 	} */
      /* Gives a little bit of a margin for text, and niceness, etc...*/
      tmp1=0.0; /* percentage of  room to leave */
      if (Xmin<0.) {
	Xmin=Xmin*(1.+tmp1);
      }else{
	Xmin=Xmin*(1.-tmp1);
      }
      if (Xmax>0.) {
	Xmax=Xmax*(1.+tmp1);
      }else{
	Xmax=Xmax*(1.-tmp1);
      }
      if (Ymin<0.) {
	Ymin=Ymin*(1.+tmp1);
      }else{
	Ymin=Ymin*(1.-tmp1);
      }
      if (Ymax>0.) {
	Ymax=Ymax*(1.+tmp1);
      }else{
	Ymax=Ymax*(1.-tmp1);
      }
      /* Now computes factors to put that on the map*/
      pj->sX=(pj->x2_NDC-pj->x1_NDC)/(Xmax-Xmin);
      pj->cX=pj->x1_NDC-Xmin*pj->sX;
      pj->sY=(pj->y2_NDC-pj->y1_NDC)/(Ymax-Ymin);
      pj->cY=pj->y1_NDC-Ymin*pj->sY;
    }
  return 1;
}

/*		Compute the text extent for a label.			*/

int getextent(pxy,label,pTt,pTo,ext)
     Gpoint *pxy;
     char *label;
     struct table_text *pTt;
     struct table_chorn *pTo;
     Gextent *ext;
{
  float dx,dy;
  float sn,cn,x,y;
  
  if (pTo->chpath == 'r' || pTo->chpath == 'l')
    {
      /*dx=(int)strlen(label)*(pTo->chh*pTt->txexp+pTt->txsp);*/
      dx=(int)strlen(label)*(pTo->chh*pTt->txexp);
      dy=pTo->chh;
    }
  else
    {
      /*dy=(int)strlen(label)*(pTo->chh*pTt->txexp+pTt->txsp);*/
      dy=(int)strlen(label)*(pTo->chh*pTt->txexp);
      dx=pTo->chh;
    }
  if (pTo->chalh == 'c')
    {
      ext->ll.x=-0.5*dx;
      ext->ll.y=-0.5*dy;
      ext->lr.x=+0.5*dx;
      ext->lr.y=-0.5*dy;
      ext->ul.x=+0.5*dx;
      ext->ul.y=+0.5*dy;
      ext->ur.x=-0.5*dx;
      ext->ur.y=+0.5*dy;
    }
  else if (pTo->chalh == 'l')
    {
      ext->ll.x=0.0;
      ext->ll.y=-0.5*dy;
      ext->lr.x=dx;
      ext->lr.y=-0.5*dy;
      ext->ul.x=dx;
      ext->ul.y=+0.5*dy;
      ext->ur.x=0.0;
      ext->ur.y=+0.5*dy;
    }
  else if (pTo->chalh == 'r')
    {
      ext->ll.x=-dx;
      ext->ll.y=-0.5*dy;
      ext->lr.x=0.0;
      ext->lr.y=-0.5*dy;
      ext->ul.x=0.0;
      ext->ul.y=+0.5*dy;
      ext->ur.x=-dx;
      ext->ur.y=+0.5*dy;
    }
  if (pTo->chalv == 't')
    {
      ext->ll.y=-dy;
      ext->lr.y=-dy;
      ext->ul.y=0.0;
      ext->ur.y=0.0;
    }
  if (pTo->chalv == 'c')
    {
      ext->ll.y=-0.9*dy;
      ext->lr.y=-0.9*dy;
      ext->ul.y=0.1*dy;
      ext->ur.y=0.1*dy;
    }
  if (pTo->chalv == 'b')
    {
      ext->ll.y=-0.1*dy;
      ext->lr.y=-0.1*dy;
      ext->ul.y=0.9*dy;
      ext->ur.y=0.9*dy;
    }
  if (pTo->chalv == 's')
    {
      ext->ll.y=0.0;
      ext->lr.y=0.0;
      ext->ul.y=dy;
      ext->ur.y=dy;
    }
  sn=sin(pTo->chua*3.15159265/180.0);
  cn=cos(pTo->chua*3.15159265/180.0);
  x= cn*ext->ll.x+sn*ext->ll.y;
  y=-sn*ext->ll.x+cn*ext->ll.y;
  ext->ll.x=x+pxy->x;
  ext->ll.y=y+pxy->y;
  x= cn*ext->lr.x+sn*ext->lr.y;
  y=-sn*ext->lr.x+cn*ext->lr.y;
  ext->lr.x=x+pxy->x;
  ext->lr.y=y+pxy->y;
  x= cn*ext->ul.x+sn*ext->ul.y;
  y=-sn*ext->ul.x+cn*ext->ul.y;
  ext->ul.x=x+pxy->x;
  ext->ul.y=y+pxy->y;
  x= cn*ext->ur.x+sn*ext->ur.y;
  y=-sn*ext->ur.x+cn*ext->ur.y;
  ext->ur.x=x+pxy->x;
  ext->ur.y=y+pxy->y;
  return 1;
}
/*			Robinson psuedocylindrical projection.		*/

int robinson(
	     int num,		/* Number of points to project.		*/
	     Gpoint lam_phi[],
	     /*	float lam[],		 Longitudes, in degrees, to project.
		float phi[],		 Latitudes, in degrees, to project.
	     */
	     Gpoint x_y[]
	     /*	float x[],		 Output normalized x coordinates.
		float y[]		 Output normalized y coordinates.
	     */
	     )
{
  int i,j;
  float dphi,lamda,fx,fy,lam0,yy;
  struct project_attr *pj;
  
  pj=&p_PRJ;
  
  fx=(pj->x2_NDC-pj->x1_NDC)/TWOPI;
  fy=(pj->y2_NDC-pj->y1_NDC)*0.5;
  lam0=0.5*(pj->X2+pj->X1);
  for (j=0;j<num;j++)
    {
      lamda=(lam_phi[j].x-lam0)*DEG_TO_RAD;
      i = floor((dphi = fabs(DEG_TO_RAD*lam_phi[j].y)) * C1);
      if (i >= NODES) i = NODES - 1;
      dphi = RAD_TO_DEG * (dphi - RC1 * i);
      
      x_y[j].x =pj->x1_NDC+(PI +V(X[i], dphi) * lamda) * fx;
      yy = V(Y[i], dphi);
      if (lam_phi[j].y < 0.0) yy = -yy;
      x_y[j].y=pj->y1_NDC+(1.0 + yy) * fy;
      
    }
  return 1;
}

/*			Mollweide psuedocylindrical projection.		*/

int mollweide(
	      int num,		/* Number of points to project.		*/
	      Gpoint lam_phi[],	/* Lats & lons, in degrees, to project.	*/
	      Gpoint x_y[]		/* Output normalized x &y coordinates.	*/
	      )
{
  double k, fv;
  double fx,fy;
  double lat,lam0,lamda;
  int i,j;
  struct project_attr *pj;
  
  pj=&p_PRJ;
  
  fx=(pj->x2_NDC-pj->x1_NDC)*0.5;
  fy=(pj->y2_NDC-pj->y1_NDC)*0.5;
  lam0=0.5*(pj->X2+pj->X1);
  
  for (i=0;i < num;i++)
    {
      lat=lam_phi[i].y * DEG_TO_RAD;
      lamda=(lam_phi[i].x-lam0)*DEG_TO_RAD;
      k = (PI + sin(PI)) * sin(lat);
      for (j = 10; j ; --j)
	{
	  lat -= fv = (lat + sin(lat) - k) /
	    (1. + cos(lat));
	  if (fabs(fv) < 1.e-7)
	    break;
	}
      if (!j)
	lat = (lat < 0.) ? -HALFPI : HALFPI;
      else
	lat *= 0.5;
      x_y[i].x = pj->x1_NDC + fx * (1.0+(lamda/PI)*cos(lat));
      x_y[i].y = pj->y1_NDC + fy * (1.0 + sin(lat));
    }
  return 1;
}



/*			Polar Stereographic projection.			*/

int polar(
	  int num,		/* Number of points to project.		*/
	  Gpoint lam_phi[],	/* Lats & lons, in degrees, to project.	*/
	  Gpoint x_y[]		/* Output normalized x &y coordinates.	*/
	  )
{
  int i;
  double r,lamda,pol,fx,fy,fx0,fy0,r0,rmax;
  struct project_attr *pj;
  
  pj=&p_PRJ;
  fx=pj->x2_NDC-pj->x1_NDC;
  fx0=(pj->x2_NDC+pj->x1_NDC)*0.5;
  fy=pj->y2_NDC-pj->y1_NDC;
  fy0=(pj->y2_NDC+pj->y1_NDC)*0.5;
  /*	r0=(fabs(fx) > fabs(fy)) ? fy : fx;
	r0=0.5*fabs(r0);
  */
  r0=0.5*fabs(fy);
  pol = (pj->Y1 < pj->Y2) ? -90.0 : 90.0;
  rmax = fabs(pol - pj->Y2);
  if (rmax == 0.0)
    {
      err_warn(1,fperr,"Error - specifying space for polar projection.\n");
      return 0;
    }
  r0=r0/rmax;
  
  for (i=0;i<num;i++)
    {
/*       printf("ok in: lam_phi[i].x lam_phi[i].y, i %f,%f,%i\n",  lam_phi[i].x, lam_phi[i].y, i); */
      lamda=(lam_phi[i].x+pj->X1)*DEG_TO_RAD;
      r=r0*fabs(lam_phi[i].y-pol);
      if (pol < 0.0)
	{
	  x_y[i].x=fx0+r*sin(lamda);
	  x_y[i].y=fy0+r*cos(lamda);
/* 	  printf("ok out: x_y[i].x x_y[i].y, i %f,%f,%i\n",  x_y[i].x, x_y[i].y, i); */
	}
      else
	{
	  x_y[i].x=fx0+r*sin(-lamda);
	  x_y[i].y=fy0+r*cos(-lamda);
/* 	  printf("ok out: x_y[i].x x_y[i].y, i %f,%f,%i\n",  x_y[i].x, x_y[i].y, i); */
	}
    }
  return 1;
}

/*	Projection function.						*/
/*	Transform (x,y) points in user coordinates to normalized	*/
/*	device coordinates, replacing (x,y) values.			*/

int project (n,x,y)
     int n;		/* Number of x,y pairs.				*/
     float x[];	        /* X coordinates in and out.			*/
     float y[];	        /* Y coordinates in and out.			*/
{
  int i,ierr,k;
  struct project_attr *pj;
  double tmp1,tmp2; /* temporary values for projection */
  
  pj=&p_PRJ;
  if (pj->proj_type > 0)
    {
      k=0;
      for (i=0;i<n;i++)
	{
	  ierr=gctp_conv((double)x[i],(double)y[i],&tmp1,&tmp2,0);
	  if (ierr==0)
	    {
	      x[k]=tmp1;
	      y[k]=tmp2;
	      k=k+1;
	    }
	}
      
      n=k;
      for (i=0; i < n; i++)
	{
	  x[i]=pj->cX+pj->sX*x[i];
	  y[i]=pj->cY+pj->sY*y[i];
	}
    }
  else
    { 
      return project_dean(n,x,y);
    }
  return 0;
}

/*	Projection function.						*/
/*	Transform (x,y) points in user coordinates to normalized	*/
/*	device coordinates, replacing (x,y) values.			*/
int project_dean (n,x,y)
     int n;		/* Number of x,y pairs.				*/
     float x[];	/* X coordinates in and out.			*/
     float y[];	/* Y coordinates in and out.			*/
{
  int i;
  Gpoint pnt;
  struct project_attr *pj;
  
  pj=&p_PRJ;
  
  switch (pj->projno)
    {
    case 0:
      
      /*		It's a linear projection.				*/
      
      for (i=0; i < n; i++)
	{
	  x[i]=pj->cX+pj->sX*x[i];
	  y[i]=pj->cY+pj->sY*y[i];
	}
      break;
    case -1:
      /*		It's a robinson projection.				*/
      
      for (i=0; i < n; i++)
	{
	  pnt.x=x[i];
	  pnt.y=y[i];
	  robinson(1,&pnt,&pnt);
	  x[i]=pnt.x;
	  y[i]=pnt.y;
	}
      break;
    case -2:
      /*		It's a mollweide projection.				*/
      for (i=0; i < n; i++)
	{
	  pnt.x=x[i];
	  pnt.y=y[i];
	  mollweide(1,&pnt,&pnt);
	  x[i]=pnt.x;
	  y[i]=pnt.y;
	}
      break;
    case -3:
      /*		It's a polar projection.				*/
      for (i=0; i < n; i++)
	{
	  pnt.x=x[i];
	  pnt.y=y[i];
	  polar(1,&pnt,&pnt);
	  x[i]=pnt.x;
	  y[i]=pnt.y;
	}
      break;
    default:
		  err_warn(1,fperr,"Error - projection not available.\n");
		  return 1;
    }
  return 0;
}
/* int proj_gfa_dean (n,pxy) */
/*      int n; */
/*      Gpoint pxy[]; */
/* { */
/*   int i,j,m,k; */
/*   Gpoint pnt[10000]; */
/*   double dx,dy,ddx,ddy,dpx,dpy,x0,y0,r,dr,pol; */
/*   double px1,py1,px2,py2,pnx1,pny1,pnx2,pny2; */
/*   double Precis = 250.0; */
  
/*   struct project_attr *pj; */
  
/*   pj=&p_PRJ; */
  
/*   if (n <= 1) return 1; */
  
/*   if (n >= 10000) */
/*     { */
/*       err_warn(1,fperr,"Error - too many points for fill area.\n"); */
/*       return 0; */
/*     } */
  
/*   pnx1 = pj->x1_NDC; pnx2 = pj->x2_NDC; */
/*   pny1 = pj->y1_NDC; pny2 = pj->y2_NDC; */
/*   px1 = pj->X1; px2 = pj->X2; */
/*   py1 = pj->Y1; py2 = pj->Y2; */
/*   dx=(fabs(pnx2-pnx1)/fabs(px2-px1))*Precis; */
/*   dy=(fabs(pny2-pny1)/fabs(py2-py1))*Precis; */
  
/*   switch (pj->projno) */
/*     { */
/*     case 0: */
/*       /\*		It's a linear projection.				*\/ */
/*       for (i=0; i < n; i++) */
/* 	{ */
/* 	  pnt[i].x=pj->cX+pj->sX*pxy[i].x; */
/* 	  pnt[i].y=pj->cY+pj->sY*pxy[i].y; */
/* 	} */
/*       gfa(n,pnt); */
/*       break; */
      
/*     case 1: */
/*       /\*		It's a robinson projection.				*\/ */
/*       pnt[0].x=pxy[0].x; */
/*       pnt[0].y=pxy[0].y; */
/*       for (i=0,j=0; i < n-1; i++) */
/* 	{ */
/* 	  dpx=pxy[i+1].x-pxy[i].x; */
/* 	  dpy=pxy[i+1].y-pxy[i].y; */
/* 	  ddx=fabs(dpx)*dx; */
/* 	  ddy=fabs(dpy)*dy; */
/* 	  /\*		     m=sqrt(ddy*ddy+ddx*ddx);		*\/ */
/* 	  m=ddy; */
/* 	  if (m > 1) */
/* 	    { */
/* 	      dpx/=m; */
/* 	      dpy/=m; */
/* 	      for (k=1;k < m;k++) */
/* 		{ */
/* 		  if (++j == 10000) */
/* 		    { */
/* 		      err_warn(1,fperr, */
/* 			       "Error - too many points for fill.\n"); */
/* 		      return 0; */
/* 		    } */
/* 		  pnt[j].x=pnt[j-1].x+dpx; */
/* 		  pnt[j].y=pnt[j-1].y+dpy; */
/* 		} */
/* 	    } */
/* 	  if (++j == 10000) */
/* 	    { */
/* 	      err_warn(1,fperr,"Error - too many points for fill.\n"); */
/* 	      return 0; */
/* 	    } */
/* 	  pnt[j].x=pxy[i+1].x; */
/* 	  pnt[j].y=pxy[i+1].y; */
/* 	} */
/*       /\*			Take care of closure.				*\/ */
      
/*       dpx=pxy[0].x-pxy[n-1].x; */
/*       dpy=pxy[0].y-pxy[n-1].y; */
/*       ddx=fabs(dpx)*dx; */
/*       ddy=fabs(dpy)*dy; */
/*       /\*		  m=sqrt(ddy*ddy+ddx*ddx);		*\/ */
/*       m=ddy; */
/*       if (m > 1) */
/* 	{ */
/* 	  dpx/=m; */
/* 	  dpy/=m; */
/* 	  for (k=1;k < m;k++) */
/* 	    { */
/* 	      if (++j == 10000) */
/* 		{ */
/* 		  err_warn(1,fperr, */
/* 			   "Error - too many points for fill.\n"); */
/* 		  return 0; */
/* 		} */
/* 	      pnt[j].x=pnt[j-1].x+dpx; */
/* 	      pnt[j].y=pnt[j-1].y+dpy; */
/* 	    } */
/* 	} */
/*       if (j > 0) */
/* 	{ */
/* 	  robinson(j+1,&pnt[0],&pnt[0]); */
/* 	  gfa(j+1,pnt); */
/* 	} */
/*       break; */
/*     case 2: */
/*       /\*		It's a mollweide projection.				*\/ */
/*       pnt[0].x=pxy[0].x; */
/*       pnt[0].y=pxy[0].y; */
/*       for (i=0,j=0; i < n-1; i++) */
/* 	{ */
/* 	  dpx=pxy[i+1].x-pxy[i].x; */
/* 	  dpy=pxy[i+1].y-pxy[i].y; */
/* 	  ddx=fabs(dpx)*dx; */
/* 	  ddy=fabs(dpy)*dy; */
/* 	  /\*		     m=sqrt(ddy*ddy+ddx*ddx);		*\/ */
/* 	  m=ddy; */
/* 	  if (m > 1) */
/* 	    { */
/* 	      dpx/=m; */
/* 	      dpy/=m; */
/* 	      for (k=1;k < m;k++) */
/* 		{ */
/* 		  if (++j == 10000) */
/* 		    { */
/* 		      err_warn(1,fperr, */
/* 			       "Error - too many points for fill.\n"); */
/* 		      return 0; */
/* 		    } */
/* 		  pnt[j].x=pnt[j-1].x+dpx; */
/* 		  pnt[j].y=pnt[j-1].y+dpy; */
/* 		} */
/* 	    } */
/* 	  if (++j == 10000) */
/* 	    { */
/* 	      err_warn(1,fperr,"Error - too many points for fill.\n"); */
/* 	      return 0; */
/* 	    } */
/* 	  pnt[j].x=pxy[i+1].x; */
/* 	  pnt[j].y=pxy[i+1].y; */
/* 	} */
/*       dpx=pxy[0].x-pxy[n-1].x; */
/*       dpy=pxy[0].y-pxy[n-1].y; */
/*       ddx=fabs(dpx)*dx; */
/*       ddy=fabs(dpy)*dy; */
/*       /\*		  m=sqrt(ddy*ddy+ddx*ddx);		*\/ */
/*       m=ddy; */
/*       if (m > 1) */
/* 	{ */
/* 	  dpx/=m; */
/* 	  dpy/=m; */
/* 	  for (k=1;k < m;k++) */
/* 	    { */
/* 	      if (++j == 10000) */
/* 		{ */
/* 		  err_warn(1,fperr, */
/* 			   "Error - too many points for fill.\n"); */
/* 		  return 0; */
/* 		} */
/* 	      pnt[j].x=pnt[j-1].x+dpx; */
/* 	      pnt[j].y=pnt[j-1].y+dpy; */
/* 	    } */
/* 	} */
/*       if (j > 0) */
/* 	{ */
/* 	  mollweide(j+1,&pnt[0],&pnt[0]); */
/* 	  gfa(j+1,pnt); */
/* 	} */
/*       break; */
/*     case -3: */
/*       /\*		It's a polar projection.				*\/ */
/*       pnt[0].x=pxy[0].x; */
/*       pnt[0].y=pxy[0].y; */
/*       x0 = pnt[0].x; */
/*       y0 = pnt[0].y; */
/*       pol = (pj->Y1 < pj->Y2) ? -90.0 : 90.0; */
      
/*       /\*		   dy=(fabs(pj->y2_NDC-pj->y1_NDC)/fabs(pj->Y2-pj->Y1))*Precis; */
/* 			   r=(   fabs(pj->y2_NDC-pj->y1_NDC)/ */
/* 			   (2*fabs(pj->Y2-pj->Y1))	 )  *  Precis  *\/ */
      
/*       r=dy; */
      
/*       for (i=0,j=0; i < n-1; i++) */
/* 	{ */
/* 	  px1 = pxy[i].x; px2 = pxy[i+1].x; */
/* 	  py1 = pxy[i].y; py2 = pxy[i+1].y; */
/* 	  dr=r*fabs(0.5*(py1+py2)-pol)*DEG_TO_RAD; */
/* 	  dpx=px2-px1; */
/* 	  dpy=py2-py1; */
/* 	  ddx=dr*fabs(dpx); */
/* 	  m=ddx; */
/* 	  if (m > 1) */
/* 	    { */
/* 	      dpx/=m; */
/* 	      dpy/=m; */
/* 	      for (k=1;k < m;k++) */
/* 		{ */
/* 		  if (++j == 10000) */
/* 		    { */
/* 		      err_warn(1,fperr, */
/* 			       "Error - too many points for fill.\n"); */
/* 		      return 0; */
/* 		    } */
/* 		  x0 = x0 + dpx; */
/* 		  y0 = y0 + dpy; */
/* 		  pnt[j].x = x0; */
/* 		  pnt[j].y = y0; */
/* 		  /\*pnt[j].x=pnt[j-1].x+dpx; */
/* 		    pnt[j].y=pnt[j-1].y+dpy;*\/ */
/* 		} */
/* 	    } */
/* 	  if (++j == 10000) */
/* 	    { */
/* 	      err_warn(1,fperr,"Error - too many points for fill.\n"); */
/* 	      return 0; */
/* 	    } */
/* 	  pnt[j].x=pxy[i+1].x; */
/* 	  pnt[j].y=pxy[i+1].y; */
/* 	  x0 = pnt[j].x; */
/* 	  y0 = pnt[j].y; */
/* 	} */
/*       px1 = pxy[n-1].x; px2 = pxy[0].x; */
/*       py1 = pxy[n-1].y; py2 = pxy[0].y; */
/*       dr=r*fabs(0.5*(py1+py2)-pol)*DEG_TO_RAD; */
/*       dpx=px2-px1; */
/*       dpy=py2-py1; */
/*       ddx=dr*fabs(dpx); */
/*       m=ddx; */
/*       if (m > 1) */
/* 	{ */
/* 	  dpx/=m; */
/* 	  dpy/=m; */
/* 	  for (k=1;k < m;k++) */
/* 	    { */
/* 	      if (++j == 10000) */
/* 		{ */
/* 		  err_warn(1,fperr, */
/* 			   "Error - too many points for fill.\n"); */
/* 		  return 0; */
/* 		} */
/* 	      x0 = x0 + dpx; */
/* 	      y0 = y0 + dpy; */
/* 	      pnt[j].x = x0; */
/* 	      pnt[j].y = y0; */
/* 	      /\*pnt[j].x=pnt[j-1].x+dpx; */
/* 		pnt[j].y=pnt[j-1].y+dpy;*\/ */
/* 	    } */
/* 	} */
/*       if (j > 0) */
/* 	{ */
/* 	  polar(j+1,&pnt[0],&pnt[0]); */
/* 	  gfa(j+1,pnt); */
/* 	} */
/*       break; */
/*     default: */
/*       err_warn(1,fperr,"Error - projection not available.\n"); */
/*       return 1; */
/*     } */
/*   return 0; */
/* } */






/*     int proj_pl_dean (n,pxy) */
/*       int n; */
/*       Gpoint pxy[]; */
/*       { */
/*        int i,j,k,m; */
/*        Gpoint pnt[210]; */
/*        double dx,dy,ddx,ddy,dpx,dpy,x0,y0,r,dr,pol; */
/*        double Precis = 250.0; */

/*        struct project_attr *pj; */

/*        if (n <= 1) return 1; */

/*        pj=&p_PRJ; */

/*        dx=(fabs(pj->x2_NDC-pj->x1_NDC)/fabs(pj->X2-pj->X1))*Precis; */
/*        dy=(fabs(pj->y2_NDC-pj->y1_NDC)/fabs(pj->Y2-pj->Y1))*Precis; */


/*        switch (pj->projno) */
/* 	 { */
/* 	  case 0: */
/* /\*		It's a linear projection.				*\/ */
/* 		  for (i=0,j=0; i < n; i++) */
/* 		    { */
/* 		     pnt[j].x=pj->cX+pj->sX*pxy[i].x; */
/* 		     pnt[j].y=pj->cY+pj->sY*pxy[i].y; */

/* 		     if (++j == 200) */
/* 		       { */
/* 			gpl(200,pnt); */
/* 			pnt[0].x=pnt[j-1].x; */
/* 			pnt[0].y=pnt[j-1].y; */
/* 			j=1; */
/* 		       } */
/* 		    } */
/* 		  if (j > 1) gpl(j,pnt); */
/* 		  break; */

/* 	  case 1: */
/* /\*		It's a robinson projection.				*\/ */
/* 		  pnt[0].x=pxy[0].x; */
/* 		  pnt[0].y=pxy[0].y; */
/* 		  for (i=0,j=0; i < n-1; i++) */
/* 		    { */
/* 		     dpx=pxy[i+1].x-pxy[i].x; */
/* 		     dpy=pxy[i+1].y-pxy[i].y; */
/* 		     ddx=fabs(dpx)*dx; */
/* 		     ddy=fabs(dpy)*dy; */
/* /\*		     m=sqrt(ddy*ddy+ddx*ddx);		*\/ */
/* 		     m=ddy; */
/* 		     if (m > 1) */
/* 		       { */
/* 		        dpx/=m; */
/* 			dpy/=m; */
/* 			for (k=1;k < m;k++) */
/* 			  { */
/* 			   if (++j == 200) */
/* 			     { */
/* 			      x0=pnt[j-1].x; */
/* 			      y0=pnt[j-1].y; */
/* 			      robinson(200,&pnt[0],&pnt[0]); */
/* 			      gpl(200,pnt); */
/* 			      pnt[0].x=x0; */
/* 			      pnt[0].y=y0; */
/* 			      j=1; */
/* 			     } */
/* 			   pnt[j].x=pnt[j-1].x+dpx; */
/* 			   pnt[j].y=pnt[j-1].y+dpy; */
/* 			  } */
/* 		       } */
/* 		     if (++j == 200) */
/* 		       { */
/* 			x0=pnt[j-1].x; */
/* 			y0=pnt[j-1].y; */
/* 			robinson(200,&pnt[0],&pnt[0]); */
/* 			gpl(200,pnt); */
/* 			pnt[0].x=x0; */
/* 			pnt[0].y=y0; */
/* 			j=1; */
/* 		       } */
/* 		     pnt[j].x=pxy[i+1].x; */
/* 		     pnt[j].y=pxy[i+1].y; */
/* 		    } */
/* 		  if (j > 0) */
/* 		    { */
/* 		     robinson(j+1,&pnt[0],&pnt[0]); */
/* 		     gpl(j+1,pnt); */
/* 		    } */
/* 		  break; */
/* 	  case 2: */
/* /\*		It's a mollweide projection.				*\/ */
/* 		  pnt[0].x=pxy[0].x; */
/* 		  pnt[0].y=pxy[0].y; */
/* 		  for (i=0,j=0; i < n-1; i++) */
/* 		    { */
/* 		     dpx=pxy[i+1].x-pxy[i].x; */
/* 		     dpy=pxy[i+1].y-pxy[i].y; */
/* 		     ddx=fabs(dpx)*dx; */
/* 		     ddy=fabs(dpy)*dy; */
/* /\*		     m=sqrt(ddy*ddy+ddx*ddx);		*\/ */
/* 		     m=ddy; */
/* 		     if (m > 1) */
/* 		       { */
/* 		        dpx/=m; */
/* 			dpy/=m; */
/* 			for (k=1;k < m;k++) */
/* 			  { */
/* 			   if (++j == 200) */
/* 			     { */
/* 			      x0=pnt[j-1].x; */
/* 			      y0=pnt[j-1].y; */
/* 			      mollweide(200,&pnt[0],&pnt[0]); */
/* 			      gpl(200,pnt); */
/* 			      pnt[0].x=x0; */
/* 			      pnt[0].y=y0; */
/* 			      j=1; */
/* 			     } */
/* 			   pnt[j].x=pnt[j-1].x+dpx; */
/* 			   pnt[j].y=pnt[j-1].y+dpy; */
/* 			  } */
/* 		       } */
/* 		     if (++j == 200) */
/* 		       { */
/* 			x0=pnt[j-1].x; */
/* 			y0=pnt[j-1].y; */
/* 			mollweide(200,&pnt[0],&pnt[0]); */
/* 			gpl(200,pnt); */
/* 			pnt[0].x=x0; */
/* 			pnt[0].y=y0; */
/* 			j=1; */
/* 		       } */
/* 		     pnt[j].x=pxy[i+1].x; */
/* 		     pnt[j].y=pxy[i+1].y; */
/* 		    } */
/* 		  if (j > 0) */
/* 		    { */
/* 		     mollweide(j+1,&pnt[0],&pnt[0]); */
/* 		     gpl(j+1,pnt); */
/* 		    } */
/* 		  break; */
/* 	  case -3: */

/* /\*		It's a polar projection.				*\/ */
/* 		  pnt[0].x=pxy[0].x; */
/* 		  pnt[0].y=pxy[0].y; */
/* 		  pol = (pj->Y1 < pj->Y2) ? -90 : 90; */

/* /\*		   dy=(fabs(pj->y2_NDC-pj->y1_NDC)/fabs(pj->Y2-pj->Y1))*Precis; */
/* 		   r=(   fabs(pj->y2_NDC-pj->y1_NDC)/ */
/* 		         (2*fabs(pj->Y2-pj->Y1))	 )  *  Precis  *\/ */

/* 		  r=dy; */
/* 		  for (i=0,j=0; i < n-1; i++) */
/* 		    { */
/* 		     dr=r*fabs(0.5*(pxy[i].y+pxy[i+1].y)-pol)*DEG_TO_RAD; */
/* 		     dpx=pxy[i+1].x-pxy[i].x; */
/* 		     dpy=pxy[i+1].y-pxy[i].y; */
/* 		     ddx=dr*fabs(dpx); */
/* 		     m=ddx; */
/* 		     if (m > 1) */
/* 		       { */
/* 		        dpx/=m; */
/* 			dpy/=m; */
/* 			for (k=1;k < m;k++) */
/* 			  { */
/* 			   if (++j == 200) */
/* 			     { */
/* 			      x0=pnt[j-1].x; */
/* 			      y0=pnt[j-1].y; */
/* 			      polar(200,&pnt[0],&pnt[0]); */
/* 			      gpl(200,pnt); */
/* 			      pnt[0].x=x0; */
/* 			      pnt[0].y=y0; */
/* 			      j=1; */
/* 			     } */
/* 			   pnt[j].x=pnt[j-1].x+dpx; */
/* 			   pnt[j].y=pnt[j-1].y+dpy; */
/* 			  } */
/* 		       } */
/* 		     if (++j == 200) */
/* 		       { */
/* 			x0=pnt[j-1].x; */
/* 			y0=pnt[j-1].y; */
/* 			polar(200,&pnt[0],&pnt[0]); */
/* 			gpl(200,pnt); */
/* 			pnt[0].x=x0; */
/* 			pnt[0].y=y0; */
/* 			j=1; */
/* 		       } */
/* 		     pnt[j].x=pxy[i+1].x; */
/* 		     pnt[j].y=pxy[i+1].y; */
/* 		    } */
/* 		  if (j > 0) */
/* 		    { */
/* 		     polar(j+1,&pnt[0],&pnt[0]); */
/* 		     gpl(j+1,pnt); */
/* 		    } */
/* 		  break; */
/* 	  default: */
/* 		  err_warn(1,fperr,"Error - projection not available.\n"); */
/* 		  return 1; */
/* 	 } */
/*        return 0; */
/*       } */
