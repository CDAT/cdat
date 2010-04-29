#define NDS 4
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <errno.h>

    extern FILE *fpin,*fpout,*fperr;


struct interpolate
	{
	 char *N;		/* Pointer to data name.		*/
	 int ndim;		/* Number of dimensions.		*/
	 int ntrn[NDS];		/* Transform indicators.		*/

	 char *XN[NDS];		/* Pointers to the dimension names.	*/
	 int XS[NDS];		/* Dimension sizes.			*/
	 union
	   {
	    float *data;	/* Pointer to the input data array.	*/
	    int *idata;		/* Pointer to the input	data array.	*/
	   } in;
	 float *XB[NDS];	/* Pointers to dimension bounds for input.*/
	 float *XV[NDS];	/* Pointers to dimension values for input.*/
	 float XF[NDS];		/* First value of each dimension.	*/
	 float XL[NDS];		/* Last value of each dimension.	*/
	 int XK[NDS][2];	/* Dual values for K.			*/
	 float XC[NDS];		/* Cycle values.			*/

	 int xs[NDS];		/* Dimension sizes for output data.	*/
	 union
	   {
	    float *data;	/* Pointer to the output data array.	*/
	    int *idata;		/* Pointer to the output data array.	*/
	   } out;
	 float *xb[NDS];	/* Pointers to dimension bounds for output.*/
	 float *xv[NDS];	/* Pointers to dimension values for output.*/
	 short *pmask;		/* Pointer to the mask for input array.	*/
	 int *pi1[NDS];		/* Pointer to indices of first element.	*/
	 int *pi2[NDS];		/* Pointer to indices of last element.	*/
	 int *pki1[NDS];	/* Pointer to first wrap indices.	*/
	 int *pki2[NDS];	/* Pointer to last wrap indices.	*/
	 int I[NDS];		/* Active indices for all input dimensions.*/
	 int i[NDS];		/* Active indices for all output dimensions.*/
	};
/*			Interpolate from one array into the next.	*/

double fsir(struct interpolate ntrp, int d);


    int fintwt(ntrp)

      struct interpolate ntrp;
      
      {
	int i,j,k,m;
	int *p1,*p2,*pk1,*pk2;
	int pp,pkk;
	int dj,dk,dm;
	int jkm;

/*		Set dimension indices and wrap indices for each dimension.*/

/*for (j=0;j<ntrp.XS[1];j++)
  {
   for (i=0;i<ntrp.XS[0];i++)
     {
      fprintf(stdout,"%g,",ntrp.in.data[i+j*(ntrp.XS[0])]);
     }
   fprintf(stdout,"\n");
  }
*/
/*for (k=0;k<ntrp.ndim;k++)
for (i=0;i<ntrp.XS[k];i++)
   fprintf(stdout,"%g     %g     %g\n"
		,*(ntrp.XV[k]+i),
		*(ntrp.XB[k]+i),*(ntrp.XB[k]+i+1));
*/

	for (k=0;k < ntrp.ndim;k++)
	  {
	   if ((ntrp.pi1[k]=(int *)malloc(sizeof(int *)*(ntrp.xs[k]+1)))==NULL)
	     { fintwterr(ntrp); return 0;}
           if ((ntrp.pi2[k]=(int *)malloc(sizeof(int *)*(ntrp.xs[k]+1)))==NULL)
	     { fintwterr(ntrp); return 0;}
	   if((ntrp.pki1[k]=(int *)malloc(sizeof(int *)*(ntrp.xs[k]+1)))==NULL)
	     { fintwterr(ntrp); return 0;}
           if((ntrp.pki2[k]=(int *)malloc(sizeof(int *)*(ntrp.xs[k]+1)))==NULL)
	     { fintwterr(ntrp); return 0;}
	   p1=ntrp.pi1[k]; p2=ntrp.pi2[k]; pk1=ntrp.pki1[k]; pk2=ntrp.pki2[k];
	   if (ntrp.ntrn[k] <= 1)
	      for (i=0;i < ntrp.xs[k]; i++,p1++,p2++,pk1++,pk2++)
		{
		 findi(p1,p2,pk1,pk2,*(ntrp.xv[k]+i),
			ntrp.XV[k],ntrp.XS[k],ntrp.XC[k],ntrp.XK[k]);
		}
	   else if (ntrp.ntrn[k] == 2)
	      for (i=0;i < ntrp.xs[k]; i++,p1++,p2++,pk1++,pk2++)
		{
		 findi(p1,&pp,pk1,&pkk,*(ntrp.xb[k]+i),
			ntrp.XB[k],ntrp.XS[k]+1,ntrp.XC[k],ntrp.XK[k]);
		 findi(&pp,p2,&pkk,pk2,*(ntrp.xb[k]+i+1),
			ntrp.XB[k],ntrp.XS[k]+1,ntrp.XC[k],ntrp.XK[k]);
	 	}
/*p1=ntrp.pi1[k]; p2=ntrp.pi2[k]; pk1=ntrp.pki1[k]; pk2=ntrp.pki2[k];
fprintf(stdout,"   i     p1     p2    pk1    pk2     xv     xb     xb+\n");
for (i=0;i<ntrp.xs[k];i++)
   fprintf(stdout,"%6d %6d %6d %6d %6d     %g     %g     %g\n"
		,i,*(p1+i),*(p2+i),*(pk1+i),*(pk2+i),*(ntrp.xv[k]+i),
		*(ntrp.xb[k]+i),*(ntrp.xb[k]+i+1));
*/
	  }
	i=j=k=m=dj=dk=dm=0;
	if (ntrp.ndim > 1) dj=ntrp.xs[0];
	if (ntrp.ndim > 2) dk=dj*ntrp.xs[1];
	if (ntrp.ndim > 3) dm=dk*ntrp.xs[2];
	jkm=0;
	do
	  {
	   ntrp.i[0]=i;
	   ntrp.i[1]=j;
	   ntrp.i[2]=k;
	   ntrp.i[3]=m;
	   *(ntrp.out.data+jkm)=fsir(ntrp,ntrp.ndim);
	   i++; i=i%ntrp.xs[0]; /*DNW - i=++i%ntrp.xs[0];*/
	   if (i == 0 && ntrp.ndim > 1)
	     { j++; j=j%ntrp.xs[1]; /*DNW - j=++j%ntrp.xs[1];*/
	       if (j == 0 && ntrp.ndim > 2)
		 { k++; k=k%ntrp.xs[2]; /*DNW - k=++k%ntrp.xs[2];*/
		   if (k == 0 && ntrp.ndim > 3)
		     {
		      m++; m=m%ntrp.xs[3]; /*DNW - m=++m%ntrp.xs[3];*/
		     }
		 }
	     }
	   jkm=i+j*dj+k*dk+m*dm;
	  } while (jkm != 0);

/*for (j=0;j<ntrp.xs[1];j++)
  {
  for (i=0;i<ntrp.xs[0];i++)
    {
     fprintf(stdout,"%g,",ntrp.out.data[i+j*(ntrp.xs[0])]);
    }
  fprintf(stdout,"\n");
  }
*/

	for (i=0; i < ntrp.ndim;i++)
	  {
           free (ntrp.pi1[i]);
	   free (ntrp.pi2[i]);
	   free (ntrp.pki1[i]);
	   free (ntrp.pki2[i]);
	  }
       return 1;
      }   

/*	Find the indices (possibly wrapped) of the values of v[]+k*C
	surrounding x.  return I1,I2,k1,k2 and either true or false
	depending upon whether the value (x) was found in v[].		*/

    int findi (I1,I2,k1,k2,x,v,n,C,K)
      int *I1;		/* Returned first value for I.			*/
      int *I2;		/* Returned second value for I.			*/
      int *k1;		/* Returned first value for K wrap index.	*/
      int *k2;		/* Returned second value for K.			*/
      float x;		/* Value to find.				*/
      float v[];	/* Array of values to search.			*/
      int n;		/* Number of values in v.			*/
      float C;		/* Wrap cycle value.				*/
      int K[2];		/* Wrap indices.				*/
      {
	int i,kk,dk;
	float eps,epsi;

	dk=(K[0]<K[1])?1:-1;
	eps=x-(v[0]+K[0]*C);

	for (kk=K[0]; (kk-K[1])*(kk-K[0]) <= 0 ; kk+=dk)
	  {
	   if (n > 1)
	     {
	      for (i=1; i < n ; i++)
	        {
	         epsi=x-(v[i]+kk*C);
	         if (epsi*eps <= 0.0)
	           {
		    *I1=i-1;
		    *I2=i;
		    *k2=*k1=kk;
		    return 1;
		   }
	         eps=epsi;
	        }
	     }
	   if (kk != K[1])
	     {
	      epsi=x-(v[0]+(kk+dk)*C);
	      if (epsi*eps <= 0.0)
		{
		 *I1=n-1;
		 *k2=kk+dk;
		 *k1=kk;
		 *I2=0;
		 return 1;
		}
	      eps=epsi;
	     }
	  }
	if (fabs((double)(x-v[0]+K[0]*C)) < fabs((double)(x-v[n-1]+K[1]*C)))
	  {
	   *I1=0;
	   *k1=K[0];
	   if (n > 1) {*I2=1; *k2=K[0];}
	   else {*I2=0; *k2=( (K[0]!=K[1])?K[0]+dk:K[0]);}
	  }
	else
	  {
	   *I2=n-1;
	   *k2=K[1];
	   if (n > 1) {*I1=n-2; *k1=K[1];}
	   else {*I1=0; *k1=( (K[0]!=K[1])?K[1]-dk:K[1]);}
	  }
	return 0;
      }

   int fintwterr (ntrp)
     struct interpolate ntrp;
     {
      int i;

     err_warn(1,fperr,
		"Error - Memory for interpolating (%s) not found.\n",ntrp.N);
      for (i=0; i < ntrp.ndim;i++)
	{
         free (ntrp.pi1[i]);
	 free (ntrp.pi2[i]);
	 free (ntrp.pki1[i]);
	 free (ntrp.pki2[i]);
	}
      return 0;
     }


    double fsir (ntrp,d)
      struct interpolate ntrp;
      int d;
      {
	double XI1,XI2,xi;
	double BI1,BI2,BI2m1,BI1p1,BIp1,BI,bi,bip1,SI1,SI2,SI,Si,Wi,WI1,WI2;
	int ii,dk,dpk1,dpk2,kk,ip,im;
	int i;
	int k,point;
        int p1,p2,pk1,pk2;
	int ii2,kk2;

	d--;
	i=ntrp.i[d];

	p1=*(ntrp.pi1[d]+i);
	p2=*(ntrp.pi2[d]+i);
	pk1=*(ntrp.pki1[d]+i);
	pk2=*(ntrp.pki2[d]+i);

	Wi=0.0;
	if (ntrp.ntrn[d] <= 1)
	  {
/*			LINEAR INTERPOLATION

		(Capitals refer to original data and grid)
		(Lower-case refers to computed data and grid)

				  [x(i)-X(I1)]
	s(i)= S(I1)+[S(I2)-S(I1)] ---------------
				  [X(I2)-X(I1)]

	rearranged to give:


	       [X(I2)-x(i)]*S(I1) + [x(i)-X(I1)]*S(I2)]
	s(i) = ----------------------------------------
		     [X(I2)-x(i)] + [x(i)-X(I1)]

	where:  I1 and I2 are indices determined for each i, such that
		x(i) is between X(I1) and X(I2).

*/

	   XI1=*(ntrp.XV[d]+p1)+pk1*ntrp.XC[d];
	   XI2=*(ntrp.XV[d]+p2)+pk2*ntrp.XC[d];
	   xi=*(ntrp.xv[d]+i);
	   if (d == 0)
	     {
	      for (point=0,k=ntrp.ndim-1;k > 0;k--)
		point=(point+ntrp.I[k])*ntrp.XS[k-1];
	      if (*(ntrp.pmask+point+p1)) SI1=*(ntrp.in.data+point+p1);
	      else SI1=1.e20;
	      if (*(ntrp.pmask+point+p2)) SI2=*(ntrp.in.data+point+p2);
	      else SI2=1.e20;
	     }
	   else
	     {
	      ntrp.I[d]=p1;
	      SI1=fsir(ntrp,d);
	      ntrp.I[d]=p2;
	      SI2=fsir(ntrp,d);
	     }
	   WI1=XI2-xi;
	   WI2=xi-XI1;
	   if (fabs(SI1) >= 0.9999e20)
	     {
	      SI1=0.0;
	      WI1=WI2=0.0;
	     }
	   else
	      Wi+=WI1;

	   if (fabs(SI2) >= 0.9999e20)
	     {
	      SI2=0.0;
	      WI1=WI2=0.0;
	     }
	   else
	      Wi+=WI2;
	   Si=(WI1*SI1+WI2*SI2);

/*			Divide by the weights.				*/

	   if (Wi != 0.0) Si=Si/Wi;
	   else Si=1.e20;
	  }



	else if (ntrp.ntrn[d] == 2)
	  {
/*			AREA WEIGHTED INTERPOLATION

		(Capitals refer to original data and grid)
		(Lower-case refers to computed data and grid)

     I2-2
     SUM  { S(I)*[B(I+1)-B(I)] } + S(I1)*[B(I1+1)-b(i)] + S(I2)*[b(i+1)-B(I2-1)]
     I=I1+1
s(i)=--------------------------------------------------------------------------
     I2-2
     SUM  {B(I+1)-B(I)} + [B(I1+1)-b(i)] + [b(i+1)-B(I2-1)]
     I=I1+1


where:  I1 and I2 are indices determined for each i, such that the bounds of
	the grid surrounding the value to be computed, s(i), namely b(i)
	and b(i+1), are within the bounds, B(I1) and B(I2), of the original
	data, S(I).

For example:

      B(I1)                           B(I2)
orig.	|---*---|---*---|---*---|---*---|---*---|---*---|---*---|---*---|
	  S(I1)  S(I1+1) S(I2-2) S(I2-1)

	 b(i)                     b(i+1)
new.	--|------------*------------|------------*------------|
		      s(i)
*/
/*			Determine the direction of k.			*/

	   if (pk2 > pk1) dk=1;
	   else if (pk2 < pk1) dk=-1;
	   else dk=0;

	   dpk1=0;
	   dpk2=0;
/*			Assume there's always more than one bound value.*/
	   ip=p1+1;
	   im=p2-1;
/*			However, there may be only one K value.		*/
	   if (dk != 0)
	     {
	      if ((ip=ip%(ntrp.XS[d]+1)) == 0) dpk1=dk;
	      if ((im=(im+ntrp.XS[d]+1)%(ntrp.XS[d]+1)) == 0) dpk2=dk;
	     }

/*			Compute and sum the out-liers.			*/

	   BI2m1=*(ntrp.XB[d]+im)+(pk2-dpk2)*ntrp.XC[d];
	   BI2=*(ntrp.XB[d]+p2)+pk2*ntrp.XC[d];
	   BI1p1=*(ntrp.XB[d]+ip)+(pk1+dpk1)*ntrp.XC[d];
	   BI1=*(ntrp.XB[d]+p1)+pk1*ntrp.XC[d];
	   bi=*(ntrp.xb[d]+i);
	   bip1=*(ntrp.xb[d]+i+1);
/*			If b(i) is outside set it to the nearest limit.	*/
	   if ( (bi-BI2)*(bi-BI1) > 0.0)
	      bi=(fabs((double)(bi-BI2))>fabs((double)(bi-BI1)))?BI1:BI2;
/*			If b(i+1) is outside set it to the nearest limit.*/
	   if ( (bip1-BI2)*(bip1-BI1) > 0)
	      bip1=(fabs((double)(bip1-BI2))>fabs((double)(bip1-BI1)))?BI1:BI2;
	   WI1=BI1p1-bi;
	   WI2=bip1-BI2m1;
	   if (bi == bip1) WI1=WI2=0.0;
	   if (d == 0)
	     {
	      for (point=0.0,k=ntrp.ndim-1;k > 0;k--)
		point=(point+ntrp.I[k])*ntrp.XS[k-1];
	      if (*(ntrp.pmask+point+p1)) SI1=*(ntrp.in.data+point+p1);
	      else SI1=1.e20;
	      if (*(ntrp.pmask+point+p2-1)) SI2=*(ntrp.in.data+point+p2-1);
	      else SI2=1.e20;
	     }
	   else
	     {
	      ntrp.I[d]=p1;
	      SI1=fsir(ntrp,d);
	      ntrp.I[d]=p2-1;
	      SI2=fsir(ntrp,d);
	     }
	   if (fabs(SI1) >= 0.9999e20)
	      SI1=0.0;
	   else
	      Wi+=WI1;
	   if (fabs(SI2) >= 0.9999e20)
	      SI2=0.0;
	   else
	      Wi+=WI2;
	   Si=SI1*WI1+SI2*WI2;

/*			Set up and do the summation.			*/

	   kk=pk1;
	   kk2=pk2;
	   if (dk != 0)
	     {
	      ii=(p1+1)%(ntrp.XS[d]);
	      ii2=(p2-2+ntrp.XS[d])%(ntrp.XS[d]);
	      if ( ii == 0) kk+=dk;
	      if ( ii2 > p2-2) kk2-=dk;
	     }
	   else
	     {
	      ii=p1+1;
	      ii2=p2-2;
	     }

/*		if (kk > kk2 && kk > 10000)
		  fprintf (stdout," kk=%d kk2=%d \n",kk,kk2);
	   for (; kk != kk2 || ii <= ii2;)				*/

	   for (; (kk2-kk)*dk>=0 && (kk != kk2 || ii <= ii2);)
	     {
	      if ( (ii=(ii)%(ntrp.XS[d])) == 0 ) kk+=dk;
	      ip=ii+1;
	      BI=*(ntrp.XB[d]+ii)+kk*ntrp.XC[d];
	      BIp1=*(ntrp.XB[d]+ip)+kk*ntrp.XC[d];
	      WI1=BIp1-BI;
	      if (d == 0)
	        {
	         for (point=0,k=ntrp.ndim-1;k > 0;k--)
			point=(point+ntrp.I[k])*ntrp.XS[k-1];
		 SI=*(ntrp.in.data+point+ii);
		}
	      else
		{
		 ntrp.I[d]=ii;
		 SI=fsir(ntrp,d);
		}
	      if (fabs(SI) >= 0.9999e20) SI=0.0;
	      else Wi+=WI1;

	      Si=Si+SI*WI1;
	      ii=ip;
	     }

/*			Divide by weights.				*/

	   if (Wi != 0.0) Si=Si/Wi;
	   else Si=1.e20;
	  }
	return Si;
      }
