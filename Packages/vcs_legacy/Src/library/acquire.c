#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "cddrs.h"
#include "array.h"

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
	 short *pmask;		/* Pointer to the mask for input array.*/
	 int *pi1[NDS];		/* Pointer to indices of first element.	*/
	 int *pi2[NDS];		/* Pointer to indices of last element.	*/
	 int *pki1[NDS];	/* Pointer to first wrap indices.	*/
	 int *pki2[NDS];	/* Pointer to last wrap indices.	*/
	 int I[NDS];		/* Active indices for all input dimensions.*/
	 int i[NDS];		/* Active indices for all output dimensions.*/	
	};

/*			Prototype compile function.			*/

    struct data_flo *compile_vcs_legacy (struct a_tab *ptab);
    struct data_flo *logicomp (struct a_tab *ptab);
    int compu_log(struct data_flo **pd,struct a_attr *pa);


      
    int acquire_A(struct a_tab *ptab,char *ty)

/*    struct a_tab *ptab;	pointer to the array attribute table entry.
      char *ty;			the type of data needed for the display.*/
	
      {
	int i,i1,i2,j,k,c,C,T,lu,lud,ier,s,S,d[NDS],nd;
	int ntp,wrap;
	int xwrap[NDS];
	float *pf,*pr;
	float dxc;
	char *pc;
	int *pi;
	float xf,xl;
	int mxtrn;

	struct a_attr *pA;
	struct data_flo *pd;
	struct interpolate ntrp;

	if (ptab->FROM_CDAT) {/*If called from CDAT, then return. We already */
	   return 1;          /* have data.                                  */
	}

/*			If the description is in error or incomplete
			there is nothing that can be done.		*/

	pA=ptab->pA_attr;

	if (pA->notok)
	  {
	   err_warn(1,fperr,
			"Error - attempt to acquire data that is not ok.\n");
	   return 0;
	  }

/*		Find requested array sizes and number of dimensions
		with values > 1.					*/

	for (i=0,s=1,nd=0; i < NDS && pA->xs[i] != NULL; i++)
	  {
	   if ((d[i]=*pA->xs[i]) > 1) nd=i+1;
	   s=s*d[i];
	  }
	if (nd <= 0)
	  {
	   err_warn(1,fperr,
		"Error - scalar or no dimensions requested from (%s)\n",pA->N);
	   return 0;
	  }
	if (pA->ND <= 0)
	  {
	   err_warn(1,fperr,"Error - no dimensions available for (%s)\n",pA->N);
	   return 0;
	  }

/*		Find actual array sizes and number of dimensions.	
		Meld "wrap" into "actual" dimension size.
		Wrap interval may be less than total dimension.		*/

	for (i=0,S=1; i< pA->ND ;i++)
	  {
	   i1=findX(*pA->XF[i],pA->XV[i],*pA->XS[i]);
	   i2=findX(*pA->XL[i],pA->XV[i],*pA->XS[i]);
	   S=S*(abs(i2-i1)+1);
	  }
/*			Multiply by byte lengths.			*/

/*	pc=pA->TY;	*/
	T=toupper(pA->TY[0]);	/* Save the data type.	*/
/*	pc+=2;		*/
	C=pA->TY[2]-48; /* Save the data length.  Convert ASCII number.	*/
	S=S*C;
	if (T == 'C')
	  {
	   err_warn(1,fperr,"Error - character form for data (%d)\n",pA->N);
	   return 0;
	  }

/*	pc=ty;		*/
/*	t=toupper(*pc);	 Save the requested data type.	*/
/*	pc+=2;		*/
	c=ty[2]-48; /* Save the requested length. Convert ASCII number.	*/
	s=s*c;

/*			Is interpolation or wrap needed?		*/

	for (i=0,ntp=0,wrap=0; i < pA->ND; i++)
	  {
	   if (*pA->xi[i] > 0)  ntp++;
	   xwrap[i]=0;
	   i1=findX(*pA->XF[i],pA->XV[i],*pA->XS[i]);
	   i2=findX(*pA->XL[i],pA->XV[i],*pA->XS[i]);
	   dxc=fabs(pA->XB[i][i1] - pA->XB[i][i2+1]);
	   if ( (*pA->XK[i] != *(pA->XK[i]+1) ) && *pA->XC[i] != 0.0 &&
		fabs(dxc-*pA->XC[i]) < *pA->XC[i]*0.00001)
	     {
	      j=*pA->xs[i]-1;
	      if ((pA->xv[i][0]-*pA->XF[i])*(pA->xv[i][0]-*pA->XL[i]) > 0.0 ||
		  (pA->xv[i][j]-*pA->XF[i])*(pA->xv[i][j]-*pA->XL[i]) > 0.0)
		{
		 wrap++;
		 xwrap[i]=1;
		}
	     }
	  }

/*			If a file name is not given there must be
			an equation given.				*/

	if (pA->f != NULL)
	  {
	   if (pA->un.data!=NULL) {free((char *)pA->un.data); pA->un.data=NULL;}
	   if (pA->mask != NULL) {free((char *)pA->mask); pA->mask=NULL;}
	   if ( (pd=compile_vcs_legacy(ptab)) == NULL)
	     {
	      err_warn(1,fperr,"Error - function for (A_%s) won't compile.\n",
			ptab->name);
/*	      killA_tmp();			*/
	      return 0;
	     }
	   if ( (computer(pd,pA)) == 0)
	     {
	      err_warn(1,fperr,"Error - function for (A_%s) won't compute.\n",
			ptab->name);
	      pA->notok=1;
/*	      killA_tmp();				*/
	      return 0;
	     }
/*	   killA_tmp();				*/
	   clear_data_flo(&pd);

/*			Find the logical mask.				*/

	   if (pA->lmask != NULL)
	     {
	      if ( (pd=logicomp(ptab)) == NULL)
	        {
	         err_warn(1,fperr,
		   "Error - logical function for (A_%s) won't compile.\n",
			ptab->name);
	         return 0;
	        }
	      if ( (compu_log(&pd,pA)) == 0)
	        {
	         err_warn(1,fperr,
	 		"Error - logical function for (A_%s) won't compute.\n",
			ptab->name);
	      	pA->notok=1;
	     	return 0;
	       }
/*	      killA_tmp();				*/
	      clear_data_flo(&pd);
	     }
	   if (*ty == 'I')
		for (i=0,pf=pA->un.data,pi=pA->un.idata;
			i < s/sizeof(float);
		        pi++,pf++,i++)
	     {
	      *pi=*pf+0.5;
	     }

	   ier=mmmm(pA->un.data,pA->mask,&pA->xs[0],&pA->xw[0],nd,
			&pA->XC[0],&pA->xv[0],&pA->min,&pA->max,&pA->mean);
	  }
	else if (pA->F != NULL)
	  {
	   if (pA->un.data!=NULL) {free((char *)pA->un.data); pA->un.data=NULL;}
	   if (pA->mask != NULL) {free((char *)pA->mask); pA->mask=NULL;}
	   if ((pA->un.data=(float *)malloc(s)) == NULL)
	     {
	      err_warn(1,fperr,"Error - memory for %s data not found \n",pA->N);
	      return 0;
	     }
	   if ((pA->mask=(short *)malloc(sizeof(short)*(s/c))) == NULL)
	     {
	      err_warn(1,fperr,"Error - memory for %s mask not found \n",pA->N);
	      free ((char *)pA->un.data); pA->un.data=NULL;
	      return 0;
	     }

	   for (i=0;i<(s/c);i++) *(pA->mask+i)=1; /* Set all values valid.*/
	   for (i=0;i<(s/c);i++) *(pA->un.data+i)=1.e20; /* Set missing	*/

	   lu=10;
	   lud=11;
	   if ((k=Aslun(lu,pA->F,lud," ",IDRS_READ)) != IDRS_SUCCESS)
	     {
	      err_warn(1,fperr,"Error - file (%s) not available, cdunif # - %d\n",
		 pA->F,k);
	      free ((char *)pA->un.data); pA->un.data=NULL;
	      free ((char *)pA->mask); pA->mask=NULL;
	      return 0;
	     }
	   ier=Cluvdb();
	   ier=Setname(" ",pA->N," "," ",pA->TY);


/*			Scenario # 1 - No interpolation and no wrap.	*/

	   if (ntp == 0 && wrap == 0)
	     {
	      for (i=0;i < pA->ND ;i++)
	         Setdim(i+1,pA->XN[i]," ",*pA->xs[i],
			(double)pA->xv[i][0],(double)pA->xv[i][*pA->xs[i]-1]);
	      if ( (ier=Getdat(lu,pA->un.data,s)) != IDRS_SUCCESS)
		{
		 err_warn(1,fperr,
		    "Error - (cdunif # %d) data (%s) cannot be read.\n",ier,pA->N);
	         free ((char *)pA->un.data); pA->un.data=NULL;
	         free ((char *)pA->mask); pA->mask=NULL;
		 Cllun (lu);
		 return 0;
		}

/*			Find the logical mask.				*/

	      if (pA->lmask != NULL)
	        {
	         if ( (pd=logicomp(ptab)) == NULL)
	           {
	            err_warn(1,fperr,
		      "Error - logical function for (A_%s) won't compile.\n",
			ptab->name);
	            return 0;
	           }
	         if ( (compu_log(&pd,pA)) == 0)
	           {
	            err_warn(1,fperr,
	 		"Error - logical function for (A_%s) won't compute.\n",
			ptab->name);
	      	   pA->notok=1;
	     	   return 0;
	          }
/*	         killA_tmp();				*/
	         clear_data_flo(&pd);
	        }

	      if (*ty == 'I' && *pA->TY == 'R')
		for (i=0,pf=pA->un.data,pi=pA->un.idata;
			i < s/sizeof(float);
		        pi++,pf++,i++)
		  {
		   *pi=*pf+0.5;
		  }
	      else if (*ty == 'R' && *pA->TY == 'I')
		for (i=0,pf=pA->un.data,pi=pA->un.idata;
			i < s/sizeof(int);
		        pi++,pf++,i++)
		  {
		   *pf=*pi;
		  }
	      if (*ty == 'I')
		{
		 ier=immmm(pA->un.idata,pA->mask,&pA->xs[0],&pA->xw[0],nd,
			&pA->XC[0],&pA->xv[0],&pA->min,&pA->max,&pA->mean);
		}
	      if (*ty == 'R')
		{
		 ier=mmmm(pA->un.data,pA->mask,&pA->xs[0],&pA->xw[0],nd,
			&pA->XC[0],&pA->xv[0],&pA->min,&pA->max,&pA->mean);
		}
	     }

/*			Scenario # 2 - No interpolation but wrap one
			dimension.					*/

	   else if (ntp == 0)
	     {
/*				Scenario # 2 - just wrap.		*/

	      for (i=0,S=sizeof(float);i < pA->ND ;i++)
	        {
		 if (xwrap[i] == 0)
		   {
		    Setdim(i+1,pA->XN[i]," ",*pA->xs[i],
			(double)pA->xv[i][0],(double)pA->xv[i][*pA->xs[i]-1]);
		    S=S*(*pA->xs[i]);
		   }
		 else
		   {
		    Setdim(i+1,pA->XN[i]," ",*pA->XS[i],
				(double)*pA->XF[i],(double)*pA->XL[i]);
		    S=S*(*pA->XS[i]);
		   }
	        }
	      if ((pf=(float *)malloc(S)) == NULL)
	        {
	         err_warn(1,fperr,"Error - Memory for (%s) not found.\n",pA->N);
	         free ((char *)pA->un.data); pA->un.data=NULL;
	         free ((char *)pA->mask); pA->mask=NULL;
		 Cllun (lu);
	         return 0;
	        }
	      if ( (ier=Getdat(lu,pf,S)) != IDRS_SUCCESS)
		{
		 err_warn(1,fperr,
		    "Error - (cdunif # %d) data (%s) cannot be read.\n",ier,pA->N);
	         free ((char *)pA->un.data); pA->un.data=NULL;
	         free ((char *)pA->mask); pA->mask=NULL;
		 free ((char *)pf);
		 Cllun (lu);
		 return 0;
		}

	      fwrap(pA,pf,xwrap,pA->ND);

	      free ((char*)pf);

/*			Find the logical mask.				*/

	      if (pA->lmask != NULL)
	        {
	         if ( (pd=logicomp(ptab)) == NULL)
	           {
	            err_warn(1,fperr,
		      "Error - logical function for (A_%s) won't compile.\n",
			ptab->name);
	            return 0;
	           }
	         if ( (compu_log(&pd,pA)) == 0)
	           {
	            err_warn(1,fperr,
	 		"Error - logical function for (A_%s) won't compute.\n",
			ptab->name);
	      	   pA->notok=1;
	     	   return 0;
	          }
/*	         killA_tmp();				*/
	         clear_data_flo(&pd);
	        }

	      if (*ty == 'I' && *pA->TY == 'R')
		for (i=0,pf=pA->un.data,pi=pA->un.idata;
			i < s/sizeof(float);
		        pi++,pf++,i++)
		  {
		   *pi=*pf+0.5;
		  }
	      else if (*ty == 'R' && *pA->TY == 'I')
		for (i=0,pf=pA->un.data,pi=pA->un.idata;
			i < s/sizeof(int);
		        pi++,pf++,i++)
		  {
		   *pf=*pi;
		  }
	      if (*ty == 'I')
		{
		 ier=immmm(pA->un.idata,pA->mask,&pA->xs[0],&pA->xw[0],nd,
			&pA->XC[0],&pA->xv[0],&pA->min,&pA->max,&pA->mean);
		}
	      if (*ty == 'R')
		{
		 ier=mmmm(pA->un.data,pA->mask,&pA->xs[0],&pA->xw[0],nd,
			&pA->XC[0],&pA->xv[0],&pA->min,&pA->max,&pA->mean);
		}
	     }

/*				Scenario # 3 - interpolate real only.	*/

	   else if (*pA->TY == 'R')
	     {
	      ntrp.N=pA->N;
	      ntrp.pmask=pA->mask;

	      ntrp.out.data=pA->un.data;
/* ****************************************************
	      ntrp.ndim=pA->ND;
	      for (j=0;j<pA->ND;j++)
		{
		 if (ntrp.xs[j] <= 1 && ntrp.XS[j] <= 1)
		   {
		    ntrp.ndim=j;
		    break;
		   }
		}
************************************************** */
	      mxtrn=0;
	      S=sizeof(float);
	      for (i=0;i < pA->ND ;i++)
	        {
		 if (*pA->xi[i] > 0)
			ntrp.ntrn[i]=transform_type(pA->trnf,pA->XN[i]);
	         else
			ntrp.ntrn[i]=0;
		 mxtrn=((mxtrn < ntrp.ntrn[i])?ntrp.ntrn[i]:mxtrn);
	         ntrp.XN[i]=pA->XN[i];
	         ntrp.XK[i][0]=*pA->XK[i];
	         ntrp.XK[i][1]=*(pA->XK[i]+1);
	         ntrp.XC[i]=*pA->XC[i];

	         ntrp.xs[i]=*pA->xs[i];
	         ntrp.xb[i]=pA->xb[i];
	         ntrp.xv[i]=pA->xv[i];

		 if (xwrap[i] > 0)
		   {
		    i1=findX(*pA->XF[i],pA->XV[i],*pA->XS[i]);
		    i2=findX(*pA->XL[i],pA->XV[i],*pA->XS[i]);
		    ntrp.XS[i]=i2-i1+1;
		    ntrp.XB[i]=pA->XB[i]+i1;
		    ntrp.XV[i]=pA->XV[i]+i1;
		    xf=ntrp.XF[i]=*pA->XF[i];
		    xl=ntrp.XL[i]=*pA->XL[i];
		   }
		 else
		   {
		    if (ntrp.ntrn[i] == 2)
		      {
		       i1=findX(*pA->xb[i],pA->XV[i],*pA->XS[i]);
		       i2=findX(*(pA->xb[i]+*pA->xs[i]),pA->XV[i],
							*pA->XS[i]);
		      }
		    else
		      {
		       i1=findX(pA->xv[i][0],pA->XV[i],*pA->XS[i]);
		       i2=findX(pA->xv[i][*pA->xs[i]-1],pA->XV[i],*pA->XS[i]);

		       if (i1 > 0 && (pA->xv[i][0]-*(pA->XV[i]+i1))*
					(pA->xv[i][0]-*(pA->XV[i]+i1-1)) < 0.0)
				i1--;
			j=*pA->xs[i]-1;
		       if(i2+1<*pA->XS[i]&&(pA->xv[i][j]-*(pA->XV[i]+i2))*
					(pA->xv[i][j]-*(pA->XV[i]+i2-1)) < 0.0)
				i2++;
		      }
		    ntrp.XS[i]=i2-i1+1;
		    ntrp.XB[i]=pA->XB[i]+i1;
		    ntrp.XV[i]=pA->XV[i]+i1;
		    xf=ntrp.XF[i]=*(pA->XV[i]+i1);
		    xl=ntrp.XL[i]=*(pA->XV[i]+i2);
		   }

		 Setdim(i+1,pA->XN[i]," ",ntrp.XS[i],
				(double)xf,(double)xl);
	         S=S*ntrp.XS[i];
		 if (ntrp.ntrn[i] == 2 && cmpncs("latitude",pA->XN[i]) == 0)
		   {
		    pr=ntrp.XB[i];
		    if((ntrp.XB[i]=(float*)malloc(sizeof(float)*(ntrp.XS[i]+1)))
				 == NULL)
		      {
		       err_warn(1,fperr,
				"Error - Memory for (%s) not found.\n",pA->N);
		       free ((char *)pA->un.data); pA->un.data=NULL;
		       free ((char *)pA->mask); pA->mask=NULL;
		       Cllun (lu);
		       return 0;
		      }

/*			Compute sin(latitude) for area weighting. */

		    for (pf=ntrp.XB[i],j=0;j <= ntrp.XS[i];j++,pf++,pr++)
			*pf=sin(*pr*3.1415926536/180.);

		    pr=ntrp.xb[i];
	            if((ntrp.xb[i]=(float*)malloc(sizeof(float)*(ntrp.xs[i]+1)))
					 == NULL)
		      {
		       err_warn(1,fperr,
				"Error - Memory for (%s) not found.\n",pA->N);
		       free ((char *)pA->un.data); pA->un.data=NULL;
		       free ((char *)pA->mask); pA->mask=NULL;
		       free ((char *)ntrp.XB[i]);
		       Cllun (lu);
		       return 0;
		      }

/*			Compute sin(latitude) for area weighting. */

		    for (pf=ntrp.xb[i],j=0;j <= ntrp.xs[i];j++,pf++,pr++)
			*pf=sin(*pr*3.1415926536/180.);

		    ntrp.XC[i]=sin(ntrp.XC[i]*3.1415926536/180.);
		   }

	        }	/* end for (i=	*/

	      if ((pf=ntrp.in.data=(float *)malloc(S)) == NULL)
	        {
	         err_warn(1,fperr,"Error - Memory for (%s) not found.\n",pA->N);
	         free ((char *)pA->un.data); pA->un.data=NULL;
	         free ((char *)pA->mask); pA->mask=NULL;
		 for (j=0; j<pA->ND;j++)
		   {
		    if (ntrp.ntrn[j] == 2 && cmpncs("latitude",pA->XN[j]) == 0)
		      {
		       free ((char *)ntrp.XB[j]);
		       free ((char *)ntrp.xb[j]);
		      }
		   }
		 return 0;
	        }
	      if ((ntrp.pmask=
		    (short *)malloc( (S*sizeof(short))/sizeof(float) )) == NULL)
	        {
	         err_warn(1,fperr,"Error - Memory for (%s) not found.\n",pA->N);
	         free ((char *)pA->un.data); pA->un.data=NULL;
	         free ((char *)pA->mask); pA->mask=NULL;
		 for (j=0; j<pA->ND;j++)
		   {
		    if (ntrp.ntrn[j] == 2 && cmpncs("latitude",pA->XN[j]) == 0)
		      {
		       free ((char *)ntrp.XB[j]);
		       free ((char *)ntrp.xb[j]);
		      }
		   }
		 free ((char *)ntrp.in.data); ntrp.in.data=NULL;
		 return 0;
	        }
	      for (i=0;i<(S/sizeof(float));i++) *(ntrp.pmask+i)=1;
	      for (i=0;i<(S/sizeof(float));i++) *(ntrp.in.data+i)=1.e20;

	      if ( (ier=Getdat(lu,pf,S)) != IDRS_SUCCESS)
	        {
	         err_warn(1,fperr,
		    "Error - (cdunif # %d) data (%s) cannot be read.\n",ier,pA->N);
	         free ((char *)pA->un.data); pA->un.data=NULL;
	         free ((char *)pA->mask); pA->mask=NULL;
		 for (j=0; j<pA->ND;j++)
		   {
		    if (ntrp.ntrn[j] == 2 && cmpncs("latitude",pA->XN[j]) == 0)
		      {
		       free ((char *)ntrp.XB[j]);
		       free ((char *)ntrp.xb[j]);
		      }
		   }
		 free ((char *)ntrp.in.data); ntrp.in.data=NULL;
		 free ((char *)ntrp.pmask); ntrp.pmask=NULL;
		 Cllun (lu);
	         return 0;
	        }

/*			Mask out missing or invalid data.		*/
	      for (i=0;i<(S/sizeof(float));i++)
			if (fabs(ntrp.in.data[i]) > 0.999e20) ntrp.pmask[i]=0;
/*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&*/
	      ntrp.ndim=pA->ND;
	      for (j=0;j<pA->ND;j++)
		{
		 if (ntrp.xs[j] <= 1 && ntrp.XS[j] <= 1)
		   {
		    ntrp.ndim=j;
		    break;
		   }
		}
/*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&*/

	      ier=fintwt(ntrp);

/*			Find the logical mask.				*/

	      if (pA->lmask != NULL)
	        {
	         if ( (pd=logicomp(ptab)) == NULL)
	           {
	            err_warn(1,fperr,
		      "Error - logical function for (A_%s) won't compile.\n",
			ptab->name);
	            return 0;
	           }
	         if ( (compu_log(&pd,pA)) == 0)
	           {
	            err_warn(1,fperr,
	 		"Error - logical function for (A_%s) won't compute.\n",
			ptab->name);
	      	   pA->notok=1;
	           free ((char *)ntrp.in.data); ntrp.in.data=NULL;
	           free ((char *)ntrp.pmask); ntrp.pmask=NULL;
	           for (j=0; j < pA->ND;j++)
		     {
		      if (ntrp.ntrn[j]==2 && cmpncs("latitude",pA->XN[j]) == 0)
		        {
		         free ((char *)ntrp.XB[j]);
		         free ((char *)ntrp.xb[j]);
		        }
		     }
	     	   return 0;
	          }
/*	         killA_tmp();				*/
	         clear_data_flo(&pd);
	        }

	      ier=mmmm(pA->un.data,pA->mask,&pA->xs[0],&pA->xw[0],nd,
			&pA->XC[0],&pA->xv[0],&pA->min,&pA->max,&pA->mean);

	      free ((char *)ntrp.in.data); ntrp.in.data=NULL;
	      free ((char *)ntrp.pmask); ntrp.pmask=NULL;
	      for (j=0; j < pA->ND;j++)
		{
		 if (ntrp.ntrn[j] == 2 && cmpncs("latitude",pA->XN[j]) == 0)
		   {
		    free ((char *)ntrp.XB[j]);
		    free ((char *)ntrp.xb[j]);
		   }
		}
	     }  /* end - else if real					*/
	   else
	     {
	      pA->notok=1;
	      err_warn(1,fperr,
			"Error - interpolation for integers not available.\n");
	      Cllun (lu);
	      return 0;
	     }
	   Cllun(lu);
	  }
	else
	  {
	   pA->notok=1;
	   err_warn(1,fperr,
		"Error - data for (A_%s) not defined.\n",pA->N);
	   return -1;
	  }
        return 1;
      }


/*		Find the max, min, weighted mean, and set the mask
		true if the value is valid (i.e. < 0.999e20).
		Assume max number of dimensions is 4.
		Assume integer values of the data.			*/

    int immmm(pdat,pmask,pdim,pw,nd,XC,xv,pmin,pmax,pmean)
      int *pdat;	/*  Data array (integers).			*/
      short *pmask;	/*  Data mask.					*/
      int *pdim[];	/*  Dimensions.					*/
      float *pw[];	/*  Weights to use for mean.			*/
      int nd;		/*  Number of dimensions.			*/
      float *XC[];	/*  Cycle length.				*/
      float *xv[];	/*  Dimension values.				*/
      float *pmin;	/*  Minimum data value.				*/
      float *pmax;	/*  Maximum data value.				*/
      float *pmean;	/*  Mean data value.				*/
      {
	int i,j,k,m,c;
	int di,dj,dk,dm;
	float *pwi,*pwj,*pwk,*pwm,one,data;
	float pw3,pw2,pw1,pw0;
	double weight,mean,w;

	if (nd < 1) return 0;

	weight=0.0;
	mean=0.0;
	one=1.0;
	*pmean=0.0;
	*pmin=1.e20;
	*pmax=-1.e20;
	di=dj=dk=dm=1;
	if (nd > 0) di=*pdim[0];
	if (nd > 1) dj=*pdim[1];
	if (nd > 2) dk=*pdim[2];
	if (nd > 3) dm=*pdim[3];

	for (m=0,pwm=(nd>3)?pw[3]:&one;m < dm;m++,pwm++)
	  {
	   pw3=*pwm;
	   if (nd > 3 && *XC[3] > 0.0)
	   pw3=(fabs((double)(*(xv[3]+m)-*(xv[3]))) >=
		(1.0-.01/dm)*fabs((double)*XC[3])) ? 0.0:*pwm;
	   for (k=0,pwk=(nd>2)?pw[2]:&one;k < dk;k++,pwk++)
	     {
	      pw2=*pwk;
	      if (nd > 2 && *XC[2] > 0.0)
	      pw2=(fabs((double)(*(xv[2]+k)-*(xv[2]))) >=
		   (1.0-.01/dk)*fabs((double)*XC[2])) ? 0.0:*pwk;
	      for (j=0,pwj=(nd>1)?pw[1]:&one;j < dj;j++,pwj++)
		{
	         pw1=*pwj;
	         if (nd > 1 && *XC[1] > 0.0)
	         pw1=(fabs((double)(*(xv[1]+j)-*(xv[1]))) >=
		      (1.0-.01/dj)*fabs((double)*XC[1])) ? 0.0:*pwj;
		 for (i=0,pwi=(nd>0)?pw[0]:&one;i < di;i++,pwi++,pdat++,pmask++)
		   {
		    pw0=*pwi;
		    if (*XC[0] > 0.0)
		    pw0=(fabs((double)(*(xv[0]+i)-*(xv[0]))) >=
			(1.0-.01/di)*fabs((double)*XC[0])) ? 0.0:*pwi;
		    data=*pdat;
		    if (*pdat != 9999 && *pmask)
		      {
		       w=pw0*pw1*pw2*pw3;
		       mean=mean+w*data;
		       weight=weight+w;
		       *pmin=(data < *pmin)? data : *pmin;
		       *pmax=(data > *pmax)? data : *pmax;
		      }
		    else
		      {
		       *pmask=0;
		      }
		   }
		}
	     }
	  }
	if (weight != 0.0)
	  {
	   *pmean=mean/weight;
	   c=1;
	  }
	else
	  {
	   c=0;
	  }
	return c;
      }


/*		Find the max, min, weighted mean, and set the mask
		true if the value is valid (i.e. < 0.999e20).
		Assume max number of dimensions is 4.
		Assume float values of the data.			*/

    int mmmm(float *pdat,short *pmask,int *pdim[],float *pw[],int nd,float *XC[],float *xv[],float *pmin,float *pmax,float *pmean)
/*       float *pdat;	/\*  Data array (float).				*\/ */
/*       short *pmask;	/\*  Data mask.					*\/ */
/*       int *pdim[];	/\*  Dimensions.					*\/ */
/*       float *pw[];	/\*  Weights to use for mean.			*\/ */
/*       int nd;		/\*  Number of dimensions.			*\/ */
/*       float *XC[];	/\*  Cycle length.				*\/ */
/*       float *xv[];	/\*  Dimension values.				*\/ */
/*       float *pmin;	/\*  Minimum data value.				*\/ */
/*       float *pmax;	/\*  Maximum data value.				*\/ */
/*       float *pmean;	/\*  Mean data value.				*\/ */
      {
	int i,j,k,m,c;
	int di,dj,dk,dm;
	float *pwi,*pwj,*pwk,*pwm,one;
	float pw3,pw2,pw1,pw0;
	double w,weight,mean;

	if (nd < 1) return 0;

	weight=0.0;
	one=1.0;
	mean=0.0;
	*pmean=0.0;
	*pmin=1.e20;
	*pmax=-1.e20;
	di=dj=dk=dm=1;
	if (nd > 0) di=*pdim[0];
	if (nd > 1) dj=*pdim[1];
	if (nd > 2) dk=*pdim[2];
	if (nd > 3) dm=*pdim[3];

	for (m=0,pwm=(nd>3)?pw[3]:&one;m < dm;m++,pwm++)
	  {
	   pw3=*pwm;
	   if (nd > 3 && *XC[3] > 0.0)
	   pw3=(fabs((double)(*(xv[3]+m)-*(xv[3]))) >=
		(1.0-.01/dm)*fabs((double)*XC[3])) ? 0.0:*pwm;
	   for (k=0,pwk=(nd>2)?pw[2]:&one;k < dk;k++,pwk++)
	     {
	      pw2=*pwk;
	      if (nd > 2 && *XC[2] > 0.0)
	      pw2=(fabs((double)(*(xv[2]+k)-*(xv[2]))) >=
		   (1.0-.01/dk)*fabs((double)*XC[2])) ? 0.0:*pwk;
	      for (j=0,pwj=(nd>1)?pw[1]:&one;j < dj;j++,pwj++)
		{
	         pw1=*pwj;
	         if (nd > 1 && *XC[1] > 0.0)
	         pw1=(fabs((double)(*(xv[1]+j)-*(xv[1]))) >=
		      (1.0-.01/dj)*fabs((double)*XC[1])) ? 0.0:*pwj;
		 for (i=0,pwi=(nd>0)?pw[0]:&one;i < di;i++,pwi++,pdat++,pmask++)
		   {
		    pw0=*pwi;
		    if (*XC[0] > 0.0)
		    pw0=(fabs((double)(*(xv[0]+i)-*(xv[0]))) >=
			(1.0-.01/di)*fabs((double)*XC[0])) ? 0.0:*pwi;
		    if (*pdat < 0.999e20 && *pdat > -0.999e20 && *pmask)
		      {
		       w=pw0*pw1*pw2*pw3;
		       mean=mean+w*(*pdat);
		       weight=weight+w;
		       *pmin=(*pdat < *pmin)? *pdat : *pmin;
		       *pmax=(*pdat > *pmax)? *pdat : *pmax;
		      }
		    else
		      {
		       *pmask=0;
		      }
		   }
		}
	     }
	  }
	if (weight != 0.0)
	  {
	   *pmean=mean/weight;
	   c=1;
	  }
	else
	  {
	   c=0;
	  }
	return c;
      }


/*		Find the max, min, weighted mean, and set the mask
		true if the value is valid (i.e. < 0.999e20).
		Assume max number of dimensions is 4.
		Assume float values of the data.			*/

    int fwrap(pA,pDAT,pwrap,nd)
      struct a_attr *pA;/*  Array attribute pointer.			*/
      float *pDAT;	/*  Data array to be wrapped into pdat.		*/
      int pwrap[];	/*  Wrap indicators.				*/
      int nd;		/*  Number of dimensions.			*/
      {
	int i,j,k,m,n,ijkm;
	int i1,i2,j1,j2,k1,k2,m1,m2;
	int ki1,ki2,kj1,kj2,kk1,kk2,km1,km2;
	int Ni,Nj,Nk,Nm;
	int km,kk,kj,ki;

	if (nd < 1) return 0;

/*			Get the index limits for the input array.	*/
	i1=i2=j1=j2=k1=k2=m1=m2=0;
	ki1=ki2=kj1=kj2=kk1=kk2=km1=km2=0;
	Ni=Nj=Nk=Nm=1;
	if (pA->ND > 0)
	  {
	   if (pwrap[0])
	     {
/*	      i1=findx(&ki1,&Ni,*pA->XF[0],*pA->XL[0],*pA->xf[0],pA->XV[0],
			*pA->XS[0],*pA->XC[0],pA->XK[0]);
	      i2=findx(&ki2,&Ni,*pA->XF[0],*pA->XL[0],*pA->xl[0],pA->XV[0],
			*pA->XS[0],*pA->XC[0],pA->XK[0]);
*/
	      i1=findx(&ki1,&Ni,*pA->XF[0],*pA->XL[0],pA->xv[0][0],pA->XV[0],
			*pA->XS[0],*pA->XC[0],pA->XK[0]);
	      i2=findx(&ki2,&Ni,*pA->XF[0],*pA->XL[0],pA->xv[0][*pA->xs[0]-1],
			pA->XV[0],*pA->XS[0],*pA->XC[0],pA->XK[0]);
	     }
	   else
	     {
	      i1=0;
	      i2=*pA->xs[0]-1;
	      ki1=ki2=0;
	      Ni=*pA->xs[0];
	     }
	  }
	if (pA->ND > 1)
	  {
	   if (pwrap[1])
	     {
/*	      j1=findx(&kj1,&Nj,*pA->XF[1],*pA->XL[1],*pA->xf[1],pA->XV[1],
			*pA->XS[1],*pA->XC[1],pA->XK[1]);
	      j2=findx(&kj2,&Nj,*pA->XF[1],*pA->XL[1],*pA->xl[1],pA->XV[1],
			*pA->XS[1],*pA->XC[1],pA->XK[1]);
*/
	      j1=findx(&kj1,&Nj,*pA->XF[1],*pA->XL[1],pA->xv[1][0],pA->XV[1],
			*pA->XS[1],*pA->XC[1],pA->XK[1]);
	      j2=findx(&kj2,&Nj,*pA->XF[1],*pA->XL[1],pA->xv[1][*pA->xs[1]-1],
			pA->XV[1],*pA->XS[1],*pA->XC[1],pA->XK[1]);
	     }
	   else
	     {
	      j1=0;
	      j2=*pA->xs[1]-1;
	      kj1=kj2=0;
	      Nj=*pA->xs[1];
	     }
	  }
	if (pA->ND > 2)
	  {
	   if (pwrap[2])
	     {
	      k1=findx(&kk1,&Nk,*pA->XF[2],*pA->XL[2],pA->xv[2][0],pA->XV[2],
			*pA->XS[2],*pA->XC[2],pA->XK[2]);
	      k2=findx(&kk2,&Nk,*pA->XF[2],*pA->XL[2],pA->xv[2][*pA->xs[2]-1],
			pA->XV[2],*pA->XS[2],*pA->XC[2],pA->XK[2]);
	     }
	   else
	     {
	      k1=0;
	      k2=*pA->xs[2]-1;
	      kk1=kk2=0;
	      Nk=*pA->xs[2];
	     }
	  }
	if (pA->ND > 3)
	  {
	   if (pwrap[3])
	     {
/*	      m1=findx(&km1,&Nm,*pA->XF[3],*pA->XL[3],*pA->xf[3],pA->XV[3],
			*pA->XS[3],*pA->XC[3],pA->XK[3]);
	      m2=findx(&km2,&Nm,*pA->XF[3],*pA->XL[3],*pA->xl[3],pA->XV[3],
			*pA->XS[3],*pA->XC[3],pA->XK[3]);
*/
	      m1=findx(&km1,&Nm,*pA->XF[3],*pA->XL[3],pA->xv[3][0],pA->XV[3],
			*pA->XS[3],*pA->XC[3],pA->XK[3]);
	      m2=findx(&km2,&Nm,*pA->XF[3],*pA->XL[3],pA->xv[3][*pA->xs[3]-1],
			pA->XV[3],*pA->XS[3],*pA->XC[3],pA->XK[3]);
	     }
	   else
	     {
	      m1=0;
	      m2=*pA->xs[3]-1;
	      km1=km2=0;
	      Nm=*pA->xs[3];
	     }
	  }

	n=0;
	for (km=km1; (km-km1)*(km-km2) <= 0 ;km=km+((km1<=km2)?1:-1))
	  {
	   for (m=((km==km1)?m1:0); m < ((km==km2)?m2+1:Nm);m++)
	     {
	      for (kk=kk1; (kk-kk1)*(kk-kk2) <= 0 ;kk=kk+((kk1<=kk2)?1:-1))
		{
		 for (k=((kk==kk1)?k1:0); k < ((kk==kk2)?k2+1:Nk);k++)
		   {
		    for (kj=kj1;(kj-kj1)*(kj-kj2) <= 0 ;kj=kj+((kj1<=kj2)?1:-1))
		      {
		       for (j=((kj==kj1)?j1:0); j < ((kj==kj2)?j2+1:Nj);j++)
			 {
			  for(ki=ki1;(ki-ki1)*(ki-ki2)<=0;
						ki=ki+((ki1<=ki2)?1:-1))
			    {
			     ijkm=m*Ni*Nj*Nk+k*Ni*Nj+j*Ni;
			     for(i=((ki==ki1)?i1:0);i < ((ki==ki2)?i2+1:Ni);i++)
			       {
				*((pA->un.data)+n++)=*(pDAT+ijkm+i);
			       }
			    }
			 }
		      }
		   }
		}  
	     }
	  }
	return 1;
      }

/*		Find the index of the value of v[i] nearest to x.	*/

    int findX (x,v,n)
      float x;		/* Value to search for.				*/
      float v[];	/* Values to search.				*/
      int n;		/* Number of values of v.			*/
      {
	int i;
	float eps,epsi;

	i=0;
	if (n > 1)
	  {
	   eps=x-v[0];
	   epsi=x-v[1];
	   if (fabs(eps) > fabs(epsi))
	     {
	      for (i=1; i < n ; i++)
	        {
	         epsi=x-v[i];
	         if (epsi*eps <= 0.0)
		   {
		    if (fabs(eps) <= fabs(epsi)) i--;
		    return i;
		   }
	         eps=epsi;
	        }
	      i=n-1;
	     }
	  }

	return i;
      }

/*	Find the index (possibly wrapped) of the value v[i] nearest x.	*/

    int findx (k,N,df,dl,x,v,n,C,K)
      int *k;		/* Returned value for K.			*/
      int *N;		/* Returned number of values in the dimension.	*/
      float df,dl;	/* Values defining the wrap range.		*/
      float x;		/* Value to find.				*/
      float v[];	/* Array of values to search.			*/
      int n;		/* Number of values in v.			*/
      float C;		/* Wrap cycle value.				*/
      int K[2];		/* Wrap indices.				*/
      {
	int i1,i2;
	int i,bi,dk,bk;
	float eps,epsi;

	i1=findX(df,v,n);
	i2=findX(dl,v,n);
	*N=i2-i1+1;

/* 	If there isn't any wrap then use findX to find the index.	*/

	if (K[0] == 0 && K[1] == 0)
	  {
	   i=findX(x,&v[i1],*N);
	   *k=0;
	   return i;
	  }

	dk=(K[0]<K[1])?1:-1;
	eps=x-(v[0+i1]+K[0]*C);
	if (n > 1)
	  {
	   epsi=x-(v[1+i1]+K[0]*C);
	   bi=1;
	   bk=0;
	  }
	else
	  {
	   epsi=x-(v[0+i1]+(K[1]+dk)*C);
	   bi=0;
	   bk=1;
	  }

	*k=K[0];
	if (fabs(eps) < fabs(epsi)) return 0;

	for (*k=K[bk]; (*k-K[1])*(*k-K[0]) <= 0 ; *k+=dk)
	  {
	   for (i=bi; i < *N ; i++)
	     {
	      epsi=x-(v[i+i1]+*k*C);
	      if (epsi*eps <= 0.0)
	        {
		 if (fabs(eps) <= fabs(epsi))
		   {
		    if (i > 0)
		      {
		       i--;
		      }
		    else
		      {
		       i=n-1;
		       *k-=dk;
		      }
		   }
		 return i;
		}
	      eps=epsi;
	     }
	   bi=0;
	  }
	*k-=dk;
	return i-1;
      }

/*			Return a transform type number according to
			the transform specifications.			*/

    int transform_type (str,xn)
      char str[];	/* String of transformation specifications.	*/
      char xn[];	/* Dimension name.				*/
      {
	int i,j,k,ir;
	char name[20];
	char dim[20];

	if (xn == NULL) return -1;
	ir=1;
	if (cmpncs(xn,"latitude") == 0 || cmpncs(xn,"longitude") == 0) ir=2;
	if (str == NULL) return ir;
	if (doexist(xn,str))
	  {
	   k=i=0;
	   while (xn[i] != '\0')
		if (isalpha(xn[i++])) dim[k++]=xn[i-1];
	   dim[k]='\0';
	   for (i=0;str[i] != '\0';i++)
	     {
	      if (isalpha(str[i]))
		{
		 j=0;
		 name[j]=str[i];
		 while (isalpha(str[++i]))
			 if (j < 19) name[++j]=str[i];
		 name[++j]='\0';
	         if (str[i] == '(')
		   while (str[++i] != ')' && str[i] != '\0')
		     {
		      j=0;
		      while (str[i] == xn[j])
			{
			 i++;
			 j++;
			}
		      if (j == k)
			{
			 if (cmpncs("awt",name) == 0) ir=2;
			 else if (cmpncs("linear",name) == 0) ir=1;
			}
		     }
		}
	     }
	  }
	return ir;
      }
