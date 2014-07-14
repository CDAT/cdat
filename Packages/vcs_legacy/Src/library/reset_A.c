#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "cddrs.h"


    extern struct a_tab A_tab;

    extern int update_ind;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

    char *repstr(char *s2,char *s1);

/*		Designed for interactive portion of this software to use
		for changing an array attribute set.			*/


    int reset_A (
	struct a_tab *ptab,	/*  Name of the array attributes.	*/
	float XF[],		/*  First value for each dimension.	*/ 
	float XL[],		/*  Last value for each dimension.  	*/
	float XC[],		/*  Cycle for each dimension.		*/
/*			Wrap indices cause wrap of XF to XL only.	*/
	int XKtop[],		/*  Wrap top index for each dimension.	*/
	int XKbot[],		/*  Wrap bottom index for each dimension.*/
	int XO[],		/*  Order of each dimension.		*/
	int xj[],		/*  Stride value.
				    0 - random.
				    1 - no stride.
				    >1 - stride length.			*/
	int xsize[],	/* Number of selected values for each dimension.*/
	float xf[],	/*First selected range value for each dimension.*/ 
	float xl[],	/*Last selected range value for each dimension. */
	float *xv[] )	/* Pointers to arrays of selected values for
							 each dimen.	*/
      {
	int i,j,k,m,n,c,d,rev;
	int change=0;
	char *pc,*a_name;
	char str[256];
/*	struct a_tab *ptab,*ptb;	*/
	struct a_attr *pA;
	char *pal,*paf;
	
	a_name=ptab->name;

	pA=ptab->pA_attr;

	if (swap_dim(XO,pA) == 0)
	  {
	   err_warn(1,fperr,"Error - dimension re-order is wrong.\n");
	   return 0;
	  }
/*				Can't continue if dimension is null.	*/

	if (pA->ND <= 0)
	  {
	   err_warn(1,fperr,"Error - no dimensions for A_%s.\n",ptab->name);
	   return 0;
	  }

	for (k=0;k<pA->ND;k++)
	   if (xsize[k] < 1)
	     {
	      err_warn(1,fperr,
		"Error - dimension (%s) is set zero so no changes were made.\n",
				pA->XN[k]);
	      return 0;
	     }

	for (i=0;i<pA->ND;i++)
	  {
	   k=XO[i];
/*			If it's assigned but not randomly selected
			it can't be changed so bypass it.		*/

	   if (pA->axv[i] != NULL && *pA->xj[i] != 0) continue;

	   change=change || k!=i;

/*				Change wrap indices?			*/
	   if(pA->XK[i]==NULL)
		{
		 if ((pA->XK[i]=(int *)malloc(sizeof(int)*2))==NULL)
		   {
		    err_warn(1,fperr,
			"Error - memory for A_%s changes not found. \n",
					a_name);
		    return 0;
		   }
	         *pA->XK[i]=*(pA->XK[i]+1)=0;
		}
	   
	   if(XKtop[k]!=*pA->XK[i]||XKbot[k]!=*(pA->XK[i]+1))
	     {
	      sprintf (str,"%d,%d",XKtop[k],XKbot[k]);
	      pA->aXK[i]=repstr(pA->aXK[i],str);
	      change=1;
	     }
/*				Change cycle?				*/
	   if(pA->XC[i]==NULL)
		{
		 if ((pA->XC[i]=(float *)malloc(sizeof(float)))==NULL)
		   {
		    err_warn(1,fperr,
			"Error - memory for A_%s changes not found. \n",
					a_name);
		    return 0;
		   }
	         *pA->XC[i]=0.0;
		}

	   if (XC[k] != *pA->XC[i])
	     {
	      sprintf (str,"%-18.9g",XC[k]);
	      trimbl(str,18);
	      pA->aXC[i]=repstr(pA->aXC[i],str);
	      change=1;
	     }

	   if (*pA->XF[i] != XF[k] || *pA->XL[i] != XL[k])
	     {
	      sprintf (str,"%-18.9g",XF[k]);
	      trimbl(str,18);
	      pA->aXF[i]=repstr(pA->aXF[i],str);
	      sprintf (str,"%-18.9g",XL[k]);
	      trimbl(str,18);
	      pA->aXL[i]=repstr(pA->aXL[i],str);
	      change=1;
	     }

/*			If a random selection is made or changed.	*/

	   if(pA->xj[i]==NULL)
		{
		 if ((pA->xj[i]=(int *)malloc(sizeof(int)))==NULL)
		   {
		    err_warn(1,fperr,
			"Error - memory for A_%s changes not found. \n",
					a_name);
		    return 0;
		   }
	         *pA->xj[i]=1;
		}
	   if (xj[k] == 0 && (xsize[k] != *pA->xs[i] ||
				 compare_x(xsize[k],xv[k],pA->xv[i]) == 0))
	     {
	      change=1;
	      if (pA->axv[i] != NULL) free(pA->axv[i]);
	      if ((pA->axv[i]=(char *)malloc(20*xsize[k])) == NULL)
	        {
		 err_warn(1,fperr,
			"Error - memory for A_%s changes not found. \n",
					a_name);
		 return 0;
	        }
	      for (m=j=0;j < xsize[k];j++)
		{
		 sprintf(pA->axv[i]+m,"%-18.9g",*(xv[k]+j));
		 trimbl(pA->axv[i]+m,18);
		 for (pc=pA->axv[i]+m; *pc != '\0';pc++,m++);
		 if (j < xsize[k]-1) *(pA->axv[i]+(m++))=',';
		}
	      if (pA->axj[i] != NULL) free(pA->axj[i]);
	      if ((pA->axj[i]=(char *)malloc(8)) == NULL)
	        {
		 err_warn(1,fperr,
			"Error - memory for A_%s changes not found. \n",
					a_name);
		 return 0;
	        }
	      sprintf(pA->axj[i],"%d",xj[k]);
	      if (pA->axf[i] != NULL) {free(pA->axf[i]); pA->axf[i]=NULL;}
	      if (pA->axl[i] != NULL) {free(pA->axl[i]); pA->axl[i]=NULL;}
	      change=1;
	     }
	   else if (xj[k] != 0 && xj[k] != *pA->xj[i])
	     {
	      if (pA->axv[i] != NULL) {free(pA->axv[i]); pA->axv[i]=NULL;}
	      if (pA->axj[i] != NULL) {free(pA->axj[i]); pA->axj[i]=NULL;}
	      if ( (pA->axj[i]=(char *)malloc(20)) == NULL)
	        {
	         err_warn(1,fperr,
			"Error - memory for A_%s changes not found.\n",
					a_name);
		 return 0;
		}
	      sprintf (pA->axj[i],"%d",xj[k]);

	      if (*pA->xf[i] != xf[k] || *pA->xl[i] != xl[k]) /*DNW - 7/27/95*/
		{
	         if (pA->axf[i] != NULL) {free(pA->axf[i]); pA->axf[i]=NULL;}
		 if ((pA->axf[i]=(char *)malloc(20)) == NULL)
	           {
		    err_warn(1,fperr,
			"Error - memory for A_%s changes not found. \n",
					a_name);
		    return 0;
	           }
	         sprintf(pA->axf[i],"%-18.9g",xf[k]);
		 trimbl(pA->axf[i],18);
		}

	      if (*pA->xl[i] != xl[k] || *pA->xf[i] != xf[k]) /*DNW - 7/27/95*/
		{
	         if (pA->axl[i] != NULL) {free(pA->axl[i]); pA->axl[i]=NULL;}
	         if ((pA->axl[i]=(char *)malloc(20)) == NULL)
	           {
		    err_warn(1,fperr,
			"Error - memory for A_%s changes not found. \n",
					a_name);
		    return 0;
	           }
	         sprintf(pA->axl[i],"%-18.9g",xl[k]);
		 trimbl(pA->axl[i],18);
		}
	      change=1;
	     }
/*				If all values or a stride of values
				within a range are chosen.		*/

	   else if(*pA->xf[i] != xf[k] || *pA->xl[i] != xl[k])
	     {
	      paf=pal=NULL;
	      if ((paf=(char *)malloc(20)) == NULL ||
		        (pal=(char *)malloc(20)) == NULL )
	        {
	         err_warn(1,fperr,
			"Error - memory for A_%s changes not found. \n",
					a_name);
		 free(paf);
		 free(pal);
		 return 0;
		}
	      sprintf (paf,"%-18.9g",xf[k]);
	      trimbl(paf,18);
	      sprintf (pal,"%-18.9g",xl[k]);
	      trimbl(pal,18);
	      if (pA->axf[i] != NULL) {free(pA->axf[i]); pA->axf[i]=NULL;}
	      if (pA->axl[i] != NULL) {free(pA->axl[i]); pA->axl[i]=NULL;}
	      pA->axf[i]=paf;
	      pA->axl[i]=pal;
	      change=1;
	     }

	   if (change)
	     {
	      for (j=pA->ND-1;j>-1;j--)
	        {
	         if (pA->aXN[j] == NULL)
		   {
		    if ( (pA->aXN[j]=(char *)malloc(CW_MAX_NAME)) == NULL)
		      {
		       err_warn(1,fperr,
			"Error - memory for A_%s changes not found. \n",
					a_name);
		      }
	            strncpy(pA->aXN[j],pA->XN[j],CW_MAX_NAME);pA->aXN[j][CW_MAX_NAME-1] = '\0';
		   }
	        }
	     }
	   change=0;
	  }
	return 1;
      }


    int swap_dim(int XO[],struct a_attr *pA)
      {
	int i,j,k,nd;
	char *ch[NDS];
	int *in[NDS];
	float *fp[NDS];

	nd=pA->ND;
	for (i=0;i<nd;i++)
	  {
	   if (XO[i] != i) break;
	  }
	if (i == nd) return 1;		/* No reordering required.	*/

/*		Just in case, check that all values exist in XO.	*/

	for (i=0;i<nd;i++)
	  {
	   for (j=0;j<nd;j++)
	     {
	      if (XO[j] == i) break;
	     }
	   if (j == nd)
	     {
	      err_warn(1,fperr,
		"Error - (swap_dim) dimension order indices not correct.\n");
	      return 0;
	     }
	  }

	for (i=0;i<nd;i++) ch[i]=pA->XN[XO[i]];
	for (i=0;i<nd;i++) pA->XN[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->XU[XO[i]];
	for (i=0;i<nd;i++) pA->XU[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->xn[XO[i]];
	for (i=0;i<nd;i++) pA->xn[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->xu[XO[i]];
	for (i=0;i<nd;i++) pA->xu[i]=ch[i];

	for (i=0;i<nd;i++) ch[i]=pA->aXN[XO[i]];
	for (i=0;i<nd;i++) pA->aXN[i]=ch[i];

	for (i=0;i<nd;i++) ch[i]=pA->aXU[XO[i]];
	for (i=0;i<nd;i++) pA->aXU[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->axn[XO[i]];
	for (i=0;i<nd;i++) pA->axn[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->axu[XO[i]];
	for (i=0;i<nd;i++) pA->axu[i]=ch[i];

	for (i=0;i<nd;i++) in[i]=pA->XS[XO[i]];
	for (i=0;i<nd;i++) pA->XS[i]=in[i];
	for (i=0;i<nd;i++) in[i]=pA->XK[XO[i]];
	for (i=0;i<nd;i++) pA->XK[i]=in[i];
	for (i=0;i<nd;i++) in[i]=pA->xs[XO[i]];
	for (i=0;i<nd;i++) pA->xs[i]=in[i];
	for (i=0;i<nd;i++) in[i]=pA->xj[XO[i]];
	for (i=0;i<nd;i++) pA->xj[i]=in[i];
	for (i=0;i<nd;i++) in[i]=pA->xi[XO[i]];
	for (i=0;i<nd;i++) pA->xi[i]=in[i];

	for (i=0;i<nd;i++) ch[i]=pA->aXS[XO[i]];
	for (i=0;i<nd;i++) pA->aXS[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->aXK[XO[i]];
	for (i=0;i<nd;i++) pA->aXK[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->axs[XO[i]];
	for (i=0;i<nd;i++) pA->axs[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->axj[XO[i]];
	for (i=0;i<nd;i++) pA->axj[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->axi[XO[i]];
	for (i=0;i<nd;i++) pA->axi[i]=ch[i];

	for (i=0;i<nd;i++) fp[i]=pA->XC[XO[i]];
	for (i=0;i<nd;i++) pA->XC[i]=fp[i];
	for (i=0;i<nd;i++) fp[i]=pA->XF[XO[i]];
	for (i=0;i<nd;i++) pA->XF[i]=fp[i];
	for (i=0;i<nd;i++) fp[i]=pA->XL[XO[i]];
	for (i=0;i<nd;i++) pA->XL[i]=fp[i];
	for (i=0;i<nd;i++) fp[i]=pA->xf[XO[i]];
	for (i=0;i<nd;i++) pA->xf[i]=fp[i];
	for (i=0;i<nd;i++) fp[i]=pA->xl[XO[i]];
	for (i=0;i<nd;i++) pA->xl[i]=fp[i];

	for (i=0;i<nd;i++) ch[i]=pA->aXC[XO[i]];
	for (i=0;i<nd;i++) pA->aXC[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->aXF[XO[i]];
	for (i=0;i<nd;i++) pA->aXF[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->aXL[XO[i]];
	for (i=0;i<nd;i++) pA->aXL[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->axf[XO[i]];
	for (i=0;i<nd;i++) pA->axf[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->axl[XO[i]];
	for (i=0;i<nd;i++) pA->axl[i]=ch[i];

	for (i=0;i<nd;i++) fp[i]=pA->XV[XO[i]];
	for (i=0;i<nd;i++) pA->XV[i]=fp[i];
	for (i=0;i<nd;i++) fp[i]=pA->XB[XO[i]];
	for (i=0;i<nd;i++) pA->XB[i]=fp[i];
	for (i=0;i<nd;i++) fp[i]=pA->XW[XO[i]];
	for (i=0;i<nd;i++) pA->XW[i]=fp[i];
	for (i=0;i<nd;i++) fp[i]=pA->xv[XO[i]];
	for (i=0;i<nd;i++) pA->xv[i]=fp[i];
	for (i=0;i<nd;i++) fp[i]=pA->xb[XO[i]];
	for (i=0;i<nd;i++) pA->xb[i]=fp[i];
	for (i=0;i<nd;i++) fp[i]=pA->xw[XO[i]];
	for (i=0;i<nd;i++) pA->xw[i]=fp[i];

	for (i=0;i<nd;i++) ch[i]=pA->aXV[XO[i]];
	for (i=0;i<nd;i++) pA->aXV[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->aXB[XO[i]];
	for (i=0;i<nd;i++) pA->aXB[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->aXW[XO[i]];
	for (i=0;i<nd;i++) pA->aXW[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->axv[XO[i]];
	for (i=0;i<nd;i++) pA->axv[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->axb[XO[i]];
	for (i=0;i<nd;i++) pA->axb[i]=ch[i];
	for (i=0;i<nd;i++) ch[i]=pA->axw[XO[i]];
	for (i=0;i<nd;i++) pA->axw[i]=ch[i];
        return 1;
      }


    int compare_x(int n,float x[],float x1[])
      {
	int i,j,k;

	for (i=0;i<n;i++)
	  {
	   if (x[i] != x1[i]) return 0;
	  }
	return 1;
      }
