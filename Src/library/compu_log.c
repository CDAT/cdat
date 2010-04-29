#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "array.h"


#define NFUNC 2

    extern FILE *fpin, *fpout, *fperr;/* i/o and error files for scripts */
    extern struct a_tab A_tab;

    extern struct a_tab A_tmp;

/*  Structures for assignment of array.

    struct argmnt
	{
	 char name[17];
	 int ok;
	 struct a_attr *paa;
	 float v;
	 struct data_flo *pdf;
	};

    struct data_flo
	{
	 struct data_flo *prev; |+  Pointer to previous structure.	|+
	 struct data_flo *next; |+  Pointer to next structure.		|+
	 struct argmnt rtrn;	 |+  Return argument for function.	|+
	 char func[17];		 |+  Function name.			|+
	 struct argmnt argm[MAXARGS];|+  Given argument(s) for function.|+
	};
*/

/*			Integer adjustable indices			*/

    extern int I;
    extern int J;
    extern int K;
    extern int L;
    extern int M;
    extern int N;

/*		Function prototypes.					*/

    int err_warn (int beep,FILE *fp,char *fmt,...);
    char *repstr(char *s2,char *s1);
    int GT_scalar(struct a_attr *pa,struct a_attr *PA,float v);
    int LT_scalar(struct a_attr *pa,struct a_attr *PA,float v);
    int GE_scalar(struct a_attr *pa,struct a_attr *PA,float v);
    int LE_scalar(struct a_attr *pa,struct a_attr *PA,float v);
    int EQ_scalar(struct a_attr *pa,struct a_attr *PA,float v);
    int NE_scalar(struct a_attr *pa,struct a_attr *PA,float v);

    int GT_array(struct a_attr *pa,struct a_attr *PA1,struct a_attr *PA2);
    int LT_array(struct a_attr *pa,struct a_attr *PA1,struct a_attr *PA2);
    int GE_array(struct a_attr *pa,struct a_attr *PA1,struct a_attr *PA2);
    int LE_array(struct a_attr *pa,struct a_attr *PA1,struct a_attr *PA2);
    int EQ_array(struct a_attr *pa,struct a_attr *PA1,struct a_attr *PA2);
    int NE_array(struct a_attr *pa,struct a_attr *PA1,struct a_attr *PA2);


    int compu_log(struct data_flo **pd,struct a_attr *pa)
      {
       int j,err;
       struct data_flo *pdd;
       struct a_tab *ptab;
       struct a_attr *pA;

       for (pdd=*pd;pdd != NULL;pdd=pdd->next)
	 {
	  if (pdd->rtrn.paa != pa)
	    {
	     err_warn(1,fperr,
		"Error - return arg isn't right for (%s).\n",pdd->rtrn.name);
/*	     killA_tmp();			*/
	     clear_data_flo(pd);
	     pa->notok=1;
	     return 0;
	    }
	  for (j=0;j < 2;j++)
	    {
	     if ((pA=pdd->argm[j].paa) != NULL )
	       {
		if (pdd->argm[j].pdf != NULL)
		  {
		   if (compu_log(&pdd->argm[j].pdf,pA) == 0)
		     {
		      err_warn(1,fperr,
			"Error - in dimensions for (%s).\n",pdd->rtrn.name);
/*		      killA_tmp();			*/
		      clear_data_flo(pd);
		      pa->notok=1;
		      return 0;
		     }
		  }
	        else if (pA->un.data == NULL)
	          {
		   for(ptab=&A_tab;
			ptab != NULL && pA != ptab->pA_attr;ptab=ptab->next);
		   if (ptab == NULL)
			for (ptab=&A_tmp;
			  ptab != NULL && pA != ptab->pA_attr;ptab=ptab->next);
		   if (ptab == NULL)
		     {
		      err_warn(1,fperr,
		       "Error - no array table entry for mask element.\n");
/*		      killA_tmp();			*/
		      clear_data_flo(pd);
		      pa->notok=1;
		      return 0;
		     }
#ifdef cray
		   if (ptab->pA_attr->un.data == NULL &&
						acquire_A(ptab,"R*8") == 0)
#else
		   if (ptab->pA_attr->un.data == NULL &&
						acquire_A(ptab,"R*4") == 0)
#endif
		     {
		      err_warn(1,fperr,
			  "Error - cannot acquire A_%s for logical mask.\n",
								ptab->name);
/*		      killA_tmp();			*/
		      clear_data_flo(pd);
		      pa->notok=1;
		      return 0;
		     }
		  }
	       }
	    }
	  err=1;
	  if (pdd->argm[0].paa != NULL && pdd->argm[1].paa != NULL)
	    {
	     if      (strcmp(pdd->func,"AND")==0)
			err=AND_array(pa,pdd->argm[0].paa,pdd->argm[1].paa);
	     else if (strcmp(pdd->func,"OR")==0)
			err=OR_array(pa,pdd->argm[0].paa,pdd->argm[1].paa);
	     else if (strcmp(pdd->func,"LT")==0)
			err=LT_array(pa,pdd->argm[0].paa,pdd->argm[1].paa);
	     else if (strcmp(pdd->func,"LE")==0)
			err=LE_array(pa,pdd->argm[0].paa,pdd->argm[1].paa);
	     else if (strcmp(pdd->func,"GT")==0)
			err=GT_array(pa,pdd->argm[0].paa,pdd->argm[1].paa);
	     else if (strcmp(pdd->func,"GE")==0)
			err=GE_array(pa,pdd->argm[0].paa,pdd->argm[1].paa);
	     else if (strcmp(pdd->func,"EQ")==0)
			err=EQ_array(pa,pdd->argm[0].paa,pdd->argm[1].paa);
	     else if (strcmp(pdd->func,"NE")==0)
			err=NE_array(pa,pdd->argm[0].paa,pdd->argm[1].paa);
	     else
	       {
		err_warn(1,fperr,
			"Error - array function not found (%s).\n",pdd->func);
/*		killA_tmp();			*/
		clear_data_flo(pd);
		pa->notok=1;
		return 0;
	       }
	    }
	  else if (pdd->argm[0].paa != NULL && pdd->argm[1].v != 1.e20)
	    {
	     if      (strcmp(pdd->func,"LT")==0)
			err=LT_scalar(pa,pdd->argm[0].paa,pdd->argm[1].v);
	     else if (strcmp(pdd->func,"LE")==0)
			err=LE_scalar(pa,pdd->argm[0].paa,pdd->argm[1].v);
	     else if (strcmp(pdd->func,"GT")==0)
			err=GT_scalar(pa,pdd->argm[0].paa,pdd->argm[1].v);
	     else if (strcmp(pdd->func,"GE")==0)
			err=GE_scalar(pa,pdd->argm[0].paa,pdd->argm[1].v);
	     else if (strcmp(pdd->func,"NE")==0)
			err=NE_scalar(pa,pdd->argm[0].paa,pdd->argm[1].v);
	     else if (strcmp(pdd->func,"EQ")==0)
			err=EQ_scalar(pa,pdd->argm[0].paa,pdd->argm[1].v);
	     else
	       {
		err_warn(1,fperr,
			"Error - scalar function not found (%s).\n",pdd->func);
/*		killA_tmp();			*/
		clear_data_flo(pd);
		pa->notok=1;
		return 0;
	       }
	    }
	  else if (pdd->argm[0].v != 1.e20 && pdd->argm[1].paa != NULL)
	    {
	     if      (strcmp(pdd->func,"LT")==0)
			err=GE_scalar(pa,pdd->argm[1].paa,pdd->argm[0].v);
	     else if (strcmp(pdd->func,"LE")==0)
			err=GT_scalar(pa,pdd->argm[1].paa,pdd->argm[0].v);
	     else if (strcmp(pdd->func,"GT")==0)
			err=LE_scalar(pa,pdd->argm[1].paa,pdd->argm[0].v);
	     else if (strcmp(pdd->func,"GE")==0)
			err=LT_scalar(pa,pdd->argm[1].paa,pdd->argm[0].v);
	     else if (strcmp(pdd->func,"NE")==0)
			err=NE_scalar(pa,pdd->argm[1].paa,pdd->argm[0].v);
	     else if (strcmp(pdd->func,"EQ")==0)
			err=EQ_scalar(pa,pdd->argm[1].paa,pdd->argm[0].v);
	     else
	       {
		err_warn(1,fperr,
			"Error - scalar function not found (%s).\n",pdd->func);
/*		killA_tmp();		*/
		clear_data_flo(pd);
		pa->notok=1;
		return 0;
	       }
	    }
	  if (err == 0)
	    {
	     err_warn(1,fperr,
		"Error - invalid mask function A_%s = A_%s (%s) A_%s.\n",
		 pdd->rtrn.name,pdd->argm[0].name,pdd->func,pdd->argm[1].name);
/*	     killA_tmp();			*/
	     clear_data_flo(pd);
	     pa->notok=1;
	     return 0;
	    }
	 }

       return 1;
      }

/*			And array pA1 with pA2 and store in pa.		*/

    int AND_array(struct a_attr *pa,struct a_attr *pA1,struct a_attr *pA2)
      {
       int i,j,k,l,n;
       int IX1[NDS],IX2[NDS];
       int im[NDS],IM;
       int I1[NDS],I2[NDS];
       short a,b;

       for (i=0;i<NDS;i++) {I1[i]=I2[i]=-1; IX1[i]=IX2[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two masks to be anded.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - AND of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA2->ND;i++) if (cmpncs(pa->xn[n],pA2->xn[i]) == 0)
	    {
	     I2[i]=n;
	     break;
	    }
	  if ( (i==pA2->ND && *pa->xs[n]>1) ||
		 (i<pA2->ND && *pa->xs[n]!=*pA2->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - AND of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	  if (I2[i] < 0 || *pA2->xs[I2[i]] == 1) IX2[i] = 0;
	  else
	     for (n=0;n<I2[i];n++) IX2[i]=*pA2->xs[n]*IX2[i];
	 }
       if (pA1->mask == NULL || pA2->mask == NULL)
	 {
	  err_warn(1,fperr,"Error - mask not available (%s) AND (%s).\n",
			pA1->n,pA2->n);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/
       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->mask[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  b = pA2->mask[i*IX2[0]+j*IX2[1]+k*IX2[2]+l*IX2[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a & b;
		 }
	return 1;
      }

/*			OR array pA1 with pA2 and store into pa.	*/

    int OR_array(struct a_attr *pa,struct a_attr *pA1,struct a_attr *pA2)
      {
       int i,j,k,l,n;
       int IX1[NDS],IX2[NDS];
       int im[NDS],IM;
       int I1[NDS],I2[NDS];
       short a,b;

       for (i=0;i<NDS;i++) {I1[i]=I2[i]=-1; IX1[i]=IX2[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two masks to be ored.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - OR of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA2->ND;i++) if (cmpncs(pa->xn[n],pA2->xn[i]) == 0)
	    {
	     I2[i]=n;
	     break;
	    }
	  if ( (i==pA2->ND && *pa->xs[n]>1) ||
		 (i<pA2->ND && *pa->xs[n]!=*pA2->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - OR of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	  if (I2[i] < 0 || *pA2->xs[I2[i]] == 1) IX2[i] = 0;
	  else
	     for (n=0;n<I2[i];n++) IX2[i]=*pA2->xs[n]*IX2[i];
	 }

       if (pA1->mask == NULL || pA2->mask == NULL)
	 {
	  err_warn(1,fperr,"Error - mask not available (%s) OR (%s).\n",
			pA1->n,pA2->n);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->mask[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  b = pA2->mask[i*IX2[0]+j*IX2[1]+k*IX2[2]+l*IX2[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a | b;
		 }
	return 1;
      }


/*			Store logical ( pA1 GT pA2 ) into pa.	*/

    int GT_array(struct a_attr *pa,struct a_attr *pA1,struct a_attr *pA2)
      {
       int i,j,k,l,n;
       int IX1[NDS],IX2[NDS];
       int im[NDS],IM;
       int I1[NDS],I2[NDS];
       float a,b;

       for (i=0;i<NDS;i++) {I1[i]=I2[i]=-1; IX1[i]=IX2[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two arrays to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - GT of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA2->ND;i++) if (cmpncs(pa->xn[n],pA2->xn[i]) == 0)
	    {
	     I2[i]=n;
	     break;
	    }
	  if ( (i==pA2->ND && *pa->xs[n]>1) ||
		 (i<pA2->ND && *pa->xs[n]!=*pA2->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - GT of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	  if (I2[i] < 0 || *pA2->xs[I2[i]] == 1) IX2[i] = 0;
	  else
	     for (n=0;n<I2[i];n++) IX2[i]=*pA2->xs[n]*IX2[i];
	 }

       if (pA1->un.data == NULL || pA2->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) GT (%s).\n",
			pA1->n,pA2->n);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  b = pA2->un.data[i*IX2[0]+j*IX2[1]+k*IX2[2]+l*IX2[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a > b;
		 }
	return 1;
      }


/*			Store logical ( pA1 GT v ) into pa.	*/

    int GT_scalar(struct a_attr *pa,struct a_attr *pA1,float v)
      {
       int i,j,k,l,n;
       int IX1[NDS];
       int im[NDS],IM;
       int I1[NDS];
       float a;

       for (i=0;i<NDS;i++) {I1[i]=-1; IX1[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the the array to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - GT of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	 }

       if (pA1->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) GT (%g).\n",
			pA1->n,v);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a > v;
		 }
	return 1;
      }


/*			Store logical ( pA1 GE pA2 ) into pa.	*/

    int GE_array(struct a_attr *pa,struct a_attr *pA1,struct a_attr *pA2)
      {
       int i,j,k,l,n;
       int IX1[NDS],IX2[NDS];
       int im[NDS],IM;
       int I1[NDS],I2[NDS];
       float a,b;

       for (i=0;i<NDS;i++) {I1[i]=I2[i]=-1; IX1[i]=IX2[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two arrays to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - GE of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA2->ND;i++) if (cmpncs(pa->xn[n],pA2->xn[i]) == 0)
	    {
	     I2[i]=n;
	     break;
	    }
	  if ( (i==pA2->ND && *pa->xs[n]>1) ||
		 (i<pA2->ND && *pa->xs[n]!=*pA2->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - GE of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	  if (I2[i] < 0 || *pA2->xs[I2[i]] == 1) IX2[i] = 0;
	  else
	     for (n=0;n<I2[i];n++) IX2[i]=*pA2->xs[n]*IX2[i];
	 }

       if (pA1->un.data == NULL || pA2->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) GE (%s).\n",
			pA1->n,pA2->n);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  b = pA2->un.data[i*IX2[0]+j*IX2[1]+k*IX2[2]+l*IX2[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a >= b;
		 }
	return 1;
      }


/*			Store logical ( pA1 GE v ) into pa.	*/

    int GE_scalar(struct a_attr *pa,struct a_attr *pA1,float v)
      {
       int i,j,k,l,n;
       int IX1[NDS];
       int im[NDS],IM;
       int I1[NDS];
       float a;

       for (i=0;i<NDS;i++) {I1[i]=-1; IX1[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two arrays to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - GE of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	 }

       if (pA1->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) GE (%g).\n",
			pA1->n,v);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a >= v;
		 }
	return 1;
      }

/*			Store logical ( pA1 LT pA2 ) into pa.	*/

    int LT_array(struct a_attr *pa,struct a_attr *pA1,struct a_attr *pA2)
      {
       int i,j,k,l,n;
       int IX1[NDS],IX2[NDS];
       int im[NDS],IM;
       int I1[NDS],I2[NDS];
       float a,b;

       for (i=0;i<NDS;i++) {I1[i]=I2[i]=-1; IX1[i]=IX2[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two arrays to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - LT of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA2->ND;i++) if (cmpncs(pa->xn[n],pA2->xn[i]) == 0)
	    {
	     I2[i]=n;
	     break;
	    }
	  if ( (i==pA2->ND && *pa->xs[n]>1) ||
		 (i<pA2->ND && *pa->xs[n]!=*pA2->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - LT of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	  if (I2[i] < 0 || *pA2->xs[I2[i]] == 1) IX2[i] = 0;
	  else
	     for (n=0;n<I2[i];n++) IX2[i]=*pA2->xs[n]*IX2[i];
	 }

       if (pA1->un.data == NULL || pA2->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) LT (%s).\n",
			pA1->n,pA2->n);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  b = pA2->un.data[i*IX2[0]+j*IX2[1]+k*IX2[2]+l*IX2[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a < b;
		 }
	return 1;
      }


/*			Store logical ( pA1 LT v ) into pa.	*/

    int LT_scalar(struct a_attr *pa,struct a_attr *pA1,float v)
      {
       int i,j,k,l,n;
       int IX1[NDS];
       int im[NDS],IM;
       int I1[NDS];
       float a;

       for (i=0;i<NDS;i++) {I1[i]=-1; IX1[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two arrays to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - LT of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	 }

       if (pA1->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) LT (%g).\n",
			pA1->n,v);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a < v;
		 }
	return 1;
      }

/*			Store logical ( pA1 LE pA2 ) into pa.	*/

    int LE_array(struct a_attr *pa,struct a_attr *pA1,struct a_attr *pA2)
      {
       int i,j,k,l,n;
       int IX1[NDS],IX2[NDS];
       int im[NDS],IM;
       int I1[NDS],I2[NDS];
       float a,b;

       for (i=0;i<NDS;i++) {I1[i]=I2[i]=-1; IX1[i]=IX2[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two arrays to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - LE of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA2->ND;i++) if (cmpncs(pa->xn[n],pA2->xn[i]) == 0)
	    {
	     I2[i]=n;
	     break;
	    }
	  if ( (i==pA2->ND && *pa->xs[n]>1) ||
		 (i<pA2->ND && *pa->xs[n]!=*pA2->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - LE of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	  if (I2[i] < 0 || *pA2->xs[I2[i]] == 1) IX2[i] = 0;
	  else
	     for (n=0;n<I2[i];n++) IX2[i]=*pA2->xs[n]*IX2[i];
	 }

       if (pA1->un.data == NULL || pA2->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) LE (%s).\n",
			pA1->n,pA2->n);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  b = pA2->un.data[i*IX2[0]+j*IX2[1]+k*IX2[2]+l*IX2[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a <= b;
		 }
	return 1;
      }


/*			Store logical ( pA1 LE v ) into pa.	*/

    int LE_scalar(struct a_attr *pa,struct a_attr *pA1,float v)
      {
       int i,j,k,l,n;
       int IX1[NDS];
       int im[NDS],IM;
       int I1[NDS];
       float a;

       for (i=0;i<NDS;i++) {I1[i]=-1; IX1[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two arrays to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - LE of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	 }

       if (pA1->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) LE (%g).\n",
			pA1->n,v);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a <= v;
		 }
	return 1;
      }

/*			Store logical ( pA1 EQ pA2 ) into pa.	*/

    int EQ_array(struct a_attr *pa,struct a_attr *pA1,struct a_attr *pA2)
      {
       int i,j,k,l,n;
       int IX1[NDS],IX2[NDS];
       int im[NDS],IM;
       int I1[NDS],I2[NDS];
       float a,b;

       for (i=0;i<NDS;i++) {I1[i]=I2[i]=-1; IX1[i]=IX2[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two arrays to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - EQ of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA2->ND;i++) if (cmpncs(pa->xn[n],pA2->xn[i]) == 0)
	    {
	     I2[i]=n;
	     break;
	    }
	  if ( (i==pA2->ND && *pa->xs[n]>1) ||
		 (i<pA2->ND && *pa->xs[n]!=*pA2->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - EQ of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	  if (I2[i] < 0 || *pA2->xs[I2[i]] == 1) IX2[i] = 0;
	  else
	     for (n=0;n<I2[i];n++) IX2[i]=*pA2->xs[n]*IX2[i];
	 }

       if (pA1->un.data == NULL || pA2->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) EQ (%s).\n",
			pA1->n,pA2->n);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  b = pA2->un.data[i*IX2[0]+j*IX2[1]+k*IX2[2]+l*IX2[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a == b;
		 }
	return 1;
      }


/*			Store logical ( pA1 EQ v ) into pa.	*/

    int EQ_scalar(struct a_attr *pa,struct a_attr *pA1,float v)
      {
       int i,j,k,l,n;
       int IX1[NDS];
       int im[NDS],IM;
       int I1[NDS];
       float a;

       for (i=0;i<NDS;i++) {I1[i]=-1; IX1[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two arrays to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - EQ of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	 }

       if (pA1->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) EQ (%g).\n",
			pA1->n,v);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a == v;
		 }
	return 1;
      }

/*			Store logical ( pA1 NE pA2 ) into pa.	*/

    int NE_array(struct a_attr *pa,struct a_attr *pA1,struct a_attr *pA2)
      {
       int i,j,k,l,n;
       int IX1[NDS],IX2[NDS];
       int im[NDS],IM;
       int I1[NDS],I2[NDS];
       float a,b;

       for (i=0;i<NDS;i++) {I1[i]=I2[i]=-1; IX1[i]=IX2[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two arrays to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - NE of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA2->ND;i++) if (cmpncs(pa->xn[n],pA2->xn[i]) == 0)
	    {
	     I2[i]=n;
	     break;
	    }
	  if ( (i==pA2->ND && *pa->xs[n]>1) ||
		 (i<pA2->ND && *pa->xs[n]!=*pA2->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - NE of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	  if (I2[i] < 0 || *pA2->xs[I2[i]] == 1) IX2[i] = 0;
	  else
	     for (n=0;n<I2[i];n++) IX2[i]=*pA2->xs[n]*IX2[i];
	 }

       if (pA1->un.data == NULL || pA2->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) NE (%s).\n",
			pA1->n,pA2->n);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  b = pA2->un.data[i*IX2[0]+j*IX2[1]+k*IX2[2]+l*IX2[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a != b;
		 }
	return 1;
      }


/*			Store logical ( pA1 NE v ) into pa.	*/

    int NE_scalar(struct a_attr *pa,struct a_attr *pA1,float v)
      {
       int i,j,k,l,n;
       int IX1[NDS];
       int im[NDS],IM;
       int I1[NDS];
       float a;

       for (i=0;i<NDS;i++) {I1[i]=-1; IX1[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the two arrays to be compared.
			I[i] points to the corresponding dimension to
			be added.					*/

       for (n=0;n<pa->ND;n++)
	 {
	  for (i=0;i<pA1->ND;i++) if (cmpncs(pa->xn[n],pA1->xn[i]) == 0)
	    {
	     I1[i]=n;
	     break;
	    }
	  if ( (i==pA1->ND && *pa->xs[n]>1) ||
		 (i<pA1->ND && *pa->xs[n]!=*pA1->xs[i]) )
	    {
	     err_warn(1,fperr,"Error - NE of mask dimensions won't fit.\n");
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I1[i] < 0 || *pA1->xs[I1[i]] == 1) IX1[i] = 0;
	  else
	     for (n=0;n<I1[i];n++) IX1[i]=*pA1->xs[n]*IX1[i];
	 }

       if (pA1->un.data == NULL)
	 {
	  err_warn(1,fperr,"Error - comparison not available (%s) NE (%g).\n",
			pA1->n,v);
	  return 0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(sizeof(short)*IM)) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory for mask not found \n");
	     free ((char *)pa->un.data); pa->un.data=NULL;
	     return 0;
	    }

          for (i=0;i<IM;i++) *(pa->mask+i)=1; /* Set all values valid.*/
	 }

/*			Compute the mask.				*/

       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pA1->un.data[i*IX1[0]+j*IX1[1]+k*IX1[2]+l*IX1[3]];
		  pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = a != v;
		 }
	return 1;
      }

