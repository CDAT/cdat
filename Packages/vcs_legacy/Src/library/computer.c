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
	 char *s;
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

    struct func_list
      {
       char name[17];
       int num_args;
      } funcs[NFUNC]={"sqrt",1,"mean",1};				*/


    struct func_list
      {
       char name[17];
       int num_args;
      };

    extern struct func_list funcs[NFUNC];

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
    int ADD_scalar(struct a_attr *pa,float v);
    int SUB_scalar(struct a_attr *pa,float v);
    int MUL_scalar(struct a_attr *pa,float v);
    int DIV_scalar(struct a_attr *pa,float v);
    int SQRT_scalar(struct a_attr *pa,float v);
    int POWER_array(struct a_attr *pa,struct a_attr *pA,float v);
    int MEAN(struct a_attr *pa,struct argmnt *parg);


    int computer(struct data_flo *pd,struct a_attr *pa)
      {
       int i,j,n,ier;
       struct data_flo *pdd;
       struct a_tab *ptab;
       struct a_attr *pA;

       if (pd == NULL) return 0;

       for (pdd=pd;pdd != NULL;pdd=pdd->next)
	 {
	  if (pdd->rtrn.paa != pa)
	    {
	     err_warn(1,fperr,
		"Error - return arg isn't right for (%s).\n",pdd->rtrn.name);
/*	     killA_tmp();			*/
	     clear_data_flo(&pd);
	     pa->notok=1;
	     return 0;
	    }
	  for (i=0;i < NFUNC && cmpncs(pdd->func,funcs[i].name) != 0;i++);
	  if (i == NFUNC) n=1;
	  else n=funcs[i].num_args;
	  for (j=0;j < n;j++)
	    {
	     if (pdd->argm[j].paa != NULL )
	       {
		if (pdd->argm[j].pdf != NULL)
		  {
		   if (computer(pdd->argm[j].pdf,pdd->argm[j].paa) == 0)
		     {
		      err_warn(1,fperr,
			"Error - in dimensions for (%s).\n",pdd->rtrn.name);
/*		      killA_tmp();			*/
		      clear_data_flo(&pd);
		      pa->notok=1;
		      return 0;
		     }
		  }
	       }
	    }
	  if (i < NFUNC)
	    {
	     if ((pA=pdd->argm[0].paa) != NULL)
	       {
		if (pA->un.data == NULL)
		  {
		   for(ptab=&A_tab;
			ptab != NULL && pA != ptab->pA_attr;ptab=ptab->next);
		   if (ptab == NULL)
			for (ptab=&A_tmp;
			  ptab != NULL && pA != ptab->pA_attr;ptab=ptab->next);
		   if (ptab == NULL)
		     {
		      err_warn(1,fperr,
		       "Error - no array table entry for (%s).\n",
							pdd->argm[0].name);
/*		      killA_tmp();			*/
		      clear_data_flo(&pd);
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
			  "Error - cannot acquire A_%s for l.h.s.\n",
								ptab->name);
/*		      killA_tmp();				*/
		      clear_data_flo(&pd);
		      pa->notok=1;
		      return 0;
		     }
		  }
		ier=1;
	        if (i == 0)  /*		SQRT function needed.		*/
			ier=SQRT_array(pa,pdd->argm[0].paa);
		else if (i == 1) /*	MEAN function needed.		*/
			ier=MEAN(pa,pdd->argm);
	       }
	     else if (pdd->argm[0].v != 1.e20)
	       {
		ier=1;
	        if (i == 0)  /*		SQRT function needed.		*/
			ier=SQRT_scalar(pa,pdd->argm[0].v);
	       }
	     if (ier == 0)
	       {
		err_warn(1,fperr,
		    "Error - function (%s) variable (%s).\n",pdd->func,
							pdd->argm[0].name);
		clear_data_flo(&pd);
		pa->notok=1;
		return 0;
	       }
	    }
	  else if ((pA=pdd->argm[0].paa) != NULL)
	    {
	     if (pA->un.data == NULL)
	       {
		for(ptab=&A_tab;
			ptab != NULL && pA != ptab->pA_attr;ptab=ptab->next);
		if (ptab == NULL)
			for (ptab=&A_tmp;
			  ptab != NULL && pA != ptab->pA_attr;ptab=ptab->next);
		if (ptab == NULL)
		  {
		   err_warn(1,fperr,
		       "Error - no array table entry for computing element.\n");
/*		   killA_tmp();				*/
		   clear_data_flo(&pd);
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
			  "Error - cannot acquire A_%s for l.h.s.\n",
								ptab->name);
/*		   killA_tmp();				*/
		   clear_data_flo(&pd);
		   pa->notok=1;
		   return 0;
		  }
	       }
	     ier=0;
	     if      (strcmp(pdd->func,"ADD")==0)
				 ier=ADD_array(pa,pdd->argm[0].paa);
	     else if (strcmp(pdd->func,"SUB")==0)
				 ier=SUB_array(pa,pdd->argm[0].paa);
	     else if (strcmp(pdd->func,"MUL")==0)
				 ier=MUL_array(pa,pdd->argm[0].paa);
	     else if (strcmp(pdd->func,"DIV")==0)
				 ier=DIV_array(pa,pdd->argm[0].paa);
	     else if (strcmp(pdd->func,"power")==0)
			ier=POWER_array(pa,pdd->argm[0].paa,pdd->argm[1].v);
	     else
	       {
		err_warn(1,fperr,
			"Error - array function not found (%s).\n",pdd->func);
		clear_data_flo(&pd);
		pa->notok=1;
		return 0;
	       }
	     if (ier == 0)
	       {
		err_warn(1,fperr,
		    "Error - function (%s) variable (%s).\n",pdd->func,
							pdd->argm[0].name);
		clear_data_flo(&pd);
		pa->notok=1;
		return 0;
	       }
	    }
	  else if (pdd->argm[0].v != 1.e20)
	    {
	     ier=0;
	     if      (strcmp(pdd->func,"ADD")==0)
				 ier=ADD_scalar(pa,pdd->argm[0].v);
	     else if (strcmp(pdd->func,"SUB")==0)
				 ier=SUB_scalar(pa,pdd->argm[0].v);
	     else if (strcmp(pdd->func,"MUL")==0)
				 ier=MUL_scalar(pa,pdd->argm[0].v);
	     else if (strcmp(pdd->func,"DIV")==0)
				 ier=DIV_scalar(pa,pdd->argm[0].v);
	     else
	       {
		err_warn(1,fperr,
			"Error - scalar function not found (%s).\n",pdd->func);
		clear_data_flo(&pd);
		pa->notok=1;
		return 0;
	       }
	     if (ier == 0)
	       {
		err_warn(1,fperr,
		    "Error - function (%s) value (%s).\n",pdd->func,
							pdd->argm[0].name);
		clear_data_flo(&pd);
		pa->notok=1;
		return 0;
	       }
	    }
	 }
       pa->notok=0;
       return 1;
      }

/*			Add array pA into pa.				*/

    int ADD_array(struct a_attr *pa,struct a_attr *pA)
      {
       int i,j,k,l,n;
       int IX[NDS];
       int im[NDS],IM;
       int I[NDS];
       double a,b;

       if (pA->notok == 1)
	 {
	  err_warn(1,fperr,"Error - variable (%s) not ok.\n",pA->n);
	  return 0;
	 }

       for (i=0;i<NDS;i++) {I[i]=-1; IX[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the lhs and the rhs variable.
			I[i] points to the corresponding dimension.	*/

       for (n=0;n<pA->ND;n++)
	 {
	  for (i=0;i<pa->ND;i++) if (strcmp(pA->xn[n],pa->XN[i]) == 0)
	    {
	     I[i]=n;
	     break;
	    }
	  if ( (i==pa->ND && *pA->xs[n]>1) ||
		 (i<pa->ND && (*pA->xs[n]>*pa->XS[i] ||
			(*pA->xs[n]>1 && *pA->xs[n]!=*pa->XS[i]))) )
	    {
	     err_warn(1,fperr,"Error - variable (%s) won't add.\n",pA->n);
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I[i] < 0 || *pA->xs[I[i]] == 1) IX[i] = 0;
	  else
	     for (n=0;n<I[i];n++) IX[i]=*pA->xs[n]*IX[i];
	 }

       if (pa->un.data == NULL)
	 {
	  if ((pa->un.data=(float *)malloc(IM*sizeof(float))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->un.data[i]=0.0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(IM*sizeof(short))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->mask[i]=1;
	 }
       for (l=0;l<im[3];l++)
	 for (k=0;k<im[2];k++)
	   for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))];
		  b = pA->un.data[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]];
		  if (pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] <= 0 ||
			pA->mask[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]] <= 0 ||
			 fabs(a) >= 0.99e20 || fabs(b) >= 0.99e20)
		    {
		     pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = 0;
		     pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = 1.e20;
		    }
		  else pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = a + b;
		 }
       return 1;
      }

/*			Add scalar v into pa.				*/

    int ADD_scalar(struct a_attr *pa,float v)
      {
       int i,j,k,l;
       int im[NDS],IM;
       double a,b;

       IM=1;
       for (i=0;i<NDS;i++)
	 {
	  if (pa->xs[i] != NULL && *pa->xs[i] > 0) im[i]=*pa->xs[i];
	  else im[i]=1;
	  IM=IM*im[i];
	 }
       if (pa->un.data == NULL)
	 {
	  if ((pa->un.data=(float *)malloc(IM*sizeof(float))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->un.data[i]=0.0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(IM*sizeof(short))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->mask[i]=1;
	 }

	 for (l=0;l<im[3];l++)
	   for (k=0;k<im[2];k++)
	     for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  b = v;
		  a = pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))];
		  if (pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] <= 0 ||
			 fabs(a) >= 0.99e20 || fabs(b) >= 0.99e20)
		    {
		     pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = 0;
		     pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = 1.e20;
		    }
		  else pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = a + b;
		 }
       return 1;
      }

/*			Subtract array pA into pa.			*/

    int SUB_array(struct a_attr *pa,struct a_attr *pA)
      {
       int i,j,k,l,n;
       int IX[NDS];
       int im[NDS],IM;
       int I[NDS];
       double a,b;

       if (pA->notok == 1)
	 {
	  err_warn(1,fperr,"Error - variable (%s) not ok.\n",pA->n);
	  return 0;
	 }

       for (i=0;i<NDS;i++) {I[i]=-1; IX[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the lhs and the rhs variable.
			I[i] points to the corresponding dimension.	*/

       for (n=0;n<pA->ND;n++)
	 {
	  for (i=0;i<pa->ND;i++) if (strcmp(pA->xn[n],pa->XN[i]) == 0)
	    {
	     I[i]=n;
	     break;
	    }
	  if ( (i==pa->ND && *pA->xs[n]>1) ||
		 (i<pa->ND && (*pA->xs[n]>*pa->XS[i] ||
			(*pA->xs[n]>1 && *pA->xs[n]!=*pa->XS[i]))) )
	    {
	     err_warn(1,fperr,"Error - variable (%s) won't add.\n",pA->n);
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I[i] < 0 || *pA->xs[I[i]] == 1) IX[i] = 0;
	  else
	     for (n=0;n<I[i];n++) IX[i]=*pA->xs[n]*IX[i];
	 }

       if (pa->un.data == NULL)
	 {
	  if ((pa->un.data=(float *)malloc(IM*sizeof(float))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->un.data[i]=0.0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(IM*sizeof(short))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->mask[i]=1;
	 }

	 for (l=0;l<im[3];l++)
	   for (k=0;k<im[2];k++)
	     for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))];
		  b = pA->un.data[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]];
		  if (pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] <= 0 ||
			pA->mask[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]] <= 0 ||
			 fabs(a) >= 0.99e20 || fabs(b) >= 0.99e20)
		    {
		     pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = 0;
		     pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = 1.e20;
		    }
		  else pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = a - b;
		 }
       return 1;
      }

/*			Add scalar v into pa.				*/

    int SUB_scalar(struct a_attr *pa,float v)
      {
       int i,j,k,l;
       int im[NDS],IM;
       float a,b;

       IM=1;
       for (i=0;i<NDS;i++)
	 {
	  if (pa->xs[i] != NULL && *pa->xs[i] > 0) im[i]=*pa->xs[i];
	  else im[i]=1;
	  IM=IM*im[i];
	 }
       if (pa->un.data == NULL)
	 {
	  if ((pa->un.data=(float *)malloc(IM*sizeof(float))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->un.data[i]=0.0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(IM*sizeof(short))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->mask[i]=1;
	 }

	 for (l=0;l<im[3];l++)
	   for (k=0;k<im[2];k++)
	     for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  b=v;
		  a = pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))];
		  if (pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] <= 0 ||
			 fabs(a) >= 0.99e20 || fabs(b) >= 0.99e20)
		    {
		     pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = 0;
		     pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = 1.e20;
		    }
		  else pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = a - b;
		 }
       return 1;
      }

/*			Multiply array pA into pa.			*/

    int MUL_array(struct a_attr *pa,struct a_attr *pA)
      {
       int i,j,k,l,n;
       int IX[NDS];
       int im[NDS],IM;
       int I[NDS];
       double a,b;

       if (pA->notok == 1)
	 {
	  err_warn(1,fperr,"Error - variable (%s) not ok.\n",pA->n);
	  return 0;
	 }

       for (i=0;i<NDS;i++) {I[i]=-1; IX[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the lhs and the rhs variable.
			I[i] points to the corresponding dimension.	*/

       for (n=0;n<pA->ND;n++)
	 {
	  for (i=0;i<pa->ND;i++) if (strcmp(pA->xn[n],pa->XN[i]) == 0)
	    {
	     I[i]=n;
	     break;
	    }
	  if ( (i==pa->ND && *pA->xs[n]>1) ||
		 (i<pa->ND && (*pA->xs[n]>*pa->XS[i] ||
			(*pA->xs[n]>1 && *pA->xs[n]!=*pa->XS[i]))) )
	    {
	     err_warn(1,fperr,"Error - variable (%s) won't add.\n",pA->n);
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I[i] < 0 || *pA->xs[I[i]] == 1) IX[i] = 0;
	  else
	     for (n=0;n<I[i];n++) IX[i]=*pA->xs[n]*IX[i];
	 }

       if (pa->un.data == NULL)
	 {
	  if ((pa->un.data=(float *)malloc(IM*sizeof(float))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->un.data[i]=0.0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(IM*sizeof(short))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->mask[i]=1;
	 }

	 for (l=0;l<im[3];l++)
	   for (k=0;k<im[2];k++)
	     for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))];
		  b = pA->un.data[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]];
		  if (pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] <= 0 ||
			pA->mask[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]] <= 0 ||
			 fabs(a) >= 0.99e20 || fabs(b) >= 0.99e20)
		    {
		     pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = 0;
		     pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = 1.e20;
		    }
		  else pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = a * b;

		 }
       return 1;
      }

/*			Multiply scalar v into pa.			*/

    int MUL_scalar(struct a_attr *pa,float v)
      {
       int i,j,k,l;
       int im[NDS],IM;
       float a,b;

       IM=1;
       for (i=0;i<NDS;i++)
	 {
	  if (pa->xs[i] != NULL && *pa->xs[i] > 0) im[i]=*pa->xs[i];
	  else im[i]=1;
	  IM=IM*im[i];
	 }
       if (pa->un.data == NULL)
	 {
	  if ((pa->un.data=(float *)malloc(IM*sizeof(float))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->un.data[i]=0.0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(IM*sizeof(short))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->mask[i]=1;
	 }

	 for (l=0;l<im[3];l++)
	   for (k=0;k<im[2];k++)
	     for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))];
		  b = v;
		  if (pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] <= 0 ||
			 fabs(a) >= 0.99e20 || fabs(b) >= 0.99e20)
		    {
		     pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = 0;
		     pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = 1.e20;
		    }
		  else pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = a * b;
		 }
       return 1;
      }

/*			Divide array pA into pa.			*/

    int DIV_array(struct a_attr *pa,struct a_attr *pA)
      {
       int i,j,k,l,n;
       int IX[NDS];
       int im[NDS],IM;
       int I[NDS];
       double a,b;

       if (pA->notok == 1)
	 {
	  err_warn(1,fperr,"Error - variable (%s) not ok.\n",pA->n);
	  return 0;
	 }

       for (i=0;i<NDS;i++) {I[i]=-1; IX[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the lhs and the rhs variable.
			I[i] points to the corresponding dimension.	*/

       for (n=0;n<pA->ND;n++)
	 {
	  for (i=0;i<pa->ND;i++) if (strcmp(pA->xn[n],pa->XN[i]) == 0)
	    {
	     I[i]=n;
	     break;
	    }
	  if ( (i==pa->ND && *pA->xs[n]>1) ||
		 (i<pa->ND && (*pA->xs[n]>*pa->XS[i] ||
			(*pA->xs[n]>1 && *pA->xs[n]!=*pa->XS[i]))) )
	    {
	     err_warn(1,fperr,"Error - variable (%s) won't add.\n",pA->n);
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I[i] < 0 || *pA->xs[I[i]] == 1) IX[i] = 0;
	  else
	     for (n=0;n<I[i];n++) IX[i]=*pA->xs[n]*IX[i];
	 }

       if (pa->un.data == NULL)
	 {
	  if ((pa->un.data=(float *)malloc(IM*sizeof(float))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->un.data[i]=0.0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(IM*sizeof(short))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->mask[i]=1;
	 }

	 for (l=0;l<im[3];l++)
	   for (k=0;k<im[2];k++)
	     for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))];
		  b = pA->un.data[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]];
		  if (pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] <= 0 ||
			pA->mask[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]] <= 0 ||
			 fabs(b) == 0.0 || fabs(b)*1.e10 < fabs(a) )
		    {
		     pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = 0;
		     pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = 1.e20;
		    }
		  else pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = a / b;

		 }
       return 1;
      }

/*			Divide scalar v into pa and store in pa.	*/

    int DIV_scalar(struct a_attr *pa,float v)
      {
       int i,j,k,l;
       int im[NDS],IM;
       float a,b;

       IM=1;
       for (i=0;i<NDS;i++)
	 {
	  if (pa->xs[i] != NULL && *pa->xs[i] > 0) im[i]=*pa->xs[i];
	  else im[i]=1;
	  IM=IM*im[i];
	 }
       if (pa->un.data == NULL)
	 {
	  if ((pa->un.data=(float *)malloc(IM*sizeof(float))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->un.data[i]=0.0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(IM*sizeof(short))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->mask[i]=1;
	 }

	 for (l=0;l<im[3];l++)
	   for (k=0;k<im[2];k++)
	     for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  a = pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))];
		  b = v;
		  if (pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] <= 0 ||
			 fabs(b) == 0.0 || fabs(b)*1.e10 < fabs(a))
		    {
		     pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = 0;
		     pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = 1.e20;
		    }
		  else pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = a / b;
		 }
       return 1;
      }


/*			Square root and store array pA into pa.		*/

    int SQRT_array(struct a_attr *pa,struct a_attr *pA)
      {
       int i,j,k,l,n;
       int IX[NDS];
       int im[NDS],IM;
       int I[NDS];
       double a,b;

       if (pA->notok == 1)
	 {
	  err_warn(1,fperr,"Error - variable (%s) not ok.\n",pA->n);
	  return 0;
	 }

       for (i=0;i<NDS;i++) {I[i]=-1; IX[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the lhs and the rhs variable.
			I[i] points to the corresponding dimension.	*/

       for (n=0;n<pA->ND;n++)
	 {
	  for (i=0;i<pa->ND;i++) if (strcmp(pA->xn[n],pa->XN[i]) == 0)
	    {
	     I[i]=n;
	     break;
	    }
	  if ( (i==pa->ND && *pA->xs[n]>1) ||
		 (i<pa->ND && (*pA->xs[n]>*pa->XS[i] ||
			(*pA->xs[n]>1 && *pA->xs[n]!=*pa->XS[i]))) )
	    {
	     err_warn(1,fperr,"Error - variable (%s) not compatable.\n",pA->n);
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I[i] < 0 || *pA->xs[I[i]] == 1) IX[i] = 0;
	  else
	     for (n=0;n<I[i];n++) IX[i]=*pA->xs[n]*IX[i];
	 }

       if (pa->un.data == NULL)
	 {
	  if ((pa->un.data=(float *)malloc(IM*sizeof(float))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->un.data[i]=0.0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(IM*sizeof(short))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->mask[i]=1;
	 }

	 for (l=0;l<im[3];l++)
	   for (k=0;k<im[2];k++)
	     for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  b = pA->un.data[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]];
		  if (pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] <= 0 ||
			pA->mask[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]] <= 0 ||
			 fabs(b) >=0.99e20 || b < 0.0 || 
		     (a=sqrt(b)) < 0.0 || errno == EDOM || errno == ERANGE)
		    {
		     pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = 0;
		     a = 1.e20;
		    }
		  pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = a;
		 }
       return 1;
      }

/*			Square root scalar v and store into pa.		*/

    int SQRT_scalar(struct a_attr *pa,float v)
      {
       int i,j,k,l;
       int im[NDS],IM;
       double a,b;

       IM=1;
       for (i=0;i<NDS;i++)
	 {
	  if (pa->xs[i] != NULL && *pa->xs[i] > 0) im[i]=*pa->xs[i];
	  else im[i]=1;
	  IM=IM*im[i];
	 }
       if (pa->un.data == NULL)
	 {
	  if ((pa->un.data=(float *)malloc(IM*sizeof(float))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->un.data[i]=0.0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(IM*sizeof(short))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->mask[i]=1;
	 }

	 for (l=0;l<im[3];l++)
	   for (k=0;k<im[2];k++)
	     for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {

		  b=v;
		  if (pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] <= 0 ||
			 fabs(b) >=0.99e20 || b < 0.0 || 
		     (a=sqrt(b)) < 0.0 || errno == EDOM || errno == ERANGE)
		    {
		     pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = 0;
		     a = 1.e20;
		    }
		  pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = a;
		 }
       return 1;
      }


/*			Exponentiate and store array pA into pa.	*/

    int POWER_array(struct a_attr *pa,struct a_attr *pA,float v)
      {
       int i,j,k,l,n;
       int IX[NDS];
       int im[NDS],IM;
       int I[NDS];
       double a,b;

       if (pA->notok == 1)
	 {
	  err_warn(1,fperr,"Error - variable (%s) not ok.\n",pA->n);
	  return 0;
	 }

       for (i=0;i<NDS;i++) {I[i]=-1; IX[i]=1; im[i]=1;}

/*			Find the relationship between the dimensions
			of the lhs and the rhs variable.
			I[i] points to the corresponding dimension.	*/

       for (n=0;n<pA->ND;n++)
	 {
	  for (i=0;i<pa->ND;i++) if (strcmp(pA->xn[n],pa->XN[i]) == 0)
	    {
	     I[i]=n;
	     break;
	    }
	  if ( (i==pa->ND && *pA->xs[n]>1) ||
		 (i<pa->ND && (*pA->xs[n]>*pa->XS[i] ||
			(*pA->xs[n]>1 && *pA->xs[n]!=*pa->XS[i]))) )
	    {
	     err_warn(1,fperr,"Error - variable (%s) won't add.\n",pA->n);
	     return 0;
	    }
	 }
       IM=1;
       for (i=0;i<pa->ND;i++)
	 {
	  if ((im[i]=*pa->xs[i]) > 0) IM=IM*im[i];
	  if (I[i] < 0 || *pA->xs[I[i]] == 1) IX[i] = 0;
	  else
	     for (n=0;n<I[i];n++) IX[i]=*pA->xs[n]*IX[i];
	 }

       if (pa->un.data == NULL)
	 {
	  if ((pa->un.data=(float *)malloc(IM*sizeof(float))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->un.data[i]=0.0;
	 }
       if (pa->mask == NULL)
	 {
	  if ((pa->mask=(short *)malloc(IM*sizeof(short))) == NULL)
	    {
	     err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	     return 0;
	    }
	  for (i=0;i<IM;i++) pa->mask[i]=1;
	 }

	 for (l=0;l<im[3];l++)
	   for (k=0;k<im[2];k++)
	     for (j=0;j<im[1];j++)
	       for (i=0;i<im[0];i++)
		 {
		  b = pA->un.data[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]];
		  if (pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] <= 0 ||
			pA->mask[i*IX[0]+j*IX[1]+k*IX[2]+l*IX[3]] <= 0 ||
			 fabs(b) >= 0.99e20 || fabs(v) >= 0.99e20 || 
		     fabs(a=pow(b,v)) >= 0.99e20 || errno == EDOM ||
			     errno == ERANGE)
		    {
		     pa->mask[i+im[0]*(j+im[1]*(k+im[2]*l))] = 0;
		     a = 1.e20;
		    }
		  pa->un.data[i+im[0]*(j+im[1]*(k+im[2]*l))] = a;

		 }
       return 1;
      }

/*			Compute the MEAN of specified dimensions
			of the given array into pa.		*/

    int MEAN(struct a_attr *pa,struct argmnt *parg)
      {
       int i,j,k,l,n,nd;
       int ix[NDS],IX,Lx,Kx,Jx,Ix;
       int im[NDS],IM,Lm,Km,Jm,Im;

       float *paw,w,w1,w2,w3;
       double a,b;
       struct a_attr *pA;
       struct a_tab *ptab;
	

       if (parg[0].ok == 0)
	 {
	  err_warn(1,fperr,
		"Error - first argument (%s) for MEAN not ok.\n",parg[0].name);
	  return 0;
	 }
       if (parg[0].paa == NULL)
	 {
	  err_warn(1,fperr,
	   "Error - first argument (%s) for MEAN is not data.\n",parg[0].name);
	 }
       pA=parg[0].paa;

       for (i=0;i<NDS;i++) {ix[i]=im[i]=1;}

/*			Find the relationship between the dimensions
			of the lhs and the rhs variable.

			Compute the im[] and ix[] for pA and pa indices.*/

       IM=IX=1;
       for (n=0;n<pA->ND;n++)
	 {
	  im[n]=*pA->xs[n];
	  IM*=im[n];
	  for (i=0;i<pa->ND;i++)
	    {
	     if (strcmp(pa->xn[i],pA->XN[n]) == 0)
	       {
		IX*=*pa->xs[i];
	        break;
	       }
	     else ix[n]*=*pa->xs[i];
	    }

	  if (i < pa->ND && *pa->xs[i] != *pA->XS[n])
	    {
	     err_warn(1,fperr,
		"Error - variable (%s) won't work for MEAN.\n",pA->n);
	     return 0;
	    }

	  if (i == pa->ND) ix[n]=0;
	 }

       if (IX == 0)
	 {
	  err_warn(1,fperr,
	    "Error - no dimensions for (%s). You cannot eliminate all"
	    " dimensions with MEAN.\n",pa->n);
	  return 0;
	 }

       if (pa->un.data != NULL) free ((char *)pa->un.data);
       if ((pa->un.data=(float *)malloc(IX*sizeof(float))) == NULL)
	 {
	  err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	  return 0;
	 }
       if ((paw=(float *)malloc(IX*sizeof(float))) == NULL)
	 {
	  err_warn(1,fperr,
		"Error - memory not available for weights (%s).\n",pa->n);
	  return 0;
	 }
       for (i=0;i<IX;i++) pa->un.data[i]=0.0;
       for (i=0;i<IX;i++) paw[i]=0.0;

       if (pa->mask != NULL) free((char *)pa->mask);
       if ((pa->mask=(short *)malloc(IX*sizeof(short))) == NULL)
	 {
	  err_warn(1,fperr,"Error - memory not available (%s).\n",pa->n);
	  return 0;
	 }
       for (i=0;i<IX;i++) pa->mask[i]=1;

       for (ptab=&A_tmp;
		 ptab!=NULL && strcmp(parg[0].name,ptab->name) != 0;
							 ptab=ptab->next);
       if (ptab == NULL)
	 {
	  err_warn(1,fperr,
		"Error - no array table entry for computing element.\n");
	  pa->notok=1;
	  return 0;
	 }
/*       if (computer(parg[0].pdf,ptab->pA_attr) == 0)
	 {
	  err_warn(1,fperr,
		"Error - argument for MEAN doesn't compute.\n");
	  pa->notok=1;
	  return 0;
	 }
*/

       for (l=0;l<im[3];l++)
	 {
	  Lm=im[2]*l;
	  Lx=ix[3]*l;
	  if (pA->xs[3] != NULL && *pA->xs[3] >1) w3=fabs(pA->xw[3][l]);
	  else w3=1.0;
	  for (k=0;k<im[2];k++)
	    {
	     Km=im[1]*(k+Lm);
	     Kx=ix[2]*k+Lx;
	     if (pA->xs[2] != NULL && *pA->xs[2] > 1) w2=fabs(pA->xw[2][k])*w3;
	     else w2=1.0;
	     for (j=0;j<im[1];j++)
	       {
		Jm=im[0]*(j+Km);
		Jx=ix[1]*j+Kx;
		if (pA->xs[1]!=NULL && *pA->xs[1]>1) w1=fabs(pA->xw[1][j])*w2;
		else w1=1.0;
	        for (i=0;i<im[0];i++)
		  {
		   Im=i+Jm;
		   Ix=ix[0]*i+Jx;
		   if (pA->xs[0]!=NULL && *pA->xs[0]>1) w=fabs(pA->xw[0][i])*w1;
		   else w=1.0;

		   a = pA->un.data[Im];
		   if (pa->mask[Ix]<=0 || pA->mask[Im]<=0 || fabs(a)>=0.99e20 )
		     {
		      a = 0.0;
		      w = 0.0;
		     }
		   pa->un.data[Ix] = pa->un.data[Ix]+a*w;
		   paw[Ix] = paw[Ix]+w;
		  }
	       }
	    }
	 }
       for (i=0;i<IX;i++)
	 {
	   if (paw[i] > 0.0)
	     pa->un.data[i] = pa->un.data[i] / paw[i];
	   else
	     {
	      pa->un.data[i] = 1.e20;
	      pa->mask[i]=0;
	     }
	 }
       free ((char *)paw);
       return 1;
      }
