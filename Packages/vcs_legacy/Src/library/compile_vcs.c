#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <stdarg.h>
#include "array.h"

#define NFUNC 2
#define MAXELEM 120

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
      } funcs[NFUNC]={"sqrt",1,"mean",MAXARGS};				*/


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

   struct data_flo *get_init_flo();
   int add_it(struct parsed **psr,struct data_flo *pd,char *name);
   int err_warn (int beep,FILE *fp,char *fmt,...);
   char *repstr(char *s2,char *s1);

/*			Compile an assignment statement into a linked
			data-flo path.					*/

    struct data_flo *compile_vcs_legacy (struct a_tab *ptab)

      {
       int k;
       struct parsed pars[MAXELEM+1],*psr;
       struct data_flo *pd;
       char *name;		/*The array table name.			*/
       struct a_attr *pa;	/*Pointer to the array attribute structure.*/


       psr=&pars[0];

	for (k=0;k<=MAXELEM;k++)
	  {
	   pars[k].str[0]='\0';
	   pars[k].tok1=0;
	   pars[k].tok2=0;
	   pars[k].v=1.e20;
	  }
       pa=ptab->pA_attr;
       name=ptab->name;

       if (pa->f == NULL)
	 {
	  err_warn(1,fperr,"Error - no equation given for (A_%s)\n",name);
	  pa->notok=1;
	  return NULL;
	 }
       if ((k=pars_eq(pa->f,psr,ptab->name)) == 0)
	 {
	  err_warn(1,fperr,"Error - equation doesn't parse for (A_%s)\n",name);
	  pa->notok=1;
	  return NULL;
	 }

       /* Set the actual name of the data to the new variable name */
       pa->aN=repstr(pa->aN,name);
       pa->n=repstr(pa->n,name);

       if ((pd=get_init_flo()) == NULL) return NULL;
       pd->rtrn.paa=pa;
       strcpy(pd->rtrn.name,name);

       if ( (add_it(&psr,pd,name)) == 0)
	 {
/*	  killA_tmp();						*/
	  clear_data_flo(&pd);
	  err_warn(1,fperr,"Error - catastrophic in compiling (A_%s).\n",name);
	  pa->notok=1;
	  return NULL;
	 }
       if (set_dim(pd,pa) == 0)
	 {
	  err_warn(1,fperr,
		  "Error - setting dimensions for assignment of A_%s.\n",name);
/*	  killA_tmp();							*/
/*			This is already done in set_dim.		*/
/*	  killA_tmp();							*/
	  clear_data_flo(&pd);						
	  pa->notok=1;
	  return NULL;
	 }
       return pd;
      }

/*			Handle an add.					*/


    int add_it(struct parsed **psr,struct data_flo *pdm,char *name)

      {
       int c,d;
       struct data_flo *pd;
       struct a_attr *pa;

       pa=pdm->rtrn.paa; /*	Needed to check dimensions.		*/

       while ( (c=(*psr)->tok1)=='+' || c=='-')
	 {
	  if (pdm->func[0] != '\0')
	    {
	     if ( (pd=get_init_flo()) == NULL) return 0;
	     pd->rtrn.paa=pdm->rtrn.paa;
	     strcpy(pd->rtrn.name,pdm->rtrn.name);
	     pdm->next=pd;
	     pdm=pd;
	    }

	  if (c == '+') strcpy(pdm->func,"ADD");
	  else strcpy(pdm->func,"SUB");

	  d=(*psr)->tok2;

/*			Is it a single added term?			*/

	  if (d == '+' || d == '-' || d == 0)
	    {
	     if (setarg_AorV(pdm,psr,pa) == 0) return 0;
	     if (d == 0) return 1;
	     (*psr)++;
	     continue;
	    }

/*			Now handle anything but + or -.			*/

	  while ((d=(*psr)->tok2) != '+' && d != '-')
	    {
/*			Is the following a power?			*/

	     if (d == '^')
	       {
		if ((*psr)->str[0] != '\0')
		     if (setarg_AorV(pdm,psr,pa) == 0) return 0;
		if (power_it(psr,pdm)== 0) return 0;
		d=(*psr)->tok2;
	        if (d == 0) return 1;
		if (d == '+' || d == '-') continue;
	       }
/*	 		Does it have a multiplier?			*/

	     if (d == '*' || d == '/')
	       {
		if ((*psr)->str[0] != '\0')
		  if (setarg_AorV(pdm,psr,pa) == 0) return 0;
		if (mul_it(psr,pdm)==0) return 0;
		d=(*psr)->tok2;
		if (d == 0) return 1;
		if (d == '+' || d == '-') break;
		if (d == ')') return d;
	       }
/*			Is it a function call?				*/

	     else if ((*psr)->str[0] != '\0' && d == '(')
	       {
		if (func_it(psr,pdm) == 0) return 0;
		d=(*psr)->tok2;
		if (d == 0) return 1;
		if (d == '+' || d == '-') break;
	       }
/*			Is it a parenthetical expression?		*/

	     else if (d == '(')
	       {
		if (paren_it(psr,pdm) == 0) return 0;
		d=(*psr)->tok2;
		if (d == 0) return 1;
		if (d == '+' || d == '-') break;
	       }
	     else if (d == ')')
	       {
		if ( ((c=(*psr)->tok1)=='+' || c=='-') && (*psr)->str!='\0')
		    if (setarg_AorV (pdm,psr,pa) == 0) return 0;
		return d;
	       }
	     else if (d == ',')
	       {
		if ( ((c=(*psr)->tok1)=='+' || c=='-') && (*psr)->str!='\0')
		    if (setarg_AorV (pdm,psr,pa) == 0) return 0;
		return d;
	       }
	     if (d == 0)
	       {
/*		if ((*psr)->str[0]!='\0'&&setarg_AorV(pdm,psr,pa)==0) return 0;
*/
	        return 1;
	       }
	     (*psr)++;
	    }
	  (*psr)++;
	 }

       return 1;
      }


/*		Handle a multiply.					*/

    int mul_it(struct parsed **psr,struct data_flo *pdm)

      {
       int c,d;
       struct data_flo *pd,*pdd;
       struct a_attr *pa;

	if ((pd=get_init_flo()) == NULL) return 0;

	if (setmp_A(&A_tmp,&pa,pd->rtrn.name) == 0) return 0;

	pd->rtrn.paa=pa;
	strcpy(pd->func,"ADD");
	pd->argm[0].paa=pdm->argm[0].paa;
	strcpy(pd->argm[0].name,pdm->argm[0].name);
	pd->argm[0].pdf=pdm->argm[0].pdf;
	pd->argm[0].v=pdm->argm[0].v;


	pdm->argm[0].pdf=pd;
	pdm->argm[0].paa=pd->rtrn.paa;
	strcpy(pdm->argm[0].name,pd->rtrn.name);
	pdm->argm[0].v=1.e20;
	
	(*psr)++;

       while ( (c=(*psr)->tok1) == '*' || c == '/')
	 {
	  if ((pdd=pd->next=get_init_flo()) == NULL) return 0;
	  strcpy(pdd->rtrn.name,pd->rtrn.name);
	  pdd->rtrn.paa=pd->rtrn.paa;
	  pd=pd->next;
	  d=(*psr)->tok2;
	  if (c == '*') strcpy(pd->func,"MUL");
	  else strcpy(pd->func,"DIV");

/*			Is the following a power?			*/

	  if (d == '^')
	    {
	     if ((*psr)->str[0] != '\0')
	          if (setarg_AorV(pd,psr,pa) == 0) return 0;
	     if (power_it(psr,pd) == 0) return 0;
	     d=(*psr)->tok2;
	    }

          else if (d == '+' || d == '-' || d == 0)
	    {
	     if (setarg_AorV(pd,psr,pa) == 0) return 0;
	     return 1;
	    }

/*			Does it have a multiplier?			*/

	  else if (d == '*' || d == '/')
	    {
             if (setarg_AorV (pd,psr,pa) == 0) return 0;
	    }
/*			Is it a function call?				*/

	  else if ((*psr)->str[0] != '\0' && d == '(')
	    {
	     if (func_it(psr,pd) == 0) return 0;
	     (*psr)++;
	     d=(*psr)->tok2;
	     if (d == 0) return 1;
	    }
/*			Is it a parenthetical expression?		*/

	  else if (d == '(')
	    {
	     if (paren_it(psr,pd) == 0) return 0;
	     (*psr)++;
	     d=(*psr)->tok2;
	     if (d == 0) return 1;
	    }
	  else if (d == ')')
	    {
	     if (setarg_AorV (pd,psr,pa) == 0) return 0;
	     return d;
	    }
	  else if (d == ',')
	    {
	     if (setarg_AorV (pd,psr,pa) == 0) return 0;
	     return d;
	    }
	  if (d == '*' || d == '/') (*psr)++;
	 }

       return 1;
      }

/*			Parse an assignment string.			*/

    int pars_eq(char str[],struct parsed p[],char name[])
      {
	int i,j,k,c,d;
	int m,n;
	char s[1025];
	struct a_tab *ptab;

	for (j=0,i=0;i<1024&&str[i]!='\0';i++)
	  {
	   if (str[i]>' ') s[j++]=str[i];
	  }
	s[j]='\0';
	if (j == 0) return 0;

	i=0;
	if (eqtok(c=s[0])&&(c=='+'||c=='-')) {p[0].tok1=c; i++;}
	else p[0].tok1='+';
	p[0].v=1.e20;
	p[0].str[0]='\0';
	p[0].tok2=0;

	for (k=0,j=0;i < 1024 && (c=s[i]) != '\0';i++)
	  {

	   if (!eqtok(c))
	     {
	      if (c > ' ' && j < 18)
		{
		 p[k].str[j++]=c;
	         p[k].str[j]='\0';
		}
	      else if (j > 18)
		{
		 err_warn(1,fperr,"Error - syntax (%s)\n",s);
		 return 0;
		}
	      continue;
	     }

/*			Handle tokens.					*/

	   if (j > 0 && isnum(p[k].str)) sscanf(p[k].str,"%g",&p[k].v);
	   j=0;
	   if (c == '(' || c == ',')
	     {
	      p[k].tok2=c;
	      if (++k >= MAXELEM)
		{
		 err_warn(1,fperr,"Error - table overflow (%s)\n",s);
		 return 0;
		}
	      p[k].tok1=c;
	      p[k].v=1.e20;
	      p[k].str[0]='\0';
	      j=0;
	      if (eqtok(c=s[i+1]))
	        {
	         i++;
		 p[k].tok2=c;
		 if (++k >= MAXELEM)
		   {
		    err_warn(1,fperr,"Error - table overflow (%s)\n",s);
		    return 0;
		   }
		 p[k].tok1=c;
		}
	      else
		{
		 p[k].tok2='+';
		 if (++k >= MAXELEM)
		   {
		    err_warn(1,fperr,"Error - table overflow (%s)\n",s);
		    return 0;
		   }
		 p[k].tok1='+';
		}
	     }
	   else if (c == '*' && s[i+1] == '*')
	     {
	      i++;
	      p[k].tok2='^';
	      if (++k >= MAXELEM)
		{
		 err_warn(1,fperr,"Error - table overflow (%s)\n",s);
		 return 0;
		}
	      p[k].tok1=p[k-1].tok2;
	     }
	   else
	     {
	      p[k].tok2=c;
	      if (++k >= MAXELEM)
		{
		 err_warn(1,fperr,"Error - table overflow (%s)\n",s);
		 return 0;
		}
	      p[k].tok1=c;
	     }
	   p[k].v=1.e20;
	   p[k].str[0]='\0';
	   p[k].tok2=0;
	  }		/*  End of 'for' loop.				*/

	if (p[k].str[0] == '\0' && p[k].tok1 != ')')
	  {
	   err_warn(1,fperr,"Error - equation is incomplete (%s)\n",s);
	   return 0;
	  }
	if (isnum(p[k].str)) sscanf(p[k].str,"%g",&p[k].v);
	k++;
	p[k].tok1=0;
/*			Check too many operators.			*/

	for (m=n=i=0;i<k;i++)
	  {
	   c=p[i].tok1;
	   d=p[i].tok2;
	   if ( p[i].str[0]=='\0' &&
		(c=='+'||c=='-'||c=='*'||c=='/'||c=='^') &&
		(d=='+'||d=='-'||d=='*'||d=='/'||d=='^') )
	     {
	      err_warn(1,fperr,
		"Error - syntax, multiple sequential operators.\n %s\n",s);
	      return 0;
	     }
	  }
/*			Check variable and function names.		*/
	for (m=n=i=0;i<k;i++)
	  {	
	   if (strlen(p[i].str) > (size_t) 0 && !isnum(p[i].str))
	     {
	      if (p[i].tok2 != '(')
		{
	         for (ptab=&A_tab;
			ptab!=NULL && cmpnbl(ptab->name,p[i].str)!=0;
				ptab=ptab->next);
		 if (cmpnbl(name,p[i].str) == 0)
		   {
		    err_warn(1,fperr,
			"Error - (A_%s) cannot be a function of itself (%s).\n",
				name,str);
			free(ptab->pA_attr->f);
			ptab->pA_attr->f = NULL;
		    return 0;
		   }
/*			Allow strings that are enclosed within quotes,
			or at least begin with quotes.			*/

	         if (ptab == NULL && (p[i].str[0]!='\"' && p[i].str[0]!='\''))
		   {
		    err_warn(1,fperr,
		      "Error - variable name (%s) doesn't exist.\n",p[i].str);
		    return 0;
		   }
		}
	      else if (p[i].tok2 == '(')
		{
		 for (j=0;j<NFUNC && cmpncs(funcs[j].name,p[i].str) != 0;j++);
		 if (j == NFUNC)
		   {
		    err_warn(1,fperr,
		      "Error - function name (%s) doesn't exist.\n",p[i].str);
		    return 0;
		   }
		}
	     }
	  }
/*			Check parentheses.				*/
	for (m=n=i=0;i<k;i++)
	  {	
	   if (p[i].tok1 == '(') m++;
	   else if (p[i].tok1 == ')') m--;
	   if (p[i].tok2 == '(') n++;
	   else if (p[i].tok2 == ')') n--;
	   if (m < 0 || n < 0)
	     {
	      err_warn(1,fperr,
		"Error - syntax, unbalanced parentheses.\n %s\n",s);
	      return 0;
	     }
	  }
	if (m != 0 || n != 0)
	  {
	   err_warn(1,fperr,
		"Error - syntax, unbalanced parentheses.\n %s\n",s);
	   return 0;
	  }
/*			Optimize for space.				*/
	for (m=n=i=0;i<k;i++)
	  {
	   
	  }
	return k;
	
      }

/*		Tokens are defined to be +*-/(),  			*/

    int eqtok (int c)
/*	Test whether it is a token or not and return T (1) or F (0).	*/
      {
	if (c=='+'||c=='-'||c=='*'||c=='/'||c=='('||c==')'||c==',') return 1;
	return 0;
      }


   struct data_flo *get_init_flo()
      {
       int i;
       struct data_flo *pd;

       if ((pd=(struct data_flo *)malloc(sizeof(struct data_flo)))==NULL)
	 {
	  err_warn(1,fperr,"Error - memory for computation not found.\n");
	  return pd;
	 }
       pd->prev=NULL;
       pd->next=NULL;
       pd->rtrn.name[0]='\0';
       pd->rtrn.ok=0;
       pd->rtrn.v=1.e20;
       pd->rtrn.s=NULL;
       pd->rtrn.paa=NULL;
       pd->rtrn.pdf=NULL;
       pd->func[0]='\0';
       for (i=0;i<MAXARGS;i++)
	 {
	  pd->argm[i].name[0]='\0';
	  pd->argm[i].ok=0;
	  pd->argm[i].v=1.e20;
	  pd->argm[i].s=NULL;
	  pd->argm[i].paa=NULL;
	  pd->argm[i].pdf=NULL;
	 }
       return pd;
      }

/*			Check and set dimensions for computed array
			against actual data used in the computation.	*/

    int check_dimens (struct a_attr *pa,struct a_attr *pA)
      {
       if (pa->ND == 0) set_copyA(pa,pA);
       return 1;
      }
/*			Check and set dimensions for computed array
			against actual data used in the computation.	*/

    int set_copyA (struct a_attr *pa,struct a_attr *pA)
      {
       int i,j,k;
       int ND,nd;
       int I[NDS];
       float dx;
       char **pS;
       char **pSs;
       char **ps;

       pS=&(pa->S);
       pSs=&(pa->s);
       ps=&(pA->s);

/*			Loop over a_attr from pa.S to pa.s and move
			naming strings to those that don't already have
			naming strings.					*/

       while (pS != &(pa->s)) 
	 {
/*
	  if (*ps != NULL)
	    {
	     if (*pS == NULL) *pS=repstr(*pS,*ps);
	     if (*pSs == NULL) *pSs=repstr(*pSs,*ps);
	    }
	  else
	    {
*/
	     if (*pS == NULL) *pS=repstr(*pS,"");
	     if (*pSs == NULL) *pSs=repstr(*pSs,"");

/*	    }
*/
	  pS++;
	  pSs++;
	  ps++;
	 }
/*			Dimension order may be changed, but all
			non-singular dimensions must be specified
			and all previous dimensions must be specified.
			For example, if dimension 2 is specified then
			dimension 1 must be also.

			Lower case dimension names from the R.H.S.
			are used in the assignment.			*/

       for (i=0;i<NDS;i++) I[i]=0;
       ND=pA->ND;
       nd=0;
/*			Check how many dimension names are assigned
			and make sure all previous are assigned.	*/
       for (i=0;i<NDS;i++)
	 {
	  if (pa->XN[i] != NULL)
	    {
	     if (nd != i)
	       {
		err_warn(1,fperr,
		  "Error - previous dimension names must be specified (%s).\n",
					pa->XN[i]);
		return 0;
	       }
	     else nd++;
	    }
	 }
/*			Check multi-valued dimensions are assigned
			if any have been.  Set an array of indices
			pointing to corresponding dimensions for
			later use.					*/

       for (i=0;i<ND;i++)
	 {
	  I[i]=NDS;
	  for (j=0;j<nd;j++)
	    {
	     if(pa->XN[j] != NULL && strcmp(pA->xn[i],pa->XN[j]) == 0)
	       {
		if (pa->XS[j] != NULL && *pA->xs[i] != *pa->XS[j])
		  {
		   err_warn(1,fperr,"Error - dimension sizes must agree (%s)\n",
				pA->xn[i]);
		   return 0;
		  }
		if (pa->XS[j] != NULL && *pa->XS[j] > 1 && pa->XV[j] != NULL)
		  {
		   dx=(0.01*fabs(pa->XV[j][*pa->XS[j]-1]-pa->XV[j][0])) /
								(*pa->XS[j]-1);
		   for (k=0;k<*pa->XS[j];k++)
		      if (fabs(pA->xv[i][k]-pa->XV[j][k]) > dx)
			{
			 err_warn(1,fperr,
			    "Error - dimension values must agree (%s)\n",
				pA->xn[i]);
			 return 0;
			}
		  }
		I[i]=j;
		break;
	       }
	    }
	  if (nd > 0 && *pA->xs[i] > 1 && I[i] == NDS)
	    {
	     err_warn(1,fperr,
		 "Error - all non-singular dimensions must be specified (%s)\n",
				pA->xn[i]);
	     return 0;
	    }
	 }
/*			Fill in non-assigned index values in the array of
			indices.					*/

       for (k=i=0;i<ND;i++)
	 {
	  while (I[i] == NDS)
	    {
	     for (j=0;j<ND;j++)
		{
		 if (I[j] == k) {k++; break;}
		}
	     if (j == ND) I[i]=k++;
	    }
	 }

       for (i=0;i<ND && pA->xn[i] != NULL;i++)
	 {
	  pa->XN[I[i]]=repstr(pa->XN[I[i]],pA->xn[i]);
	  pa->XU[I[i]]=repstr(pa->XU[I[i]],pA->xu[i]);
	  pa->xn[I[i]]=repstr(pa->xn[I[i]],pA->xn[i]);
	  pa->xu[I[i]]=repstr(pa->xu[I[i]],pA->xu[i]);
	  if (pa->XS[I[i]]==NULL && 
		(pa->XS[I[i]]=(int *)malloc(sizeof(int)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  k=*pa->XS[I[i]]=*pA->xs[i];
	  if (pa->XV[I[i]] != NULL) free((char *)pa->XV[I[i]]);
	  if (pa->XB[I[i]] != NULL) free((char *)pa->XB[I[i]]);
	  if (pa->XW[I[i]] != NULL) free((char *)pa->XW[I[i]]);
	  if( (pa->XV[I[i]]=(float *)malloc(k*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  if( (pa->XB[I[i]]=(float *)malloc((k+1)*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  if( (pa->XW[I[i]]=(float *)malloc(k*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  for (j=0;j<k;j++)
	    {
	     *(pa->XV[I[i]]+j)=*(pA->xv[i]+j);
	     *(pa->XB[I[i]]+j)=*(pA->xb[i]+j);
	     *(pa->XW[I[i]]+j)=*(pA->xw[i]+j);
	    }
	  *(pa->XB[I[i]]+k)=*(pA->xb[i]+k);

	  if (pa->XK[I[i]]==NULL && 
		(pa->XK[I[i]]=(int *)malloc(sizeof(int)*2))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
/*	  *pa->XK[I[i]]=*(pA->XK[i]);				*/
	  *pa->XK[I[i]]=*(pa->XK[I[i]]+1)=0;

	  if (pa->XC[I[i]]==NULL && 
		(pa->XC[I[i]]=(float *)malloc(sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
/*	  *pa->XC[I[i]]=*(pA->XC[i]);				*/
	  *pa->XC[I[i]]=0.0;

	  if (pa->XF[I[i]]==NULL && 
		(pa->XF[I[i]]=(float *)malloc(sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  *pa->XF[I[i]]=*(pA->xv[i]);

	  if (pa->XL[I[i]]==NULL && 
		(pa->XL[I[i]]=(float *)malloc(sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  *pa->XL[I[i]]=*(pA->xv[i]+k-1);


	  if (pa->xs[I[i]]==NULL && 
		(pa->xs[I[i]]=(int *)malloc(sizeof(int)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  k=*pa->xs[I[i]]=*pA->xs[i];
	  if (pa->xv[I[i]] != NULL) free((char *)pa->xv[I[i]]);
	  if (pa->xb[I[i]] != NULL) free((char *)pa->xb[I[i]]);
	  if (pa->xw[I[i]] != NULL) free((char *)pa->xw[I[i]]);
	  if( (pa->xv[I[i]]=(float *)malloc(k*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  if( (pa->xb[I[i]]=(float *)malloc((k+1)*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  if( (pa->xw[I[i]]=(float *)malloc(k*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  for (j=0;j<k;j++)
	    {
	     *(pa->xv[I[i]]+j)=*(pA->xv[i]+j);
	     *(pa->xb[I[i]]+j)=*(pA->xb[i]+j);
	     *(pa->xw[I[i]]+j)=*(pA->xw[i]+j);
	    }
	  *(pa->xb[I[i]]+k)=*(pA->xb[i]+k);

	  if (pa->xi[I[i]]==NULL && 
		(pa->xi[I[i]]=(int *)malloc(sizeof(int)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  *pa->xi[I[i]]=0;

	  if (pa->xj[I[i]]==NULL && 
		(pa->xj[I[i]]=(int *)malloc(sizeof(int)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  *pa->xj[I[i]]=1;

	  if (pa->xf[I[i]]==NULL && 
		(pa->xf[I[i]]=(float *)malloc(sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  *pa->xf[I[i]]=*(pA->xf[i]);

	  if (pa->xl[I[i]]==NULL && 
		(pa->xl[I[i]]=(float *)malloc(sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - (copyA) memory overflow.\n");
	     return 0;
	    }
	  *pa->xl[I[i]]=*(pA->xl[i]);
	 }
       if (ND > pa->ND) pa->ND=ND;

       return 1;
      }

/*			Clear assignments made during the compilation
			of assignments for a variable.			*/

    int clear_data_flo (struct data_flo **pdm)
      {
       int i;
       struct data_flo *pd,*pdd,*pdf;

       pd=*pdm;
       while (pd != NULL)
	 {
	  for (i=0;i<MAXARGS;i++)
	    {
	     if ((pdf=pd->argm[i].pdf) != NULL)
	       {
		clear_data_flo (&pdf);
		/*free((char *)pdf);* DNW - July 1, 1996			*/
		pd->argm[i].pdf=NULL;
	       }
	    }
	  pdd=pd;
	  pd=pd->next;
	  free ((char *) *pdm);
	  *pdm=NULL;
	 }
       return 1;
      }

/*			Handle parenthetical expression.		*/

    int paren_it (struct parsed **psr,struct data_flo *pdm)
      {
       int c;
       struct data_flo *pd;
       struct a_attr *pa;


	if ((pd=get_init_flo()) == NULL) return 0;
	
	if (setmp_A(&A_tmp,&pa,pd->rtrn.name) == 0) return 0;
	pd->rtrn.paa=pa;

	pdm->argm[0].pdf=pd;
	strcpy(pdm->argm[0].name,pd->rtrn.name);
	pdm->argm[0].paa=pa;
	(*psr)+=2;



       if ((c=(*psr)->tok1) == '+' || c == '-')
	 {
	  if ( add_it(psr,pd,pd->rtrn.name) != ')') return 0;
	 }
       else return 0;
       if ((*(psr)+1)->tok2 == '^')
	 {
	  (*psr)++;
	  if ( power_it(psr,pdm) == 0) return 0;
	 }
       return 1;
      }


/*			Handle functions.				*/

    int func_it (struct parsed **psr,struct data_flo *pdm)
      {
       int i,j,c,d,n,na;
       struct data_flo *pd,*pdd;
       struct a_attr *pa;

       for (j=0;j<NFUNC && cmpncs(funcs[j].name,(*psr)->str) != 0;j++);
       if (j == NFUNC)
	 {
	  err_warn(1,fperr,
	      "Error - function name (%s) doesn't exist.\n",(*psr)->str);
	  return 0;
	 }
       na=funcs[j].num_args;

       if ((pd=get_init_flo()) == NULL) return 0;
		
       if (setmp_A(&A_tmp,&pa,pd->rtrn.name) == 0) return 0;
       pd->rtrn.paa=pa;
       strcpy(pd->func,(*psr)->str);

       pdm->argm[0].pdf=pd;
       strcpy(pdm->argm[0].name,pd->rtrn.name);
       pdm->argm[0].paa=pa;
       (*psr)+=2;

       if ((c=(*psr)->tok1) == '+' || c == '-')
	 {
	  for (i=0;i<na;i++,(*psr)+=2)
	    {
	     if ((*psr)->str[0] == '\"' || (*psr)->str[0] == '\'')
	       {
		n=strlen((*psr)->str);
		pd->argm[i].s=(char *)malloc(n);
	        strncpy(pd->argm[i].s,&((*psr)->str[1]),n-2); pd->argm[i].s[n-2]='\0';
		pd->argm[i].ok=1;
		d=(*psr)->tok2;
	       }
	     else if ((*psr)->v != (float)1.e20)
	       {
		pd->argm[i].v=(*psr)->v;
		pd->argm[i].ok=1;
		d=(*psr)->tok2;
	       }
	     else
	       {
		if ((pdd=get_init_flo()) == NULL) return 0;
		
		if (setmp_A(&A_tmp,&pa,pdd->rtrn.name) == 0) return 0;
		pdd->rtrn.paa=pa;

		if ( (d=add_it(psr,pdd,pdd->rtrn.name)) == ')' || d == ',')
		  {
		   pd->argm[i].paa=pa;
		   pd->argm[i].pdf=pdd;
		   strcpy(pd->argm[i].name,pdd->rtrn.name);
	          }
	        else
	          {
	           err_warn(1,fperr,
			"Error - incorrect argument delimiter (%c).\n",d);
		   return 0;
		  }
		pd->argm[i].ok=1;
	       }
	     if (d == ')')
	       {
	        if (((*psr)+1)->tok2 == '^')
	          {
	           (*psr)++;
	           if ( power_it(psr,pdm) == 0) return 0;
	          }
	        break;
	       }
	    }
	  if (i+1 != na && na != MAXARGS)
	    {
	     err_warn(1,fperr,
		"Error - incorrect number of arguments (%d instead of %d)"
			" for (%s).\n",i+1,na,funcs[j].name);
	     return 0;
	    }
	 }
       else return 0;
       return 1;
      }

/*		Set up memory for array table and array attributes.	*/

    int setmp_A(struct a_tab *pt,struct a_attr **pa,char *name)
      {
	int k=1;
	char str[17];

	for ( ;pt->next != NULL;pt=pt->next,k++);
	if (pt->pA_attr != NULL)
	  {
	   k++;
	   if ((pt->next=(struct a_tab *)malloc(sizeof(A_tab))) == NULL)
	     {
	      err_warn(1,fperr,"Error - memory for %s not found \n",str);
	      return 0;
	     }
	   pt=pt->next;
	   pt->next=NULL;
	  }
	sprintf(str,"QQQ%03d",k);
	strcpy(pt->name,str);
	if ((pt->pA_attr=*pa=
		(struct a_attr *)malloc(sizeof(struct a_attr))) == NULL)
	  {
	   err_warn(1,fperr,"Error - memory for %s not found \n",str);
	   return 0;
	  }
	zeroA_attr(*pa); /* nullify the set of attributes */
	strcpy(name,str);
	return 1;
      }

/*		Set up a named array or literal constant argument.	*/

    int setarg_AorV (struct data_flo *pd,struct parsed **psr,
							struct a_attr *pa)

      {
       struct a_tab *ptab;


       strcpy(pd->argm[0].name,(*psr)->str);
       if ((*psr)->v != (float)1.e20)
	 {
	  pd->argm[0].v=(*psr)->v;
	 }
       else
	 {
	  for (ptab=&A_tab;ptab != NULL && ptab->pA_attr != NULL &&
		     cmpnbl((*psr)->str,ptab->name)!=0;ptab=ptab->next);
	  if (ptab == NULL || ptab->pA_attr == NULL)
	    {
	     err_warn(1,fperr,
			"Error - assignment variable (%s) not found.\n",
								(*psr)->str);
	     return 0;
	    }
	  pd->argm[0].paa=ptab->pA_attr;
	  if (!check_dimens(pa,pd->argm[0].paa))
	    {
	     err_warn(1,fperr,
			"Error - dimensions won't fit.\n (%s).\n",
								(*psr)->str);
	     return 0;
	    }
	 }
       pd->argm[0].ok=1;
       return 1;
      }

/*			Handle a power.					*/

    int power_it (struct parsed **psr,struct data_flo *pdm)
       {
	struct data_flo *pd;
	struct a_attr *pa;

	if ((pd=get_init_flo()) == NULL) return 0;

	if (setmp_A(&A_tmp,&pa,pd->rtrn.name) == 0) return 0;

	pd->rtrn.paa=pa;
	pd->argm[0].paa=pdm->argm[0].paa;
	pd->argm[0].pdf=pdm->argm[0].pdf;
	strcpy(pd->argm[0].name,pdm->argm[0].name);
	pd->argm[0].v=pdm->argm[0].v;

	pdm->argm[0].pdf=pd;
	pdm->argm[0].paa=pa;
	strcpy(pdm->argm[0].name,pd->rtrn.name);
	pdm->argm[0].v=1.e20;
	pdm->argm[0].ok=1;

	(*psr)++;
	strcpy(pd->func,"power");

	if ( (*psr)->tok2 == '(')
	  {
	   (*psr)++;
	   if ( (*psr)->tok2 == '-')
	     {
	      (*psr)++;
	      if ((*psr)->tok2 != ')')
		{
		 err_warn(1,fperr,"Error - exponent syntax.\n");
		 return 0;
		}
	      (*psr)->v=-((*psr)->v);
	     }
	   else if ( (*psr)->tok2 == '+')
	     {
	      (*psr)++;
	      if ((*psr)->tok2 != ')')
		{
		 err_warn(1,fperr,"Error - exponent syntax.\n");
		 return 0;
		}
	     }
	   if ((*psr)->tok2 != ')')
	     {
	      err_warn(1,fperr,"Error - exponent syntax.\n");
	      return 0;
	     }
	   if ((*psr)->str[0] == '\0' && (*psr)->v == 1.e20)
	     {
	      err_warn(1,fperr,"Error - exponent syntax.\n");
	      return 0;
	     }
	   strcpy(pd->argm[1].name,(*psr)->str);
	   pd->argm[1].v=(*psr)->v;
	   (*psr)++;
	  }
	else
	  {
	   if ((*psr)->str[0] == '\0' && (*psr)->v == 1.e20)
	     {
	      err_warn(1,fperr,"Error - exponent syntax.\n");
	      return 0;
	     }
	   strcpy(pd->argm[1].name,(*psr)->str);
	   pd->argm[1].v=(*psr)->v;
	   (*psr)->str[0]='\0';
	  }
	if ( (*psr)->tok2 == '^')
	  {
	   err_warn(1,fperr,"Error - exponent of exponent not allowed.\n");
	   return 0;
	  }
	return 1;
       }

/*			Kill the temporary array table.			*/

    int killA_tmp ()
      {
       struct a_tab *pa,*paa;
       struct a_attr *pA;

/*			Release array storage.				*/

/*        printf("in there !\n"); */
       pa=paa=&A_tmp;
       if (pa->pA_attr != NULL) killA_attr(pa->pA_attr);
       pa=pa->next;
       while (pa != NULL)
	 {
	  paa=pa;
	  pa=pa->next;
	  pA=paa->pA_attr;
/* 	  printf("killing %s !\n",paa->name); */
	  killA_attr(pA);
	  free ((char *)paa->pA_attr);
	  free ((char *)paa);
	 }
       A_tmp.next=NULL;
       A_tmp.pA_attr=NULL;
       A_tmp.name[0]='\0';

       return 1;
      }


    int set_dim(struct data_flo *pd,struct a_attr *pa)
      {
       int i,n;
       struct data_flo *pdd;
       struct a_attr *paa;

       paa=pa;
       for (pdd=pd;pdd != NULL;pdd=pdd->next)
	 {
	  if (pdd->rtrn.paa != paa)
	    {
	     err_warn(1,fperr,
		"Error - return arg isn't right for (%s).\n",pdd->rtrn.name);
	/*     clear_data_flo(&pd); DNW */
	     return 0;
	    }
	  for (i=0;i < NFUNC && strcmp(pdd->func,funcs[i].name) != 0;i++);
	  if (i == NFUNC) n=1;
	  else n=funcs[i].num_args;
	  if (i == 1)
	    {
	     if (mock_mean(pa,pdd) == 0)
	       {
	        err_warn(1,fperr,
		   "Error - can't set dimensions for (%s).\n",pdd->rtrn.name);
	        /*clear_data_flo(&pd); DNW */
	        return 0;
	       }
	    }
	  else
	    {
	     for (i=0;i < n;i++)
	       {
	        if (pdd->argm[i].paa != NULL )
	          {
		   if (pdd->argm[i].pdf != NULL)
		     {
		      if (set_dim(pdd->argm[i].pdf,pdd->argm[i].paa) == 0)
		        {
		         err_warn(1,fperr,
			  "Error - in dimensions for (%s).\n",pdd->rtrn.name);
		         /*clear_data_flo(&pd); DNW */
		         return 0;
		        }
		     }
		   if (set_copyA(pa,pdd->argm[i].paa) == 0)
		     {
		      err_warn(1,fperr,
			"Error - in dimensions for (%s).\n",pdd->rtrn.name);
		      /* clear_data_flo(&pd); DNW */
		      return 0;
		     }
		  }
	       }
	    }
	 }

       return 1;
      }

/*		Set dimensions for the MEAN of the array.		*/

    int mock_mean (struct a_attr *pa, struct data_flo *pd)
      {
       int i,j,k,m,n,nd;

       struct a_attr *pA;

/*		Find the number of arguments for the MEAN function.	*/

       for (i=nd=0;i<MAXARGS;i++)
	 {
	  if (pd->argm[i].ok) nd++;
	  else break;
	 }

       if (nd <= 1)
	 {
	  err_warn(1,fperr,
	    "Error - Not enough arguments for MEAN (requires more than 1).\n");
	  return 0;
	 }

/*		The first argument must be the array operand.		*/

       if ((pA=pd->argm[0].paa) == NULL)
	 {
	  err_warn(1,fperr,
		"Error - the first argument for MEAN must be the array.\n");
	  return 0;
	 }
/*		Check that the other arguments are dimension names for the
		array.							*/

       for (i=1;i<nd;i++)
	 {
	  for (j=0; j < pA->ND; j++)
		 if (cmpncs(pA->xn[j],pd->argm[i].s) == 0) break;
	  if (j == pA->ND)
	    {
	     err_warn(1,fperr,
	       "Warning - argument %d - %s isn't a dimension name as needed"
		" for function (mean).\n",i, pd->argm[i].s);
	    }
	 }
       for (j=n=0; j < pA->ND; j++)
	 {
	  for (i=1; i < nd && cmpncs(pA->xn[j],pd->argm[i].s) != 0; i++);
	  if (i == nd)
	    {
	     pa->XN[n]=repstr(pa->XN[n],pA->xn[j]);
	     pa->XU[n]=repstr(pa->XU[n],pA->xu[j]);
	     pa->xn[n]=repstr(pa->xn[n],pA->xn[j]);
	     pa->xu[n]=repstr(pa->xu[n],pA->xu[j]);

	     if (pa->xs[n]==NULL && 
		(pa->xs[n]=(int *)malloc(sizeof(int)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     if (pa->XS[n]==NULL && 
		(pa->XS[n]=(int *)malloc(sizeof(int)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     k=*pa->XS[n]=*pa->xs[n]=*pA->xs[j];
	     if (pa->XV[n] != NULL) free((char *)pa->XV[n]);
	     if (pa->XB[n] != NULL) free((char *)pa->XB[n]);
	     if (pa->XW[n] != NULL) free((char *)pa->XW[n]);
	     if (pa->xv[n] != NULL) free((char *)pa->xv[n]);
	     if (pa->xb[n] != NULL) free((char *)pa->xb[n]);
	     if (pa->xw[n] != NULL) free((char *)pa->xw[n]);

	     if( (pa->XV[n]=(float *)malloc(k*sizeof(float)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     if( (pa->XB[n]=(float *)malloc((k+1)*sizeof(float)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     if( (pa->XW[n]=(float *)malloc(k*sizeof(float)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     if( (pa->xv[n]=(float *)malloc(k*sizeof(float)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     if( (pa->xb[n]=(float *)malloc((k+1)*sizeof(float)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     if( (pa->xw[n]=(float *)malloc(k*sizeof(float)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     for (m=0;m<k;m++)
	       {
	        pa->XV[n][m]=pa->xv[n][m]=pA->xv[j][m];
	        pa->XB[n][m]=pa->xb[n][m]=pA->xb[j][m];
	        pa->XW[n][m]=pa->xw[n][m]=pA->xw[j][m];
	       }
	     pa->XB[n][k]=pa->xb[n][k]=pA->xb[j][k];

	     if (pa->XK[n]==NULL && 
		(pa->XK[n]=(int *)malloc(sizeof(int)*2))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     pa->XK[n][0]=pa->XK[n][1]=0;

	     if (pa->XC[n]==NULL && 
		(pa->XC[n]=(float *)malloc(sizeof(float)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     *pa->XC[n]=0.0;

	     if (pa->xi[n]==NULL && 
		(pa->xi[n]=(int *)malloc(sizeof(int)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     *pa->xi[n]=0;

	     if (pa->xj[n]==NULL && 
		(pa->xj[n]=(int *)malloc(sizeof(int)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     *pa->xj[n]=1;

	     if (pa->xf[n]==NULL && 
		(pa->xf[n]=(float *)malloc(sizeof(float)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }

	     if (pa->XF[n]==NULL && 
		(pa->XF[n]=(float *)malloc(sizeof(float)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     *pa->XF[n]=*pa->xf[n]=pA->xv[j][0];

	     if (pa->xl[n]==NULL && 
		(pa->xl[n]=(float *)malloc(sizeof(float)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }

	     if (pa->XL[n]==NULL && 
		(pa->XL[n]=(float *)malloc(sizeof(float)))==NULL)
	       {
	        err_warn(1,fperr,"Error - (MOCK_MEAN) memory overflow.\n");
	        return 0;
	       }
	     *pa->XL[n]=*pa->xl[n]=pA->xv[j][k-1];
	     n++;
	    }
	  pa->ND=n;
	 }

       pa->S=repstr(pa->S,pA->s);
       pa->s=repstr(pa->s,pA->s);
       pa->N=repstr(pa->N,pA->n);
       pa->n=repstr(pa->n,pA->n);
       pa->TI=repstr(pa->TI,pA->ti);
       pa->ti=repstr(pa->ti,pA->ti);
       pa->U=repstr(pa->U,pA->u);
       pa->u=repstr(pa->u,pA->u);
       pa->TY=repstr(pa->TY,pA->ty);
       pa->ty=repstr(pa->ty,pA->ty);
       pa->CRD=repstr(pa->CRD,pA->crd);
       pa->crd=repstr(pa->crd,pA->crd);
       pa->CRT=repstr(pa->CRT,pA->crt);
       pa->crt=repstr(pa->crt,pA->crt);
       pa->com1=repstr(pa->com1,pA->com1);
       pa->com2=repstr(pa->com2,pA->com2);
       pa->com3=repstr(pa->com3,pA->com3);
       pa->com4=repstr(pa->com4,pA->com4);

/*		If all dimensions disappear then it is an error.	*/

       if (n == 0) {pa->notok=1; return 0;}
       return 1;
      }
