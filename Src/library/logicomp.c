#include <stdio.h>
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
	 struct data_flo *pdf;
	};

    struct data_flo
	{
	 struct data_flo *prev; |+  Pointer to previous structure.	|+
	 struct data_flo *next; |+  Pointer to next structure.		|+
	 struct argmnt rtrn;	 |+  Return argument for function.	|+
	 char func[17];		 |+  Function name.			|+
	 struct argmnt argm[MAXARGS];|+  Given argument(s) for function.|+
	};								*/


/*			Integer adjustable indices			*/

    extern int I;
    extern int J;
    extern int K;
    extern int L;
    extern int M;
    extern int N;

/*		Function prototypes.					*/

   struct data_flo *get_init_flo();
    int err_warn (int beep,FILE *fp,char *fmt,...);
   char *repstr(char *s2,char *s1);

/*			Compile a logical assignment statement into a linked
			data-flo path.					*/

    struct data_flo *logicomp (struct a_tab *ptab)

      {
       int i,k;
/*******************************************************************/
	char bstr[101];
/*******************************************************************/

       struct parslog pars[MAXELEM+1],*psr;
       struct data_flo *pd;
       char *name;		/*The array table name.			*/
       struct a_attr *pa;	/*Pointer to the array attribute structure.*/

/*********************************************************************/

	for (i=0;i<100;i++) bstr[i]='\0';
	bstr[0]=' ';
/*******************************************************************/

       pa=ptab->pA_attr;
       name=ptab->name;

       if (pa->lmask == NULL) return NULL;

       psr=&pars[0];

	for (k=0;k<=MAXELEM;k++)
	  {
	   pars[k].str[0]='\0';
	   pars[k].tok1[0]=0;
	   pars[k].tok2[0]=0;
	   pars[k].tok1[1]=0;
	   pars[k].tok2[1]=0;
	   pars[k].tok1[2]='\0';
	   pars[k].tok2[2]='\0';
	   pars[k].v=1.e20;
	  }
/*			Parse the logical statement.			*/

       if ((k=pars_logic(pa->lmask,psr,name)) == 0)
	 {
	  pa->notok=1;
	  return NULL;
	 }
/**************************************************************/
/*	for(i=0;
	pars[i].str[0] !='\0'||pars[i].tok1[0] !='\0'|| pars[i].tok2[0]!='\0';
		i++)
	fprintf(stdout,"%s / %s / %s / %g\n",
		pars[i].tok1,pars[i].str,pars[i].tok2,
				pars[i].v);
*/
       if ((pd=get_init_flo()) == NULL) return NULL;
       pd->rtrn.paa=pa;
       strcpy(pd->rtrn.name,name);
       strcpy(pd->argm[0].name,name);
       pd->argm[0].paa=pa;
       if (psr->tok1[0] == '\0')
	 {
	  psr->tok1[0]='&';
	  psr->tok1[1]='&';
	 }

       if ( and_it(&psr,pd,name) == 0)
	 {
	  killA_tmp();
	  clear_data_flo(&pd);
	  err_warn(1,fperr,"Error - catastrophic in compiling (A_%s).\n",name);
	  pa->notok=1;
	  return NULL;
	 }
/**************************************************************/

/*       printer(bstr,pd);			*/
       return pd;
      }

/*			Handle an 'and' or 'or'.			*/


    int and_it(struct parslog **psr,struct data_flo *pdm,char *name)

      {
       int c,d;
       struct data_flo *pd;
       struct a_attr *pa,*pA;

       pa=pdm->rtrn.paa; /*	Needed to check dimensions.		*/


/*		Loop while its & or |.					*/

       while ( (c=(*psr)->tok1[0])=='&' || c=='|')
	 {
	  if (c == '&') strcpy(pdm->func,"AND");
	  else strcpy(pdm->func,"OR");

	  d=(*psr)->tok2[0];

/*			Is it a single logical term?			*/

	  if (d == '&' || d == '|' || d == '\0')
	    {
	     if (setarg_log_AorV(2,pdm,psr,pa) == 0) return 0;
	     if (d == '\0') return 1;
	     (*psr)++;
	    }
/*			Handle a parenthetical element.			*/
	  else if (d == '(')
	    {
	     if (paren_log_it(psr,pdm,name) == 0) return 0;
	     (*psr)++;
	    }
/*			Recursive return to paren_log_it.		*/

	  else if (d == ')') return 1;


/*			Now handle anything but & or |.			*/

	  else
	    {
/*			Set memory for returning the comparison.	*/

	     if ((pd=get_init_flo()) == (int)NULL) return (int)NULL;
	     if (setmp_A(&A_tmp,&pA,pd->rtrn.name) == 0) return 0;
	     pd->rtrn.paa=pA;
	     strcpy(pdm->argm[1].name,pd->rtrn.name);
	     pdm->argm[1].paa=pA;
	     pdm->argm[1].pdf=pd;

/*	 		Is it a comparison?				*/

	     if (d == '<')
	       {
		if ((*psr)->tok2[1] == '\0')
		  {
		   if (setarg_log_AorV(1,pd,psr,pA) == 0) return 0;
		   strcpy(pd->func,"LT");
		   (*psr)++;
		   if (setarg_log_AorV(2,pd,psr,pA) == 0) return 0;
		   (*psr)++;
		  }
		else if ((*psr)->tok2[1] == '=')
		  {
		   if (setarg_log_AorV(1,pd,psr,pA) == 0) return 0;
		   strcpy(pd->func,"LE");
		   (*psr)++;
		   if (setarg_log_AorV(2,pd,psr,pA) == 0) return 0;
		   (*psr)++;
		  }
	       }
	     else if (d == '>')
	       {
		if ((*psr)->tok2[1] == '\0')
		  {
		   if (setarg_log_AorV(1,pd,psr,pA) == 0) return 0;
		   strcpy(pd->func,"GT");
		   (*psr)++;
		   if (setarg_log_AorV(2,pd,psr,pA) == 0) return 0;
		   (*psr)++;
		  }
		else if ((*psr)->tok2[1] == '=')
		  {
		   if (setarg_log_AorV(1,pd,psr,pA) == 0) return 0;
		   strcpy(pd->func,"GE");
		   (*psr)++;
		   if (setarg_log_AorV(2,pd,psr,pA) == 0) return 0;
		   (*psr)++;
		  }
	       }

	     else if (d == '!')
	       {
		if (setarg_log_AorV(1,pd,psr,pA) == 0) return 0;
		strcpy(pd->func,"NE");
		(*psr)++;
		if (setarg_log_AorV(2,pd,psr,pA) == 0) return 0;
		(*psr)++;
	       }
	     else if (d == '=')
	       {
		if (setarg_log_AorV(1,pd,psr,pA) == 0) return 0;
		strcpy(pd->func,"EQ");
		(*psr)++;
		if (setarg_log_AorV(2,pd,psr,pA) == 0) return 0;
		(*psr)++;
	       }

	     else
	       {
		err_warn(1,fperr,"Error - syntax in logical mask for A_%s.\n",
			name);
		return 0;
	       }
	    }
	  if ( (*psr)->str[0] != '\0' || (*psr)->tok2[0] != '\0')
	    {
	     if ((pd=get_init_flo()) == NULL) return 0;
	     pdm->next=pd;
	     pdm=pd;
	     pdm->argm[0].paa=pa;
	     strcpy(pdm->argm[0].name,name);
	     pdm->rtrn.paa=pa;
	     strcpy(pdm->rtrn.name,name);
	    }
	 }

       return 1;
      }


/*		Parse a logical assignment string.			*/
/*		Logicals are #name or a<b or a>b or a<=b or a>=b a==b or a!=b
		Logical operators are && and ||				*/

    int pars_logic(char str[],struct parslog p[],char name[])
      {
	int i,j,k,n;
	unsigned int c;
	struct a_tab *ptab;


/*		Place the elements of the string in "parsed" structures.*/

	for (i=k=0;k < MAXELEM && (c=str[i]) != '\0';)
	  {
	   if (c <= ' ') {i++; continue;}

/*		Store a name or number in the string.			*/

	   for (j=0;!logtok(c=str[i]) && c != '\0';i++)
	     {
	      if (c == '(')
		{
		 p[k].tok2[0]=c;
		 k++;
		 p[k].tok1[0]=c;
		 i++;
		 break;
		}
	      else if (c == ')')
		{
		 p[k].tok2[0]=c;
		 if (isnum(p[k].str)) sscanf(p[k].str,"%g",&p[k].v);
		 k++;
		 p[k].tok1[0]=c;
		 i++;
		 break;
		}
	      else if (c > ' ')
		{
	         if (j < 18)
		   {
		    p[k].str[j++]=c;
	            p[k].str[j]='\0';
		   }
	         else if (j > 18)
		   {
		    err_warn(1,fperr,
			"Error - logical assignment name too long (%s)\n",
						p[k].str);
		    return 0;
		   }
		}
	     }
/*		There must be a name or number between each set
			of logical operators.				*/

	   if (j == 0 && p[k].tok1[0] != '(' && p[k].tok1[0] != ')')
	     {
	      err_warn(1,fperr,
		"Error - syntax of logical assignment.  A_%s(lmask='%s')\n",
						name,str);
	      return 0;
	     }
/*		If the string is a number convert and store in v.	*/

	   if (isnum(p[k].str)) sscanf(p[k].str,"%g",&p[k].v);

/*		Store the following logical operator in token2 and in
		token1 for the following name or number element.	*/

	   if (logtok(c))
	     {
	      p[k].tok2[0]=c;
	      if (logtok(c=str[++i]))
		{
		 p[k].tok2[1]=c;
		 i++;
		}
	      k++;
	      p[k].tok1[0]=p[k-1].tok2[0];
	      p[k].tok1[1]=p[k-1].tok2[1];
	      j=0;
	     }
	   if (k >= MAXELEM)
	     {
	      err_warn(1,fperr,
			"Error - too many terms in mask assignment. "
					" A_%s(lmask='%s')\n",name,str);
	      return 0;
	     }
	  }
/*		Check the syntax and name references.			*/

	for (j=0;j < k;j++)
	  {
	   n=0;
	   if (p[j].str[0] == '#') n=1;
	   if (!isnum(p[j].str) && p[j].str[0] != '\0')
	     {
	      for(ptab=&A_tab;
			ptab != NULL && strcmp(ptab->name,&p[j].str[n]) != 0;
				ptab=ptab->next);
	      if (ptab == NULL)
		{
		 err_warn(1,fperr,
		   "Error - undefined name (%s) in mask assignment"
			" A_%s(lmask='%s').\n",
				p[j].str,name,str);
		 return 0;
		}
/*	      if (strcmp(ptab->name,name) == 0) DNW - July 1, 1996
		{
		 err_warn(1,fperr,
		   "Error - identical name (%s) in mask assignment"
			" A_%s(lmask='%s').\n",
				p[j].str,name,str);
		 return 0;
		}*/
	     }
/*		Logical operators can only operate on logicals, i.e.
		logical operators cannot follow each other without a
		logical value between.					*/

	   if (p[j].tok1[0]=='&' || p[j].tok1[0]=='|')
	     {
	      if (p[j].tok1[0] != p[j].tok1[1])
		{
		 err_warn(1,fperr,
			"Error - illegal operator (%s) in A_%s(lmask= .\n",
				p[j].tok1,name);
		 return 0;
		}
	      if (p[j].tok2[0]=='&' || p[j].tok2[0]=='|')
		{
		 if (p[j].str[0] != '#')
		   {
		    err_warn(1,fperr,
			"Error - illogical sequence of logical operators in "
			"A_%s(lmask='%s').\n",name,str);
		    return 0;
		   }
		}
	      if (p[j].str[0] == '#' && 
		   (p[j].tok1[0]!='&'&&p[j].tok1[0]!='|'&&p[j].tok1[0]!=0) &&
		   (p[j].tok2[0]!='&'&&p[j].tok2[0]!='|'&&p[j].tok2[0]!=0) )
					 
		{
		 err_warn(1,fperr,
		   "Error - illegal op - #operand - op sequence (%s%s%s) in"
			" A_%s(lmask= .\n",
				p[j].tok1,p[j].str,p[j].tok2,name);
		 return 0;
		}
	     }
	   else if (p[j].tok1[0]=='<'||p[j].tok1[0]=='>'||p[j].tok1[0]=='='||
			p[j].tok1[0]=='!')
	     {
	      if (p[j].tok2[0]=='<'||p[j].tok2[0]=='>'||p[j].tok2[0]=='='||
			p[j].tok2[0]=='!')
		{
		 err_warn(1,fperr,
		   "Error - illegal operator sequence in A_%s(lmask='%s')\n",
				name,str);
		 return 0;
		}
	      if (p[j].tok1[1]!='\0' && p[j].tok1[1]!='=')
		{
		 err_warn(1,fperr,
		   "Error - illegal operator in A_%s(lmask='%s')\n",
				name,str);
		 return 0;
		}
	      if ( (p[j].tok1[0]=='='||p[j].tok1[0]=='!') && p[j].tok1[1]!='=')
		{
		 err_warn(1,fperr,
		   "Error - illegal compare operator in A_%s(lmask='%s')\n",
				name,str);
		 return 0;
		}
	     }
	   else if (p[j].tok1[0] == '(' && p[j].tok2[0] == ')')
	     {
	      if (j > 0)
		{
		 p[j].tok1[0]=p[j-1].tok2[0];
		 p[j].tok1[1]=p[j-1].tok2[1];
		}
	      else p[j].tok1[0]=p[j].tok1[1]=0;
	      if (j < k-1)
		{
		 p[j].tok2[0]=p[j+1].tok1[0];
		 p[j].tok2[1]=p[j+1].tok1[1];
		}
	      else p[j].tok2[0]=p[j].tok2[1]=0;
	     }
	  }

	if (p[k].tok2[0] != 0 && p[k].tok2[0] != ')')
	  {
	   err_warn(1,fperr,
		"Error - logical assign cannot terminate with an operator.\n",
						str);
	   return 0;
	  }
	return 1;
      }


/*			Handle parenthetical expression.		*/

    int paren_log_it (struct parslog **psr,struct data_flo *pdm,char *name)
      {
	struct data_flo *pd;
	struct a_attr *pa;


	if ((pd=get_init_flo()) == NULL) return 0;
	
	if (setmp_A(&A_tmp,&pa,pd->rtrn.name) == 0) return 0;
	pd->rtrn.paa=pa;
	pd->argm[0].paa=pa;
	strcpy(pd->argm[0].name,pd->rtrn.name);
	pdm->argm[1].pdf=pd;
	strcpy(pdm->argm[1].name,pd->rtrn.name);
	pdm->argm[1].paa=pa;
	(*psr)++;
	(*psr)->tok1[0]='&';
	(*psr)->tok1[1]='&';

	if (and_it(psr,pd,pd->rtrn.name) == 0) return 0;

	return 1;
      }


/*		Set up a named array or literal constant argument.	*/

    int setarg_log_AorV(int n,struct data_flo *pd,struct parslog **psr,
						struct a_attr *pa)

      {
       int m;

       struct a_tab *ptab;


       strcpy(pd->argm[n-1].name,(*psr)->str);
       if ((*psr)->v != (float)1.e20)
	 {
	  pd->argm[n-1].v=(*psr)->v;
	 }
       else
	 {
	  m=0;
	  if ((*psr)->str[0] == '#') m=1;
	  for (ptab=&A_tab;ptab != NULL && ptab->pA_attr != NULL &&
		     cmpnbl(&((*psr)->str[m]),ptab->name)!=0;ptab=ptab->next);
	  if (ptab == NULL || ptab->pA_attr == NULL)
	    {
	     err_warn(1,fperr,
			"Error - assignment variable (%s) not found.\n",
								(*psr)->str);
	     return 0;
	    }
	  pd->argm[n-1].paa=ptab->pA_attr;
	  if (!compare_dims(pa,pd->argm[n-1].paa))
	    {
	     err_warn(1,fperr,
			"Error - dimensions won't fit.\n (%s).\n",
								(*psr)->str);
	     return 0;
	    }
	 }
       pd->argm[n-1].ok=1;
       return 1;
      }


/*			Check and set dimensions for computed array
			against actual data used in the computation.	*/

    int compare_dims (struct a_attr *pa,struct a_attr *pA)
      {
       if (pa->ND == 0) set_copyA(pa,pA);
       return 1;
      }

/*		Tokens are defined to be <>=!|&,  			*/

    int logtok (int c)
/*	Test whether it is a token or not and return T (1) or F (0).	*/
      {
	if (c=='<'||c=='>'||c=='='||c=='&'||c=='!'||c=='|') return 1;
	return 0;
      }

