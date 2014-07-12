#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "display.h"
#define STRMAX 256


    extern FILE *fpin,*fpout,*fperr;

    extern struct display_tab D_tab;
    extern struct a_tab A_tab;
    extern int	  Inactive;

    extern int I;
    extern int J;
    extern int K;
    extern int L;
    extern int M;
    extern int N;

/*	Process an index (I,J,K,L,M,N) increment or change command.	*/

    int procInd(str,tok)

      char str[];
      int *tok;

      {
	int tokm;
	int i,j,k,l,m,n;
	int c,d,a,r;
	char strm[STRMAX+1];

	i=I; j=J; k=K; l=L; m=M; n=N;

	if (*tok!='(')
	  {
	   err_warn(1,fperr,
		"Error (Index) - improper token (%s%c).\n",str,*tok);
	   return 0;
	  }
	c=getsttk(strm,&tokm);
	while ( tokm == '+' || tokm == '-' || tokm == '=' || tokm == ',')
	  {
/*			"I="						*/

	   if (c > 0 && tokm == '=')
	     {
	      d=toupper((int)strm[0]);
	      c=getsttk(strm,&tokm);

/*			I = num+	(increment I by num)		*/

	      if (c > 0 && tokm == '+')
		{
		 if (c > 0 && isnum(strm))
			for (a=0,r=0;strm[r]!='\0';r++) a=10*a+(strm[r]-'0');
		 else a=1;
		 if (d == 'I') i+=a;
		 else if (d == 'J') j+=a;
		 else if (d == 'K') k+=a;
		 else if (d == 'L') l+=a;
		 else if (d == 'M') m+=a;
		 else if (d == 'N') n+=a;
		 c=getsttk(strm,&tokm);	/* need to find ',' or ')'	*/
		}

/*			I = num-	(decrement I by num)		*/

	      if (c > 0 && tokm == '-')
		{
		 if (c > 0 && isnum(strm))
			for (a=0,r=0;strm[r]!='\0';r++) a=10*a+(strm[r]-'0');
		 else a=1;
		 if (d == 'I') i=(i > a)?i-a:1;
		 else if (d == 'J') j=(j > a)?j-a:1;
		 else if (d == 'K') k=(k > a)?k-a:1;
		 else if (d == 'L') l=(l > a)?l-a:1;
		 else if (d == 'M') m=(m > a)?m-a:1;
		 else if (d == 'N') n=(n > a)?n-a:1;

		 c=getsttk(strm,&tokm);	/* need to find ',' or ')'	*/
		}

/*			"I=num[,]/[)]"		(set I to num)		*/

	      else if (c > 0 && isnum(strm))
		{
		 for (a=0,r=0;strm[r]!='\0';r++) a=10*a+(strm[r]-'0');
		 if (a > 0)
		   {
		    if (d == 'I') i=a;
		    else if (d == 'J') j=a;
		    else if (d == 'K') k=a;
		    else if (d == 'L') l=a;
		    else if (d == 'M') m=a;
		    else if (d == 'N') n=a;
		   }
		 else
		   {
		    err_warn(1,fperr,
		     "Error - improper index value <= 0 (%s%c%s%c).\n",
							str,*tok,strm,tokm);
		    return 0;
		   }
		}
	     }
/*			"I+"		(increment I by 1)		*/

	   else if (c > 0 && tokm == '+')
	     {
	      d=toupper((int)strm[0]);
	      if (d == 'I') i++;
	      else if (d == 'J') j++;
	      else if (d == 'K') k++;
	      else if (d == 'L') l++;
	      else if (d == 'M') m++;
	      else if (d == 'N') n++;
	      c=getsttk(strm,&tokm);	/* need to find ',' or ')'	*/
	     }
/*			"I-"		(decrement I by 1)		*/

	   else if (c > 0 && tokm == '-')
	     {
	      d=toupper((int)strm[0]);
	      if (d == 'I') i=(i>1)?i-1:1;
	      else if (d == 'J') j=(j>1)?j-1:1;
	      else if (d == 'K') k=(k>1)?k-1:1;
	      else if (d == 'L') l=(l>1)?l-1:1;
	      else if (d == 'M') m=(m>1)?m-1:1;
	      else if (d == 'N') n=(n>1)?n-1:1;
	      c=getsttk(strm,&tokm);	/* need to find ',' or ')'	*/
	     }
	   else
	     {
	      err_warn(1,fperr,"Error - setting indices (%s%c)\n",str,*tok);
	      return 0;
	     }

	   if (tokm != ')') c=getsttk(strm,&tokm);
	  } 

	if (tokm != ')')
	  {
	   err_warn(1,fperr,
		"Error (INDEX) - not a proper token (%s%c%s%c).\n",
			str,*tok,strm,tokm);
	   return 0;
	  }

	check_index(i,j,k,l,m,n);

	return 1;
      }
/*		Check all places where indices are used and check
		whether a display is affected.  Set values.		*/

    int check_index (di,dj,dk,dl,dm,dn)

      int di,dj,dk,dl,dm,dn;

      {
	int i,c;
	int C[6],D[6];

	int change;
	int count;

	struct a_attr *pA;
	struct a_tab *pa;
	struct l_val *pt;

	char *pc,**pcc;

	change=0;
	for (i=0;i<6;i++) C[i]=0;

	if (I!=di) {C[0]='I'; D[0]=di; change = 1;}
	if (J!=dj) {C[1]='J'; D[1]=dj; change = 1;}
	if (K!=dk) {C[2]='K'; D[2]=dk; change = 1;}
	if (L!=dl) {C[3]='L'; D[3]=dl; change = 1;}
	if (M!=dm) {C[4]='M'; D[4]=dm; change = 1;}
	if (N!=dn) {C[5]='N'; D[5]=dn; change = 1;}

	if (change == 0) return 0;

	I=di; J=dj; K=dk; L=dl; M=dm; N=dn;


	pa=&A_tab;
	while (pa != NULL && pa->pA_attr != NULL)
	  {
	   pA=pa->pA_attr;
	   change=0;
	   for (pcc=&pA->aF;pcc != &pA->axu[4];pcc++)
	     {
	      pc=*pcc;
	      if ( findL_name(pc,&pt,&count) != 0 && (c=find_ind(pc)) != 0)
		{
		 for (i=0;i<6;i++)
		    if (C[i] == c) {change=1; break;}
		}
	     }
	   for (pcc=&pA->aXC[0];pcc != &pA->axl[4];pcc++)
	     {
	      pc=*pcc;
	      if ( findL_name(pc,&pt,&count) != 0 && (c=find_ind(pc)) != 0)
		{
		 for (i=0;i<6;i++)
		    if (C[i] == c) {change=1; break;}
		}
	     }
	   for (pcc=&pA->aXV[0];pcc != &pA->axw[4];pcc++)
	     {
	      pc=*pcc;
	      if ( findL_name(pc,&pt,&count) != 0 && (c=find_ind(pc)) != 0)
		{
		 for (i=0;i<6;i++)
		    if (C[i] == c) {change=1; break;}
		}
	     }
	   if (change)
	     {
	      /*if (check_d_A(pa->name))
	   		checkA_attr(pa);*/
/*   		DNW - Always check the display and update all varialbes */
	      check_d_A(pa->name);
	      checkA_attr(pa);
	     }
	   pa=pa->next;
	  }
       return 1;
      }
/*			Find an index (I-N) on a list name, return the
			index character or 0 if it isn't found.		*/

    int find_ind (str)
      char *str;
      {
       int c;

       if (str == NULL) return 0;
/*			Find the left bracket.			*/
       while (*str != '\0' && *str != '[') str++;
       c = 0;
       if (*str++ == '[')
	 {
/*			Find the index character (I-N).			*/
	  while (*str != '\0' && *str != ']')
	    {
	     if (*str >= 'I' && *str <= 'N' && c == 0) c = *str;
	     str++;
	    }
/*		If the bracket isn't closed, an index wasn't found.	*/
	  if (*str != ']') return 0;
	 }
       return c;
      }

/*			Find an index (I-N) on a list name, return the
			index character or 0 if it isn't found.		*/

    int find_index (str)
      char *str;
      {
       int c;

       if (str == NULL) return 0;
/*			Find the "L_" for a list name.			*/
       c = '\0';
       while (*str != '\0' && *str != c)
	  if (*str++ == 'L' && *str == '_') c = '[';
       c = 0;
       if (*str++ != '\0')
	 {
/*			List name and left bracket found.
			Find the index character (I-N).			*/
	  while (*str != '\0' && *str != ']')
	    {
	     if (*str >= 'I' && *str <= 'N') c = *str;
	     str++;
	    }
/*		If the bracket isn't closed, an index wasn't found.	*/
	  if (*str != ']') return 0;
	 }
       return c;
      }
	


/*		Print Index values for script output.			*/

    int prtInd(FILE *fp)
      {
       fprintf (fp,"Index(I=%d,J=%d,K=%d,L=%d,M=%d,N=%d)\n",I,J,K,L,M,N);
       return 1;
      }
