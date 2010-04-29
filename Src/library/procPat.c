#include "gks.h"
#include "gksshort.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "picture.h"

#define STRMAX 256

    extern FILE *fpin,*fpout,*fperr;
    extern struct table_pattern Pat_tab;
    extern struct table_fill Tf_tab; 
    extern struct display_tab D_tab;

/*	Process a pattern table assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procPat_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,c,index,ix,iy;
	int tokm, ilim;
	int *atab;
	char strm[STRMAX+1];
	int patt[1024];
	float v;
	struct table_pattern *ptab, *ptb;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }

	i=0;
	ilim=1026;
	atab=NULL;
	while (i < ilim && (c=getsttk(strm,&tokm)) != 0 &&
		isnum(strm) && tokm != EOF && (tokm == ',' || tokm == ')') )
	  {
	   sscanf (strm,"%f",&v);

	      
	   if      (i == 0) index=v;
	   else if (i == 1) ix=v;
	   else if (i == 2) iy=v;
	   else if (i < 1024) patt[i-3]=v;
	   else
	     {
	      err_warn(1,fperr,
		"Error - pattern  (Pat_%d) overflow."
				"  Nothing changed.\n",index);
	      return 0;
	     }
	   i++;
	   if (tokm == ')') break;
	  }
	if (c == 0)
	  {
	   err_warn(1,fperr,
		"Error - empty value in the pattern (Pat_%d)."
			"  Not used.\n",index);
	   return 0;
	  }
	if (!isnum(strm))
	  {
	   err_warn(1,fperr,
		"Error - non-number in the pattern table (%s=%s).\n",str,strm);
	   return 0;
	  }
	if (i-3 != ix*iy)
	  {
	   err_warn(1,fperr,"Error - pattern (%s) is incomplete."
							"  Deleted.\n",str);
	   return 0;
	  }
	if (tokm != ')' && tokm != ',' && tokm != EOF)
	  {
	   err_warn(1,fperr,
		"Error - pattern delimiter (%s=xxx%c) is wrong.\n",
							str,tokm);
	   return 0;
	  }
	if (tokm == EOF)
	  {
	   err_warn(1,fperr,"Error - EOF in pattern (%s).\n",str);
	   return 0;
	  }

/*		Look in the pattern table for the index.		*/
/*		Make a table entry if it doesn't exist			*/

	for (ptb=ptab=&Pat_tab; ptab!=NULL && ptab->index != index;)
	  {
	   if (ptab->next == NULL)
	     {
	      if (((ptab->next)=
		(struct table_pattern *)malloc(sizeof(Pat_tab))) == NULL)
	        {
		 err_warn(1,fperr,"Error - memory for %s not found \n",str);
		 return 0;
	        }
	      ptb=ptab;
	      ptab=ptab->next;
	      ptab->array=NULL;
	      ptab->next=NULL;
	      break;
	     }
	   else {ptb=ptab; ptab=ptab->next;}
	  }
	if (ptab == &Pat_tab)
	  {
	   err_warn(1,fperr,"Error - cant overide default pattern.\n");
	   return 0;
	  }
	if ( (atab=(int *)malloc(sizeof(int)*ix*iy)) == NULL)
	  {
	   err_warn(1,fperr,"Error - memory for Pat_%d not found. \n",index);
	   if (ptab->array == NULL)
	     {
	      free((char *)ptab);
	      ptb->next=NULL;
	     }
	   return 0;
	  }
	if (ptab->array != NULL) free((char *)ptab->array);
	ptab->array=atab;
	ptab->ix=ix;
	ptab->iy=iy;
	ptab->index=index;
	for (i=0;i<ix*iy;i++) *(atab++)=patt[i];
	set_patterns();
	check_d_pattern(ptab);
	return 1;
      }

/*		Check whether the pattern is used for a graphics
		descriptor.  If so, a display update is needed.		*/

    int check_d_pattern(struct table_pattern *pat)

      {
       struct table_fill   *ptab;
       

       ptab=&Tf_tab;

       while (ptab != NULL)
	 {
	   if (ptab->fais_size!=0)
	     if (ptab->fais[0] == 2 && ptab->fasi[0] == pat->index)
	       check_d_fill(ptab->name);
	   ptab=ptab->next;
         }

       return 1;
      }

/*		Set patterns into GKS.					*/

    int set_patterns()
      {
	int i;
	int *pi;
	struct table_pattern *ptab;
	Gptbundl pt;
       Gintlist wsid;

       wsid.number = 0;
       wsid.integers = NULL;
       gqopwk(&wsid);
       pi=wsid.integers;
       for (i=0;i<wsid.number;pi++,i++)
	 {
          if (*pi == 1 || *pi == 2)
	    {
	     for(ptab=&Pat_tab; ptab != NULL; ptab=ptab->next)
	       {
	        pt.size.x=ptab->ix;
	        pt.size.y=ptab->iy;
	        pt.array=ptab->array;
	        gspar(*pi,ptab->index,&pt);
	       }
	    }
	 }
	if (wsid.number > 0 && wsid.integers != NULL) 
	    free((char *)wsid.integers);
	return 1;
      }

/*			Print a pattern.				*/

    int prtPat(FILE *fp,struct table_pattern *ptab)
      {
	int i,j;
	int *pi;

	if (ptab == NULL) return 0;

	fprintf (fp,"Pat_%d(%d,%d,%d,\n",ptab->index,ptab->index,
					ptab->ix,ptab->iy);
	   
	if ((pi=ptab->array) != NULL)
	  {
	   for (j=ptab->iy; j>0; j--,pi++)
	     {
	      fprintf(fp,"  ");
	      for (i=ptab->ix; i>1; i--,pi++)
		   fprintf(fp,"%d,",*pi);
	      if (j != 1)
		   fprintf(fp,"%d,\n",*pi);
	      else
		   fprintf(fp,"%d)\n",*pi);
	     }
	  }
	return 1;
      }
