#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"


    extern struct a_tab A_tab;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

    extern int Inactive;

    char *repstr(char *s2,char *s1);

    int select_A (a_name,source,name,title,units,fname,func)
       char * a_name;
       char * source;
       char * name;
       char * title;
       char * units;
       char * fname;
       char * func;
      {
	int c,d;
	char *pc;
	struct a_tab *ptab,*ptb;
	struct a_attr *pA_;

/*		This function is for the interactive portion of this
		software.  It will create an attribute set for the array
		definition, by looking in the array attribute table for the
		name, and making a table entry if it doesn't exist, or
		returning a zero if it does.

		If a new entry in the array table is made the script output
		file (if open) receives the attribute assignment.	*/

	for(ptb=ptab=&A_tab,c=1;ptab->pA_attr!=NULL &&
					(c=cmpnbl(a_name,ptab->name))!=0;)
	  {
	   if (ptab->next == NULL)
	     {
              if ((ptab->next=(struct a_tab *)malloc(sizeof(A_tab))) == NULL)
	        {
		 err_warn(1,fperr,
		    "Error - table entry memory for A_%s not found \n",a_name);
		 return 0;
	        }
	      ptb=ptab;
	      ptab=ptab->next;
	      ptab->name[0]='\0';
	      ptab->pA_attr=NULL;
	      ptab->next=NULL;
	     }
	   else {ptb=ptab; ptab=ptab->next;}
	  }
	if (c == 0)
	  {
	   err_warn(1,fperr,"Error - A_%s already exists.\n",a_name);
	   return 0; /*  The name exists, can't select.	*/
	  }

/*		If it doesn't exist try to add the attribute set
		and set the name into the table.			*/

	else if ((pA_=ptab->pA_attr) == NULL)
	  {
	   if ( (pA_=ptab->pA_attr=
	           (struct a_attr *) malloc(sizeof(*pA_))) == NULL)
	     {
	      if (ptb != ptab) {free(ptb->next); ptb->next=NULL;} 
	      err_warn(1,fperr,"Error - memory for A_%s not found. \n",a_name);
	      free((char *)ptab);
	      return 0;
             };
	   strncpy (ptab->name,a_name,17); ptab->name[16]='\0';
	   trimbl(ptab->name,17);
	   ptab->FROM_CDAT = 0; /* array originated from VCS */
	   zeroA_attr(pA_); /* nullify the set of attributes */
	  }

	if (fname != NULL)
	  {
	   if ((pA_->aF=repstr(pA_->F,fname))==NULL ||
		(pA_->aS=repstr(pA_->S,source)) == NULL ||
		(pA_->aN=repstr(pA_->N,name)) == NULL ||
		(pA_->aTI=repstr(pA_->TI,title)) == NULL ||
		(pA_->aU=repstr(pA_->U,units)) == NULL )
	     {
	      free(pA_->aF);
	      free(pA_->aS);
	      free(pA_->aN);
	      free(pA_->aTI);
	      free(pA_->aU);
	      free((char *)pA_);
	      ptab->pA_attr=NULL;
	      ptb->next=NULL;
	      if (ptab != &A_tab) free((char *)ptab);
	      return 0;
	     }
	

	  }
	else if (func != NULL)
	  {
	   pA_->af=repstr(pA_->af,func);
	   pA_->notok=1;
	   if (strcmp(pA_->af,"CDAT") == 0)
	      ptab->FROM_CDAT = 1; /* array originated from CDAT */
	   c = 1;
	  }
	else
	  {
	   err_warn(1,fperr,"Error - no file or function for select A_%s.\n",
						a_name);
	   return 0;
	  }
/*		Check it out.						*/

	if (!ptab->FROM_CDAT) {
	   c=checkA_attr(ptab);
	   if (c != 0)
	     {
	      if (!Inactive && fpout != NULL) prtA(fpout,ptab);
	     }
	   else
	      removeA(ptab->name);
	}

	return c;
      }



