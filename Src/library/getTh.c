#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "picture.h"
#include "display.h"
#include "graph.h"
#include "list.h"

#define STRMAX 256

    extern FILE *fpin,*fpout,*fperr;

    extern struct table_form Th_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Get a format assignment for the named format.			*/

    struct table_form *getTh(char *str)
      {
       struct table_form *p,*p1;
       struct form *pL,*pL1,*pLn;


/*		Search list table for attributes to be copied.	*/

       for (p1=&Th_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL || p1->variety == NULL)
	 {
	  err_warn(1,fperr,
	   "Errot - Format (%s) can't be found for get.\n",str);
	  return 0;
	 }

/*		Create a new table structure and copy to it.	*/

       if((p=(struct table_form *)malloc(sizeof(struct table_form)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Format table (%s) not found.\n",str);
	  return NULL;
	 }

       strcpy(p->name,p1->name);
       p->variety=NULL;
       p->next=NULL;
       pL=NULL;

       for (pL1=p1->variety;pL1 != NULL;pL1=pL1->next)
	 {
	  if ((pLn=(struct form *)malloc(sizeof(struct form)))==NULL)
	    {
	     err_warn(1,fperr,
	      "Error - memory for getting Format values (%s) not found./n",str);
	     killTh(p);
	     return NULL;
	    }
	  if (p->variety == NULL) pL=p->variety=pLn;
	  else {pL->next=pLn; pL=pL->next;}
	  strcpy(pL->s_name,pL1->s_name);
	  strcpy(pL->s_units,pL1->s_units);
	  strcpy(pL->format,pL1->format);
	  pL->next=NULL;
	 }

       return p;
      }

/*			Check whether a list change affects the display
			and move a list into the table.			*/

    int chk_mov_Th (struct table_form *ptab)
      {
       struct table_form *ptr,*ptl;
       
/*		Look in the format table for the name and replace it
		if it exists, otherwise insert it.			*/

	ptl=ptr=&Th_tab;
	do
	  {
	   if (cmpnbl(ptab->name,ptr->name) == 0)
	     {
	      if (ptr == &Th_tab)
		{
		 err_warn(0,fperr,
			"Warning - can't replace the default format (Th_%s).\n",
				ptab->name);
		 killTh(ptab);
		 return 0;
		}
	      ptl->next=ptab;
	      ptab->next=ptr->next;
	      killTh(ptr);
	      check_d_fmt(ptab->name);
	      break;
	     }
	   ptl=ptr;
	   ptr=ptr->next;
	  } while (ptr != NULL);
	if (!Inactive && fpout != NULL) prtTh(fpout,ptab);
	return 1;
      }

/*			Kill a format table entry and values.		*/

    int killTh(struct table_form *p)
      {
       struct form *pL,*pL1;

       pL1=p->variety;
       free((char *)p);
       while (pL1 != NULL)
	 {
	  pL=pL1->next;
	  free((char *) pL1);
	  pL1=pL;
         }
       return 1;
      }
