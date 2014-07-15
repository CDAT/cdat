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

    extern struct l_tab L_tab[2];

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Get a list assignment for the named list.			*/

    struct l_tab *getList(char *str)
      {
       struct l_tab *p,*p1;
       struct l_val *pL,*pL1,*pLn;


/*		Search list table for attributes to be copied.	*/

       for (p1=&L_tab[0];p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL || p1->val == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - List (%s) can't be found for get.\n",str);
	  return 0;
	 }

/*		Create a new table structure and copy to it.	*/

       if((p=(struct l_tab *)malloc(sizeof(struct l_tab)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting List table (%s) not found.\n",str);
	  return NULL;
	 }

       strcpy(p->name,p1->name);
       p->count=p1->count;
       p->val=NULL;

       for (pL1=p1->val,pLn=NULL;pL1 != NULL;pL1=pL1->next)
	 {
	  if ((pLn=(struct l_val *)malloc(sizeof(struct l_val)))==NULL)
	    {
	     err_warn(1,fperr,
	      "Error - memory for getting List values (%s) not found./n",str);
	     killL(p);
	     return NULL;
	    }
	  if ((pLn->str=(char *)malloc(strlen(pL1->str)+1))==NULL)
	    {
	     err_warn(1,fperr,
	      "Error - memory for getting List values (%s) not found./n",str);
	     killL(p);
	     return NULL;
	    }
	  if (p->val == NULL) pL=p->val=pLn;
	  else pL->next=pLn;
	  pL=pLn;

	  pL->data=pL1->data;
	  strcpy(pL->str,pL1->str);
	  pL->next=NULL;
	 }

       return p;
      }

/*			Check whether a list change affects the display
			and move a list into the table.			*/

    int chk_mov_L (struct l_tab *ptab)
      {
       struct l_tab *ptr,*ptl;

       if (ptab == NULL || strprt(ptab->name) == 0)
	 {
	  err_warn(1,fperr,"Error - no list given (%s).\n",ptab->name);
	  if (ptab != NULL) killL(ptab);
	  return 0;
	 }
       
/*		Look in the list table for the name and replace it
		if it exists, otherwise insert it.			*/

	ptl=ptr=&L_tab[0];
	do
	  {
	   if (cmpnbl(ptab->name,ptr->name) == 0)
	     {
	      if (ptr == &L_tab[0] || ptr == &L_tab[1])
		{
		 err_warn(1,fperr,
			"Error - can't replace the default list (%s).\n",
				ptab->name);
		 killL(ptab);
		 return 0;
		}
	      ptl->next=ptab;
	      ptab->next=ptr->next;
	      killL(ptr);
	      check_L_display(ptab->name);
	      break;
	     }
	   ptl=ptr;
	   ptr=ptr->next;
	  } while (ptr != NULL);
	if (ptr == NULL)
	  {
	   ptl->next=ptab;
	   ptab->next=NULL;
	  }
	if (!Inactive && fpout != NULL) prtL(fpout,ptab);
	return 1;
      }

/*			Kill a list table entry and values.		*/

    int killL(struct l_tab *p)
      {
       struct l_val *pL,*pL1;

       pL1=p->val;
       free((char *)p);
       while (pL1 != NULL)
	 {
	  pL=pL1->next;
	  free((char *)pL1->str);
	  free((char *) pL1);
	  pL1=pL;
         }
       return 1;
      }
