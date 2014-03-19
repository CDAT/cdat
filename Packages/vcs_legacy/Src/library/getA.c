#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "display.h"
#include "graph.h"

#define STRMAX 256

    extern FILE *fpin,*fpout,*fperr;

    extern struct a_tab A_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;

/*		Get an array table entry, and saveit.			*/


    struct a_tab *getA(char *str)
      {
       struct a_tab *p,*p1;
       struct a_attr *pa;

/* 		If str is null, then return. No error message needed.   */
       if (str[0] == '\0') return NULL; 

/*		Search array table for attributes to be copied.		*/

       for (p1=&A_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL) return 0;

/*		Create a new table structure and copy to it.	*/

       if((p=(struct a_tab *)malloc(sizeof(struct a_tab)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting array table entry (%s) not found.\n",
						str);
	  return NULL;
	 }
       if((pa=p->pA_attr=(struct a_attr *)malloc(sizeof(struct a_attr)))==NULL)
	 {
	  err_warn(0,fperr,
	    "Error - memory for array attributes (%s) can't be found.\n",str);
	  free((char *)p);
	  return 0;
	 }
/* 		Zero the new array attributes.				*/

       zeroA_attr(pa);

       strcpy(p->name,p1->name);

       copyA_attr(p,p1);

       p->next=NULL;

       return p;
      }
