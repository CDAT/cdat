#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "picture.h"
#include "display.h"
#include "graph.h"

#define STRMAX 256

    extern FILE *fpin,*fpout,*fperr;

    extern struct p_tab Pic_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;

/*		Get a template, and saveit.				*/


    struct p_tab *getP(char *str)
      {
       struct p_tab *p,*p1;

/*		Search picture template table for attributes to be copied.*/

       for (p1=&Pic_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Picture template (%s) can't be found for get.\n",str);
	  return 0;
	 }

/*		Create a new table structure and copy to it.	*/

       if((p=(struct p_tab *)malloc(sizeof(struct p_tab)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting picture template(%s) not found.\n",
						str);
	  return NULL;
	 }

       strcpy(p->name,p1->name);

       copyP_attr(p1,p);

       p->next=NULL;

       return p;
      }


    int killP(struct p_tab *p)
      {

       free((char *)p);
       return 1;
      }
