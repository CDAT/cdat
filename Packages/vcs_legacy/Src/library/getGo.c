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

    extern struct go_tab Go_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;

/*	Remove an outline graphics assignment.
	The string defining the name must be in "str".			*/


/*		Get a Graphics outline, and saveit.			*/


    struct go_tab *getGo(char *str)
      {
       int i;
       struct go_tab *p,*p1;
       struct go_attr *pa,*pa1;

/*		Search outline table for attributes to be copied.	*/

       for (p1=&Go_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL || p1->pGo_attr == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Graphics outlines (%s) can't be found for get.\n",str);
	  return 0;
	 }
       pa1=p1->pGo_attr;

/*		Create a new table structure and copy to it.	*/

       if((p=(struct go_tab *)malloc(sizeof(struct go_tab)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Graphics outlines(%s) not found.\n",
						str);
	  return NULL;
	 }

       strcpy(p->name,p1->name);

/*		Create a new attribute structure and copy to it.	*/

       if((pa=(struct go_attr *)malloc(sizeof(struct go_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Graphics outlines(%s) not found.\n",
						str);
	  free((char *) p);
	  return NULL;
	 }
       p->pGo_attr=pa;
       strncpy(pa->proj,pa1->proj,256); pa->proj[255] = '\0';
       strncpy(pa->xtl1,pa1->xtl1,256); pa->xtl1[255] = '\0';
       strncpy(pa->xtl2,pa1->xtl2,256); pa->xtl2[255] = '\0';
       strncpy(pa->xmt1,pa1->xmt1,256); pa->xmt1[255] = '\0';
       strncpy(pa->xmt2,pa1->xmt2,256); pa->xmt2[255] = '\0';
       strncpy(pa->ytl1,pa1->ytl1,256); pa->ytl1[255] = '\0';
       strncpy(pa->ytl2,pa1->ytl2,256); pa->ytl2[255] = '\0';
       strncpy(pa->ymt1,pa1->ymt1,256); pa->ymt1[255] = '\0';
       strncpy(pa->ymt2,pa1->ymt2,256); pa->ymt2[255] = '\0';
       strncpy(pa->timeunits,pa1->timeunits,256); pa->timeunits[255] = '\0';
       for (i=0;i<4;i++) pa->dsp[i]=pa1->dsp[i];
       for (i=0;i<4;i++) pa->idsp[i]=pa1->idsp[i];
       pa->calendar=pa1->calendar;
       if (((strcmp(pa1->xat, "linear") != 0) || (strcmp(pa1->xat, "") == 0)) &&            (strcmp(pa->proj, "linear") == 0) ) {
          strncpy(pa->xat,pa1->xat,17); pa->xat[16] = '\0';
       } else {
          strncpy(pa->xat,"linear",17); pa->xat[16] = '\0';
       }
       if (((strcmp(pa1->yat, "linear") != 0) || (strcmp(pa1->yat, "") == 0)) &&            (strcmp(pa->proj, "linear") == 0) ) {
          strncpy(pa->yat,pa1->yat,17); pa->yat[16] = '\0';
       } else {
          strncpy(pa->yat,"linear",17); pa->yat[16] = '\0';
       }
       pa->n=pa1->n;
       for (i=0;i<pa->n;i++) pa->out[i]=pa1->out[i];
       strncpy(pa->lb,pa1->lb,17); pa->lb[16] = '\0';

       return p;
      }


    int killGo(struct go_tab *p)
      {
       struct go_attr *pa;

       pa=p->pGo_attr;
       free((char *)p);
       free((char *)pa);
       return 1;
      }
