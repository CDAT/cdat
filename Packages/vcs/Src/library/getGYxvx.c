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

    extern struct gYx_tab GYx_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;

/*	Get a Y(x) vs x graphics assignment.
	The string defining the name must be in "str".			*/


/*		Get a Graphics Y(x) vs x, and saveit.			*/


    struct gYx_tab *getGYx(char *str)
      {
       int i;
       struct gYx_tab *p,*p1;
       struct gYx_attr *pa,*pa1;

/*		Search Y(x) vs x table for attributes to be copied.	*/

       for (p1=&GYx_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL || p1->pGYx_attr == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Graphics Y(x) vs x (%s) can't be found for get.\n",str);
	  return 0;
	 }
       pa1=p1->pGYx_attr;

/*		Create a new table structure and copy to it.		*/

       if((p=(struct gYx_tab *)malloc(sizeof(struct gYx_tab)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Graphics Y(x) vs x(%s) not found.\n",
						str);
	  return NULL;
	 }

       strcpy(p->name,p1->name);

/*		Create a new attribute structure and copy to it.	*/

       if((pa=(struct gYx_attr *)malloc(sizeof(struct gYx_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Graphics Y(x) vs x(%s) not found.\n",
						str);
	  free((char *) p);
	  return NULL;
	 }
       p->pGYx_attr=pa;
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
       strncpy(pa->lb,pa1->lb,17); pa->lb[16] = '\0';
       strncpy(pa->mb,pa1->mb,17); pa->mb[16] = '\0';

       return p;
      }


    int killGYx(struct gYx_tab *p)
      {
       struct gYx_attr *pa;

       pa=p->pGYx_attr;
       free((char *)p);
       free((char *)pa);
       return 1;
      }
