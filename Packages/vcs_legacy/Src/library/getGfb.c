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

    extern struct gfb_tab Gfb_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;

/*	Remove a boxfill graphics assignment.
	The string defining the name must be in "str".			*/


/*		Get a Graphics boxfill, and saveit.			*/


    struct gfb_tab *getGfb(char *str)
      {
       int i;
       struct gfb_tab *p,*p1;
       struct gfb_attr *pa,*pa1;
       struct fill_range *pifr,*pifr1,*pifrn;

/*		Search boxfill table for attributes to be copied.	*/

       for (p1=&Gfb_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL || p1->pGfb_attr == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Graphics boxfill (%s) can't be found for get.\n",str);
	  return 0;
	 }
       pa1=p1->pGfb_attr;

/*		Create a new table structure and copy to it.		*/

       if((p=(struct gfb_tab *)malloc(sizeof(struct gfb_tab)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Graphics boxfill(%s) not found.\n",
						str);
	  return NULL;
	 }

       strcpy(p->name,p1->name);

/*		Create a new attribute structure and copy to it.	*/

       if((pa=(struct gfb_attr *)malloc(sizeof(struct gfb_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Graphics boxfill(%s) not found.\n",
						str);
	  free((char *) p);
	  return NULL;
	 }
       p->pGfb_attr=pa;
       pa->line=NULL;
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

       if (((strcmp(pa1->xat, "linear") != 0) || (strcmp(pa1->xat, "") == 0)) &&
            (strcmp(pa->proj, "linear") == 0) ) {
          strncpy(pa->xat,pa1->xat,17); pa->xat[16] = '\0';
       } else {
          strncpy(pa->xat,"linear",17); pa->xat[16] = '\0';
       }
       if (((strcmp(pa1->yat, "linear") != 0) || (strcmp(pa1->yat, "") == 0)) &&
            (strcmp(pa->proj, "linear") == 0) ) {
          strncpy(pa->yat,pa1->yat,17); pa->yat[16] = '\0';
       } else {
          strncpy(pa->yat,"linear",17); pa->yat[16] = '\0';
       }
       pa->lev1=pa1->lev1;
       pa->lev2=pa1->lev2;
       pa->color_1=pa1->color_1;
       pa->color_2=pa1->color_2;
       pa->boxfill_type=pa1->boxfill_type;
       if (pa1->legend != NULL) {
          if ((pa->legend=(char *)malloc((strlen(pa1->legend))*sizeof(char)+1))
                == NULL) {
              err_warn(1,fperr,
                      "Error - memory for legend(%s) changes not found. \n",
                      pa1->legend);
             return 0;
          }
          strcpy(pa->legend, pa1->legend);
       } else
          pa->legend=pa1->legend;
       pa->ext_1=pa1->ext_1;
       pa->ext_2=pa1->ext_2;
       pa->missing=pa1->missing;
       pifr=NULL;
       for (pifr1=pa1->line,pifrn=NULL;pifr1 != NULL;pifr1=pifr1->next)
         {
          if ((pifrn=
                (struct fill_range *)malloc(sizeof(struct fill_range)))==NULL)
            {
             err_warn(1,fperr,
                        "Error - memory for getting Graphics isofill(%s) not found./n", str);
             killGfb(p);
             return 0;
            }
	  if (pa->line == NULL) pa->line=pifrn;
	  else pifr->next=pifrn;
	  pifr=pifrn;

          pifr->id=pifr1->id;
          pifr->lev1=pifr1->lev1;
	  pifr->lev2=pifr1->lev2;
	  strncpy(pifr->fill_name,pifr1->fill_name,17); pifr->fill_name[16]='\0';
	  pifr1->fill_name[16]='\0';
	  pifr->next=NULL;
	 }

       return p;
      }


    int killGfb(struct gfb_tab *p)
      {
       struct gfb_attr *pa;
       struct fill_range *pifr,*pifr1;

       pa=p->pGfb_attr;
       pifr1=pa->line;
       if (pa->legend != NULL) free((char *)pa->legend);
       free((char *)pa);
       free((char *)p);
       while (pifr1 != NULL)
         {
          pifr=pifr1->next;
          free((char *) pifr1);
          pifr1=pifr;
         }
       return 1;
      }
