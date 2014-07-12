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

    extern struct gi_tab Gi_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;

/*	Remove an isoline graphics assignment.
	The string defining the name must be in "str".

	An isoline discriptor may be removed instead by following "name"
	with "#<id no.>" as: "test#2"					*/


/*		Get a Graphics isoline, and saveit.			*/


    struct gi_tab *getGi(char *str)
      {
       int i;
       struct gi_tab *p,*p1;
       struct gi_attr *pa,*pa1;
       struct iso *piso,*piso1,*pison;


/*		Search isoline table for attributes to be copied.	*/

       for (p1=&Gi_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL || p1->pGi_attr == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Graphics isolines (%s) can't be found for get.\n",str);
	  return 0;
	 }
       pa1=p1->pGi_attr;

/*		Create a new table structure and copy to it.	*/

       if((p=(struct gi_tab *)malloc(sizeof(struct gi_tab)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Graphics isolines(%s) not found.\n",
						str);
	  return NULL;
	 }

       strcpy(p->name,p1->name);

/*		Create a new attribute structure and copy to it.	*/

       if((pa=(struct gi_attr *)malloc(sizeof(struct gi_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Graphics isolines(%s) not found.\n",
						str);
	  free((char *) p);
	  return NULL;
	 }
       p->pGi_attr=pa;
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
       pa->labels=pa1->labels;
       
       for (piso1=pa1->line,pison=NULL;piso1 != NULL;piso1=piso1->next)
	 {
	  if ((pison=(struct iso *)malloc(sizeof(struct iso)))==NULL)
	    {
	     err_warn(1,fperr,
	      "Error - memory for getting Graphics isolines(%s) not found./n",
					str);
	     piso1=pa->line;
	     free((char *)p);
	     free((char *)pa);
	     while (piso1 != NULL)
	       {
		piso=piso1->next;
		free((char *) piso1);
		piso1=piso;
	       }
	     return NULL;
	    }
	  if (pa->line == NULL) pa->line=pison;
	  else piso->next=pison;
	  piso=pison;

	  piso->id=piso1->id;
	  piso->p=piso1->p;
	  piso->lev=piso1->lev;
	  piso->incr=piso1->incr;
	  piso->hici=piso1->hici;
	  strncpy(piso->lab,piso1->lab,13); piso->lab[12] = '\0';
	  strncpy(piso->lb,piso1->lb,17); piso->lb[16] = '\0';
	  strncpy(piso->tb,piso1->tb,17); piso->tb[16] = '\0';
	  strncpy(piso->to,piso1->to,17); piso->to[16] = '\0';
	  piso->cw=piso1->cw;
	  piso->ls=piso1->ls;
	  piso->angle=piso1->angle;
	  piso->spc=piso1->spc;
	  piso->next=NULL;
	 }

       return p;
      }


    int killGi(struct gi_tab *p)
      {
       struct gi_attr *pa;
       struct iso *piso,*piso1;

       pa=p->pGi_attr;
       piso1=pa->line;
       free((char *)p);
       free((char *)pa);
       while (piso1 != NULL)
	 {
	  piso=piso1->next;
	  free((char *) piso1);
	  piso1=piso;
         }
       return 1;
      }
