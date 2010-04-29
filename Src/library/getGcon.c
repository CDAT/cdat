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

    extern struct gcon_tab Gcon_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;

/*	Remove a continents assignment.
	The string defining the name must be in "str".			*/


/*		Get a continents, and saveit.				*/


    struct gcon_tab *getGcon(char *str)
      {
       int i;
       struct gcon_tab *p,*p1;
       struct gcon_attr *pa,*pa1;

/*		Search continents table for attributes to be copied.	*/

       for (p1=&Gcon_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL || p1->pGcon_attr == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Continents (%s) can't be found for get.\n",str);
	  return 0;
	 }
       pa1=p1->pGcon_attr;

/*		Create a new table structure and copy to it.	*/

       if((p=(struct gcon_tab *)malloc(sizeof(struct gcon_tab)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Continents (%s) not found.\n",
						str);
	  return NULL;
	 }

       strcpy(p->name,p1->name);

/*		Create a new attribute structure and copy to it.	*/

       if((pa=(struct gcon_attr *)malloc(sizeof(struct gcon_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Continents (%s) not found.\n",
						str);
	  free((char *) p);
	  return NULL;
	 }
       p->pGcon_attr=pa;
       strncpy(pa->proj,pa1->proj,256); pa->proj[255] = '\0';
       strncpy(pa->xtl1,pa1->xtl1,256); pa->xtl1[255] = '\0';
       strncpy(pa->xtl2,pa1->xtl2,256); pa->xtl2[255] = '\0';
       strncpy(pa->xmt1,pa1->xmt1,256); pa->xmt1[255] = '\0';
       strncpy(pa->xmt2,pa1->xmt2,256); pa->xmt2[255] = '\0';
       strncpy(pa->ytl1,pa1->ytl1,256); pa->ytl1[255] = '\0';
       strncpy(pa->ytl2,pa1->ytl2,256); pa->ytl2[255] = '\0';
       strncpy(pa->ymt1,pa1->ymt1,256); pa->ymt1[255] = '\0';
       strncpy(pa->ymt2,pa1->ymt2,256); pa->ymt2[255] = '\0';
       for (i=0;i<4;i++) pa->dsp[i]=pa1->dsp[i];
       strncpy(pa->lb,pa1->lb,17); pa->lb[16] = '\0';
       pa->cont_type = pa1->cont_type;

       return p;
      }


    int killGcon(struct gcon_tab *p)
      {
       struct gcon_attr *pa;

       pa=p->pGcon_attr;
       free((char *)p);
       free((char *)pa);
       return 1;
      }
