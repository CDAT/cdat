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

    extern struct table_chorn To_tab;
    extern struct table_chorn To_tab1;
    extern struct table_chorn To_tab2;
    extern struct table_chorn To_tab3;
    extern struct table_chorn To_tab4;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Get a chorn assignment for the named chorn attributes.		*/

    struct table_chorn *getTo(char *str)
      {
       struct table_chorn *p,*p1;

/*		Search chorn table for attributes to be copied.	*/

       for (p1=&To_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Chorn attributes (%s) can't be found for get.\n",str);
	  return NULL;
	 }

/*		Create a new table structure and copy to it.	*/

       if((p=(struct table_chorn *)malloc(sizeof(struct table_chorn)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Chorn table (%s) not found.\n",str);
	  return 0;
	 }

       strcpy(p->name,p1->name);
       p->chh=p1->chh;
       p->chua=p1->chua;
       p->chpath=p1->chpath;
       p->chalh=p1->chalh;
       p->chalv=p1->chalv;
       p->next=NULL;

       return p;
      }

    int chk_mov_To (struct table_chorn *pt)

      {
       struct table_chorn *ptb,*ptab;

       if (pt == NULL) return 0;
       	for (ptb=ptab=&To_tab;
		ptab!=NULL && cmpnbl(pt->name,ptab->name)!=0;
			ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

         if (ptab == &To_tab || ptab == &To_tab1 || ptab == &To_tab2 ||
	     ptab == &To_tab3 || ptab == &To_tab4 )
	  {
	   err_warn(1,fperr,"Error - can't replace the defaults (To_%s).\n",
				pt->name);
	   free((char *)pt);
	   return 0;
	  }

	if (ptab == NULL)
	  {
	   ptb->next=ptab=pt;
	  }
	else
	  {
/* 	    if (pt->chh != ptab->chh) printf("chh diff %f,%f\n",pt->chh, ptab->chh); */
/* 	   if (pt->chua != ptab->chua) printf("cua diff\n"); */
/* 	   if (pt->chpath != ptab->chpath) printf("chpath diff\n"); */
/* 	   if (pt->chalh != ptab->chalh) printf("chalh diff %d,%d\n",pt->chalh, ptab->chalh); */
/* 	   if (pt->chalv != ptab->chalv) printf("chalv diff\n"); */
	   if (pt->chh != ptab->chh || pt->chua != ptab->chua ||
	       pt->chpath != ptab->chpath || pt->chalh != ptab->chalh ||
	       pt->chalv != ptab->chalv)
	     {
	      ptab->chh=pt->chh;
	      ptab->chua=pt->chua;
	      ptab->chpath=pt->chpath;
	      ptab->chalh=pt->chalh;
	      ptab->chalv=pt->chalv;
	      check_d_chorn(ptab->name);
	     }
	   free((char *)pt);
	   pt=NULL;
	  }
	if (!Inactive && fpout != NULL) prtTo(fpout,ptab);
	return 1;
       } 
