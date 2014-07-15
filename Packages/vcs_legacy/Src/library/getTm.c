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

    extern struct table_mark Tm_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Get a mark assignment for the named mark attributes.		*/

    struct table_mark *getTm(char *str)
      {
       float             deflt=1.0;
       struct table_mark *p,*p1;

/*		Search mark table for attributes to be copied.	*/

       for (p1=&Tm_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Mark attributes (%s) can't be found for get.\n",str);
	  return NULL;
	 }

/*		Create a new table structure and copy to it.	*/

       if((p=(struct table_mark *)malloc(sizeof(struct table_mark)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Mark table (%s) not found.\n",str);
	  return 0;
	 }

       strcpy(p->name,p1->name);
       strcpy(p->proj,p1->proj);
       copy_int_array( &p->mtyp, &p1->mtyp, &p->mtyp_size, p1->mtyp_size,1 );
       copy_float_array( &p->msize, &p1->msize, &p->msize_size, p1->msize_size,&deflt );
       copy_int_array( &p->mci, &p1->mci, &p->mci_size, p1->mci_size,241 );
       p->priority=p1->priority;
       p->mvp[0]=p1->mvp[0];
       p->mvp[1]=p1->mvp[1];
       p->mvp[2]=p1->mvp[2];
       p->mvp[3]=p1->mvp[3];
       p->mwc[0]=p1->mwc[0];
       p->mwc[1]=p1->mwc[1];
       p->mwc[2]=p1->mwc[2];
       p->mwc[3]=p1->mwc[3];
       if (copy_points( &p->mx, p1->mx) == 0) return 0;
       if (copy_points( &p->my, p1->my) == 0) return 0;
       p->next=NULL;

       return p;
      }

/*			Move a mark structure back to the table of
			such values and check whether changes
			will affect the display.			*/

    int chk_mov_Tm (struct table_mark *pt)

      {
       float             deflt=1.0;
       struct table_mark *ptb,*ptab;

       if (pt == NULL) return 0;
       	for (ptb=ptab=&Tm_tab;
		ptab!=NULL && cmpnbl(pt->name,ptab->name)!=0;
			ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

         if (ptab == &Tm_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (Tm_%s).\n",
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
	   if (pt->mtyp != ptab->mtyp || pt->msize != ptab->msize ||
	       pt->mci != ptab->mci || ptab->priority != pt->priority ||
               ptab->mvp[0] != pt->mvp[0] || ptab->mvp[1] != pt->mvp[1] ||
               ptab->mvp[2] != pt->mvp[2] || ptab->mvp[3] != pt->mvp[3] ||
               ptab->mwc[0] != pt->mwc[0] || ptab->mwc[1] != pt->mwc[1] ||
               ptab->mwc[2] != pt->mwc[2] || ptab->mwc[3] != pt->mwc[3] ||
               ptab->mx != pt->mx || ptab->my != pt->my || (strcmp(ptab->proj,pt->proj) != 0)
              )
	     {
	      strcpy(ptab->proj,pt->proj);
	      copy_int_array( &ptab->mtyp,&pt->mtyp,&ptab->mtyp_size,pt->mtyp_size,1 );
	      copy_float_array( &ptab->msize, &pt->msize, &ptab->msize_size,
                                 pt->msize_size,1.0, &deflt );
	      copy_int_array( &ptab->mci, &pt->mci, &ptab->mci_size, pt->mci_size,241 );
	      ptab->priority=pt->priority;
              ptab->mvp[0]=pt->mvp[0];
              ptab->mvp[1]=pt->mvp[1];
              ptab->mvp[2]=pt->mvp[2];
              ptab->mvp[3]=pt->mvp[3];
              ptab->mwc[0]=pt->mwc[0];
              ptab->mwc[1]=pt->mwc[1];
              ptab->mwc[2]=pt->mwc[2];
              ptab->mwc[3]=pt->mwc[3];
              free_points( &ptab->mx );
              free_points( &ptab->my );
              if (copy_points( &ptab->mx, pt->mx) == 0) return 0;
              if (copy_points( &ptab->my, pt->my) == 0) return 0;
              free_points( &pt->mx );
              free_points( &pt->my );
              if (pt->mtyp != NULL) free((char *) pt->mtyp); pt->mtyp = NULL;
              if (pt->msize != NULL) free((char *) pt->msize); pt->msize = NULL;
              if (pt->mci != NULL) free((char *) pt->mci); pt->mci = NULL;
	      free((char *)pt);
	      pt=NULL;
	      check_d_mark(ptab->name);
	     }
	  }
	if (!Inactive && fpout != NULL) prtTm(fpout,ptab);
	return 1;
       } 
