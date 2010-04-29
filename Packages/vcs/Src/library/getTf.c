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

    extern struct table_fill Tf_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Get a fill assignment for the named fill attributes.		*/

    struct table_fill *getTf(char *str)
      {
       struct table_fill *p,*p1;

/*		Search fill table for attributes to be copied.	*/

       for (p1=&Tf_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Fill attributes (%s) can't be found for get.\n",str);
	  return NULL;
	 }

/*		Create a new table structure and copy to it.	*/

       if((p=(struct table_fill *)malloc(sizeof(struct table_fill)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Fill table (%s) not found.\n",str);
	  return 0;
	 }

       strcpy(p->name,p1->name);
       strcpy(p->proj,p1->proj);
       copy_int_array(&p->fais, &p1->fais, &p->fais_size, p1->fais_size, 1 );
       copy_int_array(&p->fasi, &p1->fasi, &p->fasi_size, p1->fasi_size, 1 );
       copy_int_array(&p->faci, &p1->faci, &p->faci_size, p1->faci_size, 241 );
       p->x=p1->x;
       p->y=p1->y;
       p->w=p1->w;
       p->h=p1->h;
       p->priority=p1->priority;
       p->fvp[0]=p1->fvp[0];
       p->fvp[1]=p1->fvp[1];
       p->fvp[2]=p1->fvp[2];
       p->fvp[3]=p1->fvp[3];
       p->fwc[0]=p1->fwc[0];
       p->fwc[1]=p1->fwc[1];
       p->fwc[2]=p1->fwc[2];
       p->fwc[3]=p1->fwc[3];
       if (copy_points( &p->fx, p1->fx) == 0) return 0;
       if (copy_points( &p->fy, p1->fy) == 0) return 0;
       p->next=NULL;

       return p;
      }

    int chk_mov_Tf (struct table_fill *pt)

      {
       struct table_fill *ptb,*ptab;

       if (pt == NULL) return 0;
       for (ptb=ptab=&Tf_tab;
		ptab!=NULL && cmpnbl(pt->name,ptab->name)!=0;
			ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

       if (ptab == &Tf_tab)
	 {
	  err_warn(1,fperr,"Error - can't replace the default (Tf_%s).\n",
				pt->name);
	  free((char *)pt);
	  return 0;
	 }

       if (ptab == NULL)
	  {
	   ptb->next=ptab=pt;
	   ptab->next=NULL;
	  }
	else
	  {
	   if (pt->fais != ptab->fais || pt->fasi != ptab->fasi ||
	       pt->faci != ptab->faci || pt->x != ptab->x ||
	       pt->y != ptab->y || pt->w != ptab->w || pt->h != ptab->h ||
               pt->priority != ptab->priority || ptab->fvp[0] != pt->fvp[0] ||
               ptab->fvp[1] != pt->fvp[1] || ptab->fvp[2] != pt->fvp[2] ||
               ptab->fvp[3] != pt->fvp[3] || ptab->fwc[0] != pt->fwc[0] ||
               ptab->fwc[1] != pt->fwc[1] || ptab->fwc[2] != pt->fwc[2] ||
               ptab->fwc[3] != pt->fwc[3] || ptab->fx != pt->fx ||
               ptab->fy != pt->fy || (strcmp(ptab->proj,pt->proj) != 0)
              )
	     { 
	      strcpy(ptab->proj,pt->proj);
	      copy_int_array(&ptab->fais,&pt->fais,&ptab->fais_size,pt->fais_size,1);
	      copy_int_array(&ptab->fasi,&pt->fasi,&ptab->fasi_size,pt->fasi_size,1);
	      copy_int_array(&ptab->faci,&pt->faci,&ptab->faci_size,pt->faci_size,241);
	      ptab->x=pt->x;
 	      ptab->y=pt->y;
	      ptab->w=pt->w;
	      ptab->h=pt->h;
	      ptab->priority=pt->priority;
              ptab->fvp[0]=pt->fvp[0];
              ptab->fvp[1]=pt->fvp[1];
              ptab->fvp[2]=pt->fvp[2];
              ptab->fvp[3]=pt->fvp[3];
              ptab->fwc[0]=pt->fwc[0];
              ptab->fwc[1]=pt->fwc[1];
              ptab->fwc[2]=pt->fwc[2];
              ptab->fwc[3]=pt->fwc[3];
              free_points( &ptab->fx );
              free_points( &ptab->fy );
              if (copy_points( &ptab->fx, pt->fx) == 0) return 0;
              if (copy_points( &ptab->fy, pt->fy) == 0) return 0;
              free_points( &pt->fx );
              free_points( &pt->fy );
              if (pt->fais != NULL) free((char *) pt->fais); pt->fais = NULL;
              if (pt->fasi != NULL) free((char *) pt->fasi); pt->fasi = NULL;
              if (pt->faci != NULL) free((char *) pt->faci); pt->faci = NULL;
	      free((char *)pt);
	      pt=NULL;
	      check_d_fill(ptab->name);
	     }
	  }
	if (!Inactive && fpout != NULL) prtTf(fpout,ptab);
	return 1;
       } 
