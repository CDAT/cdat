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

    extern struct table_text Tt_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Get a text assignment for the named text attributes.		*/

    struct table_text *getTt(char *str)
      {
       struct table_text *p,*p1;

/*		Search text table for attributes to be copied.	*/

       for (p1=&Tt_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Text attributes (%s) can't be found for get.\n",str);
	  return NULL;
	 }

/*		Create a new table structure and copy to it.	*/

       if((p=(struct table_text *)malloc(sizeof(struct table_text)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Text table (%s) not found.\n",str);
	  return 0;
	 }

       strcpy(p->name,p1->name);
       strcpy(p->proj,p1->proj);
       p->txfont=p1->txfont;
       p->txpr=p1->txpr;
       p->txexp=p1->txexp;
       p->txsp=p1->txsp;
       p->txci=p1->txci;
       p->txfci=p1->txfci;
       p->priority=p1->priority;
       p->tvp[0]=p1->tvp[0];
       p->tvp[1]=p1->tvp[1];
       p->tvp[2]=p1->tvp[2];
       p->tvp[3]=p1->tvp[3];
       p->twc[0]=p1->twc[0];
       p->twc[1]=p1->twc[1];
       p->twc[2]=p1->twc[2];
       p->twc[3]=p1->twc[3];
       if (copy_points( &p->tx, p1->tx) == 0) return 0;
       if (copy_points( &p->ty, p1->ty) == 0) return 0;
       if (copy_strings( &p->ts, p1->ts) == 0) return 0;
       p->next=NULL;

       return p;
      }

/*			Move a text structure back to the table of
			such values and check whether changes
			will affect the display.			*/

    int chk_mov_Tt (struct table_text *pt)

      {
       struct table_text *ptb,*ptab;



       if (pt == NULL) return 0;
       	for (ptb=ptab=&Tt_tab;
		ptab!=NULL && cmpnbl(pt->name,ptab->name)!=0;
			ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

         if (ptab == &Tt_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (Tt_%s).\n",
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
/* 	    if (pt->txfont != ptab->txfont ) printf("txfont diff %d,%d\n",pt->txfont, ptab->txfont); */
/* 	    if (pt->txpr != ptab->txpr) printf("txpr diff %d,%d\n",pt->txpr, ptab->txpr); */
/* 	    if (pt->txexp != ptab->txexp ) printf("txexp diff\n"); */
/* 	    if (pt->txsp != ptab->txsp) printf("txsp diff\n");	        */
/* 	    if ( pt->txci != ptab->txci ) printf("txci diff %d,%d\n",pt->txci, ptab->txci); */
/* 	    if ( pt->priority != ptab->priority ) printf("priority diff\n"); */
/* 	    if (ptab->tvp[0] != pt->tvp[0] || ptab->tvp[1] != pt->tvp[1] || */
/* 		ptab->tvp[2] != pt->tvp[2] || ptab->tvp[3] != pt->tvp[3] ) printf("tvp diff\n"); */
/* 	    if ( ptab->twc[0] != pt->twc[0] || ptab->twc[1] != pt->twc[1] || */
/* 		 ptab->twc[2] != pt->twc[2] || ptab->twc[3] != pt->twc[3] ) printf("twc diff\n"); */
/* 	    if (compare_points(ptab->tx, pt->tx )!=1) printf("tx difff\n"); */
/* 	    if (compare_points(ptab->ty, pt->ty )!=1) printf("ty difff\n"); */
/* 	    if (compare_strings(ptab->ts, pt->ts )!=1) printf("ts diff %s %s\n",ptab->ts,pt->ts); */
/* 	    if (strcmp(ptab->proj,pt->proj) != 0) printf("proj diff\n"); */
	       
	   if (pt->txfont != ptab->txfont || pt->txpr != ptab->txpr ||
	       pt->txexp != ptab->txexp || pt->txsp != ptab->txsp ||
	       pt->txci != ptab->txci || pt->txfci != ptab->txfci || 
	       pt->priority != ptab->priority ||
               ptab->tvp[0] != pt->tvp[0] || ptab->tvp[1] != pt->tvp[1] ||
               ptab->tvp[2] != pt->tvp[2] || ptab->tvp[3] != pt->tvp[3] ||
               ptab->twc[0] != pt->twc[0] || ptab->twc[1] != pt->twc[1] ||
               ptab->twc[2] != pt->twc[2] || ptab->twc[3] != pt->twc[3] ||
               compare_points(ptab->tx, pt->tx )!=1 ||
	       compare_points(ptab->ty, pt->ty )!=1 ||
               compare_strings(ptab->ts, pt->ts )!=1 ||
	       (strcmp(ptab->proj,pt->proj) != 0)
	       )
	     {
	       strcpy(ptab->proj,pt->proj);
	       ptab->txfont=pt->txfont;
	       ptab->txpr=pt->txpr;
	       ptab->txexp=pt->txexp;
	       ptab->txsp=pt->txsp;
	       ptab->txci=pt->txci;
	       ptab->txfci=pt->txfci;
	       ptab->priority=pt->priority;
	       ptab->tvp[0]=pt->tvp[0];
	       ptab->tvp[1]=pt->tvp[1];
	       ptab->tvp[2]=pt->tvp[2];
	       ptab->tvp[3]=pt->tvp[3];
	       ptab->twc[0]=pt->twc[0];
	       ptab->twc[1]=pt->twc[1];
	       ptab->twc[2]=pt->twc[2];
	       ptab->twc[3]=pt->twc[3];
	       free_points( &ptab->tx );
	       free_points( &ptab->ty );
	       free_strings( &ptab->ts );
	       if (copy_points( &ptab->tx, pt->tx) == 0) return 0;
	       if (copy_points( &ptab->ty, pt->ty) == 0) return 0;
	       if (copy_strings( &ptab->ts, pt->ts) == 0) return 0;
	       free_points( &pt->tx );
	       free_points( &pt->ty );
	       free_strings( &pt->ts );
	       check_d_text(ptab->name);
	     }
	   free((char *)pt);
	   pt=NULL;
	  }
	if (!Inactive && fpout != NULL) prtTt(fpout,ptab);
	return 1;
      } 
