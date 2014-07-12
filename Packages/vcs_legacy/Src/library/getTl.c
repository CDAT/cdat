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

    extern struct table_line Tl_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Get a line assignment for the named line attributes.		*/

    struct table_line *getTl(char *str)
      {
        float			deflt=1.0;
        struct table_line 	*p,*p1;
        extern int		copy_points();

/*		Search line table for attributes to be copied.	*/

       for (p1=&Tl_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Line attributes (%s) can't be found for get.\n",str);
	  return NULL;
	 }

/*		Create a new line table structure and copy to it.	*/

       if((p=(struct table_line *)malloc(sizeof(struct table_line)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Line table (%s) not found.\n",str);
	  return 0;
	 }

       strcpy(p->name,p1->name);
       strcpy(p->proj,p1->proj);
       copy_int_array( &p->ltyp, &p1->ltyp, &p->ltyp_size, p1->ltyp_size, 1 );
       copy_float_array( &p->lwsf, &p1->lwsf, &p->lwsf_size, p1->lwsf_size, &deflt );
       copy_int_array( &p->lci, &p1->lci, &p->lci_size, p1->lci_size, 241 );
       p->priority=p1->priority;
       p->lvp[0]=p1->lvp[0];
       p->lvp[1]=p1->lvp[1];
       p->lvp[2]=p1->lvp[2];
       p->lvp[3]=p1->lvp[3];
       p->lwc[0]=p1->lwc[0];
       p->lwc[1]=p1->lwc[1];
       p->lwc[2]=p1->lwc[2];
       p->lwc[3]=p1->lwc[3];
       if (copy_points( &p->lx, p1->lx) == 0) return 0;
       if (copy_points( &p->ly, p1->ly) == 0) return 0;
       p->next=NULL;

       return p;
      }

    int chk_mov_Tl (struct table_line *pt)

      {
       float		 deflt=1.0;
       struct table_line *ptb,*ptab;
       extern int	 copy_points();
       extern void	 free_points();

       if (pt == NULL) return 0;
       	for (ptb=ptab=&Tl_tab;
		ptab!=NULL && cmpnbl(pt->name,ptab->name)!=0;
			ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

         if (ptab == &Tl_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (Tl_%s).\n",
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
	   if (pt->ltyp != ptab->ltyp || pt->lwsf != ptab->lwsf ||
	       pt->lci != ptab->lci || pt->priority != ptab->priority ||
               ptab->lvp[0] != pt->lvp[0] || ptab->lvp[1] != pt->lvp[1] ||
               ptab->lvp[2] != pt->lvp[2] || ptab->lvp[3] != pt->lvp[3] ||
               ptab->lwc[0] != pt->lwc[0] || ptab->lwc[1] != pt->lwc[1] ||
               ptab->lwc[2] != pt->lwc[2] || ptab->lwc[3] != pt->lwc[3] ||
               ptab->lx != pt->lx || ptab->ly != pt->ly || (strcmp(ptab->proj,pt->proj) != 0)
               )
	     {
	      strcpy(ptab->proj,pt->proj);
	      copy_int_array( &ptab->ltyp,&pt->ltyp,&ptab->ltyp_size,pt->ltyp_size, 1 );
	      copy_float_array(&ptab->lwsf,&pt->lwsf,&ptab->lwsf_size,pt->lwsf_size,&deflt);
	      copy_int_array( &ptab->lci, &pt->lci, &ptab->lci_size, pt->lci_size, 241 );
              ptab->priority=pt->priority;
	      ptab->lvp[0]=pt->lvp[0];
	      ptab->lvp[1]=pt->lvp[1];
	      ptab->lvp[2]=pt->lvp[2];
	      ptab->lvp[3]=pt->lvp[3];
	      ptab->lwc[0]=pt->lwc[0];
	      ptab->lwc[1]=pt->lwc[1];
	      ptab->lwc[2]=pt->lwc[2];
	      ptab->lwc[3]=pt->lwc[3];
              free_points( &ptab->lx );
              free_points( &ptab->ly );
	      if (copy_points( &ptab->lx, pt->lx) == 0) return 0;
	      if (copy_points( &ptab->ly, pt->ly) == 0) return 0;
              free_points( &pt->lx );
              free_points( &pt->ly );
              if (pt->ltyp != NULL) free((char *) pt->ltyp); pt->ltyp = NULL;
              if (pt->lwsf != NULL) free((char *) pt->lwsf); pt->lwsf = NULL;
              if (pt->lci != NULL) free((char *) pt->lci); pt->lci = NULL;
	      free((char *)pt);
	      pt=NULL;
	      check_d_line(ptab->name);
	     }
	  }
	if (!Inactive && fpout != NULL) prtTl(fpout,ptab);
	return 1;
       } 
