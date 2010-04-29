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
    extern int Inactive;

/*	Remove a box fill graphics assignment.
	The string defining the name must be in "str".			*/

    int removeGfb_name(char *str)
      {
       struct gfb_tab *p,*ptb;
       struct display_tab *pd;
       struct fill_range *piso,*piso1;

       if (str == NULL || strprt(str) == 0) return 0;

       if (strcmp(str,Gfb_tab.name) == 0)
	 {
	  err_warn(1,fperr,
		"Error - Boxfill default (Gfb_%s) can't be removed.\n",str);
	  return 0;
	 }

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && cmpncs(pd->type,"boxfill") == 0 &&
						 strcmp(pd->g_name,str) == 0)
	    {
	     err_warn(1,fperr,
	      "Error - Graphics Boxfill (Gfb_%s) is in use, not removed.\n",
									str);
	     return 0;
	    }
	 }
       for (p=ptb=&Gfb_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str,p->name) == 0)
	    {
	     ptb->next=p->next;
	     if (p->pGfb_attr->legend != NULL)
		free((char *)p->pGfb_attr->legend);
	     piso1=p->pGfb_attr->line;
	     while (piso1 != NULL) {
	        piso=piso1->next;
	        free((char *) piso1);
	        piso1=piso;
	     }
	     free((char *)p->pGfb_attr);
	     free((char *)p);
	     if (!Inactive && fpout != NULL)
			fprintf(fpout,"REMOVE(Gfb_%s)\n",str);
	     return 1;
	    }
	  ptb=p;
	 }
       if (p == NULL)
	 {
	  err_warn(1,fperr,
	    "Error - Graphics Boxfills (Gfb_%s) not found for remove.\n",str);
	  return 0;
	 }
       return 1;
      }

/*		Rename a Graphics Boxfills.  (str2 -> str1)		*/

    int renameGfb_name(char *str1,char *str2)
      {
       int i,j;
       struct gfb_tab *p;
       struct display_tab *pd;

       if (str1==NULL || strprt(str1)==0 || str2==NULL || strprt(str2)==0)
					return 0;
/*		Check whether the old or new Boxfills is a default.	*/

       if (strcmp(str1,Gfb_tab.name) == 0)
	 {
	  err_warn(1,fperr,
		"Error - default Boxfill (Gfb_%s) can't be renamed.\n",str1);
	  return 0;
	 }
       if (strcmp(str2,Gfb_tab.name) == 0) 
	 {
	  err_warn(1,fperr,
	   "Error - can't rename (Gfb_%s) to the default Boxfills (Gfb_%s)\n",
						str1,str2);
	  return 0;
	 }

/*		Check whether the old Boxfill name is used in a display.*/

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && strcmp(pd->g_name,str1) == 0)
	    {
	     err_warn(1,fperr,
	      "Error - Boxfills (Gfb_%s) is in use, not renamed.\n",str1);
	     return 0;
	    }
	 }
/*		Check whether the new Boxfill name is in use.		*/

       for (p=&Gfb_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	     "Error - Boxfills (Gfb_%s) exists. Can't rename (Gfb_%s).\n",
							str2,str1);
	     return 0;
	    }
	 }
/*	Find the old Boxfill name.  Rename if it isn't the default.	*/

      for (p=&Gfb_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str1,p->name) == 0)
	    {
	     strcpy(p->name,str2);
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(Gfb_%s,Gfb_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(1,fperr,
	"Error - Graphics Boxfills (Gfb_%s) can't be found to rename.\n",str1);
       return 0;
      }

/*		Copy Graphics Boxfill to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_Gfb_name(char *str1,char *str2)
      {
       int i,j;
       struct gfb_tab *p,*ptb,*p1;
       struct display_tab *pd;
       struct gfb_attr *pa,*pa1;
       struct fill_range *pifr,*pifr1,*pifr2,*pifrn;

       if (str1==NULL || strprt(str1)==0 || str2==NULL || strprt(str2)==0)
							 return 0;

/*			Copy to itself - not necessary.			*/

       if (strcmp(str1,str2) == 0) return 1;

/*			Can't copy to default Boxfill.			*/

       if (strcmp(Gfb_tab.name,str2) == 0)
	 {
	  err_warn(1,fperr,
	  	"Error - Can't copy to default Boxfill (Gfb_%s).\n",str2);
	  return 0;
	 }
/*		Is it a copy to an existing Boxfill attribute set.	*/

       for (ptb=p=&Gfb_tab;p != NULL;ptb=p,p=p->next)
	 if (strcmp(str2,p->name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - Can't copy Boxfills (Gfb_%s) to existing (Gfb_%s).\n",
							str1,str2);
	  return 0;
	 }

/*		Search Boxfill table for attributes to be copied.	*/

       for (p1=&Gfb_tab;p1 != NULL;p1=p1->next)
				  if (strcmp(str1,p1->name) == 0) break;

       if (p1 == NULL || p1->pGfb_attr == NULL)
	 {
	  err_warn(1,fperr,
	  	"Error - Boxfills (Gfb_%s) not found for copy.\n",str1);
	  return 0;
	 }
       pa1=p1->pGfb_attr;

/*		Add a table entry for the new attribute set.		*/

       if((p=(ptb->next)=(struct gfb_tab *)malloc(sizeof(Gfb_tab))) == NULL)
	 {
	  err_warn(1,fperr,
	  "Error - memory for Graphics Boxfills (Gfb_%s) not found for copy.\n",
								str2);
	  return 0;
	 }
       p->next=NULL;
       strcpy(p->name,str2);

/*		Create a new attribute structure and copy to it.	*/

       if((pa=(struct gfb_attr *)malloc(sizeof(struct gfb_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	 "Error - memory for Graphics Boxfills (Gfb_%s) not found for copy.\n",
								str1);
	  free((char *)p);
	  return 0;
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
       strncpy(pa->ymt1,pa1->ymt1,256);pa->ymt1[255] = '\0';
       strncpy(pa->ymt2,pa1->ymt2,256); pa->ymt2[255] = '\0';
       strncpy(pa->timeunits,pa1->timeunits,256); pa->timeunits[255] = '\0';
       for (i=0;i<4;i++) pa->dsp[i]=pa1->dsp[i];
       for (i=0;i<4;i++) pa->idsp[i]=pa1->idsp[i];
       pa->calendar=pa1->calendar;
      
       strncpy(pa->xat,pa1->xat,17); pa->xat[16] = '\0';
       strncpy(pa->yat,pa1->yat,17); pa->yat[16] = '\0';
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

       for (pifr1=pa1->line,pifrn=NULL;pifr1 != NULL;pifr1=pifr1->next)
	 {
	  if ((pifrn=(struct fill_range *)
			malloc(sizeof(struct fill_range)))==NULL)
	    {
	     err_warn(1,fperr,
	"Error - memory for Graphics Boxfill (Gfb_%s) not found for copy./n",
							str2);
	     pifr1=pa->line;
	     free((char *)pa);
	     while (pifr1 != NULL)
	       {
		pifr=pifr1->next;
		free((char *) pifr1);
		pifr1=pifr;
	       }
	     return 0;
	    }
	  if (pa->line == NULL) pa->line=pifrn;
	  else pifr->next=pifrn;
	  pifr=pifrn;

	  pifr->id=pifr1->id;
	  pifr->lev1=pifr1->lev1;
	  pifr->lev2=pifr1->lev2;
	  strncpy(pifr->fill_name,pifr1->fill_name,17); pifr->fill_name[16] = '\0';
	  pifr->next=NULL;
	 }

/*		Create a table entry.				*/

       if((p=(ptb->next)=(struct gfb_tab *)malloc(sizeof(Gfb_tab))) == NULL)
	 {
	  err_warn(1,fperr,
	  "Error - memory for Graphics Boxfill (Gfb_%s) not found for copy.\n",
								str1);
	  pifr1=pa->line;
	  free((char *)pa);
	  while (pifr1 != NULL)
	    {
	     pifr=pifr1->next;
	     free((char *) pifr1);
	     pifr1=pifr;
	    }
	  return 0;
	 }
       p->pGfb_attr=pa;
       p->next=NULL;
       strcpy(p->name,str2);

       if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(Gfb_%s,Gfb_%s)\n",str1,str2);
       return 1;
      }
