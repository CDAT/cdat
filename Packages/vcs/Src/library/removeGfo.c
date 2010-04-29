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

    extern struct gfo_tab Gfo_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Remove a fill outline graphics assignment.
	The string defining the name must be in "str".			*/

    int removeGfo_name(char *str)
      {
       struct gfo_tab *p,*ptb;
       struct display_tab *pd;

       if (str == NULL || strprt(str) == 0) return 0;

       if (strcmp(str,Gfo_tab.name) == 0)
	 {
	  err_warn(2,fperr,
	      "Error - Can't remove the default Fill Outlines (Gfo_%s).\n",str);
	  return 0;
	 }

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && cmpncs(pd->type,"outfill") == 0 &&
						 strcmp(pd->g_name,str) == 0)
	    {
	     err_warn(1,fperr,
	      "Error - Fill Outlines (Gfo_%s) is in use, not removed.\n",str);
	     return 0;
	    }
	 }
       for (ptb=p=&Gfo_tab;p != NULL;ptb=p,p=p->next)
	 {
	  if (strcmp(str,p->name) == 0)
	    {
	     ptb->next=p->next;
	     free((char *)p->pGfo_attr);
	     free((char *)p);
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"REMOVE(Gfo_%s)\n",str);
	     return 1;
	    }
	 }
       err_warn(1,fperr,
	     "Error - Fill Outlines (Gfo_%s) can't be found to remove.\n",str);
       return 0;
      }

/*		Rename a Fill outline.  (str2 -> str1)		*/

    int renameGfo_name(char *str1,char *str2)
      {
       struct gfo_tab *p;
       struct display_tab *pd;

       if (str2==NULL || strprt(str2)==0 || str1==NULL || strprt(str1)==0)
								return 0;

       if (strcmp(str1,Gfo_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	     "Error - The default Fill outline (Gfo_%s) can't be renamed.\n",
								str1);
	  return 0;
	 }
       if (strcmp(str2,Gfo_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	  "Error - Can't rename to the default Fill Outlines (Gfo_%s).\n",
								str2);
	  return 0;
	 }
       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && strcmp(pd->g_name,str1) == 0)
	    {
	     err_warn(1,fperr,
	   "Error - Fill outline (Gfo_%s) is in use.  Can't rename (Gfo_%s).\n",
								str2,str1);
	     return 0;
	    }
	 }
       for (p=&Gfo_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	   "Error - Can't rename Fill outline (Gfo_%s) to existing (Gfo_%s).\n",
								str1,str2);
	     return 0;
	    }
	 }
      for (p=&Gfo_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str1,p->name) == 0)
	    {
	     strcpy(p->name,str2);
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(Gfo_%s,Gfo_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(1,fperr,
	"Error - Fill Outlines (%s) can't be found to rename.\n",
							str1);
       return 0;
      }

/*		Copy Graphics Fill Outlines to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_Gfo_name(char *str1,char *str2)
      {
       int i,j;
       struct gfo_tab *p,*ptb,*p1;
       struct display_tab *pd;
       struct gfo_attr *pa,*pa1;
       char s[17];

       if (str1==NULL || strprt(str1)==0 || str2==NULL || strprt(str2)==0)
							 return 0;
       if (strcmp(str1,str2) == 0) return 1;

       if (strcmp(str2,Gfo_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	    "Error - Can't copy to the default Fill Outlines (Gfo_%s).\n",
							str2);
	  return 0;
	 }

/*		See if the target Fill Outlines exists.			*/
       for (ptb=p=&Gfo_tab; p != NULL; ptb=p,p=p->next)
	 {
	  if (strcmp(str2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	      "Error - Can't copy to existing Fill Outlines (Gfo_%s).\n",
							str2);
	     return 0;
	    }
	 }

/*		Search Fill outline table for attributes to be copied.	*/

       for (p1=&Gfo_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str1,p1->name) == 0) break;

       if (p1 == NULL || p1->pGfo_attr == NULL)
	 {
	  err_warn(1,fperr,
	   "Error - Fill Outlines (Gfo_%s) can't be found for copy.\n",str1);
	  return 0;
	 }
       pa1=p1->pGfo_attr;

/*		Create a new attribute structure and copy to it.	*/

       if((pa=(struct gfo_attr *)malloc(sizeof(struct gfo_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for Fill Outlines (Gfo_%s) not found for copy.\n",
								str2);
	  return 0;
	 }
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
       strncpy(pa->xat,pa1->xat,17); pa->xat[16] = '\0';
       strncpy(pa->yat,pa1->yat,17); pa->yat[16] = '\0';
       strncpy(pa->f,pa1->f,17); pa->f[16] = '\0';
       pa->n=pa1->n;
       if (pa->n > 0) for (i=0;i<pa->n;i++) pa->out[i]=pa1->out[i];
       
/*		See if the attribute name exists in the fill outline table.
		If not create a table entry.				*/

       if((p=(ptb->next)=(struct gfo_tab *)malloc(sizeof(Gfo_tab))) == NULL)
	 {
	  err_warn(1,fperr,
	     "Error - memory for Fill Outlines (%s) not found for copy.\n",
								str1);
	  free ((char *)pa);
	  return 0;
	 }
       p->pGfo_attr=pa;
       p->next=NULL;
       strcpy(p->name,str2);

       if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(Gfo_%s,Gfo_%s)\n",str1,str2);
       return 1;
      }
