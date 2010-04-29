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
    extern int Inactive;

/*	Remove a continents assignment.
	The string defining the name must be in "str".			*/

    int removeGcon_name(char *str)
      {
       struct gcon_tab *p,*ptb;
       struct display_tab *pd;

       if (str == NULL || strprt(str) == 0) return 0;

       if (strcmp(str,Gcon_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	    "Error - The default Continents (Gcon_%s) can't be removed.\n",
								str);
	  return 0;
	 }

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && cmpncs(pd->type,"continents") == 0 &&
						 strcmp(pd->g_name,str) == 0)
	    {
	     err_warn(1,fperr,
	      "Error - Continents (Gcon_%s) is in use, not removed.\n",str);
	     return 0;
	    }
	 }
       for (ptb=p=&Gcon_tab;p != NULL;ptb=p,p=p->next)
	 {
	  if (strcmp(str,p->name) == 0)
	    {
	     ptb->next=p->next;
	     free((char *)p->pGcon_attr);
	     free((char *)p);
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"REMOVE(Gcon_%s)\n",str);
	     return 1;
	    }
	 }
       err_warn(1,fperr,
		"Error - Continents (Gcon_%s) not found to remove.\n",str);
       return 0;
      }

/*		Rename a Continents.  (str2 -> str1)		*/

    int renameGcon_name(char *str1,char *str2)
      {
       struct gcon_tab *p;
       struct display_tab *pd;

       if (str2==NULL || strprt(str2)==0 || str1==NULL || strprt(str1)==0)
							return 0;

       if (strcmp(str1,Gcon_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - The default Continents (Gcon_%s) can't be renamed.\n",
								str1);
	  return 0;
	 }
       if (strcmp(str2,Gcon_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	"Error - Can't rename (Gcon_%s) to the default Continents (Gcon_%s).\n",
								str1,str2);
	  return 0;
	 }

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && strcmp(pd->g_name,str1) == 0)
	    {
	     err_warn(1,fperr,
	   "Error - continents (Gcon_%s) is in use.  Can't rename (Gcon_%s).\n",
								str1);
	     return 0;
	    }
	 }
       for (p=&Gcon_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	   "Error - Can't rename Continents (Gcon_%s) to existing (Gcon_%s).\n",
							str1,str2);
	     return 0;
	    }
	 }
      for (p=&Gcon_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str1,p->name) == 0)
	    {
	     strcpy(p->name,str2);
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(Gcon_%s,Gcon_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(1,fperr,
	   "Error - Continents (Gcon_%s) can't be found to rename.\n",str1);
       return 0;
      }

/*		Copy Graphics Continents to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_Gcon_name(char *str1,char *str2)
      {
       int i,j;
       struct gcon_tab *p,*ptb,*p1;
       struct display_tab *pd;
       struct gcon_attr *pa,*pa1;

       if (str1==NULL || strprt(str1)==0 || str2==NULL || strprt(str2)==0)
						return 0;

       if (strcmp(str1,str2) == 0) return 1;

       if (strcmp(str2,Gcon_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - Can't copy to the default Continents (Gcon_%s).\n",
							str2);
	  return 0;
	 }

/*		Is it a copy to an existing Continents attribute set.	*/

       for (ptb=p=&Gcon_tab; p != NULL; ptb=p,p=p->next)
	 if (strcmp(str2,p->name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - Can't copy Continents (Gcon_%s) to existing (Gcon_%s).\n",
							str1,str2);
	  return 0;
	 }

/*		Search Continents table for attributes to be copied.	*/

       for (p1=&Gcon_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str1,p1->name) == 0) break;

       if (p1 == NULL || p1->pGcon_attr == NULL)
	 {
	  err_warn(1,fperr,
	   "Error - Continents (Gcon_%s) not found for copy.\n",str1);
	  return 0;
	 }
       pa1=p1->pGcon_attr;

/*		Create a new attribute structure and copy to it.	*/

       if((pa=(struct gcon_attr *)malloc(sizeof(struct gcon_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for Continents(Gcon_%s) not found for copy.\n",
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
       for (i=0;i<4;i++) pa->dsp[i]=pa1->dsp[i];
       strncpy(pa->lb,pa1->lb,17); pa->lb[16] = '\0';
       pa->cont_type = pa1->cont_type;
       
       if((p=(ptb->next)=(struct gcon_tab *)malloc(sizeof(Gcon_tab)))==NULL)
         {
          err_warn(1,fperr,
	     "Error - memory for Continents (Gcon_%s) found for copy.\n",str2);
          free ((char *)pa);
          return 0;
         }
       p->pGcon_attr=pa;
       p->next=NULL;
       strcpy(p->name,str2);

       if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(Gcon_%s,Gcon_%s)\n",str1,str2);
       return 1;
      }
