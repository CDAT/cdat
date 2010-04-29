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

    extern struct gfm_tab Gfm_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Remove an meshfill graphics assignment.
	The string defining the name must be in "str".		*/

    int removeGfm_name(char *str)
      {
       int i;
       struct gfm_tab *p,*ptb;
       struct display_tab *pd;
       struct gfm_attr *pa;
       struct fill_range *piso,*piso1;

       if (str == NULL || strprt(str) == 0) return 0;

       if (strcmp(str,Gfm_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	  "Error - The default Graphics Meshfills (Gfm_%s) can't be removed.\n",
								str);
	  return 0;
	 }

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && cmpncs(pd->type,"meshfill") == 0 &&
					 strcmp(pd->g_name,str) == 0)
	    {
	     err_warn(1,fperr,
	         "Error - Graphics Meshfills (Gfm_%s) is in use, not deleted.\n",
							str);
	     return 0;
	    }
	 }
       for (p=ptb=&Gfm_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str,p->name) == 0)
	    {
	     ptb->next=p->next;
	     if (p->pGfm_attr->legend != NULL)
                free((char *)p->pGfm_attr->legend);
	     pa=p->pGfm_attr;
	     piso1=pa->line;
	     free((char *)pa);
	     while (piso1 != NULL)
	       {
		piso=piso1->next;
		free((char *) piso1);
		piso1=piso;
	       }
	     free((char *)p);
	     if (!Inactive && fpout != NULL)
			fprintf(fpout,"REMOVE(Gfm_%s)\n",str);
	     return 1;
	    }
	  ptb=p;
	 }
       err_warn(1,fperr,
	 "Error - Graphics Meshfills (Gfm_%s) can't be found to remove.\n",str);
       return 0;
      }

/*		Rename a Graphics Meshfill.  (str2 -> str1)		*/

    int renameGfm_name(char *str1,char *str2)
      {
       int i,j;
       char s[17];
       struct gfm_tab *p;
       struct display_tab *pd;
       struct fill_range *piso;

       if (str1==NULL || str2==NULL || strprt(str1)==0 || strprt(str2)==0) 
								return 0;

       if (strcmp(str1,Gfm_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	    "Error - The default Graphics Meshfill (Gfm_%s) can't be renamed.\n",
								str1);
	  return 0;
	 }
       if (strcmp(s,Gfm_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	    "Error - Can't rename (Gfm_%s) to the default Meshfills (Gfm_%s).\n",
								str1,str2);
	  return 0;
	 }
       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && strcmp(pd->g_name,str1) == 0)
	    {
	     err_warn(1,fperr,
		      "Error - Graphics Meshfills (Gfm_%s) is in use."
			"  Can't rename (Gfm_%s).\n",str2,str1);
	     return 0;
	    }
	 }

       for (p=&Gfm_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s,p->name) == 0)
	    {
	     err_warn(1,fperr,
      "Error - Can't rename Graphics Meshfills (Gfm_%s) to existing (Gfm_%s).\n",
					str1,str2);
	     return 0;
	    }
	 }
      for (p=&Gfm_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str1,p->name) == 0)
	    {
	     strcpy(p->name,str2);
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(Gfm_%s,Gfm_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(1,fperr,
	    "Error - Graphics Meshfills (Gfm_%s) can't be found to rename.\n",
						str1);
       return 0;
      }

/*		Copy Graphics Fill between Meshlines to another (str1 -> str2)
		if a name exists in str2.				*/

    int copy_Gfm_name(char *str1,char *str2)
      {
       int i;
       struct gfm_tab *p,*ptb,*p1;
       struct display_tab *pd;
       struct gfm_attr *pa,*pa1;
       struct fill_range *pifr,*pifr1,*pifr2,*pifrn;

       if (str1==NULL || str2==NULL || strprt(str1)==0 || strprt(str2)==0)
								return 0;
/*		It's an internal copy of meshfill descriptions.	*/

       if (strcmp(str1,str2) == 0) return 1;

/*		Is it a copy to the default meshfill attribute set.	*/

       if (strcmp(str2,Gfm_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - Can't copy to the default Graphics Meshfills (Gfm_%s).\n",
							str2);
	  return 0;
	 }

/*		Is it a copy to an existing Meshfill attribute set.	*/

       for (ptb=p=&Gfm_tab;p != NULL; ptb=p,p=p->next)
	 if (strcmp(str2,p->name) == 0)
	 {
	  err_warn(1,fperr,
	   "Errof - Can't copy Meshfills (Gfm_%s) to existing (Gfm_%s).\n",
							str1,str2);
	  return 0;
	 }

/*		Search Meshfill table for attributes to be copied.	*/

       for (p1=&Gfm_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str1,p1->name) == 0) break;

       if (p1 == NULL || p1->pGfm_attr == NULL)
	 {
	  err_warn(1,fperr,
	   "Error - Graphics Meshfills (Gfm_%s) not found for copy.\n",str1);
	  return 0;
	 }

       pa1=p1->pGfm_attr;

/*		Create a new attribute structure and copy to it.	*/

       if((pa=(struct gfm_attr *)malloc(sizeof(struct gfm_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	  "Error - memory for Graphics Meshfills (Gfm_%s) not found for copy.\n",
							str2);
	  return 0;
	 }
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
       strncpy(pa->xat,pa1->xat,17); pa->xat[16] = '\0';
       strncpy(pa->yat,pa1->yat,17); pa->yat[16] = '\0';
       pa->missing=pa1->missing;
       pa->xwrap=pa1->xwrap;
       pa->ywrap=pa1->ywrap;
       pa->mesh=pa1->mesh;
       
       if (pa1->legend != NULL) {
          if ((pa->legend=(char *)malloc((strlen(pa1->legend))*sizeof(char)+1))
                == NULL) {
              err_warn(1,fperr, "Error - memory for legend(%s) changes not found. \n",
                         pa1->legend);
              return 0;
	  }
	  strcpy(pa->legend, pa1->legend);
       } else
          pa->legend=pa1->legend;

       for (pifr1=pa1->line,pifrn=NULL;pifr1 != NULL;pifr1=pifr1->next)
	 {
	  if ((pifrn=(struct fill_range *)
			malloc(sizeof(struct fill_range)))==NULL)
	    {
	     err_warn(1,fperr,
	"Error - memory for Graphics Meshfills (Gfm_%s) not found for copy./n",
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

       if((p=(ptb->next)=(struct gfm_tab *)malloc(sizeof(Gfm_tab))) == NULL)
	 {
	  err_warn(1,fperr,
	  "Error - memory for Graphics Meshfills (Gfm_%s) not found for copy.\n",
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
       p->pGfm_attr=pa;
       p->next=NULL;
       strcpy(p->name,str2);

       if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(Gfm_%s,Gfm_%s)\n",str1,str2);
       return 1;
      }
