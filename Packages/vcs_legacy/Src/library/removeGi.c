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

    extern struct gi_tab Gi_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Remove an isoline graphics assignment.
	The string defining the name must be in "str".			*/

    int removeGi_name(char *str)
      {
       struct gi_tab *p,*ptb;
       struct display_tab *pd;
       struct gi_attr *pa;
       struct iso *piso,*piso1;

       if (str == NULL || strprt(str) == 0) return 0;

       if (strcmp(str,Gi_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	    "Error - The default Graphics Isolines (Gi_%s) can't be removed.\n",
								str);
	  return 0;
	 }

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && cmpncs(pd->type,"isoline") == 0 &&
						 strcmp(pd->g_name,str) == 0)
	    {
	     err_warn(1,fperr,
	      "Error - Graphics Isolines (Gi_%s) is in use, not removed.\n",
								str);
	     return 0;
	    }
	 }
       for (p=ptb=&Gi_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str,p->name) == 0)
	    {
	     ptb->next=p->next;
	     pa=p->pGi_attr;
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
			fprintf(fpout,"REMOVE(Gi_%s)\n",str);
	     return 1;
	    }
	  ptb=p;
	 }
       err_warn(1,fperr,
	 "Error - Graphics Isolines (Gi_%s) can't be found to remove.\n",str);
       return 0;
      }

/*		Rename a Graphics Isoline.  (str2 -> str1)		*/

    int renameGi_name(char *str1,char *str2)
      {
       struct gi_tab *p;
       struct display_tab *pd;
       struct iso *piso;

       if (str2==NULL || strprt(str2)==0 || str1==NULL || strprt(str1)==0)
							return 0;

       if (strcmp(str1,Gi_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - The default Graphics Isolines (Gi_%s) can't be renamed.\n",
								str1);
	  return 0;
	 }
       if (strcmp(str2,Gi_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	    "Error - Can't rename (Gi_%s) to the default Isolines (Gi_%s).\n",
								str1,str2);
	  return 0;
	 }

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && strcmp(pd->g_name,str1) == 0)
	    {
	     err_warn(1,fperr,
	"Error - Graphics Isolines (Gi_%s) is in use.  Can't rename (Gi_%s).\n",
								str2,str1);
	     return 0;
	    }
	 }

       for (p=&Gi_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	 "Error - Can't rename Graphics Isolines (Gi_%s) to existing (Gi_%s)\n",
							str1,str2);
	     return 0;
	    }
	 }
      for (p=&Gi_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str1,p->name) == 0)
	    {
	     strcpy(p->name,str2);
	     if (!Inactive && fpout != NULL)
			fprintf(fpout,"RENAME(Gi_%s,Gi_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(1,fperr,
	"Error - Graphics Isolines (Gi_%s) can't be found to rename.\n",
						str1);
       return 0;
      }

/*		Copy Graphics Isolines to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_Gi_name(char *str1,char *str2)
      {
       int i;
       struct gi_tab *p,*ptb,*p1;
       struct display_tab *pd;
       struct gi_attr *pa,*pa1;
       struct iso *piso,*piso1,*piso2,*pison;

       if (str1==NULL || strprt(str1)==0 || str2==NULL || strprt(str2)==0)
						return 0;

/*		It's an internal copy of isoline descriptions.		*/

       if (strcmp(str1,str2) == 0) return 1;

       if (strcmp(str2,Gi_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - Can't copy to the default Graphics Isolines (Gi_%s).\n",
							str2);
	  return 0;
	 }

/*		Is it a copy to an existing Isolines attribute set.	*/

       for (ptb=p=&Gi_tab; p != NULL; ptb=p,p=p->next)
	 if (strcmp(str2,p->name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - Can't copy Isolines (Gi_%s) to existing (Gi_%s).\n",
							str1,str2);
	  return 0;
	 }

/*		Search Isoline table for attributes to be copied.	*/

       for (p1=&Gi_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str1,p1->name) == 0) break;

       if (p1 == NULL || p1->pGi_attr == NULL)
	 {
	  err_warn(1,fperr,
	   "Error - Graphics Isolines (Gi_%s) not found for copy.\n",str1);
	  return 0;
	 }
       pa1=p1->pGi_attr;

/*		Create a new attribute structure and copy to it.	*/

       if((pa=(struct gi_attr *)malloc(sizeof(struct gi_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	  "Error - memory for Graphics Isolines (Gi_%s) not found for copy.\n",
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
       pa->labels=pa1->labels;
       
       for (piso1=pa1->line,pison=NULL;piso1 != NULL;piso1=piso1->next)
	 {
	  if ((pison=(struct iso *)malloc(sizeof(struct iso)))==NULL)
	    {
	     err_warn(1,fperr,
	   "Error - memory for Graphics Isolines (Gi_%s) not found for copy./n",
							str2);
	     piso1=pa->line;
	     free((char *)pa);
	     while (piso1 != NULL)
	       {
		piso=piso1->next;
		free((char *) piso1);
		piso1=piso;
	       }
	     return 0;
	    }
	  if (pa->line == NULL) pa->line=pison;
	  else piso->next=pison;
	  piso=pison;

	  piso->id=piso1->id;
	  piso->p=piso1->p;
	  piso->lev=piso1->lev;
	  piso->incr=piso1->incr;
	  piso->hici=piso1->hici;
	  strncpy(piso->lab,piso1->lab,13); piso->lab[12] = '\0';
	  strncpy(piso->lb,piso1->lb,17); piso->lb[16] = '\0';
	  strncpy(piso->tb,piso1->tb,17); piso->tb[16] = '\0';
	  strncpy(piso->to,piso1->to,17); piso->to[16] = '\0';
	  piso->cw=piso1->cw;
	  piso->ls=piso1->ls;
	  piso->angle=piso1->angle;
	  piso->spc=piso1->spc;
	  piso->next=NULL;
	 }

       if((p=(ptb->next)=(struct gi_tab *)malloc(sizeof(Gi_tab))) == NULL)
	 {
	  err_warn(1,fperr,
	  "Error - memory for Graphics Isolines (Gi_%s) not found for copy.\n",
								str2);
	  piso1=pa->line;
	  free((char *)pa);
	  while (piso1 != NULL)
	    {
	     piso=piso1->next;
	     free((char *) piso1);
	     piso1=piso;
	    }
	  return 0;
	 }
       p->pGi_attr=pa;
       p->next=NULL;
       strcpy(p->name,str2);

       if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(Gi_%s,Gi_%s)\n",str1,str2);
       return 1;
      }
