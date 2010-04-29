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

    extern struct gYx_tab GYx_tab;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Remove a Yxvsx graphics assignment.
	The string defining the name must be in "str".		*/

    int removeGYx_name(char *str)
      {
       struct gYx_tab *p,*ptb;
       struct display_tab *pd;
       struct gYx_attr *pa;

       if (str == NULL || strprt(str) == 0) return 0;

       if (strcmp(str,GYx_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	    "Error - The default Graphics Yxvsx (GYx_%s) can't be removed.\n",
								str);
	  return 0;
	 }

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && cmpncs(pd->type,"Yxvsx") == 0 &&
						 strcmp(pd->g_name,str) == 0)
	    {
	     err_warn(0,fperr,
	      "Error - Graphics Yxvsx (GYx_%s) is in use, not removed.\n",str);
	     return 0;
	    }
	 }
       for (p=ptb=&GYx_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str,p->name) == 0)
	    {
	     ptb->next=p->next;
	     pa=p->pGYx_attr;
	     free((char *)pa);
	     free((char *)p);
	     if (!Inactive && fpout != NULL)
			fprintf(fpout,"REMOVE(GYx_%s)\n",str);
	     return 1;
	    }
	  ptb=p;
	 }
       err_warn(1,fperr,
		"Error - Graphics Yxvsx (GYx_%s) not found to remove.\n",str);
       return 0;
      }

/*		Rename a Graphics Y(x) vs x.  (str2 -> str1)		*/

    int renameGYx_name(char *str1,char *str2)
      {
       struct gYx_tab *p;
       struct display_tab *pd;

       if (str2==NULL || strprt(str2)==0 || str1==NULL || strprt(str1)==0)
							return 0;

       if (strcmp(str1,GYx_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - The default Graphics Yxvsx (GYx_%s) can't be renamed.\n",
								str1);
	  return 0;
	 }
       if (strcmp(str2,GYx_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	    "Error - Can't rename (GYx_%s) to the default Yxvsx (GYx_%s).\n",
								str1,str2);
	  return 0;
	 }

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && strcmp(pd->g_name,str1) == 0)
	    {
	     err_warn(1,fperr,
	 "Error - Graphics Yxvsx (GYx_%s) is in use.  Can't rename (GYx)%s).\n",
								str2,str1);
	     return 0;
	    }
	 }

       for (p=&GYx_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	 "Error - Can't rename Graphics Yxvsx (GYx_%s) to existing (GYx_%s).\n",
								str1,str2);
	     return 0;
	    }
	 }
       for (p=&GYx_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str1,p->name) == 0)
	    {
	     strcpy(p->name,str2);
	     if (!Inactive && fpout != NULL)
			fprintf(fpout,"RENAME(GYx_%s,GYx_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(1,fperr,
	"Error - Graphics Yxvsx (GYx_%s) can't be found to rename.\n",
					str1);
       return 0;
      }

/*		Copy Graphics Yxvx to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_GYx_name(char *str1,char *str2)
      {
       int i;
       struct gYx_tab *p,*ptb,*p1;
       struct display_tab *pd;
       struct gYx_attr *pa,*pa1;

       if (str1==NULL || strprt(str1)==0 || str2==NULL || strprt(str2)==0)
						return 0;

/*		It's not an internal copy of Y(x) vs x descriptions.	*/

       if (strcmp(str1,str2) == 0) return 1;

       if (strcmp(str2,GYx_tab.name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - Can't copy to the default Graphics Yxvsx (GYx_%s).\n",
							str2);
	  return 0;
	 }

/*		Is it a copy to an existing Yxvsx attribute set.	*/

       for (ptb=p=&GYx_tab; p != NULL; ptb=p,p=p->next)
	 if (strcmp(str2,p->name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - Can't copy Yxvsx (GYx_%s) to existing (GYx_%s).\n",
							str1,str2);
	  return 0;
	 }

/*		Search GYx table for attributes to be copied.		*/

       for (p1=&GYx_tab;p1 != NULL;p1=p1->next)
	  if (strcmp(str1,p1->name) == 0) break;

       if (p1 == NULL || p1->pGYx_attr == NULL)
	 {
	  err_warn(1,fperr,
	   "Error - Graphics Yxvsx (GYx_%s) not found for copy.\n",str1);
	  return 0;
	 }
       pa1=p1->pGYx_attr;

/*		Create a new attribute structure and copy to it.	*/

       if((pa=(struct gYx_attr *)malloc(sizeof(struct gYx_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for Graphics Yxvsx (GYx_%s) not found for copy.\n",
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
       strncpy(pa->lb,pa1->lb,17); pa->lb[16] = '\0';
       strncpy(pa->mb,pa1->mb,17); pa->mb[16] = '\0';

       if((p=(ptb->next)=(struct gYx_tab *)malloc(sizeof(GYx_tab))) == NULL)
	 {
	  err_warn(1,fperr,
	     "Error - memory for Graphics Yxvsx (GYx_%s) not found for copy.\n",
								str2);
	  free((char *)pa);
	  return 0;
	 }
       p->pGYx_attr=pa;
       p->next=NULL;
       strcpy(p->name,str2);


       if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(GYx_%s,GYx_%s)\n",str1,str2);
       return 1;
      }
