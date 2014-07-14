
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "list.h"
#include "display.h"
#include "array.h"
#include "graph.h"
#include "picture.h"

#define STRMAX 256

    extern struct table_chorn To_tab;
    extern struct display_tab D_tab;

    extern int update_ind;
    extern int Inactive;

    extern int I,J,K,L,M,N;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

/*		Remove a Chorn name and its values.			*/

    int removeTo_name(str)

      char str[];
      {
	struct table_chorn *ptab,*ptb;

	for (ptab=ptb=&To_tab;ptab != NULL; ptb=ptab,ptab=ptab->next)
	  {
	   if (cmpnbl(str,ptab->name) == 0)
	     {
	      if (ptab == &To_tab)
		{
		 err_warn(0,fperr,
			"Warning - Chorn (%s) default can't be removed.\n",str);
		 return 0;
		}
	      ptb->next=ptab->next;
	      check_d_chorn(str);
	      if (update_ind == 0)
		{
	         ptb->next=ptab->next;
		 free((char *)ptab);
		}
	      else
		{
		 err_warn(0,fperr,
			"Warning - Chorn (%s) in use can't be removed.\n",str);
		 return 0;
		}
	      if (!Inactive && fpout != NULL)
			fprintf(fpout,"REMOVE(To_%s)\n",str);
	      return 1;
	     }
	  }
	err_warn(0,fperr,
		"Warning - Chorn (%s) can't be found to remove.\n",str);

	return 0;
      }

/*		Rename a Chorn descriptor.  (str2 -> str1)		*/

    int renameTo_name(char *str1,char *str2)

      {
       int i,j;
       char s2[17];
       char s1[17];
       struct table_chorn *p;

       for (i=0,j=0;str2 != NULL && i < 16 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';
       for (i=0,j=0;str1 != NULL && i < 16 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';
       if (str2 == NULL || str1 == NULL || strprt(s1) == 0 || strprt(s2) == 0)
	 {
	  err_warn(0,fperr,
		"Warning - a Chorn name is empty, not renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,To_tab.name) == 0)
	 {
	  err_warn(1,fperr,"Error - the default chorn can't be renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0)
	 {
	  err_warn(0,fperr,"Warning - rename (%s) to (%s).\n",s1,s2);
	  return 1;
	 }
       for (p=&To_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	     "Error - rename to (%s) would duplicate an existing name.\n",s2);
	     return 0;
	    }
	 }
       for (p=&To_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s1,p->name) == 0)
	    {
	     check_d_chorn(s1);
	     if (update_ind == 0)
		strcpy(p->name,s2);
	     else
	       {
		 err_warn(0,fperr,
			"Warning - Chorn (%s) in use can't be renamed.\n",s1);
		 return 0;
	       }
	     if (!Inactive && fpout != NULL)
			fprintf(fpout,"RENAME(To_%s,To_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(0,fperr,"Warning - Chorn (%s) can't be found.\n",
							str1);
       return 0;
      }


/*		Copy Text Orientation to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_To_name(char *str1,char *str2)
      {
       int i,j;
       struct table_chorn *p,*p1;
       char s1[17];
       char s2[17];

       for (i=0,j=0;str2 != NULL && i < 16 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';
       for (i=0,j=0;str1 != NULL && i < 16 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';
       if (str1 == NULL || strprt(s1) == 0)
	 {
	  err_warn(1,fperr,
		"Error - Chorn name is empty, not copied.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0) return 1;

/*		If a target chorn name isn't given return 0.	*/

       if (str2 == NULL || strprt(s2) == 0) return 0;

/*		Search Chorn table for Chorn to be created.		*/
       for (p=&To_tab;p != NULL;p=p->next)
	if (strcmp(s2,p->name) == 0)
	  {
	   err_warn(1,fperr,
	      "Error - Chorn (%s) already exists copy would duplicate.\n",s2);
	   return 0;
	  }	
/*		Search chorn table for chorn to be copied.		*/
       for (p1=&To_tab;p1 != NULL;p1=p1->next)
		 if (strcmp(s1,p1->name) == 0) break;

       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Chorn (%s) can't be found for copy.\n",str1);
	  return 0;
	 }

/*		Find the last chorn table entry.			*/

       for (p=&To_tab;p != NULL;p=p->next) 
		if (p->next == NULL) break;

       if (p->next == NULL &&
	(p->next=(struct table_chorn *)
		malloc(sizeof(struct table_chorn)))==NULL)
	 {
	  err_warn(1,fperr,
	 	 "Error - memory for chorn (%s) can't be found.\n",s2);
	  return 0;
	 }
	p=p->next;
	strcpy (p->name,s2);
	p->next=NULL;
	p->chh=p1->chh;
	p->chua=p1->chua;
	p->chpath=p1->chpath;
	p->chalh=p1->chalh;
	p->chalv=p1->chalv;
      
	if (!Inactive && fpout != NULL)
			fprintf(fpout,"COPY(To_%s,To_%s)\n",str1,str2);
       return 1;
      }
