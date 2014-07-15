
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

    extern struct table_form Th_tab;
    extern struct display_tab D_tab;

    extern int update_ind;
    extern int Inactive;

    extern int I,J,K,L,M,N;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

/*		Remove a Form name and its values.			*/

    int removeTh_name(str)

      char str[];
      {
	struct table_form *ptab,*ptb;
	struct form *pf,*pf1;
	int upd;

	for (ptab=ptb=&Th_tab;ptab != NULL; ptb=ptab,ptab=ptab->next)
	  {
	   if (cmpnbl(str,ptab->name) == 0)
	     {
	      if (ptab == &Th_tab)
		{
		 err_warn(0,fperr,
			"Warning - Form (%s) default can't be removed.\n",str);
		 return 0;
		}
	      upd=update_ind;
	      update_ind=0;
	      check_d_fmt(str);
	      if (update_ind == 0)
		{
		 ptb->next=ptab->next;
		 killTh(ptab);
		 update_ind=upd;
		}
	      else
		{
		 err_warn(0,fperr,
			"Warning - Form (%s) in use can't be removed.\n",str);
		 return 0;
		}
	      if (!Inactive && fpout != NULL)
		fprintf(fpout,"REMOVE(Th_%s)\n",str);
	      return 1;
	     }
	  }
	err_warn(0,fperr,"Warning - Form (%s) can't be found to remove.\n",str);

	return 0;
      }

/*		Rename a Form descriptor.  (str2 -> str1)		*/

    int renameTh_name(char *str1,char *str2)

      {
       int i,j;
       char s2[17];
       char s1[17];
       struct table_form *p;

       for (i=0,j=0;str2 != NULL && i < 16 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';
       for (i=0,j=0;str1 != NULL && i < 16 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';
       if (str2 == NULL || str1 == NULL || strprt(s1) == 0 || strprt(s2) == 0)
	 {
	  err_warn(0,fperr,
		"Warning - a Form name is empty, not renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,Th_tab.name) == 0)
	 {
	  err_warn(1,fperr,"Error - the default form can't be renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0)
	 {
	  err_warn(0,fperr,"Warning - rename (%s) to (%s).\n",s1,s2);
	  return 1;
	 }
       for (p=&Th_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	     "Error - rename to (%s) would duplicate an existing name.\n",s2);
	     return 0;
	    }
	 }
       for (p=&Th_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s1,p->name) == 0)
	    {
	     check_d_fmt(s1);
	     if (update_ind == 0)
		strcpy(p->name,s2);
	     else
	       {
		 err_warn(0,fperr,
			"Warning - Form (%s) in use can't be renamed.\n",s1);
		 return 0;
	       }
	      if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(Th_%s,Th_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(0,fperr,"Warning - Form (%s) can't be found.\n",
							str1);
       return 0;
      }


/*		Copy Format to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_Th_name(char *str1,char *str2)
      {
       int i,j;
       struct table_form *p,*p1;
       struct form *pf,*ptf,*pf1;

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
		"Error - Form name is empty, not copied.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0) return 1;

/*		If a target form name isn't given return 0.		*/

       if (str2 == NULL || strprt(s2) == 0) return 0;

/*		Search Form table for Form to be created.		*/
       for (p=&Th_tab;p != NULL;p=p->next)
	if (strcmp(s2,p->name) == 0)
	  {
	   err_warn(1,fperr,
	      "Error - Form (%s) already exists copy would duplicate it.\n",s2);
	   return 0;
	  }	
/*		Search form table for form to be copied.		*/
       for (p1=&Th_tab;p1 != NULL;p1=p1->next)
		 if (strcmp(s1,p1->name) == 0) break;

       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Form (%s) can't be found for copy.\n",str1);
	  return 0;
	 }

/*		Find the last form table entry.				*/

       for (p=&Th_tab;p != NULL;p=p->next) 
		if (p->next == NULL) break;

       if (p->next == NULL &&
	(p->next=(struct table_form *)malloc(sizeof(struct table_form)))==NULL)
	 {
	  err_warn(1,fperr,
	 	 "Error - memory for copy form (%s) can't be found.\n",s2);
	  return 0;
	 }
	p=p->next;
	strcpy (p->name,s2);
	p->next=NULL;
	p->variety=NULL;
	p->next=NULL;
	for (pf1=p1->variety;pf1!=NULL;pf1=pf1->next)
	  {
	   if ((pf=(struct form *)malloc(sizeof(struct form)))==NULL)
	     {
	      err_warn(1,fperr,
	 	 "Error - memory for copy form (%s) can't be found.\n",s2);
	      return 0;
	     }
	   if (p->variety == NULL) ptf=p->variety=pf;
	   else ptf->next=pf;
	   strcpy(pf->s_name,pf1->s_name);
	   strcpy(pf->s_units,pf1->s_units);
	   strcpy(pf->format,pf1->format);
	   pf->next=NULL;
	   ptf=pf;
	  }
        if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(Th_%s,Th_%s)\n",str1,str2);
      
       return 1;
      }
