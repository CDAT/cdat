
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

    extern struct table_text Tt_tab;
    extern struct display_tab D_tab;

    extern int update_ind;
    extern int Inactive;

    extern int I,J,K,L,M,N;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

/*		Remove a Text name and its values.			*/

    int removeTt_name(str)

      char str[];
      {
	struct table_text *ptab,*ptb;

	for (ptab=ptb=&Tt_tab;ptab != NULL; ptb=ptab,ptab=ptab->next)
	  {
	   if (cmpnbl(str,ptab->name) == 0)
	     {
	      if (ptab == &Tt_tab)
		{
		 err_warn(0,fperr,
			"Warning - Text (%s) default can't be removed.\n",str);
		 return 0;
		}
	      ptb->next=ptab->next;
	      check_d_text(str);
	      if (update_ind == 0)
		{
	         ptb->next=ptab->next;
                 free_points( &ptab->tx );
                 free_points( &ptab->ty );
                 free_strings( &ptab->ts );
		 free((char *)ptab);
		}
	      else
		{
		 err_warn(0,fperr,
			"Warning - Text (%s) in use can't be removed.\n",str);
		 return 0;
		}
	      if (!Inactive && fpout != NULL)
		fprintf(fpout,"REMOVE(Tt_%s)\n",str);
	      return 1;
	     }
	  }
	err_warn(0,fperr,"Warning - Text (%s) can't be found to remove.\n",str);

	return 0;
      }

/*		Rename a Text descriptor.  (str2 -> str1)		*/

    int renameTt_name(char *str1,char *str2)

      {
       int i,j;
       char s2[17];
       char s1[17];
       struct table_text *p;

       for (i=0,j=0;str2 != NULL && i < 16 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';
       for (i=0,j=0;str1 != NULL && i < 16 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';
       if (str2 == NULL || str1 == NULL || strprt(s1) == 0 || strprt(s2) == 0)
	 {
	  err_warn(0,fperr,
		"Warning - a Text name is empty, not renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,Tt_tab.name) == 0)
	 {
	  err_warn(1,fperr,"Error - the default text can't be renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0)
	 {
	  err_warn(0,fperr,"Warning - rename (%s) to (%s).\n",s1,s2);
	  return 1;
	 }
       for (p=&Tt_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	     "Error - rename to (%s) would duplicate an existing name.\n",s2);
	     return 0;
	    }
	 }
       for (p=&Tt_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s1,p->name) == 0)
	    {
	     check_d_text(s1);
	     if (update_ind == 0)
		strcpy(p->name,s2);
	     else
	       {
		 err_warn(0,fperr,
			"Warning - Text (%s) in use can't be renamed.\n",s1);
		 return 0;
	       }
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(Tt_%s,Tt_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(0,fperr,"Warning - Text (%s) can't be found.\n",
							str1);
       return 0;
      }

/*		Copy Text to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_Tt_name(char *str1,char *str2)
      {
       int i,j;
       struct table_text *p,*p1;
       char s1[256];
       char s2[256];

       for (i=0,j=0;str2 != NULL && i < 16 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';
       for (i=0,j=0;str1 != NULL && i < 16 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';
       if (str1 == NULL || strprt(s1) == 0)
	 {
	  err_warn(1,fperr,
		"Error - Text name is empty, not copied.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0) return 1;

/*		If a target text name isn't given return 0.		*/

       if (str2 == NULL || strprt(s2) == 0) return 0;

/*		Search Text table for Text to be created.		*/
       for (p=&Tt_tab;p != NULL;p=p->next)
	if (strcmp(s2,p->name) == 0)
	  {
	   err_warn(1,fperr,
	      "Error - Text (%s) already exists copy would duplicate it.\n",s2);
	   return 0;
	  }	
/*		Search text table for text to be copied.		*/
       for (p1=&Tt_tab;p1 != NULL;p1=p1->next)
		 if (strcmp(s1,p1->name) == 0) break;

       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Text (%s) can't be found for copy.\n",str1);
	  return 0;
	 }

/*		Find the last text table entry.				*/

       for (p=&Tt_tab;p != NULL;p=p->next) 
		if (p->next == NULL) break;

       if (p->next == NULL &&
	(p->next=(struct table_text *)malloc(sizeof(struct table_text)))==NULL)
	 {
	  err_warn(1,fperr,
	 	 "Error - memory for text (%s) can't be found.\n",s2);
	  return 0;
	 }
	p=p->next;
	strcpy (p->name,s2);
	strcpy (p->proj,p1->proj);
	p->next=NULL;
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
        if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(Tt_%s,Tt_%s)\n",str1,str2);
      
       return 1;
      }
