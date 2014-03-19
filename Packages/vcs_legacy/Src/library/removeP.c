#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "picture.h"
#include "display.h"

#define STRMAX 256

    extern FILE *fpin,*fpout,*fperr;

    extern struct p_tab Pic_tab;
    extern struct p_tab Pic_tab_dud;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Remove a template assignment.
	The string defining the name must be in "str".		*/

    int removeP_name(char *str)
      {
       struct p_tab *p,*ptb;
       struct display_tab *pd;

       if (str == NULL || str[0] < ' ') return 0;

       if (strcmp(str,Pic_tab.name) == 0 || strcmp(str,Pic_tab_dud.name) == 0)
	 {
	  err_warn(1,fperr,
		"Error - the default template (P_%s) can't be removed.\n",str);
	  return 0;
	 }
       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && strcmp(pd->p_name,str) == 0)
	    {
	     err_warn(1,fperr,
	      "Error - the template (P_%s) is in use, not removed.\n",str);
	     return 0;
	    }
	 }
       for (p=ptb=&Pic_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(str,p->name) == 0)
	    {
	     ptb->next=p->next;
	     free((char *)p);
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"REMOVE(P_%s)\n",str);
	     return 1;
	    }
	  ptb=p;
	 }
       if (p == NULL)
	 {
	  err_warn(1,fperr,
		"Error - the template (P_%s) not found for remove.\n",str);
	  return 0;
	 }
       return 1;
      }

/*		Rename a picture template.  (str2 -> str1)		*/

    int renameP_name(char *str1,char *str2)
      {
       int i,j;
       char s1[17],s2[17];
       char *pc;
       struct p_tab *p;
       struct display_tab *pd;

       for (i=0,j=0;str2 != NULL && i < 17 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';

       for (i=0,j=0;str1 != NULL && i < 17 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';
/*		Check whether the old template is a default.		*/
       if (strcmp(s1,Pic_tab.name) == 0 || strcmp(s1,Pic_tab_dud.name) == 0)
	 {
	  err_warn(1,fperr,
	      "Error - the default template (P_%s) can't be renamed.\n",s1);
	  return 0;
	 }
/*		Check whether the new template is a default.		*/
       if (strcmp(s2,Pic_tab.name) == 0 || strcmp(s2,Pic_tab_dud.name) == 0)
	 {
	  err_warn(1,fperr,
	    "Error - can't rename (P_%s) to the default template (P_%s).\n",
							s1,s2);
	  return 0;
	 }
/*		Check whether the new name is empty.			*/

       if (str2 == NULL || strprt(s2) == 0)
	 {
	  err_warn(1,fperr,
	      "Error - the new template name is empty, not renamed.\n");
	  return 0;
	 }
/*	Check whether the old template name is used in a display.*/

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && strcmp(pd->p_name,s1)==0)
	    {
	     err_warn(1,fperr,
	      "Error - the template (P_%s) is in use, not renamed.\n",s1);
	     return 0;
	    }
	 }
/*		Check whether the new template name is in use.		*/

       for (p=&Pic_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	       "Error - (P_%s) exists, can't rename (P_%s).\n",s2,s1);
	     return 0;
	    }
	 }
/*	Find the old template name.  Rename if it isn't the default.	*/

       for (p=&Pic_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s1,p->name) == 0)
	    {
	     strcpy(p->name,s2);
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(P_%s,P_%s)\n",s1,s2);
	     return 1;
	    }
	 }

       if (p == NULL)
	 {
	  err_warn(1,fperr,
		"Error - the template (P_%s) can't be found to rename.\n",s2);
	  return 0;
	 }
       return 1;
      }

/*		Copy picture template to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_P_name(char *str1,char *str2)
      {
       int i,j;
       struct p_tab *p,*ptb,*p1;
       struct display_tab *pd;
       char *pc;
       int *pi;
       int I[51];

       if (str1==NULL || strprt(str1)==0 || str2==NULL || strprt(str2)==0)
								 return 0;

/*			Copy to itself - not necessary.			*/

       if (strcmp(str1,str2) == 0) return 1;

/*			Can't copy to default templates.		*/

       if (strcmp(str2,Pic_tab.name) == 0 || strcmp(str2,Pic_tab_dud.name) == 0)
	 {
	  err_warn(1,fperr,
		"Error - cannot copy to a default template (P_%s).\n",str2);
	  return 0;
	 }

/*		Search template table for attributes to be copied.	*/

       for (p1=&Pic_tab;p1 != NULL;p1=p1->next)
				if (strcmp(str1,p1->name) == 0) break;

       if (p1 == NULL)
	 {
	  err_warn(1,fperr,
	       "Error - template (P_%s) can't be found for copy.\n",str1);
	  return 0;
	 }

/*		See if the new attribute name exists in the template table.*/

       for (ptb=p=&Pic_tab;p != NULL;ptb=p,p=p->next)
	 {
	  if (strcmp(str2,p->name) == 0)
	    {
	     err_warn(1,fperr,
		   "Error - can't copy (P_%s) to existing template (P_%s).\n",
				str1,str2);
	     return 0;
	    }
	 }
/*		Add a table entry for the new attribute set.		*/

       if((p=ptb->next=(struct p_tab *)malloc(sizeof(Pic_tab))) == NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for template (P_%s) can't be found.\n",str2);
	  return 0;
	 }
/* 		Zero the new picture elements.				*/

       for (pc=(char *)p,i=0; i < sizeof(Pic_tab); i++,pc++) *pc=0;

       p->next=NULL;
       strcpy(p->name,str2);

       copyP_attr(p1,p);

       if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(P_%s,P_%s)\n",str1,str2);
       return 1;
      }
