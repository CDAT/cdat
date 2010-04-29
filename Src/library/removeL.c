
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "list.h"
#include "display.h"
#include "array.h"
#include "graph.h"

#define STRMAX 256

    extern struct l_tab L_tab[2];
    extern struct display_tab D_tab;
    extern struct gi_tab Gi_tab;
    extern struct go_tab Go_tab;
    extern struct gfi_tab Gfi_tab;
    extern struct a_tab A_tab;

    extern int update_ind;
    extern int Inactive;

    extern int I,J,K,L,M,N;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

/*		Remove a List name and its values and strings.		*/

    int removeL_name(str)

      char str[];
      {
	struct l_tab *ptab,*ptb;

	for (ptab=ptb=&L_tab[0];ptab != NULL; ptb=ptab,ptab=ptab->next)
	  {
	   if (cmpnbl(str,ptab->name) == 0)
	     {
	      if (ptab == &L_tab[0] || ptab == &L_tab[1])
		{
		 err_warn(1,fperr,
			"Error - default List (L_%s) can't be removed.\n",str);
		 return 0;
		}
	      if (chkisL_in_A(str) || chkisL_in_G(str))
		{
		 err_warn(1,fperr,
		  "Error - List (L_%s) is in a display and can't be removed.\n",
						str);
		 return 0;
		}
	      ptb->next=ptab->next;
	      killL(ptab);
	      check_L_display(str);
	      if (!Inactive && fpout != NULL)
		fprintf(fpout,"REMOVE(L_%s)\n",str);
	      return 1;
	     }
	  }
	err_warn(1,fperr,"Error - List (L_%s) can't be found to remove.\n",str);

	return 0;
      }
/*		Rename a List.  (str2 -> str1)				*/

    int renameL_name(char *str1,char *str2)

      {
       int i,j;
       char s2[17];
       char s1[17];
       struct l_tab *p;

       if (str2==NULL || str1==NULL || strprt(str1)==0 || strprt(str2)==0)
	 {
	  err_warn(1,fperr,
		"Error - a List name is empty, not renamed.\n");
	  return 0;
	 }

       for (i=0,j=0;str2 != NULL && i < 16 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';
       for (i=0,j=0;str1 != NULL && i < 16 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';

       if (strcmp(s1,L_tab[0].name) == 0 || strcmp(s1,L_tab[1].name) == 0)
	 {
	  err_warn(1,fperr,
		"Error - the default List (L_%s) can't be renamed.\n",s1);
	  return 0;
	 }
       if (chkisL_in_A(s1) || chkisL_in_G(s1))
	 {
	  err_warn(1,fperr,
	     "Error - List (L_%s) is in a display and can't be renamed.\n",
						s1);
	  return 0;
	 }
       if (strcmp(s1,s2) == 0)
	 {
	  err_warn(1,fperr,"Error - rename (L_%s) to (L_%s).\n",s1,s2);
	  return 1;
	 }
       for (p=&L_tab[0];p != NULL;p=p->next)
	 {
	  if (strcmp(s2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	     "Error - rename to (L_%s) would duplicate an existing name.\n",s2);
	     return 0;
	    }
	 }
       for (p=&L_tab[0];p != NULL;p=p->next)
	 {
	  if (strcmp(s1,p->name) == 0)
	    {
	     strcpy(p->name,s2);
	     check_L_display(s1);
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(L_%s,L_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(1,fperr,"Error - List (L_%s) can't be found.\n",
							str1);
       return 0;
      }


/*		Copy List to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_L_name(char *str1,char *str2)
      {
       int i,j,n;
       struct l_tab *p,*ptb,*p1;
       struct l_val *pa,*pa1,*pv;

       if (str1==NULL || strprt(str1)==0 || str2==NULL || strprt(str2)==0)
	 {
	  err_warn(1,fperr,
		"Error - List name is empty, not copied.\n");
	  return 0;
	 }

       if (strcmp(str1,str2) == 0) return 1;

       if (strcmp(str2,L_tab[0].name)==0 || strcmp(str2,L_tab[1].name)==0)
	 {
	  err_warn(1,fperr,"Error - can't copy to the default List (L_%s).\n",
							str2);
	  return 0;
	 }
/*		Search List table for List to be created.		*/

       for (p=&L_tab[0];p != NULL;p=p->next)
	if (strcmp(str2,p->name) == 0)
	  {
	   err_warn(1,fperr,
	    "Error - List (L_%s) already exists copy would duplicate it.\n",
								str2);
	   return 0;
	  }	
/*		Search List table for List to be copied.		*/

       for (p1=&L_tab[0];p1 != NULL;p1=p1->next)
		 if (strcmp(str1,p1->name) == 0) break;

       if (p1 == NULL || p1->count == 0)
	 {
	  err_warn(1,fperr,
	   "Error - List (L_%s) can't be found for copy.\n",str1);
	  return 0;
	 }
       pa1=p1->val;

/*		Find the last List table entry.				*/

       for (ptb=p=&L_tab[0];p != NULL;ptb=p=p->next) 
		if (p->next == NULL) break;

       if (p->next == NULL &&
		(p->next=(struct l_tab *)malloc(sizeof(struct l_tab)))==NULL)
	 {
	  err_warn(1,fperr,
	 	 "Error - memory for List (L_%s) can't be found.\n",str2);
	  return 0;
	 }
	p=p->next;
	p->count=p1->count;
	p->val=NULL;
	strcpy (p->name,str2);
	p->next=NULL;

/*		Create a new List structure and copy to it.	*/
       for (n=0,pa=p->val,pa1=p1->val;pa1!=NULL&&n<p->count;pa1=pa1->next,n++)
	 {
	  if((pv=(struct l_val *)malloc(sizeof(struct l_val)))==NULL)
	    {
	     err_warn(1,fperr,
		"Error - memory for List (L_%s) not found.\n",str2);
	     killL(p);
	     ptb->next=NULL;
	     return 0;
	    }
	  if (p->val == NULL) p->val=pv;
	  else pa->next=pv;
	  pa=pv;
	  if ((pa->str=(char *)malloc(strlen(pa1->str)+1))==NULL)
	    {
	     err_warn(1,fperr,
		"Error - memory for List (L_%s) not found.\n",str2);
	     killL(p);
	     ptb->next=NULL;
	     return 0;
	    }
	  strcpy(pa->str,pa1->str);
	  pa->data=pa1->data;
	  pa->next=NULL;
	 }       
       if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(L_%s,L_%s)\n",str1,str2);
       return 1;
      }
