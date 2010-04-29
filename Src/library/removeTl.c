
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

    extern struct table_line Tl_tab;
    extern struct display_tab D_tab;

    extern int update_ind;
    extern int Inactive;

    extern int I,J,K,L,M,N;
    extern void free_points();

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

/*		Remove a Line name and its values.			*/

    int removeTl_name(str)

      char str[];
      {
	struct table_line *ptab,*ptb;

	for (ptab=ptb=&Tl_tab;ptab != NULL; ptb=ptab,ptab=ptab->next)
	  {
	   if (cmpnbl(str,ptab->name) == 0)
	     {
	      if (ptab == &Tl_tab)
		{
		 err_warn(0,fperr,
			"Warning - Line (%s) default can't be removed.\n",str);
		 return 0;
		}
	      ptb->next=ptab->next;
	      check_d_line(str);
	      if (update_ind == 0)
		{
	         ptb->next=ptab->next;
                 free_points( &ptab->lx );
                 free_points( &ptab->ly );
		 if (ptab->ltyp != NULL) free((char *)ptab->ltyp);
		 if (ptab->lwsf != NULL) free((char *)ptab->lwsf);
		 if (ptab->lci != NULL)  free((char *)ptab->lci);
		 free((char *)ptab);
		}
	      else
		{
		 err_warn(0,fperr,
			"Warning - Line (%s) in use can't be removed.\n",str);
		 return 0;
		}
	      if (!Inactive && fpout != NULL)
			fprintf(fpout,"REMOVE(Tl_%s)\n",str);
	      return 1;
	     }
	  }
	err_warn(0,fperr,"Warning - Line (%s) can't be found to remove.\n",str);

	return 0;
      }

/*		Rename a Line descriptor.  (str2 -> str1)		*/

    int renameTl_name(char *str1,char *str2)

      {
       int i,j;
       char s2[17];
       char s1[17];
       struct table_line *p;

       for (i=0,j=0;str2 != NULL && i < 16 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';
       for (i=0,j=0;str1 != NULL && i < 16 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';
       if (str2 == NULL || str1 == NULL || strprt(s1) == 0 || strprt(s2) == 0)
	 {
	  err_warn(0,fperr,
		"Warning - a Line name is empty, not renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,Tl_tab.name) == 0)
	 {
	  err_warn(1,fperr,"Error - the default line can't be renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0)
	 {
	  err_warn(0,fperr,"Warning - rename (%s) to (%s).\n",s1,s2);
	  return 1;
	 }
       for (p=&Tl_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	     "Error - rename to (%s) would duplicate an existing name.\n",s2);
	     return 0;
	    }
	 }
       for (p=&Tl_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s1,p->name) == 0)
	    {
	     check_d_line(s1);
	     if (update_ind == 0)
		strcpy(p->name,s2);
	     else
	       {
		 err_warn(0,fperr,
			"Warning - Line (%s) in use can't be renamed.\n",s1);
		 return 0;
	       }
	     if (!Inactive && fpout != NULL)
			fprintf(fpout,"RENAME(Tl_%s,Tl_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(0,fperr,"Warning - Line (%s) can't be found.\n",
							str1);
       return 0;
      }


/*		Copy Line to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_Tl_name(char *str1,char *str2)
      {
       int i,j;
       float            deflt=1.0;
       struct table_line *p,*p1;
       char 		s1[17];
       char 		s2[17];
       extern int	copy_points();

       for (i=0,j=0;str2 != NULL && i < 16 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';
       for (i=0,j=0;str1 != NULL && i < 16 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';
       if (str1 == NULL || strprt(s1) == 0)
	 {
	  err_warn(1,fperr,
		"Error - Line name is empty, not copied.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0) return 1;

/*		If a target line name isn't given return 0.	*/

       if (str2 == NULL || strprt(s2) == 0) return 0;

/*		Search Line table for Line to be created.		*/
       for (p=&Tl_tab;p != NULL;p=p->next)
	if (strcmp(s2,p->name) == 0)
	  {
	   err_warn(1,fperr,
	      "Error - Line (%s) already exists copy would duplicate it.\n",s2);
	   return 0;
	  }	
/*		Search line table for line to be copied.		*/
       for (p1=&Tl_tab;p1 != NULL;p1=p1->next)
		 if (strcmp(s1,p1->name) == 0) break;

       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Line (%s) can't be found for copy.\n",str1);
	  return 0;
	 }

/*		Find the last line table entry.				*/

       for (p=&Tl_tab;p != NULL;p=p->next) 
		if (p->next == NULL) break;

       if (p->next == NULL &&
	(p->next=(struct table_line *)malloc(sizeof(struct table_line)))==NULL)
	 {
	  err_warn(1,fperr,
	 	 "Error - memory for line (%s) can't be found.\n",s2);
	  return 0;
	 }
	p=p->next;
	strcpy (p->name,s2);
	strcpy (p->proj,p1->proj);
	p->next=NULL;
        copy_int_array( &p->ltyp, &p1->ltyp, &p->ltyp_size, p1->ltyp_size, 1 );
        copy_float_array( &p->lwsf, &p1->lwsf, &p->lwsf_size, p1->lwsf_size, &deflt );
        copy_int_array( &p->lci, &p1->lci, &p->lci_size, p1->lci_size, 241 );
	p->priority=p1->priority;
        p->lvp[0]=p1->lvp[0];
        p->lvp[1]=p1->lvp[1];
        p->lvp[2]=p1->lvp[2];
        p->lvp[3]=p1->lvp[3];
        p->lwc[0]=p1->lwc[0];
        p->lwc[1]=p1->lwc[1];
        p->lwc[2]=p1->lwc[2];
        p->lwc[3]=p1->lwc[3];
        if (copy_points( &p->lx, p1->lx) == 0) return 0;
        if (copy_points( &p->ly, p1->ly) == 0) return 0;
        if (!Inactive && fpout != NULL)
	   fprintf(fpout,"COPY(Tl_%s,Tl_%s)\n",str1,str2);
      
       return 1;
      }
