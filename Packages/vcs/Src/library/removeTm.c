
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

    extern struct table_mark Tm_tab;
    extern struct display_tab D_tab;

    extern int update_ind;
    extern int Inactive;

    extern int I,J,K,L,M,N;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

/*		Remove a Mark name and its values.			*/

    int removeTm_name(str)

      char str[];
      {
	struct table_mark *ptab,*ptb;

	for (ptab=ptb=&Tm_tab;ptab != NULL; ptb=ptab,ptab=ptab->next)
	  {
	   if (cmpnbl(str,ptab->name) == 0)
	     {
	      if (ptab == &Tm_tab)
		{
		 err_warn(0,fperr,
			"Warning - Mark (%s) default can't be removed.\n",str);
		 return 0;
		}
	      ptb->next=ptab->next;
	      check_d_mark(str);
	      if (update_ind == 0)
		{
	         ptb->next=ptab->next;
                 free_points( &ptab->mx );
                 free_points( &ptab->my );
		 if (ptab->mtyp != NULL)  free((char *)ptab->mtyp);
		 if (ptab->msize != NULL) free((char *)ptab->msize);
		 if (ptab->mci != NULL)   free((char *)ptab->mci);
		 free((char *)ptab);
		}
	      else
		{
		 err_warn(0,fperr,
			"Warning - Mark (%s) in use can't be removed.\n",str);
		 return 0;
		}
	      if (!Inactive && fpout != NULL)
		fprintf(fpout,"REMOVE(Tm_%s)\n",str);
	      return 1;
	     }
	  }
	err_warn(0,fperr,"Warning - Mark (%s) can't be found to remove.\n",str);

	return 0;
      }

/*		Rename a Mark descriptor.  (str2 -> str1)		*/

    int renameTm_name(char *str1,char *str2)

      {
       int i,j;
       char s2[17];
       char s1[17];
       struct table_mark *p;

       for (i=0,j=0;str2 != NULL && i < 16 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';
       for (i=0,j=0;str1 != NULL && i < 16 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';
       if (str2 == NULL || str1 == NULL || strprt(s1) == 0 || strprt(s2) == 0)
	 {
	  err_warn(0,fperr,
		"Warning - a Mark name is empty, not renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,Tm_tab.name) == 0)
	 {
	  err_warn(1,fperr,"Error - the default mark can't be renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0)
	 {
	  err_warn(0,fperr,"Warning - rename (%s) to (%s).\n",s1,s2);
	  return 1;
	 }
       for (p=&Tm_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	     "Error - rename to (%s) would duplicate an existing name.\n",s2);
	     return 0;
	    }
	 }
       for (p=&Tm_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s1,p->name) == 0)
	    {
	     check_d_mark(s1);
	     if (update_ind == 0)
		strcpy(p->name,s2);
	     else
	       {
		 err_warn(0,fperr,
			"Warning - Mark (%s) in use can't be renamed.\n",s1);
		 return 0;
	       }
	      if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(Tm_%s,Tm_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(0,fperr,"Warning - Mark (%s) can't be found.\n",
							str1);
       return 0;
      }

/*		Copy Mark to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_Tm_name(char *str1,char *str2)
      {
       int i,j;
       float            deflt=1.0;
       struct table_mark *p,*p1;
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
		"Error - Mark copy name is empty, not copied.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0)
	 {
	  err_warn(1,fperr,
	     "Error - Mark copy to identical name (Tm_%s), not copied.\n",s2);
	  return 0;
	 }
/*		If a target mark name isn't given return 0.		*/

       if (str2 == NULL || strprt(s2) == 0) return 0;

/*		Search Mark table for Mark to be created.		*/
       for (p=&Tm_tab;p != NULL;p=p->next)
	if (strcmp(s2,p->name) == 0)
	  {
	   err_warn(1,fperr,
	      "Error - Mark (Tm_%s) already exists copy would duplicate it.\n",
									s2);
	   return 0;
	  }	
/*		Search mark table for mark to be copied.		*/
       for (p1=&Tm_tab;p1 != NULL;p1=p1->next)
		 if (strcmp(s1,p1->name) == 0) break;

       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Mark (%s) can't be found for copy.\n",str1);
	  return 0;
	 }

/*		Find the last mark table entry.				*/

       for (p=&Tm_tab;p != NULL;p=p->next) 
		if (p->next == NULL) break;

       if (p->next == NULL &&
	(p->next=(struct table_mark *)malloc(sizeof(struct table_mark)))==NULL)
	 {
	  err_warn(1,fperr,
	 	 "Error - memory for mark (Tm_%s) can't be found.\n",s2);
	  return 0;
	 }
	p=p->next;
	strcpy (p->name,s2);
	strcpy (p->proj,p1->proj);
	p->next=NULL;
        copy_int_array( &p->mtyp, &p1->mtyp, &p->mtyp_size, p1->mtyp_size,1 );
        copy_float_array( &p->msize, &p1->msize, &p->msize_size, p1->msize_size,&deflt
 );
        copy_int_array( &p->mci, &p1->mci, &p->mci_size, p1->mci_size,241 );
	p->priority=p1->priority;
        p->mvp[0]=p1->mvp[0];
        p->mvp[1]=p1->mvp[1];
        p->mvp[2]=p1->mvp[2];
        p->mvp[3]=p1->mvp[3];
        p->mwc[0]=p1->mwc[0];
        p->mwc[1]=p1->mwc[1];
        p->mwc[2]=p1->mwc[2];
        p->mwc[3]=p1->mwc[3];
        if (copy_points( &p->mx, p1->mx) == 0) return 0;
        if (copy_points( &p->my, p1->my) == 0) return 0;
        if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(Tm_%s,Tm_%s)\n",str1,str2);
      
       return 1;
      }
