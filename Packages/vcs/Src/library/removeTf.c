
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

    extern struct table_fill Tf_tab;
    extern struct display_tab D_tab;

    extern int update_ind;
    extern int Inactive;

    extern int I,J,K,L,M,N;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

/*		Remove a Fill name and its values.			*/

    int removeTf_name(str)

      char str[];
      {
	struct table_fill *ptab,*ptb;

	for (ptab=ptb=&Tf_tab;ptab != NULL; ptb=ptab,ptab=ptab->next)
	  {
	   if (cmpnbl(str,ptab->name) == 0)
	     {
	      if (ptab == &Tf_tab)
		{
		 err_warn(0,fperr,
			"Warning - Fill (%s) default can't be removed.\n",str);
		 return 0;
		}
	      ptb->next=ptab->next;
	      check_d_fill(str);
	      if (update_ind == 0)
		{
	         ptb->next=ptab->next;
                 free_points( &ptab->fx );
                 free_points( &ptab->fy );
		 if (ptab->fais != NULL) free((char *)ptab->fais);
		 if (ptab->fasi != NULL) free((char *)ptab->fasi);
		 if (ptab->faci != NULL) free((char *)ptab->faci);
		 free((char *)ptab);
		}
	      else
		{
		 err_warn(0,fperr,
			"Warning - Fill (%s) in use can't be removed.\n",str);
		 return 0;
		}
	      if (!Inactive && fpout != NULL)
		fprintf(fpout,"REMOVE(Tf_%s)\n",str);
	      return 1;
	     }
	  }
	err_warn(0,fperr,"Warning - Fill (%s) can't be found to remove.\n",str);

	return 0;
      }

/*		Rename a Fill descriptor.  (str2 -> str1)		*/

    int renameTf_name(char *str1,char *str2)

      {
       int i,j;
       char s2[17];
       char s1[17];
       struct table_fill *p;

       for (i=0,j=0;str2 != NULL && i < 16 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';
       for (i=0,j=0;str1 != NULL && i < 16 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';
       if (str2 == NULL || str1 == NULL || strprt(s1) == 0 || strprt(s2) == 0)
	 {
	  err_warn(0,fperr,
		"Warning - a Fill name is empty, not renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,Tf_tab.name) == 0)
	 {
	  err_warn(1,fperr,"Error - the default fill can't be renamed.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0)
	 {
	  err_warn(0,fperr,"Warning - rename (%s) to (%s).\n",s1,s2);
	  return 1;
	 }
       for (p=&Tf_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	     "Error - rename to (%s) would duplicate an existing name.\n",s2);
	     return 0;
	    }
	 }
       for (p=&Tf_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s1,p->name) == 0)
	    {
	     check_d_fill(s1);
	     if (update_ind == 0)
		strcpy(p->name,s2);
	     else
	       {
		 err_warn(0,fperr,
			"Warning - Fill (%s) in use can't be renamed.\n",s1);
		 return 0;
	       }
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(Tf_%s,Tf_%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(0,fperr,"Warning - Fill (%s) can't be found.\n",
							str1);
       return 0;
      }


/*		Copy Fill to another (str1 -> str2) if a
		name exists in str2.					*/

    int copy_Tf_name(char *str1,char *str2)
      {
       int i,j;
       struct table_fill *p,*p1;
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
		"Error - Fill name is empty, not copied.\n");
	  return 0;
	 }
       if (strcmp(s1,s2) == 0) return 1;

/*		If a target fill name isn't given return 0.		*/

       if (str2 == NULL || strprt(s2) == 0) return 0;

/*		Search Fill table for Fill to be created.		*/
       for (p=&Tf_tab;p != NULL;p=p->next)
	if (strcmp(s2,p->name) == 0)
	  {
	   err_warn(1,fperr,
	      "Error - Fill (%s) already exists copy would duplicate it.\n",s2);
	   return 0;
	  }	
/*		Search fill table for fill to be copied.		*/
       for (p1=&Tf_tab;p1 != NULL;p1=p1->next)
		 if (strcmp(s1,p1->name) == 0) break;

       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Fill (%s) can't be found for copy.\n",str1);
	  return 0;
	 }

/*		Find the last fill table entry.				*/

       for (p=&Tf_tab;p != NULL;p=p->next) 
		if (p->next == NULL) break;

       if (p->next == NULL &&
	(p->next=(struct table_fill *)malloc(sizeof(struct table_fill)))==NULL)
	 {
	  err_warn(1,fperr,
	 	 "Error - memory for fill (%s) can't be found.\n",s2);
	  return 0;
	 }
	p=p->next;
	strcpy (p->name,s2);
	strcpy (p->proj,p1->proj);
	p->next=NULL;
        copy_int_array(&p->fais, &p1->fais, &p->fais_size, p1->fais_size, 1 );
        copy_int_array(&p->fasi, &p1->fasi, &p->fasi_size, p1->fasi_size, 1 );
        copy_int_array(&p->faci, &p1->faci, &p->faci_size, p1->faci_size, 241 );
	p->faci_size=p1->faci_size;
        p->x=0;		/* Note: not in use at this time */
        p->y=0;		/* Note: not in use at this time */
        p->w=0.1;	/* Note: not in use at this time */
        p->h=0.1;	/* Note: not in use at this time */
	p->priority=p1->priority;
        p->fvp[0]=p1->fvp[0];
        p->fvp[1]=p1->fvp[1];
        p->fvp[2]=p1->fvp[2];
        p->fvp[3]=p1->fvp[3];
        p->fwc[0]=p1->fwc[0];
        p->fwc[1]=p1->fwc[1];
        p->fwc[2]=p1->fwc[2];
        p->fwc[3]=p1->fwc[3];
        if (copy_points( &p->fx, p1->fx) == 0) return 0;
        if (copy_points( &p->fy, p1->fy) == 0) return 0;
      
       if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(Tf_%s,Tf_%s)\n",str1,str2);
       return 1;
      }
