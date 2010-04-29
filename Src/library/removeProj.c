#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "picture.h"
#include "display.h"
#include "graph.h"
#include "project.h"

#define STRMAX 256

    extern FILE *fpin,*fpout,*fperr;

    extern struct projection_attr p_PRJ_list;

    extern struct display_tab D_tab;
 
    extern int update_ind;
    extern int Inactive;

/*	Remove an meshfill graphics assignment.
	The string defining the name must be in "str".		*/

    int removeProj_name(char *str)
      {
       int i;
       struct projection_attr *p,*ptb;
       struct display_tab *pd;
       struct projection_attr *pa;

       if (str == NULL || strprt(str) == 0) return 0;

       if (strcmp(str,p_PRJ_list.name) == 0)
	 {
	  err_warn(1,fperr,
	  "Error - The default projection (%s) can not be removed.\n",
								str);
	  return 0;
	 }

       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && strcmp(pd->proj_name,str) == 0)
	    {
	     err_warn(1,fperr,
	         "Error -Projection (%s) is in use, not deleted.\n",
							str);
	     return 0;
	    }
	 }
       for (p=ptb=&p_PRJ_list;p != NULL;p=p->next)
	 {
	  if (strcmp(str,p->name) == 0)
	    {
	     ptb->next=p->next;
	     free((char *)p);
	     if (!Inactive && fpout != NULL)
			fprintf(fpout,"REMOVE(Proj_%s)\n",str);
	     return 1;
	    }
	  ptb=p;
	 }
       err_warn(1,fperr,
	 "Error - Projection (Proj_%s) can't be found to remove.\n",str);
       return 0;
      }

/*		Rename a Projection.  (str2 -> str1)		*/

    int renameProj_name(char *str1,char *str2)
      {
       int i,j;
       char s[17];
       struct projection_attr *p;
       struct display_tab *pd;

       if (str1==NULL || str2==NULL || strprt(str1)==0 || strprt(str2)==0) 
								return 0;

       if (strcmp(str1,p_PRJ_list.name) == 0)
	 {
	  err_warn(1,fperr,
	    "Error - The default Projection (%s) can't be renamed.\n",
								str1);
	  return 0;
	 }
       if (strcmp(s,p_PRJ_list.name) == 0)
	 {
	  err_warn(1,fperr,
	    "Error - Can't rename (%s) to the default Projection (Proj_%s).\n",
								str1,str2);
	  return 0;
	 }
       for (pd=&D_tab;pd != NULL; pd=pd->next)
	 {
	  if (!(pd->off) && strcmp(pd->proj_name,str1) == 0)
	    {
	     err_warn(1,fperr,
		      "Error - Projection (%s) is in use."
			"  Can't rename (%s).\n",str2,str1);
	     return 0;
	    }
	 }

       for (p=&p_PRJ_list;p != NULL;p=p->next)
	 {
	  if (strcmp(s,p->name) == 0)
	    {
	     err_warn(1,fperr,
      "Error - Can't rename Projection (%s) to existing (%s).\n",
					str1,str2);
	     return 0;
	    }
	 }
      for (p=&p_PRJ_list;p != NULL;p=p->next)
	 {
	  if (strcmp(str1,p->name) == 0)
	    {
	     strcpy(p->name,str2);
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(%s,%s)\n",str1,str2);
	     return 1;
	    }
	 }
       err_warn(1,fperr,
	    "Error - Projection (%s) can't be found to rename.\n",
						str1);
       return 0;
      }

/*		Copy Projection to another (str1 -> str2)
		if a name exists in str2.				*/

    int copy_Proj_name(char *str1,char *str2)
      {
       int i;
       struct projection_attr *p,*ptb,*p1;
       struct display_tab *pd;

       if (str1==NULL || str2==NULL || strprt(str1)==0 || strprt(str2)==0)
								return 0;
/*		It's an internal copy of projection descriptions.	*/

       if (strcmp(str1,str2) == 0) return 1;

/*		Is it a copy to the default projection attribute set.	*/

       if (strcmp(str2,p_PRJ_list.name) == 0)
	 {
	  err_warn(1,fperr,
	   "Error - Can't copy to the default Projections (%s).\n",
							str2);
	  return 0;
	 }

/*		Is it a copy to an existing Projection attribute set.	*/

       for (ptb=p=&p_PRJ_list;p != NULL; ptb=p,p=p->next)
	 if (strcmp(str2,p->name) == 0)
	 {
	  err_warn(1,fperr,
	   "Errof - Can't copy Projections (%s) to existing (%s).\n",
							str1,str2);
	  return 0;
	 }

/*		Search Projection table for attributes to be copied.	*/

       for (p1=&p_PRJ_list;p1 != NULL;p1=p1->next)
	  if (strcmp(str1,p1->name) == 0) break;

       if (p1 == NULL)
	 {
	  err_warn(1,fperr,
	   "Error - Projections (%s) not found for copy.\n",str1);
	  return 0;
	 }



/*		Create a table entry.				*/

/*        printf("ptb name: %s\n",ptb->name); */
       if((p=(struct projection_attr *)malloc(sizeof(struct projection_attr))) == NULL)
	 {
	  err_warn(1,fperr,
	  "Error - memory for Projections (Proj_%s) not found for copy.\n",
								str1);
	 }
       strcpy(p->name,str2);
       
       p->next=NULL;
       ptb->next=p;
/*        printf("Naming it: %s\n",str2); */
/*        printf("ptb name is: %s\n",ptb->name); */
       strcpy(p->name,str2);
       p->proj_type=p1->proj_type;
       for (i=0;i<15;i++)
	 {
	   p->parm[i]=p1->parm[i];
	 }

       if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(%s,%s)\n",str1,str2);

/*        printf("Names are now:\n"); */
/*        for (p1=&p_PRJ_list;p1 != NULL;p1=p1->next) printf("%s\n",p1->name); */
       return 1;
      }
