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

/*	Remove an projection graphics assignment.
	The string defining the name must be in "str".

	An projection descriptor may be removed instead by following "name"
	with "#<id no.>" as: "test#2"					*/


/*		Get a Graphics projection, and saveit.			*/


    struct projection_attr *getProj(char *str)
      {
       int i;
       struct projection_attr *p,*p1;

/*		Search projection table for attributes to be copied.	*/

       for (p1=&p_PRJ_list;p1 != NULL;p1=p1->next)
	  if (strcmp(str,p1->name) == 0) break;
       if (p1 == NULL)
	 {
	  err_warn(0,fperr,
	   "Warning - Projection (%s) can't be found for get.\n",str);
	  return 0;
	 }
       if((p=(struct projection_attr *)malloc(sizeof(struct projection_attr)))==NULL)
	 {
	  err_warn(1,fperr,
	    "Error - memory for getting Projection(%s) not found.\n",
						str);
	  return NULL;
	 }

       strcpy(p->name,p1->name);

       for (i=0;i<15;i++)
	 {
	   p->parm[i]=p1->parm[i];
	 }
       p->next=NULL;
       return p;
      }


    int killProj(struct projection_attr *p)
      {
/* 	printf("Freeing: %s\n",p->name); */
	free((char *)p);
	return 1;
      }











