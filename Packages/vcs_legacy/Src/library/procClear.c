#include "gks.h"
#include "gksshort.h"
#define STRMAX 256

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "display.h"

    extern FILE *fpin,*fpout,*fperr;

    extern int Inactive;
    extern int user_defer_update;

    extern struct display_tab D_tab;
    extern struct a_tab A_tab;

/*	Process a clear display command.				*/

    int procClear(str,tok)

      char str[257];
      int *tok;

      {
	int tokm;

	char strm[STRMAX+1];

	if (*tok != '(')
	  {
	   err_warn(1,fperr,
		"Error (CLEAR) - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }

	while ( getsttk(strm,&tokm) > 0 && (tokm == ')' || tokm == ','))
	  {
	   clear_display(strm);
	   if (tokm == ')') break;
	  } 

	if (tokm != ')')
	  {
	   err_warn(1,fperr,
		"Error (CLEAR) - not a proper token (%s%c%s%c).\n",
			str,*tok,strm,tokm);
	   return 0;
	  }

	return 1;
      }
/*		Clear a single named display or ALL displays,
		determined by the input string.				*/

    int clear_display (str)

      char str[];

      {
	int i,j,k,c;
	int *pi,*pip;

	struct display_tab *pd,*pdd,*pdp;
	struct a_attr *pA;
	struct a_tab *pa;

	Gint wks;

	wks=check_canvas_defer();

	if (cmpncs(str,"ALL") == 0 && !Inactive && fpout != NULL)
				fprintf(fpout,"CLEAR(%s)\n",str);

	pdp=pd=&D_tab;
	while (pd != NULL)
	  {
	   if (cmpncs(pd->name,str) == 0 || cmpncs(str,"ALL") == 0)
	     {
	      if (cmpncs(str,"ALL") != 0 && !Inactive && fpout != NULL)
				fprintf(fpout,"CLEAR(%s)\n",str);


/*			Delete segments.				*/

	      for (pi=&pd->F_seg[0];pi != &pd->dsp_seg[4];pi+=4)
	         if (*pi > 0) gdsg(*pi);

/*			Determine if arrays are used elsewhere;
			if not, remove data space.			*/

	      for (i=0; i < pd->na;i++)
		{
		 if (strlen(pd->a[i]) > (size_t) 0)
		   {
		    k=0;
		    pdd=&D_tab;
		    while (pdd != NULL)
		      {
		       if (pd != pdd)
			 {
			  c=1;
		          for (j=0;j < pdd->na &&
				(c=strcmp(pdd->a[j],pd->a[i])) != 0;j++);
		          if (c == 0)
			    {
			     k=1;
			     break;
			    }
			 }
		       pdd=pdd->next;
		      }
		    if (k == 0)
		      {
		       for (pa=&A_tab; pa != NULL;pa=pa->next)
			 {
			  if (strcmp(pa->name,pd->a[i]) == 0)
			    {
			     pA=pa->pA_attr;
			     if (pA->mask != NULL) free((char *)pA->mask);
			     if (pA->un.data != NULL) free((char *)pA->un.data);
			     pA->mask=NULL;
			     pA->un.data=NULL;
			     pA->min=1.e20;
			     pA->max=1.e20;
			     pA->mean=1.e20;
			     break;
			    }
			 }
		      }
		   }
		}

/*			Remove the display assignment.			*/

/*				If necessary, move attributes to the
				first table entry, or zero out the first
				table.					*/

	      if (pd == &D_tab)
		{
		 if ((pd=pd->next) != NULL)
		   {
		    pdp->wkst_id=pd->wkst_id;
		    strcpy(pdp->name,pd->name);
		    pdp->off=pd->off;
		    pdp->pri=pd->pri;
		    strcpy(pdp->type,pd->type);
		    strcpy(pdp->g_name,pd->g_name);
		    strcpy(pdp->p_name,pd->p_name);
		    pdp->na=pd->na;
		    for (i=0;i < pd->na;i++) strcpy(pdp->a[i],pd->a[i]);
		    for (pi=&pd->F_seg[0],pip=&pdp->F_seg[0];
				pi != &pd->dsp_seg[4];pi++,pip++)
		      *pip=*pi;
		    pdp->next=pd->next;
		    free((char *)pd);
/*				Be sure D_tab is checked again.		*/
		    pd=pdp;
		   }
		 else
		   {
		    pdp->wkst_id=1;
		    pdp->name[0]='\0';
		    pdp->off=0;
		    pdp->pri=0;
		    pdp->g_name[0]='\0';
		    pdp->p_name[0]='\0';
		    pdp->na=0;
		    for (i=0;i < 6;i++)
		      pdp->a[i][0]='\0';
		    for (pi=&pdp->F_seg[0];pi != &pdp->dsp_seg[4];pi++)
			*pi=0;
		    pd=pdp->next=NULL;
		    
		   }
		}
	      else {pdp->next=pd->next; free ((char *) pd); pd=pdp->next;}
	     }
	   else
	     {
	      pdp=pd;
	      pd=pd->next;
	     }
	  }

           if (wks > 0) {   /* Update will occur in the update module. */
	      if (user_defer_update==0)
                 guwk(wks,GPERFORM);
           }
/* DNW - Nov. 1, 1998	if (Inactive == 1) * Do not update if VCS interface is up.   *
           if (wks > 0) {   * Update will occur in the update module. *
	      if ((Inactive==1) && (user_defer_update==0))
                 guwk(wks,GPERFORM);
              else if ((Inactive==0) && (user_defer_update==0))
                 guwk(wks,GPERFORM);
           }
*/
        return 1;
      }

