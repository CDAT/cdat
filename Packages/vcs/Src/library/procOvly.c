
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "display.h"
#include "graph.h"
#include "array.h"

    extern FILE *fpin, *fpout, *fperr;/*input, output, error files */

    extern struct display_tab D_tab;
    extern struct a_tab A_tab;
    extern struct displays d_type[NTYPES];

/*			Choice of default continents for overlay
			when dimensions are longitude,latitude. */

    extern struct default_continents Dc;
    extern int update_ind;
    extern int Inactive;


/*		Process an overlay_continents command.			*/

    int procOvly(str,tok)

      char str[];
      int *tok;

      {
	int tokm;
	int i,c;

	char strm[1024];

	if (*tok != '(')
	  {
	   err_warn(1,fperr,
		"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }
	c=getsttk(strm,&tokm);
	if (tokm == ')')
	  {
	   if (c > 0)
	     {
	      i=atoi(strm);
              if ((i < 0) || (i > 12))
                  i=0;
	      if (i != Dc.selected)
		{
		 Dc.selected=i;
		 chk_d_ovly();
	        }
	     }
	   else
	     {
	      err_warn(0,fperr,
		"Warning - no overlay given (%s%c%s%c) - no change made.\n",
			str,*tok,strm,tokm);
	     }
	  } 
	else
	  {
	   err_warn(1,fperr,
	      "Error - not a proper token (%s%c%s%c) - no change made.\n",
			str,*tok,strm,tokm);
	   return 0;
	  }

	return 1;
      }

/*		Check whether an update is needed for the change of
		continent overlay type.					*/

    int chk_d_ovly()
      {
	int regenerate;
	struct display_tab *dtab;
	struct a_tab *pa;

	regenerate=0;
        dtab=&D_tab;
        while (dtab != NULL) {
           if ( dtab->name[0] != '\0' &&
                ( cmpncs(dtab->type,d_type[0].type) == 0 || 
                  cmpncs(dtab->type,d_type[3].type) == 0  || 
                  cmpncs(dtab->type,d_type[5].type) == 0  || 
                  cmpncs(dtab->type,d_type[6].type) == 0  ) ) {
	      /* Get the set attribute structure */
	      pa = &A_tab;
	      while (pa != NULL && strcmp(pa->name,dtab->a[0]) != 0)
                 pa = pa->next;

	      /* Set the display to regenerate if needed */
	      if ( (pa != NULL) && (pa->pA_attr != NULL) &&
                   (doexist(pa->pA_attr->xn[0],"longitude")) &&
		   (doexist(pa->pA_attr->xn[1],"latitude")) ) {
	          dtab->dsp_seg[3] = 1;
		  regenerate = 1;
	      }
	   }
	   dtab = dtab->next;
	}
	if (regenerate) {
 	   update_ind = 1;
	}
       if (!Inactive && fpout != NULL) prtOvly(fpout);
       return 1;
      }		

/*		Print the overaly option command.			*/

    int prtOvly(FILE *fp)
      {
       fprintf (fp,"OVERLAY_CONTINENTS(%d)\n",Dc.selected);
       return 1;
      }
