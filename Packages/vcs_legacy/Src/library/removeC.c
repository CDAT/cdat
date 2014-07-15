#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "color.h"


    extern struct color_table C_tab;
    extern char active_colors[17];

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

    extern int update_ind;
    extern int Inactive;

    int removeC (char *c_name)
      {
	int c;
	struct color_table *ptab,*ptb;

/*		Look in the Colormap table for the name.		*/
/*		Make a table entry if it doesn't exist, return a zero
		if it does.						*/

	for(ptb=ptab=&C_tab;
		ptab!=NULL && (c=cmpnbl(c_name,ptab->name))!=0;
						ptb=ptab,ptab=ptab->next);
	if (c != 0)
	  {
	   err_warn(1,fperr,
	    "Error - Colormap (C_%s), not found for remove.\n",c_name);
	   return 0;
	  }
	else if (ptab == &C_tab)
	  {
	   err_warn(1,fperr,
		"Error - Can't remove the default Colormap (C_%s).\n",c_name);
	   return 0;
	  }
	else
	  {
	   ptb->next=ptab->next;
	   free((char *)ptab);
	   if (strcmp(c_name,active_colors) == 0)
	     {
	      strcpy(active_colors,"default");
	      set_active_colors();
	     }
	  }
	if (!Inactive && fpout != NULL)
		fprintf(fpout,"REMOVE(C_%s)\n",c_name);
	return 1;
      }
