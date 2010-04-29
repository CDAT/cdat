
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

    extern FILE *fpin, *fpout, *fperr;/*input, output, error files */


/*			Choice of default Hints.		*/

    extern int control_panel;

    extern int Inactive;


/*		Process a Control command which determines the control
		panel layout.  Either "Power" or other.			*/

    int procControl(str,tok)

      char str[];
      int *tok;

      {
	int tokm;
	int i,c;

	char strm[1024];

	if (*tok != '(')
	  {
	   err_warn(1,fperr,
		"Error - not a proper HINTS token (%s%c).\n",str,*tok);
	   return 0;
	  }
	c=getsttk(strm,&tokm);
	if (tokm == ')')
	  {
	   if (c > 0 && cmpncs("power",strm) == 0)
	     control_panel=1;
	   else
	     control_panel=0;
	  } 
	else
	  {
	   err_warn(1,fperr,
	    "Error - improper token for CONTROL (%s%c%s%c) - no change made.\n",
			str,*tok,strm,tokm);
	   return 0;
	  }

       if (!Inactive && fpout != NULL) prtControl(fpout);
	return 1;
      }

/*		Print the CONTROL option command.			*/

    int prtControl(FILE *fp)
      {
       if (control_panel) fprintf (fp,"CONTROL(power)\n");
       else fprintf (fp,"CONTROL(dummy)\n");
       return 1;
      }
