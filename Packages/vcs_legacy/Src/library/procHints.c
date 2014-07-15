
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

    extern FILE *fpin, *fpout, *fperr;/*input, output, error files */


/*			Choice of default Hints.		*/

    extern int hints_flg;

    extern int Inactive;


/*		Process a Hints command.				*/

    int procHints(str,tok)

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
	   if (c > 0 && cmpncs("off",strm) == 0)
	     hints_flg=0;
	   else
	     hints_flg=1;
	  } 
	else
	  {
	   err_warn(1,fperr,
	      "Error - improper token for HINTS (%s%c%s%c) - no change made.\n",
			str,*tok,strm,tokm);
	   return 0;
	  }

       if (!Inactive && fpout != NULL) prtHints(fpout);
	return 1;
      }

/*		Print the HINTS option command.			*/

    int prtHints(FILE *fp)
      {
       if (hints_flg) fprintf (fp,"HINTS(on)\n");
       else fprintf (fp,"HINTS(off)\n");
       return 1;
      }
