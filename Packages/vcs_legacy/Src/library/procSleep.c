#include <stdio.h>

    extern FILE *fperr;

/*	Process a pause.						*/

    int procSleep(str,tok)

      char str[257];
      int *tok;

      {
	int c;
	int tokm;
	char strm[257];
	float v;
	unsigned int iv;


/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }
	c=getsttk(strm,&tokm);
	if (c != 0 && isnum(strm) && tokm != EOF && tokm == ')' )
	  {
	   sscanf (strm,"%f",&v);
	   if (v < 1.0) v=1.0;
	   if (v > 100.) v=100.;
	   iv=v;
	  }
	sleep(iv);
	return 1;
      }


/*			Print Pause command.				*/

    int prtSleep (FILE *fp, int s)
      {
	unsigned int i;

	i=s;
	if (s > 100) i=100;
	else if (s < 1) i=1;
	fprintf (fp,"Sleep(%d)\n",i);
       return 1;
      }
