#include <stdio.h>

    char outstr[81];
    int lout=0;

    int getp(fpin,fpout)

      FILE *fpin,*fpout;

      {
	int c;
	while ((c=getc(fpin)) != EOF && (c < ' ' || c > '~'));
	if (c != EOF)
	  {
	   if (lout >= 80)
	     {
	      outstr[lout]='\0';
/*	      if (fpout) fprintf (fpout,"%s\n",outstr);			*/
	      lout=0;
	     }
	   outstr[lout++]=c;
	  }
	else if (lout > 0)
	  {
	   outstr[lout]='\0';
/*	   if (fpout) fprintf (fpout, "%s \n",outstr);			*/
	   lout=0;
	  }
	return c;
      }

    int flushp(fpout)
      FILE *fpout;
      {
	outstr[lout]='\0';
/*	if (lout > 0 && fpout) fprintf (fpout,"%s \n",outstr);		*/
	lout=0;
	return 1;
      }

    int ungetp(c,fpin)
      int c;
      FILE *fpin;
      {
	if (lout > 0) lout--;
	ungetc(c,fpin);
	return 1;
      }
