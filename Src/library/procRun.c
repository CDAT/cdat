#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "workstations.h"

    extern FILE *fpin,*fpout,*fperr;


    extern struct workstations Wkst[];
    extern struct orientation Page;

    extern char script_file[1024];
    extern int Inactive;


/*		Process a Run command.					*/

    int procRun(str,tok)

      char str[257];
      int *tok;

      {
	int tokm;
	int i,j,c;
	char *ptri;
	char strm[1024];

       if (*tok != -99) { /* Python if check */
	if (*tok != '(')
	  {
	   err_warn(1,fperr,
		"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }
	c=getsttk(strm,&tokm);
	if (tokm == ')')
	  {
	   if (c == 0)
	     {
	      err_warn(1,fperr,"Error - no file given, no script run.\n");
	      return 0;
	     }
/*			If it's "./filename" add the current working
			directory to replace "./".			*/
	   ptri=strm;
	   if (strncmp("./",ptri,2) == 0) 
	     {
	      ptri+=2;
	      get_a_path(script_file);
	      strcat(script_file,"/");
	      strcat(script_file,ptri);
	     }
/*			If it's just "filename" add the directory
			of the file with the run command (i.e. the
			current script file directory.			*/
	   else if (strchr(ptri,'/')==NULL)
	     {
	      for (i=0;(c=script_file[i]) != '\0';i++) if (c == '/') j=i;
	      strcpy(&script_file[j+1],ptri);
	     }
/*			Else just use the given path and file name.	*/

	   else strcpy(script_file,strm);

	   if (rscript() == 0)
		return 0;
	  } 
	else
	  {
	   err_warn(1,fperr,
	      "Error - not a proper token (%s%c%s%c) - no script run.\n",
			str,*tok,strm,tokm);
	   return 0;
	  }

	return 1;
       } else {
/*                      If it's "./filename" add the current working
                        directory to replace "./".                      */
	   strcpy(strm, str);
           ptri=strm;
           if ((strncmp("/",ptri,1) != 0) && 
               (strncmp("./",ptri,2) != 0) && (strncmp("../",ptri,3) != 0))
             {
              ptri+=2;
              get_a_path(script_file);
              strcat(script_file,"/");
              strcat(script_file,strm);
             }
/*                      If it's just "filename" add the directory
                        of the file with the run command (i.e. the
                        current script file directory.                  */
           else if (strchr(ptri,'/')==NULL)
             {
	      strcpy(script_file, strm);
             }
/*                      Else just use the given path and file name.     */

           else strcpy(script_file,strm);

           if (rscript() == 0)
                return 0;
       }
       return 1;
      }

/*			Run a script.  Interactive.			*/

    int run_script()
      {
       int c;

       c=1;
       if (script_file[0] == '\0') return 0;
       if (fpout != NULL) fprintf (fpout,"Run(%s)\n",script_file);
       Inactive=1;
       c=rscript();
       Inactive=0;
       return c;
      }
