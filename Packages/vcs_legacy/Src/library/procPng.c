#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "workstations.h"
#include "my_access_mode.h"

    extern FILE *fpin,*fpout,*fperr;
    extern FILE *fpcgm,*frast;

    extern int Inactive;

    extern struct workstations Wkst[];
    extern char png_file[1024];
    extern char dirbase[1024];

/*	Process a PNG command.					*/

    int procPng(str,tok)

      char str[];
      int *tok;
      {
	int tokm;
	int i,c;
	int app;

	struct workstations *w;

        Gintlist wsid;
        int *pid;

	char strm[256];
	char strz[256];

	if (*tok != '(')
	  {
	   err_warn(1,fperr,
		"Error - PNG command (%c) not a proper token.\n",*tok);
	   return 0;
	  }
	c=getsttk(strm,&tokm);
	if (tokm == ',')
	  {
	   c=getsttk(strz,&tokm);
	   if (strz[0] == 'a' || strz[0] == 'A') app=1;
	   else app=0;	     
	  }
	else app=1;

	if (tokm != ')')
	  {
	   err_warn(1,fperr,
		"Error - PNG command (%s%c%s%c) not a proper token.\n",
						str,*tok,strm,tokm);
	   return 0;
	  }
	trimbl(strm,256);

/*		Check whether a canvas is open for dumping.		*/

	for (w=&Wkst[0];w->id != 0 && cmpncs(w->type,"X_WIN") != 0;w++);

        wsid.number = 0;
        wsid.integers = NULL;
        gqopwk(&wsid);
        for (i=0,pid=wsid.integers;i<wsid.number;pid++,i++)
	  {
           if (*pid == w->id)
	     break;
	  }
	if (*pid != w->id)
	  {
	   err_warn(1,fperr,"Error - canvas not open, no PNG dump.\n");
	   return 0;
	  }

	if (wsid.number > 0 && wsid.integers != NULL)
	    free((char *)wsid.integers);
	c=png_dump(strm,app);
	return c;
      }


/*			Open a PNG file, if needed, and dump
			PNG image.  If a file is named or the default
			is used and it already exists, images will be
			appended.					*/

/*			PNG files will have ".png" appended to their
			names by this software.				*/

    int png_dump (name,append)
      char name[];  /*  PNG dump file name.				*/
      int append;   /*  Indicate append (true) or replace (false).	*/

      {
	int i;
	int ffd;
	char deflt[12]="default.png";
	char *pc;

	if (name == NULL) return 0;

	for (i=0;name[i] != '\0';i++) {png_file[i]=name[i]; png_file[i+1]='\0';}
	if (i == 0)
	  {
	   if (png_file[0] == '\0')
	     {
	      err_warn(0,fperr,
		"Warning - no file assigned for Output of PNG images, "
			"%s/default.png used.\n",dirbase);
	      for (i=0,pc=dirbase;i<1023 && *pc!='\0';pc++,i++) png_file[i]=*pc;
	      png_file[i++]='/';
	      for (pc=deflt;i<1023 && *pc!='\0';pc++,i++) png_file[i]=*pc;
	      png_file[i]='\0';
	     }
	  }
	else
	  {
	   xtend(png_file,".png");
	  }
        ffd = access(png_file, F_OK); /* Check to see if the file exist. */
        if (ffd != 0)		/* File does not exist. */
	  {
	   err_warn(0,fperr,
		"Info - create the PNG Output file (%s).\n",png_file);   
	  }
	else
	  {
	   if (append)
		err_warn(0,fperr,
		 "Info - append to PNG Output file (%s).\n",png_file);
	   else
	     {
	      err_warn(0,fperr,
		"Info - replace the PNG Output file (%s).\n",png_file);
	      remove(png_file);
	     }
	  }
	frast=fopen(png_file,"a");
	if (frast == NULL)
	  {
	   png_file[0]='\0';
	   err_warn(1,fperr,
	  "Error - can't open Output file (%s)."
					" No PNG image dump performed.\n",
			png_file);
	   return 0;
	  }
	else
	  {
	   save_image(frast);
	   fclose (frast);
	   frast=NULL;
	  }
	if (!Inactive && fpout != NULL) prtPNG (fpout,png_file,append);

	return 1;
      }


/*		Print PNG command.					*/

    int prtPNG (fp,filename,app)
      FILE *fp;
      char *filename;
      int app;
      {
       if (app) fprintf (fp,"PNG(%s,append)\n",filename);
       else fprintf (fp,"PNG(%s,replace)\n",filename);
       return 1;
      }
