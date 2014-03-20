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
    extern char gif_file[1024];
    extern char dirbase[1024];

/*	Process a GIF command.					*/

    int procGif(str,tok)

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
		"Error - GIF command (%c) not a proper token.\n",*tok);
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
		"Error - GIF command (%s%c%s%c) not a proper token.\n",
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
	   err_warn(1,fperr,"Error - canvas not open, no GIF dump.\n");
	   return 0;
	  }

	if (wsid.number > 0 && wsid.integers != NULL)
	    free((char *)wsid.integers);
	c=gif_dump(strm,app);
	return c;
      }


/*			Open a GIF file, if needed, and dump
			GIF image.  If a file is named or the default
			is used and it already exists, images will be
			appended.					*/

/*			GIF files will have ".gif" appended to their
			names by this software.				*/

    int gif_dump (name,append)
      char name[];  /*  GIF dump file name.				*/
      int append;   /*  Indicate append (true) or replace (false).	*/

      {
	int i;
	int ffd;
	char deflt[12]="default.gif";
	char *pc;

	if (name == NULL) return 0;

	for (i=0;name[i] != '\0';i++) {gif_file[i]=name[i]; gif_file[i+1]='\0';}
	if (i == 0)
	  {
	   if (gif_file[0] == '\0')
	     {
	      err_warn(0,fperr,
		"Warning - no file assigned for Output of GIF images, "
			"%s/default.gif used.\n",dirbase);
	      for (i=0,pc=dirbase;i<1023 && *pc!='\0';pc++,i++) gif_file[i]=*pc;
	      gif_file[i++]='/';
	      for (pc=deflt;i<1023 && *pc!='\0';pc++,i++) gif_file[i]=*pc;
	      gif_file[i]='\0';
	     }
	  }
	else
	  {
	   xtend(gif_file,".gif");
	  }
        ffd = access(gif_file, F_OK); /* Check to see if the file exist. */
        if (ffd != 0)		/* File does not exist. */
	  {
	   err_warn(0,fperr,
		"Info - create the GIF Output file (%s).\n",gif_file);   
	  }
	else
	  {
	   if (append)
		err_warn(0,fperr,
		 "Info - append to GIF Output file (%s).\n",gif_file);
	   else
	     {
	      err_warn(0,fperr,
		"Info - replace the GIF Output file (%s).\n",gif_file);
	      remove(gif_file);
	     }
	  }
	frast=fopen(gif_file,"a");
	if (frast == NULL)
	  {
	   gif_file[0]='\0';
	   err_warn(1,fperr,
	  "Error - can't open GIF Output file (%s)."
					" No GIF image dump performed.\n",
			gif_file);
	   return 0;
	  }
	else
	  {
	   save_gif_image_vcs_legacy(frast);
	   fclose (frast);
	   frast=NULL;
	  }
	if (!Inactive && fpout != NULL) prtGIF (fpout,gif_file,append);

	return 1;
      }


/*		Print GIF command.					*/

    int prtGIF (fp,filename,app)
      FILE *fp;
      char *filename;
      int app;
      {
       if (app) fprintf (fp,"GIF(%s,append)\n",filename);
       else fprintf (fp,"GIF(%s,replace)\n",filename);
       return 1;
      }
