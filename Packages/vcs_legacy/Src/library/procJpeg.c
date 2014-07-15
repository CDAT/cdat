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
    extern char jpeg_file[1024];
    extern char dirbase[1024];

/*	Process a JPEG command.					*/

    int procJpeg(str,tok)

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
		"Error - JPEG command (%c) not a proper token.\n",*tok);
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
		"Error - JPEG command (%s%c%s%c) not a proper token.\n",
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
	   err_warn(1,fperr,"Error - canvas not open, no JPEG dump.\n");
	   return 0;
	  }

	if (wsid.number > 0 && wsid.integers != NULL)
	    free((char *)wsid.integers);
	c=jpeg_dump(strm,app);
	return c;
      }


/*			Open a JPEG file, if needed, and dump
			JPEG image.  If a file is named or the default
			is used and it already exists, images will be
			appended.					*/

/*			JPEG files will have ".jpg" appended to their
			names by this software.				*/

    int jpeg_dump (name,append)
      char name[];  /*  JPEG dump file name.				*/
      int append;   /*  Indicate append (true) or replace (false).	*/

      {
	int i;
	int ffd;
	char deflt[12]="default.jpg";
	char *pc;

	if (name == NULL) return 0;

	for (i=0;name[i] != '\0';i++) {jpeg_file[i]=name[i]; jpeg_file[i+1]='\0';}
	if (i == 0)
	  {
	   if (jpeg_file[0] == '\0')
	     {
	      err_warn(0,fperr,
		"Warning - no file assigned for Output of JPEG images, "
			"%s/default.jpg used.\n",dirbase);
	      for (i=0,pc=dirbase;i<1023 && *pc!='\0';pc++,i++) jpeg_file[i]=*pc;
	      jpeg_file[i++]='/';
	      for (pc=deflt;i<1023 && *pc!='\0';pc++,i++) jpeg_file[i]=*pc;
	      jpeg_file[i]='\0';
	     }
	  }
	else
	  {
	   xtend(jpeg_file,".jpg");
	  }
        ffd = access(jpeg_file, F_OK); /* Check to see if the file exist. */
        if (ffd != 0)		/* File does not exist. */
	  {
	   err_warn(0,fperr,
		"Info - create the JPEG Output file (%s).\n",jpeg_file);   
	  }
	else
	  {
	   if (append)
		err_warn(0,fperr,
		 "Info - append to JPEG Output file (%s).\n",jpeg_file);
	   else
	     {
	      err_warn(0,fperr,
		"Info - replace the JPEG Output file (%s).\n",jpeg_file);
	      remove(jpeg_file);
	     }
	  }
	frast=fopen(jpeg_file,"a");
	if (frast == NULL)
	  {
	   jpeg_file[0]='\0';
	   err_warn(1,fperr,
	  "Error - can't open Output file (%s)."
					" No JPEG image dump performed.\n",
			jpeg_file);
	   return 0;
	  }
	else
	  {
	   save_image(frast);
	   fclose (frast);
	   frast=NULL;
	  }
	if (!Inactive && fpout != NULL) prtJPEG (fpout,jpeg_file,append);

	return 1;
      }


/*		Print JPEG command.					*/

    int prtJPEG (fp,filename,app)
      FILE *fp;
      char *filename;
      int app;
      {
       if (app) fprintf (fp,"JPEG(%s,append)\n",filename);
       else fprintf (fp,"JPEG(%s,replace)\n",filename);
       return 1;
      }
