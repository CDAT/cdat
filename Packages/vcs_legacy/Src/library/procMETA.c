#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <sys/file.h>
#include "color.h"
#include "display.h"
#include "workstations.h"
#include "my_access_mode.h"
#include "vcs_legacy_canvas.h"

    extern FILE *fpin,*fpout,*fperr;
    extern FILE *fpmeta;

    extern int Inactive;
    extern int user_defer_update;
    extern struct color_table C_tab;
    extern struct workstations Wkst[];
    extern char meta_file[1024];
    extern char dirbase[1024];
    extern struct display_tab D_tab;
    extern struct orientation Page;
    extern char active_colors[17];
    extern struct c_val std_color[16];

/*	Process a svg command.					*/

    int procMETA(str,tok)

      char str[];
      int *tok;

      {
	int tokm;
	int c;
	int app;

	char strm[256];
	char strz[256];

	if (*tok != '(')
	  {
	   flushp(fpout);
	   err_warn(1,fperr,
		"Error - (META) not a proper token (%c).\n",*tok);
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
			"Error - (META) not a proper token (%s%c%s%c).\n",
						str,*tok,strm,tokm);
	   return 0;
	  }
	trimbl(strm,256);
	c=out_meta(strm,app);
	return c;
      }


/*			Open a svg file, if needed, and dump
			segments.					*/
    int segCompareMETA(const void *a, const void *b)
       {
          return (*(const int *)a) - (*(const int *)b);
       }


    int out_meta (name,append, connect_id, dptr)
      char name[];  /*  svg file name.					*/
      int append;   /*  Indicate append (true) or replace (false).	*/
      Gconid_X_drawable connect_id;    /*  Current workstation id.      */
      display_name_list *dptr; /* VCS Canvas Display List               */

      {
	int i,j,k;
	int ffd, wfd, meta_dir_len;
	float x0,x1,y0,y1;
	char deflt[12]="default.meta";
	char *pc, *meta_ptr;
	char tempfile[1024], meta_dir[1024];
	char tempname[1024];

	char *con_id;
	Gint wstype;

/*	Glimit pwin_req;
	Glimit pwin_cur;
	Glimit pvp_req;
	Glimit pvp_cur;
	Gwstus wst_updst;
	Gwstran wreq;
	Gwstran wcur;
*/
	Gwsti wst_updst;
	Glimit pwin,pview;

	int *pwk;
	struct workstations *w;
	Gintlist pseg;
	int *psg;

	long int cur_pos;
	char s[1024];
	unsigned char       data;
	unsigned char       bytes[1024];
	size_t read_it;
        FILE *fp;
	struct color_table *ptab;
#ifdef PYTHON
	SEG_ORDER_LINK   tptr=NULL, hptr=NULL, aptr=NULL, bptr=NULL;
        int *pi, dct=0, *aseg;
	char *display_name;
	struct display_tab *pd;
	extern struct display_tab D_tab;
 	extern char *    return_display_name();
#endif


	con_id=&tempfile[0];

	for (w=&Wkst[0];w->id != 0 && cmpncs(w->type,"cairo") != 0;w++);
	if (w->id == 0)
	  {
	   err_warn(0,fperr,"Warning - no such META workstation.\n");
	   return 1;
	  }

	for (i=0,j=0,k=-1;name[i] != '\0';i++)
	  {
	   if (name[i] > ' ')
	     {
	      tempname[j++]=name[i];
	      if (tempname[j-1]=='/') k=j-1;
	     }
	  }
	tempname[j]='\0';
	/*if (j > 0) xtend(tempname,".meta"); */ /*commented out python always sends with right extension */


/*		If name is a nil string and meta_file isn't, use meta_file
		as if it were named.					*/
	if (j == 0 && meta_file[0] != '\0')
	  {
	   strcpy (tempname,meta_file);
	   k=0;
	  }

/*		If no directory given then add the base directory.	*
	if (k < 0)
	  {
	   for (i=0,pc=dirbase;i<1023 && *pc!='\0';pc++,i++) tempfile[i]=*pc;
	   k=i;
	   tempfile[i++]='/';
	   if (j > 0)
	       for (pc=tempname;i<1023 && *pc!='\0';pc++,i++) tempfile[i]=*pc;
	   else
	       for (pc=deflt;i<1023 && *pc!='\0';pc++,i++) tempfile[i]=*pc;
	   tempfile[i]='\0';
	  }
	else
	  strcpy (tempfile,tempname);
*/
        if (k < 0)
          {
           tempfile[0] = '.';
           tempfile[1] = '/';
           tempfile[2] = '\0';
           strcat(tempfile, tempname);
          }
        else
	   strcpy (tempfile,tempname);

/*			Test for landscape orientation.			*/
	if (strcmp(Page.page_orient,"landscape") == 0) wstype=1901;
	else wstype=1900;


 /*			Check access of the file.                 	*/
        ffd = access(tempfile, F_OK); /* Check to see if the file exist. */
        if (ffd != 0) { /* File does not exist. */
	   meta_ptr = (char *)strrchr(tempfile, '/');
	   meta_dir_len = strlen(tempfile) - strlen(meta_ptr);
	   if (meta_dir_len < 1)
              wfd = -1;
	   else {
	      strcpy(meta_dir, tempfile);
              meta_dir[meta_dir_len] = '\0';
              wfd=access(meta_dir,W_OK);/*Check permission for directory*/
	   }
	   if (wfd != 0) {/* Can not write to the dir. No write permission. */
              err_warn(1,fperr,
                 "Error - Can not write to the directory.\n"
                 "        No write permission given for (%s).\n", meta_dir);
              return 0;
	   }
/*	  err_warn(0,fperr,"Info - create the PostScript file (%s).\n",tempfile);*/

	} else {        /* File does exist. */
	   wfd=access(tempfile, W_OK);/* Check write permission for file. */
           if (wfd != 0) {/* Can not write to the file. No write permission. */
              err_warn(1,fperr,
                 "Error - Can not write to the file.\n"
                 "        No write permission given for (%s).\n", tempfile);
              return 0;
	   }
/*	  if (append)
	    err_warn(0,fperr,"Info - append to PostScript file (%s).\n",tempfile);
	  else*/
          if (!append)
	    {
	     err_warn(0,fperr,"Info - replace the PostScript file (%s).\n",tempfile);
	     remove(tempfile);
	    }

	}

/*			Save postscript output in the file.			*/
	set_active_colors();
	set_patterns();
        gopwk(w->id,con_id,"MO");/* Landscape workstation */
	strcpy(meta_file,tempfile);

/*		Copy segments to meta workstation.		*/
	gacwk(w->id);
        display_name=return_display_name(connect_id);
        if ((display_name == NULL) && (w->id == 3)) { /* If bg = 1 (i.e., no VCS Canvas shown) */
          while (dptr != NULL) {
             pd=&D_tab;
             while ((pd != NULL) &&
                   (strcmp(pd->name, dptr->display_name) != 0))
                   pd = pd->next;
             if (pd == NULL) break;

             for (pi=&pd->F_seg[0];pi != &pd->dsp_seg[4];pi+=4)
                 if (*pi > 0) ++dct; /* Get the number of segments */

             if ((aseg=(int *)malloc(dct*sizeof(int)))==NULL) { /* malloc the segment array */
                err_warn(1, "Error - memory overflow in creating segment array.");
                return 0;
             }

             dct = 0;   /* Store the segments in malloc'ed array */
             for (pi=&pd->F_seg[0];pi != &pd->dsp_seg[4];pi+=4)
                 if (*pi > 0) { aseg[dct] = *pi; ++dct; }

             qsort(aseg, dct, sizeof(int), segCompareMETA); /* Sort the array in ascending order */

             for (i = 0; i < dct; i++) {  /* Show the segments on the plot */
               gcsgwk(w->id, aseg[i]);
             }

             free((char *) aseg); /* Free the segment array */

             dptr = dptr->next;
          }
        } else if (display_name!=NULL) {
          while (display_name!=NULL) {
             for (pd=&D_tab;pd != NULL;pd=pd->next) {
                 if (strcmp(display_name,pd->name)==0) {
		   /*printf("Ok display plotted is: %s\n",pd->name);*/
                     for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4) {
                       if (*pi > 0 && *(pi+3) == 0) {
                          if((tptr=(SEG_ORDER_LINK)malloc(sizeof(SEG_ORDER))) == NULL) {
                              err_warn(1,fperr,"Error - No memory for segments\n");
                              return 0;
                          }
                          tptr->num = *pi;
                          tptr->next = NULL;
                          if (hptr == NULL)
                             hptr = tptr;
                          else {
                             aptr = bptr = hptr;
                             while (aptr != NULL) {
                                 if ((*pi < aptr->num) && (aptr == hptr)) {
                                    tptr->next = hptr;
                                    hptr = tptr;
                                    break;
                                 } else if ((*pi<aptr->num) && (aptr!=hptr)) {
                                    bptr->next = tptr;
                                    tptr->next = aptr;
                                    break;
                                 }
                                 bptr = aptr;
                                 aptr = aptr->next;
                             }
                             if (aptr == NULL)
                                bptr->next = tptr;
                          }
  
                       }
                     }
                 }
             }
             aptr = hptr;
             while (aptr != NULL) {
	       gcsgwk(w->id,aptr->num);
	       aptr = aptr->next;
             }
             aptr = hptr;
             while (aptr != NULL) {
                  bptr = aptr;
                  aptr = aptr->next;
                  free((char *) bptr);
             }
             hptr = NULL;
  
             display_name=return_display_name(connect_id);
          }
        }

 
        if (w->id > 0) {
           if ((Inactive==1) && (user_defer_update==0))
              guwk(w->id,GPERFORM);
           else if ((Inactive==0) && (user_defer_update==0))
              guwk(w->id,GPERFORM);
        }

	if (!Inactive && fpout != NULL) prtMETA (fpout,meta_file,append);

/*			Close the postscript workstation and file.		*/
/*	gclrwk(w->id,GALWAYS);*/
	gdacwk(w->id);
 	gclwk(w->id);

	return 1;
      }


/*		Print PostScript command.					*/

    int prtMETA (fp,filename,app)
      FILE *fp;
      char *filename;
      int app;
      {
       if (app) fprintf (fp,"META(%s,append)\n",filename);
       else fprintf (fp,"META(%s,replace)\n",filename);
       return 1;
      }
