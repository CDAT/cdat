#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "list.h"
#include "picture.h"
#include "display.h"
#include "run.h"

#define STRMAX 256

    extern struct runit Run;

    extern struct a_tab A_tab;
    extern struct l_tab L_tab[2];

    extern struct display_tab D_tab;
    extern struct displays d_type[NTYPES];

    extern char A_strg[38][12];
    extern char A_intg[5][NDS][8];
    extern char A_sflt[5][NDS][8];
    extern char A_vflt[6][NDS][8];

    extern int I,J,K,L,M,N;
    extern int update_ind;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */
    extern char script_file[1024];

    int procCOMM (char *,int *);

/*			Read a script file.  The file name must be
			in the global (char script_file[1024]), and
			a global file pointer (fpin) must be available.	*/

/*			This function is used recursively from procRun.	*/

    int rscript ()

/*	This function reads and interprets a script.

	Attributes for an array are given following "A_name".

	Values and strings for a list are given following "L_name".

	Return gives:	1 - when an end of file was reached
			0 - if the file was empty or uninterpretable	*/

      {

	int i,c;
	long int    cur_pos;
	char str[STRMAX+1];/* used for a string from the script */
	int tok;	/* used to return a token following the string */
	struct runit *pr,*prm;

/*		This function reads elements of a script and stores the
		attributes defined there.				*/

/*		Check the script isn't already being run (i.e. infinite
		loop) and find pointer to last (pr) and next to last (prm)
		file pointer and script file name.			*/

	for (i=0,prm=pr=&Run;pr!=NULL;i++,pr=pr->next)
	  {
	   if (strcmp(script_file,pr->scr_file) == 0)
	     {
	      err_warn(1,fperr,
		"Error - infinite loop on RUN script file (%s).\n",script_file);
	      script_file[0]='\0';
	      fpin=NULL;

	      while (pr!=NULL) {prm=pr; pr=pr->next;}
	      strcpy(script_file,prm->scr_file);
	      fpin=prm->fpscr;
	      if (prm == &Run)
	        {
	         prm->scr_file[0]='\0';
	         prm->fpscr=NULL;
	        }
	      return 0;
	     }
	   prm=pr;
	  }

/*		Open the input script file.				*/
	if (script_file[0] == '\0' || (fpin=fopen(script_file,"r")) == NULL)
	  {
	   err_warn(1,fperr,
		"Error - opening file (%s) - no script run.\n",script_file);
	   script_file[0]='\0';
	   if (fpin != NULL) fclose(fpin);
	   fpin=NULL;
	   strcpy(script_file,prm->scr_file);
	   fpin=prm->fpscr;
	   if (prm == &Run)
	     {
	      prm->scr_file[0]='\0';
	      prm->fpscr=NULL;
	     }
	   return 0;
	  }
	pr=prm;
	if (pr != &Run || pr->fpscr != NULL)
	  if((pr=prm->next=(struct runit *)malloc(sizeof(struct runit)))== NULL)
	  {
	   err_warn(1,fperr,
		"Error - infinite loop on RUN script file (%s).\n",script_file);
	   script_file[0]='\0';
	   strcpy(script_file,prm->scr_file);
	   fpin=prm->fpscr;
	   if (prm == &Run)
	     {
	      prm->scr_file[0]='\0';
	      prm->fpscr=NULL;
	     }
	   return 0;
	  }
	strcpy(pr->scr_file,script_file);
	pr->fpscr=fpin;
	pr->next=NULL;	
	
/*		Get an attribute name or command			*/

	if ((c = getsttk(str,&tok)) == EOF || c == 0 || tok == EOF)
	  {
	    err_warn(1,fperr,"Error - empty file \n");
	    script_file[0]='\0';
	    fclose(fpin);
	    fpin=NULL;
	    if (pr != &Run) free((char *)pr);
	    else {pr->fpscr=NULL; pr->scr_file[0]='\0';}
	    prm->next=NULL;
	    strcpy(script_file,prm->scr_file);
	    fpin=prm->fpscr;
	    if (prm == &Run)
	      {
	       prm->scr_file[0]='\0';
	       prm->fpscr=NULL;
	      }
	    return 0;
	  };
/*		Process the attribute assignment or command		*/
	do
	  {
/*		Is it an array attribute name?				*/
	   if (strncmp(str,"A_",2) == 0)
	      c=procA_name(str,&tok);
/*		Is it a list name?					*/
	   else if (strncmp(str,"L_",2) == 0)
	      c=procL_name(str,&tok);
/*		Is it a colormap name?					*/
	   else if (strncmp(str,"C_",2) == 0)
	      c=procC_name(str,&tok);
/*		Is it a pattern table name?				*/
	   else if (strncmp(str,"Pat_",4) == 0)
	      c=procPat_name(str,&tok);
/*		Is it a picture element name?				*/
	   else if (strncmp(str,"P_",2) == 0)
	      c=procP_name(str,&tok);
/*		Is it a graphic isoline name?				*/
	   else if (strncmp(str,"Gi_",3) == 0)
	      c=procGi_name(str,&tok);
/*		Is it a graphic outline name?				*/
	   else if (strncmp(str,"Go_",3) == 0)
	      c=procGo_name(str,&tok);
/*		Is it a graphic fill isoline name?			*/
	   else if (strncmp(str,"Gfi_",4) == 0)
	      c=procGfi_name(str,&tok);
/*		Is it a graphic meshfill  name?			*/
	   else if (strncmp(str,"Gfm_",4) == 0)
	      c=procGfm_name(str,&tok);
/*		Is it a graphic outline fill name?			*/
	   else if (strncmp(str,"Gfo_",4) == 0)
	      c=procGfo_name(str,&tok);
/*		Is it a continents name?				*/
	   else if (strncmp(str,"Gcon_",5) == 0)
	      c=procGcon_name(str,&tok);
/*		Is it an boxfill name?					*/
	   else if (strncmp(str,"Gfb_",4) == 0)
	      c=procGfb_name(str,&tok);
/*		Is it a vector name?					*/
	   else if (strncmp(str,"Gv_",3) == 0)
	      c=procGv_name(str,&tok);
/*		Is it an X(y) vs y name?				*/
	   else if (strncmp(str,"GXy_",4) == 0)
	      c=procGXyvy_name(str,&tok);
/*		Is it an Y(x) vs x name?				*/
	   else if (strncmp(str,"GYx_",4) == 0)
	      c=procGYxvx_name(str,&tok);
/*		Is it an X(t) vs Y(t) name?				*/
	   else if (strncmp(str,"GXY_",4) == 0)
	      c=procGXY_name(str,&tok);

/*		Is it a ScatterPlot name?				*/
	   else if (strncmp(str,"GSp_",4) == 0)
	      c=procGSp_name(str,&tok);

/*		Is it a text table name?				*/
	   else if (strncmp(str,"Tt_",3) == 0)
	      c=procTt_name(str,&tok);
/*		Is it a character orientation table name?		*/
	   else if (strncmp(str,"To_",3) == 0)
	      c=procTo_name(str,&tok);
/*		Is it a line table name?				*/
	   else if (strncmp(str,"Tl_",3) == 0)
	      c=procTl_name(str,&tok);
/*		Is it a fill area table name?				*/
	   else if (strncmp(str,"Tf_",3) == 0)
	      c=procTf_name(str,&tok);
/*		Is it a marker table name?				*/
	   else if (strncmp(str,"Tm_",3) == 0)
	      c=procTm_name(str,&tok);
/*		Is it a format table name?				*/
	   else if (strncmp(str,"Th_",3) == 0)
	      c=procTh_name(str,&tok);
/*		Is it a DISPLAY command.				*/
	   else if (strncmp(str,"D_",2) == 0)
		c=procDisp(str,&tok);
/*		Is it a projection name.				*/
	   else if (strncmp(str,"Proj_",5) == 0)
		c=procProj_name(str,&tok);
/*		Is it a Taylordiagram name.				*/
	   else if (strncmp(str,"Gtd_",4) == 0)
	     {
	       while (strncmp(str,")",1) != 0)
		 {
		   fscanf(fpin,"%s",str);		   
		 }
	       continue;
	     }
/*		If none of the above					*/
	   else
	     {
	      if (procCOMM(str,&tok) == 0)
		{
	         err_warn(1,fperr,
			"Error - (%s%c) not found or erroneous.\n",str,tok);
		 script_file[0]='\0';
		 fclose(fpin);
		 fpin=NULL;
		 if (pr != &Run) free((char *)pr);
		 else {pr->fpscr=NULL; pr->scr_file[0]='\0';}
		 prm->next=NULL;
		 strcpy(script_file,prm->scr_file);
		 fpin=prm->fpscr;
		 if (prm == &Run)
		   {
		    prm->scr_file[0]='\0';
		    prm->fpscr=NULL;
		   }
		 return 0;
		}
	     }
	   vcs_legacy_canvas_update(0);
	  } while (c != EOF && c != 0 &&
		(c=getsttk(str,&tok)) != EOF && tok != EOF);
	if (c == EOF) c=1;
	
	cur_pos = ftell(fpin);     /* Get the file pointer position */

	script_file[0]='\0';
        fclose(fpin);
	fpin=NULL;
	if (pr != &Run) free((char *)pr);
	else {pr->fpscr=NULL; pr->scr_file[0]='\0';}
	prm->next=NULL;
	strcpy(script_file,prm->scr_file);
	fpin=prm->fpscr;
	if (prm == &Run)
	  {
	   prm->scr_file[0]='\0';
	   prm->fpscr=NULL;
	  }
	return c;
      }
