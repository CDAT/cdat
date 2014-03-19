#include "gks.h"
#include "gksshort.h"
#ifdef X11WM
#include <X11/Xlib.h>
#endif
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "color.h"
#include "workstations.h"

#define STRMAX 256

    extern struct color_table C_tab;
    extern char active_colors[17];
    extern struct c_val std_color[16];
    extern int update_ind;
#ifdef X11WM
    extern Colormap n_cmap;
#endif
    extern Gconid_X_drawable connect_id;

    extern int Inactive;

    extern FILE *fpin,*fpout,*fperr;

/*	Process the reassignment of the active colormap.  A COLOR
	command.  The string defining the command must be in str[] and
	the following token must be in tok.				*/

    int procColor(str,tok)

      char str[257];
      int *tok;

      {
	int c;
	int tokm;
	char strm[STRMAX+1];
	struct color_table *ptab;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }
	if ((c=getsttk(strm,&tokm)) != 0 && tokm == ')')
	  {
	   for(ptab=&C_tab;ptab != NULL && (c=strcmp(ptab->name,strm)) != 0;
				ptab=ptab->next);

	   if (c != 0)
	     {
	      err_warn(1,fperr,
		"Error - No such colormap name COLOR(%s).\n",strm);
	      return 0;
	     }
	   else if (strcmp(active_colors,strm) != 0)
	     {
	      strncpy(active_colors,strm,17); active_colors[16] = '\0';
	      set_active_colors();
	     }
	  }
	else if (tokm == EOF)
	  {
	   err_warn(1,fperr,"Error - EOF in activate colors: COLOR(name).\n");
	   return EOF;
	  }
	return 1;
      }

/*		Set active colors into GKS.				*/

    int set_active_colors()
      {
	int i,j,c;
	/*Grgb color;*/
	Gcobundl color;
	struct color_table *ptab;
        Gintlist wsid;
        int *pid;
#ifdef X11WM
        extern Visual *visual;
#endif

	for(ptab=&C_tab;ptab != NULL && (c=strcmp(ptab->name,active_colors))!=0;
					ptab=ptab->next);
	if (c != 0)
	  {
	   err_warn(1,fperr,
		"Error - No such colormap name COLOR(%s).\n",active_colors);
	   return 0;
	  }
	else
	  {
           wsid.number = 0;
           wsid.integers = NULL;
           gqopwk(&wsid);
	   /*printf("integers: %i, number: %i\n",wsid.integers,wsid.number);*/
	   for (i=0,pid=wsid.integers;i<wsid.number;pid++,i++)
	     {
	       if (*pid == 1 && !Inactive) {
#ifdef USEX11
		 setXcolormap(ptab);
#endif
	       }
	       else if (*pid == 1 || *pid == 2)
		 {
		   for (j=0;j<16;j++)
		     {
		       color.red=std_color[j].red/100.;
		       color.green=std_color[j].green/100.;
		       color.blue=std_color[j].blue/100.;
/* 		       			 printf("special color: j, r,g,b: %i %f, %f, %f\n",j,color.red,color.green,color.blue); */
/* 		       gscr(*pid,j+240,&color); */
		     }
		   for (j=0;j<240;j++)
		     {
/* 		       printf("j is: %i\n",j); */
		       color.red=ptab->cval[j].red/100.;
		       color.green=ptab->cval[j].green/100.;
		       color.blue=ptab->cval[j].blue/100.;
/* 		       gscr(*pid,j,&color); */
		     }
		   if (*pid == 1) update_ind=1;
		 }
#ifdef X11WM
	       else if ((*pid > 6) && (visual->class != PseudoColor))
		 {
		   for (j=0;j<16;j++)
		     {
		       color.red=std_color[j].red/100.;
		       color.green=std_color[j].green/100.;
		       color.blue=std_color[j].blue/100.;
/* 		       			 printf("Special color: j, r,g,b: %i %f, %f, %f\n",j,color.red,color.green,color.blue); */
/* 		       gscr(*pid,j+240,&color); */
		     }
		   for (j=0;j<240;j++)
		     {
		       color.red=ptab->cval[j].red/100.;
		       color.green=ptab->cval[j].green/100.;
		       color.blue=ptab->cval[j].blue/100.;
/* 		       gscr(*pid,j,&color); */
		     }
		 }
#endif
	     }
           if (wsid.number > 0 && wsid.integers != NULL)
	     free((char *)wsid.integers);
	  }
	return 1;
      }

/*			move colors to the X colormap "n_cmap".			*/
    int setXcolormap(struct color_table *pc)
      {
       int i;
#ifdef X11WM
       XColor xc[256];
#endif

#ifdef X11WM
	for (i=0;i<240;i++)
	  {
	   xc[i].pixel=i;
	   xc[i].red  =(int)(pc->cval[i].red  *2.550+0.5)<<8;
	   xc[i].green=(int)(pc->cval[i].green*2.550+0.5)<<8;
	   xc[i].blue =(int)(pc->cval[i].blue *2.550+0.5)<<8;
	   xc[i].flags=DoRed|DoGreen|DoBlue;
	  }
	XStoreColors (connect_id.display,connect_id.n_cmap,xc,240);
#else
	printf("insert here your WM colormap setting thing\n");
#endif
	return 1;
      }

/*			Print the active colormap name.		*/

    int prtColor(FILE *fp)
      {
	fprintf (fp,"color(%s)\n",active_colors);
	return 1;
      }
