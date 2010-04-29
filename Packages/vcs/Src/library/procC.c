#include "gks.h"
#include "gksshort.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "color.h"

#define STRMAX 256

    extern struct color_table C_tab;
    extern char active_colors[17];
    extern struct c_val std_color[16];

    extern FILE *fpin,*fpout,*fperr;

    extern int Inactive;

/*	Process a colormap assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procC_name(str,tok)

      char str[257];
      int *tok;

      {
	int j,k,c;
	int tokm;
	char strm[STRMAX+1];
	float v;

	struct c_val colors[240];


/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }
        c=1;
	j=k=0;
	while ((c=getsttk(strm,&tokm)) != 0 &&
		 isnum(strm) && tokm != EOF && (tokm == ',' || tokm == ')') )
	  {
	   if (k < 240)
	     {
	      sscanf (strm,"%f",&v);
	      v=(v<=100.)?v:100.;
	      v=(v<0.0)?0.0:v;
	      if      (j == 0) colors[k].red=v;
	      else if (j == 1) colors[k].green=v;
	      else if (j == 2) colors[k].blue=v;
	     }
	   if (j++ == 2) {j=0; k++;}
	   if (tokm == ')') break;
	  }
	if (c == 0)
	  {
	   err_warn(1,fperr,
		"Error - empty value for the colormap (%s).\n",str);
	   return 0;
	  }
	else if (!isnum(strm))
	  {
	   err_warn(1,fperr,
		"Error - non-number in the colormap (%s=%s).\n",str,strm);
	   return 0;
	  }
	if (tokm != ')')
	  {
	   c=0;
	   if (tokm == EOF)
	     {
	      c=EOF;
	      err_warn(1,fperr,"Error - EOF in colormap (%s).\n",str);
	     }
	   else
	      err_warn(1,fperr,
		"Error - colormap delimiter (%s=xxx%c) is wrong.\n",
							str,tokm);
	   return 0;
	  }
	if (k > 240)
	  {
	   c=0;
	   err_warn(1,fperr,
	     "Warning - greater than 240 colors (%s has %d).\n",
						str,k);
	   k=(k>240)?240:k;  /* Store no more than 240 colors.	*/
	  }
	if((c=save_colors(&str[2],colors))>0&&strcmp(&str[2],active_colors)==0)
	  set_active_colors();
	return c;
      }

/*		Look in the colormap table for the name.		*/
/*		Make an entry if it doesn't exist			*/

    int save_colors (char name[],struct c_val colormap[240])
      {
	int i;
	struct color_table *ptab;

	for (ptab=&C_tab; ptab!=NULL && cmpnbl(name,ptab->name)!=0;)
	  {
	   if (ptab->next == NULL)
	     {
	      if (((ptab->next)=
			(struct color_table *)malloc(sizeof(C_tab))) == NULL)
	        {
		 err_warn(1,fperr,"Error memory for C_%s not found \n",name);
		 return 0;
	        }
	      ptab=ptab->next;
	      for (i=0; i<16 && (ptab->name[i]=name[i]) != '\0'; i++);
	      ptab->name[i]='\0';
	      ptab->next=NULL;
	      break;
	     }
	   else ptab=ptab->next;
	  }
	if (ptab == &C_tab)
	  {
	   err_warn(1,fperr,"Error can't replace C_%s colormap.\n",name);
	   return 0;
	  }
	for (i=0;i<240;i++)
	  {
	   ptab->cval[i].red=colormap[i].red;
	   ptab->cval[i].green=colormap[i].green;
	   ptab->cval[i].blue=colormap[i].blue;
	  }
	if (!Inactive && fpout != NULL) prtC(fpout,ptab);
	return 1;
      }

/*			Print colormap assignment.			*/

    int  prtC(FILE *fp,struct color_table *ptab)
      {
	int i;
	struct c_val *pval;

	if (ptab == NULL) return 0;

	fprintf (fp,"C_%s(\n",ptab->name);
	pval=(struct c_val *)(ptab->cval);
	for (i=0;i < 240;)
	 {
	  fprintf(fp,"   %g,%g,%g",
			 pval->red,pval->green,pval->blue);
	  pval++;
	  if (i++ < 239)
	    {
	     if (i%6 == 0) fprintf(fp,",\n");
	     else fprintf(fp,",");
	    }
	 }
       fprintf(fp,")\n");
       return 1;
      }
