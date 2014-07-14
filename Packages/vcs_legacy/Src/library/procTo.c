#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "picture.h"
#include "display.h"
#include "graph.h"

#define TRUE 1
#define FALSE 0

#define STRMAX 256

    extern FILE *fpin,*fpout,*fperr;
    extern struct table_chorn To_tab;
    extern struct table_chorn To_tab1;
    extern struct table_chorn To_tab2;
    extern struct table_chorn To_tab3;
    extern struct table_chorn To_tab4;
    extern struct display_tab D_tab;
    extern struct p_tab Pic_tab;
    extern struct gi_tab Gi_tab;

    extern int update_ind;

/*	Process a text table assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procTo_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,c;
	int tokm;
	int change,new;
	char strm[STRMAX+1];
	float v;
	struct table_chorn *ptab, *ptb;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }

	change=new=FALSE;

/*		Look in the character orientation table table
				 for the name.				*/
/*		Make a table entry if it doesn't exist			*/

	for (ptb=ptab=&To_tab; ptab!=NULL && cmpnbl(&str[3],ptab->name)!=0;)
	  {
	   if (ptab->next == NULL)
	     {
	      if (((ptab->next)=
			(struct table_chorn *)malloc(sizeof(To_tab))) == NULL)
	        {
		 err_warn(1,fperr,"Error memory for %s not found \n",str);
		 return 0;
	        }
	      ptb=ptab;
	      ptab=ptab->next;
	      for (i=0; i<16 && (ptab->name[i]=str[i+3]) != '\0'; i++);
	      ptab->name[i]='\0';
	      ptab->next=NULL;
	      new=TRUE;
	      break;
	     }
	   else {ptb=ptab; ptab=ptab->next;}
	  }
/*		Check if it's trying to overwrite the default.		*/

        if (ptab == &To_tab || ptab == &To_tab1 || ptab == &To_tab2 ||
	     ptab == &To_tab3 || ptab == &To_tab4 )
	  {
	   err_warn(1,fperr,"Error - can't replace the defaults (To_%s).\n",
				ptab->name);
	   return 0;
	  }
	i=0;

	while ((c=getsttk(strm,&tokm)) != 0 &&
		tokm != EOF && (tokm == ',' || tokm == ')') && i < 5)
	  {
	   if (i < 2)
	     {
	      if (isnum(strm))
	        {
		 sscanf (strm,"%f",&v);
		 if (new)
		   {
	            if (i == 0) ptab->chh=v;
	            if (i == 1) ptab->chua=v;
		   }
		 else
		   {
	            if (i == 0) {if (ptab->chh != v) change=TRUE; ptab->chh=v;}
	            if (i == 1) {if (ptab->chua!=v) change=TRUE; ptab->chua=v;}
		   }
		}
	      else
		{
	         err_warn(1,fperr,"Error - invalid character"
				" not a number in (%s[%d]=%s).\n",
					str,i,strm);
		 break;
		}
	     }
	   else if (i == 2)
	     {
	      if (!new && ptab->chpath != *strm) change=TRUE;
	      ptab->chpath=*strm;
	      if (*strm!='r' && *strm!='l' && *strm!='u' && *strm!='d')
		{
	         err_warn(1,fperr,"Error - invalid character"
				" not chpath (r,l,u,d) in (%s[2]=%s).\n",
					str,strm);
		 break;
		}
	     }
	   else if (i == 3)
	     {
	      if (!new && ptab->chalh != *strm) change=TRUE;
	      ptab->chalh=*strm;
	      if (*strm!='l' && *strm!='c' && *strm!='r')
		{
	         err_warn(1,fperr,"Error - invalid character"
				" not chalh (l,c,r) in (%s[3]=%s).\n",
					str,strm);
		 break;
		}
	     }
	   else if (i == 4)
	     {
	      if (!new && ptab->chalv != *strm) change=TRUE;
	      ptab->chalv=*strm;
	      if (*strm!='t'&&*strm!='c'&&*strm!='h'&&*strm!='b'&&*strm!='s')
		{
	         err_warn(1,fperr,"Error - invalid character"
				" not chalv (t,c,h,b,s) in (%s[4]=%s).\n",
					str,strm);
		 break;
		}
	     }
	   i++;
	   if (tokm == ')') break;
	  }
	if (c == 0)
	  {
	   err_warn(1,fperr,
	      "Error - empty value in the character orientation table (%s).\n",
					str);
	  }
	if (i != 5)
	  {
	   c=0;
	   if (ptb != ptab)
	     {
	      err_warn(1,fperr,
		  "Error - insufficient values, character orientation"
		  " table (%s) deleted.\n",str);
	      ptb->next=ptab->next;
	      free(ptab);
	     }
	   else
	     {
	      err_warn(1,fperr,
		"Error - character orientation table (default)"
					"is incomplete.\n");
	     }
	  }
	if (tokm != ')' && tokm != ',' && tokm != EOF)
	  {
	   c=0;
	   err_warn(1,fperr,
		"Error - character orientation table delimiter"
			" (%s=xxx%c) is wrong.\n",str,tokm);
	  }
	if (change) check_d_chorn(ptab->name);
	if (tokm == EOF)
	  {
	   c=EOF;
	   err_warn(1,fperr,
		"Error - EOF in character orientation table (%s).\n",
				str);
	  }
	return c;
      }

/*		Check whether the text orientation is used for a display.
		If so, a display update is needed.			*/

    int check_d_chorn(name)
      char *name;
      {
       int *pi;

       struct display_tab *pd;
       struct p_tab *pe;
       struct pe_text *pt;
       struct pe_form *pf;
       struct pe_x_lab   *pxl;
       struct pe_y_lab  *pyl;
       struct pe_leg   *plg;

       struct gi_tab   *pgi;
       struct iso      *piso;
	extern struct displays d_type[NTYPES];
	char toname[1000],*tpo;
       

       pd=&D_tab;
       while (pd != NULL)
	 {
	  if (cmpncs(pd->type,d_type[12].type)==0)
	    {
	      strcpy(toname, pd->g_name);
	      tpo = strstr(toname, ":::")+3;
	      if (strcmp(name,toname)==0)
		{
		  pd->dsp_seg[3]=1;
		}
	    }
	  pe=&Pic_tab;
	  while (pe != NULL)
	    {
	     if (strcmp(pd->p_name,pe->name) == 0)
	       {
		pi=&pd->F_seg[0];
		for (pt=&pe->F;pt != &(pe->tu)+1;pt++)
		  {
		   if (cmpnbl(pt->to,name) == 0)
		     {
		      pi++;
		      *pi++=FALSE;
		      pi++;
		      *pi++=TRUE;
		      update_ind=TRUE;
		     }
		   else pi+=4;
		  }
		pi=&pd->xv_seg[0];
		for (pf=&pe->xv;pf != &(pe->min)+1;pf++)
		  {
		   if (cmpnbl(pf->to,name) == 0)
		     {
		      pi++;
		      *pi++=FALSE;
		      pi++;
		      *pi++=TRUE;
		      update_ind=TRUE;
		     }
		   else pi+=4;
		  }
		pxl=&pe->xl1;
		if (cmpnbl(pxl->to,name) == 0)
		  {
		   pd->xl1_seg[1]=FALSE;
		   pd->xl1_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pxl=&pe->xl2;
		if (cmpnbl(pxl->to,name) == 0)
		  {
		   pd->xl2_seg[1]=FALSE;
		   pd->xl2_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pyl=&pe->yl1;
		if (cmpnbl(pyl->to,name) == 0)
		  {
		   pd->yl1_seg[1]=FALSE;
		   pd->yl1_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pyl=&pe->yl2;
		if (cmpnbl(pyl->to,name) == 0)
		  {
		   pd->yl2_seg[1]=FALSE;
		   pd->yl2_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		plg=&pe->leg;
		if (cmpnbl(plg->to,name) == 0)
		  {
		   pd->leg_seg[1]=FALSE;
		   pd->leg_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
	       }
	     pe=pe->next;
	    }
	  pgi=&Gi_tab;
	  while (pgi != NULL)
	    {
	     if (strcmp(pd->g_name,pgi->name) == 0)
	       {
		if (pgi->pGi_attr->labels)
		  {
		   piso=pgi->pGi_attr->line;
		   while (piso != NULL)
		     {
		      if (cmpnbl(piso->to,name) == 0)
		        {
		         pd->dsp_seg[1]=FALSE;
		         pd->dsp_seg[3]=TRUE;
		         update_ind=TRUE;
		         break;
		        }
		      piso=piso->next;
		     }
		  }
	       }
	     pgi=pgi->next;
	    }
	  pd=pd->next;
         }
       return 0;
      }

/*			Print Text orientation attributes.		*/

    int prtTo (FILE *fp,struct table_chorn *ptab)
      {
	if (ptab == NULL) return 0;
	fprintf (fp,"To_%s(%g,%g,%c,%c,%c)\n",ptab->name,ptab->chh,
			ptab->chua,ptab->chpath,ptab->chalh,ptab->chalv);
	return 1;	
      }
