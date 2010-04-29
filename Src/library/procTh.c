#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "picture.h"
#include "display.h"

#define STRMAX 256
#define FALSE 0
#define TRUE 1

    extern FILE *fpin,*fpout,*fperr;
    extern struct table_form Th_tab;
    extern struct display_tab D_tab;
    extern struct p_tab Pic_tab;

    extern int update_ind;

/*	Process a format table assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procTh_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,c;
	int tokm;
	char strm[STRMAX+1];
	struct table_form *ptab,*ptb;
	struct form *pform,*pf;
	int change,new;

	struct form f;

	f.s_name[0]='\0';
	f.s_units[0]='\0';
	f.format[0]='\0';
	f.next=NULL;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }
	change=new=FALSE;

/*		Look in the form table for the name.			*/
/*		Make a table entry if it doesn't exist			*/

	for (ptb=ptab=&Th_tab; ptab!=NULL && cmpnbl(&str[3],ptab->name)!=0;)
	  {
	   if (ptab->next == NULL)
	     {
	      if (((ptab->next)=
			(struct table_form *)malloc(sizeof(Th_tab))) == NULL)
	        {
		 err_warn(1,fperr,"****Error memory for %s not found \n",str);
		 return 0;
	        }
	      ptb=ptab;
	      ptab=ptab->next;
	      for (i=0; i<16 && (ptab->name[i]=str[i+3]) != '\0'; i++);
	      ptab->name[i]='\0';
	      ptab->variety=NULL;
	      ptab->next=NULL;
	      new=TRUE;
	      break;
	     }
	   else ptab=ptab->next;
	  }
	if (ptab == &Th_tab)
	  {
	   err_warn(0,fperr,
			"Warning - can't replace the default format (Th_%s).\n",
				ptab->name);
	   return 0;
	  }

	while ( ((c=getsttk(strm,&tokm)) == 0 || c != 0) &&
		( tokm == '(' || tokm == ','))
	  {
	   if (tokm == ',') continue;
	   f.s_name[0]='\0';
	   f.s_units[0]='\0';
	   f.format[0]='\0';
	   getsttk(strm,&tokm);
	   if (tokm == '\'' || tokm == '\"')
	     {
	      for (i=0;(c=getp(fpin,fpout)) != '\'' && c != '\"' && c != EOF;)
		if (i<16) f.s_name[i++]=c;
	      f.s_name[i]='\0';
	      if (c == EOF) break;
	      while ( (c=getp(fpin,fpout)) != '\'' && c != '\"' &&
						 c != EOF && c != ')');
	      if (c == ')') continue;
	      if (c == EOF) break;

	      for (i=0;(c=getp(fpin,fpout)) != '\'' && c != '\"' && c != EOF;)
		if (i<40) f.s_units[i++]=c;
	       f.s_units[i]='\0';
	      if (c == EOF) break;
	      while ( (c=getp(fpin,fpout)) != '\'' && c != '\"' &&
						 c != EOF && c != ')');
	      if (c == ')') continue;
	      if (c == EOF) break;

	      for (i=0;(c=getp(fpin,fpout)) != '\'' && c != '\"' && c != EOF;)
		if (i<120) f.format[i++]=c;
	       f.format[i]='\0';
	      if (c == EOF) break;
	      while ( (c=getp(fpin,fpout)) != ')' && c != '\"' && c != EOF);
	      if (c == EOF) break;
	     }
	   pf=pform=ptab->variety;
	   while (pf != NULL &&
		(strcmp(pf->s_name,f.s_name)!=0 ||
		 strcmp(pf->s_units,f.s_units)!=0) )
		if ((pf=pf->next) != NULL) pform=pf;
	   if (pf == NULL)
	     {
	      if ((pf=(struct form *)malloc(sizeof(struct form))) == NULL)
	        {
		 err_warn(1,fperr,"Error memory for %s not found \n",str);
		 ptb->next=ptab->next;
		 killTh(ptab);
		 return 0;
	        }
	      new=TRUE;
	      if (pform == NULL) pform=ptab->variety=pf;
	      else
		{
		 pform->next= pf;
		 pform=pform->next;
		}
	      pform->s_name[0]='\0';
	      pform->s_units[0]='\0';
	      pform->format[0]='\0';
	      pform->next=NULL;
	     }
	   if (new)
	     {
	      if (f.s_name[0]!='\0') strcpy(pform->s_name,f.s_name);
	      if (f.s_units[0]!='\0') strcpy(pform->s_units,f.s_units);
	      if (f.format[0]!='\0') strcpy(pform->format,f.format);
	     }
	   else
	     {
	      if (f.s_name[0]!='\0' && strcmp(pform->s_name,f.s_name) != 0)
							 change=TRUE;
	      if (f.s_units[0]!='\0' && strcmp(pform->s_units,f.s_units) != 0)
							 change=TRUE;
	      if (f.format[0]!='\0' && strcmp(pform->format,f.format) != 0)
							 change=TRUE;
	      if (f.s_name[0]!='\0') strcpy(pform->s_name,f.s_name);
	      if (f.s_units[0]!='\0') strcpy(pform->s_units,f.s_units);
	      if (f.format[0]!='\0') strcpy(pform->format,f.format);
	     }
	  }
	c=1;

	if (tokm != ')' && tokm != EOF)
	  {
	   c=0;
	   err_warn(1,fperr,
		"Error - format table delimiter (%s=xxx%c) is wrong.\n",
							str,tokm);
	  }
	if (change) check_d_fmt(ptab->name);

	if (tokm == EOF)
	  {
	   c=EOF;
	   err_warn(1,fperr,"Error - EOF in format table (%s).\n",str);
	  }
	return c;
      }

/*		Check whether the format set is used for a display.
		If so, a display update is needed.			*/

    int check_d_fmt(name)
      char *name;
      {
       struct display_tab *pd;
       struct p_tab *pe;
       struct pe_form *pf;

       pd=&D_tab;
       while (pd != NULL)
	 {
	  pe=&Pic_tab;
	  while (pe != NULL)
	    {
	     if (strcmp(pd->p_name,pe->name) == 0)
	       {
		if (pd->mean_seg[0] != 0)
		  {
		   pf=&pe->mean;
		   if (strcmp(pf->fmt,name) == 0)
		     {
		      pd->mean_seg[1]=FALSE;
		      pd->mean_seg[3]=TRUE;
		      update_ind=TRUE;
		     }
		  }
		if (pd->max_seg[0] != 0)
		  {
		   pf=&pe->max;
		   if (strcmp(pf->fmt,name) == 0)
		     {
		      pd->max_seg[1]=FALSE;
		      pd->max_seg[3]=TRUE;
		      update_ind=TRUE;
		     }
		  }
		if (pd->min_seg[0] != 0)
		  {
		   pf=&pe->min;
		   if (strcmp(pf->fmt,name) == 0)
		     {
		      pd->min_seg[1]=FALSE;
		      pd->min_seg[3]=TRUE;
		      update_ind=TRUE;
		     }
		  }
		if (pd->xv_seg[0] != 0)
		  {
		   pf=&pe->xv;
		   if (strcmp(pf->fmt,name) == 0)
		     {
		      pd->xv_seg[1]=FALSE;
		      pd->xv_seg[3]=TRUE;
		      update_ind=TRUE;
		     }
		  }
		if (pd->yv_seg[0] != 0)
		  {
		   pf=&pe->yv;
		   if (strcmp(pf->fmt,name) == 0)
		     {
		      pd->yv_seg[1]=FALSE;
		      pd->yv_seg[3]=TRUE;
		      update_ind=TRUE;
		     }
		  }
		if (pd->zv_seg[0] != 0)
		  {
		   pf=&pe->zv;
		   if (strcmp(pf->fmt,name) == 0)
		     {
		      pd->zv_seg[1]=FALSE;
		      pd->zv_seg[3]=TRUE;
		      update_ind=TRUE;
		     }
		  }
		if (pd->tv_seg[0] != 0)
		  {
		   pf=&pe->tv;
		   if (strcmp(pf->fmt,name) == 0)
		     {
		      pd->tv_seg[1]=FALSE;
		      pd->tv_seg[3]=TRUE;
		      update_ind=TRUE;
		     }
		  }
	       }
	     pe=pe->next;
	    }
	  pd=pd->next;
         }
       return 0;
      }
/*			Print a format table entry.			*/

    int prtTh (FILE *fp,struct table_form *ptab)
      {
	int k;
	struct form *pv;

	if (ptab==NULL||strlen(ptab->name)==0||(ptab->variety)==NULL)
		return 0;
	k=fprintf (fp,"Th_%s(",ptab->name);

	for (pv=ptab->variety; pv != NULL; pv=pv->next)
	  {
	   if (k > 60) k=fprintf (fp,",\n   ");
	   k+=fprintf(fp,"(\"%s\",\"%s\",\"%s\")",
					pv->s_name,pv->s_units,pv->format);
	  }

	fprintf (fp,")\n");
	return 1;
      }
