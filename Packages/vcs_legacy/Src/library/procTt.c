#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "picture.h"
#include "display.h"
#include "graph.h"
#include "vcs_legacy_names_length.h"

#define TRUE 1
#define FALSE 0

#define STRMAX 256

    extern FILE *fpin,*fpout,*fperr;
    extern struct table_text Tt_tab;
    extern struct display_tab D_tab;
    extern struct p_tab Pic_tab;
    extern struct gi_tab Gi_tab;

    extern int update_ind;

/*	Process a text table assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procTt_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,c,stop;
	int tokm;
	int change,new;
	char *pc;
	char strm[STRMAX+1];
	float v;
	struct table_text *ptab, *ptb;

	change=new=FALSE;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }

/*		Look in the text table for the name.			*/
/*		Make a table entry if it doesn't exist			*/

	for (ptb=ptab=&Tt_tab; ptab!=NULL && cmpnbl(&str[3],ptab->name)!=0;)
	  {
	   if (ptab->next == NULL)
	     {
	      if (((ptab->next)=
			(struct table_text *)malloc(sizeof(Tt_tab))) == NULL)
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

        if (ptab == &Tt_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (Tt_%s).\n",
				ptab->name);
	   return 0;
	  }
/*                       nullify the set of attributes                  */

        if (new) {
           ptab->txfont = 1;
           ptab->txpr = 1;
           ptab->txexp = 1.0;
           ptab->txsp = 0.2;
           ptab->txci = 1;
           ptab->txfci = 0;
           ptab->priority = 1;
	   strcpy(ptab->proj,"default");
/* 	   for (pc=&(ptab->proj[0]),i=0; i < VCS_MX_NM_LEN; *pc='\0',pc++,i++); */
           for (j=0; j < 4; j++) {
               if (j == 0 || j == 2) {
                  ptab->tvp[j]=0.0;
                  ptab->twc[j]=0.0;
               } else {
                  ptab->tvp[j]=1.0;
                  ptab->twc[j]=1.0;
               }
           }
           ptab->tx = NULL;
           ptab->ty = NULL;
           ptab->ts = NULL;
        }

	i=0;
        while ( ((c=getsttk(strm,&tokm)) || c == 0) )
          {
           if (c == EOF || tokm == EOF)
             {
              err_warn(1,fperr, "Error - EOF in secondary text object for (%s%c).\n",
                                                str,tok);
              return 0;
             }
           if (c == 0)
             {
              err_warn(1,fperr, "Error - not a secondary text object attribute name (%s%c%s%c).\n ",
                str,*tok,strm,tokm);
              return 0;
             }

           if (i < 6)
              sscanf (strm,"%f",&v);
	   if (i==6) {
	     /* C.Doutriaux, code to read in the old vcs_legacy attribute  files, vs ones with fillin color */
	     if ((strcmp(strm,"projection")==0) ||
	     (strcmp(strm,"vp")==0) ||
		 (strcmp(strm,"wc")==0) ||
		 (strcmp(strm,"string")==0)||
		 (strcmp(strm,"x")==0)|| 
		 (strcmp(strm,"y")==0) ) {i+=1;ptab->txfci=240;}
	     else sscanf (strm,"%f",&v);
	   }
	 
           if ((strcmp(strm, "vp") == 0 ) || (strcmp(strm, "wc") == 0 ) ||
               (strcmp(strm, "x") == 0 ) || (strcmp(strm, "y") == 0 ) ||
               (strcmp(strm, "string") == 0 ) || (strcmp(strm, "projection") == 0) )
             {
              if (i == 0) ptab->txfont=v;
              else if (i == 1) ptab->txpr=v;
              else if (i == 2) ptab->txexp=v;
              else if (i == 3) ptab->txsp=v;
              else if (i == 4) ptab->txci=v;
              else if (i == 5) ptab->priority=v;
              else if (i == 6) ptab->txfci=v;
              else if (i > 6) {
                 if ( strcmp(strm, "projection") == 0 ) {
	            j=0;
		    pc=&(ptab->proj[0]);
	            while ( !istoken(tokm=c=getp(fpin,fpout)) ) {
		       if (c > ' ') {
		          if (j < VCS_MX_NM_LEN) {*(pc+j)=c; *(pc+j+1)='\0';}
		          j++;
		       }
		    }
	            if (c != ',' && c != ')') {
		       err_warn(1,fperr,
			      "Error - syntax is incorrect (%s%c%s%c%s%c).\n",
			      str,*tok,strm,tokm,pc,c);
		       return 0;
		    }
		 } else if ( strcmp(strm, "vp") == 0 ) {
                     for (j=0; j<4; j++) {
                        c=getsttk(strm,&tokm);
                        sscanf (strm,"%f",&ptab->tvp[j]);
                     }
                     getsttk(strm,&tokm);
                 } else if ( strcmp(strm, "wc") == 0 )  {
                     for (j=0; j<4; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%f",&ptab->twc[j]);
                     }
                     getsttk(strm,&tokm);
                 } else if ( strcmp(strm, "x") == 0 )  {
                     stop = 0;
                     if (get_points( &ptab->tx, &stop) == 0) return 0;
                     if (stop) break;
                 } else if ( strcmp(strm, "y") == 0 )  {
                     stop = 0;
                     if (get_points( &ptab->ty, &stop ) == 0) return 0;
                     if (stop) break;
                 } else if ( strcmp(strm, "string") == 0 )  {
                     if (get_strings( &ptab->ts, &stop ) == 0) return 0;
                     getsttk(strm,&tokm);
                 }
              }
             }
           else
             {
              if (i == 0) {if (ptab->txfont != v) change=TRUE; ptab->txfont=v;}
              else if (i == 1) {if (ptab->txpr != v) change=TRUE; ptab->txpr=v;}
              else if (i == 2) {if (ptab->txexp!=v) change=TRUE; ptab->txexp=v;}
              else if (i == 3) {if (ptab->txsp != v) change=TRUE; ptab->txsp=v;}
              else if (i == 4) {if (ptab->txci != v) change=TRUE; ptab->txci=v;}
              else if (i == 6) {if (ptab->txfci != v) change=TRUE; ptab->txfci=v;}
              else if (i == 5) {if (ptab->priority != v) change=TRUE; ptab->priority=v;}
             }
           i++;
           if (tokm == ')') break;
          }
/*
	while (i < 5 && (c=getsttk(strm,&tokm)) != 0 &&
		isnum(strm) && tokm != EOF && (tokm == ',' || tokm == ')') )
	  {
	   sscanf (strm,"%f",&v);
	   if (i == 0) ptab->txfont=v;
	   if (new)
	     {
	      if (i == 0) ptab->txfont=v;
	      else if (i == 1) ptab->txpr=v;
	      else if (i == 2) ptab->txexp=v;
	      else if (i == 3) ptab->txsp=v;
	      else if (i == 4) ptab->txci=v;
	     }
	   else
	     {
	      if (i == 0) {if (ptab->txfont != v) change=TRUE; ptab->txfont=v;}
	      else if (i == 1) {if (ptab->txpr != v) change=TRUE; ptab->txpr=v;}
	      else if (i == 2) {if (ptab->txexp!=v) change=TRUE; ptab->txexp=v;}
	      else if (i == 3) {if (ptab->txsp != v) change=TRUE; ptab->txsp=v;}
	      else if (i == 4) {if (ptab->txci != v) change=TRUE; ptab->txci=v;}
	     }
	   i++;
	   if (tokm == ')') break;
	  }
*/
	if (c == 0)
	  {
	   err_warn(1,fperr,
		"Error - empty value in the text table (%s).\n",str);
	  }
	/*if (!isnum(strm))
	  {
	   err_warn(1,fperr,
		"Error - non-number in the text table (%s=%s).\n",str,strm);
	  }*/
	if (i < 6)
	  {
	   c=0;
	   if (ptb != ptab)
	     {
	      err_warn(1,fperr,
		  "Error - insufficient values, text table (%s) deleted.\n",
							str);
	      ptb->next=ptab->next;
	      free(ptab);
	     }
	   else
	     {
	      err_warn(1,fperr,"Error - text table (default) is incomplete.\n");
	     }
	  }
	if (change) check_d_text(ptab->name);

	if (tokm != ')' && tokm != ',' && tokm != EOF)
	  {
	   c=0;
	   err_warn(1,fperr,
		"Error - text table delimiter (%s=xxx%c) is wrong.\n",
							str,tokm);
	  }
	if (tokm == EOF)
	  {
	   c=EOF;
	   err_warn(1,fperr,"Error - EOF in text table (%s).\n",str);
	  }
	return c;
      }

/*		Check whether the text set is used for a display.
		If so, a display update is needed.			*/

    int check_d_text(name)
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
       struct gi_tab   *pg;
       struct gi_attr  *pgi;
       struct iso      *piso;
       extern struct displays d_type[NTYPES];
       char ttname[1000],*tpt;
       

       pd=&D_tab;
       while (pd != NULL)
	 {
	  if (cmpncs(pd->type,d_type[12].type)==0)
	    {
	      strcpy(ttname, pd->g_name);
	      tpt = strtok(ttname, ":::");
	      if (strcmp(name,ttname)==0)
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
		   if (strcmp(pt->tb,name) == 0)
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
		   if (strcmp(pf->tb,name) == 0)
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
		if (strcmp(pxl->tb,name) == 0)
		  {
		   pd->xl1_seg[1]=FALSE;
		   pd->xl1_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pxl=&pe->xl2;
		if (strcmp(pxl->tb,name) == 0)
		  {
		   pd->xl2_seg[1]=FALSE;
		   pd->xl2_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pyl=&pe->yl1;
		if (strcmp(pyl->tb,name) == 0)
		  {
		   pd->yl1_seg[1]=FALSE;
		   pd->yl1_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pyl=&pe->yl2;
		if (strcmp(pyl->tb,name) == 0)
		  {
		   pd->yl2_seg[1]=FALSE;
		   pd->yl2_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		plg=&pe->leg;
		if (strcmp(plg->tb,name) == 0)
		  {
		   pd->leg_seg[1]=FALSE;
		   pd->leg_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
	       }
	     pe=pe->next;
	    }
	  pg=&Gi_tab;
	  while (pg != NULL)
	    {
	     if (strcmp(pd->g_name,pg->name) == 0)
	       {
		pgi=pg->pGi_attr;
		if (pgi->labels)
		  {
		   piso=pgi->line;
		   while (piso != NULL)
		     {
		      if (strcmp(piso->tb,name) == 0)
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
	     pg=pg->next;
	    }
	  pd=pd->next;
         }
/*        printf("update_ind is now: %d\n",update_ind); */
       return 1;
      }

/*			Print text attributes.				*/

    int prtTt (FILE *fp,struct table_text *ptab)
      {
	if (ptab == NULL) return 0;
	fprintf (fp,"Tt_%s(%d,%d,%g,%g,%d,%d,%d,",ptab->name,ptab->txfont,
			ptab->txpr,ptab->txexp,ptab->txsp,ptab->txci,ptab->priority,ptab->txfci);
	fprintf (fp,"\n   projection=%s,", ptab->proj);
        fprintf (fp,"\n   vp(%g,%g,%g,%g),",
                ptab->tvp[0],ptab->tvp[1],ptab->tvp[2],ptab->tvp[3]);
        fprintf (fp,"\n   wc(%g,%g,%g,%g)",
                ptab->twc[0],ptab->twc[1],ptab->twc[2],ptab->twc[3]);
        if (ptab->tx != NULL) {
           fprintf (fp,",\n   x(");
           print_points(fp, ptab->tx->ps);
        }
        if (ptab->ty != NULL) {
           fprintf (fp,",\n   y(");
           print_points(fp, ptab->ty->ps);
        }
        if (ptab->ts != NULL) {
           fprintf (fp,",\n   string(");
           print_strings(fp, ptab->ts->ss);
        }
        fprintf (fp,"\n  )\n");
	return 1;
      }
