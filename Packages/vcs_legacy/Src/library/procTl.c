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
    extern struct table_line Tl_tab;
    extern struct display_tab D_tab;
    extern struct p_tab Pic_tab;
    extern struct gi_tab Gi_tab;
    extern struct go_tab Go_tab;
    extern struct gcon_tab Gcon_tab;
    extern struct gv_tab Gv_tab;
    extern struct gXy_tab GXy_tab;
    extern struct gYx_tab GYx_tab;
    extern struct gXY_tab GXY_tab;

    extern int update_ind;

/*	Process a line table assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procTl_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,c,stop=0,iv;
	int tokm;
	int change, new;
	char strm[STRMAX+1];
	char *pc;
	float v;
	struct table_line *ptab, *ptb;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }
	change=new=FALSE;

/*		Look in the line table table for the name.		*/
/*		Make a table entry if it doesn't exist			*/

	for (ptb=ptab=&Tl_tab; ptab!=NULL && cmpnbl(&str[3],ptab->name)!=0;)
	  {
	   if (ptab->next == NULL)
	     {
	      if (((ptab->next)=
			(struct table_line *)malloc(sizeof(Tl_tab))) == NULL)
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

        if (ptab == &Tl_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (Tl_%s).\n",
				ptab->name);
	   return 0;
	  }

/*                       nullify the set of attributes                  */

        if (new) {
           ptab->priority = 1;
/* 	   for (pc=&(ptab->proj[0]),i=0; i < VCS_MX_NM_LEN; *pc='\0',pc++,i++); */
	   strcpy(ptab->proj,"default");
           for (j=0; j < 4; j++) {
               if (j == 0 || j == 2) {
                  ptab->lvp[j]=0.0;
                  ptab->lwc[j]=0.0;
               } else {
                  ptab->lvp[j]=1.0;
                  ptab->lwc[j]=1.0;
               }
           }
           ptab->lx = NULL;
           ptab->ly = NULL;
           ptab->ltyp = NULL; ptab->ltyp_size = 0;
           ptab->lwsf = NULL; ptab->lwsf_size = 0;
           ptab->lci = NULL; ptab->lci_size = 0;
        }

	i=0;
        while ( ((c=getsttk(strm,&tokm)) || c == 0) )
	  {
           if (c == EOF || tokm == EOF)
             {
              err_warn(1,fperr, "Error - EOF in secondary line object for (%s%c).\n",
                                                str,tok);
              return 0;
             }
           if (c == 0)
             {
              err_warn(1,fperr, "Error - not a secondary line object attribute name (%s%c%s%c).\n",
                str,*tok,strm,tokm);
              return 0;
             }

           if (i < 4)
	      sscanf (strm,"%g",&v);
	   if ((strcmp(strm, "vp") == 0 ) || (strcmp(strm, "wc") == 0 ) ||
               (strcmp(strm, "x") == 0 ) || (strcmp(strm, "y") == 0 ) ||
	       (strcmp(strm, "projection") == 0) )
	     {
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
                        sscanf (strm,"%f",&ptab->lvp[j]);
                     }
                     getsttk(strm,&tokm);
                 } else if ( strcmp(strm, "wc") == 0 )  {
                     for (j=0; j<4; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%f",&ptab->lwc[j]);
                     }
                     getsttk(strm,&tokm);
                 } else if ( strcmp(strm, "x") == 0 )  {
                     stop = 0;
                     if (get_points( &ptab->lx, &stop) == 0) return 0;
	             if (stop) break;

                 } else if ( strcmp(strm, "y") == 0 )  {
                     stop = 0;
                     if (get_points( &ptab->ly, &stop ) == 0) return 0;
	             if (stop) break;
                 }
	     }
	   else
	     {
              if (i == 0) {
                 ptab->ltyp_size=1; change=1;
                 get_int_size_and_set_value( &ptab->ltyp, 1, (int)v );
              } else if (i == 1) {
                   if ( strcmp(strm, "ltyp") != 0 ) {
                      ptab->lwsf_size=1; change=1;
                      get_float_size_and_set_value( &ptab->lwsf, 1, &v );
                   } else {
                      iv = ptab->ltyp_size = ptab->ltyp[0];
                      get_int_size_and_set_value( &ptab->ltyp, iv, (int) v );
                      for (j=0; j<iv; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%i",&ptab->ltyp[j]);
                      }
                      getsttk(strm,&tokm);
                      --i;
                   }
              } else if (i == 2) {
                   if ( strcmp(strm, "lwsf") != 0 ) {
                      ptab->lci_size=1; change=1;
                      get_int_size_and_set_value( &ptab->lci, 1, (int)v );
                   } else {
                      iv = ptab->lwsf_size = ptab->lwsf[0];
                      get_float_size_and_set_value( &ptab->lwsf, iv, &v );
                      for (j=0; j<iv; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%g",&ptab->lwsf[j]);
                      }
                      getsttk(strm,&tokm);
                      --i;
                   }
              } else if (i == 3) {
                  if ( strcmp(strm, "lci") != 0 ) {
                     if (ptab->priority != v) change=1;
                     ptab->priority=v;
                   } else {
                      iv = ptab->lci_size = ptab->lci[0];
                      get_int_size_and_set_value( &ptab->lci, iv, (int) v );
                      for (j=0; j<iv; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%i",&ptab->lci[j]);
                      }
                      getsttk(strm,&tokm);
                      --i;
                  }
                }
	     }
	   i++;
	   if (tokm == ')') break;
	  }
	if (c == 0)
	  {
	   err_warn(1,fperr,
		"Error - empty value in the line table (%s).\n",str);
	  }
/*
	if (!isnum(strm))
	  {
	   err_warn(1,fperr,
		"Error - non-number in the line table (%s=%s).\n",str,strm);
	  }
	if (i != 3)
	  {
	   c=0;
	   if (ptb != ptab)
	     {
	      err_warn(1,fperr,
		  "Error - insufficient values, line table (%s) deleted.\n",
							str);
	      ptb->next=ptab->next;
	      free(ptab);
	     }
	   else
	     {
	      err_warn(1,fperr,"Error - line table (default) is incomplete.\n");
	     }
	  }
	if (tokm != ')' && tokm != ',' && tokm != EOF)
	  {
	   c=0;
	   err_warn(1,fperr,
		"Error - line table delimiter (%s=xxx%c) is wrong.\n",
							str,tokm);
	  }
*/
	if (change) check_d_line(ptab->name);
	if (tokm == EOF)
	  {
	   c=EOF;
	   err_warn(1,fperr,"Error - EOF in line table (%s).\n",str);
	  }
	return c;
      }

/*		Check whether the line set is used for a display.
		If so, a display update is needed.			*/

    int check_d_line(name)
      char *name;
      {
   
       struct display_tab *pd;
       struct p_tab *pe;
       struct pe_x_tic *pxt;
       struct pe_y_tic *pyt;
       struct pe_box   *pbx;
       struct pe_line  *pln;
       struct pe_leg   *plg;

       struct gi_tab   *pgi;
       struct go_tab   *pgo;
       struct gcon_tab *pgcon;
       struct gv_tab   *pgv;
       struct gXy_tab  *pgXy;
       struct gYx_tab  *pgYx;
       struct gXY_tab  *pgXY;
       struct iso      *piso;
       

       pd=&D_tab;
       while (pd != NULL)
	 {
	  if (cmpncs(pd->type,"line")==0)
	    {
	      if (strcmp(name,pd->g_name)==0)
		{
		  pd->dsp_seg[3]=1;
		}
	    }
	  pe=&Pic_tab;
	  while (pe != NULL)
	    {
	     if (strcmp(pd->p_name,pe->name) == 0)
	       {
		pxt=&pe->xt1;
		if (strcmp(pxt->ln,name) == 0)
		  {
		   pd->xt1_seg[1]=FALSE;
		   pd->xt1_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pxt=&pe->xt2;
		if (strcmp(pxt->ln,name) == 0)
		  {
		   pd->xt2_seg[1]=FALSE;
		   pd->xt2_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pxt=&pe->xmta;
		if (strcmp(pxt->ln,name) == 0)
		  {
		   pd->xmta_seg[1]=FALSE;
		   pd->xmta_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pxt=&pe->xmtb;
		if (strcmp(pxt->ln,name) == 0)
		  {
		   pd->xmtb_seg[1]=FALSE;
		   pd->xmtb_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pyt=&pe->yt1;
		if (strcmp(pyt->ln,name) == 0)
		  {
		   pd->yt1_seg[1]=FALSE;
		   pd->yt1_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pyt=&pe->yt2;
		if (strcmp(pyt->ln,name) == 0)
		  {
		   pd->yt2_seg[1]=FALSE;
		   pd->yt2_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pyt=&pe->ymta;
		if (strcmp(pyt->ln,name) == 0)
		  {
		   pd->ymta_seg[1]=FALSE;
		   pd->ymta_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pyt=&pe->ymtb;
		if (strcmp(pyt->ln,name) == 0)
		  {
		   pd->ymtb_seg[1]=FALSE;
		   pd->ymtb_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pbx=&pe->b1;
		if (strcmp(pbx->ln,name) == 0)
		  {
		   pd->b1_seg[1]=FALSE;
		   pd->b1_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pbx=&pe->b2;
		if (strcmp(pbx->ln,name) == 0)
		  {
		   pd->b2_seg[1]=FALSE;
		   pd->b2_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pbx=&pe->b3;
		if (strcmp(pbx->ln,name) == 0)
		  {
		   pd->b3_seg[1]=FALSE;
		   pd->b3_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pbx=&pe->b4;
		if (strcmp(pbx->ln,name) == 0)
		  {
		   pd->b4_seg[1]=FALSE;
		   pd->b4_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pln=&pe->l1;
		if (strcmp(pln->ln,name) == 0)
		  {
		   pd->l1_seg[1]=FALSE;
		   pd->l1_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pln=&pe->l2;
		if (strcmp(pln->ln,name) == 0)
		  {
		   pd->l2_seg[1]=FALSE;
		   pd->l2_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pln=&pe->l3;
		if (strcmp(pln->ln,name) == 0)
		  {
		   pd->l3_seg[1]=FALSE;
		   pd->l3_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		pln=&pe->l4;
		if (strcmp(pln->ln,name) == 0)
		  {
		   pd->l4_seg[1]=FALSE;
		   pd->l4_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
		plg=&pe->leg;
		if (strcmp(plg->ln,name) == 0)
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
		piso=pgi->pGi_attr->line;
		while (piso != NULL)
		  {
		   if (strcmp(piso->lb,name) == 0)
		     {
		      pd->dsp_seg[1]=FALSE;
		      pd->dsp_seg[3]=TRUE;
		      update_ind=TRUE;
		      break;
		     }
		   piso=piso->next;
		  }
	       }
	     pgi=pgi->next;
	    }
	  pgo=&Go_tab;
	  while (pgo != NULL)
	    {
	     if (strcmp(pd->g_name,pgo->name) == 0)
	       {
		if (strcmp(pgo->pGo_attr->lb,name) == 0)
		  {
		   pd->dsp_seg[1]=FALSE;
		   pd->dsp_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
	       }
	     pgo=pgo->next;
	    }
	  pgcon=&Gcon_tab;
	  while (pgcon != NULL)
	    {
	     if (strcmp(pd->g_name,pgcon->name) == 0)
	       {
		if (strcmp(pgcon->pGcon_attr->lb,name) == 0)
		  {
		   pd->dsp_seg[1]=FALSE;
		   pd->dsp_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
	       }
	     pgcon=pgcon->next;
	    }
	  pgv=&Gv_tab;
	  while (pgv != NULL)
	    {
	     if (strcmp(pd->g_name,pgv->name) == 0)
	       {
		if (strcmp(pgv->pGv_attr->lb,name) == 0)
		  {
		   pd->dsp_seg[1]=FALSE;
		   pd->dsp_seg[3]=TRUE;
		   pd->leg_seg[1]=FALSE;
		   pd->leg_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
	       }
	     pgv=pgv->next;
	    }
	  pgXy=&GXy_tab;
	  while (pgXy != NULL)
	    {
	     if (strcmp(pd->g_name,pgXy->name) == 0)
	       {
		if (strcmp(pgXy->pGXy_attr->lb,name) == 0)
		  {
		   pd->dsp_seg[1]=FALSE;
		   pd->dsp_seg[3]=TRUE;
		   pd->leg_seg[1]=FALSE;
		   pd->leg_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
	       }
	     pgXy=pgXy->next;
	    }
	  pgYx=&GYx_tab;
	  while (pgYx != NULL)
	    {
	     if (strcmp(pd->g_name,pgYx->name) == 0)
	       {
		if (strcmp(pgYx->pGYx_attr->lb,name) == 0)
		  {
		   pd->dsp_seg[1]=FALSE;
		   pd->dsp_seg[3]=TRUE;
		   pd->leg_seg[1]=FALSE;
		   pd->leg_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
	       }
	     pgYx=pgYx->next;
	    }
	  pgXY=&GXY_tab;
	  while (pgXY != NULL)
	    {
	     if (strcmp(pd->g_name,pgXY->name) == 0)
	       {
		if (strcmp(pgXY->pGXY_attr->lb,name) == 0)
		  {
		   pd->dsp_seg[1]=FALSE;
		   pd->dsp_seg[3]=TRUE;
		   pd->leg_seg[1]=FALSE;
		   pd->leg_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
	       }
	     pgXY=pgXY->next;
	    }
	  pd=pd->next;
         }
       return 0;
      }

/*			Print line attributes.				*/
    int prtTl(FILE *fp,struct table_line *ptab)
      {
        struct array_segments *aptr;

	if (ptab == NULL) return 0;
        fprintf (fp,"Tl_%s(", ptab->name);

        fprintf (fp,"\n   %d, ltyp(", ptab->ltyp_size);
        print_ints(fp, ptab->ltyp, ptab->ltyp_size);
        fprintf (fp,"\n   %d, lwsf(", ptab->lwsf_size);
        print_floats(fp, ptab->lwsf, ptab->lwsf_size);
        fprintf (fp,"\n   %d, lci(", ptab->lci_size);
        print_ints(fp, ptab->lci, ptab->lci_size);

        fprintf (fp,"\n   %d,", ptab->priority);

	fprintf (fp,"\n   projection=%s,", ptab->proj);
        fprintf (fp,"\n   vp(%g,%g,%g,%g),",
                ptab->lvp[0],ptab->lvp[1],ptab->lvp[2],ptab->lvp[3]);
        fprintf (fp,"\n   wc(%g,%g,%g,%g)",
                ptab->lwc[0],ptab->lwc[1],ptab->lwc[2],ptab->lwc[3]);
        if (ptab->lx != NULL) {
           fprintf (fp,",\n   x(");
           print_points(fp, ptab->lx->ps);
        }
        if (ptab->ly != NULL) {
           fprintf (fp,",\n   y(");
           print_points(fp, ptab->ly->ps);
        }
        fprintf (fp,"\n  )\n");
	return 1;
      }
