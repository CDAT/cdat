#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "picture.h"
#include "display.h"
#include "graph.h"
#include "vcs_legacy_names_length.h"

#define FALSE 0
#define TRUE 1
#define STRMAX 256

    extern FILE *fpin,*fpout,*fperr;
    extern struct table_mark Tm_tab;
    extern struct display_tab D_tab;
    extern struct gXy_tab GXy_tab;
    extern struct gYx_tab GYx_tab;
    extern struct gXY_tab GXY_tab;
    extern struct gSp_tab GSp_tab;

    extern int update_ind;

/*	Process a mark table assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procTm_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,c,stop=0,iv;
	int tokm;
	int change,new;
	char strm[STRMAX+1];
	char *pc;
	float v;
	struct table_mark *ptab, *ptb;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }
	change=new=FALSE;

/*		Look in the mark table table for the name.		*/
/*		Make a table entry if it doesn't exist			*/

	for (ptb=ptab=&Tm_tab; ptab!=NULL && cmpnbl(&str[3],ptab->name)!=0;)
	  {
	   if (ptab->next == NULL)
	     {
	      if (((ptab->next)=
			(struct table_mark *)malloc(sizeof(Tm_tab))) == NULL)
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

         if (ptab == &Tm_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (Tm_%s).\n",
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
                  ptab->mvp[j]=0.0;
                  ptab->mwc[j]=0.0;
               } else {
                  ptab->mvp[j]=1.0;
                  ptab->mwc[j]=1.0;
               }
           }
           ptab->mx = NULL;
           ptab->my = NULL;
           ptab->mtyp = NULL; ptab->mtyp_size = 0;
           ptab->msize = NULL; ptab->msize_size = 0;
           ptab->mci = NULL; ptab->mci_size = 0;
        }

	i=0;
        while ( ((c=getsttk(strm,&tokm)) || c == 0) )
          {
           if (c == EOF || tokm == EOF)
             {
              err_warn(1,fperr, "Error - EOF in secondary marker object for (%s%c).\n",
                                                str,tok);
              return 0;
             }
           if (c == 0)
             {
              err_warn(1,fperr, "Error - not a secondary marker object attribute name (%s%c%s%c).\n ",
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
                        sscanf (strm,"%f",&ptab->mvp[j]);
                     }
                     getsttk(strm,&tokm);
                 } else if ( strcmp(strm, "wc") == 0 )  {
                     for (j=0; j<4; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%f",&ptab->mwc[j]);
                     }
                     getsttk(strm,&tokm);
                 } else if ( strcmp(strm, "x") == 0 )  {
                     stop = 0;
                     if (get_points( &ptab->mx, &stop) == 0) return 0;
                     if (stop) break;

                 } else if ( strcmp(strm, "y") == 0 )  {
                     stop = 0;
                     if (get_points( &ptab->my, &stop ) == 0) return 0;
                     if (stop) break;
                 }
             }
           else
             {
              if (i == 0) {
                 ptab->mtyp_size=1; change=1;
                 get_int_size_and_set_value( &ptab->mtyp, 1, (int)v );
              } else if (i == 1) {
                   if ( strcmp(strm, "mtyp") != 0 ) {
                      ptab->msize_size=1; change=1;
                      get_float_size_and_set_value( &ptab->msize, 1, &v );
                   } else {
                      iv = ptab->mtyp_size = ptab->mtyp[0];
                      get_int_size_and_set_value( &ptab->mtyp, iv, &v );
                      for (j=0; j<iv; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%i",&ptab->mtyp[j]);
                      }
                      getsttk(strm,&tokm);
                      --i;
                   }
              } else if (i == 2) {
                   if ( strcmp(strm, "msize") != 0 ) {
                      ptab->mci_size=1; change=1;
                      get_int_size_and_set_value( &ptab->mci, 1, (int)v );
                   } else {
                      iv = ptab->msize_size = ptab->msize[0];
                      get_float_size_and_set_value( &ptab->msize, iv, &v );
                      for (j=0; j<iv; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%g",&ptab->msize[j]);
                      }
                      getsttk(strm,&tokm);
                      --i;
                   }
              } else if (i == 3) {
                  if ( strcmp(strm, "mci") != 0 ) {
                     if (ptab->priority != v) change=1;
                     ptab->priority=v;
                   } else {
                      iv = ptab->mci_size = ptab->mci[0];
                      get_int_size_and_set_value( &ptab->mci, iv, (int) v );
                      for (j=0; j<iv; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%i",&ptab->mci[j]);
                      }
                      getsttk(strm,&tokm);
                      --i;
                  }
                }
             }
           i++;
           if (tokm == ')') break;
          }
/*
	while (i < 3 && (c=getsttk(strm,&tokm)) != 0 &&
		isnum(strm) && tokm != EOF && (tokm == ',' || tokm == ')') )
	  {
	   sscanf (strm,"%f",&v);
	   if (new)
	     {
	      if (i == 0) ptab->mtyp=v;
	      if (i == 1) ptab->msize=v;
	      if (i == 2) ptab->mci=v;
	     }
	   else
	     {
	      if (i == 0) {if (ptab->mtyp != v) change=TRUE; ptab->mtyp=v;}
	      if (i == 1) {if (ptab->msize != v) change=TRUE; ptab->msize=v;}
	      if (i == 2) {if (ptab->mci != v) change=TRUE; ptab->mci=v;}
	     }
	   i++;
	   if (tokm == ')') break;
	  }
*/
	if (c == 0)
	  {
	   err_warn(1,fperr,
		"Error - empty value in the mark table (%s).\n",str);
	  }
/*
	if (!isnum(strm))
	  {
	   err_warn(1,fperr,
		"Error - non-number in the mark table (%s=%s).\n",str,strm);
	  }
	if (i != 4)
	  {
	   c=0;
	   if (ptb != ptab)
	     {
	      err_warn(1,fperr,
		  "Error - insufficient values, mark table (%s) deleted.\n",
							str);
	      ptb->next=ptab->next;
	      free(ptab);
	     }
	   else
	     {
	      err_warn(1,fperr,"Error - mark table (default) is incomplete.\n");
	     }
	  }
	if (tokm != ')' && tokm != ',' && tokm != EOF)
	  {
	   c=0;
	   err_warn(1,fperr,
		"Error - mark table delimiter (%s=xxx%c) is wrong.\n",
							str,tokm);
	  }
*/
	if (change) check_d_line(ptab->name);
	if (tokm == EOF)
	  {
	   c=EOF;
	   err_warn(1,fperr,"Error - EOF in mark table (%s).\n",str);
	  }
	return c;
      }

/*		Check whether the mark set is used for a graphics
		descriptor.  If so, a display update is needed.		*/

    int check_d_mark(name)
      char *name;
      {
   
       struct display_tab *pd;
       
       struct gXy_tab  *pgXy;
       struct gYx_tab  *pgYx;
       struct gXY_tab  *pgXY;
       struct gSp_tab  *pgSp;

       pd=&D_tab;
       while (pd != NULL)
	 {
	  if (cmpncs(pd->type,"marker")==0)
	    {
	      if (strcmp(name,pd->g_name)==0)
		{
		  pd->dsp_seg[3]=1;
		}
	    }
	  pgXy=&GXy_tab;
	  while (pgXy != NULL)
	    {
	     if (strcmp(pd->g_name,pgXy->name) == 0)
	       {
		if (strcmp(pgXy->pGXy_attr->mb,name) == 0)
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
		if (strcmp(pgYx->pGYx_attr->mb,name) == 0)
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
		if (strcmp(pgXY->pGXY_attr->mb,name) == 0)
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
	  pgSp=&GSp_tab;
	  while (pgSp != NULL)
	    {
	     if (strcmp(pd->g_name,pgSp->name) == 0)
	       {
		if (strcmp(pgSp->pGSp_attr->mb,name) == 0)
		  {
		   pd->dsp_seg[1]=FALSE;
		   pd->dsp_seg[3]=TRUE;
		   pd->leg_seg[1]=FALSE;
		   pd->leg_seg[3]=TRUE;
		   update_ind=TRUE;
		  }
	       }
	     pgSp=pgSp->next;
	    }
	  pd=pd->next;
         }

       if (name[0] == '\0') return 0;
       return 1;
      }

/*			Print marker attributes.			*/
    int prtTm(FILE *fp,struct table_mark *ptab)
      {
	if (ptab == NULL) return 0;
	fprintf (fp,"Tm_%s(", ptab->name);

	fprintf (fp,"\n   %d, mtyp(", ptab->mtyp_size);
        print_ints(fp, ptab->mtyp, ptab->mtyp_size);
	fprintf (fp,"\n   %d, msize(", ptab->msize_size);
        print_floats(fp, ptab->msize, ptab->msize_size);
	fprintf (fp,"\n   %d, mci(", ptab->mci_size);
        print_ints(fp, ptab->mci, ptab->mci_size);

        fprintf (fp,"\n   %d,", ptab->priority);
	fprintf (fp,"\n   projection=%s,\n", ptab->proj);
        fprintf (fp,"\n   vp(%g,%g,%g,%g),",
                ptab->mvp[0],ptab->mvp[1],ptab->mvp[2],ptab->mvp[3]);
        fprintf (fp,"\n   wc(%g,%g,%g,%g)",
                ptab->mwc[0],ptab->mwc[1],ptab->mwc[2],ptab->mwc[3]);
        if (ptab->mx != NULL) {
           fprintf (fp,",\n   x(");
           print_points(fp, ptab->mx->ps);
        }
        if (ptab->my != NULL) {
           fprintf (fp,",\n   y(");
           print_points(fp, ptab->my->ps);
        }
        fprintf (fp,"\n  )\n");
	return 1;
      }
