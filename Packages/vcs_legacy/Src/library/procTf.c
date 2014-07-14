#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "picture.h"
#include "display.h"
#include "graph.h"
#include "vcs_legacy_names_length.h"

#define STRMAX 256
#define FALSE 0
#define TRUE 1

    extern FILE *fpin,*fpout,*fperr;

    extern struct display_tab D_tab;
    extern struct table_fill Tf_tab;
    extern struct gfi_tab Gfi_tab;
    extern struct gfo_tab Gfo_tab;

    extern int update_ind;

/*	Process a fill table assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procTf_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,c,stop,iv;
	int tokm;
	int change,new;
	char *pc;
	char strm[STRMAX+1];
	float v;
	struct table_fill *ptab, *ptb;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }

/*		Look in the fill table table for the name.		*/
/*		Make a table entry if it doesn't exist			*/

	change=new=FALSE;

	for (ptb=ptab=&Tf_tab; ptab!=NULL && cmpnbl(&str[3],ptab->name)!=0;)
	  {
	   if (ptab->next == NULL)
	     {
	      if (((ptab->next)=
			(struct table_fill *)malloc(sizeof(Tf_tab))) == NULL)
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

       if (ptab == &Tf_tab)
	 {
	  err_warn(1,fperr,"Error - can't replace the default (Tf_%s).\n",
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
                  ptab->fvp[j]=0.0;
                  ptab->fwc[j]=0.0;
               } else {
                  ptab->fvp[j]=1.0;
                  ptab->fwc[j]=1.0;
               }
           }
           ptab->fx = NULL;
           ptab->fy = NULL;
           ptab->x=0;
           ptab->y=0;
           ptab->w=0.1;
           ptab->h=0.1;
           ptab->y=0;
           ptab->fais = NULL; ptab->fais_size = 0;
           ptab->fasi = NULL; ptab->fasi_size = 0;
           ptab->faci = NULL; ptab->faci_size = 0;
        }
	i=0;
        while ( ((c=getsttk(strm,&tokm)) || c == 0) )
          {
           if (c == EOF || tokm == EOF)
             {
              err_warn(1,fperr, "Error - EOF in secondary fill area object for (%s%c).\n",
                                                str,tok);
              return 0;
             }
           if (c == 0)
             {
              err_warn(1,fperr, "Error - not a secondary fill area object attribute name (%s%c%s%c).\n ",
                str,*tok,strm,tokm);
              return 0;
             }

           if (i < 8)
              sscanf (strm,"%f",&v);
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
                        sscanf (strm,"%f",&ptab->fvp[j]);
                     }
                     getsttk(strm,&tokm);
                 } else if ( strcmp(strm, "wc") == 0 )  {
                     for (j=0; j<4; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%f",&ptab->fwc[j]);
                     }
                     getsttk(strm,&tokm);
                 } else if ( strcmp(strm, "x") == 0 )  {
                     stop = 0;
                     if (get_points( &ptab->fx, &stop) == 0) return 0;
                     if (stop) break;
                 } else if ( strcmp(strm, "y") == 0 )  {
                     stop = 0;
                     if (get_points( &ptab->fy, &stop ) == 0) return 0;
                     if (stop) break;
                 }
             }
           else
             {
              if (i == 0) {
                 ptab->fais_size=1; change=1;
                 get_int_size_and_set_value( &ptab->fais, 1, (int)v );
              } else if (i == 1) {
                   if ( strcmp(strm, "fais") != 0 ) {
                      ptab->fasi_size=1; change=1;
                      get_int_size_and_set_value( &ptab->fasi, 1, (int)v );
                   } else {
                      iv = ptab->fais_size = ptab->fais[0];
                      get_int_size_and_set_value( &ptab->fais, iv, (int) v );
                      for (j=0; j<iv; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%i",&ptab->fais[j]);
                      }
                      getsttk(strm,&tokm);
                      --i;
                   }
              } else if (i == 2) {
                   if ( strcmp(strm, "fasi") != 0 ) {
                      ptab->faci_size=1; change=1;
                      get_int_size_and_set_value( &ptab->faci, 1, (int)v );
                   } else {
                      iv = ptab->fasi_size = ptab->fasi[0];
                      get_int_size_and_set_value( &ptab->fasi, iv, (int) v );
                      for (j=0; j<iv; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%i",&ptab->fasi[j]);
                      }
                      getsttk(strm,&tokm);
                      --i;
                   }
              } else if (i == 3) {
                  if ( strcmp(strm, "faci") != 0 ) {
                     if (ptab->x != v) change=1;
                     ptab->x=v;
                   } else {
                      iv = ptab->faci_size = ptab->faci[0];
                      get_int_size_and_set_value( &ptab->faci, iv, (int) v );
                      for (j=0; j<iv; j++) {
                         c=getsttk(strm,&tokm);
                         sscanf (strm,"%i",&ptab->faci[j]);
                      }
                      getsttk(strm,&tokm);
                      --i;
                  }
              }
              else if (i == 4) {if (ptab->y != v) change=1; ptab->y=v;}
              else if (i == 5) {if (ptab->w != v) change=1; ptab->w=v;}
              else if (i == 6) {if (ptab->h != v) change=1; ptab->h=v;}
              else if (i == 7) {if (ptab->priority != v) change=1; ptab->priority=v;}
             }
           i++;
           if (tokm == ')') break;
          }
	if (c == 0)
	  {
	   err_warn(1,fperr,
		"Error - empty value in the fill table (%s).\n",str);
	  }
/*
	if (!isnum(strm))
	  {
	   err_warn(1,fperr,
		"Error - non-number in the fill table (%s=%s).\n",str,strm);
	  }
	if (i < 6)
	  {
	   c=0;
	   if (ptb != ptab)
	     {
	      err_warn(1,fperr,
		  "Error - insufficient values, fill table (%s) deleted.\n",
							str);
	      ptb->next=ptab->next;
	      free(ptab);
	     }
	   else
	     {
	      err_warn(1,fperr,
			"Error - fill table entry (%s) is incomplete.\n",str);
	     }
	  }
	else if (change)
	  {

*		Insert search through graphics descriptions.		*

	   check_d_fill(ptab->name);

	  }
	if (tokm != ')' && tokm != ',' && tokm != EOF)
	  {
	   c=0;
	   err_warn(1,fperr,
		"Error - fill table delimiter (%s=xxx%c) is wrong.\n",
							str,tokm);
	  }
*/
        if (change) check_d_fill(ptab->name);
	if (tokm == EOF)
	  {
	   c=EOF;
	   err_warn(1,fperr,"Error - EOF in fill table (%s).\n",str);
	  }
	return c;
      }
/*		Check whether the fill set is used for a display.
		If so, a display update is needed.			*/

    int check_d_fill(name)
      char *name;
      {
   
       struct display_tab *pd;

       struct gfi_tab   *pg;
       struct gfi_attr  *pgfi;
       struct fill_range *piso;

       struct gfo_tab   *pgo;
       struct gfo_attr  *pgfo;
       
       pd=&D_tab;
       while (pd != NULL)
	 {
	  if (cmpncs(pd->type,"fillarea")==0)
	    {
	      if (strcmp(name,pd->g_name)==0)
		{
		  pd->dsp_seg[3]=1;
		}
	    }
	  if (cmpncs(pd->type,"isofill") == 0)
	    {
	     pg=&Gfi_tab;
	     while (pg != NULL)
	       {
	        if (strcmp(pd->g_name,pg->name) == 0)
	          {
		   pgfi=pg->pGfi_attr;
		   piso=pgfi->line;
		   while (piso != NULL)
		     {
		      if (strcmp(piso->fill_name,name) == 0)
		        {
		         pd->dsp_seg[1]=FALSE;
		         pd->dsp_seg[3]=TRUE;
		         pd->leg_seg[1]=FALSE;
		         pd->leg_seg[3]=TRUE;
		         update_ind=TRUE;
		         break;
		        }
		      piso=piso->next;
		     }
	          }
	        pg=pg->next;
	       }
	    }
	  if (cmpncs(pd->type,"outfill") == 0)
	    {
	     pgo=&Gfo_tab;
	     while (pgo != NULL)
	       {
	        if (strcmp(pd->g_name,pgo->name) == 0)
	          {
		   pgfo=pgo->pGfo_attr;
		   if (strcmp(pgfo->f,name) == 0)
		     {
		      pd->dsp_seg[1]=FALSE;
		      pd->dsp_seg[3]=TRUE;
		      update_ind=TRUE;
		     }
	          }
	        pgo=pgo->next;
	       }
	    }
	  pd=pd->next;
         }
       return 1;
      }

/*			Print fill area attributes.			*/

    int prtTf(FILE *fp,struct table_fill *ptab)
      {
	if (ptab == NULL) return 0;
	fprintf (fp,"Tf_%s(", ptab->name);
	if (ptab->fais_size!=0)  
	  {
	    fprintf (fp,"\n  %d, fais(", ptab->fais_size);
	    print_ints(fp, ptab->fais, ptab->fais_size);
	  }
	if (ptab->fasi_size!=0)
	  {
	    fprintf (fp,"\n   %d, fasi(", ptab->fasi_size);
	    print_ints(fp, ptab->fasi, ptab->fasi_size);
	  }
	if (ptab->faci_size!=0)
	  {
	    fprintf (fp,"\n   %d, faci(", ptab->faci_size);
	    print_ints(fp, ptab->faci, ptab->faci_size);
	  }
        fprintf (fp,"\n   %g,%g,%g,%g,%d,",
                 ptab->x,ptab->y,ptab->w,ptab->h,ptab->priority);
	fprintf (fp,"\n  projection=%s,", ptab->proj);
        fprintf (fp,"\n   vp(%g,%g,%g,%g),",
                ptab->fvp[0],ptab->fvp[1],ptab->fvp[2],ptab->fvp[3]);
        fprintf (fp,"\n   wc(%g,%g,%g,%g)",
                ptab->fwc[0],ptab->fwc[1],ptab->fwc[2],ptab->fwc[3]);
        if (ptab->fx != NULL) {
           fprintf (fp,",\n   x(");
           print_points(fp, ptab->fx->ps);
        }
        if (ptab->fy != NULL) {
           fprintf (fp,",\n   y(");
           print_points(fp, ptab->fy->ps);
        }
        fprintf (fp,"\n  )\n");
	return 1;
      }
