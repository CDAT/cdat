#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "graph.h"
#include "display.h"
#include <project.h>
#define STRMAX 256
#define TRUE 1
#define FALSE 0

extern FILE *fpin,*fpout,*fperr;

extern int update_ind;

extern struct display_tab D_tab;

extern struct projection_attr p_PRJ_list;

extern struct project_attr p_PRJ;

extern char Proj_attr[2][16];

    extern int Inactive;



    int procProj_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,k,c;
	int I[4];
	int tokm,tokf;
	char strm[STRMAX+1];
	char strf[STRMAX+1];
	char *pc;
	float v;
	struct projection_attr *pat;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }


/*			Set up the attribute structure for projection.	*/

	if ( (pat=(struct projection_attr *) malloc(sizeof(struct projection_attr))) == NULL)
	  {
	   err_warn(1,fperr,"Error memory for %s not found \n",str);
	   return 0;
          }

	strncpy (pat->name,&str[5],255); pat->name[254]='\0';
	pat->next=NULL;
/*			 nullify the set of attributes			*/

           pat->proj_type=0;
	   for (i=0;i<15;i++) pat->parm[i]=0.;

/*			Read in the attribute member assignments.	*/

	while ( ((c=getsttk(strm,&tokm)) || c == 0) && tokm != ')')
	  {
	   if (c == EOF || tokm == EOF)
	     {
	      err_warn(1,fperr,
		"Error - EOF in projection attributes for (%s%c).\n",
						str,tok);
	      killProj(pat);
	      return 0;
	     }
	   if (tokm != '(' && tokm != '=')
	     {
	      err_warn(1,fperr,
		"Error - not a proper token (%s%c%s %c).\n",
				str,*tok,strm,tokm);
	      killProj(pat);
	      return 0;
	     }
	   if (c == 0)
	     {
	      err_warn(1,fperr,
		"Error - not a projection attribute name (%s%c%s%c).\n",
		str,*tok,strm,tokm);
	      killProj(pat);
	      return 0;
	     }
	   for (pc=&(pat->name[0]), i=0;
		 i < 2 && cmpnbl(strm,Proj_attr[i]) != 0; i++)
	     {
	     }
	   if (i==0) /* parameters */
	     {
	      if (tokm == '=')
		while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF);
	      if (tokm != '(')
	        {
	         err_warn(1,fperr,
		   "Error - not a proper token (%s%c%s %c).\n",
						str,*tok,strm,tokm);
		 killProj(pat);
	         return 0;
	        }
	      c=j=0;
	      while (c != ')')
		{
		 if (j == 15)
		   {
		    err_warn(1,fperr,
			"Error - too many numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		    killProj(pat);
		    return 0;
		   }
		 for (k=0; k < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; k++)
				strf[k]=c;
		 if (c == EOF) {killProj(pat); return EOF;}
		 strf[k]='\0';
		 if (isnum(strf)) 
		   {
		     sscanf (strf,"%g",&v);
		     pat->parm[j]=(double)v;
		     j=j+1;
		   }
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    killProj(pat);
		    return 0;
		   }
		}
	      if (j != 15)
		{
		 err_warn(1,fperr,
			"Error - too few numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		 killProj(pat);
		 return 0; 
		}
	      while (!istoken(tokm=getp(fpin,fpout)))
		if (tokm == EOF)
		  {
		   killProj(pat);
		   return EOF;
		  }
/*	      if (tokm == ')') ungetp(tokm,fpin);			*/
	     }
	   else if (i==1)
	     {
	       for (j=0;j<STRMAX && (tokm=getp(fpin,fpout))!=',' && tokm!=')';
		    strf[j++]=tokm);
	       strf[j]='\0';
	       if (isnum(strf))
		 {
		   sscanf(strf,"%f",&v);
		   pat->proj_type=(long)v;
		 }
	     }
	   else
	     
	     /*		didn't find a name.					*/
	     
	     {
	       err_warn(1,fperr,"Error - didn't find the name (%s%c%s%c).\n",
			str,*tok,strm,tokm);
	       killProj(pat);
	       return 0;
	     }
	   if (tokm == ')') break;
	  }
	c=chk_mov_Proj (pat);
	return c;
      }








/*			Check the projection table entry and move
			to the table if it's ok.			*/

    int chk_mov_Proj (struct projection_attr *pj)

      {
	int i,k;
	struct projection_attr *pat,*pan;
	struct projection_attr *ptab,*ptb;
	struct display_tab *pd;
	int I[13];

	for(ptb=ptab=&p_PRJ_list;
		ptab != NULL && strcmp(ptab->name,pj->name) != 0;
						ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

         if (ptab == &p_PRJ_list)
	  {
	   err_warn(1,fperr,"Error - can't replace the default projection (%s).\n",
				pj->name);
	   killProj(pj);
	   return 0;
	  }


/*	If it isn't modifying a projection entry then add it.	*/

	if (ptab == NULL)
	  {
	   ptb->next=pj;
	   pj->next=NULL;
	   return 1;
	  }

	for (i=0;i<13;i++) I[i]=0;

/*		Check for changes and move modified parameters.		*/

	pat=pj;
	pan=ptab;

	for (i=0;i<15;i++)
	  {
	    if (pat->parm[i]!=pan->parm[i]) I[0]=1;
	  }
	/* Now sets everything to I[0] */
	for (i=1;i<13;i++) I[i]=I[0];


/*		Check whether changes were made.  If not we're done.	*/

	for (i=0,k=0;i<12;i++) k+=I[i];
	if (k == 0)
	  {
	   ptb->next=ptab;
	   killProj(pj);
	   return 1;
	  }
	ptb->next=pj;
	pj->next=ptab->next;
	killProj(pj);

/*		If the table entry is being modified and it's being
		displayed, set for replacement of segments.		*/

	for(pd=&D_tab; pd!=NULL; pd=pd->next)
	  {
	   if ( strcmp(pd->proj_name,pj->name)==0)
	     {

	      if (I[0] || I[9])
	        {
	         pd->xt1_seg[3]=TRUE;
	         pd->xt2_seg[3]=TRUE;
	         pd->xmta_seg[3]=TRUE;
	         pd->xmtb_seg[3]=TRUE;
	         pd->yt1_seg[3]=TRUE;
	         pd->yt2_seg[3]=TRUE;
	         pd->ymta_seg[3]=TRUE;
	         pd->ymtb_seg[3]=TRUE;
	         pd->xl1_seg[3]=TRUE;
	         pd->xl2_seg[3]=TRUE;
	         pd->yl1_seg[3]=TRUE;
	         pd->yl2_seg[3]=TRUE;
	         pd->dsp_seg[3]=TRUE;
	         update_ind=TRUE;
	        }
	      if (I[1])
	        {
	         pd->xt1_seg[3]=TRUE;
	         pd->xl1_seg[3]=TRUE;
	         update_ind=TRUE;
	        }
	      if (I[2])
	        {
	         pd->xt2_seg[3]=TRUE;
	         pd->xl2_seg[3]=TRUE;
	         update_ind=TRUE;
	        }
	      if (I[3]) {pd->xmta_seg[3]=TRUE;update_ind=TRUE;}
	      if (I[4]) {pd->xmtb_seg[3]=TRUE;update_ind=TRUE;}
	      if (I[5])
	        {
	         pd->yt1_seg[3]=TRUE;
	         pd->yl1_seg[3]=TRUE;
	         update_ind=TRUE;
	        }
	      if (I[6])
	        {
	         pd->yt2_seg[3]=TRUE;
	         pd->yl2_seg[3]=TRUE;
	         update_ind=TRUE;
	        }
	      if (I[7]) {pd->ymta_seg[3]=TRUE;update_ind=TRUE;}
	      if (I[8]) {pd->ymtb_seg[3]=TRUE;update_ind=TRUE;}
	      if (I[9] || I[10])
	        {
	         pd->dsp_seg[3]=TRUE;
	         pd->leg_seg[3]=TRUE;
	         update_ind=TRUE;
	        }
	     }
	  }
	if (!Inactive && fpout != NULL) prtProj(fpout,pj);
	return 1;
      }

/* 		Print the Projection attributes that are defined. */

int prtProj(FILE *fp,struct projection_attr *ptab)
     
{
  int i;
  int k=0;
  char *pc;
  struct projection_attr *p;
  
  if (ptab == NULL) return 0;
  fprintf (fp,"Proj_%s(\nparameters(",ptab->name);
  for (i=0;i < 14;i++) fprintf (fp,"%f, ",ptab->parm[i]);
  fprintf (fp,"%g ",ptab->parm[14]);
  fprintf (fp,"),\n");
  fprintf (fp,"type = %i",ptab->proj_type);
  fprintf (fp,"  )\n");
return 1;
}
