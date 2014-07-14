#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "graph.h"
#include "display.h"

#define STRMAX 256
#define TRUE 1
#define FALSE 0

    extern FILE *fpin,*fpout,*fperr;

    extern int update_ind;

    extern struct gcon_tab Gcon_tab;
    extern struct display_tab D_tab;

/*			Continents attributes.				*/

    extern char Gcon_attr[12][16];

    extern int Inactive;

/*	Process a graphics contour assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procGcon_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,c;
	char strm[STRMAX+1];
	char strf[STRMAX+1];
	char *pc;
	int tokm;
	struct gcon_tab *ptab;
	struct gcon_attr *pat;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }

/*		Set up a table entry for continents.			*/

	if ((ptab=(struct gcon_tab *)malloc(sizeof(Gcon_tab))) == NULL)
	  {
	   err_warn(1,fperr,"Error - memory for %s not found \n",str);
	   return 0;
	  }
	strncpy (ptab->name,&str[5],17); ptab->name[16]='\0';
	ptab->next=NULL;
	ptab->pGcon_attr=NULL;

/*			Set up the attribute structure for continents.	*/

	if ( (pat=ptab->pGcon_attr=
	        (struct gcon_attr *) malloc(sizeof(struct gcon_attr))) == NULL)
	  {
	   err_warn(1,fperr,"Error memory for %s not found \n",str);
	   free((char *)ptab);
	   return 0;
          };

/*			 Nullify the set of attributes			*/

	for (pc=&(pat->proj[0]),i=0; i < 9;*pc='\0',pc+=256,i++);
	for (i=0; i < 4; pat->dsp[i++]=0.0);
	pat->lb[0]='\0';

/*			Read in the attribute member assignments.	*/

	while ( ((c=getsttk(strm,&tokm)) || c == 0) && tokm != ')')
	  {
	   if (c == EOF || tokm == EOF)
	     {
	      err_warn(1,fperr,
		"Error - EOF in continents attributes for (%s%c).\n",
						str,tok);
	      free((char *)ptab);
	      free((char *)pat);
	      return 0;
	     }
	   if (tokm != '(' && tokm != '=')
	     {
	      err_warn(1,fperr,
		"Error - not a proper token (%s%c%s %c).\n",
				str,*tok,strm,tokm);
	      free((char *)ptab);
	      free((char *)pat);
	      return 0;
	     }
	   if (c == 0)
	     {
	      err_warn(1,fperr,
		"Error - not a continents attribute name (%s%c%s%c).\n",
		str,*tok,strm,tokm);
	      free((char *)ptab);
	      free((char *)pat);
	      return 0;
	     }

/*			Find which attribute is being assigned.		*/

	   for (pc=&(pat->proj[0]),i=0;i<11&&cmpnbl(strm,Gcon_attr[i])!=0;i++)
	      if (i < 9) pc+=256;

/*			Assign labelling and projection attributes.	*/

	   if (i < 9)
	     {
	      j=0;
	      *pc='\0';
	      while (!istoken(tokm=c=getp(fpin,fpout)) || c == '*')
		{
		 if (c > ' ')
		   {
		    if (j < 255)
		      {*(pc+j)=c; *(pc+j+1)='\0';}
		    j++;
		   }
		}
	      if (c != ',' && c != ')')
		{
		 err_warn(1,fperr,
			"Error - syntax is incorrect (%s%c%s%c%s%c).\n",
			str,*tok,strm,tokm,pc,c);
		 free((char *)ptab);
		 free((char *)pat);
		 return 0;
		}
	     }

/*		Assign the real coordinates for the display space.	*/

	   else if (i == 9)
	     {
	      if (tokm == '=')
		while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF);
	      if (tokm != '(')
	        {
	         err_warn(1,fperr,
		   "Error - not a proper token (%s%c%s %c).\n",
						str,*tok,strm,tokm);
		 free((char *)ptab);
		 free((char *)pat);
	         return 0;
	        }
	      c=j=0;
	      while (c != ')')
		{
		 if (j == 4)
		   {
		    err_warn(1,fperr,
			"Error - too many numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		    free((char *)ptab);
		    free((char *)pat);
		    return 0;
		   }
		 for (i=0; i < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; i++)
				strf[i]=c;
		 if (c == EOF) return EOF;
		 strf[i]='\0';
		 if (isnum(strf)) sscanf (strf,"%f",&(pat->dsp[j++]));
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    free((char *)ptab);
		    free((char *)pat);
		    return 0;
		   }
		}
	      if (j != 4)
		{
		 err_warn(1,fperr,
			"Error - too few numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		 free((char *)ptab);
		 free((char *)pat);
		 return 0; 
		}
	      while (!istoken(tokm=getp(fpin,fpout)))
		if (tokm == EOF)
		  {
		   free((char *)ptab);
		   free((char *)pat);
		   return EOF;
		  }
	      if (tokm == ')') ungetp(tokm,fpin);
	     }

/*		Store line bundle name.					*/
	   else if (i == 10)
	     {
	      j=0;
	      pc=&pat->lb[0];
	      *pc='\0';
	      while (!istoken(tokm=c=getp(fpin,fpout)) )
		{
		 if (isalnum(c) || c == '.')
		   {
		    if (j < 16)
		      {*(pc+j)=c; *(pc+j+1)='\0';}
		    j++;
		   }
		}
	      if (c != ',' && c != ')')
		{
		 err_warn(1,fperr,
			"Error - syntax is incorrect (%s%c%s%c%s%c).\n",
			str,*tok,strm,tokm,pc,c);
		 free((char *)ptab);
		 free((char *)pat);
		 return 0;
		}
	     }

	   else if (i == 11)
	     {
              for (j=0;j<STRMAX && (tokm=getp(fpin,fpout))!=',' && tokm!=')';
                                        strf[j++]=tokm);
              strf[j]='\0';
              if (isnum(strf)) {
		 sscanf(strf,"%d",&pat->cont_type);
	      } else {
		 err_warn(1,fperr,
			"Error - syntax is incorrect (%s%c%s%c%s%c).\n",
			str,*tok,strm,tokm,pc,c);
		 free((char *)ptab);
		 free((char *)pat);
		 return 0;
	      }
	     }

	   else

/*		i > 11, didn't find a name.				*/

	     {
	      err_warn(1,fperr,"Error - didn't find the name (%s%c%s%c).\n",
				str,*tok,strm,tokm);
	      free((char *)ptab);
	      free((char *)pat);
	      return 0;
	     }
	   if (tokm == ')') break;
	  }
	c=chk_mov_Gcon (ptab);
	return c;
      }

/*			Check the continents table entry and move
			to the table if it's ok.			*/

    int chk_mov_Gcon (struct gcon_tab *gtab)

      {
	int i,k;
	struct gcon_attr *pat,*pan;
	struct gcon_tab *ptab,*ptb;
	struct display_tab *pd;
	int I[12];

	for(ptb=ptab=&Gcon_tab;
		ptab != NULL && strcmp(ptab->name,gtab->name) != 0;
						ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

        if (ptab == &Gcon_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (Gcon_%s).\n",
				gtab->name);
	   killGcon(gtab);
	   return 0;
	  }


/*	If it isn't modifying a continents table entry then add it.	*/

	if (ptab == NULL)
	  {
	   ptb->next=gtab;
	   return 1;
	  }

	for (i=0;i<12;i++) I[i]=0;

/*		Check for changes and move modified parameters.		*/

	pat=gtab->pGcon_attr;
	pan=ptab->pGcon_attr;

	if (strncmp(pat->proj,pan->proj,255) != 0)
	  { strncpy(pan->proj,pat->proj,256);  pan->proj[255]='\0'; I[0]=1; }
	if (strncmp(pat->xtl1,pan->xtl1,255) != 0)
	  { strncpy(pan->xtl1,pat->xtl1,256);  pan->xtl1[255]='\0'; I[1]=1; }
	if (strncmp(pat->xtl2,pan->xtl2,255) != 0)
	  { strncpy(pan->xtl2,pat->xtl2,256);  pan->xtl2[255]='\0'; I[2]=1; }
	if (strncmp(pat->xmt1,pan->xmt1,255) != 0)
	  { strncpy(pan->xmt1,pat->xmt1,256);  pan->xmt1[255]='\0'; I[3]=1; }
	if (strncmp(pat->xmt2,pan->xmt2,255) != 0)
	  { strncpy(pan->xmt2,pat->xmt2,256);  pan->xmt2[255]='\0'; I[4]=1; }
	if (strncmp(pat->ytl1,pan->ytl1,255) != 0)
	  { strncpy(pan->ytl1,pat->ytl1,256);  pan->ytl1[255]='\0'; I[5]=1; }
	if (strncmp(pat->ytl2,pan->ytl2,255) != 0)
	  { strncpy(pan->ytl2,pat->ytl2,256);  pan->ytl2[255]='\0'; I[6]=1; }
	if (strncmp(pat->ymt1,pan->ymt1,255) != 0)
	  { strncpy(pan->ymt1,pat->ymt1,256);  pan->ymt1[255]='\0'; I[7]=1; }
	if (strncmp(pat->ymt2,pan->ymt2,255) != 0)
	  { strncpy(pan->ymt2,pat->ymt2,256);  pan->ymt2[255]='\0'; I[8]=1; }
	if (pat->dsp[0] != pan->dsp[0]) {pan->dsp[0]=pat->dsp[0]; I[9]=1; }
	if (pat->dsp[1] != pan->dsp[1]) {pan->dsp[1]=pat->dsp[1]; I[9]=1; }
	if (pat->dsp[2] != pan->dsp[2]) {pan->dsp[2]=pat->dsp[2]; I[9]=1; }
	if (pat->dsp[3] != pan->dsp[3]) {pan->dsp[3]=pat->dsp[3]; I[9]=1; }
	if (strncmp(pat->lb,pan->lb,16) != 0)
	  { strncpy(pan->lb,pat->lb,17);  pan->lb[16]='\0'; I[10]=1; }
	if (pat->cont_type != pan->cont_type) {pan->cont_type=pat->cont_type; I[11]=1; }


/*		Check whether changes were made.  If not we're done.	*/

	for (i=0,k=0;i<12;i++) k+=I[i];
	killGcon(gtab);
	if (k == 0) return 1;

/*		If the table entry is being modified and it's being
		displayed, set for replacement of segments.		*/

	for(pd=&D_tab;pd!=NULL;pd=pd->next)
	 if(cmpncs("continents",pd->type)==0&&strcmp(pd->g_name,ptab->name)==0)
	  {
	   if (I[0])
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
	   if (I[9] || I[10] || I[11])
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
	      pd->leg_seg[3]=TRUE;
	      update_ind=TRUE;
	     }
	  }
	if (!Inactive && fpout != NULL) prtGcon(fpout,ptab);
	return 1;
      }

/*		Print the continents attributes that are defined.	*/

    int prtGcon(FILE *fp,struct gcon_tab *ptab)

      {
	int i;
	int k=0;
	char *pc;
	struct gcon_attr *pgcon;

	if ((pgcon=ptab->pGcon_attr) != NULL)
	    {
	     pc=pgcon->proj;
	     fprintf (fp,"Gcon_%s(\n   ",ptab->name);
	     k=0;
	     for (i=0;i < 9;i++,pc+=256)
	       {
		if (*pc != '\0')
			k+=fprintf (fp,"%s=%s,",Gcon_attr[i],pc);
		if (k > 60)
		  {
		   fprintf (fp,"\n   ");
		   k=3;
		  }
	       }
	     fprintf (fp,"\n   %s(%g,%g,%g,%g),",
		Gcon_attr[9],pgcon->dsp[0],pgcon->dsp[1],
		pgcon->dsp[2],pgcon->dsp[3]);
	     fprintf (fp,"\n   %s=%s,",Gcon_attr[10],pgcon->lb);
	     fprintf (fp,"\n   %s=%d,",Gcon_attr[11],pgcon->cont_type);
	     fprintf (fp,"\n  )\n");
	    }
       return 1;
      }
