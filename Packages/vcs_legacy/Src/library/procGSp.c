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

    extern struct gSp_tab GSp_tab;
    extern struct display_tab D_tab;

/*	Graphics X([x][,y][,z][,t]) vs Y([x][,y][,z][,t]) names.*/

    extern char GSp_attr[16][16];


    extern int Inactive;

/*	Process a graphics ScatterPlot of X(x,y,z,t) vs Y(x,y,z,t) assignment.
	The string "scatter" must be in str[] and the following
	token must be in tok.						*/

    int procGSp_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,k,c;
	int tokm;
	char strm[STRMAX+1];
	char strf[STRMAX+1];
	char *pc;
	float v;
	int iv;
	struct gSp_tab *ptab;
	struct gSp_attr *pat;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }

/*		Set up a table entry for X(x,y,z,t) vs Y(x,y,z,t).	*/

	if ((ptab=(struct gSp_tab *)malloc(sizeof(GSp_tab))) == NULL)
	  {
	   err_warn(1,fperr,"Error - memory for %s not found \n",str);
	   return 0;
	  }
	strncpy (ptab->name,&str[4],17); ptab->name[16]='\0';
	ptab->next=NULL;
	ptab->pGSp_attr=NULL;

/*		Set up the attribute structure for X(t) vs Y(t).	*/

	if ( (pat=ptab->pGSp_attr=
	         (struct gSp_attr *) malloc(sizeof(struct gSp_attr))) == NULL)
	  {
	   err_warn(1,fperr,"Error memory for %s not found \n",str);
	   killGSp(ptab);
	   return 0;
          };

/*			 nullify the set of attributes			*/

	for (pc=&(pat->proj[0]),i=0; i < 9; *pc='\0',pc+=256,i++);
	for (i=0; i < 4; pat->dsp[i++]=1.e20);
	for (i=0; i < 4; pat->idsp[i++]=0);

	pat->xat[0]='\0';
	pat->yat[0]='\0';
	pat->mb[0]='\0';
	strcpy(pat->timeunits,VCS_DEFAULT_TIME_UNITS);
	pat->calendar=VCS_DEFAULT_CALENDAR;

/*			Read in the attribute member assignments.	*/

	while ( ((c=getsttk(strm,&tokm)) || c == 0) && tokm != ')')
	  {
	   if (c == EOF || tokm == EOF)
	     {
	      err_warn(1,fperr,
		"Error - EOF in graphic ScatterPlot attributes for (%s%c).\n",
						str,tok);
	      killGSp(ptab);
	      return 0;
	     }
	   if (tokm != '(' && tokm != '=')
	     {
	      err_warn(1,fperr,
		"Error - not a proper token (%s%c%s %c).\n",
				str,*tok,strm,tokm);
	      killGSp(ptab);
	      return 0;
	     }
	   if (c == 0)
	     {
	      err_warn(1,fperr,
	      "Error - not a graphic ScatterPlot attribute name (%s%c%s%c).\n",
		str,*tok,strm,tokm);
	      killGSp(ptab);
	      return 0;
	     }

/*			Find which attribute is being assigned.		*/

	   for (pc=&(pat->proj[0]), i=0;
		 i < 16 && cmpnbl(strm,GSp_attr[i]) != 0; i++)
	     {
	      if (i < 9) pc+=256;
	     }

/*			Assign labelling and projection attributes.	*/

	   if (i < 9)
	     {
	      j=0;
	      *pc='\0';
	      while (!istoken(tokm=c=getp(fpin,fpout)) || c == '*' )
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
		 killGSp(ptab);
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
		 killGSp(ptab);
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
		    killGSp(ptab);
		    return 0;
		   }
		 for (k=0; k < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; k++)
				strf[k]=c;
		 if (c == EOF) {killGSp(ptab); return EOF;}
		 strf[k]='\0';
		 if (isnum(strf)) sscanf (strf,"%f",&(pat->dsp[j++]));
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    killGSp(ptab);
		    return 0;
		   }
		}
	      if (j != 4)
		{
		 err_warn(1,fperr,
			"Error - too few numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		 killGSp(ptab);
		 return 0; 
		}
	      while (!istoken(tokm=getp(fpin,fpout)))
		if (tokm == EOF)
		  {
		   killGSp(ptab);
		   return EOF;
		  }
/*	      if (tokm == ')') ungetp(tokm,fpin);			*/
	     }

/*		Store the X(t) vs Y(t) range definitions.		*/

	   else if (i >= 10 && i < 13)
	     {

	      for (j=0;j<STRMAX && (tokm=getp(fpin,fpout))!=',' && tokm!=')';
					strf[j++]=tokm);
	      strf[j]='\0';
              if (i == 10) {strncpy(pat->xat,strf,17); pat->xat[16] ='\0';}
              if (i == 11) {strncpy(pat->yat,strf,17); pat->yat[16] ='\0';}
	      if (i == 12) {strncpy(pat->mb,strf,17); pat->mb[16] = '\0';}
	     }
	   else if (i == 13)
             {
              for (j=0;j<STRMAX && (tokm=getp(fpin,fpout))!=',' && tokm!=')';
                                        strf[j++]=tokm);
              strf[j]='\0';
	      strcpy(pat->timeunits,strf);

             }
	   else if (i == 14)
             {
              for (j=0;j<STRMAX && (tokm=getp(fpin,fpout))!=',' && tokm!=')';
                                        strf[j++]=tokm);
              strf[j]='\0';
              if (isnum(strf))
                {
                 sscanf(strf,"%d",&iv);
                 pat->calendar=iv;
                }
             }
	   else if (i == 15)
	     {
	      if (tokm == '=')
		while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF);
	      if (tokm != '(')
	        {
	         err_warn(1,fperr,
		   "Error - not a proper token (%s%c%s %c).\n",
						str,*tok,strm,tokm);
		 killGSp(ptab);
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
		    killGSp(ptab);
		    return 0;
		   }
		 for (k=0; k < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; k++)
				strf[k]=c;
		 if (c == EOF) {killGSp(ptab); return EOF;}
		 strf[k]='\0';
		 if (isnum(strf)) sscanf (strf,"%d",&(pat->idsp[j++]));
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    killGSp(ptab);
		    return 0;
		   }
		}
	      if (j != 4)
		{
		 err_warn(1,fperr,
			"Error - too few numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		 killGSp(ptab);
		 return 0; 
		}
	      while (!istoken(tokm=getp(fpin,fpout)))
		if (tokm == EOF)
		  {
		   killGSp(ptab);
		   return EOF;
		  }
/*	      if (tokm == ')') ungetp(tokm,fpin);			*/
	     }

	   else

/*		didn't find a name.					*/

	     {
	      err_warn(1,fperr,"Error - didn't find the name (%s%c%s%c).\n",
				str,*tok,strm,tokm);
	      killGSp(ptab);
	      return 0;
	     }
	   if (tokm == ')') break;
	  }
	c=chk_mov_GSp (ptab);
	return c;
      }



/*			Check the ScatterPlot table entry and move
			to the table if it's ok.			*/

    int chk_mov_GSp (struct gSp_tab *gtab)

      {
	int i,k;
	struct gSp_attr *pat,*pan;
	struct gSp_tab *ptab,*ptb;
	struct display_tab *pd;
	int I[12];

	for(ptb=ptab=&GSp_tab;
		ptab != NULL && strcmp(ptab->name,gtab->name) != 0;
						ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

         if (ptab == &GSp_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (GSp_%s).\n",
				gtab->name);
	   killGSp(gtab);
	   return 0;
	  }


/*	If it isn't modifying a ScatterPlot table entry then add it.	*/

	ptb->next=gtab;
	gtab->next=NULL;
	if (ptab == NULL) return 1;

	for (i=0;i<12;i++) I[i]=0;

/*		Check for changes and move modified parameters.		*/

	pat=gtab->pGSp_attr;
	pan=ptab->pGSp_attr;

	if (strncmp(pat->proj,pan->proj,255) != 0) I[0]=1;
	if (strncmp(pat->xtl1,pan->xtl1,255) != 0) I[1]=1;
	if (strncmp(pat->xtl2,pan->xtl2,255) != 0) I[2]=1;
	if (strncmp(pat->xmt1,pan->xmt1,255) != 0) I[3]=1;
	if (strncmp(pat->xmt2,pan->xmt2,255) != 0) I[4]=1;
	if (strncmp(pat->ytl1,pan->ytl1,255) != 0) I[5]=1;
	if (strncmp(pat->ytl2,pan->ytl2,255) != 0) I[6]=1;
	if (strncmp(pat->ymt1,pan->ymt1,255) != 0) I[7]=1;
	if (strncmp(pat->ymt2,pan->ymt2,255) != 0) I[8]=1;
	if (pat->dsp[0] != pan->dsp[0] || pat->dsp[1] != pan->dsp[1] ||
	    pat->dsp[2] != pan->dsp[2] || pat->dsp[3] != pan->dsp[3])
						 I[9]=1;
	if (pat->idsp[0] != pan->idsp[0]) I[9]=1;
	if (pat->idsp[1] != pan->idsp[1]) I[9]=1;
	if (pat->idsp[2] != pan->idsp[2]) I[9]=1;
	if (pat->idsp[3] != pan->idsp[3]) I[9]=1;
	if (strncmp(pat->timeunits,pan->timeunits,255) != 0) I[9]=1;
	if (pat->calendar != pan->calendar ) I[9]=1;
	if (strncmp(pat->xat,pan->xat,16) != 0) I[9]=I[10]=1;
	if (strncmp(pat->yat,pan->yat,16) != 0) I[9]=I[10]=1;
	if (strncmp(pat->mb,pan->mb,16) != 0) I[9]=I[10]=1;

/*		Check whether changes were made.  If not we're done.	*/

	for (i=0,k=0;i<11;i++) k+=I[i];
	if (k == 0)
	  {
	   ptb->next=ptab;
	   killGSp(gtab);
	   return 1;
	  }
	ptb->next=gtab;
	gtab->next=ptab->next;
	killGSp(ptab);

/*		If the table entry is being modified and it's being
		displayed, set for replacement of segments.		*/

	for(pd=&D_tab; pd!=NULL; pd=pd->next)
	 if (cmpncs("Scatter",pd->type)==0 && strcmp(pd->g_name,gtab->name)==0)
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
	if (!Inactive && fpout != NULL) prtGSp(fpout,gtab);
	return 1;
      }

/*	Print the X(t) vs Y(t) graph attributes that are defined.	*/

    int prtGSp(FILE *fp,struct gSp_tab *ptab)

      {
	int i;
	int k=0;
	char *pc;
	struct gSp_attr *pgi;

	if (ptab == NULL) return 0;
	if ((pgi=ptab->pGSp_attr) != NULL)
	    {
	     pc=pgi->proj;
	     fprintf (fp,"GSp_%s(\n   ",ptab->name);
	     k=0;
	     for (i=0;i < 9;i++,pc+=256)
	       {
		if (*pc != '\0')
			k+=fprintf (fp,"%s=%s,",GSp_attr[i],pc);
		if (k > 60)
		  {
		   fprintf (fp,"\n   ");
		   k=3;
		  }
		}
	     fprintf (fp,"\n   %s(%g,%g,%g,%g),",
		GSp_attr[9],pgi->dsp[0],pgi->dsp[1],pgi->dsp[2],pgi->dsp[3]);
	     fprintf (fp,"\n   %s(%d,%d,%d,%d),",
		GSp_attr[15],pgi->idsp[0],pgi->idsp[1],pgi->idsp[2],pgi->idsp[3]);

	     if (pgi->xat[0] != '\0')
                fprintf (fp,"\n   %s=%s,",GSp_attr[10],pgi->xat);
	     if (pgi->yat[0] != '\0')
                fprintf (fp,"\n   %s=%s,",GSp_attr[11],pgi->yat);
	     if (pgi->timeunits[0] != '\0')
                fprintf (fp,"\n   %s=%s,",GSp_attr[13],pgi->timeunits);
             fprintf (fp,"\n   %s=%d,",GSp_attr[14],pgi->calendar);

	     fprintf (fp,
			"\n   %s=%s)\n",
			GSp_attr[12],pgi->mb);
	    }
       return 1;
      }
