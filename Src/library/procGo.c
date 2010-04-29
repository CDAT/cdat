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

    extern struct go_tab Go_tab;
    extern struct display_tab D_tab;
 
    extern int update_ind;

/*			Graphics outline names				*/

    extern char Go_attr[17][16];

    extern int Inactive;


/*	Process a graphics outline assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procGo_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,k,c,a,n;
	int tokm,tokf;
	char strm[STRMAX+1];
	char strf[STRMAX+1];
	char *pc;
	struct go_tab *ptab;
	struct go_attr *pat;
	int iv;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }
/*		Set up a table entry for outlines.			*/

	if ((ptab=(struct go_tab *)malloc(sizeof(Go_tab))) == NULL)
	  {
	   err_warn(1,fperr,"Error - memory for %s not found \n",str);
	   return 0;
	  }
	strncpy (ptab->name,&str[3],17); ptab->name[16]='\0';
	ptab->next=NULL;
	ptab->pGo_attr=NULL;

/*			Set up the attribute structure for outlines.	*/

	if ( (pat=ptab->pGo_attr=
	           (struct go_attr *) malloc(sizeof(struct go_attr))) == NULL)
	  {
	   err_warn(1,fperr,"Error memory for %s not found \n",str);
	   killGo(ptab);
	   return 0;
          }

/*			 Nullify the set of attributes			*/

	for (pc=&(pat->proj[0]),i=0; i < 9;
			*pc='\0',pc+=256,i++);
	for (i=0; i < 4; pat->dsp[i++]=0.0);
	for (i=0; i < 4; pat->idsp[i++]=0);
	pat->xat[0]='\0';
	pat->yat[0]='\0';
	pat->lb[0]='\0';
	pat->n=0;
	strcpy(pat->timeunits,VCS_DEFAULT_TIME_UNITS);
	pat->calendar=VCS_DEFAULT_CALENDAR;

/*			Read in the attribute member assignments.	*/

	while ( ((c=getsttk(strm,&tokm)) || c == 0) && tokm != ')')
	  {
	   if (c == EOF || tokm == EOF)
	     {
	      err_warn(1,fperr,
		"Error - EOF in graphic outline attributes for (%s%c).\n",
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
	      killGo(ptab);
	      return 0;
	     }
	   if (c == 0)
	     {
	      err_warn(1,fperr,
		"Error - not a graphic outline attribute name (%s%c%s%c).\n",
		str,*tok,strm,tokm);
	      killGo(ptab);
	      return 0;
	     }

/*			Find which attribute is being assigned.		*/

	   for (pc=&(pat->proj[0]), i=0;
		 i < 17 && cmpnbl(strm,Go_attr[i]) != 0;
		 pc+=256,i++);

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
		 killGo(ptab);
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
		 killGo(ptab);
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
		    killGo(ptab);
		    return 0;
		   }
		 for (k=0; k < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; k++)
				strf[k]=c;
		 if (c == EOF) {killGo(ptab); return EOF;}
		 strf[k]='\0';
		 if (isnum(strf)) sscanf (strf,"%f",&(pat->dsp[j++]));
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
	  	    killGo(ptab);
		    return 0;
		   }
		}

	      while (!istoken(tokm=getp(fpin,fpout)))
			if (tokm == EOF) {killGo(ptab); return EOF;}
	     }

/*              Store the x-axis type.                 */
           else if (i == 10)
             {
              j=0;
              while (!istoken(tokm=c=getp(fpin,fpout)) )
                 if (j < 16) pat->xat[j++]=c;
              pat->xat[j]='\0';
             }

/*              Store the y-axis type.                 */
           else if (i == 11)
             {
              j=0;
              while (!istoken(tokm=c=getp(fpin,fpout)) )
                 if (j < 16) pat->yat[j++]=c;
              pat->yat[j]='\0';
             }

/*		Store line bundle name.			*/
	   else if (i == 12)
	     {
	      j=0;
	      while (!istoken(tokm=c=getp(fpin,fpout)) )
	         if (j < 16) pat->lb[j++]=c;
	      pat->lb[j]='\0';
	     }

/*		Store the outline definitions.				*/

	   else if (i == 13)
	     {
	      a=0;
	      if (tokm == '=')
		while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF)
		  if (tokm >= '0' && tokm <= '9') a=10*a+(tokm-'0');
	      if (a > 0)
		{
		 pat->out[0]=a;
		 pat->n=1;
		 ungetp(tokm,fpin);
		 continue;  /* NOTE!!!! skip out here.*/
		}
	      if (tokm != '(')
	        {
	         err_warn(1,fperr,
		   "Error - not a proper token (%s%c%s %c).\n",
						str,*tok,strm,tokm);
		  killGo(ptab);
	         return 0;
	        }
	      n=0;
	      while ((c=getsttk(strf,&tokf))>0 && (tokf==',' || tokf==')'))
		{
		 if (isnum(strf))
		   {
		    a=0;
		    for (k=0;strf[k]!='\0' && strf[k]>='0' && strf[k]<='9';k++)
			a=10*a+(strf[k]-'0');
		    if (strf[k] == '\0') {pat->out[n++]=a; pat->n=n;}
		    else
			err_warn(1,fperr,"Error - syntax (%s%c%s%c%s%c).\n",
				str,*tok,strm,tokm,strf,tokf);
		   }
		 if (tokf == ')') break;
		}
	      if (tokf == EOF || c == EOF)
		{
		 err_warn(1,fperr,
			"Error - EOF while reading (%s%c%s%c).\n",
				str,*tok,strm,tokm);
		 killGo(ptab);
		 return 0;
		}
	      if (tokf != ')')
		{
		 err_warn(1,fperr,"Error - syntax error (%s%c%s%c%c).\n",
			 		str,*tok,strm,tokm,c);
		 killGo(ptab);
		 return 0;
		}
	     }
	   else if (i == 14)
             {
              for (j=0;j<STRMAX && (tokm=getp(fpin,fpout))!=',' && tokm!=')';
                                        strf[j++]=tokm);
              strf[j]='\0';
	      strcpy(pat->timeunits,strf);

             }
	   else if (i == 15)
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

	   else if (i == 16)
	     {
	      if (tokm == '=')
		while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF);
	      if (tokm != '(')
	        {
	         err_warn(1,fperr,
		   "Error - not a proper token (%s%c%s %c).\n",
						str,*tok,strm,tokm);
		 killGo(ptab);
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
		    killGo(ptab);
		    return 0;
		   }
		 for (i=0; i < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; i++)
				strf[i]=c;
		 if (c == EOF) {killGo(ptab); return EOF;}
		 strf[i]='\0';
		 if (isnum(strf)) sscanf (strf,"%d",&(pat->idsp[j++]));
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    killGo(ptab);
		    return 0;
		   }
		}

	      while (!istoken(tokm=getp(fpin,fpout)))
			if (tokm == EOF) {killGo(ptab); return EOF;}
	     }
	   else

/*		i > 13, didn't find a name.				*/

	     {
	      err_warn(1,fperr,"Error - didn't find the name (%s%c%s%c).\n",
				str,*tok,strm,tokm);
	      killGo(ptab);
	      return 0;
	     }
	   if (tokm == ')') break;
	  }
	c=chk_mov_Go (ptab);
	return 1;
      }
/*			Check the outline table entry and move
			to the table if it's ok.			*/

    int chk_mov_Go (struct go_tab *gtab)

      {
	int i,k;
	struct go_attr *pat,*pan;
	struct go_tab *ptab,*ptb;
	struct display_tab *pd;
	int I[12];

	for(ptb=ptab=&Go_tab;
		ptab != NULL && strcmp(ptab->name,gtab->name) != 0;
						ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

         if (ptab == &Go_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (Go_%s).\n",
				gtab->name);
	   killGo(gtab);
	   return 0;
	  }


/*	If it isn't modifying an outline table entry then add it.	*/

	if (ptab == NULL)
	  {
	   ptb->next=gtab;
	   return 1;
	  }
	gtab->next=ptab->next;

	for (i=0;i<12;i++) I[i]=0;

/*		Check for changes and move modified parameters.		*/

	pat=gtab->pGo_attr;
	pan=ptab->pGo_attr;

	if (strncmp(pat->proj,pan->proj,255) != 0)
	  { strncpy(pan->proj,pat->proj,256); pan->proj[255]='\0'; I[0]=1; }
	if (strncmp(pat->xtl1,pan->xtl1,255) != 0)
	  { strncpy(pan->xtl1,pat->xtl1,256); pan->xtl1[255]='\0'; I[1]=1; }
	if (strncmp(pat->xtl2,pan->xtl2,255) != 0)
	  { strncpy(pan->xtl2,pat->xtl2,256); pan->xtl2[255]='\0'; I[2]=1; }
	if (strncmp(pat->xmt1,pan->xmt1,255) != 0)
	  { strncpy(pan->xmt1,pat->xmt1,256); pan->xmt1[255]='\0'; I[3]=1; }
	if (strncmp(pat->xmt2,pan->xmt2,255) != 0)
	  { strncpy(pan->xmt2,pat->xmt2,256); pan->xmt2[255]='\0'; I[4]=1; }
	if (strncmp(pat->ytl1,pan->ytl1,255) != 0)
	  { strncpy(pan->ytl1,pat->ytl1,256); pan->ytl1[255]='\0'; I[5]=1; }
	if (strncmp(pat->ytl2,pan->ytl2,255) != 0)
	  { strncpy(pan->ytl2,pat->ytl2,256); pan->ytl2[255]='\0'; I[6]=1; }
	if (strncmp(pat->ymt1,pan->ymt1,255) != 0)
	  { strncpy(pan->ymt1,pat->ymt1,256); pan->ymt1[255]='\0'; I[7]=1; }
	if (strncmp(pat->ymt2,pan->ymt2,255) != 0)
	  { strncpy(pan->ymt2,pat->ymt2,256); pan->ymt2[255]='\0'; I[8]=1; }
	if (pat->dsp[0] != pan->dsp[0]) {pan->dsp[0]=pat->dsp[0]; I[9]=1; }
	if (pat->dsp[1] != pan->dsp[1]) {pan->dsp[1]=pat->dsp[1]; I[9]=1; }
	if (pat->dsp[2] != pan->dsp[2]) {pan->dsp[2]=pat->dsp[2]; I[9]=1; }
	if (pat->dsp[3] != pan->dsp[3]) {pan->dsp[3]=pat->dsp[3]; I[9]=1; }
	if (pat->idsp[0] != pan->idsp[0]) I[9]=1;
	if (pat->idsp[1] != pan->idsp[1]) I[9]=1;
	if (pat->idsp[2] != pan->idsp[2]) I[9]=1;
	if (pat->idsp[3] != pan->idsp[3]) I[9]=1;
	if (strncmp(pat->timeunits,pan->timeunits,255) != 0) I[9]=1;
	if (pat->calendar != pan->calendar ) I[9]=1;
	if (strncmp(pat->xat,pan->xat,16) != 0)
	  { strncpy(pan->xat,pat->xat,17); pan->xat[16]='\0'; I[9]=I[10]=1; }
	if (strncmp(pat->yat,pan->yat,16) != 0)
	  { strncpy(pan->yat,pat->yat,17); pan->yat[16]='\0'; I[9]=I[10]=1; }
	if (strncmp(pat->lb,pan->lb,16) != 0)
	  { strncpy(pan->lb,pat->lb,17); pan->lb[16]='\0'; I[10]=1; }
	if (pat->n != pan->n) {pan->n=pat->n; I[10]=1; }
	else
	  for (i=0;i<pat->n;i++)
	     if (pat->out[i] != pan->out[i]) {pan->out[i]=pat->out[i]; I[10]=1;}


/*		Check whether changes were made.  If not we're done.	*/

	for (i=0,k=0;i<12;i++) k+=I[i];
	if (k == 0)
	  {
	   killGo(gtab);
	   return 1;
	  }
	ptb->next=gtab;
	killGo(ptab);

/*		If the table entry is being modified and it's being
		displayed, set for replacement of segments.		*/

	for(pd=&D_tab; pd!=NULL; pd=pd->next)
	 if (cmpncs("outline",pd->type)==0 && strcmp(pd->g_name,gtab->name)==0)
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
	   if (I[9] || I[10])
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
	      pd->leg_seg[3]=TRUE;
	     }
	      
	  }
	if (!Inactive && fpout != NULL) prtGo(fpout,gtab);
	return 1;
      }

/*		Print the outline graph attributes that are defined.	*/

    int prtGo(FILE *fp,struct go_tab *ptab)

      {
	int i,n;
	int k=0;
	char *pc;
	struct go_attr *pgo;

	if ((pgo=ptab->pGo_attr) != NULL)
	    {
	     pc=pgo->proj;
	     fprintf (fp,"Go_%s(\n   ",ptab->name);
	     k=0;
	     for (i=0;i < 9;i++,pc+=256)
	       {
		if (*pc != '\0')
			k+=fprintf (fp,"%s=%s,",Go_attr[i],pc);
		if (k > 60)
		  {
		   fprintf (fp,"\n   ");
		   k=3;
		  }
		}
	     fprintf (fp,"\n   %s(%g,%g,%g,%g),",
		Go_attr[9],pgo->dsp[0],pgo->dsp[1],pgo->dsp[2],pgo->dsp[3]);
	     if (pgo->xat[0] != '\0')
                fprintf (fp,"\n   %s=%s,",Go_attr[10],pgo->xat);
	     if (pgo->yat[0] != '\0')
                fprintf (fp,"\n   %s=%s,",Go_attr[11],pgo->yat);
	     fprintf (fp,"\n   %s=%s,",Go_attr[12],pgo->lb);

	     if (pgo->timeunits[0] != '\0')
                fprintf (fp,"\n   %s=%s,",Go_attr[14],pgo->timeunits);
             fprintf (fp,"\n   %s=%d",Go_attr[15],pgo->calendar);
	     if (pgo->n > 0)
	       {
	        fprintf (fp,",\n   %s(",Go_attr[13]);
	        for (n=0;n<pgo->n;n++) {
                   if (n < (pgo->n-1))
		      fprintf (fp,"%d, ",pgo->out[n]);
                   else
		      fprintf (fp,"%d",pgo->out[n]);
		}
	        fprintf (fp,")");
	       }
	     fprintf (fp,")\n");
	    }
       return 1;
      }
