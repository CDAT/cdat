#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "list.h"
#include "display.h"
#include "array.h"
#include "graph.h"

#define STRMAX 256
#define TRUE 1
#define FALSE 0

    extern FILE *fpin,*fpout,*fperr;

    extern int update_ind;

    extern struct gfb_tab Gfb_tab;
    extern struct display_tab D_tab;

/*			Graphics fill boxfill names.			*/

    extern char Gfb_attr[25][16];

/*                      Graphics fill isoline descriptor names  */

        extern char Gfi_iso[4][16];
	

    extern int Inactive;

/*	Process a graphics boxfill assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procGfb_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,k,m,c;
	int tokm,tokf,str_ct;
	int I[4];
	long int hold_pos;
	char strm[STRMAX+1];
	char strf[STRMAX+1];
	char *pc;
	float v;
	struct gfb_tab *ptab;
	struct gfb_attr *pat;
	struct fill_range *pifr,**pfr;
        struct fill_range liso;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }

/*		Set up a table entry for boxfill.			*/

	if ((ptab=(struct gfb_tab *)malloc(sizeof(Gfb_tab))) == NULL)
	  {
	   err_warn(1,fperr,"Error - memory for %s not found \n",str);
	   return 0;
	  }
	strncpy (ptab->name,&str[4],17); ptab->name[16]='\0';
	ptab->next=NULL;
	ptab->pGfb_attr=NULL;

/*			Set up the attribute structure for boxfill.	*/

	if ( (pat=ptab->pGfb_attr=
	         (struct gfb_attr *) malloc(sizeof(struct gfb_attr))) == NULL)
	  {
	   err_warn(1,fperr,"Error memory for %s not found \n",str);
	   killGfb(ptab);
	   return 0;
          };

/*			 nullify the set of attributes			*/

	for (pc=&(pat->proj[0]),i=0; i < 9; *pc='\0',pc+=256,i++);
	for (i=0; i < 4; pat->dsp[i++]=0.0);
	for (i=0; i < 4; pat->idsp[i++]=0);

	pat->xat[0]='\0';
        pat->yat[0]='\0';
	pat->lev1=1.e20;
	pat->lev2=1.e20;
	pat->color_1=16;
	pat->color_2=239;
	pat->boxfill_type=0;
	pat->legend=NULL;
	pat->ext_1='n';
	pat->ext_2='n';
	pat->missing=241;
	pat->line=NULL;
	strcpy(pat->timeunits,VCS_DEFAULT_TIME_UNITS);
	pat->calendar=VCS_DEFAULT_CALENDAR;

/*			Read in the attribute member assignments.	*/

	while ( ((c=getsttk(strm,&tokm)) || c == 0) && tokm != ')')
	  {
	   if (c == EOF || tokm == EOF)
	     {
	      err_warn(1,fperr,
		"Error - EOF in graphic boxfill attributes for (%s%c).\n",
						str,tok);
	      killGfb(ptab);
	      return 0;
	     }
	   if (tokm != '(' && tokm != '=')
	     {
	      err_warn(1,fperr,
		"Error - not a proper token (%s%c%s %c).\n",
				str,*tok,strm,tokm);
	      killGfb(ptab);
	      return 0;
	     }
	   if (c == 0)
	     {
	      err_warn(1,fperr,
		"Error - not a graphic boxfill attribute name (%s%c%s%c).\n",
		str,*tok,strm,tokm);
	      killGfb(ptab);
	      return 0;
	     }

/*			Find which attribute is being assigned.		*/

	   for (pc=&(pat->proj[0]), i=0;
		 i < 24 && cmpnbl(strm,Gfb_attr[i]) != 0; i++)
	     {
	      if (i < 9) pc+=256;
	     }
/*			For backward compatibiity, if strm = 'legend_type, then
 *			change it to boxfill_type and set i to 16.			*/
	     if (cmpnbl(strm,"legend_type") == 0)  i = 16;

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
		 killGfb(ptab);
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
		 killGfb(ptab);
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
		    killGfb(ptab);
		    return 0;
		   }
		 for (k=0; k < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; k++)
				strf[k]=c;
		 if (c == EOF) {killGfb(ptab); return EOF;}
		 strf[k]='\0';
		 if (isnum(strf)) sscanf (strf,"%f",&(pat->dsp[j++]));
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    killGfb(ptab);
		    return 0;
		   }
		}
	      if (j != 4)
		{
		 err_warn(1,fperr,
			"Error - too few numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		 killGfb(ptab);
		 return 0; 
		}
	      while (!istoken(tokm=getp(fpin,fpout)))
		if (tokm == EOF)
		  {
		   killGfb(ptab);
		   return EOF;
		  }
/*	      if (tokm == ')') ungetp(tokm,fpin);			*/
	     }
	   else if (i == 23)
	     {
	      if (tokm == '=')
		while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF);
	      if (tokm != '(')
	        {
	         err_warn(1,fperr,
		   "Error - not a proper token (%s%c%s %c).\n",
						str,*tok,strm,tokm);
		 killGfb(ptab);
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
		    killGfb(ptab);
		    return 0;
		   }
		 for (k=0; k < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; k++)
				strf[k]=c;
		 if (c == EOF) {killGfb(ptab); return EOF;}
		 strf[k]='\0';
		 if (isnum(strf)) sscanf (strf,"%i",&(pat->idsp[j++]));
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    killGfb(ptab);
		    return 0;
		   }
		}
	      if (j != 4)
		{
		 err_warn(1,fperr,
			"Error - too few numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		 killGfb(ptab);
		 return 0; 
		}
	      while (!istoken(tokm=getp(fpin,fpout)))
		if (tokm == EOF)
		  {
		   killGfb(ptab);
		   return EOF;
		  }
/*	      if (tokm == ')') ungetp(tokm,fpin);			*/
	     }
/*		   Store the isofill range definitions.			*/
	     else if (i == 24)
	       {

/*  Accept either name=(n=value,...)(...) or name(n=value,...)(...).	*/

	            if (tokm == '=')
		      while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF);
	            if (tokm != '(')
	              {
	               err_warn(1,fperr,
		         "Error - not a proper token (%s%c%s %c).\n",
						      str,*tok,strm,tokm);
		       killGfb(ptab);
	               return 0;
	              }
      
	            while (tokm == '(')
		      {
	               for (j=0;j<4;j++) I[j]=0;
      
	               liso.id=0;
	               liso.lev1=0.0;
	               liso.lev2=5.0;
	               strcpy(liso.fill_name,"default");
		       do		 
		         {
		          while ((c=getsttk(strf,&tokf))==0||c==EOF)
		            {
		             if (tokf == EOF || c == EOF)
			       {
			        err_warn(1,fperr,
				      "Error - EOF while reading (%s%c%s%c).\n",
				      str,*tok,strm,tokm);
			        killGfb(ptab);
			        return 0;
			       }
		             if (tokf != ')' && tokf != ',')
			       {
			        err_warn(1,fperr,
				      "Error - syntax error (%s%c%s%c%c).\n",
			 		      str,*tok,strm,tokm,c);
			        killGfb(ptab);
			        return 0;
			       }
		             else if (tokf == ')')
			       {
			        break;
			       }		       
		            }
		          if (c != 0)
		            {
		             if (tokf != '=')
			       {
			        err_warn(1,fperr,
				      "Error - syntax error (%s%c%s%c%s%c).\n",
				      str,*tok,strm,tokm,strf,tokf);
			        killGfb(ptab);
			        return 0;
			       }
      
      /*			Search for the name.				*/
      
		             for (k=0; k < 4 && cmpnbl(Gfi_iso[k],strf)!=0; k++);
		             if (k == 4)
			       {
			        err_warn(1,fperr,
			        "Error - isoline element not found (%s%c%s%c%s%c).\n",
				      str,*tok,strm,tokm,strf,tokf);
			        killGfb(ptab);
			        return 0;
			       }
		             for (j=0;
			       j<STRMAX && (tokf=getp(fpin,fpout))!=',' && tokf!=')';
					      strf[j++]=tokf);
		             strf[j]='\0';
		             if (k < 3)
			       {
			        if (isnum(strf))
			          {
			           sscanf(strf,"%f",&v);
			           if (k == 0) liso.id=v;
			           else if (k == 1) liso.lev1=v;
			           else if (k == 2) liso.lev2=v;
			          }
			        else
			          {
			           err_warn(1,fperr,
				      "Error - should be number (%s%c%s%c%s=%s).\n",
					      str,*tok,strm,tokm,Gfi_iso[k],strf);
			           killGfb(ptab);
			           return 0;
			          }
			       }
		             else if (k == 3)
			       {
			        strncpy(liso.fill_name,strf,17); liso.fill_name[16]='\0';
			       }
		             I[k]+=1;
		            }
		         } while (tokf == ',');
      
		       pifr=pat->line;
		       pfr=&pat->line;
      
		       while (pifr != NULL)
		         {
		          if (pifr->id == liso.id) break;
		          pfr=&pifr->next;
		          pifr=pifr->next;
		         }
		       if (pifr == NULL)
		         {
		          if ((pifr=(struct fill_range *)
				      malloc(sizeof(struct fill_range))) == NULL)
		            {
		             err_warn(1,fperr,
				      "Error - memory for (%s%c%s%c) not found.\n",
					      str,*tok,strm,tokm);
		             killGfb(ptab);
		             return 0;
		            }
		          if (pat->line == NULL) pat->line=pifr;
		          else *pfr=pifr;
		          pifr->id=liso.id;
		          pifr->lev1=liso.lev1;
		          pifr->lev2=liso.lev2;
		          strncpy(pifr->fill_name,liso.fill_name,17); pifr->fill_name[16]='\0';
		          pifr->next=NULL;
		         }
		       else
		         {
		          if (I[0]>0) pifr->id=liso.id;
		          if (I[1]>0) pifr->lev1=liso.lev1;
		          if (I[2]>0) pifr->lev2=liso.lev2;
		          if (I[3]>0) {strncpy(pifr->fill_name,liso.fill_name,17); pifr->fill_name[16]='\0';}
		         }
      
		       while ((tokm=getp(fpin,fpout)) == ' ' );
		       if (tokm == EOF)
		         {
		          err_warn(1,fperr,"Error (procGfi) - EOF found (%s%c).\n",
			      str,*tok);
		          killGfb(ptab);
		          return EOF;
		         }
		       if (tokm != '(' && tokm != ')') ungetp(tokm,fpin);
      
		      }   /*      end of while (tokm == '(')		*/
	       }

/*		Store the boxfill range definitions.		*/

	   else if (i >= 10 && i < 24)
	     {

	      for (j=0;j<STRMAX && (tokm=getp(fpin,fpout))!=',' && tokm!=')';
					strf[j++]=tokm);
	      strf[j]='\0';
	      if (isnum(strf))
		{
		 sscanf(strf,"%f",&v);
		 if (i == 12) pat->lev1=v;
		 else if (i == 13) pat->lev2=v;
		 else if (i == 14) pat->color_1=v;
		 else if (i == 15) pat->color_2=v;
		 else if (i == 16) pat->boxfill_type=(int)v;
		 else if (i == 20) pat->missing=v;
		 else if (i == 22) pat->calendar=(int)v;
		}
	      else
		{
		 if (i == 10) strcpy(pat->xat,strf);
		 else if (i == 11) strcpy(pat->yat,strf);
		 else if (i == 21) strcpy(pat->timeunits,strf);
		 else if (i == 17) {
                    hold_pos=ftell(fpin); /* back up the file pointer to '(' */
                    fseek(fpin, (hold_pos-(long int)strlen(strf)), SEEK_SET);
                    str_ct=0; /* count the number of legend characters */
                    while ((c=getc(fpin)) && (c !=')'))
                       ++str_ct;
		    if (str_ct > 0) {
                       str_ct+=1; /* malloc the string size */
                       if ((pat->legend=(char *)malloc(str_ct)) == NULL) {
                            err_warn(1,fperr,
                            "Error - memory for legend(%s) changes not found. \n",
                            strf);
                            return 0;
                       }
                       fseek(fpin, (hold_pos-(long int)strlen(strf)), SEEK_SET);
                       m = 0; /* copy the legend string into legend space */
                       while ((c=getp(fpin)) && (c !=')')) {
		          pat->legend[m]= c;
                          ++m;
                       }
		       pat->legend[m] = '\0';
		    }
                    tokm=c=getp(fpin,fpout); /* get the next token */
		 } else if (i == 18) pat->ext_1=strf[0];
		 else if (i == 19) pat->ext_2=strf[0];
		} /* stop if checks */
	     }
	   else

/*		didn't find a name.					*/

	     {
	      err_warn(1,fperr,"Error - didn't find the name (%s%c%s%c).\n",
				str,*tok,strm,tokm);
	      killGfb(ptab);
	      return 0;
	     }
	   if (tokm == ')') break;
	  }
	pat=ptab->pGfb_attr;
	pifr=pat->line;
	if (pifr == NULL)
	  {
	   if((pat->line=pifr=
		(struct fill_range *)malloc(sizeof(struct fill_range))) == NULL)
	     {
	      err_warn(1,fperr,
				"Error - memory for (%s%c%s%c) not found.\n",
					str,*tok,strm,tokm);
	      killGfb(ptab);
	      return 0;
	     }
	   pifr->id=0;
	   pifr->lev1=1.e20;
	   pifr->lev2=1.e20;
	   strcpy(pifr->fill_name,"default");
	   pifr->next=NULL;
	  }
	c=chk_mov_Gfb (ptab);
	return c;
      }



/*			Check the boxfill table entry and move
			to the table if it's ok.			*/

    int chk_mov_Gfb (struct gfb_tab *gtab)

      {
	int i,k;
	struct fill_range *pfro,*pfrn,*pfri,*pfrk;
	struct gfb_attr *pat,*pan;
	struct gfb_tab *ptab,*ptb;
	struct display_tab *pd;
	int I[12];

	for(ptb=ptab=&Gfb_tab;
		ptab != NULL && strcmp(ptab->name,gtab->name) != 0;
						ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

         if (ptab == &Gfb_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (Gfb_%s).\n",
				gtab->name);
	   killGfb(gtab);
	   return 0;
	  }


/*	If it isn't modifying a boxfill table entry then add it.	*/

	if (ptab == NULL)
	  {
	   ptb->next=gtab;
	   gtab->next=NULL;
	   return 1;
	  }

	for (i=0;i<12;i++) I[i]=0;

/*		Check for changes and move modified parameters.		*/

	pat=gtab->pGfb_attr;
	pan=ptab->pGfb_attr;

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
	if (pat->idsp[0] != pan->idsp[0] || pat->idsp[1] != pan->idsp[1] ||
	    pat->idsp[2] != pan->idsp[2] || pat->idsp[3] != pan->idsp[3])
						 I[9]=1;
	if (strncmp(pat->timeunits,pan->timeunits,255) != 0) I[9]=1;
	if (pat->calendar != pan->calendar ) I[9]=1;
	if (strncmp(pat->xat,pan->xat,16) != 0) I[9]=I[10]=1;
        if (strncmp(pat->yat,pan->yat,16) != 0) I[9]=I[10]=1;
	if (pat->lev1 != pan->lev1 || pat->lev2 != pan->lev2 ||
	    pat->color_1 != pan->color_1 || pat->color_2 != pan->color_2)
							 I[10]=1;
	if (pat->boxfill_type != pan->boxfill_type) I[9]=1;
	if ((pat->legend != NULL) && (pan->legend == NULL))
           	I[11]=1;
	else if ((pat->legend == NULL) && (pan->legend != NULL))
           	I[11]=1;
	else if ((pat->legend != NULL) && (pan->legend != NULL) &&
	         (strcmp(pat->legend,pan->legend) != 0))
           	I[11]=1;
	if (pat->ext_1 != pan->ext_1 || pat->ext_2 != pan->ext_2) I[11]=1;
	if (pat->missing != pan->missing) I[10]=1;

	pfrn=pan->line;
	pfro=pat->line;
	if (pfrn == NULL && pfro!=NULL) I[10]=1;
	if (pfrn != NULL && pfro==NULL)
	  {
	   I[11]=1;
	   pfri=pfrn;
	   while (pfri!=NULL)
	     {
	      pfrk=pfri->next;
	      free((char *)pfri);
	      pfri=pfrk;
	     }
	   pfrn=pan->line=NULL;
	  }
	while (pfrn != NULL && pfro != NULL)
	  {
	   if (pfrn->id   != pfro->id   || pfrn->lev1 != pfro->lev1 ||
	       pfrn->lev2 != pfro->lev2 || 
	       strncmp(pfro->fill_name, pfrn->fill_name,16)  != 0) I[11]=1;
	   if ((pfrn->next == NULL && pfro->next != NULL) ||
	       (pfrn->next != NULL && pfro->next == NULL) ) I[11]=1;
	   pfro=pfro->next;
	   pfrn=pfrn->next;
	  }


/*		Check whether changes were made.  If not we're done.	*/

	for (i=0,k=0;i<12;i++) k+=I[i];
	if (k == 0)
	  {
	   ptb->next=ptab;
	   killGfb(gtab);
	   return 1;
	  }
	ptb->next=gtab;
	gtab->next=ptab->next;
	killGfb(ptab);

/*		If the table entry is being modified and it's being
		displayed, set for replacement of segments.		*/

	for(pd=&D_tab; pd!=NULL; pd=pd->next)
	 if (cmpncs("boxfill",pd->type)==0 &&strcmp(pd->g_name,gtab->name)==0)
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
	      pd->b1_seg[3]=TRUE;
	      pd->b2_seg[3]=TRUE;
	      pd->b3_seg[3]=TRUE;
	      pd->b4_seg[3]=TRUE;

	      pd->dsp_seg[3]=TRUE;
	      pd->leg_seg[3]=TRUE;
	      update_ind=TRUE;
	     }
           if (I[11])
             {
	      if (pat->boxfill_type != 2) { /* Don't do anything if type is log10 */
	         pd->b1_seg[3]=TRUE;
	         pd->b2_seg[3]=TRUE;
	         pd->b3_seg[3]=TRUE;
	         pd->b4_seg[3]=TRUE;

	         pd->leg_seg[3]=TRUE;
	         pd->dsp_seg[3]=TRUE;
	      } else
	         pd->dsp_seg[3]=TRUE;
	      update_ind=TRUE;
             }
	  }
	if (!Inactive && fpout != NULL) prtGfb(fpout,gtab);
	return 1;
      }

/*		Print the isofill graph attributes that are defined.	*/

    int prtGfb(FILE *fp,struct gfb_tab *ptab)

      {
	int i;
	int k=0;
	char *pc;
	struct gfb_attr *pgb;
	struct gfi_attr *pgi;
	struct fill_range *pfro;

	if (ptab == NULL) return 0;
	if ((pgb=ptab->pGfb_attr) != NULL)
	    {
	     pc=pgb->proj;
	     fprintf (fp,"Gfb_%s(\n   ",ptab->name);
	     k=0;
	     for (i=0;i < 9;i++,pc+=256)
	       {
		if (*pc != '\0')
			k+=fprintf (fp,"%s=%s,",Gfb_attr[i],pc);
		if (k > 60)
		  {
		   fprintf (fp,"\n   ");
		   k=3;
		  }
		}
	     fprintf (fp,"\n   %s(%g,%g,%g,%g),",
		Gfb_attr[9],pgb->dsp[0],pgb->dsp[1],pgb->dsp[2],pgb->dsp[3]);
	     fprintf (fp,"\n   %s(%d,%d,%d,%d),",
		Gfb_attr[23],pgb->idsp[0],pgb->idsp[1],pgb->idsp[2],pgb->idsp[3]);

	     if (pgb->xat[0] != '\0')
                fprintf (fp,"\n   %s=%s,",Gfb_attr[10],pgb->xat);
	     if (pgb->yat[0] != '\0')
                fprintf (fp,"\n   %s=%s,",Gfb_attr[11],pgb->yat);
	     if (pgb->timeunits[0] != '\0')
                fprintf (fp,"\n   %s=%s,",Gfb_attr[21],pgb->timeunits);

             fprintf ( fp,"\n   %s=%d,", Gfb_attr[16],pgb->boxfill_type);
	     if (pgb->legend == NULL) {
                if ((pgb->legend=(char *)malloc(2*sizeof(char)+1)) == NULL) {
                   err_warn(1,fperr,
                      "Error - memory for legend string not found. \n");
                   return 0;
                }
                pgb->legend[0] = '\0';
	     }

	     fprintf (fp,
			"\n   %s=%g,%s=%g,%s=%d,%s=%d,\n   %s=(%s),\n   %s=%c,%s=%c,%s=%d,%s=%d,",
			Gfb_attr[12],pgb->lev1, Gfb_attr[13],pgb->lev2,
			Gfb_attr[14],pgb->color_1, Gfb_attr[15],pgb->color_2,
                        Gfb_attr[17],pgb->legend,
			Gfb_attr[18],pgb->ext_1, Gfb_attr[19],pgb->ext_2,
			Gfb_attr[20],pgb->missing,
			Gfb_attr[22],pgb->calendar);

	     if ((pfro=pgb->line) != NULL)
	       {
		fprintf (fp,"\n   %s",Gfb_attr[24]);

	        for (;pfro != NULL; pfro=pfro->next)
	          {
		   fprintf (fp,
			"\n   (%s=%d,%s=%g,%s=%g,%s=%s)",
			Gfi_iso[0],pfro->id, Gfi_iso[1],pfro->lev1,
			Gfi_iso[2],pfro->lev2, Gfi_iso[3],pfro->fill_name);
	          }
	       }
	     fprintf (fp,"  )\n");
	    }
       return 1;
      }
