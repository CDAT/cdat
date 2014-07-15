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

    extern struct gfm_tab Gfm_tab;
    extern struct display_tab D_tab;

/*			Graphics fill meshfill names.			*/

    extern char Gfm_attr[20][16];

/*			Graphics fill meshfill descriptor names	*/

    extern char Gfm_iso[4][16];

    extern int Inactive;

/*	Process a graphics meshfill assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procGfm_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,k,c,m;
	int I[4];
	int tokm,tokf,str_ct;
	long int hold_pos;
	char strm[STRMAX+1];
	char strf[STRMAX+1];
	char *pc;
	float v;
	int iv;
	struct gfm_tab *ptab;
	struct gfm_attr *pat;
	struct fill_range *pifr,**pfr;
	struct fill_range liso;
/*	struct fill_range liso={0,0.0,5.0,"default",NULL};	*/

	liso.id=0;
	liso.lev1=0.0;
	liso.lev2=5.0;
	strcpy(liso.fill_name,"default");
	liso.next=NULL;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }

/*		Set up a table entry for meshfills.			*/

	if ((ptab=(struct gfm_tab *)malloc(sizeof(Gfm_tab))) == NULL)
	  {
	   err_warn(1,fperr,"Error - memory for %s not found \n",str);
	   return 0;
	  }
	strncpy (ptab->name,&str[4],17); ptab->name[16]='\0';
	ptab->next=NULL;
	ptab->pGfm_attr=NULL;

/*			Set up the attribute structure for meshfills.	*/

	if ( (pat=ptab->pGfm_attr=
	         (struct gfm_attr *) malloc(sizeof(struct gfm_attr))) == NULL)
	  {
	   err_warn(1,fperr,"Error memory for %s not found \n",str);
	   killGfm(ptab);
	   return 0;
          }

/*			 nullify the set of attributes			*/

	   for (pc=&(pat->proj[0]),i=0; i < 9; *pc='\0',pc+=256,i++);
	   for (i=0; i < 4; pat->dsp[i++]=0.0);
	   for (i=0; i < 4; pat->idsp[i++]=0);
           pat->xat[0]='\0';
           pat->yat[0]='\0';
           pat->missing=241;
           pat->legend=NULL;
           pat->mesh=0;
	   pat->xwrap=0.;
	   pat->ywrap=0.;
	   pat->line=NULL;
	   strcpy(pat->timeunits,VCS_DEFAULT_TIME_UNITS);
	   pat->calendar=VCS_DEFAULT_CALENDAR;

/*			Read in the attribute member assignments.	*/

	while ( ((c=getsttk(strm,&tokm)) || c == 0) && tokm != ')')
	  {
	   if (c == EOF || tokm == EOF)
	     {
	      err_warn(1,fperr,
		"Error - EOF in graphic meshfill attributes for (%s%c).\n",
						str,tok);
	      killGfm(ptab);
	      return 0;
	     }
	   if (tokm != '(' && tokm != '=')
	     {
	      err_warn(1,fperr,
		"Error - not a proper token (%s%c%s %c).\n",
				str,*tok,strm,tokm);
	      killGfm(ptab);
	      return 0;
	     }
	   if (c == 0)
	     {
	      err_warn(1,fperr,
		"Error - not a graphic meshfill attribute name (%s%c%s%c).\n",
		str,*tok,strm,tokm);
	      killGfm(ptab);
	      return 0;
	     }

/*			Find which attribute is being assigned.		*/

	   for (pc=&(pat->proj[0]), i=0;
		 i < 20 && cmpnbl(strm,Gfm_attr[i]) != 0;
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
		 killGfm(ptab);
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
		 killGfm(ptab);
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
		    killGfm(ptab);
		    return 0;
		   }
		 for (k=0; k < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; k++)
				strf[k]=c;
		 if (c == EOF) {killGfm(ptab); return EOF;}
		 strf[k]='\0';
		 if (isnum(strf)) sscanf (strf,"%f",&(pat->dsp[j++]));
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    killGfm(ptab);
		    return 0;
		   }
		}
	      if (j != 4)
		{
		 err_warn(1,fperr,
			"Error - too few numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		 killGfm(ptab);
		 return 0; 
		}
	      while (!istoken(tokm=getp(fpin,fpout)))
		if (tokm == EOF)
		  {
		   killGfm(ptab);
		   return EOF;
		  }
/*	      if (tokm == ')') ungetp(tokm,fpin);			*/
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

/*		Store the meshfill range definitions.			*/
	   else if (i == 12)
	     {

/*  Accept either name=(n=value,...)(...) or name(n=value,...)(...).	*/

	      if (tokm == '=')
		while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF);
	      if (tokm != '(')
	        {
	         err_warn(1,fperr,
		   "Error - not a proper token (%s%c%s %c).\n",
						str,*tok,strm,tokm);
		 killGfm(ptab);
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
			  killGfm(ptab);
			  return 0;
			 }
		       if (tokf != ')' && tokf != ',')
			 {
			  err_warn(1,fperr,
				"Error - syntax error (%s%c%s%c%c).\n",
			 		str,*tok,strm,tokm,c);
			  killGfm(ptab);
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
			  killGfm(ptab);
			  return 0;
			 }

/*			Search for the name.				*/

		       for (k=0; k < 4 && cmpnbl(Gfm_iso[k],strf)!=0; k++);
		       if (k == 4)
			 {
			  err_warn(1,fperr,
			  "Error - isoline element not found (%s%c%s%c%s%c).\n",
				str,*tok,strm,tokm,strf,tokf);
			  killGfm(ptab);
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
					str,*tok,strm,tokm,Gfm_iso[k],strf);
			     killGfm(ptab);
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
		       killGfm(ptab);
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
		    err_warn(1,fperr,"Error (procGfm) - EOF found (%s%c).\n",
			str,*tok);
		    killGfm(ptab);
		    return EOF;
		   }
		 if (tokm != '(' && tokm != ')') ungetp(tokm,fpin);

		}   /*      end of while (tokm == '(')		*/
	     }

	   else if (i == 16) {
              while ((c=getc(fpin)) && (c !='(')); /* move the point to '(' */
              hold_pos=ftell(fpin); /* get current position */
              str_ct=0; /* count the number of legend characters */
              while ((c=getc(fpin)) && (c !=')'))
                 ++str_ct;
              str_ct+=1; /* malloc the string size */
              if ((pat->legend=(char *)malloc(str_ct)) == NULL) {
                   err_warn(1,fperr,
                   "Error - memory for legend(%s) changes not found. \n",
                   strf);
                   return 0;
              }
	      fseek(fpin, hold_pos, SEEK_SET);
	      m = 0; /* copy the legend string into legend space */
	      while ((c=getp(fpin)) && (c !=')')) {
	         pat->legend[m]= c;
	         ++m;
	      }
	      pat->legend[m] = '\0';
	      tokm=c=getp(fpin,fpout); /* get the next token */
	   }

/*		Store the missing background color.			*/
	   else if (i == 13)
             {
              for (j=0;j<STRMAX && (tokm=getp(fpin,fpout))!=',' && tokm!=')';
                                        strf[j++]=tokm);
              strf[j]='\0';
              if (isnum(strf))
                {
                 sscanf(strf,"%i",&iv);
                 pat->missing=iv;
                }
             }
	   else if (i == 15)
             {
              for (j=0;j<STRMAX && (tokm=getp(fpin,fpout))!=',' && tokm!=')';
                                        strf[j++]=tokm);
              strf[j]='\0';
              if (isnum(strf))
                {
                 sscanf(strf,"%i",&iv);
                 pat->mesh=iv;
                }
             }
/*		Assign the real coordinates for the display space.	*/

	   else if (i == 14)
	     {
	      if (tokm == '=')
		while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF);
	      if (tokm != '(')
	        {
	         err_warn(1,fperr,
		   "Error - not a proper token (%s%c%s %c).\n",
						str,*tok,strm,tokm);
		 killGfm(ptab);
	         return 0;
	        }
	      c=j=0;
	      while (c != ')')
		{
		 if (j == 2)
		   {
		    err_warn(1,fperr,
			"Error - too many numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		    killGfm(ptab);
		    return 0;
		   }
		 for (k=0; k < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; k++)
				strf[k]=c;
		 if (c == EOF) {killGfm(ptab); return EOF;}
		 strf[k]='\0';
		 if (isnum(strf)) 
		   {
		     if (j==0) sscanf (strf,"%f",&(pat->ywrap));
		     else if (j==1) sscanf (strf,"%f",&(pat->xwrap));
		     j++;
		   }
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    killGfm(ptab);
		    return 0;
		   }
		}
	      if (j != 2)
		{
		 err_warn(1,fperr,
			"Error - too few numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		 killGfm(ptab);
		 return 0; 
		}
	      while (!istoken(tokm=getp(fpin,fpout)))
		if (tokm == EOF)
		  {
		   killGfm(ptab);
		   return EOF;
		  }
	     }
	   else if (i == 17)
             {
              for (j=0;j<STRMAX && (tokm=getp(fpin,fpout))!=',' && tokm!=')';
                                        strf[j++]=tokm);
              strf[j]='\0';
	      strcpy(pat->timeunits,strf);

             }
	   else if (i == 18)
             {
              for (j=0;j<STRMAX && (tokm=getp(fpin,fpout))!=',' && tokm!=')';
                                        strf[j++]=tokm);
              strf[j]='\0';
              if (isnum(strf))
                {
                 sscanf(strf,"%f",&v);
                 pat->calendar=v;
                }
             }
	   else if (i == 19)
	     {
	      if (tokm == '=')
		while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF);
	      if (tokm != '(')
	        {
	         err_warn(1,fperr,
		   "Error - not a proper token (%s%c%s %c).\n",
						str,*tok,strm,tokm);
		 killGfm(ptab);
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
		    killGfm(ptab);
		    return 0;
		   }
		 for (k=0; k < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; k++)
				strf[k]=c;
		 if (c == EOF) {killGfm(ptab); return EOF;}
		 strf[k]='\0';
		 if (isnum(strf)) sscanf (strf,"%d",&(pat->idsp[j++]));
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    killGfm(ptab);
		    return 0;
		   }
		}
	      if (j != 4)
		{
		 err_warn(1,fperr,
			"Error - too few numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		 killGfm(ptab);
		 return 0; 
		}
	      while (!istoken(tokm=getp(fpin,fpout)))
		if (tokm == EOF)
		  {
		   killGfm(ptab);
		   return EOF;
		  }
/*	      if (tokm == ')') ungetp(tokm,fpin);			*/
	     }

/*              Store the x-axis type.                 */
	   else

/*		i > 12, didn't find a name.				*/

	     {
	      err_warn(1,fperr,"Error - didn't find the name (%s%c%s%c).\n",
				str,*tok,strm,tokm);
	      killGfm(ptab);
	      return 0;
	     }
	   if (tokm == ')') break;
	  }
	pat=ptab->pGfm_attr;
	pifr=pat->line;
	if (pifr == NULL)
	  {
	   if((pat->line=pifr=
		(struct fill_range *)malloc(sizeof(struct fill_range))) == NULL)
	     {
	      err_warn(1,fperr,
				"Error - memory for (%s%c%s%c) not found.\n",
					str,*tok,strm,tokm);
	      killGfm(ptab);
	      return 0;
	     }
	   pifr->id=0;
	   pifr->lev1=1.e20;
	   pifr->lev2=1.e20;
	   strcpy(pifr->fill_name,"default");
	   pifr->next=NULL;
	  }
	c=chk_mov_Gfm (ptab);
	return c;
      }



/*			Check the meshfill table entry and move
			to the table if it's ok.			*/

    int chk_mov_Gfm (struct gfm_tab *gtab)

      {
	int i,k;
	struct fill_range *pfro,*pfrn,*pfri,*pfrk;
	struct gfm_attr *pat,*pan;
	struct gfm_tab *ptab,*ptb;
	struct display_tab *pd;
	int I[17];

	for(ptb=ptab=&Gfm_tab;
		ptab != NULL && strcmp(ptab->name,gtab->name) != 0;
						ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

         if (ptab == &Gfm_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (Gfm_%s).\n",
				gtab->name);
	   killGfm(gtab);
	   return 0;
	  }


/*	If it isn't modifying an isoline table entry then add it.	*/

	if (ptab == NULL)
	  {
	   ptb->next=gtab;
	   gtab->next=NULL;
	   return 1;
	  }

	for (i=0;i<16;i++) I[i]=0;

/*		Check for changes and move modified parameters.		*/

	pat=gtab->pGfm_attr;
	pan=ptab->pGfm_attr;

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
	if (pat->missing != pan->missing) I[11]=1;
	if (pat->xwrap != pan->xwrap) I[12]=1;
	if (pat->ywrap != pan->ywrap) I[13]=1;
	if (pat->mesh != pan->mesh) I[14]=1;
	if ((pat->legend != NULL) && (pan->legend == NULL))
	           I[16]=1;
        else if ((pat->legend == NULL) && (pan->legend != NULL))
	           I[16]=1;
        else if ((pat->legend != NULL) && (pan->legend != NULL) &&
                 (strcmp(pat->legend,pan->legend) != 0))
	           I[16]=1;

	pfrn=pan->line;
	pfro=pat->line;
	if (pfrn == NULL && pfro!=NULL) I[15]=1;
	if (pfrn != NULL && pfro==NULL)
	  {
	   I[14]=1;
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
	       strncmp(pfro->fill_name, pfrn->fill_name,16)  != 0) I[15]=1;
	   if ((pfrn->next == NULL && pfro->next != NULL) ||
	       (pfrn->next != NULL && pfro->next == NULL) ) I[15]=1;
	   pfro=pfro->next;
	   pfrn=pfrn->next;
	  }

/*		Check whether changes were made.  If not we're done.	*/

	for (i=0,k=0;i<17;i++) 
	  {
	    k+=I[i];
	  }
	if (k == 0)
	  {
	   ptb->next=ptab;
	   killGfm(gtab);
	   return 1;
	  }
	ptb->next=gtab;
	gtab->next=ptab->next;
	killGfm(ptab);

/*		If the table entry is being modified and it's being
		displayed, set for replacement of segments.		*/

	for(pd=&D_tab; pd!=NULL; pd=pd->next)
	  {
	   if ( cmpncs("meshfill",pd->type)==0 &&
		strcmp(pd->g_name,gtab->name)==0)
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
	      if (I[9] || I[10] || I[11] || I[12] || I[13] || I[14] || I[16])
	        {
	         pd->dsp_seg[3]=TRUE;
	         pd->leg_seg[3]=TRUE;
	         update_ind=TRUE;
	        }
	     }
	  }
	if (!Inactive && fpout != NULL) prtGfm(fpout,gtab);
	return 1;
      }

/*		Print the meshfill graph attributes that are defined.	*/

    int prtGfm(FILE *fp,struct gfm_tab *ptab)

      {
	int i;
	int k=0;
	char *pc;
	struct gfm_attr *pgi;
	struct fill_range *pfro;

	if (ptab == NULL) return 0;
	if ((pgi=ptab->pGfm_attr) != NULL)
	    {
	     pc=pgi->proj;
	     fprintf (fp,"Gfm_%s(\n   ",ptab->name);
	     k=0;
	     for (i=0;i < 9;i++,pc+=256)
	       {
		if (*pc != '\0')
			k+=fprintf (fp,"%s=%s,",Gfm_attr[i],pc);
		if (k > 60)
		  {
		   fprintf (fp,"\n   ");
		   k=3;
		  }
		}
	     fprintf (fp,"\n   %s(%g,%g,%g,%g),",
		Gfm_attr[9],pgi->dsp[0],pgi->dsp[1],pgi->dsp[2],pgi->dsp[3]);
	     fprintf (fp,"\n   %s(%d,%d,%d,%d),",
		Gfm_attr[19],pgi->idsp[0],pgi->idsp[1],pgi->idsp[2],pgi->idsp[3]);

	     if (pgi->xat[0] != '\0')
                fprintf (fp,"\n   %s=%s,",Gfm_attr[10],pgi->xat);
	     if (pgi->yat[0] != '\0')
                fprintf (fp,"\n   %s=%s,",Gfm_attr[11],pgi->yat);

             fprintf (fp,"\n   %s=%i,",Gfm_attr[13],pgi->missing);
             fprintf (fp,"\n   %s=%i,",Gfm_attr[15],pgi->mesh);
	     if (pgi->timeunits[0] != '\0')
                fprintf (fp,"\n   %s=%s,",Gfm_attr[17],pgi->timeunits);
             fprintf (fp,"\n   %s=%d,",Gfm_attr[18],pgi->calendar);
             fprintf (fp,"\n   %s\n   (%g, %g),",Gfm_attr[14],pgi->ywrap,pgi->xwrap);

	     if (pgi->legend == NULL) {
                if ((pgi->legend=(char *)malloc(2*sizeof(char)+1)) == NULL) {
                   err_warn(1,fperr,
                      "Error - memory for legend string not found. \n");
                   return 0;
                }
                pgi->legend[0] = '\0';
	     }
             fprintf (fp,"\n   %s=(%s),",Gfm_attr[16],pgi->legend);

	     if ((pfro=pgi->line) != NULL)
	       {
		fprintf (fp,"\n   %s",Gfm_attr[12]);

	        for (;pfro != NULL; pfro=pfro->next)
	          {
		   fprintf (fp,
			"\n   (%s=%d,%s=%g,%s=%g,%s=%s)",
			Gfm_iso[0],pfro->id, Gfm_iso[1],pfro->lev1,
			Gfm_iso[2],pfro->lev2, Gfm_iso[3],pfro->fill_name);
	          }
	       }
	     fprintf (fp,"  )\n");
	    }
       return 1;
      }



