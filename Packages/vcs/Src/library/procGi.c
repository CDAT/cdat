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

    extern struct gi_tab Gi_tab;
    extern struct display_tab D_tab;

/*			Graphics contour names				*/

    extern char Gi_attr[17][16];

/*			Graphics contour isoline descriptor names	*/

    extern char Gi_iso[9][16];

    extern int Inactive;

/*	Process a graphics contour assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procGi_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,k,c;
	char strm[STRMAX+1];
	char strf[STRMAX+1];
	char *pc;
	int tokm,tokf;
	float v;
	struct gi_tab *ptab,*gtab;
	struct gi_attr *pat,*gat;
	struct iso *piso,*pso,*giso,**gso;
	struct iso liso;
	int I[16];

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }

/*			Set up a table entry for isolines.		*/

	if ((ptab=(struct gi_tab *)malloc(sizeof(Gi_tab))) == NULL)
	  {
	   err_warn(1,fperr,"Error - memory for %s not found \n",str);
	   return 0;
	  }
	strncpy (ptab->name,&str[3],17); ptab->name[16]='\0';
	ptab->next=NULL;
	ptab->pGi_attr=NULL;

/*			Set up the attribute structure for isolines.	*/

	if ( (pat=ptab->pGi_attr=
	           (struct gi_attr *) malloc(sizeof(struct gi_attr))) == NULL)
	  {
	   err_warn(1,fperr,"Error memory for %s not found \n",str);
	   killGi(ptab);
	   return 0;
          };

/*			 Nullify the set of attributes			*/

	for (pc=&(pat->proj[0]),i=0; i < 9;*pc='\0',pc+=256,i++);
	for (i=0; i < 4; pat->dsp[i++]=0.0);
	for (i=0; i < 4; pat->idsp[i++]=0);
	pat->xat[0]='\0';
	pat->yat[0]='\0';
	pat->labels='n';
	pat->line=NULL;
	strcpy(pat->timeunits,VCS_DEFAULT_TIME_UNITS);
	pat->calendar=VCS_DEFAULT_CALENDAR;

/*			Move old attributes to the new table entry
			if it already exists.				*/
	for (gtab=&Gi_tab;gtab != NULL && strcmp(gtab->name,ptab->name) != 0;
				gtab=gtab->next);
	if (gtab != NULL)
	  {
	   ptab->next=gtab->next;
	   gat=gtab->pGi_attr;
	   strncpy(pat->proj,gat->proj,256); pat->proj[255] = '\0';
	   strncpy(pat->xtl1,gat->xtl1,256); pat->xtl1[255] = '\0';
	   strncpy(pat->xtl2,gat->xtl2,256); pat->xtl2[255] = '\0';
	   strncpy(pat->xmt1,gat->xmt1,256); pat->xmt1[255] = '\0';
	   strncpy(pat->xmt2,gat->xmt2,256); pat->xmt2[255] = '\0';
	   strncpy(pat->ytl1,gat->ytl1,256); pat->ytl1[255] = '\0';
	   strncpy(pat->ytl2,gat->ytl2,256); pat->ytl2[255] = '\0';
	   strncpy(pat->ymt1,gat->ymt1,256); pat->ymt1[255] = '\0';
	   strncpy(pat->ymt2,gat->ymt2,256); pat->ymt2[255] = '\0';
	   strncpy(pat->timeunits,gat->timeunits,256); pat->timeunits[255] = '\0';
	   for (i=0;i<4;i++) pat->dsp[i]=gat->dsp[i];
	   for (i=0;i<4;i++) pat->idsp[i]=gat->idsp[i];
	   strncpy(pat->xat,gat->xat,17); pat->xat[16] = '\0';
	   strncpy(pat->yat,gat->yat,17); pat->yat[16] = '\0';
	   pat->calendar=gat->calendar;
	   pat->labels=gat->labels;
	   pat->line=NULL;
	   gso=&(pat->line);
	   for (giso=gat->line; giso != NULL; giso=giso->next)
	     {
	      if ((piso=(struct iso *)malloc(sizeof(struct iso)))==NULL)
	        {
		 err_warn(1,fperr,
			"Error - memory for processing Graphics isolines (%s)"
				" not found./n",str);
		 killGi(ptab);
		 return 0;
	        }
	      piso->id=giso->id;
	      piso->p=giso->p;
	      piso->lev=giso->lev;
	      piso->incr=giso->incr;
	      piso->hici=giso->hici;
	      strncpy(piso->lab,giso->lab,13); piso->lab[12]='\0';
	      strncpy(piso->lb,giso->lb,17); piso->lb[16]='\0';
	      strncpy(piso->tb,giso->lb,17); piso->tb[16]='\0';
	      strncpy(piso->to,giso->to,17); piso->to[16]='\0';
	      piso->cw=0;
	      piso->ls=1.;
	      piso->angle=35.;
	      piso->spc=1.;
	      piso->next=NULL;
	      *gso=piso;
	      gso=&(piso->next);
	     }
	  }

/*			Read in the attribute member assignments.	*/

	while ( ((c=getsttk(strm,&tokm)) || c == 0) && tokm != ')')
	  {
	   if (c == EOF || tokm == EOF)
	     {
	      err_warn(1,fperr,
		"Error - EOF in graphic isoline attributes for (%s%c).\n",
						str,tok);
	      killGi(ptab);
	      return 0;
	     }
	   if (tokm != '(' && tokm != '=')
	     {
	      err_warn(1,fperr,
		"Error - not a proper token (%s%c%s %c).\n",
				str,*tok,strm,tokm);
	      killGi(ptab);
	      return 0;
	     }
	   if (c == 0)
	     {
	      err_warn(1,fperr,
		"Error - not a graphic isoline attribute name (%s%c%s%c).\n",
		str,*tok,strm,tokm);
	      killGi(ptab);
	      return 0;
	     }

/*			Find which attribute is being assigned.		*/

	   for (pc=&(pat->proj[0]),i=0;i<17&&cmpnbl(strm,Gi_attr[i])!=0;i++)
	      if (i < 9) pc+=256;

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
		 killGi(ptab);
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
		 killGi(ptab);
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
		    killGi(ptab);
		    return 0;
		   }
		 for (k=0; k < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; k++)
				strf[k]=c;
		 if (c == EOF) {killGi(ptab); return EOF;}
		 strf[k]='\0';
		 if (isnum(strf)) sscanf (strf,"%f",&(pat->dsp[j++]));
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    killGi(ptab);
		    return 0;
		   }
		}
	      if (j != 4)
		{
		 err_warn(1,fperr,
			"Error - too few numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		 killGi(ptab);
		 return 0; 
		}
	      while (!istoken(tokm=getp(fpin,fpout)))
		if (tokm == EOF)
		  {
		   killGi(ptab);
		   return EOF;
		  }
/*	      if (tokm == ')') ungetp(tokm,fpin);			*/
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
		 killGi(ptab);
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
		    killGi(ptab);
		    return 0;
		   }
		 for (k=0; k < STRMAX &&
			   (c=getp(fpin,fpout)) != ',' &&
			   c != ')' &&
			   c != EOF; k++)
				strf[k]=c;
		 if (c == EOF) {killGi(ptab); return EOF;}
		 strf[k]='\0';
		 if (isnum(strf)) sscanf (strf,"%d",&(pat->idsp[j++]));
		 else 
		   {
		    err_warn(1,fperr,"Error - not a number for (%s%c%s%c%s).\n",
					str,*tok,strm,tokm,strf);
		    killGi(ptab);
		    return 0;
		   }
		}
	      if (j != 4)
		{
		 err_warn(1,fperr,
			"Error - too few numbers for (%s%c%s%c).\n",
					str,*tok,strm,tokm);
		 killGi(ptab);
		 return 0; 
		}
	      while (!istoken(tokm=getp(fpin,fpout)))
		if (tokm == EOF)
		  {
		   killGi(ptab);
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

/*		Store label generation indicator.			*/
	   else if (i == 12)
	     {
	      c=getp(fpin,fpout);
	      pat->labels=c;
	      while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF);
	     }

/*		Store the isoline definitions.				*/

	   else if (i == 13)
	     {
/*  Accept either name=(n=value,...)(...) or name(n=value,...)(...).	*/

	      if (tokm == '=')
		while (!istoken(tokm=getp(fpin,fpout)) && tokm != EOF);
	      if (tokm != '(')
	        {
	         err_warn(1,fperr,
		   "Error - not a proper token (%s%c%s %c).\n",
						str,*tok,strm,tokm);
		 killGi(ptab);
	         return 0;
	        }
/*			Find isoline definitions.			*/

	      while (tokm == '(')
		{
	         for (j=0;j<16;j++) I[j]=0;

	         liso.id=0;
	         liso.p=0;
	         liso.lev=1e20;
	         liso.incr=1e20;
	         liso.hici=0;
	         strcpy(liso.lab,"*");
	         strcpy(liso.lb,"default");
	         strcpy(liso.tb,"default");
	         strcpy(liso.to,"default");
		 liso.cw=0;
		 liso.ls=1.;
		 liso.angle=35.;
		 liso.spc=1.;
		 do		 
		   {
		    while ((c=getsttk(strf,&tokf))==0||c==EOF)
		      {
		       if (tokf == EOF || c == EOF)
			 {
			  err_warn(1,fperr,
				"Error - EOF while reading (%s%c%s%c).\n",
				str,*tok,strm,tokm);
			  killGi(ptab);
			  return 0;
			 }
		       if (tokf != ')' && tokf != ',')
			 {
			  err_warn(1,fperr,
				"Error - syntax error (%s%c%s%c%c).\n",
			 		str,*tok,strm,tokm,c);
			  killGi(ptab);
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
			  killGi(ptab);
			  return 0;
			 }

/*			Search for the name.				*/

		       for (k=0; k < 13 && cmpnbl(Gi_iso[k],strf)!=0; k++);
		       if (k == 13)
			 {
			  err_warn(1,fperr,
			  "Error - isoline element not found (%s%c%s%c%s%c).\n",
				str,*tok,strm,tokm,strf,tokf);
			  killGi(ptab);
			  return 0;
			 }
		       for (j=0;
			 j<STRMAX && (tokf=getp(fpin,fpout))!=',' && tokf!=')';
					strf[j++]=tokf);
		       strf[j]='\0';
		       if ((k < 5)|| (k>8))
			 {
			  if (isnum(strf))
			    {
			     sscanf(strf,"%f",&v);
			     if (k == 0) liso.id=v;
			     else if (k == 1) liso.p=v;
			     else if (k == 2) liso.lev=v;
			     else if (k == 3) liso.incr=v;
			     else if (k == 4) liso.hici=v;
			     else if (k == 9) liso.cw=(int)v;
			     else if (k == 10) liso.ls=v;
			     else if (k == 11) liso.angle=v;
			     else if (k == 12) liso.spc=v;
			    }
			  else
			    {
			     err_warn(1,fperr,
				"Error - should be number (%s%c%s%c%s=%s).\n",
					str,*tok,strm,tokm,Gi_iso[i],strf);
			     killGi(ptab);
			     return 0;
			    }
			 }
		       else 
			 {
			  if (k == 5)
				for (j=0;j<12&&(liso.lab[j]=strf[j])!='\0';
					j++,liso.lab[j]='\0');
			  if (k == 6)
				for (j=0;j<16&&(liso.lb[j]=strf[j])!='\0';
					j++,liso.lb[j]='\0');
			  if (k == 7)
				for (j=0;j<16&&(liso.tb[j]=strf[j])!='\0';
					j++,liso.tb[j]='\0');
			  if (k == 8)
				for (j=0;j<16&&(liso.to[j]=strf[j])!='\0';
					j++,liso.to[j]='\0');
			 }
		       I[k]+=1;
		      }
		   } while (tokf == ',');

		 piso=pat->line;
		 pso=pat->line;

		 while (piso != NULL)
		   {
		    if (piso->id == liso.id) break;
		    pso=piso;
		    piso=piso->next;
		   }
		 if (piso == NULL)
		   {
		    if((piso=(struct iso *)malloc(sizeof(struct iso))) == NULL)
		      {
		       err_warn(1,fperr,
				"Error - memory for (%s%c%s%c) not found.\n",
					str,*tok,strm,tokm);
		       killGi(ptab);
		       return 0;
		      }
		    if (pat->line == NULL) pat->line=piso;
		    else pso->next=piso;
		    piso->id=liso.id;
		    piso->p=liso.p;
		    piso->lev=liso.lev;
		    piso->incr=liso.incr;
		    piso->hici=liso.hici;
		    strncpy(piso->lab,liso.lab,13); piso->lab[12]='\0';
		    strncpy(piso->lb,liso.lb,17); piso->lb[16]='\0';
		    strncpy(piso->tb,liso.tb,17); piso->tb[16]='\0';
		    strncpy(piso->to,liso.to,17); piso->to[16]='\0';
		    piso->cw=liso.cw;
		    piso->ls=liso.ls;
		    piso->angle=liso.angle;
		    piso->spc=liso.spc;
		    piso->next=NULL;
		   }
		 else
		   {
		    if (I[0]>0) piso->id=liso.id;
		    if (I[1]>0) piso->p=liso.p;
		    if (I[2]>0) piso->lev=liso.lev;
		    if (I[3]>0) piso->incr=liso.incr;
		    if (I[4]>0) piso->hici=liso.hici;
		    if (I[5]>0) {strncpy(piso->lab,liso.lab,13); piso->lab[12]='\0';}
		    if (I[6]>0) {strncpy(piso->lb,liso.lb,17); piso->lb[16]='\0';}
		    if (I[7]>0) {strncpy(piso->tb,liso.tb,17); piso->tb[16]='\0';}
		    if (I[8]>0) {strncpy(piso->to,liso.to,17); piso->to[16]='\0';}
		    if (I[9]>0) piso->cw=liso.cw;
		    if (I[10]>0) piso->ls=liso.ls;
		    if (I[11]>0) piso->angle=liso.angle;
		    if (I[12]>0) piso->spc=liso.spc;
		   }

		 while (!istoken(tokm=getp(fpin,fpout)) && tokm == ' ' )
		   {
		    if (tokm == EOF)
		      {
		       err_warn(1,fperr,"Error (procGi) - EOF found (%s%c).\n",
				str,*tok);
		       killGi(ptab);
		       return EOF;
		      }
		   }
/*		 if (tokm != '(')
			ungetp(tokm,fpin);				*/
		}   /*      end of while (tokm == '(')		*/
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
                 sscanf(strf,"%f",&v);
                 pat->calendar=v;
                }
             }
	   else

/*		i > 16, didn't find a name.				*/

	     {
	      err_warn(1,fperr,"Error - didn't find the name (%s%c%s%c).\n",
				str,*tok,strm,tokm);
	      killGi(ptab);
	      return 0;
	     }
	   if (tokm == ')') break;
	  }


	pat=ptab->pGi_attr;
	piso=pat->line;
	if (piso == NULL)
	  {
	   if((pat->line=piso=(struct iso *)malloc(sizeof(struct iso))) == NULL)
	     {
	      err_warn(1,fperr,
				"Error - memory for (%s%c%s%c) not found.\n",
					str,*tok,strm,tokm);
	      killGi(ptab);
	      return 0;
	     }
	   piso->id=0;
	   piso->p=1;
	   piso->lev=1.e20;
	   piso->incr=1.e20;
	   piso->hici=0;
	   strcpy(piso->lab,"*");
	   strcpy(piso->lb,"default");
	   strcpy(piso->tb,"default");
	   strcpy(piso->to,"default");
	   piso->cw=0;
	   piso->ls=1.;
	   piso->angle=35.;
	   piso->spc=1.;
	   piso->next=NULL;
	  }
	c=chk_mov_Gi (ptab);
	return c;
      }

/*			Check the isoline table entry and move
			to the table if it's ok.			*/

    int chk_mov_Gi (struct gi_tab *gtab)

      {
	int i,k;
	struct iso *piso,*pisn,*pisi,*pisk;
	struct gi_attr *pat,*pan;
	struct gi_tab *ptab,*ptb;
	struct display_tab *pd;
	int I[12];

	for(ptb=ptab=&Gi_tab;
		ptab != NULL && strcmp(ptab->name,gtab->name) != 0;
						ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

         if (ptab == &Gi_tab)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (Gi_%s).\n",
				gtab->name);
	   killGi(gtab);
	   return 0;
	  }


/*	If it isn't modifying an isoline table entry then add it.	*/

	if (ptab == NULL)
	  {
	   ptb->next=gtab;
	   gtab->next=NULL;
	   return 1;
	  }

	for (i=0;i<12;i++) I[i]=0;

/*		Check for changes and move modified parameters.		*/

	pat=gtab->pGi_attr;
	pan=ptab->pGi_attr;

	if (strncmp(pat->proj,pan->proj,255) != 0) I[0]=1;
	if (strncmp(pat->xtl1,pan->xtl1,255) != 0) I[1]=1;
	if (strncmp(pat->xtl2,pan->xtl2,255) != 0) I[2]=1;
	if (strncmp(pat->xmt1,pan->xmt1,255) != 0) I[3]=1;
	if (strncmp(pat->xmt2,pan->xmt2,255) != 0) I[4]=1;
	if (strncmp(pat->ytl1,pan->ytl1,255) != 0) I[5]=1;
	if (strncmp(pat->ytl2,pan->ytl2,255) != 0) I[6]=1;
	if (strncmp(pat->ymt1,pan->ymt1,255) != 0) I[7]=1;
	if (strncmp(pat->ymt2,pan->ymt2,255) != 0) I[8]=1;
	if (pat->dsp[0] != pan->dsp[0]) I[9]=1;
	if (pat->dsp[1] != pan->dsp[1]) I[9]=1;
	if (pat->dsp[2] != pan->dsp[2]) I[9]=1;
	if (pat->dsp[3] != pan->dsp[3]) I[9]=1;
	if (pat->idsp[0] != pan->idsp[0]) I[9]=1;
	if (pat->idsp[1] != pan->idsp[1]) I[9]=1;
	if (pat->idsp[2] != pan->idsp[2]) I[9]=1;
	if (pat->idsp[3] != pan->idsp[3]) I[9]=1;
	if (strncmp(pat->timeunits,pan->timeunits,255) != 0) I[9]=1;
	if (pat->calendar != pan->calendar ) I[9]=1;
	if (strncmp(pat->xat,pan->xat,16) != 0) I[9]=I[10]=1;
	if (strncmp(pat->yat,pan->yat,16) != 0) I[9]=I[10]=1;
	if (pat->labels != pan->labels) I[10]=1;

	pisn=pan->line;
	piso=pat->line;
	if (pisn == NULL && piso!=NULL) I[11]=1;
	if (pisn != NULL && piso==NULL)
	  {
	   I[10]=1;
	   pisi=pisn;
	   while (pisi!=NULL)
	     {
	      pisk=pisi->next;
	      free((char *)pisi);
	      pisi=pisk;
	     }
	   pisn=pan->line=NULL;
	  }
	while (pisn != NULL && piso != NULL)
	  {
	   if (pisn->id  != piso->id  || pisn->p    != piso->p ||
	       pisn->lev != piso->lev || pisn->incr != piso->incr ||
	       pisn->hici != piso->hici || pisn->cw != piso->cw ||
	       pisn->ls != piso->ls || pisn->angle != piso->angle ||
	       pisn->spc != piso->spc)
					I[11]=1;
	   if (strncmp(piso->lab,pisn->lab,13) != 0) I[11]=1;
	   if (strncmp(piso->lb, pisn->lb,16)  != 0) I[11]=1;
	   if (strncmp(piso->tb, pisn->tb,16)  != 0) I[11]=1;
	   if (strncmp(piso->to, pisn->to,16)  != 0) I[11]=1;
	   if ((pisn->next == NULL && piso->next != NULL) ||
		(pisn->next != NULL && piso->next == NULL) ) I[11]=1;
	   piso=piso->next;
	   pisn=pisn->next;
	  }

/*		Check whether changes were made.  If not we're done.	*/

	for (i=0,k=0;i<12;i++) k+=I[i];
	if (k == 0)
	  {
	   ptb->next=ptab;
	   killGi(gtab);
	   return 1;
	  }
	ptb->next=gtab;
	gtab->next=ptab->next;
	killGi(ptab);

/*		If the table entry is being modified and it's being
		displayed, set for replacement of segments.		*/

	for(pd=&D_tab; pd!=NULL; pd=pd->next)
	 if (cmpncs("isoline",pd->type)==0 && strcmp(pd->g_name,gtab->name)==0)
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
	   if (I[9] || I[10] || I[11])
	     {
	      pd->dsp_seg[3]=TRUE;
	      pd->leg_seg[3]=TRUE;
	      update_ind=TRUE;
	     }
	  }
	if (!Inactive && fpout != NULL) prtGi(fpout,gtab);
	return 1;
      }

/*		Print the isoline graph attributes that are defined.	*/

    int prtGi(FILE *fp,struct gi_tab *ptab)

      {
	int i;
	int k=0;
	char *pc;
	struct gi_attr *pgi;
	struct iso *piso;

	if (ptab == NULL) return 0;
	if ((pgi=ptab->pGi_attr) != NULL)
	    {
	     pc=pgi->proj;
	     fprintf (fp,"Gi_%s(\n   ",ptab->name);
	     k=0;
	     for (i=0;i < 9;i++,pc+=256)
	       {
		if (*pc != '\0')
			k+=fprintf (fp,"%s=%s,",Gi_attr[i],pc);
		if (k > 60)
		  {
		   fprintf (fp,"\n   ");
		   k=3;
		  }
		}
	     fprintf (fp,"\n   %s(%g,%g,%g,%g),",
		Gi_attr[9],pgi->dsp[0],pgi->dsp[1],pgi->dsp[2],pgi->dsp[3]);
	     fprintf (fp,"\n   %s(%d,%d,%d,%d),",
		Gi_attr[16],pgi->idsp[0],pgi->idsp[1],pgi->idsp[2],pgi->idsp[3]);

	     if (pgi->xat[0] != '\0')
	        fprintf (fp,"\n   %s=%s,",Gi_attr[10],pgi->xat);
	     if (pgi->yat[0] != '\0')
	        fprintf (fp,"\n   %s=%s,",Gi_attr[11],pgi->yat);
	     if (pgi->timeunits[0] != '\0')
                fprintf (fp,"\n   %s=%s,",Gi_attr[14],pgi->timeunits);
             fprintf (fp,"\n   %s=%d,",Gi_attr[15],pgi->calendar);

	     fprintf (fp,"\n   %s=%c",Gi_attr[12],pgi->labels);
	     if ((piso=pgi->line) != NULL)
	       {
		fprintf (fp,",\n   %s",Gi_attr[13]);

	        for (;piso != NULL; piso=piso->next)
	          {
		   fprintf (fp,
		  "\n   (%s=%d,%s=%d,%s=%g,%s=%g,%s=%d,"
		  "\n    %s=%s,%s=%s,%s=%s,%s=%s,%s=%d,%s=%g,%s=%g,%s=%g)",

		Gi_iso[0],piso->id,  Gi_iso[1],piso->p,   Gi_iso[2],piso->lev,
		Gi_iso[3],piso->incr,Gi_iso[4],piso->hici,Gi_iso[5],piso->lab,
		Gi_iso[6],piso->lb,  Gi_iso[7],piso->tb,  Gi_iso[8],piso->to,
		Gi_iso[9],piso->cw,Gi_iso[10],piso->ls,Gi_iso[11],piso->angle,
                Gi_iso[12],piso->spc);
	          }
	       }
	     fprintf (fp,"\n  )\n");
	    }
       return 1;
      }
