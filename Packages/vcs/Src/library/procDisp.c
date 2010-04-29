#define STRMAX 256
#include "gks.h"
#include "gksshort.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "picture.h"
#include "graph.h"
#include "display.h"
#include "workstations.h"

    extern FILE *fpin,*fpout,*fperr;

    extern int update_ind;
    extern int Inactive;
    extern int user_defer_update;

    extern char Display_attr[13][17];

    extern struct a_tab A_tab;
    extern struct display_tab D_tab;
    extern struct p_tab Pic_tab;
    extern struct a_tab A_tab;
    extern struct displays d_type[NTYPES];


/*	Process a DISPLAY command.					*/

    int procDisp(str,tok)

      char str[257];
      int *tok;

      {
	int tokm,tokn;
	int i,j,c;

	char strm[STRMAX+1],strn[STRMAX+1];
	struct p_tab *pp;
	struct a_tab *pa;

	char name[17];
	int off;
	char priority[17];
	char continents[17];
	char type[17];
	char g_name[1024];
	char p_name[17];
	char p_orig_name[17];
	char a[6][17];
	if (*tok != '(')
	  {
	   err_warn(1,fperr,
		"Error (procDisp) - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }

	strncpy (name,&str[2],17); name[16]='\0';
	type[0]='\0';
	off=0;
	continents[0]='\0';
	priority[0]='\0';
	g_name[0]='\0';
	p_name[0]='\0';
	p_orig_name[0]='\0';
	for (i=0; i < 6; i++) a[i][0]='\0';

	while ( (c=getsttk(strm,&tokm)) != EOF && tokm != EOF)
	  {
	   if (tokm != '=')
	     {
	      err_warn(1,fperr,
		"Error - (Display) not a proper token (%s%c%s%c).\n",
			str,*tok,strm,tokm);
	      return 0;
	     }
	   for (i=0; i < 13 && cmpncs(strm,Display_attr[i]) != 0; i++);
	   if (i == 13)
	     {
	      err_warn(1,fperr,
		"Error - name not found (%s%c%s%c).\n",str,*tok,strm,tokm);
	      return 0;
	     }
	   j=0;
	   while ( (c=getp(fpin,fpout))!=',' && c!=')' && c!=EOF)
			if (j < 16 && c > ' ' && !istoken(c)) strn[j++]=c;
	   strn[j]='\0';
	   tokn=c;

	   if (tokn == EOF)
	     {
	      err_warn(1,fperr,
		"Error - EOF found in Display(%s%c%s%c).\n",str,*tok,strm,tokm);
	      return 0;
	     }
	   if (i == 0)
	     {
	      off=0;
	      if (strn[0]=='t' || strn[0]=='T') off=1;
	      else if (isdigit(strn[0]) && strn[0]!='0') off=1;
	     }
	   else if (i == 1)
	     {
	      if (!isnum(strn) )
		{
		 err_warn(1,fperr,
		   "Error - value should be a number (%s%c%s%c%s).\n",
			str,*tok,strm,tokm,strm);
		 return 0;
		}
	      strncpy(priority,strn,17); priority[16] = '\0';
	     }
	   else if (i == 2)
	     {
	      if (!isnum(strn) )
		{
		 err_warn(1,fperr,
		   "Error - value should be a number (%s%c%s%c%s).\n",
			str,*tok,strm,tokm,strm);
		 return 0;
		}
	      strncpy(continents,strn,17); priority[16] = '\0';
	     }
	   else if (i == 3)
	     {
	      
	      strncpy(type,strn,17); type[16] = '\0';
	      for (j=0;j<NTYPES && cmpncs(d_type[j].type,type)!=0;j++);
	      if (j == NTYPES)
	        {
	         err_warn(1,fperr,
		   "Error - display type doesn't exist (%s%c%s=%s).\n",
			str,*tok,Display_attr[2],type);
	         return 0;
	        }
	     }
	   else if (i == 4)
	     {
	      strncpy(p_name,strn,17); p_name[16] = '\0';
	      for(pp=&Pic_tab;pp!=NULL&&cmpnbl(pp->name,p_name)!=0;pp=pp->next);
	      if (pp == NULL)
	        {
	         err_warn(1,fperr,
		   "Error - picture element name doesn't exist (%s%c%s=%s).\n",
			str,*tok,Display_attr[3],p_name);
	         return 0;
	        }
	     }

	   else if (i == 5)
	     {
	      strncpy(p_orig_name,strn,17); p_orig_name[16] = '\0';
/* 	      for(pp=&Pic_tab;pp!=NULL&&cmpnbl(pp->orig_name,p_orig_name)!=0;pp=pp->next); */
/* 	      if (pp == NULL) */
/* 	        { */
/* 	         err_warn(1,fperr, */
/* 		   "Error - picture element name doesn't exist (%s%c%s=%s).\n", */
/* 			str,*tok,Display_attr[3],p_name); */
/* 	         return 0; */
/* 	        } */
	     }

	   else if (i == 6)
	     {
	      
	      strncpy(g_name,strn,1024); g_name [1023] = '\0';
	     }

/*		Process the data assignments - a, b, c, d, e, f.	*/

	   else if (i > 6)
	     {
	      strncpy(a[i-7],strn,17); a[i-7][16] = '\0';
	      for (pa=&A_tab;pa!=NULL&&cmpnbl(pa->name,a[0])!=0;pa=pa->next);
	      if (pa == NULL)
	        {
	         err_warn(1,fperr,
		   "Error - display array name doesn't exist (%s%c%s=%s).\n",
			str,*tok,Display_attr[i],a[i-7]);
	         return 0;
	        }
	     }

	   if (tokn == ')') break;
	   if (tokn != ',')
	     {
	      err_warn(1,fperr,
		"Error - in Display not a proper token (%s%c%s%c%s%c).\n",
			str,*tok,strm,tokm,strn,tokn);
	      return 0;
	     }
	  } /* end of while != EOF			*/


/*		When no name was given return error.			*/

        if (strprt(name)<=0) return 0;

	if (c == EOF || tokm == EOF) {tokm=EOF; ungetp(tokm,fpin);}
	c=d_update(type,off,name,priority,continents,g_name,p_name,a);
	if (tokm == EOF) c=EOF;
	return c;
      }

/*		Update the display parameters.				*/

    int d_update (type,off,d_name,priority,continents,g_name,p_name,a)

      char type[]; 	/* Type of display.				*/
      int  off;		/* Display turned off indicator.		*/
      char d_name[];  	/* Display name.				*/
      char priority[];	/* Priority, a numeric string.			*/
      char g_name[];	/* Graphic display attributes name.		*/
      char p_name[];	/* Picture element attributes name.		*/
      char continents[];	/* continents type a numeric string.		*/
/*       char p_orig_name[];	/\* Original Picture element attributes name.		*\/ */
      char a[][17];	/* Array attributes name(s).			*/

      {
	int i,it,j,k,c,n,na;
	int I[8];
	int *pi;
	float v;
	int iv,ic;
	Gsegattr psa;
        Gintlist wsid;
        int *pid;

	struct display_tab *dtab, *dtb;
	struct a_attr *pA;
        struct p_tab *pp;
	struct a_tab *pa;

        //extern struct workstations Wkst[];
        extern Gconid_X_drawable 	connect_id;

/*		Turn the display on if it's error.			*/

	if (off == 2) off=0;

/*		The display must be named, check for nil string.	*/

	if (d_name[0] == '\0')
	  { 
	   err_warn(1,fperr,"Error - (d_update) no display name given.\n");
	   return 0;
	  }

/*		Find number of arrays required for type and check type.	*/

	if (strlen(type) > (size_t) 0)
	  {
	   for(it=0;it < NTYPES && cmpncs(type,d_type[it].type) != 0;it++);
	   if (it != NTYPES)
	      na=d_type[it].na;
	   else
	     {
	      off=2;
	      na=0;
	     }
	  }
	else
	  {
	   na=0;
	   off=2;
	  }
	if (strprt(g_name) == 0) {off=2; g_name[0]='\0';}
	if (strprt(p_name) == 0) {off=2; p_name[0]='\0';}
/* 	if (strprt(p_orig_name) == 0) {off=2; p_orig_name[0]='\0';} */

	for (n=0;n < na;n++)
	  {
	   if (strprt(a[n]) <= 0) off=2;
	  }

/*		Change priority string to a number.			*/

	if (priority[0] != '\0' && isnum(priority))
	  {
	   sscanf(priority,"%f",&v);
	   iv=v;
	  }
	else iv=0;

/*		Change priority string to a number.			*/

	if ((continents[0] != '\0') && isnum(continents))
	  {
	    sscanf(continents,"%f",&v);
	    ic=v;
	  }
	else 
	  ic=-1;
/*		Find a table entry or add one.  The first one has been
		allocated.  This comparison determines whether it's a
		change to an existing display or a new display request.	*/

	
	dtab=dtb=&D_tab;
	for(;dtab!=NULL && (c=cmpnbl(d_name,dtab->name))!=0; dtab=dtab->next)
				dtb=dtab;

	for (i=0;i<8;i++) I[i]=0;

/*		If it's a change to an existing display,
		erase the display segments and make the changes.	*/

	if (dtab != NULL)
	  {
/*			Check what changed.				*/

	   if (off != dtab->off) I[1]=1;
	   if (iv != dtab->pri) I[2]=1;
	   if ((ic!=-2) && (ic != dtab->continents)) I[3]=1;
	   if (cmpnbl(dtab->type,type) != 0) I[4]=1;
	   if (cmpncs(dtab->g_name,g_name) != 0) I[5]=1;
	   if (cmpncs(dtab->p_name,p_name) != 0) I[6]=1;
/* 	   if (cmpncs(dtab->p_orig_name,p_orig_name) != 0) I[6]=1; */
	   if (na != dtab->na) I[7]=1;
	   else
	      for (i=0;i < na;i++) if (cmpnbl(dtab->a[i],a[i]) != 0) I[7]=1;

	   if (I[2])
	     {
	      for (pi=&dtab->F_seg[0]; pi <= &dtab->dsp_seg[3]; pi+=4)
		{
		 if (*pi != 0)
		   {
		    psa.seg=*pi;
		    gqsga(&psa);
		    v=((float)iv+((psa.pri*1000.0)-dtab->pri))/1000.0;
		    gssgp(*pi,v);
		   }
		}
              wsid.number = 0;
              wsid.integers = NULL;
	      gqacwk(&wsid);
	      for (i=0,pid=wsid.integers;i<wsid.number;pid++,i++)
		if (*pid == 1)
		  {
                   if ((Inactive==1) && (user_defer_update==0))
                      guwk(1,GPERFORM);
                   else if ((Inactive==0) && (user_defer_update==0))
                      guwk(1,GPERFORM);
		  }
              if (wsid.number > 0 && wsid.integers != NULL)
                  free((char *)wsid.integers);
	     }
	   if (I[3] || I[4] || I[5] || I[6] || I[7] || I[1])
	     {
	      for (pi=&dtab->F_seg[0]; pi <= &dtab->dsp_seg[3]; pi+=4)
	        {
	         if (*pi > 0)
	      	      gdsg(*pi);
		 if (!off)
		   {
	            *pi=0;
	            *(pi+1)=0;
	            *(pi+2)=0;
	            *(pi+3)=1;
		   }
	        }
 /* DNW - Stops flashing when legend priority is set to 0 */
              if (dtab->p_name[0] != '\0') {
                 for(pp=&Pic_tab;pp!=NULL&&cmpnbl(pp->name,dtab->p_name)!=0;pp=pp->next);
                 dtab->leg_seg[3]=pp->leg.p;
              }

	      if (Inactive == 1)  /* Do not update if VCS interface is up.   */
	         /* Update will occur in the update module. */
                 if ((Inactive==1) && (user_defer_update==0))
                    guwk(1,GPERFORM);
                 else if ((Inactive==0) && (user_defer_update==0))
                    guwk(1,GPERFORM);
	      if (!off) update_ind=1;
	     }

/*		Set up the information that was given.			*/

	   dtab->off=off;
 	   if (I[2]) dtab->pri=iv;
 	   if (I[3]) dtab->continents=ic;
	   if (I[4]) {strncpy(dtab->type,type,17); dtab->type[16]='\0';}
	   if (I[5]) {strncpy(dtab->g_name,g_name,1024); dtab->g_name[1023]='\0';}
	   if (I[6]) {strncpy(dtab->p_name,p_name,17); dtab->p_name[16]='\0';}
	   dtab->na=na;
	   if (I[7])
	     {
/*			Remove array data names.			*/

	      for (i=0; i < dtab->na;i++)
		{
		 dtab->a[i][0]='\0';
		}
/*			Insert new array data names.			*/
	      for (i=0; i < n; i++)
	        {
	         strncpy(dtab->a[i],a[i],17); dtab->a[i][16]='\0';
	        }
	     }

	  }

/*		Else add a new display.					*/

	else
	  {
	   if (dtb->name[0] == '\0')
	      dtab=dtb;

	   else if ((dtab=(dtb->next)=
			(struct display_tab *)malloc(sizeof(D_tab)))==NULL)
	     {
	      err_warn(1,fperr,"Error - memory for DISPLAY not found \n");
	      return 0;
	     }
	   dtab->next=NULL;
/*		Initialize the table entry.				*/

	   D_init(dtab);

/*		Set up the information that was given.			*/

	   strncpy(dtab->name,d_name,17); dtab->name[16] = '\0';
	   
	   dtab->off=off;
 	   dtab->pri=iv;
 	   if (ic!=-2) dtab->continents=ic;
	   if (strlen(type) > (size_t) 0)
	     {
	      I[4]=1;
	      strncpy(dtab->type,type,17); dtab->type[16]='\0';
	     }
	   if (strlen(g_name) > (size_t) 0)
	     {
	      I[5]=1;
	      strncpy(dtab->g_name,g_name,1024); dtab->g_name[1023]='\0';
	     }
	   if (strlen(p_name) > (size_t) 0)
	     {
	      I[6]=1;
	      strncpy(dtab->p_name,p_name,17); dtab->p_name[16]='\0';
	     }
/* 	   if (strlen(p_orig_name) > (size_t) 0) */
/* 	     { */
/* 	      I[6]=1; */
/* 	      strncpy(dtab->p_orig_name,p_orig_name,17); dtab->p_orig_name[16]='\0'; */
/* 	     } */

 /* DNW - Stops flashing when legend priority is set to 0 */
           if (dtab->p_name[0] != '\0') {
              for(pp=&Pic_tab;pp!=NULL&&cmpnbl(pp->name,dtab->p_name)!=0;pp=pp->next);
              dtab->leg_seg[3]=pp->leg.p;
           }

	   dtab->na=na;
	   for (i=0; i < na; i++)
	     {
	      if (strlen(a[i]) > (size_t) 0)
		{
		 I[7]=I[7]+1;
	         strncpy(dtab->a[i],a[i],17); dtab->a[i][16]='\0';
		}
	     }
	   if (!off && I[7] == n  && I[3] && I[4] && I[5] && I[6] ) update_ind=1;
	  }

/*  		Add the workstation id to the display			*/
        //dtab->wkst_id = Wkst[0].id;
	dtab->wkst_id = 7+connect_id.wkst_id;

        if (!Inactive && fpout != NULL)
				   prtDisp(fpout,dtab);
	return 1;
      }

/*	Initialize the display table structure for the display.		*/

    int D_init (dtab)

      struct display_tab *dtab;
      {
	int i;
	int *pi;

        dtab->wkst_id = 1;
	dtab->off=0;
	dtab->pri=0;
	dtab->continents=-1;
	dtab->type[0]='\0';
	dtab->g_name[0]='\0';
	dtab->p_name[0]='\0';
	dtab->p_orig_name[0]='\0';
	dtab->na=0;
	for (i=0; i < 6; i++) dtab->a[i][0]='\0';
	for (pi=&(dtab->F_seg[0]); pi<=&(dtab->dsp_seg[3]);)
	  {
	   *pi++=0;
	   *pi++=0;
	   *pi++=0;
	   *pi++=1;
	  }
	dtab->next=NULL;
	return 1; 
      }

/*		Rename a display table entry.				*/


    int rename_disp(char *name,char *new_name)

      {
       struct display_tab *dtab;

/*		Find a table entry or add one.
		The first one has been allocated.			*/

	dtab=&D_tab;
	
	for (;dtab!=NULL && (cmpnbl(new_name,dtab->name))!=0;dtab=dtab->next);
	if (dtab != NULL)
	  {
	   err_warn(1,fperr,"Error - can't rename (%s) to (%s) it exists.\n",
					name,new_name);
	   return 0;
	  }
	dtab=&D_tab;
	
	for (;dtab!=NULL && (cmpnbl(name,dtab->name))!=0;dtab=dtab->next);
	if (dtab != NULL)
	  {
	   strncpy(dtab->name,new_name,17); dtab->name[16]='\0';
	  }
	else
	  {
	   err_warn(1,fperr,"Error - can't rename (%s) doesn't exist.\n",name);
	   return 0;
	  }
	return 1;
      }


/*		Copy the display table entry to another (i.e., dtab1 into dtab2).	*/

    int copy_disp (dtab1, dtab2)
      struct display_tab *dtab1;         /* copy display from */
      struct display_tab *dtab2;         /* copy display to */
      {
	int i;
	int *pi1, *pi2;

/*		Initialize the table entry.				*/
        D_init(dtab2);

	if (dtab1 == NULL) {dtab2->name[0]='\0'; return 1;}

/*		Copy the information from the first display to the second.	*/
        strncpy(dtab2->name,dtab1->name,17); dtab2->name[16] = '\0'; 
        dtab2->wkst_id = dtab1->wkst_id;
        dtab2->off = dtab1->off;
        dtab2->pri = dtab1->pri;
        dtab2->continents = dtab1->continents;
        strncpy(dtab2->type,dtab1->type,17); dtab2->type[16]='\0';
        strncpy(dtab2->g_name,dtab1->g_name,17); dtab2->g_name[16]='\0';
        strncpy(dtab2->p_name,dtab1->p_name,17); dtab2->p_name[16]='\0';
        strncpy(dtab2->p_orig_name,dtab1->p_orig_name,17); dtab2->p_orig_name[16]='\0';
        dtab2->na = dtab1->na;

	for (i=0; i < dtab1->na; i++) {
           if (strlen(dtab1->a[i]) > (size_t) 0) {
              strncpy(dtab2->a[i],dtab1->a[i],17); dtab2->a[i][16]='\0';
           }
        }

	for (pi1=&(dtab1->F_seg[0]), pi2=&(dtab2->F_seg[0]); pi1<=&(dtab1->dsp_seg[3]);)
	  {
	   *pi2++=*pi1++;
	   *pi2++=*pi1++;
	   *pi2++=*pi1++;
	   *pi2++=*pi1++;
	  }
	dtab2->next=dtab1->next;

	return 1;
      }


/*			Print a set of Display attributes.		*/

    int prtDisp(FILE *fp,struct display_tab *dtab)
      {
	int i,n;

	if (dtab == NULL) return 0;
	for (i=0,n=0;i<NTYPES;i++)
	 if (cmpncs(dtab->type,d_type[i].type)==0)
	  {
	   n=d_type[i].na;
	   break;
	  }
	fprintf(fp,"D_%s(",dtab->name);
	fprintf(fp,"%s=%d",Display_attr[0],dtab->off);
	fprintf(fp,",%s=%d",Display_attr[1],dtab->pri);
	fprintf(fp,",%s=%d",Display_attr[2],dtab->continents);
	if (dtab->type[0]!='\0')
		fprintf(fp,",%s=%s",Display_attr[3],dtab->type);
	if (dtab->p_name[0]!='\0')
		fprintf(fp,",%s=%s",Display_attr[4],dtab->p_name);
	if (dtab->p_orig_name[0]!='\0')
		fprintf(fp,",%s=%s",Display_attr[5],dtab->p_orig_name);
	if (dtab->g_name[0]!='\0')
		fprintf(fp,",%s=%s",Display_attr[6],dtab->g_name);
	for (i=0;i<n;i++)
	 if (dtab->a[i][0]!='\0')
		fprintf(fp,",%s=%s",Display_attr[i+7],dtab->a[i]);
	fprintf(fp,")\n");
	return 1;
      }
