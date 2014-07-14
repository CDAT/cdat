#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "picture.h"
#include "display.h"

#define STRMAX 256

    extern FILE *fpin,*fpout,*fperr;

    extern struct p_tab Pic_tab;
    extern struct p_tab Pic_tab_dud;

    extern char Pt_elem[22][12];
    extern char Pf_elem[7][10];

/*			Picture table element names			*/

    extern char Pxt_elem[4][10];
    extern char Pyt_elem[4][10];
    extern char Pxl_elem[2][10];
    extern char Pyl_elem[2][10];
    extern char Pbx_elem[8][10];
    extern char Pleg_elem[10];
    extern char Pdsp_elem[10];

/*			Picture table element structure names		*/

    extern char P_text[5][3];
    extern char P_fmt[6][3];
    extern char P_x_tic[4][3];
    extern char P_y_tic[4][3];
    extern char P_x_lab[4][3];
    extern char P_y_lab[4][3];
    extern char P_box[6][3];
    extern char P_leg[8][3];
    extern char P_dsp[5][3];

    extern struct display_tab D_tab;
 
    extern int update_ind;

    extern int Inactive;

/*	Process a picture table assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procP_name(str,tok)

      char str[257];
      int *tok;

      {
	int i,c;
	int tokm;
	int change,new;
	char strm[STRMAX+1];
	int disp[51];
	struct display_tab *pd;

	struct p_tab *ptab, *ptb;
	struct pe_text *pet;
	struct pe_form *pef;
	struct pe_x_tic *pext;
	struct pe_y_tic *peyt;
	struct pe_x_lab *pexl;
	struct pe_y_lab *peyl;
	struct pe_box *pebx;
	struct pe_leg *peleg;
	struct pe_dsp *pedsp;
	char *pc;
	int *pi;
	char fmt[46];

	strcpy(fmt,"Error - getting members assigned (%s%c%s%c).\n");

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }
	change=new=0;

	for (i=0;i<51;i++) disp[i]=0;
/*		Look in the picture table for the name.			*/
/*		Make a table entry if it doesn't exist.			*/

	for (ptb=ptab=&Pic_tab; ptab!=NULL && cmpnbl(&str[2],ptab->name)!=0;)
	  {
	   if (ptab->next == NULL)
	     {
	      if (((ptab->next)=
			(struct p_tab *)malloc(sizeof(Pic_tab))) == NULL)
	        {
		 err_warn(1,fperr,"Error - memory for %s not found \n",str);
		 return 0;
	        }

/* 		Zero the new picture elements.				*/

	      for (pc=(char *)ptab->next,i=0; i < sizeof(Pic_tab); i++,pc++)
		*pc=0;
	      ptb=ptab;
	      ptab=ptab->next;
	      for (i=0; i<16 && (ptab->name[i]=str[i+2]) != '\0'; i++);
	      ptab->normalized_flg=0;
	      ptab->orientation_flg=0;
	      ptab->name[i]='\0';
	      ptab->next=NULL;
	      for (pet=&(ptab->F),i=0;i < 22;i++,pet++)
		{
		 strcpy(pet->to,"default");
		 strcpy(pet->tb,"default");
		}
	      for (pef=&(ptab->xv),i=0;i < 7;i++,pef++)
		{
		 strcpy(pef->to,"default");
		 strcpy(pef->tb,"default");
		 strcpy(pef->fmt,"g");
		}
	      for (pext=&(ptab->xt1),i=0;i < 8;i++,pext++)
		{
		 strcpy(pext->ln,"default");
		}
	      for (pexl=&(ptab->xl1),i=0;i < 4;i++,pexl++)
		{
		 strcpy(pexl->tb,"default");
		 strcpy(pexl->to,"default");
		}
	      for (pebx=&(ptab->b1),i=0;i < 8;i++,pebx++)
		{
		 strcpy(pebx->ln,"default");
		}
	      peleg=&(ptab->leg);
	      strcpy(peleg->tb,"default");
	      strcpy(peleg->to,"default");
	      strcpy(peleg->ln,"default");

	      new=1;
	      break;
	     }
	   ptb=ptab;
	   ptab=ptab->next;
	  }
	if (ptab == &Pic_tab || ptab == &Pic_tab_dud)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (P_%s).\n",
				ptab->name);
	   return 0;
	  }
	do
	  {
	   if ((c = getsttk(strm,&tokm)) == EOF || *tok == EOF)
	     {
	      err_warn(1,fperr,
		"Error - EOF no picture elements for (%s%c).\n",str,tok);
	      return 0;
	     }
	   if (tokm != '(')
	     {
	      err_warn(1,fperr,
		"Error - not a proper token (%s%c%s %c).\n",str,*tok,strm,tokm);
	      return 0;
	     }
	   if (c == 0)
	     {
	      err_warn(1,fperr,
		"Error - not a picture element name (%s%c%s %c).\n",
		str,*tok,strm,tokm);
	      return 0;
	     }

/*			Search for the orientation element.		*/
	   if (cmpnbl(strm,"Orientation") == 0) {
	      while ((c = getsttk(strm,&tokm)) != EOF && tokm != EOF) {
                ptab->orientation_flg = atoi(strm);
                if (ptab->orientation_flg > 1) ptab->orientation_flg=0;
	        if (tokm == ')') { getsttk(strm,&tokm); break; }
              }
              c = getsttk(strm,&tokm);
           }
/*			Search text display elements.			*/
	   for (pet=&(ptab->F),i=0;
		 i<22 && cmpnbl(strm,Pt_elem[i])!=0;
		 pet++,i++);
	   if (i != 22)
	     {
	      if (!(c=getPtx_mem(P_text,pet,&disp[i],ptab->orientation_flg)) )
		{
		 err_warn(1,fperr,fmt,str,*tok,strm,tokm);
		 return 0;
		}
	     }
/*			Search formatted display elements.		*/
	   else
	     {
	      for (pef=&(ptab->xv), i=0;
		 i < 7 && cmpnbl(strm,Pf_elem[i]) != 0;
		 pef++,i++);
	      if (i != 7)
	        {
	         if (!(c=getPf_mem(P_fmt,pef,&disp[i+22],ptab->orientation_flg)) )
		   {
		    err_warn(1,fperr,fmt,str,*tok,strm,tokm);
		    return 0;
		   }
	        }

/*			Search x_tic display elements.			*/

	      else
	        {
	         for (pext=(struct pe_x_tic *)&(ptab->xt1), i=0;
		    i < 4 && cmpnbl(strm,Pxt_elem[i]) != 0;
		    pext++,i++);
	         if (i != 4)
	           {
	            if (!(c=getPxt_mem(P_x_tic,pext,&disp[i+22+7],ptab->orientation_flg)))
		      {
		       err_warn(1,fperr,fmt,str,*tok,strm,tokm);
		       return 0;
		      }
	           }

/*			Search y_tic display elements.			*/

	         else
	           {
	            for (peyt=(struct pe_y_tic *)&(ptab->yt1), i=0;
		       i < 4 && cmpnbl(strm,Pyt_elem[i]) != 0;
		       peyt++,i++);
	            if (i != 4)
	              {
	               if (!(c=getPyt_mem(P_y_tic,peyt,&disp[i+22+7+4],ptab->orientation_flg)))
		         {
		          err_warn(1,fperr,fmt,str,*tok,strm,tokm);
		          return 0;
		         }
	              }
/*			Search x_lab display elements.			*/

		    else
		      {
		       for (pexl=(struct pe_x_lab *)&(ptab->xl1), i=0;
			   i < 2 && cmpnbl(strm,Pxl_elem[i]) != 0;
			   pexl++,i++);
		       if (i != 2)
		         {
		          if (!(c=getPxl_mem(P_x_lab,pexl,&disp[i+22+7+4+4],ptab->orientation_flg)))
			    {
			     err_warn(1,fperr,fmt,str,*tok,strm,tokm);
			     return 0;
			    }
		         }

/*			Search y_lab display elements.			*/

		       else
		         {
		          for (peyl=(struct pe_y_lab *)&(ptab->yl1), i=0;
			      i < 2 && cmpnbl(strm,Pyl_elem[i]) != 0;
			      peyl++,i++);
		          if (i != 2)
			    {
			     if (!(c=getPyl_mem(P_y_lab,peyl,
							&disp[i+22+7+4+4+2],ptab->orientation_flg)))
			       {
			        err_warn(1,fperr,fmt,str,*tok,strm,tokm);
			        return 0;
			       }
			    }

/*			Search box display elements.			*/

		          else
			    {
			     for (pebx=(struct pe_box *)&(ptab->b1), i=0;
				i < 8 && cmpnbl(strm,Pbx_elem[i]) != 0;
				pebx++,i++);
			     if (i != 8)
			       {
			        if (!(c=getPbx_mem(P_box,pebx,
					&disp[i+22+7+4+4+2+2],ptab->orientation_flg)))
			          {
				   err_warn(1,fperr,fmt,str,*tok,strm,tokm);
				   return 0;
			          }
			       }

/*			Search legend display elements.			*/

			     else
			       {
			        for (peleg=(struct pe_leg *)&(ptab->leg), i=0;
				    i < 1 && cmpnbl(strm,Pleg_elem) != 0;
				    peleg++,i++);
			        if (i != 1)
			          {
				   if (!(c=getPleg_mem(P_leg,peleg,
						&disp[i+22+7+4+4+2+2+8],ptab->orientation_flg)))
				     {
				      err_warn(1,fperr,fmt,str,*tok,strm,tokm);
				      return 0;
				     }
				  }
/*			Search display space display elements.		*/

			        else
			          {
				   for(pedsp=(struct pe_dsp *)&(ptab->dsp), i=0;
				     i < 1 && cmpnbl(strm,Pdsp_elem) != 0;
				     pedsp++,i++);
				   if (i != 1)
				     {
				      if (!(c=getPdsp_mem(P_dsp,pedsp,
						&disp[i+22+7+4+4+2+2+8+1],ptab->orientation_flg)))
				        {
				         err_warn(1,fperr,fmt,str,*tok,
							strm,tokm);
				         return 0;
				        }
				     if (disp[i+22+7+4+4+2+2+8+1]) 
				            for (i=0;i<12;i++) disp[i+22+7]=1;
				     }
				   else
				     {
				      err_warn(1,fperr,"Error - picture element"
				   	" name not found (%s%c%s%c).\n",
					str,*tok,strm,tokm);
				      return 0;
				     }
			          }
			       }
			    }
		         }
		      }
		   }
	        }
	     }


	  } while ( (c=getsttk(strm,&tokm)) == 0 && tokm == ',');

	for (i=0;i<51;i++) if (disp[i] != 0) {change=1; break;}
	if (change)
	  {
	   pd=&D_tab;
	   while (pd != NULL)
	     {
	      if (strcmp(pd->p_name,&str[2]) == 0)
		{
		 for (pi=&pd->F_seg[3],i=0;i<51;i++,pi+=4)
		   {
		    if (disp[i] > 0)
		      {
		       *pi=1;
		       update_ind=1;
		      }
		   }
		}
	      pd=pd->next;
	     }
	  }

	if (tokm == EOF)
	  {
	   err_warn(1,fperr,
		"Error - EOF while reading picture elements (%s%c).\n",
				str,*tok);
	   return 0;
	  }
	if (tokm != ')')
	  {
	   err_warn(1,fperr,
		"Error - wrong terminator for picture elements (%s(...%c).\n",
				str,c);
	   return 0;
	  }
	return 1;
      }

float convert_value(int orientation, float v, int xory)
{
  float oratio;
  oratio=0.758800507;
  if (orientation==0)
    { return v;}
  else
    { if ( xory == 1)
      {/*x type value */
	return v/oratio;
      }
    else
      {/* y type */
	return v*oratio;
      }
    }
}
    int getPtx_mem (names,ptx,disp,orientation)
      char names[5][3];
      struct pe_text *ptx;
      int *disp;
      int orientation;
      {
	int i,j,k,c;
	int tokm;
	float v;
	char strm[257];
	char chstr[257];
	extern struct table_text Tt_tab;
	struct table_text *ptt;
	extern struct table_chorn To_tab;
	struct table_chorn *pto;
	while ((c = getsttk(strm,&tokm)) != EOF && tokm != EOF)
	 {
	  if (c == 0 && tokm == ')')
		return 1;

	  if (tokm != '=')
	    {
	     err_warn(1,fperr,
		"Error - picture text element name (%s) followed by (%c).\n",
		strm,tokm);
	     return 0;
	    }
	  for (i=0; i<5 && cmpnbl(strm,names[i]) != 0; i++);
	  if (i == 5)
	    {
	     err_warn(1,fperr,"Error - picture text element (%s) not found.\n",
									strm);
	     return 0;
	    }
	  for (j=0; j<256 && (c=getp(fpin,fpout))!=EOF
		    && c!=',' && c!=')'; chstr[j]=c, j++);
	  if (c == EOF)
	    {
	     err_warn(1,fperr,
			"Error - EOF found in picture text elements (%s).\n",
								strm);
	     return 0;
	    }
	  chstr[j]='\0';
	  if (i < 3)
	    {
	     if (isnum(chstr))
	       {
		sscanf(chstr,"%f",&v);
		if (i == 0) { if (ptx->p != v) {ptx->p=v; *disp=1;}}
		else if (i == 1) {if (ptx->x != v) {ptx->x=convert_value(orientation,v,1); *disp=1;}}
		else if (i == 2) {if (ptx->y != v) {ptx->y=convert_value(orientation,v,0); *disp=1;}}
	       }
	     else
	       {
		err_warn(1,fperr,
			"Error - numeric value assignment required (%s%c%s).\n",
			strm,tokm,chstr);
		return 0;
	       }
	    }
	  else if (i == 3)
	    {
	     if (strcmp(ptx->tb,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 16 && chstr[j] != '\0';j++) ptx->tb[k++]=chstr[j];

	     ptx->tb[k]='\0';
	     ptt=&Tt_tab;
	     while (ptt != NULL && cmpnbl(ptx->tb,ptt->name) != 0)
	       {
		ptt=ptt->next;
	       }
	     if (ptt == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - text bundle name (%s%c%s) not found,"
			" (%s) used.\n",
		    strm,tokm,ptx->tb,Tt_tab.name);
		strcpy(ptx->tb,Tt_tab.name);
	       }
	    }
	  else
	    {
	     if (strcmp(ptx->to,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 16 && chstr[j] != '\0';j++) ptx->to[k++]=chstr[j];

	     ptx->to[k]='\0';
	     pto=&To_tab;
	     while (pto != NULL && cmpnbl(ptx->to,pto->name) != 0)
	       {
		pto=pto->next;
	       }
	     if (pto == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - character orientation name (%s%c%s)"
		  " not found, %s used.\n",
		    strm,tokm,ptx->to,To_tab.name);
		strcpy(ptx->to,To_tab.name);
	       }
	    }
	  if (c == ')')
	    {
	     return 1;
	    }
	 }
       return c;
      }

    int getPf_mem (names,pfm,disp,orientation)
      char names[6][3];
      struct pe_form *pfm;
      int *disp;
      int orientation;
      {
	int i,j,k,c;
	int tokm;
	float v;
	char strm[257];
	char chstr[257];
	extern struct table_text Tt_tab;
	struct table_text *ptt;
	extern struct table_chorn To_tab;
	struct table_chorn *pto;

	while ((c = getsttk(strm,&tokm)) != EOF && tokm != EOF)
	 {
	  if (c == 0 && tokm == ')')
		return 1;

	  if (tokm != '=')
	    {
	     err_warn(1,fperr,
	      "Error - picture formatted element name (%s) followed by (%c).\n",
		strm,tokm);
	     return 0;
	    }
	  for (i=0; i<6 && cmpnbl(strm,names[i]) != 0; i++);
	  if (i == 6)
	    {
	     err_warn(1,fperr,
		"Error - picture formatted element (%s) not found.\n",
									strm);
	     return 0;
	    }
	  for (j=0; j<256 && (c=getp(fpin,fpout))!=EOF
		    && c!=',' && c!=')'; chstr[j]=c, j++);
	  if (c == EOF)
	    {
	     err_warn(1,fperr,
		"Error - EOF found in picture formatted elements (%s).\n",
								strm);
	     return 0;
	    }
	  chstr[j]='\0';
	  if (i < 3)
	    {
	     if (isnum(chstr))
	       {
		sscanf(chstr,"%f",&v);
		if (i == 0) {if (pfm->p != v) {pfm->p=v; *disp=1;}}
		else if (i == 1) {if (pfm->x != v) {pfm->x=convert_value(orientation,v,1); *disp=1;}}
		else if (i == 2) {if (pfm->y != v) {pfm->y=convert_value(orientation,v,0); *disp=1;}}
	       }
	     else
	       {
		err_warn(1,fperr,
			"Error - numeric value assignment required (%s%c%s).\n",
			strm,tokm,chstr);
		return 0;
	       }
	    }
	  else if (i == 3)
	    {
	     if (strcmp(pfm->fmt,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 30 && chstr[j] != '\0';j++)pfm->fmt[k++]=chstr[j];
	     pfm->fmt[k]='\0';
	    }
	  else if (i == 4)
	    {
	     if (strcmp(pfm->tb,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 16 && chstr[j] != '\0';j++) pfm->tb[k++]=chstr[j];

	     pfm->tb[k]='\0';
	     ptt=&Tt_tab;
	     while (ptt != NULL && cmpnbl(pfm->tb,ptt->name) != 0)
	       {
		ptt=ptt->next;
	       }
	     if (ptt == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - text bundle name (%s%c%s) not found,"
			" (%s) used.\n",
		    strm,tokm,pfm->tb,Tt_tab.name);
		strcpy(pfm->tb,Tt_tab.name);
	       }
	    }
	  else
	    {
	     if (strcmp(pfm->to,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 15 && chstr[j] != '\0';j++) pfm->to[k++]=chstr[j];

	     pfm->to[k]='\0';
	     pto=&To_tab;
	     while (pto != NULL && cmpnbl(pfm->to,pto->name) != 0)
	       {
		pto=pto->next;
	       }
	     if (pto == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - character orientation name (%s%c%s)"
		  " not found, %s used.\n",
		    strm,tokm,pfm->to,To_tab.name);
		strcpy(pfm->to,To_tab.name);
	       }
	    }
	  if (c == ')')
	    {
	     return 1;
	    }
	 }
       return c;
      }

    int getPxt_mem (names,pxt,disp,orientation)
      char names[4][3];
      struct pe_x_tic *pxt;
      int *disp;
      int orientation;
      {
	int i,j,k,c;
	int tokm;
	float v;
	char strm[257];
	char chstr[257];
	extern struct table_line Tl_tab;
	struct table_line *ptl;

	while ((c = getsttk(strm,&tokm)) != EOF || tokm != EOF)
	 {
	  if (c == 0 && tokm == ')')
		return 1;

	  if (tokm != '=')
	    {
	     err_warn(1,fperr,
		"Error - picture x tic element name (%s) followed by (%c).\n",
		strm,tokm);
	     return 0;
	    }
	  for (i=0; i<4 && cmpnbl(strm,names[i]) != 0; i++);
	  if (i == 4)
	    {
	     err_warn(1,fperr,"Error - picture x tic element (%s) not found.\n",
									strm);
	     return 0;
	    }
	  for (j=0; j<256 && (c=getp(fpin,fpout))!=EOF
		    && c!=',' && c!=')'; chstr[j]=c, j++);
	  if (c == EOF)
	    {
	     err_warn(1,fperr,
		"Error - EOF found in picture x tic elements (%s).\n",
								strm);
	     return 0;
	    }
	  chstr[j]='\0';
	  if (i < 3)
	    {
	     if (isnum(chstr))
	       {
		sscanf(chstr,"%f",&v);
		if (i == 0) {if (pxt->p != v) {pxt->p=v; *disp=1;}}
		else if (i == 1) {if (pxt->y1 != v) {pxt->y1=convert_value(orientation,v,0); *disp=1;}}
		else if (i == 2) {if (pxt->y2 != v) {pxt->y2=convert_value(orientation,v,0); *disp=1;}}
	       }
	     else
	       {
		err_warn(1,fperr,
			"Error - numeric value assignment required (%s%c%s).\n",
			strm,tokm,chstr);
		return 0;
	       }
	    }
	  else 
	    {
	     if (strcmp(chstr,pxt->ln) != 0) *disp=1;
	     for (j=0,k=0;j < 16 && chstr[j] != '\0';j++) pxt->ln[k++]=chstr[j];

	     pxt->ln[k]='\0';
	     ptl=&Tl_tab;
	     while (ptl != NULL && cmpnbl(pxt->ln,ptl->name) != 0)
	       {
		ptl=ptl->next;
	       }
	     if (ptl == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - line bundle name (%s%c%s) not found, (%s) used.\n",
		    strm,tokm,pxt->ln,Tl_tab.name);
		strcpy(pxt->ln,Tl_tab.name);
	       }
	    }
	  if (c == ')')
		return 1;
	 }
       return 1;
      }

    int getPyt_mem (names,pyt,disp,orientation)
      char names[4][3];
      struct pe_y_tic *pyt;
      int *disp;
      int orientation;
      {
	int i,j,k,c;
	int tokm;
	float v;
	char strm[257];
	char chstr[257];
	extern struct table_line Tl_tab;
	struct table_line *ptl;

	while ((c = getsttk(strm,&tokm)) != EOF || tokm != EOF)
	 {
	  if (c == 0 && tokm == ')')
		return 1;

	  if (tokm != '=')
	    {
	     err_warn(1,fperr,
		"Error - picture y tic element name (%s) followed by (%c).\n",
		strm,tokm);
	     return 0;
	    }
	  for (i=0; i<4 && cmpnbl(strm,names[i]) != 0; i++);
	  if (i == 4)
	    {
	     err_warn(1,fperr,"Error - picture y tic element (%s) not found.\n",
									strm);
	     return 0;
	    }
	  for (j=0; j<256 && (c=getp(fpin,fpout))!=EOF
		    && c!=',' && c!=')'; chstr[j]=c, j++);
	  if (c == EOF)
	    {
	     err_warn(1,fperr,
		"Error - EOF found in picture y tic elements (%s).\n",
								strm);
	     return 0;
	    }
	  chstr[j]='\0';
	  if (i < 3)
	    {
	     if (isnum(chstr))
	       {
		sscanf(chstr,"%f",&v);
		if (i == 0) {if (pyt->p != v) {pyt->p=v; *disp=1;}}
		else if (i == 1) {if (pyt->x1 != v) {pyt->x1=convert_value(orientation,v,1); *disp=1;}}
		else if (i == 2) {if (pyt->x2 != v) {pyt->x2=convert_value(orientation,v,1); *disp=1;}}
	       }
	     else
	       {
		err_warn(1,fperr,
			"Error - numeric value assignment required (%s%c%s).\n",
			strm,tokm,chstr);
		return 0;
	       }
	    }
	  else
	    {
	     if (strcmp(pyt->ln,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 15 && chstr[j] != '\0';j++) pyt->ln[k++]=chstr[j];

	     pyt->ln[k]='\0';
	     ptl=&Tl_tab;
	     while (ptl != NULL && cmpnbl(pyt->ln,ptl->name) != 0)
	       {
		ptl=ptl->next;
	       }
	     if (ptl == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - line bundle name (%s%c%s) not found,"
			" (%s) used.\n",
		    strm,tokm,pyt->ln,Tl_tab.name);
		strcpy(pyt->ln,Tl_tab.name);
	       }
	    }
	  if (c == ')')
		return 1;
	 }
       return 1;
      }

    int getPxl_mem (names,pxl,disp,orientation)
      char names[4][3];
      struct pe_x_lab *pxl;
      int *disp;
      int orientation;
      {
	int i,j,k,c;
	int tokm;
	float v;
	char strm[257];
	char chstr[257];
	extern struct table_text Tt_tab;
	struct table_text *ptt;
	extern struct table_chorn To_tab;
	struct table_chorn *pto;

	while ((c = getsttk(strm,&tokm)) != EOF || tokm != EOF)
	 {
	  if (c == 0 && tokm == ')')
		return 1;

	  if (tokm != '=')
	    {
	     err_warn(1,fperr,
		"Error - picture text element name (%s) followed by (%c).\n",
		strm,tokm);
	     return 0;
	    }
	  for (i=0; i<4 && cmpnbl(strm,names[i]) != 0; i++);
	  if (i == 4)
	    {
	     err_warn(1,fperr,"Error - picture text element (%s) not found.\n",
									strm);
	     return 0;
	    }
	  for (j=0; j<256 && (c=getp(fpin,fpout))!=EOF
		    && c!=',' && c!=')'; chstr[j]=c, j++);
	  if (c == EOF)
	    {
	     err_warn(1,fperr,
			"Error - EOF found in picture text elements (%s).\n",
								strm);
	     return 0;
	    }
	  chstr[j]='\0';
	  if (i < 2)
	    {
	     if (isnum(chstr))
	       {
		sscanf(chstr,"%f",&v);
		if (i == 0) { if (pxl->p != v) {pxl->p=v; *disp=1;}}
		else if (i == 1) { if (pxl->y != v) {pxl->y=convert_value(orientation,v,0); *disp=1;}}
	       }
	     else
	       {
		err_warn(1,fperr,
			"Error - numeric value assignment required (%s%c%s).\n",
			strm,tokm,chstr);
		return 0;
	       }
	    }
	  else if (i == 2)
	    {
	     if (strcmp(pxl->tb,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 15 && chstr[j] != '\0';j++) pxl->tb[k++]=chstr[j];

	     pxl->tb[k]='\0';
	     ptt=&Tt_tab;
	     while (ptt != NULL && cmpnbl(pxl->tb,ptt->name) != 0)
	       {
		ptt=ptt->next;
	       }
	     if (ptt == NULL)
	       {
		err_warn(1,fperr,
		  "Warning - text bundle name (%s%c%s) not found,"
			" (%s) used.\n",
		    strm,tokm,pxl->tb,Tt_tab.name);
		strcpy(pxl->tb,Tt_tab.name);
	       }
	    }
	  else
	    {
	     if (strcmp(pxl->to,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 15 && chstr[j] != '\0';j++) pxl->to[k++]=chstr[j];

	     pxl->to[k]='\0';
	     pto=&To_tab;
	     while (pto != NULL && cmpnbl(pxl->to,pto->name) != 0)
	       {
		pto=pto->next;
	       }
	     if (pto == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - character orientation name (%s%c%s)"
		  " not found, (%s) used.\n",
		    strm,tokm,pxl->to,To_tab.name);
		strcpy(pxl->to,To_tab.name);
	       }
	    }
	  if (c == ')')
		return 1;
	 }
       return 1;
      }

    int getPyl_mem (names,pyl,disp,orientation)
      char names[4][3];
      struct pe_y_lab *pyl;
      int *disp;
      int orientation;
      {
	int i,j,k,c;
	int tokm;
	float v;
	char strm[257];
	char chstr[257];
	extern struct table_text Tt_tab;
	struct table_text *ptt;
	extern struct table_chorn To_tab;
	struct table_chorn *pto;

	while ((c = getsttk(strm,&tokm)) != EOF || tokm != EOF)
	 {
	  if (c == 0 && tokm == ')')
		return 1;

	  if (tokm != '=')
	    {
	     err_warn(1,fperr,
		"Error - picture y label element name (%s) followed by (%c).\n",
		strm,tokm);
	     return 0;
	    }
	  for (i=0; i<4 && cmpnbl(strm,names[i]) != 0; i++);
	  if (i == 4)
	    {
	     err_warn(1,fperr,
			"Error - picture y label element (%s) not found.\n",
									strm);
	     return 0;
	    }
	  for (j=0; j<256 && (c=getp(fpin,fpout))!=EOF
		    && c!=',' && c!=')'; chstr[j]=c, j++);
	  if (c == EOF)
	    {
	     err_warn(1,fperr,
			"Error - EOF found in picture y label elements (%s).\n",
								strm);
	     return 0;
	    }
	  chstr[j]='\0';
	  if (i < 2)
	    {
	     if (isnum(chstr))
	       {
		sscanf(chstr,"%f",&v);
		if (i == 0) {if (pyl->p != v) {pyl->p=v; *disp=1;}}
		else if (i == 1) {if (pyl->x != v) {pyl->x=convert_value(orientation,v,1); *disp=1;}}
	       }
	     else
	       {
		err_warn(1,fperr,
			"Error - numeric value assignment required (%s%c%s).\n",
			strm,tokm,chstr);
		return 0;
	       }
	    }
	  else if (i == 2)
	    {
	     if (strcmp(pyl->tb,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 15 && chstr[j] != '\0';j++) pyl->tb[k++]=chstr[j];

	     pyl->tb[k]='\0';
	     ptt=&Tt_tab;
	     while (ptt != NULL && cmpnbl(pyl->tb,ptt->name) != 0)
	       {
		ptt=ptt->next;
	       }
	     if (ptt == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - text bundle name (%s%c%s) not found,"
			" (%s) used.\n",
		    strm,tokm,pyl->tb,Tt_tab.name);
		strcpy(pyl->tb,Tt_tab.name);
	       }
	    }
	  else
	    {
	     if (strcmp(pyl->to,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 15 && chstr[j] != '\0';j++) pyl->to[k++]=chstr[j];

	     pyl->to[k]='\0';
	     pto=&To_tab;
	     while (pto != NULL && cmpnbl(pyl->to,pto->name) != 0)
	       {
		pto=pto->next;
	       }
	     if (pto == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - character orientation name (%s%c%s)"
		  " not found, (%s) used.\n",
		    strm,tokm,pyl->to,To_tab.name);
		strcpy(pyl->to,To_tab.name);
	       }
	    }
	  if (c == ')')
		return 1;
	 }
       return 1;
      }

    int getPbx_mem (names,pbx,disp,orientation)
      char names[6][3];
      struct pe_box *pbx;
      int *disp;
      int orientation;
      {
	int i,j,k,c;
	int tokm;
	float v;
	char strm[257];
	char chstr[257];
	extern struct table_line Tl_tab;
	struct table_line *ptl;

	while ((c = getsttk(strm,&tokm)) != EOF || tokm != EOF)
	 {
	  if (c == 0 && tokm == ')')
		return 1;

	  if (tokm != '=')
	    {
	     err_warn(1,fperr,
	       "Error - picture box/line element name (%s) followed by (%c).\n",
		strm,tokm);
	     return 0;
	    }
	  for (i=0; i<6 && cmpnbl(strm,names[i]) != 0; i++);
	  if (i == 6)
	    {
	     err_warn(1,fperr,
			"Error - picture box/line element (%s) not found.\n",
									strm);
	     return 0;
	    }
	  for (j=0; j<256 && (c=getp(fpin,fpout))!=EOF
		    && c!=',' && c!=')'; chstr[j]=c, j++);
	  if (c == EOF)
	    {
	     err_warn(1,fperr,
		"Error - EOF found in picture box/line elements (%s).\n",
								strm);
	     return 0;
	    }
	  chstr[j]='\0';
	  if (i < 5)
	    {
	     if (isnum(chstr))
	       {
		sscanf(chstr,"%f",&v);
		if (i == 0) {if (pbx->p != v) {pbx->p=v; *disp=1;}}
		else if (i == 1) {if (pbx->x1 != v) {pbx->x1=convert_value(orientation,v,1); *disp=1;}}
		else if (i == 2) {if (pbx->y1 != v) {pbx->y1=convert_value(orientation,v,0); *disp=1;}}
		else if (i == 3) {if (pbx->x2 != v) {pbx->x2=convert_value(orientation,v,1); *disp=1;}}
		else if (i == 4) {if (pbx->y2 != v) {pbx->y2=convert_value(orientation,v,0); *disp=1;}}
	       }
	     else
	       {
		err_warn(1,fperr,
			"Error - numeric value assignment required (%s%c%s).\n",
			strm,tokm,chstr);
		return 0;
	       }
	    }
	  else
	    {
	     if (strcmp(pbx->ln,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 15 && chstr[j] != '\0';j++) pbx->ln[k++]=chstr[j];

	     pbx->ln[k]='\0';
	     ptl=&Tl_tab;
	     while (ptl != NULL && cmpnbl(pbx->ln,ptl->name) != 0)
	       {
		ptl=ptl->next;
	       }
	     if (ptl == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - line bundle name (%s%c%s) not found,"
			" (%s) used.\n",
		    strm,tokm,pbx->ln,Tl_tab.name);
		strcpy(pbx->ln,Tl_tab.name);
	       }
	    }
	  if (c == ')')
		return 1;
	 }
       return 1;
      }

    int getPdsp_mem (names,pdsp,disp,orientation)
      char names[5][3];
      struct pe_dsp *pdsp;
      int *disp;
      int orientation;
      {
	int i,j,c;
	int tokm;
	float v;
	char strm[257];
	char chstr[257];

	while ((c = getsttk(strm,&tokm)) != EOF || tokm != EOF)
	 {
	  if (c == 0 && tokm == ')')
		return 1;

	  if (tokm != '=')
	    {
	     err_warn(1,fperr,
	       "Error - picture display element name (%s) followed by (%c).\n",
		strm,tokm);
	     return 0;
	    }
	  for (i=0; i<5 && cmpnbl(strm,names[i]) != 0; i++);
	  if (i == 5)
	    {
	     err_warn(1,fperr,
			"Error - picture display element (%s) not found.\n",
									strm);
	     return 0;
	    }
	  for (j=0; j<256 && (c=getp(fpin,fpout))!=EOF
		    && c!=',' && c!=')'; chstr[j]=c, j++);
	  if (c == EOF)
	    {
	     err_warn(1,fperr,
		"Error - EOF found in picture display elements (%s).\n",
								strm);
	     return 0;
	    }
	  chstr[j]='\0';
	  if (isnum(chstr))
	    {
	     sscanf(chstr,"%f",&v);
	     if (i == 0) {if (pdsp->p != v) {pdsp->p=v; *disp=1;}}
	     else if (i == 1) {if (pdsp->x1 != v) {pdsp->x1=convert_value(orientation,v,1); *disp=1;}}
	     else if (i == 2) {if (pdsp->y1 != v) {pdsp->y1=convert_value(orientation,v,0); *disp=1;}}
	     else if (i == 3) {if (pdsp->x2 != v) {pdsp->x2=convert_value(orientation,v,1); *disp=1;}}
	     else if (i == 4) {if (pdsp->y2 != v) {pdsp->y2=convert_value(orientation,v,0); *disp=1;}}
/* 	     printf("i was: %i, now psdsp is: %f,%f,%f,%f\n",i,pdsp->x1,pdsp->y1,pdsp->x2,pdsp->y2); */
	    }
	  else
	    {
	     err_warn(1,fperr,
		"Error - numeric value assignment required (%s%c%s).\n",
		strm,tokm,chstr);
	     return 0;
	    }
	  if (c == ')')
		return 1;
	 }
       return 1;
      }

    int getPleg_mem (names,pleg,disp,orientation)
      char names[8][3];
      struct pe_leg *pleg;
      int *disp;
      int orientation;
      {
	int i,j,k,c;
	int tokm;
	float v;
	char strm[257];
	char chstr[257];
	extern struct table_text Tt_tab;
	struct table_text *ptt;
	extern struct table_chorn To_tab;
	struct table_chorn *pto;
	extern struct table_line Tl_tab;
	struct table_line *ptl;

	while ((c = getsttk(strm,&tokm)) != EOF && tokm != EOF)
	 {
	  if (c == 0 && tokm == ')')
		return 1;

	  if (tokm != '=')
	    {
	     err_warn(1,fperr,
	       "Error - picture legend element name (%s) followed by (%c).\n",
		strm,tokm);
	     return 0;
	    }
	  for (i=0; i<8 && cmpnbl(strm,names[i]) != 0; i++);
	  if (i == 8)
	    {
	     err_warn(1,fperr,
			"Error - picture legend element (%s) not found.\n",
									strm);
	     return 0;
	    }
	  for (j=0; j<256 && (c=getp(fpin,fpout))!=EOF
		    && c!=',' && c!=')'; chstr[j]=c, j++);
	  if (c == EOF)
	    {
	     err_warn(1,fperr,
		"Error - EOF found in picture legend elements (%s).\n",
								strm);
	     return 0;
	    }
	  chstr[j]='\0';
	  if (i < 5)
	    {
	     if (isnum(chstr))
	       {
	        sscanf(chstr,"%f",&v);
	        if (i == 0) {if (pleg->p != v) {pleg->p=v; *disp=1;}}
	        else if (i == 1) {if (pleg->x1 != v) {pleg->x1=convert_value(orientation,v,1); *disp=1;}}
	        else if (i == 2) {if (pleg->y1 != v) {pleg->y1=convert_value(orientation,v,0); *disp=1;}}
	        else if (i == 3) {if (pleg->x2 != v) {pleg->x2=convert_value(orientation,v,1); *disp=1;}}
	        else if (i == 4) {if (pleg->y2 != v) {pleg->y2=convert_value(orientation,v,0); *disp=1;}}
	       }
	     else
	       {
	        err_warn(1,fperr,
		   "Error - numeric value assignment required (%s%c%s).\n",
		   strm,tokm,chstr);
	        return 0;
	       }
	    }
	  else if (i == 5)
	    {
	     if (strcmp(pleg->tb,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 15 && chstr[j] != '\0';j++)pleg->tb[k++]=chstr[j];

	     pleg->tb[k]='\0';
	     ptt=&Tt_tab;
	     while (ptt != NULL && cmpnbl(pleg->tb,ptt->name) != 0)
	       {
		ptt=ptt->next;
	       }
	     if (ptt == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - text bundle name (%s%c%s) not found,"
			" (%s) used.\n",
		    strm,tokm,pleg->tb,Tt_tab.name);
		strcpy(pleg->tb,Tt_tab.name);
	       }
	    }
	  else if (i == 6)
	    {
	     if (strcmp(pleg->to,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 15 && chstr[j] != '\0';j++)pleg->to[k++]=chstr[j];

	     pleg->to[k]='\0';
	     pto=&To_tab;
	     while (pto != NULL && cmpnbl(pleg->to,pto->name) != 0)
	       {
		pto=pto->next;
	       }
	     if (pto == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - character orientation name (%s%c%s)"
		  " not found, (%s) used.\n",
		    strm,tokm,pleg->to,To_tab.name);
		strcpy(pleg->to,To_tab.name);
	       }
	    }

	  else if (i == 7)
	    {
	     if (strcmp(pleg->ln,chstr) != 0) *disp=1;
	     for (j=0,k=0;j < 15 && chstr[j] != '\0';j++)pleg->ln[k++]=chstr[j]; 
	     pleg->ln[k]='\0';
	     ptl=&Tl_tab;
	     while (ptl != NULL && cmpnbl(pleg->ln,ptl->name) != 0)
	       {
		ptl=ptl->next;
	       }
	     if (ptl == NULL)
	       {
		err_warn(0,fperr,
		  "Warning - line bundle name (%s%c%s)"
		  " not found, (%s) used.\n",
		    strm,tokm,pleg->ln,Tl_tab.name);
		strcpy(pleg->ln,Tl_tab.name);
	       }
	    }
	  if (c == ')')
		return 1;
	 }
       return 1;
      }


/*		Print template attributes.				*/

    int prtP (FILE *fp,struct p_tab *ptab)
      {
	int i, n_flg, o_flg;
	struct pe_text *ptx;
	struct pe_form *pfm;
	struct pe_dsp *pds;
	struct pe_leg *plg;
	struct pe_x_tic *pxt;
	struct pe_y_tic *pyt;
	struct pe_x_lab *pxl;
	struct pe_y_lab *pyl;
	struct pe_box *pbx;

        extern float glnorm();

        n_flg = ptab->normalized_flg;
        o_flg = ptab->orientation_flg;
	fprintf(fp,"P_%s(\n",ptab->name);
	fprintf(fp,"  Orientation(%d),\n",ptab->orientation_flg);
	for (i=0,ptx=&(ptab->F); i < 22; i++,ptx++)
	  {
/*	   if (ptx->p > 0)						*/
	   fprintf(fp,"  %s(%s=%d,%s=%g,%s=%g,%s=%s,%s=%s),\n",
		Pt_elem[i],P_text[0],ptx->p,
                P_text[1], glnorm(0,ptx->x,n_flg,o_flg),
                P_text[2], glnorm(1,ptx->y,n_flg,o_flg),
                P_text[3],ptx->tb,
		P_text[4],ptx->to);
	   }
	for (i=0,pfm=&(ptab->xv); i < 7; i++,pfm++)
	   {
/*	    if (pfm->p > 0)						*/
	    fprintf(fp,"  %s(%s=%d,%s=%g,%s=%g,%s=%s,%s=%s,%s=%s),\n",
		Pf_elem[i],P_fmt[0],pfm->p,
                P_fmt[1], glnorm(0,pfm->x,n_flg,o_flg),
                P_fmt[2], glnorm(1,pfm->y,n_flg,o_flg),
                P_fmt[3],pfm->fmt,
		P_fmt[4],pfm->tb,P_fmt[5],pfm->to);
	   }
	for (i=0,pxt=&(ptab->xt1); i < 4; i++,pxt++)
	   {
/*	      if (pxt->p > 0)						*/
	    fprintf(fp,"  %s(%s=%d,%s=%g,%s=%g,%s=%s),\n",
		Pxt_elem[i],P_x_tic[0],pxt->p,
                P_x_tic[1], glnorm(1,pxt->y1,n_flg,o_flg),
                P_x_tic[2], glnorm(1,pxt->y2,n_flg,o_flg),
                P_x_tic[3],pxt->ln);
	   }
	for (i=0,pyt=&(ptab->yt1); i < 4; i++,pyt++)
	   {
/*	    if (pyt->p > 0)						*/
	    fprintf(fp,"  %s(%s=%d,%s=%g,%s=%g,%s=%s),\n",
		Pyt_elem[i],P_y_tic[0],pyt->p,
                P_y_tic[1], glnorm(0,pyt->x1,n_flg,o_flg),
                P_y_tic[2], glnorm(0,pyt->x2,n_flg,o_flg),
                P_y_tic[3],pyt->ln);
	   }
	for (i=0,pxl=&(ptab->xl1); i < 2; i++,pxl++)
	   {
/*	    if (pxl->p > 0)						*/
	    fprintf(fp,"  %s(%s=%d,%s=%g,%s=%s,%s=%s),\n",
		Pxl_elem[i],P_x_lab[0],pxl->p,
                P_x_lab[1], glnorm(1,pxl->y,n_flg,o_flg),
                P_x_lab[2],pxl->tb,P_x_lab[3],pxl->to);
	   }
	for (i=0,pyl=&(ptab->yl1); i < 2; i++,pyl++)
	   {
/*	    if (pyl->p > 0)						*/
	    fprintf(fp,"  %s(%s=%d,%s=%g,%s=%s,%s=%s),\n",
		Pyl_elem[i],P_y_lab[0],pyl->p,
                P_y_lab[1], glnorm(0,pyl->x,n_flg,o_flg),
                P_y_lab[2],pyl->tb,P_y_lab[3],pyl->to);
	   }
	for (i=0,pbx=&(ptab->b1); i < 8; i++,pbx++)
	   {
/*	    if (pbx->p > 0)						*/
	    fprintf(fp,"  %s(%s=%d,%s=%g,%s=%g,%s=%g,%s=%g,%s=%s),\n",
		Pbx_elem[i],P_box[0],pbx->p,
                P_box[1], glnorm(0,pbx->x1,n_flg,o_flg),
                P_box[2], glnorm(1,pbx->y1,n_flg,o_flg),
                P_box[3], glnorm(0,pbx->x2,n_flg,o_flg),
		P_box[4], glnorm(1,pbx->y2,n_flg,o_flg),
                P_box[5],pbx->ln);
	   }
	plg=&(ptab->leg);
/*	if (plg->p > 0)						*/
	fprintf(fp,"  %s(%s=%d,%s=%g,%s=%g,%s=%g,%s=%g,"
			"%s=%s,%s=%s,%s=%s),\n",
		Pleg_elem,P_leg[0],plg->p,
                P_leg[1], glnorm(0,plg->x1,n_flg,o_flg),
                P_leg[2], glnorm(1,plg->y1,n_flg,o_flg),
                P_leg[3], glnorm(0,plg->x2,n_flg,o_flg),
		P_leg[4], glnorm(1,plg->y2,n_flg,o_flg),
                P_leg[5],plg->tb,
		P_leg[6],plg->to,P_leg[7],plg->ln);
	pds=&(ptab->dsp);
/*	if (pds->p > 0)						*/
	fprintf(fp,"  %s(%s=%d,%s=%g,%s=%g,%s=%g,%s=%g)  )\n",
		Pdsp_elem,P_dsp[0], pds->p,
                P_dsp[1], glnorm(0,pds->x1,n_flg,o_flg),
                P_dsp[2], glnorm(1,pds->y1,n_flg,o_flg),
                P_dsp[3], glnorm(0,pds->x2,n_flg,o_flg),
		P_dsp[4], glnorm(1,pds->y2,n_flg,o_flg));

        return 1;
      }

/*			Check the picture template table entry and
			move to the table if it's ok.			*/

    int chk_mov_P (struct p_tab *gtab, int do_killP)

      {
	int i;
	struct p_tab *ptab,*ptb;
	struct display_tab *pd;
	int *pi;
	int I[51];

	for(ptb=ptab=&Pic_tab;
		ptab != NULL && strcmp(ptab->name,gtab->name) != 0;
						ptb=ptab,ptab=ptab->next);

/*		Check if it's trying to overwrite the default.		*/

         if (ptab == &Pic_tab || ptab == &Pic_tab_dud)
	  {
	   err_warn(1,fperr,"Error - can't replace the default (P_%s).\n",
				gtab->name);
       killP(gtab);
	   return 0;
	  }


/*	If it isn't modifying a template table entry then add it.	*/

	if (ptab == NULL)
	  {
	   ptb->next=gtab;
	   return 1;
	  }

	compareP_attr(gtab,ptab,I);

/*		Turn on regeneration flags and set to update if needed.*/

        for (pd=&D_tab;pd != NULL; pd=pd->next)
	  {
	   if (strcmp(pd->p_name,gtab->name) == 0)
	     {
	      for (i=0,pi=&pd->F_seg[3];i<51;i++,pi+=4)
		if (I[i]) {*pi=1; update_ind=1;}
	     }
          }

	copyP_attr(gtab,ptab);

	if (!Inactive && fpout != NULL) prtP(fpout,gtab);
    if (do_killP)
	    killP(gtab);
	return 1;
      }

/*		Compare attributes of templates.			*/

    int compareP_attr(struct p_tab *ptab,struct p_tab *gtab,int *I)
      {
	int i;

	struct pe_text *pet,*pet2;
	struct pe_form *pef,*pef2;
	struct pe_x_tic *pext,*pext2;
	struct pe_y_tic *peyt,*peyt2;
	struct pe_x_lab *pexl,*pexl2;
	struct pe_y_lab *peyl,*peyl2;
	struct pe_box *pebx,*pebx2;
	struct pe_leg *peleg,*peleg2;
	struct pe_dsp *pedsp,*pedsp2;

	for (i=0;i<51;i++) I[i]=0;

	if (ptab == NULL || gtab == NULL) return 0;

	for (i=0,pet=&(gtab->F),pet2=&(ptab->F);i < 22;i++,pet++,pet2++)
	   if (pet->p != pet2->p || pet->x != pet2->x || pet->y != pet2->y ||
	     strcmp(pet->tb,pet2->tb)!=0 ||strcmp(pet->to,pet2->to)!=0) I[i]=1;

	for (i=0,pef=&(gtab->xv),pef2=&(ptab->xv);i < 7;i++,pef++,pef2++)
	   if (pef->p != pef2->p || pef->x != pef2->x || pef->y != pef2->y ||
	     strcmp(pef->fmt,pef2->fmt)!=0 || strcmp(pef->tb,pef2->tb)!=0 ||
	     strcmp(pef->to,pef2->to)!=0) I[i+22]=1;

	for (i=0,pext=&(gtab->xt1),pext2=&(ptab->xt1);i < 4;i++,pext++,pext2++)
	   if (pext->p != pext2->p || pext->y1 != pext2->y1 ||
	     pext->y2 != pext2->y2 || strcmp(pext->ln,pext2->ln)!=0)
		 I[i+22+7]=1;

        for (i=0,peyt=&(gtab->yt1),peyt2=&(ptab->yt1);i < 4;i++,peyt++,peyt2++)
	   if (peyt->p != peyt2->p || peyt->x1 != peyt2->x1 ||
	     peyt->x2 != peyt2->x2 || strcmp(peyt->ln,peyt2->ln)!=0)
		 I[i+22+7+4]=1;

        for (i=0,pexl=&(gtab->xl1),pexl2=&(ptab->xl1);i < 2;i++,pexl++,pexl2++)
	   if (pexl->p != pexl2->p || pexl->y != pexl2->y ||
	     strcmp(pexl->tb,pexl2->tb)!=0 || strcmp(pexl->to,pexl2->to)!=0)
		 I[i+22+7+4+4]=1;

        for (i=0,peyl=&(gtab->yl1),peyl2=&(ptab->yl1);i < 2;i++,peyl++,peyl2++)
	   if (peyl->p != peyl2->p || peyl->x != peyl2->x ||
	     strcmp(peyl->tb,peyl2->tb)!=0 || strcmp(peyl->to,peyl2->to)!=0)
		 I[i+22+7+4+4+2]=1;

        for (i=0,pebx=&(gtab->b1),pebx2=&(ptab->b1);i < 8;i++,pebx++,pebx2++)
	   if (pebx->p != pebx2->p || pebx->x1 != pebx2->x1 ||
	     pebx->x2 != pebx2->x2 || pebx->y1 != pebx2->y1 ||
	     pebx->y2 != pebx2->y2 || strcmp(pebx->ln,pebx2->ln)!=0)
				I[i+22+7+4+4+2+2]=1;

	peleg=&(gtab->leg);
	peleg2=&(ptab->leg);
	if (peleg->p != peleg2->p || peleg->x1 != peleg2->x1 ||
	  peleg->x2 != peleg2->x2 || peleg->y1 != peleg2->y1 ||
	  peleg->y2 != peleg2->y2 || strcmp(peleg->tb,peleg2->tb)!=0 ||
	  strcmp(peleg->to,peleg2->to)!=0 || strcmp(peleg->ln,peleg2->ln)!=0)
				I[22+7+4+4+2+2+8]=1;

	pedsp=&(gtab->dsp);
	pedsp2=&(ptab->dsp);
	if (pedsp->p != pedsp2->p || pedsp->x1 != pedsp2->x1 ||
	  pedsp->x2 != pedsp2->x2 || pedsp->y1 != pedsp2->y1 ||
	  pedsp->y2 != pedsp2->y2)
	  {
	   I[22+7+4+4+2+2+8+1]=1;
	   for (i=0;i<4;i++) I[i+22+7]=1;
	   for (i=0;i<4;i++) I[i+22+7+4]=1;
	   for (i=0;i<2;i++) I[i+22+7+4+4]=1;
	   for (i=0;i<2;i++) I[i+22+7+4+4+2]=1;
	  }

	return 1;
      }

/*			Copy template gtab to ptab.			*/

    int copyP_attr(struct p_tab *gtab,struct p_tab *ptab)
      {
	int i;

	struct pe_text *pet,*pet2;
	struct pe_form *pef,*pef2;
	struct pe_x_tic *pext,*pext2;
	struct pe_y_tic *peyt,*peyt2;
	struct pe_x_lab *pexl,*pexl2;
	struct pe_y_lab *peyl,*peyl2;
	struct pe_box *pebx,*pebx2;
	struct pe_leg *peleg,*peleg2;
	struct pe_dsp *pedsp,*pedsp2;

	if (ptab == NULL || gtab == NULL) return 0;

        ptab->normalized_flg = gtab->normalized_flg; /* Copy template normalization flag */
        ptab->orientation_flg = gtab->orientation_flg; /* Copy template orientation */

        for (i=0,pet=&(gtab->F),pet2=&(ptab->F);i < 22; i++,pet++,pet2++)
	  {
	   pet2->p=pet->p;
	   pet2->x=pet->x;
	   pet2->y=pet->y;
	   strcpy(pet2->tb,pet->tb);
	   strcpy(pet2->to,pet->to);
	  }
        for (i=0,pef=&(gtab->xv),pef2=&(ptab->xv);i < 7;i++,pef++,pef2++)
	  {
	   pef2->p=pef->p;
	   pef2->x=pef->x;
	   pef2->y=pef->y;
	   strcpy(pef2->fmt,pef->fmt);
	   strcpy(pef2->tb,pef->tb);
	   strcpy(pef2->to,pef->to);
	  }
        for (i=0,pext=&(gtab->xt1),pext2=&(ptab->xt1);i < 4;i++,pext++,pext2++)
	  {
	   pext2->p=pext->p;
	   pext2->y1=pext->y1;
	   pext2->y2=pext->y2;
	   strcpy(pext2->ln,pext->ln);
	  }
        for (i=0,peyt=&(gtab->yt1),peyt2=&(ptab->yt1);i < 4;i++,peyt++,peyt2++)
	  {
	   peyt2->p=peyt->p;
	   peyt2->x1=peyt->x1;
	   peyt2->x2=peyt->x2;
	   strcpy(peyt2->ln,peyt->ln);
	  }
        for (i=0,pexl=&(gtab->xl1),pexl2=&(ptab->xl1);i < 2;i++,pexl++,pexl2++)
	  {
	   pexl2->p=pexl->p;
	   pexl2->y=pexl->y;
	   strcpy(pexl2->tb,pexl->tb);
	   strcpy(pexl2->to,pexl->to);
	  }
        for (i=0,peyl=&(gtab->yl1),peyl2=&(ptab->yl1);i < 2;i++,peyl++,peyl2++)
	  {
	   peyl2->p=peyl->p;
	   peyl2->x=peyl->x;
	   strcpy(peyl2->tb,peyl->tb);
	   strcpy(peyl2->to,peyl->to);
	  }
        for (i=0,pebx=&(gtab->b1),pebx2=&(ptab->b1);i < 8;i++,pebx++,pebx2++)
	  {
	   pebx2->p=pebx->p;
	   pebx2->x1=pebx->x1;
	   pebx2->x2=pebx->x2;
	   pebx2->y1=pebx->y1;
	   pebx2->y2=pebx->y2;
	   strcpy(pebx2->ln,pebx->ln);
	  }
        peleg=&(gtab->leg);
        peleg2=&(ptab->leg);
        peleg2->p=peleg->p;
        peleg2->x1=peleg->x1;
        peleg2->x2=peleg->x2;
        peleg2->y1=peleg->y1;
        peleg2->y2=peleg->y2;
        strcpy(peleg2->tb,peleg->tb);
        strcpy(peleg2->to,peleg->to);
        strcpy(peleg2->ln,peleg->ln);

        pedsp=&(gtab->dsp);
        pedsp2=&(ptab->dsp);
        pedsp2->p=pedsp->p;
        pedsp2->x1=pedsp->x1;
        pedsp2->x2=pedsp->x2;
        pedsp2->y1=pedsp->y1;
        pedsp2->y2=pedsp->y2;

	/* always copy ratio */
	pedsp2->ratio = pedsp->ratio;
	return 1;
      }

/*                      calculate the picture template's new page position based
                        on the VCS Canvas page orientation (i.e., landscape or portrait)        */

    int convertP_to_landscape_portrait(struct p_tab *ptab)
      {
        int i;
        struct pe_text *ptx;
        struct pe_form *pfm;
        struct pe_x_tic *pxt;
        struct pe_y_tic *pyt;
        struct pe_x_lab *pxl;
        struct pe_y_lab *pyl;
        struct pe_box *pbx;

        extern float plnorm(int x_or_y, float value);

/*           Text                                                      */
             for (i=0,ptx=&(ptab->F); i < 22; i++,ptx++) {
                 ptx->x = plnorm(0, ptx->x);
                 ptx->y = plnorm(1, ptx->y);
             }
/*           Formats                                                   */
             for (i=0,pfm=&(ptab->xv); i < 7; i++,pfm++) {
                 pfm->x = plnorm(0, pfm->x);
                 pfm->y = plnorm(1, pfm->y);
             }
/*           X-Ticks                                                   */
             for (i=0,pxt=&(ptab->xt1); i < 4; i++,pxt++) {
                 pxt->y1 = plnorm(1, pxt->y1);
                 pxt->y2 = plnorm(1, pxt->y2);
             }
/*           Y-Ticks                                                   */
             for (i=0,pyt=&(ptab->yt1); i < 4; i++,pyt++) {
                 pyt->x1 = plnorm(0, pyt->x1);
                 pyt->x2 = plnorm(0, pyt->x2);
             }
/*           X-labels                                                  */
             for (i=0,pxl=&(ptab->xl1); i < 2; i++,pxl++)
                 pxl->y = plnorm(1, pxl->y);
/*           Y-labels                                                  */
             for (i=0,pyl=&(ptab->yl1); i < 2; i++,pyl++)
                 pyl->x = plnorm(0, pyl->x);
/*           Boxes/Lines                                              */
             for (i=0,pbx=&(ptab->b1); i < 8; i++,pbx++) {
                 pbx->x1 = plnorm(0, pbx->x1);
                 pbx->x2 = plnorm(0, pbx->x2);
                 pbx->y1 = plnorm(1, pbx->y1);
                 pbx->y2 = plnorm(1, pbx->y2);
             }
/*           Legend                                                    */
             ptab->leg.x1 = plnorm(0, ptab->leg.x1);
             ptab->leg.x2 = plnorm(0, ptab->leg.x2);
             ptab->leg.y1 = plnorm(1, ptab->leg.y1);
             ptab->leg.y2 = plnorm(1, ptab->leg.y2);
/*           Data                                                      */
             ptab->dsp.x1 = plnorm(0, ptab->dsp.x1);
             ptab->dsp.x2 = plnorm(0, ptab->dsp.x2);
             ptab->dsp.y1 = plnorm(1, ptab->dsp.y1);
             ptab->dsp.y2 = plnorm(1, ptab->dsp.y2);

	return 1;
      }
