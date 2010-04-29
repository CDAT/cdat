
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

    extern struct l_tab L_tab[2];
    extern struct display_tab D_tab;
    extern struct gi_tab Gi_tab;
    extern struct go_tab Go_tab;
    extern struct gfi_tab Gfi_tab;
    extern struct gfo_tab Gfo_tab;
    extern struct gcon_tab Gcon_tab;
    extern struct gfb_tab Gfb_tab;
    extern struct gv_tab Gv_tab;
    extern struct gXy_tab GXy_tab;
    extern struct gYx_tab GYx_tab;
    extern struct gXY_tab GXY_tab;
    extern struct gSp_tab GSp_tab;
    extern struct a_tab A_tab;

    extern char A_vflt[6][NDS][8];

    extern int Inactive;
    extern int update_ind;

    extern int I,J,K,L,M,N;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

    int procL_name(str,tok)

      char str[257];	/* The string containing the name.		*/
      int *tok;		/* The following token.				*/
      {
	int c,d,i;
	float val;

	char memerr[100];
	char ch[STRMAX+1];
	struct l_tab *ptr;
	struct l_tab *ptl;
	struct l_tab *ptab;
	struct l_val *pval;

	str[17]='\0'; /* make sure it's not too long for memerr.	*/

	sprintf (memerr,"Error - memory overflow. %s \n",str);

/*		The token following a name must be left parenthesis "("	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }

/*		Make a table entry					*/

	if ((ptab=(struct l_tab *) malloc(sizeof(L_tab))) == NULL)
	  {
	   err_warn(1,fperr,memerr);
	   return 0;
	  }
	strncpy (ptab->name,&str[2],17); ptab->name[16]='\0';
	ptab->count=0;
	ptab->next=NULL;

/*		Start a list.						*/

	if ( (pval=ptab->val=(struct l_val *) malloc(sizeof(*pval))) == NULL)
	  {
	   err_warn(1,fperr,memerr);
	   free((char *)ptab);
	   return 0;
          };
	pval->data=1.e20;
	pval->str=NULL;
	pval->next=NULL;

/*		Bring in the (value,'string') pairs.  If 'string' is not
		given, even as a null string ('') it will be assigned
		the string for the value.  If value is not given it
		will be assigned 10**20.				*/

	val=1.e20;
	do
	  {
	   for(i=0;(c=getp(fpin,fpout))!=EOF&&c!=','&&
				c!='\''&&c!='\"'&&c!=')'&&c!='(';)
		if (c!=' ' && i<STRMAX) ch[i++]=c;
	   ch[i]='\0';
/*			Handle an assignment of array dimension nodes.	*/

	   if (ptab->val == pval && c == '(')
	     {
	      free((char *)pval);
	      ptab->val=NULL;
	      if ((c=saveA_aslist(ptab,ch)) == 0) killL(ptab);
	      return 1;
	     }

	   if (i > 0 && c != EOF)
	     {
	      if (isnum(ch))
		{
		 sscanf(ch,"%f",&val);
		}
	      if (c == ',')
		{
		 while ((d=getp(fpin,fpout))!=EOF && d == ' ');
		 if (d !='\'' && d != '\"') ungetp(d,fpin);
		 else c=d;
		 if (d == EOF) c=EOF;
		}
	     }
	   if (c == '\'' || c == '\"')
	     {
	      for (i=0;(d=getp(fpin,fpout)) != EOF && d != c;)
		if (i < STRMAX) ch[i++]=d;
	      if (i == 0) ch[i++]=' ';
	      ch[i]='\0';
	      if (d==EOF) {c=EOF; i=0;}
	     }
	   if (i > 0 && c != EOF)
	     {
	      ptab->count+=1;
	      if (ptab->count > 1)
		{
		 if((pval->next=(struct l_val *)malloc(sizeof(*pval))) == NULL)
		   {err_warn(1,fperr,memerr); c=0; break;}
		 pval=pval->next;
		 pval->next=NULL;
		}
	      if ((pval->str=(char *)malloc(strlen(ch)+1)) == NULL)
		{err_warn(1,fperr,memerr); c=0; break;}
	      pval->data=val;
	      strcpy (pval->str,ch);
	      val=1.e20;
	     }
	  } while (c != EOF && c != ')' && c != 0);

	if (c == EOF || c == 0 || ptab->count == 0)
	  {
	   killL(ptab);
	   return 0;
	  }
/*		Look in the list table for the name and replace it
		if it exists.						*/

	ptl=ptr=&L_tab[0];
	do
	  {
	   if (cmpnbl(&str[2],ptr->name) == 0)
	     {
	      if (ptr == &L_tab[0] || ptr == &L_tab[1])
		{
		 err_warn(0,fperr,
			"Warning - can't replace the default list (%s).\n",
				str);
		 killL(ptab);
		 return 0;
		}
	      ptl->next=ptab;
	      ptab->next=ptr->next;
	      killL(ptr);
	      check_L_display(&str[2]);
	      break;
	     }
	   ptl=ptr;
	   ptr=ptr->next;
	  } while (ptr != NULL);
	ptl->next=ptab;
	return 1;
      }

/*		The table of displays and the elements used for the
		displays must be checked to determine if any need
		regenerated.						*/

    int check_L_display(str)

      char str[];
      {
	int i;
	int change;
	char lstr[257];
	struct a_tab *pA;
	struct a_attr *pa;
	char **pcc;
	char *pc;
	struct gi_tab *pGi;
	struct gi_attr *pgi;
	struct gfi_tab *pGfi;
	struct gfi_attr *pgfi;
	struct go_tab *pGo;
	struct go_attr *pgo;
	struct gfo_tab *pGfo;
	struct gfo_attr *pgfo;
	struct gcon_tab *pGcon;
	struct gcon_attr *pgcon;
	struct gfb_tab *pGfb;
	struct gfb_attr *pgfb;
	struct gv_tab *pGv;
	struct gv_attr *pgv;
	struct gXy_tab *pGXy;
	struct gXy_attr *pgXy;
	struct gYx_tab *pGYx;
	struct gYx_attr *pgYx;
	struct gXY_tab *pGXY;
	struct gXY_attr *pgXY;
	struct gSp_tab *pGSp;
	struct gSp_attr *pgSp;

       change=0;
       pA=&A_tab;
       strcpy(lstr,"L_");
       strcat(lstr,str);
       while (pA != NULL && pA->pA_attr != NULL)
	 {
	  pa=pA->pA_attr;
	  for (pcc=&pa->aF;change == 0 && pcc <= &pa->axu[NDS];pcc++)
	    {
	     pc=*pcc;
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
		change=1;
	    }
	  for (i=0;change == 0 && i < NDS;i++)
	    {
	     if ((pa->axv[i] != NULL && cmpnbl(lstr,pa->axv[i]) == 0) ||
		 (pa->axb[i] != NULL && cmpnbl(lstr,pa->axb[i]) == 0) ||
	         (pa->axw[i] != NULL && cmpnbl(lstr,pa->axw[i]) == 0) ||
		 (pa->axf[i] != NULL && cmpnbl(lstr,pa->axf[i]) == 0) ||
	         (pa->axl[i] != NULL && cmpnbl(lstr,pa->axl[i]) == 0)  )
	        change=1;
	     if ((pa->axv[i] != NULL && cmpnbl(str,pa->axv[i]) == 0) ||
		 (pa->axb[i] != NULL && cmpnbl(str,pa->axb[i]) == 0) ||
	         (pa->axw[i] != NULL && cmpnbl(str,pa->axw[i]) == 0) ||
		 (pa->axf[i] != NULL && cmpnbl(str,pa->axf[i]) == 0) ||
	         (pa->axl[i] != NULL && cmpnbl(str,pa->axl[i]) == 0)  )
	        change=1;
	    }
	  for (i=0;change == 0 && i < NDS;i++)
	    {
	     if ((pa->aXV[i] != NULL && cmpnbl(lstr,pa->aXV[i]) == 0) ||
		 (pa->aXB[i] != NULL && cmpnbl(lstr,pa->aXB[i]) == 0) ||
	         (pa->aXW[i] != NULL && cmpnbl(lstr,pa->aXW[i]) == 0) ||
		 (pa->aXF[i] != NULL && cmpnbl(lstr,pa->aXF[i]) == 0) ||
	         (pa->aXL[i] != NULL && cmpnbl(lstr,pa->aXL[i]) == 0)  )
	        change=1;
	     if ((pa->aXV[i] != NULL && cmpnbl(str,pa->aXV[i]) == 0) ||
		 (pa->aXB[i] != NULL && cmpnbl(str,pa->aXB[i]) == 0) ||
	         (pa->aXW[i] != NULL && cmpnbl(str,pa->aXW[i]) == 0) ||
		 (pa->aXF[i] != NULL && cmpnbl(str,pa->aXF[i]) == 0) ||
	         (pa->aXL[i] != NULL && cmpnbl(str,pa->aXL[i]) == 0)  )
	        change=1;
	    }
	  if (change > 0)
	    {
/*		Check displays for use of the array.			*/

	     if (check_d_A(pA->name))
			checkA_attr(pA);
	    }
	  pA=pA->next;
	 }
       pGi=&Gi_tab;
       while (pGi != NULL && (pgi=pGi->pGi_attr) != NULL)
	 {
	  for (pc=pgi->xtl1; change == 0 && pc < &pgi->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		change=1;
		check_d_G("isoline",pGi->name);
	       }
	    }
	  pGi=pGi->next;
	 }
/*		Check displays for use of the graphic display attributes.*/
       pGfi=&Gfi_tab;
       while (pGfi != NULL && (pgfi=pGfi->pGfi_attr) != NULL)
	 {
	  for (pc=pgfi->xtl1; change == 0 && pc < &pgfi->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		change=1;
		check_d_G("isofill",pGfi->name);
	       }
	    }
	  pGfi=pGfi->next;
	 }
/*		Check displays for use of the graphic display attributes.*/
       pGo=&Go_tab;
       while (pGo != NULL && (pgo=pGo->pGo_attr) != NULL)
	 {
	  for (pc=pgo->xtl1; change == 0 && pc < &pgo->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		change=1;
		check_d_G("outline",pGo->name);
	       }
	    }
	  pGo=pGo->next;
	 }
/*		Check displays for use of the graphic display attributes.*/
       pGfo=&Gfo_tab;
       while (pGfo != NULL && (pgfo=pGfo->pGfo_attr) != NULL)
	 {
	  for (pc=pgfo->xtl1; change == 0 && pc < &pgfo->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		change=1;
		check_d_G("outfill",pGfo->name);
	       }
	    }
	  pGfo=pGfo->next;
	 }
/*		Check displays for use of the graphic display attributes.*/
       pGcon=&Gcon_tab;
       while (pGcon != NULL && (pgcon=pGcon->pGcon_attr) != NULL)
	 {
	  for (pc=pgcon->xtl1; change == 0 && pc < &pgcon->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		change=1;
		check_d_G("continents",pGcon->name);
	       }
	    }
	  pGcon=pGcon->next;
	 }
/*		Check displays for use of the graphic display attributes.*/
       pGfb=&Gfb_tab;
       while (pGfb != NULL && (pgfb=pGfb->pGfb_attr) != NULL)
	 {
	  for (pc=pgfb->xtl1; change == 0 && pc < &pgfb->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		change=1;
		check_d_G("boxfill",pGfb->name);
	       }
	    }
	  pGfb=pGfb->next;
	 }
/*		Check displays for use of the graphic display attributes.*/
       pGv=&Gv_tab;
       while (pGv != NULL && (pgv=pGv->pGv_attr) != NULL)
	 {
	  for (pc=pgv->xtl1; change == 0 && pc < &pgv->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		change=1;
		check_d_G("vector",pGv->name);
	       }
	    }
	  pGv=pGv->next;
	 }
/*		Check displays for use of the graphic display attributes.*/
       pGXy=&GXy_tab;
       while (pGXy != NULL && (pgXy=pGXy->pGXy_attr) != NULL)
	 {
	  for (pc=pgXy->xtl1; change == 0 && pc < &pgXy->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		change=1;
		check_d_G("Xyvsy",pGXy->name);
	       }
	    }
	  pGXy=pGXy->next;
	 }
/*		Check displays for use of the graphic display attributes.*/
       pGYx=&GYx_tab;
       while (pGYx != NULL && (pgYx=pGYx->pGYx_attr) != NULL)
	 {
	  for (pc=pgYx->xtl1; change == 0 && pc < &pgYx->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		change=1;
		check_d_G("Yxvsx",pGYx->name);
	       }
	    }
	  pGYx=pGYx->next;
	 }
/*		Check displays for use of the graphic display attributes.*/
       pGXY=&GXY_tab;
       while (pGXY != NULL && (pgXY=pGXY->pGXY_attr) != NULL)
	 {
	  for (pc=pgXY->xtl1; change == 0 && pc < &pgXY->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		change=1;
		check_d_G("XvsY",pGXY->name);
	       }
	    }
	  pGXY=pGXY->next;
	 }
/*		Check displays for use of the graphic display attributes.*/
       pGSp=&GSp_tab;
       while (pGSp != NULL && (pgSp=pGSp->pGSp_attr) != NULL)
	 {
	  for (pc=pgSp->xtl1; change == 0 && pc < &pgSp->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		change=1;
		check_d_G("Scatter",pGSp->name);
	       }
	    }
	  pGSp=pGSp->next;
	 }
      return 1;
     }

/*		Check whether the list is used to define tics or axis
		labels in a graphics element that is used for a display.
		If so, a display update is needed.			*/

    int check_d_G(type,name)
      char *type;
      char *name;
      {

       struct display_tab *pd;

       pd=&D_tab;
       while (pd != NULL)
	 {
	  if (cmpncs(type,pd->type)==0 && cmpncs(pd->g_name,name)==0)
	    {
	     pd->xt1_seg[3]=1;
	     pd->xt2_seg[3]=1;
	     pd->xmta_seg[3]=1;
	     pd->xmtb_seg[3]=1;
	     pd->yt1_seg[3]=1;
	     pd->yt2_seg[3]=1;
	     pd->ymta_seg[3]=1;
	     pd->ymtb_seg[3]=1;
	     pd->xl1_seg[3]=1;
	     pd->xl2_seg[3]=1;
	     pd->yl1_seg[3]=1;
	     pd->yl2_seg[3]=1;
	     update_ind=1;
	    }
	  pd=pd->next;
         }
       return 0;
      }
	

/*	Find the list name and return the pointer to assigned or first
	value and return a count.					*/

    int findL_name(nstr,pt,count)

      char nstr[];	/* input string */
      struct l_val **pt;/* output pointer to table entry set of values.	*/
      int *count;	/* output count of values			*/
      {
	char name[17];
	int i,j,subscr;
	struct l_tab *pte;
	struct l_val *pval;

	subscr=0;

	if (nstr == NULL) return 0;

/*		Find the name and subscript, if there is one.
		It's form should be like: "name[I]"			*/

	for (i=0,j=0; j<16 && nstr[i]!='\0'; i++)
	  {
	   if (nstr[i] > ' ' && nstr[i] != '[') name[j++]=nstr[i];
	   else if (nstr[i]=='[')
	     {
	      i++;
	      if      (nstr[i] == 'I') subscr=I;
	      else if (nstr[i] == 'J') subscr=J;
	      else if (nstr[i] == 'K') subscr=K;
	      else if (nstr[i] == 'L') subscr=L;
	      else if (nstr[i] == 'M') subscr=M;
	      else if (nstr[i] == 'N') subscr=N;
	      else
		{
		 while (isdigit(nstr[i]))
		   {
		    subscr=(nstr[i++]-'0')+subscr*10;
		   }
		 i--;
		}
	      if (nstr[i+1] != ']')
		{
		 err_warn(1,fperr,"Error - subscript is wrong. (%s)\n",nstr);
		 return 0;
		}
	      name[j]='\0';
	      break;
	     }
	   else
	     {
/*	      err_warn(1,fperr,"Error - list name is wrong.  (%s)\n",nstr);*/
	      return 0;
	     }
	   name[j]='\0';
	  }

/*		Find the named list (for "name").			*/

	pte=&L_tab[0];
	while (pte != NULL && 
		cmpnbl(name,pte->name) != 0)
			 pte=pte->next;
	if (pte == NULL) return 0;
	*count=(int)pte->count;
	subscr=((subscr-1)%(*count))+1;
	pval=pte->val;

/*		If there is a subscript find the particular entry.	*/

	if (subscr != 0)
	  {
	   for (i=1; i<subscr; i++) pval=pval->next;
	   *count=1;
	  }
	*pt=pval;

	return 1;
      }

/*		Save array dimension values to a list.
		Given a table structure and array name.			*/

    int saveA_aslist(struct l_tab *pt,char *a_name)

      {
	int i,j,k,c,count;
	char str[257];
	int tok;
	struct a_tab *pa;
	struct a_attr *pA;
	struct l_tab *ptt,*ptr;
	struct l_val *pv,*pvv;

	for(pa=&A_tab;pa!=NULL && strcmp(pa->name,a_name)!=0;pa=pa->next);
	if (pa == NULL)
	  {
	   err_warn(1,fperr,
	   "Error - assign list from A_%s which doesn't exist.\n",a_name);
	   return 0;
	  }
	pA=pa->pA_attr;
	if ((c=getsttk(str,&tok)) == 0 || tok != ')')
	  {
	   err_warn(1,fperr,"Error - dimension name A_%s(%s%c not valid.\n",
			a_name,str,tok);
	   return 0;
	  }
	for (k=0;k<6;k++)
	  {
	   for(i=0;i<NDS && pA->xs[i]!=NULL;i++)
		if ((c=strcmp(A_vflt[k][i],str))==0) break;
	   if (c == 0) break;
	  }
	if (i==NDS || pA->xs[i]==NULL)
	  {
	   err_warn(1,fperr,"Error - dimension name A_%s(%s%c not valid.\n",
			a_name,str,tok);
	   return 0;
	  }
	if ((c=getsttk(str,&tok)) != 0 || tok != ')')
	  {
	   err_warn(1,fperr,"Error - syntax for L_%s(A_%s(%s)%s%c not valid.\n",
			pt->name,a_name,A_vflt[k][i],str,tok);
	   return 0;
	  }
	count=*pA->xs[i];
	if (k < 3) count=*pA->XS[i];
	if (k == 1 || k == 4) count+=1;

	for (j=0;j < count;j++)
	  {
	   if((pvv=(struct l_val *)malloc(sizeof(struct l_val))) == NULL)
	     {
	      err_warn(1,fperr,"Error - memory overflow. %s \n");
	      c=0;
	      break;
	     }
	   pvv->next=NULL;
	   if (pt->val == NULL)
		 pt->val=pv=pvv;
	   else
	     {
	      pv->next=pvv;
	      pv=pv->next;
	     }
	   if (k == 0)      pv->data=*(pA->XV[i]+j);
	   else if (k == 1) pv->data=*(pA->XB[i]+j);
	   else if (k == 2) pv->data=*(pA->XW[i]+j);
	   else if (k == 3) pv->data=*(pA->xv[i]+j);
	   else if (k == 4) pv->data=*(pA->xb[i]+j);
	   else if (k == 5) pv->data=*(pA->xw[i]+j);

	   sprintf(str,"%g",pv->data);

	   if ((pv->str=(char *)malloc(strlen(str)+1)) == NULL)
	     {
	      err_warn(1,fperr,"Error - memory overflow. %s \n");
	      c=0;
	      break;
	     }
	   strcpy (pv->str,str);

	   pt->count+=1;
	  }
/*			If it can be found in the table replace it.	*/

	for (ptt=ptr=L_tab;
	   ptt != NULL && (c=strcmp(pt->name,ptt->name)) != 0;
						ptr=ptt,ptt=ptt->next);
	if (c == 0)
	  {
	   if (ptt == &L_tab[0] || ptt == &L_tab[1])
		{
		 err_warn(1,fperr,
		   "Error - can't replace the default list (%s).\n",pt->name);
		 return 0;
		}
	   ptr->next=ptt->next;
	   killL(ptt);
	   check_L_display(pt->name);
	  }
/*			Add it to the end of the table.			*/

	for (ptt=L_tab;ptt->next != NULL;ptt=ptt->next);
	ptt->next=pt;	   
	if (!Inactive && fpout != NULL) fprintf(fpout,"L_%s(A_%s(%s))\n",
			pt->name,a_name,A_vflt[k][i]);
	return 1;
      }


/*			Print a list.					*/

    int prtL (FILE *fp,struct l_tab *ptab)
      {
	int j,m,k;
	struct l_val *pval;

	if (strlen(ptab->name) != 0 && (pval=ptab->val) != NULL)
	  {
	   k=fprintf (fp,"L_%s(",ptab->name);
	   m=0;

	   for (j=0; j < ptab->count && pval != NULL; j++)
	     {
	      if (m++ > 0)
		{
		 if (k > 60) k=fprintf (fp,",\n   ");
		 else k+=fprintf (fp,",");
	        }
	      if (pval->data < 1.e20)
		   k+=fprintf (fp,"%g,\"%s\"", pval->data,pval->str);
	      else
		   k+=fprintf (fp,"\"%s\"",pval->str);
	      pval=pval->next;
	     }

	   fprintf (fp,")\n");
	  }
       return 1;
      }
