#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "display.h"


    extern struct a_tab A_tab;

    extern struct display_tab D_tab;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

    extern int update_ind;
    extern int Inactive;

    char *repstr(char *s2,char *s1);

/*		Look in the array attribute table for the name.		*/
/*		Remove a table entry if it exists, return a zero
		if it doesn't.						*/

    int removeA (char *a_name)
      {
	int i,c;
	struct a_tab *ptab,*ptb;
	struct a_attr *pA_;
	struct display_tab *pd;

	for(ptb=ptab=&A_tab;
		ptab!=NULL && (c=cmpnbl(a_name,ptab->name))!=0;
						ptb=ptab,ptab=ptab->next);
	if (c != 0)
	  {
	   fprintf(fperr,
		"Error - no table entry for A_%s, not removed \n",a_name);
	   return 0;
	  }
	else
	  {

/*		Check whether the array is used in an active display.
		If it is, it should not be removed.			*/

	   
	   if (checkA_is_used(a_name) )
	     {
	      err_warn(1,fperr,
	       "Error - A_%s effects a display, it can not be removed.\n",
							a_name);
	      return 0;
	     }
	   if ( (pA_=ptab->pA_attr) != NULL)
	     {
/* 	      printf("Killing: %s\n",ptab->name); */
	      killA_attr(pA_);
	      free((char *)ptab->pA_attr);
	      ptab->pA_attr=NULL;
	      ptab->name[0]='\0';
	     }
	   if (ptab != &A_tab)
	     {
	      ptb->next=ptab->next;
	      free((char *)ptab);
	     }
	   else if (ptb->next != NULL)
	     {
	      ptab=ptb->next;
	      strncpy(ptb->name,ptab->name,17); ptb->name[16]='\0';
	      ptb->pA_attr=ptab->pA_attr;
	      ptb->next=ptab->next;
	      free((char *)ptab);
	     }
	   else
	     {
	      ptab->name[0]='\0';
	     }
	  }
	if (!Inactive && fpout != NULL)
		fprintf(fpout,"REMOVE(A_%s)\n",a_name);
	check_d_A(a_name);
	return 1;
      }

    int killA(struct a_tab *ptab)
      {
	struct a_attr *pa;

	pa=ptab->pA_attr;
	if (pa != NULL) killA_attr(pa);
	free((char *)ptab);
	return 1;
        
      }

    int killA_attr(struct a_attr *pA_)
      {
	char **pc;
	int **pi;
	float **pf;
	for (pc=&pA_->F;pc <= &pA_->axu[NDS-1]; pc++)
		if (*pc != NULL) free(*pc);
	for (pi=&pA_->XS[0];pi <= &pA_->xi[NDS-1]; pi++)
		if (*pi != NULL) free((char *)*pi);
	for (pc=&pA_->aXS[0];pc <= &pA_->axi[NDS-1]; pc++)
		if (*pc != NULL) free(*pc);
	for (pf=&pA_->XC[0];pf <= &pA_->xl[NDS-1]; pf++)
		if (*pf != NULL) free((char *)*pf);
	for (pc=&pA_->aXC[0];pc <= &pA_->axl[NDS-1]; pc++)
		if (*pc != NULL) free(*pc);
	for (pf=&pA_->XV[0];pf <= &pA_->xw[NDS-1]; pf++)
		if (*pf != NULL) free((char *)*pf);
	for (pc=&pA_->aXV[0];pc <= &pA_->axw[NDS-1]; pc++)
		if (*pc != NULL) free(*pc);
	if (pA_->mask != NULL) free((char *)pA_->mask);
	if (pA_->un.data != NULL) free((char *)pA_->un.data);
	return 1;

      }

/*		Rename an array table entry.  (str2 -> str1)		*/

    int renameA_name(char *str1,char *str2)
      {
       int i,j;
       char s1[17],s2[17];
       struct a_tab *p;
       struct display_tab *pd;

       if (str1 == NULL || str1[0] < ' ' || str2 == NULL || str2[0] < ' ')
	 {
	  err_warn(1,fperr,
	 "Error - the old or new array name is empty, not renamed.\n",s2);
	  return 0;
	 }

       for (i=0,j=0;str2 != NULL && i < 17 && str2[i] != '\0';i++)
	  if (str2[i] > ' ') s2[j++]=str2[i];
       s2[j]='\0';

       for (i=0,j=0;str1 != NULL && i < 17 && str1[i] != '\0';i++)
	  if (str1[i] > ' ') s1[j++]=str1[i];
       s1[j]='\0';

/*	Check whether the old array name is used in a display.		*/

       if (checkA_is_used(s1))
	 {
	  err_warn(1,fperr,
	        "Error - The array name (A_%s) is in use, not renamed.\n",s1);
	  return 0;
	 }
/*		Check whether the new array name exists.		*/
       for (p=&A_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s2,p->name) == 0)
	    {
	     err_warn(1,fperr,
	       "Error - (A_%s) exists, it can't be replaced by rename.\n",s2);
	     return 0;
	    }
	 }
/*	Find the old array name and rename it.				*/

       for (p=&A_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s1,p->name) == 0)
	    {
	     strcpy(p->name,s2);
	     if (!Inactive && fpout != NULL)
		fprintf(fpout,"RENAME(A_%s,A_%s)\n",s1,s2);
	     return 1;
	    }
	 }
       if (p == NULL)
	 {
	  err_warn(1,fperr,
		"Error - the array (A_%s) can't be found to rename.\n",s2);
	  return 0;
	 }
       return 1;
      }

/*		Copy Array Data to another table entry (str1 -> str2) if a
		name exists in str2.					*/

    int copy_A_name(char *str1,char *str2)
      {
       int j;
       struct a_tab *p,*ptb,*p1;
       struct a_attr *pa;
       char s[17];

       if (str1 == NULL || str2 == NULL || strprt(str2) == 0) return 0;

       for (p1=&A_tab;p1 != NULL;p1=p1->next)
					if (strcmp(str1,p1->name) == 0) break;
       if (p1 == NULL)
	 {
	  err_warn(1,fperr,
		"Error - the array (A_) not found for copy.\n",str1);
	  return 0;
	 }

       strncpy(s,str2,17); s[16]='\0';

       for (ptb=p=&A_tab;p != NULL;p=p->next)
	 {
	  if (strcmp(s,p->name) == 0)
	    {
	     err_warn(1,fperr,
	      "Error - Can't copy (A_%s) to existing array (A_%s).\n",
							str1,str2);
	     return 0;	     
	    }
	  ptb=p;
	 }
       if((p=ptb->next=(struct a_tab *)malloc(sizeof(A_tab))) == NULL)
	 {
	  err_warn(1,fperr,
	     "Error - memory for array table entry (A_%s) not found.\n",s);
	  return 0;
	 }
       strcpy(p->name,s);
       p->next=NULL;
       if((pa=p->pA_attr=
			(struct a_attr *)malloc(sizeof(struct a_attr))) == NULL)
	 {
	  err_warn(1,fperr,
	      "Error - memory for array attributes (A_%s) can't be found.\n",s);
	  free((char *)p);
	  ptb->next=NULL;
	  return 0;
	 }
/* 		Copy to the new array attributes.			*/

       zeroA_attr(pa);
       copyA_attr(p,p1);

       if (!Inactive && fpout != NULL)
		fprintf(fpout,"COPY(A_%s,A_%s)\n",str1,s);
       return 1;
      }

/*			Copy array attributes to a new structure.
			ptab to pnew					*/

    int copyA_attr (struct a_tab *ptab,struct a_tab *pnew)
      {
       int i,j,k;
       struct a_attr *pa,*pA;

       pa=ptab->pA_attr;
       pA=pnew->pA_attr;

      pa->F=repstr(pa->F,pA->F);
      pa->f=repstr(pa->f,pA->f);
      pa->lmask=repstr(pa->lmask,pA->lmask);
      pa->trnf=repstr(pa->trnf,pA->trnf);
      pa->S=repstr(pa->S,pA->S);
      pa->N=repstr(pa->N,pA->N);
      pa->TI=repstr(pa->TI,pA->TI);
      pa->U=repstr(pa->U,pA->U);
      pa->TY=repstr(pa->TY,pA->TY);
      pa->CRD=repstr(pa->CRD,pA->CRD);
      pa->CRT=repstr(pa->CRT,pA->CRT);
      pa->s=repstr(pa->s,pA->s);
      pa->n=repstr(pa->n,pA->n);
      pa->ti=repstr(pa->ti,pA->ti);
      pa->u=repstr(pa->u,pA->u);
      pa->ty=repstr(pa->ty,pA->ty);
      pa->crd=repstr(pa->crd,pA->crd);
      pa->crt=repstr(pa->crt,pA->crt);
      pa->com1=repstr(pa->com1,pA->com1);
      pa->com2=repstr(pa->com2,pA->com2);
      pa->com3=repstr(pa->com3,pA->com3);
      pa->com4=repstr(pa->com4,pA->com4);

      pa->aF=repstr(pa->aF,pA->aF);
      pa->af=repstr(pa->af,pA->af);
      pa->almask=repstr(pa->almask,pA->almask);
      pa->atrnf=repstr(pa->atrnf,pA->atrnf);
      pa->aS=repstr(pa->aS,pA->aS);
      pa->aN=repstr(pa->aN,pA->aN);
      pa->aTI=repstr(pa->aTI,pA->aTI);
      pa->aU=repstr(pa->aU,pA->aU);
      pa->aTY=repstr(pa->aTY,pA->aTY);
      pa->aCRD=repstr(pa->aCRD,pA->aCRD);
      pa->aCRT=repstr(pa->aCRT,pA->aCRT);
      pa->as=repstr(pa->as,pA->as);
      pa->an=repstr(pa->an,pA->an);
      pa->ati=repstr(pa->ati,pA->ati);
      pa->au=repstr(pa->au,pA->au);
      pa->aty=repstr(pa->aty,pA->aty);
      pa->acrd=repstr(pa->acrd,pA->acrd);
      pa->acrt=repstr(pa->acrt,pA->acrt);
      pa->acom1=repstr(pa->acom1,pA->acom1);
      pa->acom2=repstr(pa->acom2,pA->acom2);
      pa->acom3=repstr(pa->acom3,pA->acom3);
      pa->acom4=repstr(pa->acom4,pA->acom4);
/*									*/
      pa->ND=pA->ND;

       for (i=0;i < pa->ND && pA->xn[i] != NULL;i++)
	 {
	  pa->XN[i]=repstr(pa->XN[i],pA->XN[i]);
	  pa->XU[i]=repstr(pa->XU[i],pA->XU[i]);
	  pa->xn[i]=repstr(pa->xn[i],pA->xn[i]);
	  pa->xu[i]=repstr(pa->xu[i],pA->xu[i]);
	  pa->aXN[i]=repstr(pa->aXN[i],pA->aXN[i]);
	  pa->aXU[i]=repstr(pa->aXU[i],pA->aXU[i]);
	  pa->axn[i]=repstr(pa->axn[i],pA->axn[i]);
	  pa->axu[i]=repstr(pa->axu[i],pA->axu[i]);

	  pa->aXS[i]=repstr(pa->aXS[i],pA->aXS[i]);
	  pa->aXK[i]=repstr(pa->aXK[i],pA->aXK[i]);
	  pa->axs[i]=repstr(pa->axs[i],pA->axs[i]);
	  pa->axj[i]=repstr(pa->axj[i],pA->axj[i]);
	  pa->axi[i]=repstr(pa->axi[i],pA->axi[i]);

	  pa->aXC[i]=repstr(pa->aXC[i],pA->aXC[i]);
	  pa->aXF[i]=repstr(pa->aXF[i],pA->aXF[i]);
	  pa->aXL[i]=repstr(pa->aXL[i],pA->aXL[i]);
	  pa->axf[i]=repstr(pa->axf[i],pA->axf[i]);
	  pa->axl[i]=repstr(pa->axl[i],pA->axl[i]);

	  pa->aXV[i]=repstr(pa->aXV[i],pA->aXV[i]);
	  pa->aXB[i]=repstr(pa->aXB[i],pA->aXB[i]);
	  pa->aXW[i]=repstr(pa->aXW[i],pA->aXW[i]);
	  pa->axv[i]=repstr(pa->axv[i],pA->axv[i]);
	  pa->axb[i]=repstr(pa->axb[i],pA->axb[i]);
	  pa->axw[i]=repstr(pa->axw[i],pA->axw[i]);

	  if (pa->XS[i]==NULL && 
		(pa->XS[i]=(int *)malloc(sizeof(int)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  k=*pa->XS[i]=*pA->XS[i];
	  if (pa->XV[i] != NULL) free((char *)pa->XV[i]);
	  if (pa->XB[i] != NULL) free((char *)pa->XB[i]);
	  if (pa->XW[i] != NULL) free((char *)pa->XW[i]);
	  if( (pa->XV[i]=(float *)malloc(k*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  if( (pa->XB[i]=(float *)malloc((k+1)*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  if( (pa->XW[i]=(float *)malloc(k*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  for (j=0;j<k;j++)
	    {
	     *(pa->XV[i]+j)=*(pA->XV[i]+j);
	     *(pa->XB[i]+j)=*(pA->XB[i]+j);/*DNW 10/23/97*/
	     *(pa->XW[i]+j)=*(pA->XW[i]+j);
	    }
	  *(pa->XB[i]+k)=*(pA->XB[i]+k);

	  if (pa->XK[i]==NULL && 
		(pa->XK[i]=(int *)malloc(2*sizeof(int)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  *pa->XK[i]=*(pA->XK[i]);
	  *(pa->XK[i]+1)=*(pA->XK[i]+1);

	  if (pa->XC[i]==NULL && 
		(pa->XC[i]=(float *)malloc(sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  *pa->XC[i]=*pA->XC[i];

	  if (pa->XF[i]==NULL && 
		(pa->XF[i]=(float *)malloc(sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  *pa->XF[i]=*pA->XF[i];

	  if (pa->XL[i]==NULL && 
		(pa->XL[i]=(float *)malloc(sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  *pa->XL[i]=*pA->XL[i];


	  if (pa->xs[i]==NULL && 
		(pa->xs[i]=(int *)malloc(sizeof(int)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	    }
	  k=*pa->xs[i]=*pA->xs[i];
	  if (pa->xv[i] != NULL) free((char *)pa->xv[i]);
	  if (pa->xb[i] != NULL) free((char *)pa->xb[i]);
	  if (pa->xw[i] != NULL) free((char *)pa->xw[i]);
	  if( (pa->xv[i]=(float *)malloc(k*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  if( (pa->xb[i]=(float *)malloc((k+1)*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  if( (pa->xw[i]=(float *)malloc(k*sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  for (j=0;j<k;j++)
	    {
	     *(pa->xv[i]+j)=*(pA->xv[i]+j);
	     *(pa->xb[i]+j)=*(pA->xb[i]+j);
	     *(pa->xw[i]+j)=*(pA->xw[i]+j);
	    }
	  *(pa->xb[i]+k)=*(pA->xb[i]+k);

	  if (pa->xi[i]==NULL && (pa->xi[i]=(int *)malloc(sizeof(int)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  *pa->xi[i]=*pA->xi[i];

	  if (pa->xj[i]==NULL && (pa->xj[i]=(int *)malloc(sizeof(int)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  *pa->xj[i]=*pA->xj[i];

	  if (pa->xf[i]==NULL&&(pa->xf[i]=(float *)malloc(sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  *pa->xf[i]=*(pA->xf[i]);

	  if (pa->xl[i]==NULL&&(pa->xl[i]=(float *)malloc(sizeof(float)))==NULL)
	    {
	     err_warn(1,fperr,"Error - copy (A_%s to A_%s) memory overflow.\n",
				ptab->name,pnew->name);
	     return 0;
	    }
	  *pa->xl[i]=*(pA->xl[i]);
	 }
       pa->mean=pA->mean;
       pa->min=pA->min;
       pa->max=pA->max;
       pa->notok=pA->notok;
       pa->mask=NULL;
       pa->un.data=NULL;
       return 1;
      }

    int checkA_is_used(name)
      char *name;
       {
	int i,j;
	struct display_tab *pd;
	struct a_tab *p;
	struct a_attr *pa;

	pd=&D_tab;
	while (pd != NULL)
	  {
	   for (i=0; pd->off == 0 && i < pd->na && pd->a[i][0] != '\0';i++)
	     {
	      if (strcmp(pd->a[i],name) == 0) return 1;
	     }
	   pd=pd->next;
	  }
	p=&A_tab;
	while (p != NULL && (pa=p->pA_attr) != NULL)
	  {
	   if (pa->f != NULL && findnam(name,pa->f))
	     {
	      if (checkA_is_used(p->name)) return 1;
	     }
	   p=p->next;
	  }
	return 0;
       }
