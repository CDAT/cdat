#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "list.h"
#include "display.h"
#include "drscdf.h"

#define MAXELEM 120
#define STRMAX 256
#define TRUE 1
#define FALSE 0

    extern struct a_tab A_tab;
    extern struct l_tab L_tab[2];
    extern char A_strg[38][12];
    extern char A_intg[5][NDS][8];
    extern char A_sflt[5][NDS][8];
    extern char A_vflt[6][NDS][8];
    extern struct display_tab D_tab;

    extern int update_ind;

    extern int Inactive;

    extern int I,J,K,L,M,N;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */


    char *repstr(char *s2,char *s1);
    struct a_tab *getA(char *str);
    struct data_flo *logicomp (struct a_tab *ptab);
    int compu_log(struct data_flo **pd,struct a_attr *pa);

    int trimbl(char *s,int l);

/*	Process an array attribute name definition or assignment.
	The string defining the name must be in str[] and the following
	token must be in tok.						*/

    int procA_name(str,tok)

      char str[257];
      int *tok;

      {
	int c;
	struct a_tab *ptab;
	struct a_attr *pA_;

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%c). %s \n",*tok,str);
	   return 0;
	  }
/*		Set the end delimiting character.			*/
	*tok=')';

/* 		If the array attribute set name exists then get
		the attribute set, otherwise create a new attribute set.*/

	if ((ptab=getA(&str[2])) == 0)
	  {

/*		Make a table entry.					*/

	   if ((ptab=(struct a_tab *)malloc(sizeof(A_tab))) == NULL)
	     {
	      err_warn(1,fperr,"Error - memory for %s not found \n",str);
	      return 0;
	     }
	   strncpy (ptab->name,&str[2],17); ptab->name[16] = '\0';
	   ptab->FROM_CDAT = 0;
	   ptab->pA_attr=NULL;
	   ptab->next=NULL;
 
/*		Add the attribute set.					*/

	   if((pA_=ptab->pA_attr=(struct a_attr *)malloc(sizeof(*pA_)))==NULL)
	     {
	      err_warn(1,fperr,"Error - memory for %s not found \n",str);
	      free((char *)ptab);
	      return 0;
             }
	   strncpy (ptab->name,&str[2],17); ptab->name[16] = '\0';
	   zeroA_attr(pA_); /* nullify the set of attributes */
	   pA_->notok=1;
	  }

/*		must be A_name(member= string or value(s) )		*/

	if ((c=getA_member(str,tok,ptab->pA_attr)) == EOF || c == 0)
	  {
	   free((char *)ptab);
	   free((char *)ptab->pA_attr);
	   return c;
	  }
/*		Check it out.						*/
	c=chk_mov_A(ptab);

	return c;
      }

/*		This function gets assignments for members of an array
		attribute set.  The set of assignments are given
		as: A_name(member_name=___[,member_name1=_____[, ...]]).

		The rhs of member_name=____ can take the form
		=(value,value,...) or
		=value or
		='string' or
		="string" or
		=L_name or L_name[n] or L_name[I] or
		=A_name:member_name or
		=@ preceeding either of the last 2 choices.		*/

    int getA_member (str,tok,pta)

      char str[STRMAX+1]; /* the array attribute name - A_name		*/
      int *tok;	/* the token following that name			*/

      struct a_attr *pta;  /* pointer to the array attribute structure
				which has already been determined	*/
      {
	int i,c;
	int tokm;
	int tokmem;
	char strm[STRMAX+1];
	char mem[17];
	char *ptc,*pc;
	int typem;
	void **ptm,**ptma;
	char assig[4096];
	char memerr[100];

	int nv,nc;

/*		Error message when no memory is available.		*/

	sprintf(memerr,"Error - memory overflow (getA_member) - %s\n",str);

	do
	  {

/*			get a name and token				*/

	   if ((c=getsttk(strm,&tokm)) == EOF || c == 0 || tokm != '=')
	     {
	      if (c == EOF)
		{
		 err_warn(1,fperr,"Error - End-of-file in A_%s.\n",str);
		 return 0;
		}
	      if (tokm == *tok) return 1; /* End of member assignment.	*/
	      err_warn(1,fperr,
			"Error - not 'member_name=' instead - %s(%s%c...\n",
			 str,strm,tokm);
              return 0;
	     }
/*		Save the member name and token.				*/

	   strncpy (mem,strm,17); mem[16] = '\0';
	   tokmem=tokm;

/*		Find the member (address in ptm), its assignment equivalent
		(address in ptma), and its type ('c' or 'i' or 'f')	*/

	   if((c=findA_member(pta,mem,&typem,&ptm,&ptma))==(int)NULL)
	     {
	      err_warn(1,fperr,"Error - member %s(%s%c...) not found\n",
						str,mem,tokm);
	      return 0;
	     }
/*		Get the value to assign	or a delimiter (in tokm)	*/

	   if ((c=getsttk(strm,&tokm)) == EOF || tokm == EOF)
             return EOF;

/*		Is it of the form ="string" or ='string'?		*/

	   if (typem == 'c' && c == 0 && (tokm == '\'' || tokm == '\"'))
	     {/* Save the string */
	      i=0;
	      while ((c=getp(fpin,fpout)) != tokm)
		{
		 if (c == EOF)
		   {
		    if (i < 256) assig[i]='\0';
		    else assig[i-1]='\0';
		    err_warn(1,fperr,
			"Error - EOF in string for %s(%s=%c,%s{EOF}\n",
				str,mem,tokm,assig);
		    return 0;
		   }
		 if (i > 4094)
		   {
		    err_warn(1,fperr,
			"Error - A_%s(%s= string longer than 4096 chars.\n",
					str,mem);
		    return 0;
		   }
		 assig[i++]=c;
		}
	      assig[i]='\0';
	      tokm=0;

/*			Store the assignment.				*/

	      if ( (pc = (char *)malloc(strlen(assig)+1)) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	      strcpy(pc,assig);
	      if (*ptma != NULL) free ((char *)*ptma);
	      *ptma=(unsigned char *)pc;  /* stores the pointer	*/
	     }
	   else if ((typem == 'f' || typem == 'i') && c == 0 && tokm == '(')
	     {
	      nv=nc=0;
	      while ((c=getsttk(strm,&tokm)) != EOF &&
			tokm != EOF && c != 0 && (tokm == ',' || tokm == ')'))
	        {
		 if (!isnum(strm))
		   {
		    err_warn(1,fperr,"Error - need number for %s\n",mem);
		    return 0;
		   }
		 if (nc+2+c > 4095)
		   {
		    err_warn(1,fperr,
			"Error - A_%s(%s= string longer than 4096 chars.\n",
					str,mem);
		    return 0;
		   }
		 if (nv > 0) {assig[nc]=','; nc+=1;}
		 strcpy(&assig[nc],strm);
		 nc+=c;
		 nv+=1;
/*			test for end of sequence of values		*/
		 if (tokm == ')') break;
		}
	      assig[nc]='\0';

	      tokm=0;
/*			Store the string assignment.			*/
	      if ( (ptc = (char *)malloc(nc+1)) == NULL)
	        {err_warn(1,fperr,memerr); return 0;}
	      strcpy(ptc,assig);
	      if (*ptma != NULL) free (*ptma);
	      *ptma=(unsigned char *)ptc;  /* stores the pointer	*/
		    
	     }
/*		Is it of the form =string or value?			*/
	   else if (c > 0)
	     {
/*			Store the string assignment.			*/
	      if ( (ptc = (char *)malloc(strlen(strm)+1)) == NULL)
	        {err_warn(1,fperr,memerr); return 0;}
	      strcpy(ptc,strm);
	      if (*ptma != NULL) free (*ptma);
	      *ptma=(unsigned char *)ptc;  /* stores the pointer	*/
	     }
/*			There was an error in defining the string.	*/
	   else
	     {
	      err_warn(1,fperr,
			"Error - need string delimiters - %s(%s%c%s%c\n",
				str,mem,tokmem,strm,tokm);
	      return 0;
	     }

	   if (tokm == 0)
	     {

/*		It is necessary to find a trailing token or set token=0	*/

	      while (!istoken(c=getp(fpin,fpout)) && !isalnum(c))
		  if (c == EOF)
			return EOF;
	      if (isalnum(c))
		{
		 ungetp(c,fpin);
		 tokm=0;
		}
	      else tokm=c;
	     }
	  } while (tokm == ',');
 
       return 1;
      }

/*Search the array attribute member names for the name in
 str.  Return a pointer to the member pointer and a type 'f' or 'c'.	*/

    int findA_member(ptr,str,ch,pc,pa)

      struct a_attr *ptr;
      char str[];
      int *ch;
      void ***pa,***pc;

     {
	int i,j;

	for (i=0;i<38;i++)
	  {
	   if (cmpnbl(str,A_strg[i]) == 0)
	     {
	      *pc=(void *)(&(ptr->F)+i);
	      *pa=(void *)(&(ptr->aF)+i);
	      *ch='c';
	      return 1;
	     };
	  };
	for (i=0; i < 4; i++)
	  {
	   for (j=0; j<NDS; j++)
	     {
	      if (cmpnbl(str,A_intg[i][j]) == 0)
	        {
		 *pc=(void *)(&(ptr->XS[0])+i*NDS+j);
		 *pa=(void *)(&(ptr->aXS[0])+i*NDS+j);
	         *ch='i';
	         return 1;
	        };
	     };
	  }
	for (i=0; i < 5; i++)
	  {
	   for (j=0; j<NDS; j++)
	     {
	      if (cmpnbl(str,A_sflt[i][j]) == 0)
	        {
		 *pc=(void *)(&(ptr->XC[0])+i*NDS+j);
		 *pa=(void *)(&(ptr->aXC[0])+i*NDS+j);
	         *ch='f';
	         return 1;
	        };
	     };
	  };
	for (i=0; i < 6; i++)
	  {
	   for (j=0; j<NDS; j++)
	     {
	      if (cmpnbl(str,A_vflt[i][j]) == 0)
	        {
		 *pc=(void *)(&(ptr->XV[0])+i*NDS+j);
		 *pa=(void *)(&(ptr->aXV[0])+i*NDS+j);
	         *ch='f';
	         return 1;
		};
	     };
	  };
	return ((int)NULL);
      }


#include "cddrs.h"

      struct data_flo *compile_vcs_legacy (struct a_tab *ptab);


    int checkA_attr(struct a_tab *ptab)

      {
	int k;
	int lu;			/* logical unit for the dictionary	*/
        int lud;		/* logical unit for data		*/

      char *a_name;
      struct a_attr *pa;

	struct data_flo *pd;

	lu=10;
	lud=11;

	Seterr (0,IDRS_NOREPORT);

	pa=ptab->pA_attr;
	a_name=ptab->name;
	if (pa == NULL)
	  {
	   err_warn(1,fperr,"Error - NULL attributes for A_%s.\n",a_name);
	   return 0;
	  }

	pa->notok=1;  /* Assume the definition is not ok  */

	if (!moveA_assign(a_name,pa))
	  {
	   err_warn(1,fperr,"Error - in assignments for A_%s.\n",a_name);
	   Cllun(lu);
	   return 0;
	  }
/*		Is it a file or a function?				*/

	if (pa->f != NULL)
	  {

	   if ((pd=compile_vcs_legacy(ptab)) == NULL)
	     {
	      err_warn(1,fperr,"Error - compiling assignment for A_%s.\n",
									a_name);
	      killA_tmp();
	      return 0;
	     }

	   killA_tmp();
	   clear_data_flo(&pd);
	   pa->notok=0;
	  }
	else
	  {
	   if (pa->F == NULL)
	     {
	      err_warn(1,fperr,
			"Error no file and no assignment made for A_%s.\n",
					a_name);
	      return 0;
	     }
	   if ((k=Aslun(lu,pa->F,lud," ",IDRS_READ)) != IDRS_SUCCESS)
	     {
	      err_warn(1,fperr,"Error - file (%s) not available DRS # - %d\n",
		 pa->F,k);
	      return 0;
	     }

/*		determine if the naming strings are sufficient 		*/

	   if (!checkA_naming(lu,a_name,pa))
	     {
	      err_warn(1,fperr,"Error in naming strings for A_%s.\n",a_name);
	      Cllun(lu);
	      return 0;
	     }

/*		The VDB is now in place for further checking		*/

	   if (!fillA_dimen (lu,a_name,pa))
	     {
	      err_warn(1,fperr,"Error in dimensions for A_%s.\n",a_name);
	      Cllun(lu);
	      return 0;
	     }
	   Cllun(lu);
	   return 1;

	  }
	if (pa->lmask != NULL)
	  {
	   if ((pd=logicomp(ptab)) == NULL)
	     {
	      err_warn(1,fperr,
		"Error - compiling logical assignment for A_%s.\n",a_name);
	      return 0;
	     }

	   killA_tmp();
	   clear_data_flo(&pd);
	   pa->notok=0;
	  }
	return 1;
      }

/*	Fill the dimension values XN, XU, XS, XK, XC, XF, XL, XV, XB, XW

	Assumes the naming strings are ok.

	Return 0 - false if names and/or values don't work
	       1 - true if everything is or could be set up ok		*/

    int fillA_dimen (lu,a_name,pa)

      int lu;		/* logical unit for the dictionary		*/
      char a_name[];	/* name of the array attribute set		*/
      struct a_attr *pa;/* pointer to the array attribute set		*/

      {
	int i,j,k,c;
	int kf,kl,ki;
	int jf[NDS],jl[NDS];
	char dname[NDS][CW_MAX_NAME],dunits[NDS][41];
	char dsource[NDS][121],dtitle[NDS][81];
	char stemp[CW_MAX_NAME];
	char memerr[40];
	int nd,ndi,idv,ier;
	int j1,j2,k1,k2;
	int jj,jb,j1jb,je,jpjb,jmjb,jk;
	int mod_index_val;
	float v1,v2,vk,v;
	/*float df[NDS],dl[NDS];*/
	double df[NDS],dl[NDS];
	int dtyp[NDS],ndv[NDS],irev[NDS];
	float *pf,*pff,*pfb,*pfw,*pffb,*pffw,temp,dummy[1];
	double *pfd,*pffd,dtr,fv,fV;
	float cycle,dlon;
	float amnd,mnd;
	float ep;
	char tmp_units[NDS][CW_MAX_NAME];
	char tmp_source[NDS][CW_MAX_NAME],tmp_title[NDS][CW_MAX_NAME];
	

/*		Error message when no memory is available.		*/

	strcpy(memerr,"Error - (fillA_dimen) memory overflow.\n");

/*		Conversion from degrees to radians			*/

	dtr=3.1415926536/180.0;

/*		Set reverse indicators off				*/

	for (i=0; i<NDS; i++) irev[i]=0;

/*		Check for duplicate dimension names in the request	*/

	for (j=0; j < NDS && pa->XN[j] != NULL; j++)
	  {
	   for (i=0; i < j; i++)
	     {
	      if (cmpnbl(pa->XN[j],pa->XN[i]) == 0)
		{
		 err_warn(1,fperr,
			"Error - (A_%s) has duplicate dimension - (%s)\n",
			a_name,pa->XN[j]);
		 return 0;
		}
	     }
	  }

/*		This is the number of dimensions with names assigned.	*/

	ndi=j;

/*		Get the VDB or error return				*/

	Cluvdb();
	Setname (" ",pa->N," "," ",pa->TY);
        k=0;
	if ((c=Getdat(lu,dummy,k)) != IDRS_BADLEN && c != IDRS_SUCCESS)
	  {
	   err_warn(1,fperr,
		"Error - (fillA_dimen) getting data descriptors - %d\n",c);
	   return 0;
	  }

/*		Pick up the dimension information
		and save in array attributes.				*/

#ifdef CDCOMPAT
	cw_getnd(&nd);
#elif hpux
 	getnd(&nd);
#else
	getnd_(&nd);
#endif
	pa->ND=nd;

/*		No attributes for a dimension may be specified unless
		all preceeding and the current dimension name are
		given (i.e. its position in the order of dimensions
		must be specified).					*/

	for (i=ndi; i < NDS; i++)
	  {
	   ier=0;
	   if (pa->XN[i]!=NULL)
	     {free((char *)pa->XN[i]);	pa->XN[i]=NULL;
	      free((char *)pa->aXN[i]);	pa->aXN[i]=NULL;ier=1;
	     }
	   if (pa->XU[i]!=NULL)
	     {free((char *)pa->XU[i]);	pa->XU[i]=NULL;
	      free((char *)pa->aXU[i]);	pa->aXU[i]=NULL;ier=1;
	     }
	   if (pa->XS[i]!=NULL)
	     {free((char *)pa->XS[i]);	pa->XS[i]=NULL;
	      free((char *)pa->aXS[i]);	pa->aXS[i]=NULL;ier=1;
	     }
	   if (pa->XK[i]!=NULL)
	     {free((char *)pa->XK[i]);	pa->XK[i]=NULL;
	      free((char *)pa->aXK[i]);	pa->aXK[i]=NULL;ier=1;
	     }
	   if (pa->XC[i]!=NULL)
	     {free((char *)pa->XC[i]);	pa->XC[i]=NULL;
	      free((char *)pa->aXC[i]);	pa->aXC[i]=NULL;ier=1;
	     }
	   if (pa->XF[i]!=NULL)
	     {free((char *)pa->XF[i]);	pa->XF[i]=NULL;
	      free((char *)pa->aXF[i]);	pa->aXF[i]=NULL;ier=1;
	     }
	   if (pa->XL[i]!=NULL)
	     {free((char *)pa->XL[i]);	pa->XL[i]=NULL;
	      free((char *)pa->aXL[i]);	pa->aXL[i]=NULL;ier=1;
	     }
	   if (pa->XV[i]!=NULL)
	     {free((char *)pa->XV[i]);	pa->XV[i]=NULL;
	      free((char *)pa->aXV[i]);	pa->aXV[i]=NULL;ier=1;
	     }
	   if (pa->XB[i]!=NULL)
	     {free((char *)pa->XB[i]);	pa->XB[i]=NULL;
	      free((char *)pa->aXB[i]);	pa->aXB[i]=NULL;ier=1;
	     }
	   if (pa->XW[i]!=NULL)
	     {free((char *)pa->XW[i]);	pa->XW[i]=NULL;
	      free((char *)pa->aXW[i]);	pa->aXW[i]=NULL;ier=1;
	     }
	   if (pa->xn[i]!=NULL)
	     {free((char *)pa->xn[i]);	pa->xn[i]=NULL;
	      free((char *)pa->axn[i]);	pa->axn[i]=NULL;ier=1;
	     }
	   if (pa->xu[i]!=NULL)
	     {free((char *)pa->xu[i]);	pa->xu[i]=NULL;
	      free((char *)pa->axu[i]);	pa->axu[i]=NULL;ier=1;
	     }
	   if (pa->xs[i]!=NULL)
	     {free((char *)pa->xs[i]);	pa->xs[i]=NULL;
	      free((char *)pa->axs[i]);	pa->axs[i]=NULL;ier=1;
	     }
	   if (pa->xj[i]!=NULL)
	     {free((char *)pa->xj[i]);	pa->xj[i]=NULL;
	      free((char *)pa->axj[i]);	pa->axj[i]=NULL;ier=1;
	     }
	   if (pa->xf[i]!=NULL)
	     {free((char *)pa->xf[i]);	pa->xf[i]=NULL;
	      free((char *)pa->axf[i]);	pa->axf[i]=NULL;ier=1;
	     }
	   if (pa->xl[i]!=NULL)
	     {free((char *)pa->xl[i]);	pa->xl[i]=NULL;
	      free((char *)pa->axl[i]);	pa->axl[i]=NULL;ier=1;
	     }
	   if (pa->xv[i]!=NULL)
	     {free((char *)pa->xv[i]);	pa->xv[i]=NULL;
	      free((char *)pa->axv[i]);	pa->axv[i]=NULL;ier=1;
	     }
	   if (pa->xb[i]!=NULL)
	     {free((char *)pa->xb[i]);	pa->xb[i]=NULL;
	      free((char *)pa->axb[i]);	pa->axb[i]=NULL;ier=1;
	     }
	   if (pa->xw[i]!=NULL)
	     {free((char *)pa->xw[i]);	pa->xw[i]=NULL;
	      free((char *)pa->axw[i]);	pa->axw[i]=NULL;ier=1;
	     }
	   if (ier != 0)
	     {
	      err_warn(0,fperr,
		"Warning(fillA_dimen) - dimension %d attributes removed\n",i+1);
	      err_warn(0,fperr,
		"                       prior dimension names must be set.\n");
	     }
	  }

/*		Check whether there are more dimensions requested
		than available.  Multiple valued extra dimensions cannot
		be allowed, but single valued or non-assigned will
		be.							*/

	if (nd < ndi)
	  {
	   for (i=nd; i < ndi; i++)
	     if ((pa->xf[i]!=NULL && pa->xl[i]!=NULL && *pa->xf[i]!=*pa->xl[i])
		 || (pa->xs[i]!=NULL && *pa->xs[i]>1)   )
	       {
		err_warn(1,fperr,
		   "Error - (A_%s) has extra dimension, multi-valued - (%s)\n",
							a_name,pa->XN[i]);
		return 0;
	       }
	  }
/*			Set dimension names for getting dimensions.	*/

	Cluvdb();
	Setname (" ",pa->N," "," ",pa->TY);
	for (i=0; i < nd; i++)
	  {
	   if(i < ndi)Setdim (i+1,pa->XN[i]," ",0,1.e20,1.e20);
	   else Setdim (i+1," "," ",0,1.e20,1.e20);
	  }
	k=0;
	if ((c=Getdat(lu,dummy,k)) != IDRS_BADLEN && c != IDRS_SUCCESS)
	  {
	   err_warn(1,fperr,
		"Error - (fillA_dimen) getting data descriptors - %d\n",c);
	   return 0;
	  }

/*		Pick up dimension naming strings and limits		*/

	for (i=0; i < nd; i++)
	  {
	   /*GetedimD (i+1,tmp_source[i],dname[i],tmp_title[i],tmp_units[i],&dtyp[i],
			&ndv[i],&df[i],&dl[i]);*/
	   cw_getedimD (i+1,tmp_source[i],dname[i],tmp_title[i],tmp_units[i],&dtyp[i],
			&ndv[i],&df[i],&dl[i]);
	   strncpy(dsource[i],tmp_source[i],121); dsource[i][120] = '\0';
	   strncpy(dtitle[i],tmp_title[i],81); dtitle[i][80] = '\0';
	   strncpy(dunits[i],tmp_units[i],41); dunits[i][40] = '\0';

/*		Trim trailing blanks from the strings			*/

	   trimbl(dsource[i],121);
	   trimbl(dname[i],CW_MAX_NAME);
	   trimbl(dtitle[i],81);
	   trimbl(dunits[i],41);
	  }

/*		Check for dimension reordering				*/

/*	for (j=0; j < ((nd<ndi)?nd:ndi); j++)
	  {
*/
/*		Make sure the dimension exists, set an index (i) for it	*/

/*	   for (i=0; cmpnbl(pa->XN[j],dname[i]) != 0 && i < nd; i++);
*/
/*		Check was the dimension name found?			*/

/*	   if (i == nd)
	     {
	      err_warn(1,fperr,
		   "Error - (A_%s) dimension name doesn't exist - (%s)\n",
			a_name,pa->XN[j]);
	      return 0;
	     }
*/
/*		Swap dimension order if needed			*/

/*	   else if (i > j)
	     {
	      idv=dtyp[j];
	      dtyp[j]=dtyp[i];
	      dtyp[i]=idv;

	      temp=df[j];
	      df[j]=df[i];
	      df[i]=temp;

	      temp=dl[j];
	      dl[j]=dl[i];
	      dl[i]=temp;

	      idv=ndv[j];
	      ndv[j]=ndv[i];
	      ndv[i]=idv;

	      strncpy(stemp,dsource[j],121); stemp[120] = '\0';
	      strncpy(dsource[j],dsource[i],121); dsource[j][120] = '\0';
	      strncpy(dsource[i],stemp,121); dsource[i][120] = '\0';

	      strncpy(stemp,dname[j],CW_MAX_NAME); stemp[CW_MAX_NAME-1] = '\0';
	      strncpy(dname[j],dname[i],CW_MAX_NAME); dname[j][CW_MAX_NAME-1] = '\0';
	      strncpy(dname[i],stemp,CW_MAX_NAME); dname[i][CW_MAX_NAME-1] = '\0';

	      strncpy(stemp,dtitle[j],81); stemp[80] = '\0';
	      strncpy(dtitle[j],dtitle[i],81); dtitle[j][80] = '\0';
	      strncpy(dtitle[i],stemp,81); dtitle[i][80] = '\0';

	      strncpy(stemp,dunits[j],41); stemp[40] = '\0';
	      strncpy(dunits[j],dunits[i],41); dunits[j][40] = '\0';
	      strncpy(dunits[i],stemp,41); dunits[i][40] = '\0';
	     }
	  }
*/


/*		Set defaults and check for dimension direction		*/

	for (i=0; i < nd; i++)
	  {

/*		If values are given for (xv) make sure (xf) and (xl)
		are set to the end points.				*/

	   if (pa->xv[i] != NULL && pa->xs[i] != NULL)
	     {
	      if (pa->xl[i]==NULL &&
			(pa->xl[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	      *pa->xl[i]=*(pa->xv[i]+*pa->xs[i]-1);
	      if (pa->xf[i]==NULL &&
			(pa->xf[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	      *pa->xf[i]=*pa->xv[i];
	     }

/*		Size (xs) of xv must have been set previously
	       if xv was given.						*/

	   else if (pa->xs[i] == NULL && pa->xv[i] != NULL)
	     {
	      free((char *)pa->xv[i]);
	      pa->xv[i]=NULL;
	     }

/*		Set single valued limit to same for both l.c. limits	*/

	   if (pa->xf[i] == NULL || pa->xl[i] == NULL)
	     {
	      if (pa->xf[i] != NULL)
		{ 
	         if ((pa->xl[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
		 *(pa->xl[i])=*(pa->xf[i]);
		}
	      else if (pa->xl[i] != NULL)
		{
	         if ((pa->xf[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
		 *(pa->xf[i])=*(pa->xl[i]);
		}
	     }

/*		Set single valued limit to same for both U.C. limits	*/

	   if (pa->XF[i] == NULL || pa->XL[i] == NULL)
	     {
	      if (pa->XF[i] != NULL)
		{ 
	         if ((pa->XL[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
		 *(pa->XL[i])=*(pa->XF[i]);
		}
	      else if (pa->XL[i] != NULL)
		{
	         if ((pa->XF[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
		 *(pa->XF[i])=*(pa->XL[i]);
		}
	     }


/*		Latitude limits by default are -90 to +90		*/

	   if (doexist("latitude",dname[i]) && nd > 1 && i <= 1 &&
			doexist("longitude",dname[abs(i-1)]) )
	     {
	      if (pa->xf[i] == NULL && pa->xl[i] == NULL)
		{
		 if (pa->aXN[i] == NULL)
		   {
	            if ((pa->aXN[i]=(char *)malloc(CW_MAX_NAME)) == NULL)
		      {err_warn(1,fperr,memerr); return 0;}
		    strcpy(pa->aXN[i],dname[i]);
		   }
	         if (df[i] > dl[i]) irev[i]=1;/* indicate direction reversal */
	         temp=(df[i] <= dl[i]) ? df[i] : dl[i];
	         dl[i]=(df[i] > dl[i]) ? df[i] : dl[i];
	         df[i]=temp;
	         if ((pa->xf[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	         *pa->xf[i]=-90.;
	         if ((pa->axf[i]=(char *)malloc(20)) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	         strcpy(pa->axf[i],"-90.");
	         if ((pa->xl[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	         *pa->xl[i]=90.;
	         if ((pa->axl[i]=(char *)malloc(20)) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	         strcpy(pa->axl[i],"90.");
		}
	     }

/*		Longitude limits by default are -180 to +180
		and wrapped if needed.					*/

	   if(doexist("longitude",dname[i]) && nd > 1 && i <= 1 &&
			doexist("latitude",dname[abs(i-1)]) )
	     {
	      dlon=(ndv[i] > 1)?fabs(dl[i]-df[i])/(ndv[i]-1):0.0;
	      cycle=fabs(dl[i]-df[i])+dlon;
/*DNW-1/21/97: (fabs(cycle-360.) < .001*dlon) prevents canvas zooming
	      if (pa->xf[i]==NULL && pa->xl[i]==NULL && pa->XK[i]==NULL &&
		pa->XF[i]==NULL && pa->XL[i]==NULL && pa->XC[i]==NULL &&
				fabs(cycle-360.) < .001*dlon)
*/
	      if (pa->xf[i]==NULL && pa->xl[i]==NULL && pa->XK[i]==NULL &&
		pa->XF[i]==NULL && pa->XL[i]==NULL && pa->XC[i]==NULL)
		{
		 if (pa->aXN[i] == NULL)
		   {
	            if ((pa->aXN[i]=(char *)malloc(20)) == NULL)
		      {err_warn(1,fperr,memerr); return 0;}
		    strcpy(pa->aXN[i],dname[i]);
		   }
	         if (df[i] > dl[i]) irev[i]=1;/* indicate direction reversal */
	         temp=(df[i] <= dl[i]) ? df[i] : dl[i];
	         dl[i]=(df[i] > dl[i]) ? df[i] : dl[i];
	         df[i]=temp;
		 dlon=(ndv[i] > 1)?(dl[i]-df[i])/(ndv[i]-1):0.0;
	         if ((pa->xf[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	         *pa->xf[i]=-180.;
	         if ((pa->axf[i]=(char *)malloc(20)) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	         strcpy(pa->axf[i],"-180.");
		 
	         if ((pa->xl[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	         *pa->xl[i]=180.;
	         if ((pa->axl[i]=(char *)malloc(20)) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	         strcpy(pa->axl[i],"180.");

		 if ((pa->XF[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
		 if ((pa->aXF[i]=(char *)malloc(20)) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
		 if ((pa->XL[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
		 if ((pa->aXL[i]=(char *)malloc(20)) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
		 if (df[i] < dl[i])
		   {
		    *pa->XF[i]=df[i];
		    *pa->XL[i]=dl[i];
		    sprintf(pa->aXF[i],"%f",df[i]);
		    sprintf(pa->aXL[i],"%f",dl[i]);
		   }
	         else
		   {
		    *pa->XF[i]=dl[i];
		    *pa->XL[i]=df[i];
		    sprintf(pa->aXF[i],"%f",dl[i]);
		    sprintf(pa->aXL[i],"%f",df[i]);
		   }

	         if ((pa->XK[i]=(int *)malloc(2*sizeof(int))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	         if ((pa->aXK[i]=(char *)malloc(20)) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	         *pa->XK[i]=0;
	         *(pa->XK[i]+1)=0;
		 while(*pa->XF[i]+*pa->XK[i]*360.0 > -180.0) *pa->XK[i]-=1;
		 while(*pa->XL[i]+*(pa->XK[i]+1)*360.0<180.0) *(pa->XK[i]+1)+=1;
		 sprintf(pa->aXK[i],"%d,%d",*pa->XK[i],*(pa->XK[i]+1));

	         if ((pa->XC[i]=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	         *pa->XC[i]=360.;
	         if ((pa->aXC[i]=(char *)malloc(20)) == NULL)
		   {err_warn(1,fperr,memerr); return 0;}
		 strcpy(pa->aXC[i],"360.");

		}
	     }

/*		Set first and last values if not given.
		XF and XL are only used if wrap is requested, but they
		are set to the first and last existing values otherwise.*/

	   if (pa->XF[i] == NULL && pa->XL[i] == NULL)
	     {
	      if ((pa->XF[i]=(float *)malloc(sizeof(float))) == NULL)
		{err_warn(1,fperr,memerr); return 0;}
	      *pa->XF[i]=df[i];
	      if ((pa->XL[i]=(float *)malloc(sizeof(float))) == NULL)
		{err_warn(1,fperr,memerr); return 0;}
	      *pa->XL[i]=dl[i];
	     }
	   if (pa->xf[i] == NULL && pa->xl[i] == NULL)
	     {
	      if ((pa->xf[i]=(float *)malloc(sizeof(float))) == NULL)
		{err_warn(1,fperr,memerr); return 0;}
	      *pa->xf[i]=*pa->XF[i];
	      if ((pa->xl[i]=(float *)malloc(sizeof(float))) == NULL)
		{err_warn(1,fperr,memerr); return 0;}
	      *pa->xl[i]=*pa->XL[i];
	     }
/*		Reverse directions if xf and xl specify differently	*/

	   if ( (*pa->xl[i]-*pa->xf[i])*(dl[i]-df[i]) < 0.0)
	     {
	      temp=df[i]; df[i]=dl[i]; dl[i]=temp;
	      irev[i]=1-irev[i];
	     }

	   if (pa->XK[i] == NULL)
	     {
	      if ((pa->XK[i]=(int *)malloc(2*sizeof(int))) == NULL)
		{err_warn(1,fperr,memerr); return 0;}
	      *pa->XK[i]=0;
	      *(pa->XK[i]+1)=0;
	     }
	   if (pa->XC[i] == NULL)
	     {
	      if ((pa->XC[i]=(float *)malloc(sizeof(float))) == NULL)
		{err_warn(1,fperr,memerr); return 0;}
	      *pa->XC[i]=0.0;
	     }

/*		Reverse XF and XL (and index positions) if their
		direction doesn't agree with the actual dimension
		direction						*/

	   if ( (*pa->XL[i] - *pa->XF[i])*(dl[i] - df[i]) < 0.0)
	     {
	      temp=*pa->XF[i];
	      *pa->XF[i]=*pa->XL[i];
	      *pa->XL[i]=temp;
	      if (*pa->XF[i] < *pa->XL[i])
		{
		 if (pa->XK[i][0] > pa->XK[i][1])
		   {
		    temp=pa->XK[i][0];
		    pa->XK[i][0]=pa->XK[i][1];
		    pa->XK[i][1]=temp;
		   }
		}
	      else if (*pa->XF[i] > *pa->XL[i])
		{
		 if (pa->XK[i][0] < pa->XK[i][1])
		   {
		    temp=pa->XK[i][0];
		    pa->XK[i][0]=pa->XK[i][1];
		    pa->XK[i][1]=temp;
		   }
		}
	     }

/*		Save the number of dimension values			*/

	   if (pa->XS[i] == NULL &&
	         (pa->XS[i]=(int *)malloc(sizeof(int))) == NULL)
	     {err_warn(1,fperr,memerr); return 0;}
	   *pa->XS[i]=ndv[i];
	  }

/*		Set VDB naming information				*/

	Cluvdb();
	Setname (" ",pa->N," "," "," ");

/*		Set the VDB dimensions					*/

	/*DNWfor (i=0; i < nd; i++)
	  {
	   if (dtyp[i] == IDRS_EQUALLY_SPACED)
	      Setdim (i+1,dname[i]," ",ndv[i],1.0e20,1.0e20);
	   else
	      Setvdim (i+1," ",dname[i]," "," ",1.0e20,1.0e20);
	  }*/
	for (i=0; i < nd; i++)
	  {
	   if (dtyp[i] == IDRS_EQUALLY_SPACED)
	      Setdim (i+1,dname[i]," ",ndv[i],df[i],dl[i]);
	   else
	      Setvdim (i+1," ",dname[i]," "," ",df[i],dl[i]);
	  }

/*		Do a fake retrieval to get the VDB			*/

	if ((c=Getdat(10,df,0)) != IDRS_BADLEN && c != IDRS_SUCCESS)
	  {
	   err_warn(1,fperr,"Error getting data VDB - %d\n",c);
	   return 0;
	  }

/*		Get dimension attributes from the VDB and save them	*/

	for (i=0; i < nd; i++)
	  {
	   if (pa->XV[i] != NULL) free((char *)pa->XV[i]);
	   if ((pa->XV[i]=(float *)malloc(ndv[i]*sizeof(float))) == NULL)
	     {err_warn(1,fperr,memerr); return 0;}

	   if ((c=Getcdim(i+1,tmp_source[i],dname[i],tmp_title[i],tmp_units[i],&dtyp[i],
			ndv[i],pa->XV[i],&idv)) != IDRS_SUCCESS)
		err_warn(1,fperr,"Error - getting dimensions for - %d %s\n",
					c,dname[i]);
	   strncpy(dsource[i],tmp_source[i],121); dsource[i][120] = '\0';
	   strncpy(dtitle[i],tmp_title[i],81); dtitle[i][80] = '\0';
	   strncpy(dunits[i],tmp_units[i],41); dunits[i][40] = '\0';
	   
	   trimbl(dname[i],CW_MAX_NAME);


/*		Store the dimension name and units			*/

	   if ((pa->XN[i]=repstr(pa->XN[i],dname[i])) == NULL) return 0;
	   if ((pa->XU[i]=repstr(pa->XU[i],dunits[i])) == NULL) return 0;
	   if (pa->axn[i] == NULL)
	     {if ((pa->xn[i]=repstr(pa->xn[i],dname[i])) == NULL) return 0;}
	   else
	     if ((pa->xn[i]=repstr(pa->xn[i],pa->axn[i])) == NULL) return 0;
	   if (pa->axu[i] == NULL)
	     {if ((pa->xu[i]=repstr(pa->xu[i],dunits[i])) == NULL) return 0;}
	   else
	     if ((pa->xu[i]=repstr(pa->xu[i],pa->axu[i])) == NULL) return 0;


/*		Set XF and XL to the nearest value and save the index	*/

	   amnd=1.e20;
	   for (pf=pa->XV[i],j=0; j < *pa->XS[i]; j++, pf++)
	     {
	      mnd=fabs(*pf-*pa->XF[i]);
	      if (mnd < amnd) {pff=pf; jf[i]=j, amnd=mnd;}
	     }
	   *pa->XF[i]=*pff;

	   amnd=1.e20;
	   for (pf=pa->XV[i],j=0; j < *pa->XS[i]; j++, pf++)
	     {
	      mnd=fabs(*pf-*pa->XL[i]);
	      if (mnd < amnd) {pff=pf; jl[i]=j, amnd=mnd;}
	     }
	   *pa->XL[i]=*pff;
	  }

/*		Pick up or set grid box limits (XB) and weights (XW)	*/

	for (i=0; i < nd; i++)
	  {
	   if (pa->XB[i] != NULL) {free((char *)pa->XB[i]);pa->XB[i]=NULL;}
	   if (pa->XW[i] != NULL) {free((char *)pa->XW[i]);pa->XW[i]=NULL;}
	   pfd=NULL;
	   pffd=NULL;
	   if (ndv[i] > 1)
	     {

	      if (doexist("latitude",dname[i]))

	/*&&dtyp[i]==IDRS_UNEQUALLY_SPACED)*/
		{
		 if ((pfd=(double *)malloc((ndv[i]+1)*sizeof(double)))!=NULL
					 &&
		      (pffd=(double *)malloc(ndv[i]*sizeof(double))) != NULL
					 &&
		   (pa->XB[i]=(float *)malloc((ndv[i]+1)*sizeof(float)))!=NULL
					 &&
		   (pa->XW[i]=(float *)malloc(ndv[i]*sizeof(float)))!=NULL  )
		   {

/*			Check whether it is the CSU grid.		*/

		    if (fabs(fabs(pa->XV[i][ndv[i]-1]-pa->XV[i][0])/(ndv[i]-1)-
			4.0) < 0.001 && fabs(fabs(pa->XV[i][0])-86.0) < 0.001 &&
			fabs(fabs(pa->XV[i][ndv[i]-1])-86.0) < 0.001 )
		      {
		       if (pa->XV[i][0] < pa->XV[i][1])
			 {
			  pa->XB[i][0]=-90.0;
			  for (j=1;j<ndv[i];j++)
			    {
			     pa->XB[i][j]=pa->XV[i][j-1]+2.0;
			    }
			  pa->XB[i][ndv[i]]=90.0;
			 }
		       else
			 {
			  pa->XB[i][0]=90.0;
			  for (j=1;j<ndv[i];j++)
			    {
			     pa->XB[i][j]=pa->XV[i][j-1]-2.0;
			    }
			  pa->XB[i][ndv[i]]=-90.0;
			 }
		       for (j=0;j<ndv[i];j++)
			 {
			  pa->XW[i][j]=
			    fabs(sin(pa->XB[i][j+1]*dtr)-sin(pa->XB[i][j]*dtr));
			 }
		       free((char *)pfd);
		       free((char *)pffd);
		       pfd=pffd=NULL;
		       continue;
		      }
/*			Check whether it is LMD grid or not and set bounds
			if it is.					*/

		    if (lmd_grid(pa->XV[i],pa->XB[i],pa->XW[i],*pa->XS[i]) != 0)
		      {
		       free((char *)pfd);
		       free((char *)pffd);
		       pfd=pffd=NULL;
		       continue;
		      }

/*			Compute gaussian latitudes and weights -
			symmetric about the equator and see if they
			match those given here.				*/

#ifdef hpux
 		    gauaw(pfd,pffd,&ndv[i]);
#elif aix
                    gauaw(pfd,pffd,&ndv[i]);
#elif cray
                    GAUAW(pfd,pffd,&ndv[i]);
#else
		    gauaw_(pfd,pffd,&ndv[i]);
#endif

/*			This works for -ve to +ve or +ve to -ve latitudes*/

		    for (j=0; j < ndv[i]; j++)
		      {
		       fv=fabs(asin(pfd[j])/dtr);
		       fV=fabs((double)pa->XV[i][j]);
		       if (fabs(fv-fV) > (double) 0.001) break;
		      }
		    if (j == ndv[i])
		      {
		       pfd[0]=-90.;
		       if (pa->XV[i][0] > 0) pfd[0]=90.0;

		       for (k=1; k <= ndv[i]/2; k++)
			 {
			  fv=sin(dtr*pfd[k-1]);
			  if (pfd[0] < 0)
			     pfd[k]=asin( fv+pffd[k-1] )/dtr;
			  else
			     pfd[k]=asin( fv-pffd[k-1] )/dtr;

			  pfd[ndv[i]-k]=-pfd[k];
			 }
		       pfd[ndv[i]]=-pfd[0];
		       if (ndv[i]%2 == 0) pfd[ndv[i]/2]=0.0;
		       if (*pa->XV[i] > 0.0)
			 for (k=0; k <= ndv[i]; k++)
			   pfd[k]=-pfd[k];
		       for (j=0;j<ndv[i];j++)
			 {
		          pa->XB[i][j]=pfd[j];
		          pa->XW[i][j]=pffd[j];
			 }
		       pa->XB[i][ndv[i]]=pfd[ndv[i]];
		       free((char *)pfd);
		       free((char *)pffd);
		       pfd=pffd=NULL;
		       continue;
		      }
		    else
		      {
			free((char *)pfd);
			free((char *)pffd);
			pfd=NULL;
			pffd=NULL;
		      }
		   }
		 else
		   {
		    if (pfd != NULL) free((char *)pfd);
		    if (pffd != NULL) free((char *)pffd);
		    err_warn(1,fperr,memerr);
		    return 0;
		   }
		}
	     }

/*		Try to get the bounds					*/

	   Cluvdb();
	   for (stemp[0]='b',k=1;(stemp[k]=dname[i][k-1]) != '\0'; k++);
	   stemp[k++]='b';
	   stemp[k]='\0';
	   if ((pa->XB[i]=(float *)malloc((ndv[i]+1)*sizeof(float))) == NULL)
		{err_warn(1,fperr,memerr); return 0;}
	   Setname (" ",stemp," "," "," ");
	   /*Setdim (1," "," ",ndv[i]+1,irev[i]*20000.,(1-irev[i])*20000.); DNW - Jan. 21, 2000 */
	   if (irev[i])
	      Setdim (1," "," ",ndv[i],dl[i],df[i]);
	   else
	      Setdim (1," "," ",ndv[i],df[i],dl[i]);

	   if (Getdat(lu,pa->XB[i],(ndv[i]+1)*sizeof(float)) != IDRS_SUCCESS)
	     {

/*		pfd may contain the bounds if it was latitude,
		so just move them.					*/

	      if (pfd != NULL)
	      for (k=0; k <= ndv[i]; k++) *(pa->XB[i]+k)=*(pfd+k);

/*		This is the standard computation of bounds, with kinks
		for "plev" and "latitude".				*/

	      else
		{
		 if (ndv[i] > 1) dlon=(dl[i] - df[i])/(ndv[i]-1);
		 else if (*pa->XC[i] > 0.0) dlon=*pa->XC[i];
		 else dlon=1.0;
		 pf=pa->XB[i];
		 *(pf++)=*pa->XV[i]-0.5*dlon;
		 for(k=0;k<ndv[i]-1;k++)
				*(pf++)=0.5*(*(pa->XV[i]+k+1)+*(pa->XV[i]+k));

		 /* DNW - Calculate the last and first boundary */
		 if (k != 0) {
		    *pf=pa->XV[i][k]-((pa->XV[i][k-1]-pa->XV[i][k])*0.5);
		    pa->XB[i][0]=pa->XV[i][0]+((pa->XV[i][0]-pa->XV[i][1])*0.5);
	         } else {
		    *pf=pa->XV[i][k];
		    pa->XB[i][0]=pa->XV[i][0]-1.0;
		    pa->XB[i][1]=pa->XV[i][0]+1.0;
	         }
		 
		 /**pf=dl[i]+0.5*dlon;*/
		 if (doexist("plev",dname[i]))
		   {
/* DNW - 8/3/00 - No need to set the first boundary condition for the 
		  Pressure level. This causes a bug in the code when 
		  you have pressure levels ranging from 1000 to 100000. 
		    *pf=(*pf > 1013.)? 1013. : *pf;
		    *pf=(*pf < 0.)? 0. : *pf;
		    pf=pa->XB[i];
		    *pf=(*pf > 1013.)? 1013. : *pf;
		    *pf=(*pf < 0.)? 0. : *pf;
*/
		   }
		 else if (doexist("latitude",dname[i]))
		   {
		    *pf=(*pf > 90.)? 90. : *pf;
		    *pf=(*pf < -90.)? -90. : *pf;
		    pf=pa->XB[i];
		    *pf=(*pf > 90.)? 90. : *pf;
		    *pf=(*pf < -90.)? -90. : *pf;
		   }
		}
	     }

/*		Try to get the weights					*/

	      Cluvdb();
	      for (stemp[0]='w',k=1;(stemp[k]=dname[i][k-1]) != '\0'; k++);
	      stemp[k++]='w';
	      stemp[k]='\0';
	      if((pa->XW[i]=(float *)malloc(ndv[i]*sizeof(float)))==NULL)
		{err_warn(1,fperr,memerr); return 0;}
	      Setname (" ",stemp," "," "," ");
	      /*Setdim (1," "," ",ndv[i],irev[i]*20000.,(1-irev[i])*20000.); DNW - Jan. 21, 2000 */
	      if (irev[i])
	         Setdim (1," "," ",ndv[i],pa->XL[i][0],pa->XF[i][0]);
	      else
	         Setdim (1," "," ",ndv[i],pa->XF[i][0],pa->XL[i][0]);
	      if (Getdat(lu,pa->XW[i],ndv[i]*sizeof(float)) != IDRS_SUCCESS)
	        {

/*		pffd may contain the weights if it was latitude,
		so just move them.					*/

		 if (pffd != NULL)
		   {
		    for (k=0; k<ndv[i]; k++) *(pa->XW[i]+k)=*(pffd+k);
		   }

/*		This is the standard computation of weights, with a kink
		for "latitude".						*/

		 else
		   {
		    pf=pa->XW[i];
		    if (doexist("latitude",dname[i]))
		      {
		       for (k=0; k<ndv[i]; k++,pf++)
		         {if((*pf=0.5*(sin(*(pa->XB[i]+k+1)*dtr)-
			      sin(*(pa->XB[i]+k)*dtr)))<0.)
			    *pf=-*pf;
		         }
		      }
		    else
		       for (k=0; k < ndv[i]; k++)
		          *(pf++)=fabs(*(pa->XB[i]+k+1)-*(pa->XB[i]+k));
		   }
		}

/*		Free the space used by gauaw, if it was used		*/

	   if (pfd != NULL) {free((char *)pfd); pfd=NULL;}
	   if (pffd != NULL) {free((char *)pffd); pffd=NULL;}
	  }
/*		Set grid values (xv), limits (xb), and weights (xw)	*/

	for (i=0; i < nd; i++)
	  {
/*			Compute an epsilon for limit tolerance.		*/


	   if (*pa->XS[i] == 1)
	     {
	      ep=fabs(*pa->XV[i])*1.e-5;
	      ep=(ep>1.e-6)?ep:1.e-6;
	     }
	   else
	     {
	      ep=fabs(*pa->XV[i]-*(pa->XV[i]+*pa->XS[i]-1))/(*pa->XS[i])*0.0001;
	     }

	   if (pa->xj[i]==NULL)
	     {
	      if ((pa->xj[i]=(int *)malloc(sizeof(int)))==NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	      *pa->xj[i]=1;
	     }
	   if (pa->xv[i] == NULL || pa->xs[i] == NULL || *pa->xs[i] <= 0)
	     {
	      if (pa->xb[i] != NULL) {free((char *)pa->xb[i]);pa->xb[i]=NULL;}
	      if (pa->xw[i] != NULL) {free((char *)pa->xw[i]);pa->xw[i]=NULL;}
	      if (pa->xv[i] != NULL) {free((char *)pa->xv[i]);pa->xv[i]=NULL;}
	      if (pa->xs[i]==NULL && 
			   (pa->xs[i]=(int *)malloc(sizeof(int)))==NULL)
		{err_warn(1,fperr,memerr); return 0;}

	      if (*pa->xj[i] < 0) *pa->xj[i]=1;

	      if ( (*(pa->XK[i])==0 && *(pa->XK[i]+1)==0) || *(pa->XC[i])<=0.0)
		{
		 jf[i]=0;
		 jl[i]=*pa->XS[i]-1;
		 *pa->XK[i]=0;
		 *(pa->XK[i]+1)=0;
/*		 *pa->XC[i]=0.0;					*/
		}

/*		Set the k (first and last and increment)
		for the k (or wrap) loop				*/

	      kf=*pa->XK[i];
	      kl=*(pa->XK[i]+1);
	      ki=(kf <= kl) ? 1: -1;

/*		Set j and k index value defaults			*/

	      j2=j1=-1;
	      k2=k1=0;

/*		loop over the k (or wrap) index.			*/

	      for (k=kf; (k-kf)*(k-kl) <= 0; k+=ki)
		{

/*		loop over the wrapped set of values to find the index of
		the included (between xf and xl) value. 		*/

		 vk=k*(*pa->XC[i]);

		 pf=pa->XV[i]+jf[i];
		 for (j=jf[i];(j1 == -1 || j2 == -1) && j <= jl[i]; pf++,j++)
		   {
		    v1=v2=*pf+vk;
		    if (j < jl[i]) v2=*(pf+1)+vk;
		    else if (k!=kl) v2=*(pa->XV[i]+jf[i])+(k+ki)*(*pa->XC[i]);

/*				Check tolerances.			*/

		    if (fabs(*pa->xf[i]-v1) < ep) *pa->xf[i]=v1;
		    if (fabs(*pa->xf[i]-v2) < ep) *pa->xf[i]=v2;
		    if (fabs(*pa->xl[i]-v1) < ep) *pa->xl[i]=v1;
		    if (fabs(*pa->xl[i]-v2) < ep) *pa->xl[i]=v2;

		    if (j1 == -1 && (*pa->xf[i]-v1)*(*pa->xf[i]-v2) <= 0.0)
		      {/* use the interior value */
		       j1=j; k1=k;

		       if (fabs(*pa->xf[i] - v1) > ep)
			 {
		          if (j < jl[i]) j1+=1;
		          else if (k != kl) {j1=jf[i]; k1+=ki;}
			 }

		      }

		    if (j2 == -1 && (*pa->xl[i]-v1)*(*pa->xl[i]-v2) <= 0.0)
		      {/*  use the interior value */
		       j2=j; k2=k;
/*		Take care of the instance when the limits xf and xl
		are between grid values and possibly equal.		*/

		       if (k1 == k2 && j2 < j1)
			 {
			  v=0.5*(*pa->xf[i]+*pa->xl[i]);
			  if (fabs(v-v1) < fabs(v-v2))
			     j2=j1=j;
			  else if (j < jl[i]) j2=j1=j+1;
			  else if (k < kl) { j1=j2=jf[i]; k1=k2=k+ki;}
			  else {j1=j2=j; k1=k2=k;}
			 }
		       else if (fabs(*pa->xl[i] - v2) < ep)
			 {
		          if (j < jl[i]) j2+=1;
		          else if (k != kl) {j2=jf[i]; k2+=ki;}
			 }
		      }
		   }
		}
	      v1=*(pa->XV[i]+jf[i])+kf*(*pa->XC[i]);
	      v2=*(pa->XV[i]+jl[i])+kl*(*pa->XC[i]);

/* 
 * D.N.W. 2/5/96, if the dimension value is greater than range, then 
 *		  use the modulus to find a value within the dimension
 *                range.                                                
 */
	      if (j1 == -1 && j2 == -1) {
		 if ( (*pa->xf[i]-v1)*(*pa->xl[i]-v1) > 0.0) {
		    if (fabs(*pa->xf[i]-v1)>fabs(*pa->xf[i]-v2)) {
                       mod_index_val = (int)(*pa->xf[i]) % (int)v2;
		       if (*pa->xf[i] == 0) mod_index_val = v2;
                       j2 = j1 = mod_index_val - 1;
                    }
                 }
	      }

/*		If neither was found inside and both are on the same
		side of the first value then the requested limits are
		outside the data.					*/

	      if (j1 == -1 && j2 == -1)
		{
		 if ( (*pa->xf[i]-v1)*(*pa->xl[i]-v1) > 0.0)
		   {
                    err_warn(1,fperr,
                     "Error - limits of A_%s(%s) outside the range of nodes.\n",                                    a_name,dname[i]);
		    if (fabs(*pa->xf[i]-v1)>fabs(*pa->xf[i]-v2))
		      {
                       *pa->xf[i]=*pa->xl[i]=v1;
		       j1=j2=jf[i];
		       k1=k2=kf;
		      }
		    else
		      {
		       *pa->xf[i]=*pa->xl[i]=v2;
		       j1=j2=jl[i];
		       k1=k2=kl;
		      }
		    err_warn(1,fperr,
		     "        first and last set to nearest node value (%g).\n",
					*pa->xf[i]);
		    if (pa->axf[i] != NULL)
		      {
		       free((char *)pa->axf[i]);
		       pa->axf[i]=NULL;
		      }
		    if (pa->axl[i] != NULL)
		      {
		       free((char *)pa->axl[i]);
		       pa->axl[i]=NULL;
		      }
/*		    return 0;						*/
		   }
		 else {j1=jf[i];k1=kf; j2=jl[i];k2=kl;}
		}
	      else
		{
		 if (j1 == -1) {j1=jf[i];k1=kf;}
		 if (j2 == -1) {j2=jl[i];k2=kl;}
		}

	      *pa->xs[i]=c=j2+abs(k2-k1)*(jl[i]-jf[i]+1)-j1+1;
	      if (*pa->xj[i] > 1) *pa->xs[i]=(c+*pa->xj[i]-1)/(*pa->xj[i]);

	      if ((pf=pa->xv[i]=(float *)malloc(c*sizeof(float)))==NULL)
		{err_warn(1,fperr,memerr); return 0;}
	      if ((pfb=pa->xb[i]=(float *)malloc((c+1)*sizeof(float)))==NULL)
		{err_warn(1,fperr,memerr); return 0;}
	      if ((pfw=pa->xw[i]=(float *)malloc(c*sizeof(float)))==NULL)
		{err_warn(1,fperr,memerr); return 0;}

/*		Loop over the k (or wrap) index.			*/

	      jj=(*pa->xj[i]<1)?1:*pa->xj[i];
	      jb=(jj-1)/2;
	      j1jb=j1-jb;
	      if (j1jb >= jf[i])
		{
		 *pfb=*(pa->XB[i]+j1jb)+k1*(*pa->XC[i]);
		}
	      else if ((*pa->XK[i]!=0 || *(pa->XK[i]+1)!=0) && *pa->XC[i]!=0)
		{
		 *pfb=*(pa->XB[i]+jl[i]-(jf[i]-j1jb-1))+(k1-ki)*(*pa->XC[i]);
		}
	      else {*pfb=*(pa->XB[i]+jf[i])+k1*(*pa->XC[i]);}

	      jk=0;

	      for (k=k1; (k-k1)*(k-k2) <= 0; k+=ki)
		{

/*		Loop over the wrapped set of values to fill in values.	*/

		 vk=k*(*pa->XC[i]);

		 if (k == k1) j=j1;
		 else j=jf[i];
		 if (k == k2) je=j2;
		 else je=jl[i];

		 pff=pa->XV[i]+j;
		 pffb=pa->XB[i]+j;
		 pffw=pa->XW[i]+j;

		 for ( ;j<=je;j++,pff++,pffb++,pffw++,jk++)
		   {
		    if (jk%jj == 0)
		      {
		       *pf=*pff+vk; /* store xv values */
		       *pfw=fabs(*pffw);  /* store xw (weight) values */
	    	       jpjb=j-jj+1+jb;
		       jmjb=j+jj-jb;

/*			Compute and store bounds			*/

		       if (jpjb >= jf[i])
		         {
			 *pfb=0.5*(*pfb+*(pa->XB[i]+jpjb)+k*(*pa->XC[i]));
		         }
		      else if((*pa->XK[i]!=0||*(pa->XK[i]+1)!=0)&&*pa->XC[i]!=0)
		         {
			  *pfb=0.5*(*pfb+*(pa->XB[i]+jl[i]-(jf[i]-jpjb-1))+
						(k-ki)*(*pa->XC[i]));
		         }
		       else
		         {
			 *pfb=0.5*(*pfb+*(pa->XB[i]+jf[i])+(k-ki)*(*pa->XC[i]));
		         }
		       if (jmjb <= jl[i]+1)
			  *(pfb+1)=*(pa->XB[i]+jmjb)+k*(*pa->XC[i]);
		       else if ((*pa->XK[i]!=0 || *(pa->XK[i]+1)!=0) &&
								 *pa->XC[i]!=0)
			  *(pfb+1)=*(pa->XB[i]+jf[i]+jmjb-jl[i]-1)+
					(k+ki)*(*pa->XC[i]);
		       else *(pfb+1)=*(pa->XB[i]+jl[i]+1)+k*(*pa->XC[i]);

		       pf++;
		       pfw++;
		       pfb++;
		      }
		   }
	        }
	     }
 
/*		If grid values (xv) are given, assign/check bounds and
		weights.						*/

	   else
	     {
	      if (pa->xb[i] == NULL)
	        {
		 idv=*pa->xs[i];
	         if ((pfb=pa->xb[i]=
			(float *)malloc((idv+1)*sizeof(float)))==NULL)
		   {err_warn(1,fperr,memerr); return 0;}
		
/*		This is the standard computation of bounds, with kinks
		for "plev" and "latitude".				*/

		 if (idv > 1)
		   {
/*				Compute increment for random selection.	*/

		    if (*pa->xj[i] == 0)
			dlon=(pa->XV[i][*pa->XS[i]-1]-pa->XV[i][0])/
								(*pa->XS[i]-1);
/*				Compute increment for other.		*/
		    else
			dlon=(pa->xv[i][idv-1]-pa->xv[i][0])/(idv-1);

		   }
		 else if (*pa->XC[i] > 0.0) dlon=*pa->XC[i];
	 	 else dlon=1.0;


		 *(pfb++)=*pa->xv[i]-0.5*dlon;
		 for (k=0; k < idv-1; k++)
		       *(pfb++)=0.5*(*(pa->xv[i]+k+1)+*(pa->xv[i]+k));
/*				this is it.				*/

		 /* DNW - Calculate the last and first boundary */
		 *pfb=pa->xv[i][k]-((pa->xv[i][k-1]-pa->xv[i][k])*0.5);
		 pa->xb[i][0]=pa->xv[i][0]+((pa->xv[i][0]-pa->xv[i][1])*0.5);
		 
		 /**pfb=*(pa->xv[i]+idv-1)+0.5*dlon;*/
		 if (doexist("plev",dname[i]))
		   {
		    *pfb=(*pfb > 1013.)? 1013. : *pfb;
		    *pfb=(*pfb < 0.)? 0. : *pfb;
		    pfb=pa->xb[i];
		    *pfb=(*pfb > 1013.)? 1013. : *pfb;
		    *pfb=(*pfb < 0.)? 0. : *pfb;
		   }
		 else if (doexist("latitude",dname[i]))
		   {
		    *pfb=(*pfb > 90.)? 90. : *pfb;
		    *pfb=(*pfb < -90.)? -90. : *pfb;
		    pfb=pa->xb[i];
		    *pfb=(*pfb > 90.)? 90. : *pfb;
		    *pfb=(*pfb < -90.)? -90. : *pfb;
		   }
	        }
	      if (pa->xw[i] == NULL)
	        {
		 idv=*pa->xs[i];
	         if ((pfw=pa->xw[i]=(float *)malloc(idv*sizeof(float)))==NULL)
		   {err_warn(1,fperr,memerr); return 0;}
		 idv=*pa->xs[i];
		 if (doexist("latitude",dname[i]))
		   {
		    for (k=0; k<idv; k++,pfw++)
		      {if((*pfw=0.5*(sin(*(pa->xb[i]+k+1)*dtr)-
			      sin(*(pa->xb[i]+k)*dtr)))<0.)
			    *pfw=-*pfw;
		      }
		   }
		 else
		    for (k=0; k < idv; k++)
		          *(pfw++)=fabs(*(pa->xb[i]+k+1)-*(pa->xb[i]+k));
	        }
	     }

/*			Is interpolation needed? How about wrap?	*/

	   if ((pa->xi[i]=(int *)malloc(sizeof(int)))==NULL)
		   {err_warn(1,fperr,memerr); return 0;}
	   *pa->xi[i]=need_interp(pa->XS[i],pa->XK[i],pa->XC[i],pa->XV[i],
			pa->XB[i],pa->xs[i],pa->xj[i],pa->xv[i],pa->xb[i]);
	   if (*pa->xi[i] == -1)
	     {
	      if (doexist("latitude",pa->XN[i]))
		{
	          err_warn(0,fperr,"Warning - data request %s(%s) extends "
			"outside the actual domain.\n",pa->N,pa->XN[i]);
		 *pa->xi[i]=1;
		}
	      else
		{
	         err_warn(1,fperr,"Error - data request (%s) extends "
			"outside the actual domain.\n",pa->N,pa->XN[i]);
	         pa->notok++;
		}
	     }
/*		Remove weights for duplicated values in a
				wrapped dimension.			*/

	   if (*pa->XC[i] > 0.0 && pa->XK[i][0] != pa->XK[i][1])
	     {
	      for (j=1;j < *pa->xs[i];j++)
		{
		 if(fabs(pa->xb[i][j]-pa->xb[i][0])-*pa->XC[i]>
							-*pa->XC[i]*.0001)
		    pa->xw[i][j]=0.0;
		}
	     }
/*			Set first and last values actually used.	*/

/*	   *pa->xf[i]=*(pa->xv[i]);
	   *pa->xl[i]=*(pa->xv[i]+*pa->xs[i]-1);
*/
	  }

/*		Just to make sure that dimension sizes are alright.	*/

	for (i=0;i<pa->ND;i++)
	  {
	   if (*pa->xs[i] <= 0) {pa->notok=1; return 0;}
	  }

        if (pa->notok == 1) pa->notok=0;
	else pa->notok=1;
        return 1;
      }

/*		Check naming strings in A_name attributes do specify a
		variable in the dictionary.  Attempt to verify with
		minimal naming (name,title) first.  Replace naming
		strings in A_name.
		Return 0 - false, can't find) or
		       1 - true, found and replaced all naming strings	*/

    int checkA_naming(lu,a_name,pa)

      int lu;		/* logical unit for DRS dictionary file	 	*/
      char a_name[];	/* name of array attribute set (print only)	*/
      struct a_attr *pa;/* pointer to array attribute set		*/

      {
	int i,C,T;
	char source[122],name[CW_MAX_NAME],title[82],units[42];
	char crd[10],crt[10],type[10];
	char *pc;
	char tmp_source[CW_MAX_NAME],tmp_title[CW_MAX_NAME],tmp_units[CW_MAX_NAME];
	char tmp_crd[CW_MAX_NAME],tmp_crt[CW_MAX_NAME],tmp_type[CW_MAX_NAME];

	int nd;

	extern char *repstr();

	for (i=0;i<122;i++) source[i]='\0';
	for (i=0;i<CW_MAX_NAME ;i++) name[i]='\0';
	for (i=0;i<82 ;i++) title[i]='\0';
	for (i=0;i<42 ;i++) units[i]='\0';
	for (i=0;i<10;i++) crd[i]=crt[i]=type[i]='\0';
	source[0]=name[0]=title[0]=units[0]=type[0]=' ';

/*		Check name and title for existence			*/

	if (pa->N != NULL && strlen(pa->N) != 0) {
	   strncpy (name,pa->N,CW_MAX_NAME); name[CW_MAX_NAME-1] = '\0';
	} else
	   return 0;
	if (pa->TI != NULL && strlen(pa->TI) > (size_t)0)
	   strncpy (title,pa->TI,81); title[80] = '\0';

	Cluvdb ();
	Setname (" ",name," "," "," ");

	if (Inqdict (lu,IDRS_GETFIRSTVAR) == IDRS_SUCCESS)
	  { 
/*		How many with this name and title?			*/
	   i=1;
	   while (Inqdict (lu,IDRS_GETNEXTVAR) == IDRS_SUCCESS) i++;
/*		If more than one, request with full naming info		*/
	   if (i > 1)
	     {
	      if (pa->S != NULL && strlen(pa->S) > (size_t) 0) {
                 strncpy (source,pa->S,121); source[120] = '\0';
	      }
	      if (pa->U != NULL && strlen(pa->U) > (size_t) 0) {
                 strncpy (units,pa->U,41); units[40] = '\0';
	      }

	      Cluvdb();
	      Setname (" ",name," "," "," ");

	      if (Inqdict (lu,IDRS_GETFIRSTVAR) != IDRS_SUCCESS)
		{
		 err_warn(1,fperr,
			"Error - variable does not exist for: A_%s\n",a_name);
		 return 0;
		}
	      else
		{
		 i=1;
		 while (Inqdict(lu,IDRS_GETNEXTVAR)==IDRS_SUCCESS) i++;
		 if (i > 1)
		   {
		    err_warn(1,fperr,
			"Error - a naming ambiguity exists in A_%s\n",
					a_name);
		    return 0;
		   }
		}
	     }
	  }
	else
	  {
	   err_warn(1,fperr,
			"Error - variable does not exist for: A_%s\n",a_name);
	   return 0;
	  }

/*		Variable was found, now make sure the VDB is set	*/

	Cluvdb();
	Setname (" ",name," "," "," ");
	Inqdict (lu,IDRS_GETFIRSTVAR);

/*		Pick up the naming strings and put in A_name		*/

	Getname (tmp_source,name,tmp_title,tmp_units,tmp_crd,tmp_crt,tmp_type,&nd);
	strncpy(source,tmp_source,121); source[120] = '\0';
	strncpy(title,tmp_title,81); title[80] = '\0';
	strncpy(units,tmp_units,41); units[40] = '\0';
	strncpy(crd,tmp_crd,10); crd[9] = '\0';
	strncpy(crt,tmp_crt,10); crt[9] = '\0';
	strncpy(type,tmp_type,10); type[9] = '\0';

	trimbl(source,121);
	trimbl(name,CW_MAX_NAME);
	trimbl(title,81);
	trimbl(units,41);
	trimbl(crd,9);
	trimbl(crt,9);
	trimbl(type,9);
					     /* Force cast of reals and integers to 4-byte representation */
#ifdef CDCOMPAT
#ifdef cray
	if(type[0] == 'R') strcpy(type,"R*8");
	if(type[0] == 'I') strcpy(type,"I*8");
#else
	if(type[0] == 'R') strcpy(type,"R*4");
	if(type[0] == 'I') strcpy(type,"I*4");
#endif
#endif
	
	pa->S=repstr(pa->S,source);
	if (pa->s == NULL) pa->s=repstr(pa->s,source);
	pa->N=repstr(pa->N,name);
	if (pa->n == NULL) pa->n=repstr(pa->n,name);
	pa->TI=repstr(pa->TI,title);
	if (pa->ti == NULL) pa->ti=repstr(pa->ti,title);
	pa->U=repstr(pa->U,units);
	if (pa->u == NULL) pa->u=repstr(pa->u,units);

	pa->CRD=repstr(pa->CRD,crd);
	if (pa->aCRD != NULL) { free(pa->aCRD); pa->aCRD=NULL; }
	if (pa->acrd == NULL) pa->crd=repstr(pa->crd,crd);
	else pa->crd=repstr(pa->crd,pa->acrd);

	pa->CRT=repstr(pa->CRT,crt);
	if (pa->aCRT != NULL) { free(pa->aCRT); pa->aCRT=NULL; }
	if (pa->acrt == NULL) pa->crt=repstr(pa->crt,crt);
	else pa->crt=repstr(pa->crt,pa->acrt);
	
	pa->TY=repstr(pa->TY,type);
	if (pa->aTY != NULL) { free(pa->aTY); pa->aTY=NULL; }
	if (pa->aty != NULL) { free(pa->aty); pa->aty=NULL; }
	pa->ty=repstr(pa->ty,type);

/*			Check usefulness of the data.			*/
	pc=pa->TY;
	T=toupper(*pc);
	if (T == 'I' || T == 'R')
	  {
	   pc+=2;
	   if (isnum(pc)) C=*pc-48;/*  ISNUM */
	  }
	else
	  {
	   err_warn(1,fperr,
			"Error - (%s) data type is character/non-useable.\n",
			pa->N);
	   return 0;
	  }
	if (T == 'R' && C != 4 && C != 8)
	  {
	   err_warn(1,fperr,
		"Error - (%s) data length for REAL is non-useable.\n",
			pa->N);
	   return 0;
	  }
	if (T == 'I' && (C != 1 && C != 2 && C != 4))
	  {
	   err_warn(1,fperr,
		"Error - (%s) data length for INT is non-useable.\n", pa->N);
	   return 0;
	  }

/*			Check usefulness of the request for data change.
	pc=pa->ty;
	t=toupper(*pc);
	if (t == 'I' || t == 'R')
	  {
	   pc+=2;
	   if (isnum(pc)) c=*pc-48;
	  }
	else
	  {
	   err_warn(1,fperr,
		"Error - (%s) requested data type is character/non-useable.\n",
			pa->N);
	   return 0;
	  }
	if (t == 'R' && c != 4)
	  {
	   err_warn(1,fperr,
		"Error - (%s) requested data length for REAL is non-useable.\n",
			pa->N);
	   return 0;
	  }
	if (t == 'I' && (c != 1 && c != 2 && c != 4))
	  {
	   err_warn(1,fperr,
		"Error - (%s) requested data length for INT is non-useable.\n",
			pa->N);
	   return 0;
	  }							*/

	return 1;
      }

/*		Move list values into array attributes where they're
		assigned.						*/

    int moveA_assign(a_name,pa)

      char a_name[];	/* name of the array attribute set		*/
      struct a_attr *pa;/* pointer to the array attribute set		*/

      {
	int i,j,count,ns,ir,iblock;
	struct l_val *pval;
	char *ptc;
	char **pc,**pt;
	int **pi,*pti;
	float **pf,*pr,*ptf;
	float v;

	ir=1;

/*		This set of array attributes must be set not ok
		until checking is done.					*/

	pa->notok=1;

	pa->ND=0;
	pa->mean=1.e20;
	pa->min=1.e20;
	pa->max=1.e20;
	if (pa->mask != NULL) {free(pa->mask); pa->mask=NULL;}
	if (pa->un.data != NULL) {free(pa->un.data); pa->un.data=NULL;}
	if (pa->af != NULL && (int)strlen(pa->af) > 0)
	  {
	   if (pa->aF != NULL) {free(pa->aF); pa->aF=NULL;}
	   if (pa->almask != NULL) {free(pa->almask); pa->almask=NULL;}
	   if (pa->atrnf != NULL) {free(pa->atrnf); pa->atrnf=NULL;}

	   if (pa->aS != NULL) {free(pa->aS); pa->aS=NULL;}
	   if (pa->aN != NULL) {free(pa->aS); pa->aS=NULL;}
	   if (pa->aTI != NULL) {free(pa->aS); pa->aS=NULL;}
	   if (pa->aU != NULL) {free(pa->aS); pa->aS=NULL;}
	   if (pa->aTY != NULL) {free(pa->aS); pa->aS=NULL;}
	   if (pa->aCRD != NULL) {free(pa->aS); pa->aS=NULL;}
	   if (pa->aCRT != NULL) {free(pa->aS); pa->aS=NULL;}
	  }

/*		Pick up and move strings to attributes			*/

	iblock = &(pa->aXN[0]) - &(pa->aF);
	for (pc=&(pa->aF),pt=&(pa->F),i=0;
		i < iblock;
		i++,pc++,pt++)
	  {
	   if (*pt != NULL)
	     {
	      free( (char *)*pt);
	      *pt=NULL;
	     }
	   if ((ptc=*pc) != NULL)
	     {
	      if (strncmp(*pc,"L_",2) == 0)
	        {

/*	 	Look in the list table for the name.
		Only one string required for each, so only one used.	*/

	         if (findL_name (ptc+2,&pval,&count))
	           {
	            if ((ptc=(char *)malloc(strlen(pval->str)+1)) == NULL)
	              {err_warn(1,fperr,"Error - no memory\n"); return 0;}
	            strcpy(ptc,pval->str);
		    *pt=ptc;
		   }
	         else
		   {
		    err_warn(1,fperr,
			"Error - list (%s) for (%s.%s) doesn't exist.\n",
			*pc,a_name,A_strg[i]);
		    ir=0;
		   }
	        }
	      else if (findL_name (ptc,&pval,&count))
	        {
	         if ((ptc=(char *)malloc(strlen(pval->str)+1)) == NULL)
	              {err_warn(1,fperr,"Error - no memory\n"); return 0;}
	         strcpy(ptc,pval->str);
		 *pt=ptc;
		}
	      else if (**pc == '@')
		{
	         if (strncmp(ptc+1,"L_",2) == 0)
	           {

/*	 	Look in the list table for the name.

		   Only one string required for each, so only one used.	*/

	            if (findL_name (ptc+3,&pval,&count))
	              {
	               if ((ptc=(char *)malloc(strlen(pval->str)+1)) == NULL)
	                 {err_warn(1,fperr,"Error - no memory\n"); return 0;}
	               strcpy(ptc,pval->str);
		       *pt=ptc;
		       free((char *)*pc);
		       if ((*pc=(char *)malloc(strlen(pval->str)+1)) == NULL)
			 {err_warn(1,fperr,"Error - no memory\n"); return 0;}
		       strcpy(*pc,pval->str);
		      }
	            else
		      {
		       err_warn(1,fperr,
			   "Error - list (%s) for (%s.%s) doesn't exist.\n",
			   *pc,a_name,A_strg[i]);
		       ir=0;
		      }
	           }
	         else if (findL_name (ptc,&pval,&count))
	           {
	            if ((ptc=(char *)malloc(strlen(pval->str)+1)) == NULL)
	              {err_warn(1,fperr,"Error - no memory\n"); return 0;}
	            strcpy(ptc,pval->str);
		    *pt=ptc;
		    free((char *)*pc);
	            if ((*pc=(char *)malloc(strlen(pval->str)+1)) == NULL)
	              {err_warn(1,fperr,"Error - no memory\n"); return 0;}
	            strcpy(*pc,pval->str);
		   }
		}
	      else
	        {
	         if ((ptc=(char *)malloc(strlen(*pc)+1)) == NULL)
		   {err_warn(1,fperr,"Error - no memory\n"); return 0;}
	         strcpy(ptc,*pc);
	         *pt=ptc;
	        }
	     }
	  }
	for (pc=&(pa->aXN[0]),pt=&(pa->XN[0]),i=0; i < 4*NDS;i++,pc++,pt++)
	  {
	   if (*pt != NULL)
	     {
	      free( (char *)*pt);
	      *pt=NULL;
	     }
	   if ((ptc=*pc) != NULL)
	     {
	      if ((ptc=(char *)malloc(strlen(*pc)+1)) == NULL)
		   {err_warn(1,fperr,"Error - no memory\n"); return 0;}
	      strcpy(ptc,*pc);
	      *pt=ptc;

	     }
	  }

/*		Pick up and move integers to attributes			*/

/*			Dimension sizes can't be set by the user.	*/
	for (i=0;i<NDS;i++)
	   {
	    if (pa->aXS[i] != NULL)
	      {
	       free ((char *)pa->aXS[i]);
	       pa->aXS[i]=NULL;
	       free ((char *)pa->axs[i]);
	       pa->axs[i]=NULL;
	      }
	   }

	iblock = (int **)&(pa->aXS[0]) - &(pa->XS[0]);
	for (pc=&pa->aXS[0],pi=&pa->XS[0],i=0;
		i < iblock;
		i++,pc++,pi++)
	  {
	   if (*pi != NULL)
	     {
	      free((char *)*pi);
	      *pi=NULL;
	     }
	   if ((ptc=*pc) != NULL)
	     {
	      j=i%NDS;
	      if (strncmp(*pc,"L_",2) == 0)
		{
/*	 	Look in the list table for the name.			*/

		 if (findL_name (ptc+2,&pval,&count))
		   {
/*			XK requires two values.				*/

		    if (doexist("wrap",A_intg[i/NDS][0]))
		       ns=2;
		    else
		       ns=1;
	            if ((pti=(int *)malloc(ns*sizeof(int))) == NULL)
	              {err_warn(1,fperr,"Error - no memory\n"); return 0;}
		    *pi=pti;
		    while (ns-- >0)
		      {
			if (pval != NULL)
			   *pti=pval->data;
			 else
			   *pti=0;
			pval=pval->next;
			pti++;
		      }
		   }
	         else
		   {
		    free((char *)*pc);
		    *pc=NULL;
		    err_warn(1,fperr,
			"Error - (A_%s(%s=%s) list doesn't exist.\n",
			a_name,A_intg[i][i%NDS],*pc);
		    ir=0;
		   }
	        }
	      else if (findL_name (ptc,&pval,&count))
		{
/*			XK requires two values.				*/

		 if (doexist("wrap",A_intg[i/NDS][0]))
		    ns=2;
		 else
		    ns=1;
		 if ((pti=(int *)malloc(ns*sizeof(int))) == NULL)
	              {err_warn(1,fperr,"Error - no memory.\n"); return 0;}
		 *pi=pti;
		 while (ns-- >0)
		   {
		    if (pval != NULL)
		       *pti=pval->data;
		    else
		       *pti=0;
		    pval=pval->next;
		    pti++;
		   }
		}
	      else if (*ptc == '@')
		{
	         if (strncmp(ptc+1,"L_",2) == 0)
		   {
/*	 	Look in the list table for the name.			*/

		    if (findL_name (ptc+3,&pval,&count))
		      {
/*			XK requires two values.				*/

		       if (doexist("wrap",A_intg[i/NDS][0]))
		          ns=2;
		       else
		          ns=1;
	               if ((pti=(int *)malloc(ns*sizeof(int))) == NULL)
	                 {err_warn(1,fperr,"Error - no memory.\n"); return 0;}
		       free((char *)*pc);
	               if ((*pc=(char *)malloc(ns*16)) == NULL)
	                 {err_warn(1,fperr,"Error - no memory.\n"); return 0;}
		       *pi=pti;
		       ptc=*pc;
		       while (ns-- >0)
		         {
			   if (pval != NULL)
			     {
			      *pti=pval->data;
			      if (ns > 0) sprintf(ptc,"%d,",*pti);
			      else        sprintf(ptc,"%d",*pti);
			      while(*ptc != '\0') ptc++;
			     }
			   else
			     {
			      *pti=0;
			      if (ns > 0) sprintf(ptc,"0,");
			      else        sprintf(ptc,"0");
			      while(*ptc != '\0') ptc++;
			     }
			   pval=pval->next;
			   pti++;
		         }
		      }
	            else
		      {
		       free((char *)*pc);
		       *pc=NULL;
		       err_warn(1,fperr,
			   "Error - (A_%s(%s=%s) list doesn't exist.\n",
			   a_name,A_intg[i][i%NDS],*pc);
		       ir=0;
		      }
	           }
	         else if (findL_name (ptc,&pval,&count))
		   {
/*			XK requires two values.				*/

		    if (doexist("wrap",A_intg[i/NDS][0]))
		       ns=2;
		    else
		       ns=1;
		    if ((pti=(int *)malloc(ns*sizeof(int))) == NULL)
	                 {err_warn(1,fperr,"Error - no memory\n"); return 0;}
		    free((char *)*pc);
	            if ((*pc=(char *)malloc(ns*16)) == NULL)
	                 {err_warn(1,fperr,"Error - no memory.\n"); return 0;}
		    *pi=pti;
		    while (ns-- >0)
		      {
		       if (pval != NULL)
			 {
		          *pti=pval->data;
			  if (ns > 0) sprintf(ptc,"%d,",*pti);
			  else        sprintf(ptc,"%d",*pti);
			  while(*ptc != '\0') ptc++;
			 }
		       else
			 {
		          *pti=0;
			  if (ns > 0) sprintf(ptc,"0,");
			  else        sprintf(ptc,"0");
			  while(*ptc != '\0') ptc++;
			 }
		       pval=pval->next;
		       pti++;
		      }
		   }
	         else
		   {
		    free((char *)*pc);
		    *pc=NULL;
		    err_warn(1,fperr,
			   "Error - (A_%s(%s=%s) list doesn't exist.\n",
			   a_name,A_intg[i][i%NDS],*pc);
		    ir=0;
		   }

		}
	      else
		{
		 for (ptc=*pc,count=1; *ptc != '\0'; ptc++)
			if (*ptc == ',') count++;/*  count the commas 	*/

/*			XK requires two values.				*/

		 if (doexist("wrap",A_intg[i/NDS][0]))
		   {
		    ns=2;
		   }
		 else
		   {
		    ns=1;
		   }
		 if ((*pi=(int *)malloc(ns*sizeof(int))) == NULL)
		   {err_warn(1,fperr,"Error - no memory\n"); return 0;}
		 for (ptc=*pc,pti=*pi,j=0; ns-- > 0;pti+=1,j++)
		   {
		    if (*ptc != '\0')
		      {
		       if (isnum(ptc)) sscanf(ptc,"%f",&v);
		       else
			 {
			  err_warn(1,fperr,
			   "Error - not a list, should be a number (%s) -"
					" nothing assigned.\n",ptc);
			  free((char *)*pi);
			  *pi=NULL;
			  break;
			 }
		       *pti=v;
		       while (*ptc != '\0')
			  if (*ptc++ == ',') break;
		      }
		    else
		      {
			*pti=0;
		      }
		   }
		}
	     }
	  }
		 
/*		Pick up and move scalar float to attributes		*/

	iblock = (float **)&(pa->aXC[0]) - &(pa->XC[0]);
	for (pc=&pa->aXC[0],pf=&pa->XC[0],i=0;
		i < iblock;
		i++,pc++,pf++)
	  {
	   if (*pf != NULL)
	     {
	      free((char *)*pf);
	      *pf=NULL;
	     }
	   if ((ptc=*pc) != NULL)
	     {
	      if (strncmp(*pc,"L_",2) == 0)
	        {
/*	 	Look in the list table for the name.
		Only one real required for each, so only one used.	*/

	         if (findL_name (ptc+2,&pval,&count))
	           {
	            if (*pf != NULL) free((char *)*pf);
	            if ((pr=(float *)malloc(sizeof(float))) == NULL)
	              {err_warn(1,fperr,"Error - no memory\n"); return 0;}
	            *pr=pval->data;
		    *pf=pr;
		   }
	         else
		   {
		    err_warn(1,fperr,
			"Error - list (%s) for (%s.%s) doesn't exist.\n",
			*pc,a_name,A_sflt[i][i%NDS]);
		    ir=0;
		   }
		}
	      else if (findL_name (ptc,&pval,&count))
	        {
	         if (*pf != NULL) free((char *)*pf);
	         if ((pr=(float *)malloc(sizeof(float))) == NULL)
	           {err_warn(1,fperr,"Error - no memory\n"); return 0;}
	         *pr=pval->data;
		 *pf=pr;
		}
	      else if (*ptc == '@')
		{
	         if (strncmp(*pc,"L_",2) == 0)
	           {
/*	 	Look in the list table for the name.
		Only one real required for each, so only one used.	*/

	            if (findL_name (ptc+2,&pval,&count))
	              {
	               if (*pf != NULL) free((char *)*pf);
	               if ((pr=(float *)malloc(sizeof(float))) == NULL)
	                 {err_warn(1,fperr,"Error - no memory\n"); return 0;}
		       free((char *)*pc);
	               if ((*pc=(char *)malloc(20)) == NULL)
	                 {err_warn(1,fperr,"Error - no memory.\n"); return 0;}
	               *pr=pval->data;
		       *pf=pr;
		       sprintf(*pc,"%9g",*pr);
		      }
	            else
		      {
		       err_warn(1,fperr,
			   "Error - list (%s) for (%s.%s) doesn't exist.\n",
			   *pc,a_name,A_sflt[i][i%NDS]);
		       ir=0;
		      }
		   }
	         else if (findL_name (ptc,&pval,&count))
	           {
	            if (*pf != NULL) free((char *)*pf);
	            if ((pr=(float *)malloc(sizeof(float))) == NULL)
	              {err_warn(1,fperr,"Error - no memory\n"); return 0;}
		    free((char *)*pc);
	            if ((*pc=(char *)malloc(20)) == NULL)
	                 {err_warn(1,fperr,"Error - no memory.\n"); return 0;}
	            *pr=pval->data;
		    *pf=pr;
		    sprintf(*pc,"%9g", *pr);
		   }
		}
	      else
		{
		 if ((ptf=(float *)malloc(sizeof(float))) == NULL)
		   {err_warn(1,fperr,"Error - no memory\n"); return 0;}
		 *pf=ptf;
		 if (isnum(*pc))
                    sscanf(*pc,"%f",ptf);
		 else
		   {
		    err_warn(1,fperr,
		       "Error - not a list, should be a number (%s)\n",ptc);
		    free((char *)*pf);
		    *pf=NULL;
		   }
		}
	     }
	  }

/*		Free XV, XB, XW.					*/

	iblock = (float **)&(pa->aXV[0]) - &(pa->xv[0]);
	for (pf=&pa->XV[0],i=0;
		i < iblock;
		i++,pc++,pf++)
	  {
	   if (*pf != NULL)
	     {
	      free((char *)*pf);
	      *pf=NULL;
	     }
	  }
/*		Pick up and move vector float to attributes		*/

	for (pc=&pa->axv[0],pf=&pa->xv[0],i=0;
		i < iblock;
		i++,pc++,pf++)
	  {
/*			Free xv, xb, xw.				*/

	   if (*pf != NULL)
	     {
	      free((char *)*pf);
	      *pf=NULL;
	     }
/*			If there is an assigned value move it to actual
			values.						*/
	   if (*pc != NULL)
	     {
	      for (ptc=*pc,count=1; *ptc != '\0'; ptc++)
		   if (*ptc == ',') count++;/*  count the commas 	*/
	      ptc=*pc;
	      if (strncmp(*pc,"L_",2) == 0)
/*	 	Look in the list table for the name.			*/

	        {
		 if (findL_name (ptc+2,&pval,&count))
		   {

/*			Don't insert values for (xb) or (xw)
					 if the count is wrong		*/

		    if (pa->xs[i%NDS] != NULL && (
			   (i/NDS == 1 && *pa->xs[i%NDS]+1 != count) ||
			   (i/NDS == 2 && *pa->xs[i%NDS]   != count)) )
		      {
		       err_warn(1,fperr,
			   "Warning - discarded A_%s(%s=%s).\n",
				a_name,A_vflt[3+i/NDS][i%NDS],*pc);
		      }
		    else
		      {
		       if (i/NDS == 0)
		         {
/*			Set the count for (xv) and remove bounds and weights */

		          if (pa->xs[i%NDS] != NULL && *pa->xs[i%NDS] != count)
			    {
		             if (pa->xb[i%NDS] != NULL)
		               {free(pa->xb[i%NDS]); pa->xb[i%NDS]=NULL;}
		             if (pa->xw[i%NDS] != NULL)
		               {free(pa->xw[i%NDS]); pa->xw[i%NDS]=NULL;}
			    }
		          else if (pa->xs[i%NDS] == NULL && 
		              (pa->xs[i%NDS]=(int *)malloc(sizeof(int)))==NULL)
			    {err_warn(1,fperr,"Error - no memory\n"); return 0;}

		          *pa->xs[i%NDS]=count;

		         }
  
	               if ((pr=(float *)malloc(count*sizeof(float))) == NULL)
	                 {err_warn(1,fperr,"Error - no memory\n"); return 0;}
		       *pf=pr;
	               for (j=0; j < count; j++,pr++,pval=pval->next)
		          *pr=pval->data;
		      }
		   }
	         else
		   {
		    err_warn(1,fperr,
			 "Error - list (%s) for (%s.%s) doesn't exist.\n",
			*pc,a_name,A_vflt[3+i/NDS][i%NDS]);
		    ir=0;
		   }
	        }
	      else if (findL_name (ptc,&pval,&count))
		{

/*			Don't insert values for (xb) or (xw)
					 if the count is wrong		*/

		 if (pa->xs[i%NDS] != NULL && (
			   (i/NDS == 1 && *pa->xs[i%NDS]+1 != count) ||
			   (i/NDS == 2 && *pa->xs[i%NDS]   != count)) )
		   {
		    err_warn(1,fperr,
			   "Warning - discarded A_%s(%s=%s).\n",
				a_name,A_vflt[3+i/NDS][i%NDS],*pc);
		   }
		 else
		   {
		    if (i/NDS == 0)
		      {
/*			Set the count for (xv) and remove bounds and weights */

		       if (pa->xs[i%NDS] != NULL && *pa->xs[i%NDS] != count)
			 {
		          if (pa->xb[i%NDS] != NULL)
		            {free(pa->xb[i%NDS]); pa->xb[i%NDS]=NULL;}
		          if (pa->xw[i%NDS] != NULL)
		            {free(pa->xw[i%NDS]); pa->xw[i%NDS]=NULL;}
			 }
		       else if (pa->xs[i%NDS] == NULL && 
		              (pa->xs[i%NDS]=(int *)malloc(sizeof(int)))==NULL)
			    {err_warn(1,fperr,"Error - no memory\n"); return 0;}

		       *pa->xs[i%NDS]=count;

/*				fprintf (stdout,"xs[%d]=count=%d\n",count);*/

		      }
  
		    if ((pr=(float *)malloc(count*sizeof(float))) == NULL)
	                 {err_warn(1,fperr,"Error - no memory\n"); return 0;}
		    *pf=pr;
	            for (j=0; j < count; j++,pr++,pval=pval->next)
		          *pr=pval->data;
		   }
		}
	      else if (*ptc == '@')
		{
	         if (strncmp(ptc+1,"L_",2) == 0)

/*	 	Look in the list table for the name.			*/

	           {
		    if (findL_name (ptc+3,&pval,&count))
		      {

/*			Don't insert values for (xb) or (xw)
					 if the count is wrong		*/

		       if (pa->xs[i%NDS] != NULL && (
			   (i/NDS == 1 && *pa->xs[i%NDS]+1 != count) ||
			   (i/NDS == 2 && *pa->xs[i%NDS]   != count)) )
		         {
		          err_warn(1,fperr,
			      "Warning - discarded A_%s(%s=%s).\n",
				a_name,A_vflt[3+i/NDS][i%NDS],*pc);
		         }
		       else
		         {
		          if (i/NDS == 0)
		            {
/*			Set the count for (xv) and remove bounds and weights */

		             if(pa->xs[i%NDS]!=NULL && *pa->xs[i%NDS]!=count)
			       {
		                if (pa->xb[i%NDS] != NULL)
		                  {free(pa->xb[i%NDS]); pa->xb[i%NDS]=NULL;}
		                if (pa->xw[i%NDS] != NULL)
		                  {free(pa->xw[i%NDS]); pa->xw[i%NDS]=NULL;}
			       }
		             else if (pa->xs[i%NDS] == NULL && 
		              (pa->xs[i%NDS]=(int *)malloc(sizeof(int)))==NULL)
			       {
				err_warn(1,fperr,"Error - no memory.\n");
				return 0;
			       }

		             *pa->xs[i%NDS]=count;

/*				fprintf (stdout,"xs[%d]=count=%d\n",count);*/

		            }
  
	                  if ((pr=(float *)malloc(count*sizeof(float))) == NULL)
	                    {err_warn(1,fperr,"Error - no memory.\n");return 0;}
			  free((char *)*pc);
	                  if ((ptc=*pc=(char *)malloc(count*16)) == NULL)
	                    {err_warn(1,fperr,"Error - no memory.\n");return 0;}
		          *pf=pr;
	                  for (j=0; j < count; j++,pr++,pval=pval->next)
			    {
		             *pr=pval->data;
			     if (j == 0) sprintf(ptc,"%g",*pr);
			     else sprintf(ptc,",%g",*pr);
			     while (*ptc != '\0') ptc++;
			    }
		         }
		      }
	            else
		      {
		       err_warn(1,fperr,
			 "Error - list (%s) for (%s.%s) doesn't exist.\n",
			*pc,a_name,A_vflt[3+i][i%NDS]);
		       ir=0;
		      }
	           }
	         else if (findL_name (ptc+1,&pval,&count))
		   {

/*			Don't insert values for (xb) or (xw)
					 if the count is wrong		*/

		    if (pa->xs[i%NDS] != NULL && (
			   (i/NDS == 1 && *pa->xs[i%NDS]+1 != count) ||
			   (i/NDS == 2 && *pa->xs[i%NDS]   != count)) )
		      {
		       err_warn(1,fperr,
			   "Warning - discarded A_%s(%s=%s).\n",
				a_name,A_vflt[3+i/NDS][i%NDS],*pc);
		      }
		    else
		      {
		       if (i/NDS == 0)
		         {
/*			Set the count for (xv) and remove bounds and weights */

		          if (pa->xs[i%NDS] != NULL && *pa->xs[i%NDS] != count)
			    {
		             if (pa->xb[i%NDS] != NULL)
		               {free(pa->xb[i%NDS]); pa->xb[i%NDS]=NULL;}
		             if (pa->xw[i%NDS] != NULL)
		               {free(pa->xw[i%NDS]); pa->xw[i%NDS]=NULL;}
			    }
		          else if (pa->xs[i%NDS] == NULL && 
		              (pa->xs[i%NDS]=(int *)malloc(sizeof(int)))==NULL)
			    {err_warn(1,fperr,"Error - no memory\n"); return 0;}

		          *pa->xs[i%NDS]=count;

/*				fprintf (stdout,"xs[%d]=count=%d\n",count);*/

		         }
  
		       if ((pr=(float *)malloc(count*sizeof(float))) == NULL)
	                 {err_warn(1,fperr,"Error - no memory\n"); return 0;}
		       free((char *)*pc);
	               if ((ptc=*pc=(char *)malloc(count*16)) == NULL)
	                    {err_warn(1,fperr,"Error - no memory.\n");return 0;}
		       *pf=pr;
	               for (j=0; j < count; j++,pr++,pval=pval->next)
			 {
		          *pr=pval->data;
			  if (j == 0) sprintf(ptc,"%g",*pr);
			  else sprintf(ptc,",%g",*pr);
			  while (*ptc != '\0') ptc++;
			 }
		      }
		   }
		}
	      else
	        {
	         for (ptc=*pc,count=1; *ptc != '\0'; ptc++)
		   if (*ptc == ',') count++;/*  count the commas 	*/


	         if (pa->xs[i%NDS] != NULL && (
			   (i/NDS == 1 && *pa->xs[i%NDS]+1 != count) ||
			   (i/NDS == 2 && *pa->xs[i%NDS]   != count)) )
		   {
		 
		    err_warn(1,fperr,
			"Warning - discarded A_%s(%s=(%s)).\n",
				a_name,A_vflt[3+i/NDS][i%NDS],*pc);
		   }
	         else
		   {
		    if ((pr=(float *)malloc(count*sizeof(float))) == NULL)
		      {err_warn(1,fperr,"Error - no memory\n"); return 0;}
		    for (ptc=*pc,*pf=ptf=pr,j=0; *ptc != '\0';ptf+=1,j++)
		      {
		       if (isnum(ptc)) sscanf(ptc,"%f",ptf);
		       else
			 {
			  err_warn(1,fperr,
			  "Error - not a list, should be a number (%s) -"
				" not assigned.\n",ptc);
			  free((char *)*pf);
			  *pf=NULL;
			  j=0;
			  break;
			 }
		       while (*ptc != ',' && *ptc != '\0') ptc++;
		       if (*ptc != '\0') ptc++;
		      }
		    if (i/NDS == 0 )
		      {
/*			Set the count for (xv) and remove bounds
			and weights if the previous count changes 	*/

		       if (pa->xs[i%NDS] != NULL && *pa->xs[i%NDS] != j)
		         {
		          if (pa->xb[i%NDS] != NULL)
		            {free(pa->xb[i%NDS]); pa->xb[i%NDS]=NULL;}
		          if (pa->xw[i%NDS] != NULL)
		            {free(pa->xw[i%NDS]); pa->xw[i%NDS]=NULL;}
		         }

		       else if (pa->xs[i%NDS] == NULL && 
		            (pa->xs[i%NDS]=(int *)malloc(sizeof(int)))==NULL)
		         {err_warn(1,fperr,"Error - no memory\n"); return 0;}

		       *pa->xs[i%NDS]=j;

/*			fprintf (stdout,"xs[%d]=j=%d\n",j);		*/
	              }
/*	            *pf=pr;		*/
		   }
	        }
	     }
	  }
	return ir;
       }

/*		nullify the attribute pointers, initialize them.	*/

    int zeroA_attr(ptr)

      struct a_attr *ptr;

      {
	int i,len,*pt;

	len=(sizeof(struct a_attr))/sizeof(int);
	for (i=0,pt=(int *)&ptr->notok;i<len;i++,pt++)
	   *pt=(int)NULL;
	ptr->notok=1;   /* There is nothing here yet. */
	return 1;
      }
/*		Determine whether interpolation is needed.
		return 0 - not needed
		return 1 - needed
		return -1 - nothing within the range to interpolate.	*/

    int need_interp(D,pK,C,pV,pB,d,jx,pv,pb)

      int *D;		/*  Dimension size of actual data.		*/
      int *d;		/*  Dimension size of requested data.		*/
      int *jx;		/*  Jump interval in requested dimension.	*/
      int *pK;		/*  Wrap coefficient limits.			*/
      float *C;		/*  Wrap cycle.					*/
      float *pV;	/*  Values of actual data dimension.		*/
      float *pB;	/*  Bounds of actual data dimension values.	*/
      float *pv;	/*  Values of requested data dimension.		*/
      float *pb;	/*  Bounds of requested data dimension values.	*/

      {
       int i,j,k,c,n,kd;
       int K1,J1,test,js;
       float *pvf,*pVf,e;

       e=(*d > 1) ? fabs((*(pv+*d-1)-*pv)/(*d-1)*1.e-2): fabs(*pv)*1.e-6;
/*		If jump interval isn't unity set for interpolation.	*/
       c=0;
       if (*jx != 1) c=1;

       K1=*pK;
       J1=0;
       js=0;
       n=0;
       pVf=pV;
       if (e == 0.0) e=1.e-6;

/*		Check whether the requested data is within the domain.	*/

       if ( (*pb-(*pB+*pK**C))*(*pb-(*(pB+*D)+*(pK+1)**C)) > 0.0 || 
	  (*(pb+*d)-(*pB+*pK**C))*(*(pb+*d)-(*(pB+*D)+*(pK+1)**C)) > 0.0)
		return -1;

/*		Loop over all of the requested dimension values.	*/

       for (i=0,pvf=pv;i < *d && c == 0;i++,pvf++)
	 {
/*			Only if the requested value is within the range
			of actual values,
			 (V[0]+K[0]*C) to (V[D-1]+K[1]*C),
			do we need to search.				*/

	  kd=(*pK < *(pK+1))? 1: -1;

/*			Search over all V[j] for each K (but don't repeat
			the test on those that were tested) to find the
			previous value of v.  Assume both V and v are
			monotonic in the same direction, and K
			also has that direction.			*/

	  for (k=K1,j=J1; (test=(k-*pK)*(k-*(pK+1))) <= 0;)
	    {
	     J1=j;
	     if (fabs((double)(*pvf-(*pVf+k**C))) <= e)
	       {

		if (n != 0 && j+kd*k*(*D)-js != 1) c=1;
		js=j+kd*k*(*D);
		n++;
		break;
	       }

	     pVf++;
	     if (++j == *D)
	       {
	        j=0;
		k+=kd;
		K1=k;
		pVf=pV;
	       }
	    }
/*			If a value of v[i] wasn't found then
			interpolation will be needed.			*/

	  if (test > 0) c=1;
/*			The check on whether requested values are within
			the domain of the actual data needs to check
			against boundary limits.			*/

	 }
/*			Return the indicator.				*/

       return c;
      }


/*		Check whether the array data is used for a display.
		If so, a display update is needed.			*/

    int check_d_A(name)
      char *name;
      {
       int i,j;
       int nseg;
       int change;

       struct display_tab *pd;
       struct a_tab *p;
       struct a_attr *pa;
       int *pi;

       pd=&D_tab;
       change=0;
       while (pd != NULL)
	 {
	  /*for (i=0; pd->off != 1 && pd->a[i][0] != '\0';i++)*/
	  /* DNW - 2/5/96 - Took the off check out! This prevented the 
		 	    updating of indices when display is turned off */
	  for (i=0; pd->a[i][0] != '\0';i++)
	    {
	     if (strcmp(pd->a[i],name) == 0)
	       {
		pi=&(pd->F_seg[0]);
		nseg=(&(pd->dsp_seg[0])-&(pd->F_seg[0])+4)/4;
		for (j=0; j < nseg; j++)
		  {
		   pi+=2;
		   *pi++=TRUE;	/* Data exists.				*/
		   *pi++=TRUE;	/* Regenerate the display.		*/
		  }
		if (pd->off ==2) pd->off=0;
		update_ind=1;
		change=1;
	       }
	    }
	  pd=pd->next;
         }
       p=&A_tab;
       while (p != NULL && (pa=p->pA_attr) != NULL)
	 {
	  if (pa->f != NULL && findnam(name,pa->f))
	    {
/*			Modified Jan 4, 1994				*/
/*	     if (check_d_A(p->name) && pa->notok != 0) checkA_attr(p);	*/
	     check_d_A(p->name);
	     checkA_attr(p);
	    }
	  p=p->next;
	 }
       return change;
      }


/*			Check the array attribute set table entry and
			move to the table if it's ok.			*/

    int chk_mov_A (struct a_tab *gtab)

      {
	int i;
	struct a_tab *ptab,*ptb;
	struct display_tab *pd;
	int *pi;
	int I[51];

	for(ptb=ptab=&A_tab;
		ptab != NULL && strcmp(ptab->name,gtab->name) != 0;
						ptb=ptab,ptab=ptab->next);


/*	If it isn't modifying an array table entry then add it.	*/

	if (ptab == NULL)
	  {
	   ptab=gtab;
	   if (checkA_attr(ptab) == 0)
	     {
	      killA(gtab);
	      return 0;
	     }
	   ptb->next=gtab;
	   if (A_tab.pA_attr == NULL)
	     {
	      ptab=&A_tab;
	      ptab->next=NULL;
	      strcpy(ptab->name,gtab->name);
	      ptab->pA_attr=gtab->pA_attr;
	      free((char *)gtab);     
	     }
	   if (!Inactive && fpout != NULL) prtA(fpout,ptab);
	   return 1;
	  }

	if (compareA_attr(gtab,ptab,I) == 0)
	  {
	   killA(gtab);
	   return 0;
	  }

	if (checkA_attr(ptab) == 0)
	  {
	   /*killA(gtab);*/
    	   removeA(gtab->name); /* DNW - have to remove the array struct */
	   return 0;
	  }

/*		Turn on regeneration flags and set to update if needed.*/

	if (I[50])
	  {
/*	   if (check_d_A(ptab->name) == 0)
	     {
	      killA(gtab);
	      return 0;
	     }
*/
	   check_d_A(ptab->name);
	  }
	else
	  {
           for (pd=&D_tab;pd != NULL; pd=pd->next)
	     {
	      if ((pd->a[0] != NULL && strcmp(pd->a[0],gtab->name) == 0) ||
		  (pd->a[1] != NULL && strcmp(pd->a[1],gtab->name) == 0) ||
		  (pd->a[2] != NULL && strcmp(pd->a[2],gtab->name) == 0) ||
		  (pd->a[3] != NULL && strcmp(pd->a[3],gtab->name) == 0) ||
		  (pd->a[4] != NULL && strcmp(pd->a[4],gtab->name) == 0) ||
		  (pd->a[5] != NULL && strcmp(pd->a[5],gtab->name) == 0)  )
	        {
	         for (i=0,pi=&pd->F_seg[3];i<51;i++,pi+=4)
			    if (I[i]) {*pi=1;  update_ind=1;}
		 if (I[5] && (
		     strcmp(pd->type,"Xyvsy") == 0 ||
		     strcmp(pd->type,"Yxvsx") == 0 ||
		     strcmp(pd->type,"XvsY") == 0)
		    )
		   {
		    pd->leg_seg[3]=1;
		    update_ind=1;
		   }
	        }
             }
	  }
	if (!Inactive && fpout != NULL) prtA(fpout,ptab);
	killA(gtab);
	return 1;
      }

/*		Compare attributes of arrays.			*/

    int compareA_attr(struct a_tab *gtab,struct a_tab *ptab,int *I)
      {
	int i,j;
	struct a_attr *pa,*pA;

	for (j=0;j<51;j++) I[j]=0;

	if (ptab == NULL || gtab == NULL) return 0;

	pa=ptab->pA_attr;
	pA=gtab->pA_attr;

	if (pA->aF != NULL || pa->aF != NULL)
	  {
	   if(pA->aF==NULL || pa->aF==NULL || strcmp(pA->aF,pa->aF)!=0)
	     {
	      for (j=0;j<51;j++) I[j]=1;
	      if (pa->aF != NULL) free(pa->aF);
	      pa->aF=pA->aF;
	      pA->aF=NULL;
	     }
	  }
	if (pA->af != NULL || pa->af != NULL)
	  {
	   if(pA->af==NULL || pa->af==NULL || strcmp(pA->af,pa->af)!=0)
	     {
	      for (j=0;j<51;j++) I[j]=1;
	      if (pa->af != NULL) free(pa->af);
	      pa->af=pA->af;
	      pA->af=NULL;
	     }
	  }
	if (pA->almask != NULL || pa->almask != NULL)
	  {
	   if(pA->almask==NULL || pa->almask==NULL ||
					 strcmp(pA->almask,pa->almask)!=0)
	     {
	      for (j=0;j<51;j++) I[j]=1;
	      if (pa->almask != NULL) free(pa->almask);
	      pa->almask=pA->almask;
	      pA->almask=NULL;
	     }
	  }
	if (pA->atrnf != NULL || pa->atrnf != NULL)
	  {
	   if(pA->atrnf==NULL || pa->atrnf==NULL ||
					strcmp(pA->atrnf,pa->atrnf)!=0)
	     {
	      for (j=0;j<51;j++) I[j]=1;
	      if (pa->atrnf != NULL) free(pa->atrnf);
	      pa->atrnf=pA->atrnf;
	      pA->atrnf=NULL;
	     }
	  }
	if (pA->aS != NULL || pa->aS != NULL)
	  {
	   if(pA->aS==NULL || pa->aS==NULL || strcmp(pA->aS,pa->aS)!=0)
	     {
	      for (j=0;j<51;j++) I[j]=1;
	      if (pa->aS != NULL) free(pa->aS);
	      pa->aS=pA->aS;
	      pA->aS=NULL;
	     }
	  }
	if (pA->aN != NULL || pa->aN != NULL)
	  {
	   if(pA->aN==NULL || pa->aN==NULL || strcmp(pA->aN,pa->aN)!=0)
	     {
	      for (j=0;j<51;j++) I[j]=1;
	      if (pa->aN != NULL) free(pa->aN);
	      pa->aN=pA->aN;
	      pA->aN=NULL;
	     }
	  }
	if (pA->aTI != NULL || pa->aTI != NULL)
	  {
	   if(pA->aTI==NULL || pa->aTI==NULL || strcmp(pA->aTI,pa->aTI)!=0)
	     {
	      for (j=0;j<51;j++) I[j]=1;
	      if (pa->aTI != NULL) free(pa->aTI);
	      if (pa->aTI != NULL) pa->aTI=pA->aTI;
	      pA->aTI=NULL;
	     }
	  }
	if (pA->aU != NULL || pa->aU != NULL)
	  {
	   if(pA->aU==NULL || pa->aU==NULL || strcmp(pA->aU,pa->aU)!=0)
	     {
	      for (j=0;j<51;j++) I[j]=1;
	      if (pa->aU != NULL) free(pa->aU);
	      pa->aU=pA->aU;
	      pA->aU=NULL;
	     }
	  }
	if (pA->as != NULL || pa->as != NULL)
	  {
	   if(pA->as==NULL || pa->as==NULL || strcmp(pA->as,pa->as)!=0)
	     {
	      I[4]=1;
	      if (pa->as != NULL) free(pa->as);
	      pa->as=pA->as;
	      pA->as=NULL;
	     }
	  }
	if (pA->an != NULL || pa->an != NULL)
	  {
	   if(pA->an==NULL || pa->an==NULL || strcmp(pA->an,pa->an)!=0)
	     {
	      I[5]=1;
	      if (pa->an != NULL) free(pa->an);
	      pa->an=pA->an;
	      pA->an=NULL;
	     }
	  }
	if (pA->ati != NULL || pa->ati != NULL)
	  {
	   if(pA->ati==NULL || pa->ati==NULL || strcmp(pA->ati,pa->ati)!=0)
	     {
	      I[6]=1;
	      if (pa->ati != NULL) free(pa->ati);
	      pa->ati=pA->ati;
	      pA->ati=NULL;
	     }
	  }
	if (pA->au != NULL || pa->au != NULL)
	  {
	   if(pA->au==NULL || pa->au==NULL || strcmp(pA->au,pa->au)!=0)
	     {
	      I[7]=1;
	      if (pa->au != NULL) free(pa->au);
	      pa->au=pA->au;
	      pA->au=NULL;
	     }
	  }
	if (pA->acrd != NULL || pa->acrd != NULL)
	  {
	   if(pA->acrd==NULL || pa->acrd==NULL || strcmp(pA->acrd,pa->acrd)!=0)
	     {
	      I[8]=1;
	      if (pa->acrd != NULL) free(pa->acrd);
	      pa->acrd=pA->acrd;
	      pA->acrd=NULL;
	     }
	  }
	if (pA->acrt != NULL || pa->acrt != NULL)
	  {
	   if(pA->acrt==NULL || pa->acrt==NULL || strcmp(pA->acrt,pa->acrt)!=0)
	     {
	      I[9]=1;
	      if (pa->acrt != NULL) free(pa->acrt);
	      pa->acrt=pA->acrt;
	      pA->acrt=NULL;
	     }
	  }
	if (pA->acom1 != NULL || pa->acom1 != NULL)
	  {
	   if(pA->acom1==NULL || pa->acom1==NULL ||
					 strcmp(pA->acom1,pa->acom1)!=0)
	     {
	      I[10]=1;
	      if (pa->acom1 != NULL) free(pa->acom1);
	      pa->acom1=pA->acom1;
	      pA->acom1=NULL;
	     }
	  }
	if (pA->acom2 != NULL || pa->acom2 != NULL)
	  {
	   if(pA->acom2==NULL || pa->acom2==NULL ||
					 strcmp(pA->acom2,pa->acom2)!=0)
	     {
	      I[11]=1;
	      if (pa->acom2 != NULL) free(pa->acom2);
	      pa->acom2=pA->acom2;
	      pA->acom2=NULL;
	     }
	  }
	if (pA->acom3 != NULL || pa->acom3 != NULL)
	  {
	   if(pA->acom3==NULL || pa->acom3==NULL ||
					 strcmp(pA->acom3,pa->acom3)!=0)
	     {
	      I[12]=1;
	      if (pa->acom3 != NULL) free(pa->acom3);
	      pa->acom3=pA->acom3;
	      pA->acom3=NULL;
	     }
	  }
	if (pA->acom4 != NULL || pa->acom4 != NULL)
	  {
	   if(pA->acom4==NULL || pa->acom4==NULL ||
					 strcmp(pA->acom4,pa->acom4)!=0)
	     {
	      I[13]=1;
	      if (pa->acom4 != NULL) free(pa->acom4);
	      pa->acom4=pA->acom4;
	      pA->acom4=NULL;
	     }
	  }
	for (i=0;i<NDS;i++)
	  {
	   if (pA->aXN[i] != NULL || pa->aXN[i] != NULL)
	     {
	      if(pA->aXN[i]==NULL || pa->aXN[i]==NULL ||
					strcmp(pA->aXN[i],pa->aXN[i])!=0)
	        {
	         for (j=0;j<51;j++) I[j]=1;
	         if (pa->aXN[i] != NULL) free(pa->aXN[i]);
	         pa->aXN[i]=pA->aXN[i];
	         pA->aXN[i]=NULL;
	        }
	     }
	   if (pA->axn[i] != NULL || pa->axn[i] != NULL)
	     {
	      if(pA->axn[i]==NULL || pa->axn[i]==NULL ||
					 strcmp(pA->axn[i],pa->axn[i])!=0)
	        {
	         I[14+i]=1;
	         if (pa->axn[i] != NULL) free(pa->axn[i]);
	         pa->axn[i]=pA->axn[i];
	         pA->axn[i]=NULL;
	        }
	     }
	   if (pA->axu[i] != NULL || pa->axu[i] != NULL)
	     {
	      if(pA->axu[i]==NULL || pa->axu[i]==NULL ||
					 strcmp(pA->axu[i],pa->axu[i])!=0)
	        {
	         I[18+i]=1;
	         if (pa->axu[i] != NULL) free(pa->axu[i]);
	         pa->axu[i]=pA->axu[i];
	         pA->axu[i]=NULL;
	        }
	     }
	   if (pA->aXK[i] != NULL || pa->aXK[i] != NULL)
	     {
	      if(pA->aXK[i]==NULL || pa->aXK[i]==NULL ||
					 strcmp(pA->aXK[i],pa->aXK[i])!=0)
	        {
	         for (j=0;j<51;j++) I[j]=1;
	         if (pa->aXK[i] != NULL) free(pa->aXK[i]);
	         pa->aXK[i]=pA->aXK[i];
	         pA->aXK[i]=NULL;
	        }
	     }
	   if (pA->axj[i] != NULL || pa->axj[i] != NULL)
	     {
	      if(pA->axj[i]==NULL || pa->axj[i]==NULL ||
					 strcmp(pA->axj[i],pa->axj[i])!=0)
	        {
	         for (j=0;j<51;j++) I[j]=1;
	         if (pa->axj[i] != NULL) free(pa->axj[i]);
	         pa->axj[i]=pA->axj[i];
	         pA->axj[i]=NULL;
	        }
	     }
	   if (pA->aXC[i] != NULL || pa->aXC[i] != NULL)
	     {
	      if(pA->aXC[i]==NULL || pa->aXC[i]==NULL ||
					 strcmp(pA->aXC[i],pa->aXC[i])!=0)
	        {
	         for (j=0;j<51;j++) I[j]=1;
	         if (pa->aXC[i] != NULL) free(pa->aXC[i]);
	         pa->aXC[i]=pA->aXC[i];
	         pA->aXC[i]=NULL;
	        }
	     }
	   if (pA->aXF[i] != NULL || pa->aXF[i] != NULL)
	     {
	      if(pA->aXF[i]==NULL || pa->aXF[i]==NULL ||
					 strcmp(pA->aXF[i],pa->aXF[i])!=0)
	        {
	         for (j=0;j<51;j++) I[j]=1;
	         if (pa->aXF[i] != NULL) free(pa->aXF[i]);
	         pa->aXF[i]=pA->aXF[i];
	         pA->aXF[i]=NULL;
	        }
	     }
	   if (pA->aXL[i] != NULL || pa->aXL[i] != NULL)
	     {
	      if(pA->aXL[i]==NULL || pa->aXL[i]==NULL ||
					 strcmp(pA->aXL[i],pa->aXL[i])!=0)
	        {
	         for (j=0;j<51;j++) I[j]=1;
	         if (pa->aXL[i] != NULL) free(pa->aXL[i]);
	         pa->aXL[i]=pA->aXL[i];
	         pA->aXL[i]=NULL;
	        }
	     }
	   if (pA->axf[i] != NULL || pa->axf[i] != NULL)
	     {
	      if(pA->axf[i]==NULL || pa->axf[i]==NULL ||
					 strcmp(pA->axf[i],pa->axf[i])!=0)
	        {
	         for (j=0;j<51;j++) I[j]=1;
	         if (pa->axf[i] != NULL) free(pa->axf[i]);
	         pa->axf[i]=pA->axf[i];
	         pA->axf[i]=NULL;
	        }
	     }
	   if (pA->axl[i] != NULL || pa->axl[i] != NULL)
	     {
	      if(pA->axl[i]==NULL || pa->axl[i]==NULL ||
					 strcmp(pA->axl[i],pa->axl[i])!=0)
	        {
	         for (j=0;j<51;j++) I[j]=1;
	         if (pa->axl[i] != NULL) free(pa->axl[i]);
	         pa->axl[i]=pA->axl[i];
	         pA->axl[i]=NULL;
	        }
	     }
	   if (pA->axv[i] != NULL || pa->axv[i] != NULL)
	     {
	      if(pA->axv[i]==NULL || pa->axv[i]==NULL ||
					 strcmp(pA->axv[i],pa->axv[i])!=0)
	        {
	         for (j=0;j<51;j++) I[j]=1;
	         if (pa->axv[i] != NULL) free(pa->axv[i]);
	         pa->axv[i]=pA->axv[i];
	         pA->axv[i]=NULL;
	        }
	     }
	   if (pA->axb[i] != NULL || pa->axb[i] != NULL)
	     {
	      if(pA->axb[i]==NULL || pa->axb[i]==NULL ||
					 strcmp(pA->axb[i],pa->axb[i])!=0)
	        {
	         for (j=0;j<51;j++) I[j]=1;
	         if (pa->axb[i] != NULL) free(pa->axb[i]);
	         pa->axb[i]=pA->axb[i];
	         pA->axb[i]=NULL;
	        }
	     }
	   if (pA->axw[i] != NULL || pa->axw[i] != NULL)
	     {
	      if(pA->axw[i]==NULL || pa->axw[i]==NULL ||
					 strcmp(pA->axw[i],pa->axw[i])!=0)
	        {
	         for (j=0;j<51;j++) I[j]=1;
	         if (pa->axw[i] != NULL) free(pa->axw[i]);
	         pa->axw[i]=pA->axw[i];
	         pA->axw[i]=NULL;
	        }
	     }
	  }
	return 1;
      }

    int prtA(FILE *fp,struct a_tab *ptab)
      {
       int j,k,c,m;
       char **pc;
       struct a_attr *pA_;
       struct l_tab *ptl;

	if (ptab == NULL) return 0;
	if ( (k=strlen(ptab->name)) != 0 && (pA_=ptab->pA_attr) != NULL)
	  {
	   k+=fprintf (fp,"A_%s(",ptab->name);
	   m=0;

	   for (pc=&pA_->aF,j=0;
		  pc <= &pA_->axu[NDS-1]; j++, pc++)
	     if (*pc != NULL && **pc != '\0')
	       {
		if ( m++ != 0)
		  {
		   if (k > 72) k=fprintf (fp,",\n   ");
		   else k+=fprintf (fp,",");
		  }
		k+=fprintf (fp,"%s=\"%s\"", A_strg[j], *pc);
	       }
/*			Print wrap coefficint limits.			*/

	   for (pc=&pA_->aXK[0],j=0; j < NDS; j++, pc++)
	      if (*pc != NULL && **pc != '\0')
		{
		 if ( m++ != 0)
		   {
		    if (k > 72) k=fprintf (fp,",\n   ");
		    else k+=fprintf (fp,",");
		   }
		 k+=fprintf (fp,"%s=(%s)",A_intg[1][j],*pc);
		}

/*			Print jump intervals.			*/

	   for (pc=&pA_->axj[0],j=0; j < NDS; j++, pc++)
	      if (*pc != NULL && **pc != '\0')
		{
		 if ( m++ != 0)
		   {
		    if (k > 72) k=fprintf (fp,",\n   ");
		    else k+=fprintf (fp,",");
		   }
		 k+=fprintf (fp,"%s=%s",A_intg[3][j],*pc);
		}

/*		Print lower case values.				*/

	   for (pc=&pA_->aXC[0],j=0; j < NDS*5 ; j++, pc++)
	      if (*pc != NULL && **pc != '\0')
		{
		 if ( m++ != 0)
		   {
		    if (k > 72) k=fprintf (fp,",\n   ");
		    else k+=fprintf (fp,",");
		   }
		 k+=fprintf (fp,"%s=%s", A_sflt[j/NDS][j%NDS], *pc);
		};
/*		Print dimension values, bounds, weights.		*/

	   for (pc=&pA_->axv[0],j=0; j < NDS*3; j++, pc++)
	      if (*pc != NULL)
		{
		 if ( m++ != 0)
		   {
		    if (k > 72) k=fprintf (fp,",\n   ");
		    else k+=fprintf (fp,",");
		   }
		 m=0;
		 if (strncmp(*pc,"L_",2) == 0) m=1;
		 else if (strlen(*pc) <= (size_t) 17)
		   {
		    ptl=&L_tab[0];
		    while (ptl != NULL && (c=cmpnbl(*pc,ptl->name)) != 0)
			 ptl=ptl->next;
		    if (c == 0) m=1;
		   }
		 if (m > 0)
			k+=fprintf (fp,"%s=%s",A_vflt[3+j/NDS][j%NDS],*pc);
		 else
			k+=fprintf (fp,"%s=(%s)", A_vflt[3+j/NDS][j%NDS],*pc);
		};
	   fprintf (fp,")\n");

	  }
	return 1;
      }

/*		Compute latitudes and grid boundaries for the LMD
		"equal area grid" for checking and setting values
		and boundaries.  Computed North to South.

		The space must be available for n values of lat and
		n+1 values of bound.					*/

    int lmd_grid (
	float *flat,	/* Input latitudes that are to be compared.	*/
	float *grid_bnd,/* Output bounds.  Returns n+1 values.		*/
	float *grid_wt,	/* Output weights.  Returns n values.		*/
	int n)		/* Number of input latitudes.			*/

      {
       int i,j;
       double size;
       double sinlat;
       double b;
/*			These were computed as below and inserted here
			to avoid unnecessary repetition of computing.	*/

       double lat[]={	-78.5217,-70.0516,-64.1581,-59.3166,-55.0848,
			-51.2606,-47.7314,-44.4270,-41.2999,-38.3161,
			-35.4505,-32.6836,-30.0000,-27.3871,-24.8346,
			-22.3337,-19.8769,-17.4576,-15.0701,-12.7090,
			-10.3698,-8.04785,-5.73917,-3.43981,-1.14599,
			1.14599,3.43981,5.73917,8.04785,10.3698,
			12.7090,15.0701,17.4576,19.8769,22.3337,
			24.8346,27.3871,30.0000,32.6836,35.4505,
			38.3161,41.2999,44.4270,47.7314,51.2606,
			55.0848,59.3166,64.1581,70.0516,78.5217
		    };

       double bound[]={	-90.0000,-73.7398,-66.9261,-61.6424,-57.1401,
			-53.1301,-49.4642,-46.0545,-42.8436,-39.7918,
			-36.8699,-34.0558,-31.3323,-28.6854,-26.1039,
			-23.5782,-21.1002,-18.6629,-16.2602,-13.8865,
			-11.5370,-9.20690,-6.89210,-4.58857,-2.29244,
			0.00000,2.29244,4.58857,6.89210,9.20690,
			11.5370,13.8865,16.2602,18.6629,21.1002,
			23.5782,26.1039,28.6854,31.3323,34.0558,
			36.8699,39.7918,42.8436,46.0545,49.4642,
			53.1301,57.1401,61.6424,66.9261,73.7398, 90.0000
		       };

/*       double lat[50];
       double bound[51];

       double rtd = 180.0/3.1415926536;

       sinlat=1.0;
       bound[0]=90.0;
       size=(double)2.0/((double)50.0);
       for (i=0;i < 25;i++)
	 {
	  b = asin(sinlat-size);
	  lat[i]=asin(sinlat-size*0.5)*rtd;
	  bound[i+1]=b*rtd;
	  sinlat=sinlat-size;
	 }
       if (50%2 == 1) {lat[i]=0.0; bound[i+1]=-bound[i]; i++;}
       else bound[i]=0.0;

       for (j=i-1; i < 50; i++,j--)
	 {
	  lat[i]=-lat[j];
	  bound[i+1]=-bound[j];
	 }
*/

       if (n <= 1 || n > 50) return 0;

/*		Now check the input latitudes.				*/

       for (i=0;i<50;i++)
	 {
	  if (n > 50-i) return 0;

	  if (fabs((double)flat[0]-lat[i]) <= 0.001)
	    {
/*		If they're in the same order.				*/

	     if ((flat[1]-flat[0])*(lat[1]-lat[0]) > 0.0)
	       for (j=0;j<n;j++)
	         {
		  if (fabs((double)flat[j]-lat[i+j]) > 0.001) return 0;
		  grid_bnd[j]=bound[i+j];
		  grid_bnd[j+1]=bound[i+j+1];
		  grid_wt[j]=0.04;
	         }
/*		If they're in opposite order.				*/

	     else
	       for (j=0;j<n;j++)
	         {
		  if (fabs((double)flat[j]-lat[i-j]) > 0.001) return 0;
		  grid_bnd[j]=bound[i-j+1];
		  grid_bnd[j+1]=bound[i-j];
		  grid_wt[j]=0.04;
	         }

	     return 1;
	    }
	 }
       return 0;
      }
