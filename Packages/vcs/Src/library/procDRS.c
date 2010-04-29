#ifdef DRS

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "drscdf.h"
#include "my_access_mode.h"

#define STRMAX 256
#define TRUE 1
#define FALSE 0


    extern FILE *fpin,*fpout,*fperr;

    extern char DRS_file[1024];
    extern int Inactive;
    extern char dirbase[1024];
    extern struct a_tab A_tab;

/*	Process a store of an array data table entry in a DRS file.  The
	command syntax is:

        DRS(array data attribute set name, DRS dictionary file name,
		append / replace).*/

    int procDRS(str,tok)

      char str[257];
      int *tok;

      {
	int c,d,e;
	int tokm,tokf,tokz;
	int append;

	char strm[20];
	char strf[257];
	char strz[257];

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }

	c=getsttk(strm,&tokm);
	d=getsttk(strf,&tokf);
	if (c==0 || d==0 || (tokf!=')' && tokf!=',' ) )
	  {
	   err_warn(1,fperr,"Error - syntax for (%s%c%s%c%s%c).\n",str,*tok,
				strm,tokm,strf,tokf);
	   return 0;
	  }
	if (tokf == ',')
	  {
	   e=getsttk(strz,&tokz);
	   if (strz[0] == 'a' || strz[0] == 'A') append=1;
	   else append=0;
	  }
	else append=1;

/*		Store the array data as DRS.				*/

	storeDRS(strm,strf,append);
	return 1;
       }
/*		Store a variable in a DRS file.				*/

    int storeDRS(char *aname,char *filename,int append)

      {
	int i,j,k,n,nd,num_vars_dims=0,intspace_temp, value;
	int uneq[NDS],indf,indl,ier,ierreof,undata;
	int found, nvars=0, append_it[NDS];
	float first, last;
	double xd;
	char *pc, end_num[10];
	char deflt[12]="default.dic";
	struct a_attr *pA;
	struct a_tab *pa;
	char tempfile[1025];
	char tmp_name[17];

	typedef char var_name[STRMAX];
        var_name *var_names, *unique_var_names=NULL;
	char snam[STRMAX],tmp_src[STRMAX],tmp_tit[STRMAX],tmp_un[STRMAX];
        char tmp_dat[STRMAX],tmp_tim[STRMAX],tmp_typ[STRMAX];
	extern int check_if_same_dimension();


/*		Find the array data.					*/

	for(pa=&A_tab;pa!=NULL&&strcmp(pa->name,aname)!=0;pa=pa->next);

	if (pa == NULL || (pA=pa->pA_attr) == NULL || pA->notok != 0)
	  {
	   err_warn(1,fperr,"Error - nothing to store in DRS.\n");
	   return 0;
	  }
	xtend(filename,".dic");
	for(i=0;filename[i]!='\0';i++)
	  {
	   DRS_file[i]=filename[i];
	   DRS_file[i+1]='\0';
	  }
	if (i == 0)
	  {
	   err_warn(0,fperr,
		"Warning - no file assigned for DRS output, "
			"%s/default.dic used.\n",dirbase);
	   for (i=0,pc=dirbase;i<1023 && *pc!='\0';pc++,i++) DRS_file[i]=*pc;
	   DRS_file[i++]='/';
	   for (pc=deflt;i<1023 && *pc!='\0';pc++,i++) DRS_file[i]=*pc;
	   DRS_file[i]='\0';
	  }
	for (i=0;i<pA->ND;i++)
	  {
	   uneq[i]=0;
	   if (*pA->xs[i] > 2)
	     {
	      xd=fabs((double)((*(pA->xv[i]+*pA->xs[i]-1)-*pA->xv[i])/
							(*pA->xs[i]-1)));
	      for (j=0;j<*pA->xs[i]-1;j++)
	        {
	         if(fabs(fabs((double)(*(pA->xv[i]+j+1)-*(pA->xv[i]+j)))-xd) >
							 0.0001*xd)
		   {
		    uneq[i]=1;
		    break;
		   }
	        }
	     }
	  }
/*			Acquire the data that is to be output to DRS.	*/

	undata=0;
	if (pA->un.data == NULL)
	  {
	   if (acquire_A(pa,pA->ty) == 0)
	     {
	      Cllun(12);
	      err_warn (1,fperr,
		   "Error - trying to acquire array data (A_%s).\n",pa->name);
	      killA_tmp();
	      return 0;
	     }
	   killA_tmp();
	   undata=1;
	  }
/*              Set the variable and dimension names.                   */
        unique_var_names=(var_name *) malloc((pA->ND+1)*sizeof(var_name));
        for (i=0; i < pA->ND; ++i)
           strcpy(unique_var_names[i], pA->xn[i]);/*copy the dimension names*/
        strcpy(unique_var_names[i], pA->n); /* copy the variable name */

/*		Open the DRS file.  If it exists extend it.		*/

	if (access(DRS_file,F_OK) == 0)
	  {
	   if (append)
	     {
	      if ((k=Aslun(12,DRS_file,13," ",IDRS_EXTEND)) != IDRS_SUCCESS)
	        {
	         err_warn(1,fperr,
			"Error - file (%s) not extendable DRS # - %d.\n",
								DRS_file,k);
		 if (undata)
		   {
		    if (pA->un.data != NULL) free(pA->un.data);
		    pA->un.data=NULL;
		    if (pA->mask != NULL) free(pA->mask);
		    pA->mask=NULL;
		   }
                 if (unique_var_names != NULL) 
                    free((char *) unique_var_names);
	         return 0;
	     	}

/*              First get number of variables and dimensions in DRS file.   */
                ier    = Cluvdb();
           	ierreof = Inqdict( 12, IDRS_GETFIRSTVAR);
                ier = Getname( tmp_src, snam, tmp_tit, tmp_un, 
                               tmp_dat, tmp_tim, tmp_typ, &nd );
                while ( (ierreof != 1037) && (ier == 0) ) {
                   ++nvars;
                   num_vars_dims = num_vars_dims + nd + 1;
           	   ierreof = Inqdict( 12, IDRS_GETNEXTVAR);
                   if (ierreof != 1037) {
                      ier = Getname( tmp_src, snam, tmp_tit, tmp_un, 
                                  tmp_dat, tmp_tim, tmp_typ, &nd );
                   }
		}

/*		Get all the variable and dimension names in the DRS file.  */
		var_names = (var_name *)malloc(num_vars_dims*sizeof(var_name));
                ier    = Cluvdb();
                j = 0;
           	ierreof = Inqdict( 12, IDRS_GETFIRSTVAR);
                ier = Getname( tmp_src, var_names[j], tmp_tit, tmp_un, 
                               tmp_dat, tmp_tim, tmp_typ, &nd );
		trimbl(var_names[j],strlen(var_names[j]));
                while ( (ierreof != 1037) && (ier == 0) ) {
                   for (i=1; i <= nd; ++i) {
                      ++j;
		      ier = Getedim(i, tmp_src, var_names[j], tmp_tit, tmp_un,
                                 &intspace_temp, &value,&first, &last );
		      trimbl(var_names[j],strlen(var_names[j]));
                   }
                   ierreof = Inqdict( 12, IDRS_GETNEXTVAR);
                   if (ierreof != 1037) {
                      ++j;
                      ier = Getname(tmp_src, var_names[j], tmp_tit, tmp_un,
                                    tmp_dat, tmp_tim, tmp_typ, &nd );
		      trimbl(var_names[j],strlen(var_names[j]));
		   }
		}

/*              Search for uniqueness in the names. If the name is found in the
                table, then add delta to insure uniqueness. */
                j = 0;
                k = 0;
                n = strlen(unique_var_names[j]);
                while (j < pA->ND +1) {
                   found = 0;
                   append_it[j] = 1;
                   for (i = 0; i < num_vars_dims; ++i) {
                      if (strcmp(var_names[i], unique_var_names[j]) == 0) {
                        found = 1;
                        ++k;
                      }
                   }

                   if (found == 1) {
                      /*if (!check_if_same_dimension(pA, j, ncid,
                                         unique_var_names[j])) {*/
                         sprintf(end_num, "%d", k);
                         unique_var_names[j][n] = '\0';
                         strcat(unique_var_names[j], end_num);
                      /*} else {
                        append_it[j] = 0; * do not add dimension to file *
                         ++j;
                         k = 0;
                         n = strlen(unique_var_names[j]);
                      }*/
                   } else {
                      ++j;
                      k = 0;
                      if (j < (pA->ND+1))
                         n = strlen(unique_var_names[j]);
                   }
                }
                free((char *) var_names);
	     }
	   else
	     {
/*			Remove the .dic and .dat files.			*/

	      remove(DRS_file);
	      for (i=0;DRS_file[i] != '\0';i++) tempfile[i]=DRS_file[i];
	      tempfile[i-2]='a'; tempfile[i-1]='t';
	      tempfile[i]='\0';
	      remove(tempfile); 
	      if ((k=Aslun(12,DRS_file,13," ",IDRS_CREATE)) != IDRS_SUCCESS)
	        {
	         err_warn(1,fperr,
			"Error - file (%s) not replaceable DRS # - %d.\n",
								DRS_file,k);
		 if (undata)
		   {
		    if (pA->un.data != NULL) free(pA->un.data);
		    pA->un.data=NULL;
		    if (pA->mask != NULL) free(pA->mask);
		    pA->mask=NULL;
		   }
                 if (unique_var_names != NULL) 
                    free((char *) unique_var_names);
	         return 0;
	     	}
	     }
	   if (append) err_warn(0,fperr,
			"Info - append to DRS file (%s).\n",DRS_file);
	   else err_warn(0,fperr,
			"Info - replace the DRS file (%s).\n",DRS_file);
	  }
	else
	  {
	   if ((k=Aslun(12,DRS_file,13," ",IDRS_CREATE)) != IDRS_SUCCESS)
	     {
	      err_warn(1,fperr,"Error - cannot create file (%s) DRS # - %d\n",
		DRS_file,k);
	      if (undata)
	        {
		 if (pA->un.data != NULL) free(pA->un.data);
		 pA->un.data=NULL;
		 if (pA->mask != NULL) free(pA->mask);
		 pA->mask=NULL;
		}
              if (unique_var_names != NULL) 
                  free((char *) unique_var_names);
	      return 0;
	     }
	   err_warn(0,fperr,"Info - create the DRS file (%s).\n",DRS_file);
	  }
	for (i=0;i<pA->ND;i++)
	  if (uneq[i] == 1)
	    {
	     Cluvdb();
	     strncpy(tmp_name,pA->xn[i],17); tmp_name[16]='\0'; /*/DNW-4/29/96*/
	     ier = Setname(pA->s,tmp_name,tmp_name,pA->XU[i]," ");
	     if (Inqdict(12,IDRS_GETFIRSTVAR) == IDRS_SUCCESS) continue;
	     Cluvdb();
	     ier = Setname(pA->s,tmp_name,tmp_name,pA->XU[i]," ");
	     if((ier=Putvdim(12,*pA->xs[i],pA->xv[i],&indf,&indl)) !=
								IDRS_SUCCESS)
	       {
	        Cllun(12);
	        err_warn (1,fperr,
		   "Error - DRS # %d, trying to store DRS dimension in (%s).\n",
								ier,DRS_file);
		if (undata)
		  {
		   if (pA->un.data != NULL) free(pA->un.data);
		   pA->un.data=NULL;
		   if (pA->mask != NULL) free(pA->mask);
		   pA->mask=NULL;
		  }
                if (unique_var_names != NULL) 
                    free((char *) unique_var_names);
	        return 0;
	       }
	    }
	Cluvdb ();
	strncpy(tmp_name,unique_var_names[pA->ND],17); tmp_name[16] = '\0';
	ier = Setname (pA->s,tmp_name,pA->ti,pA->u,pA->ty);
	for (i=0;i<pA->ND;i++)
	  {
	   strncpy(tmp_name,unique_var_names[i],17); /*DNW - 10/21/97*/
           tmp_name[16] = '\0';/*/DNW-4/29/96*/
	   if (uneq[i] == 0) Setdim(i+1,tmp_name,pA->XU[i],*pA->xs[i],
		(double)*pA->xv[i],(double)*(pA->xv[i]+*pA->xs[i]-1));
	   else
	     {
	      Setvdim(i+1,pA->s,tmp_name,tmp_name,pA->XU[i],
			(double)*pA->xv[i],(double)*(pA->xv[i]+*pA->xs[i]-1));
	     }
	  }
	if (pA->un.data != NULL && (ier=Putdat(12,pA->un.data)) != IDRS_SUCCESS)
	  {
	   Cllun(12);
	   err_warn (1,fperr,
		"Error - DRS # %d, trying to DRS store data in (%s).\n",
					ier,DRS_file);
	   if (undata)
	     {
	      if (pA->un.data != NULL) free(pA->un.data);
	      pA->un.data=NULL;
	      if (pA->mask != NULL) free(pA->mask);
	      pA->mask=NULL;
	     }
           if (unique_var_names != NULL) 
               free((char *) unique_var_names);
	   return 0;
	  }

	if (!Inactive && fpout != NULL) prtDRS (fpout,aname,DRS_file,append);
	Cllun(12);
        if (unique_var_names != NULL) 
            free((char *) unique_var_names);
	if (undata)
	  {
	   if (pA->un.data != NULL) free(pA->un.data);
	   pA->un.data=NULL;
	   if (pA->mask != NULL) free(pA->mask);
	   pA->mask=NULL;
	  }
	return 1;
      }
/*		Print DRS command.					*/

    int prtDRS (fp,aname,filename,app)
      FILE *fp;
      char *aname;
      char *filename;
      int app;
      {
       if (app) fprintf (fp,"DRS(%s,%s,append)\n",aname,filename);
       else     fprintf (fp,"DRS(%s,%s,replace)\n",aname,filename);
       return 1;
      }

#elif OSF1

       ; /* Do nothing! This preveents the empty file warning for the DEC compile. */

#endif
