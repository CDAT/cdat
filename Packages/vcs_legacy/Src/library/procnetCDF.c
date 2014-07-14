#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "netcdf.h"
#include "my_access_mode.h"


#define STRMAX 256
#define TRUE 1
#define FALSE 0

    extern FILE *fpin,*fpout,*fperr;

    extern char netCDF_file[1024];
    extern int Inactive;
    extern char dirbase[1024];
    extern struct a_tab A_tab;

/*	Process a store of an array data table entry in a netCDF file.  The
	command syntax is:

        netCDF(array data attribute set name, netCDF dictionary file name,
		append / replace).*/

    int procnetCDF(str,tok)

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

/*		Store the array data as netCDF.				*/

	storenetCDF(strm,strf,append);
	return 1;
       }
/*		Store a variable in a netCDF file.			*/

    int storenetCDF(char *aname,char *filename,int append)

      {
	nc_type var_type;
	int ncid,ier,i,j,k,n,found;
	int ndims, nvars, ngatts, recdim;
	int *dimIDs, *varIDs;
	int uneq[NDS],indf,indl,undata, append_it[NDS];
	int var_ndims, var_natts, var_shape[MAX_VAR_DIMS];
	long *var_start, *dimSizes;
	double xd;
	typedef char var_name[MAX_NC_NAME];
	var_name *var_names, *unique_var_names;
	char *pc, end_num[10];
	char deflt[12]="default.nc";
	struct a_attr *pA;
	struct a_tab *pa;
	char tmp_name[17];
	int check_if_same_dimension();


/*		Find the array data.					*/
	for (i=0; i < NDS; ++i)
		append_it[i] = 1;

	for(pa=&A_tab;pa!=NULL&&strcmp(pa->name,aname)!=0;pa=pa->next);

	if (pa == NULL || (pA=pa->pA_attr) == NULL || pA->notok != 0)
	  {
	   err_warn(1,fperr,"Error - nothing to store in netCDF.\n");
	   return 0;
	  }
	xtend(filename,".nc");
	for(i=0;filename[i]!='\0';i++)
	  {
	   netCDF_file[i]=filename[i];
	   netCDF_file[i+1]='\0';
	  }
	if (i == 0)
	  {
	   err_warn(0,fperr,
		"Warning - no file assigned for netCDF output, "
			"%s/default.nc used.\n",dirbase);
	   for (i=0,pc=dirbase;i<1023 && *pc!='\0';pc++,i++) netCDF_file[i]=*pc;
	   netCDF_file[i++]='/';
	   for (pc=deflt;i<1023 && *pc!='\0';pc++,i++) netCDF_file[i]=*pc;
	   netCDF_file[i]='\0';
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

/*		Acquire the data that is to be output to netCDF.	*/
	undata=0;
	if (pA->un.data == NULL)
	  {
	   if (acquire_A(pa,pA->ty) == 0)
	     {
	      err_warn (1,fperr,
		   "Error - trying to acquire array data (A_%s).\n",pa->name);
	      killA_tmp();
	      return 0;
	     }
	   killA_tmp();
	   undata=1;
	  }

/*		Set the variable and dimension names.			*/
	unique_var_names=(var_name *) malloc((pA->ND+1)*sizeof(var_name));
	for (i=0; i < pA->ND; ++i)
	   strcpy(unique_var_names[i], pA->xn[i]); /* copy the dimension names */
	strcpy(unique_var_names[i], pA->n); /* copy the variable name */

/*		Open the netCDF file.  If it exists, extend it.		*/
	if (access(netCDF_file,F_OK) == 0)
	  {
	   if (append)
	     {
	      if ((ncid=ncopen(netCDF_file,NC_WRITE)) == -1)
	        {
	         err_warn(1,fperr,
			 "Error - the netCDF file (%s) is not extendable.\n",
		          netCDF_file);
		 if (undata)
		   {
		    if (pA->un.data != NULL) free(pA->un.data);
		    pA->un.data=NULL;
		    if (pA->mask != NULL) free(pA->mask);
		    pA->mask=NULL;
		   }
		 free((char *) unique_var_names);
	         return 0;
	     	}

		ncredef(ncid); /* Put open netCDF file into Define Mode. */

/*		Get variable and dimensions names in the file.		*/
                ncinquire(ncid, &ndims, &nvars, &ngatts, &recdim);
		var_names = (var_name *) malloc(nvars*sizeof(var_name));
	        for (i = 0; i < nvars; ++i)
	            ncvarinq(ncid, i, var_names[i], &var_type, &var_ndims, 
                             var_shape, &var_natts);

/*		Search for uniqueness in the names. If the name is found in the
		table, then add delta to insure uniqueness. */
	        j = 0;
		k = 0;
                n = strlen(unique_var_names[j]);
		while (j < pA->ND +1) {
                   found = 0;
	           append_it[j] = 1;
                   for (i = 0; i < nvars; ++i) {
                      if (strcmp(var_names[i], unique_var_names[j]) == 0) {
                        found = 1;
                        ++k;
                      }
	           }
                   if (found == 1) {
		      if (!check_if_same_dimension(pA, j, ncid,
                                         unique_var_names[j])) {
                         sprintf(end_num, "%d", k);
                         unique_var_names[j][n] = '\0';
                         strcat(unique_var_names[j], end_num);
                      } else {
	                append_it[j] = 0; /* do not add dimension to file */
                         ++j;
		         k = 0;
                         n = strlen(unique_var_names[j]);
		      }
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

/*	If the file already exists, erase old contents and enter new contents. */
	      if ((ncid=nccreate(netCDF_file,NC_CLOBBER)) == -1)
	        {
	         err_warn(1,fperr,
		   "Error - file (%s) not replaceable. Check file permissions.\n",
		    netCDF_file);
		 if (undata)
		   {
		    if (pA->un.data != NULL) free(pA->un.data);
		    pA->un.data=NULL;
		    if (pA->mask != NULL) free(pA->mask);
		    pA->mask=NULL;
		   }
		 free((char *) unique_var_names);
	         return 0;
	     	}
	     }
	   if (append) err_warn(0,fperr,
			"Info - appended to netCDF file (%s).\n",netCDF_file);
	   else err_warn(0,fperr,
			"Info - replaced the netCDF file (%s).\n",netCDF_file);
	  } else {
	   if ((ncid=nccreate(netCDF_file,NC_CLOBBER)) == -1)
	     {
	      err_warn(1,fperr,"Error - cannot create file (%s) netCDF\n",
		netCDF_file);
	      if (undata)
	        {
		 if (pA->un.data != NULL) free(pA->un.data);
		 pA->un.data=NULL;
		 if (pA->mask != NULL) free(pA->mask);
		 pA->mask=NULL;
		}
	      free((char *) unique_var_names);
	      return 0;
	     }
	   err_warn(0,fperr,"Info - created the netCDF file (%s).\n",netCDF_file);
	  }

/*			Store dimensions.    				*/
	dimIDs = (int *)malloc(pA->ND*sizeof(int));
	varIDs = (int *)malloc((pA->ND+1)*sizeof(int));
        dimSizes = (long *)malloc((pA->ND)*sizeof(long));
	for (i=0;i<pA->ND;i++)
	    {
             dimSizes[pA->ND-i-1] = (long) (*pA->xs[i]);
	     if (append_it[i] == 1) { /* append dimension to the file */
	       dimIDs[pA->ND-i-1]=ncdimdef(ncid,unique_var_names[i],
                                        (long)*pA->xs[i]);/*Create a Dim*/

	       varIDs[pA->ND-i-1] = ncvardef(ncid,unique_var_names[i],NC_FLOAT,1,
                                        &dimIDs[pA->ND-i-1]);

	       if ((dimIDs[pA->ND-i-1] == -1) || (varIDs[pA->ND-i-1] == -1))
	           err_warn(1,fperr,"Error - Cannot append to netCDF file (%s).\n",netCDF_file);
/* 			Store dimension attributes.  			*/
               ncattput(ncid,varIDs[pA->ND-i-1],"units",NC_CHAR,
                        strlen(pA->xu[i])+1,pA->xu[i]);
	     } else
	       dimIDs[pA->ND-i-1] = ncdimid(ncid, unique_var_names[i]);
	    }

/*			Store Variable.    				*/
	if (pA->ty[0] == 'R')
	     varIDs[i]=ncvardef(ncid,unique_var_names[i],NC_FLOAT,pA->ND,dimIDs);
	else if (pA->ty[0] == 'I')
	     varIDs[i]=ncvardef(ncid,unique_var_names[i],NC_SHORT,pA->ND,dimIDs);
	else if (pA->ty[0] == 'C')
	     varIDs[i]=ncvardef(ncid,unique_var_names[i],NC_CHAR,pA->ND,dimIDs);
	else /* assume it is float if it is not defined */
	     varIDs[i]=ncvardef(ncid,unique_var_names[i],NC_FLOAT,pA->ND,dimIDs);

/*			Store Variable Attribues			*/
        ncattput(ncid,varIDs[i],"source",NC_CHAR,strlen(pA->s)+1,pA->s);
        ncattput(ncid,varIDs[i],"name",NC_CHAR,
                 strlen(unique_var_names[i])+1,unique_var_names[i]);
        ncattput(ncid,varIDs[i],"title",NC_CHAR,strlen(pA->ti)+1,pA->ti);
        ncattput(ncid,varIDs[i],"units",NC_CHAR,strlen(pA->u)+1,pA->u);
        ncattput(ncid,varIDs[i],"type",NC_CHAR,strlen(pA->ty)+1,pA->ty);
        ncattput(ncid,varIDs[i],"date",NC_CHAR,strlen(pA->crd)+1,pA->crd);
        ncattput(ncid,varIDs[i],"time",NC_CHAR,strlen(pA->crt)+1,pA->crt);

/*			We are done 'defining'. Leaving define mode.  	*/
	ncendef(ncid);

/*	Allocate the start coordinate array, the size of the max number of
        dimensions we are writing.					*/
	var_start = (long *)malloc(pA->ND*sizeof(long));
        for(j=0;j<pA->ND;j++) var_start[j] = 0;
 	for (i=0;i<pA->ND;i++) {
	   if (append_it[pA->ND-i-1] == 1) {
	     ncvarput(ncid,varIDs[i],var_start,&dimSizes[i],
                   pA->xv[pA->ND-i-1]);
           }
        }
	
/* 			Assign the data to the variable 		*/
	ncvarput(ncid,varIDs[i],var_start,dimSizes,pA->un.data);

/* 	Close the NetCDF file 					*/
	ncclose(ncid);
	free((char *) unique_var_names);

/*			Print the procedure out to the script		*/
        if (!Inactive && fpout != NULL) 
            prtnetCDF(fpout,aname,netCDF_file,append);

/* 	Free the data and mask values 					*/
        if (undata)
          {
           if (pA->un.data != NULL) free(pA->un.data);
           pA->un.data=NULL;
           if (pA->mask != NULL) free(pA->mask);
           pA->mask=NULL;
          }
	return 1;
      }
/*		Print netCDF command.					*/

    int prtnetCDF (fp,aname,filename,app)
      FILE *fp;
      char *aname;
      char *filename;
      int app;
      {
       if (app) fprintf (fp,"netCDF(%s,%s,append)\n",aname,filename);
       else     fprintf (fp,"netCDF(%s,%s,replace)\n",aname,filename);
       return 1;
      }

/*		Compare variable dimension to netCDF file dimension.	*/
    int check_if_same_dimension(pA, j, ncid, dimname)
      struct a_attr *pA;
      int j;
      int ncid;
      char *dimname;
      {
	int	i, ier, ndims, natts, dimid, att_len, coords_same = 1;
	long 	dim_size, start[1], end[1];
	char 	att_name[MAX_NC_NAME], *att_val;
        float 	*coor_vals;
	nc_type att_type;


/*		Get dimension attributes. 				*/
	dimid = ncdimid(ncid, dimname);
	if (dimid == -1)
	   return 0; /* dimname must be a variable. Therfore return 0. */

        ier = ncvarinq(ncid, dimid, 0, 0, &ndims, 0, &natts);
        ier = ncattname(ncid, dimid, (natts-1), att_name);
    	ier = ncattinq(ncid, dimid, att_name, &att_type, &att_len);
    	att_val = (char *) malloc((att_len+1) * sizeof(char));
    	ier = ncattget(ncid, dimid, att_name, (void *) att_val);

/* 		Compare dimension attribute 'units' string.		*/
	if (strcmp(pA->xu[j], att_val) != 0) {
	   free (att_val); /* remove the attribute values */
	   return 0;
	}
	free (att_val);

/*		Get dimension size from the file. 			*/
    	ier = ncdiminq(ncid, dimid, (char *) 0, &dim_size);

/*		Compare dimension size.					*/
	if (*pA->xs[j] != dim_size)
	   return 0;

/*		Get coordinates from the file. 			*/
    	coor_vals = (float *) malloc(sizeof(float)*(int)dim_size);
    	start[0] = 0;
    	end[0]   = dim_size;
	ncendef(ncid); /* Leave define mode. */
    	ier = ncvarget(ncid, dimid, start, end, (void *)coor_vals);

/*		Compare coordinate values.				*/
	i = 0;
	while ((i < dim_size) && (coords_same)) {
	   if (pA->xv[j][i] != coor_vals[i])
              coords_same = 0;
	   ++i;
	}

	free (coor_vals); /* remove the coordinate values */
	ncredef(ncid); /* Put open netCDF file back into Define Mode. */
	return (coords_same);
      }
