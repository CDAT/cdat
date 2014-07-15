#ifdef HDF

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "hdf.h"
#include "netcdf.h"
#include "my_access_mode.h"

#define STRMAX 256

    extern FILE *fpin,*fpout,*fperr;

    extern char HDF_file[1024];
    extern int Inactive;
    extern char dirbase[1024];
    extern struct a_tab A_tab;

/*	Process a store of an array data table entry in a HDF file.  The
	command syntax is:

        HDF(array data attribute set name, HDF dictionary file name,
		append / replace).*/

    int procHDF(str,tok)

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

/*		Store the array data as HDF.				*/

	storeHDF(strm,strf,append);
	return 1;
       }
/*		Store a variable in a HDF file.				*/

    int storeHDF(char *aname,char *filename,int append)

      {
	typedef char var_name[MAX_NC_NAME];
	var_name *var_names, *unique_var_names;
	int i,j,k,n,found,uneq[NDS],undata,append_it[NDS];
	int32 *dimIDs;
	int32 sd_id, sds_id, n_datasets, n_file_attrs, index, status;
        int32 dim_sizes[MAX_VAR_DIMS];
        int32 rank, num_type, attributes, istat;
	int32 data_type;
	int32 *var_start, *dimSizes, *start, *edges;
	char deflt[13]="default.hdf";
	char name[MAX_NC_NAME];
	double xd;
	char *pc, end_num[10];
	struct a_attr *pA;
	struct a_tab *pa;
	int check_if_same_hdf2_dimension();


/*		Find the array data.					*/
	for (i=0; i < NDS; ++i)
		append_it[i] = 1;

	for(pa=&A_tab;pa!=NULL&&strcmp(pa->name,aname)!=0;pa=pa->next);

	if (pa == NULL || (pA=pa->pA_attr) == NULL || pA->notok != 0)
	  {
	   err_warn(1,fperr,"Error - nothing to store in HDF.\n");
	   return 0;
	  }
	xtend(filename,".hdf");
	for(i=0;filename[i]!='\0';i++)
	  {
	   HDF_file[i]=filename[i];
	   HDF_file[i+1]='\0';
	  }
	if (i == 0)
	  {
	   err_warn(0,fperr,
		"Warning - no file assigned for HDF output, "
			"%s/default.hdf used.\n",dirbase);
	   for (i=0,pc=dirbase;i<1023 && *pc!='\0';pc++,i++) HDF_file[i]=*pc;
	   HDF_file[i++]='/';
	   for (pc=deflt;i<1023 && *pc!='\0';pc++,i++) HDF_file[i]=*pc;
	   HDF_file[i]='\0';
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

/*		Acquire the data that is to be output to HDF.		*/
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
	   strcpy(unique_var_names[i], pA->xn[i]);/* copy the dimension names */
	strcpy(unique_var_names[i], pA->n); /* copy the variable name 	*/

/*		Open the HDF file.  If it exists, extend it.		*/
	if (access(HDF_file,F_OK) == 0)
	  {
	   if (append)
	     {
	      if ((sd_id=SDstart(HDF_file,DFACC_RDWR)) == -1)
	        {
	         err_warn(1,fperr,
			 "Error - the HDF file (%s) is not extendable.\n",
		          HDF_file);
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


/*		Get variable and dimensions names in the file.		*/
/* 		Determine the contents of the file. 			*/
        	istat = SDfileinfo(sd_id, &n_datasets, &n_file_attrs);

/* 		Access the name of every data set in the file. 		*/
		var_names = (var_name *) malloc(n_datasets*sizeof(var_name));
        	for (index = 0; index < n_datasets; index++) {
                   sds_id = SDselect(sd_id, index);
                   istat = SDgetinfo(sds_id, var_names[index], &rank, dim_sizes,
                                        &num_type, &attributes);
                   istat = SDendaccess(sds_id);
        	}

/*		Search for uniqueness in the names. If the name is
		found in the table, then add delta to insure uniqueness. */
	        j = 0;
		k = 0;
                n = strlen(unique_var_names[j]);
		while (j < pA->ND +1) {
                   found = 0;
	           append_it[j] = 1;
                   for (i = 0; i < n_datasets; ++i) {
                      if (strcmp(var_names[i], unique_var_names[j]) == 0) {
                        found = 1;
                        ++k;
                      }
	           }
                   if (found == 1) {
		      if (!check_if_same_hdf2_dimension(pA, sd_id, j,
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

/*    If the file already exists, erase old contents and enter new contents.*/
	      if ((sd_id=SDstart(HDF_file,DFACC_CREATE)) == -1)
	        {
	         err_warn(1,fperr,
		   "Error - file (%s) not replaceable. Check file permissions.\n",
		    HDF_file);
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
			"Info - appended to HDF file (%s).\n",HDF_file);
	   else err_warn(0,fperr,
			"Info - replaced the HDF file (%s).\n",HDF_file);
	  }
	else
	  {
	   if ((sd_id=SDstart(HDF_file,DFACC_CREATE)) == -1)
	     {
	      err_warn(1,fperr,"Error - cannot create file (%s) HDF\n",
		HDF_file);
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
	   err_warn(0,fperr,"Info - created the HDF file (%s).\n",HDF_file);
	  }

/*			Define dimension size and space			*/
        dimSizes = (int32 *)malloc((pA->ND)*sizeof(int32));
        start    = (int32 *)malloc((pA->ND)*sizeof(int32));
        edges    = (int32 *)malloc((pA->ND)*sizeof(int32));

/* 			Create the array data set. 			*/
	if (pA->ty[0] == 'R')
	   data_type = DFNT_FLOAT32;
	else if (pA->ty[0] == 'I')
	   data_type = DFNT_INT32;
	else if (pA->ty[0] == 'C')
	   data_type = DFNT_INT8;
	for (i=0;i<pA->ND;i++)
             dimSizes[pA->ND-i-1] = (long) (*pA->xs[i]);
        sds_id = SDcreate(sd_id, unique_var_names[pA->ND], data_type, pA->ND, 
			  dimSizes);

/* 		Define the location, pattern, and size of the data set 	*/
        for (i = 0; i < pA->ND; i++) {
                start[i] = 0;
                edges[i] = dimSizes[i];
                /*edges[i] = dimSizes[i];*/
        }

/*			Store dimensions.    				*/
	dimIDs = (int32 *)malloc(pA->ND*sizeof(int32));
	for (i=0;i<pA->ND;i++)
	    {
	     if (append_it[pA->ND-i-1]==1) { /* append dimension to the file */
	        dimIDs[pA->ND-i-1] = SDgetdimid(sds_id, i);
		istat = SDsetdimname(dimIDs[pA->ND-i-1], unique_var_names[pA->ND-i-1]);
		istat = SDsetdimscale(dimIDs[pA->ND-i-1], dimSizes[i],
                                      DFNT_FLOAT32, (VOIDP) pA->xv[pA->ND-i-1]);
                istat = SDsetdimstrs(dimIDs[pA->ND-i-1], unique_var_names[pA->ND-i-1],
                                     pA->xu[pA->ND-i-1], '\0');
             } else {
	        dimIDs[pA->ND-i-1] = SDgetdimid(sds_id, i);
		istat = SDsetdimname(dimIDs[pA->ND-i-1], unique_var_names[pA->ND-i-1]);
	     }
	    }

/*			Store Variable Attribues			*/
	istat = SDsetattr(sds_id, "source", DFNT_CHAR8, strlen(pA->s)+1,
			  (VOIDP) pA->s);
	istat = SDsetattr(sds_id, "name", DFNT_CHAR8, 
                          strlen(unique_var_names[pA->ND])+1,
			  (VOIDP) unique_var_names[pA->ND]);
	istat = SDsetattr(sds_id, "title", DFNT_CHAR8, strlen(pA->ti)+1,
			  (VOIDP) pA->ti);
	istat = SDsetattr(sds_id, "units", DFNT_CHAR8, strlen(pA->u)+1,
			  (VOIDP) pA->u);
	istat = SDsetattr(sds_id, "type", DFNT_CHAR8, strlen(pA->ty)+1,
			  (VOIDP) pA->ty);
	istat = SDsetattr(sds_id, "date", DFNT_CHAR8, strlen(pA->crd)+1,
			  (VOIDP) pA->crd);
	istat = SDsetattr(sds_id, "time", DFNT_CHAR8, strlen(pA->crt)+1,
			  (VOIDP) pA->crt);

/* 		Write the stored data to the variable data set. 
		The fifth argument must be explicitly cast to a 
		generic pointer to conform to the HDF API definition
 		for SDwritedata. 					*/
        istat = SDwritedata(sds_id, start, NULL, edges, (VOIDP)pA->un.data);

/* 		Terminate access to the array. 				*/
        istat = SDendaccess(sds_id);

/* 		Terminate access to the SD interface and close the file.*/
        istat = SDend(sd_id);

	free((char *) unique_var_names);
	free((char *) dimIDs);
	free((char *) dimSizes);
	free((char *) start);
	free((char *) edges);


/*			Print the procedure out to the script		*/
        if (!Inactive && fpout != NULL) 
            prtHDF(fpout,aname,HDF_file,append);

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
/*		Print HDF command.					*/

    int prtHDF (fp,aname,filename,app)
      FILE *fp;
      char *aname;
      char *filename;
      int app;
      {
       if (app) fprintf (fp,"HDF(%s,%s,append)\n",aname,filename);
       else     fprintf (fp,"HDF(%s,%s,replace)\n",aname,filename);
       return 1;
      }

/*		Compare variable dimension to HDF file dimension.	*/
    int check_if_same_hdf2_dimension(pA, file_id, j, dimname)
      struct a_attr *pA;
      int32 file_id;
      int j;
      char *dimname;
      {
	int	i, ier, coords_same = 1;
	int32	sds_id, sds_idx, retn, dimid, count, num_attrs;
	int32   dimsize, datatype, nattrs;
        float 	*coor_vals;
        char    dim_name[MAX_NC_NAME], name[MAX_NC_NAME], units[MAX_NC_NAME];
	char	format[MAX_NC_NAME];
	VOIDP   data;

/*		Get dimension index and id.	 			*/
	sds_idx = SDnametoindex (file_id, dimname);	
	sds_id = SDselect(file_id, sds_idx);
	if (sds_id == -1) {
	   SDendaccess (sds_id);
	   return 0; /* Error in receiving the id. Therfore, return 0.	*/
	}

/*              Check to see if it is a dimension.                      */
	retn = SDiscoordvar(sds_id);
	if (retn == 0) {
	   SDendaccess (sds_id);
	   return 0; /* dimname must be a variable. Therfore, return 0. */
	}


/* 		Get the dimension identifier 				*/
     	dimid = SDgetdimid (sds_id, 0);

/*		Get dimension information and attributes		*/
	retn = SDdiminfo(dimid, dim_name, &dimsize, &datatype, &nattrs);
	retn = SDreadattr(sds_id, 0, name);
	retn = SDreadattr(sds_id, 1, units);

/* 		Compare dimension attribute 'name' string.		*/
	if ( (pA->xn[j] == NULL) ||
             (strncmp(pA->xn[j], name, strlen(pA->xn[j])) != 0) ) {
           ier = SDendaccess(sds_id); /* Terminate access to the array 	*/
	   return 0;
	}

/* 		Compare dimension attribute 'units' string.		*/
	if ( (pA->xu[j] == NULL) ||
           (strncmp(pA->xu[j], units, strlen(pA->xu[j])) != 0) ) {
           ier = SDendaccess(sds_id); /* Terminate access to the array 	*/
	   return 0;
	}

/*		Compare dimension size.					*/
	if ( (pA->xs[j] == NULL) ||
             (*pA->xs[j] != dimsize) ) {
           ier = SDendaccess(sds_id); /* Terminate access to the array 	*/
	   return 0;
	}

	if (datatype == 0) { 
           ier = SDendaccess(sds_id); /* No scale information. 		*/
	   return 0;
	}

/*		Get coordinates from the file. 				*/
    	coor_vals = (float *) malloc(sizeof(float)*(int)dimsize);
	ier = SDgetdimscale(dimid, (VOIDP) coor_vals);

/*		Compare coordinate values.				*/
	i = 0;
	while ((i < dimsize) && (coords_same)) {
	   if (pA->xv[j][i] != coor_vals[i])
              coords_same = 0;
	   ++i;
	}

	free (coor_vals); /* remove the coordinate values 		*/

        ier = SDendaccess(sds_id); /* Terminate access to the array 	*/
	return (coords_same);
      }

#elif OSF1

       ; /* Do nothing. This preveents the empty file warning for the DEC compile. */

#endif
