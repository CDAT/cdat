/* -*-Mode: C;-*-
 * Module:      cdunif DRS driver functions
 *
 * Copyright:	1994, Regents of the University of California
 *		This software may not be distributed to others without
 *		permission of the author.
 *
 * Author:      Bob Drach, Lawrence Livermore National Laboratory
 *              drach@llnl.gov
 *
 * Version:     $Id$
 *
 * Revision History:
 *
 * $Log: cdunifdrs.c,v $
 * Revision 1.14  1997/12/03  22:22:11  drach
 * - In cdunifdrs.c, dimensions which are reversed or subsetted wrt a
 *   coordinate dimension are now treated as local.
 * - Added cdDimGetDouble to cdmsslab.c
 * - Fixed wraparound, reading wraparound dimensions in cdmsslab.c
 *
 * Revision 1.13  1996/09/09  18:18:59  drach
 * - Integrate with configuration scripts
 *
 * Revision 1.12  1995/10/16  18:52:52  drach
 * - Added CuInt type, for DEC Alpha version
 * - Support -Ddrs flag on compilation
 *
 * Revision 1.11  1995/06/26  17:45:08  drach
 * Reset DRS error reporting to cddrs level, in case Seterr(.,IDRS_WARNING)
 * was issued directly to DRS, to avoid spurious 'no more variables' warnings
 *
 * Revision 1.10  1995/03/31  05:31:38  drach
 * Initial version
 *
 * Revision 1.9  1995/03/15  02:40:21  drach
 * Solaris port
 *
 * Revision 1.8  1995/02/15  20:56:05  drach
 * - Fixed floating-point overflow bug
 *
 * Revision 1.7  1995/01/18  02:51:59  drach
 * - Made seterropts a dispatch function
 *
 * Revision 1.6  1994/12/20  23:09:08  drach
 * - Added success return on cuvarget
 *
 *
 * Revision 1.3  1994/12/17  00:37:49  drach
 * - Use cugetlu, cufreelu to handle logical units
 *
 * Revision 1.2  1994/11/18  00:13:32  drach
 * Added error processing routines and externs.
 *
 *
 */

#ifdef drs
#include <cdunifint.h>
#include <drscdf.h>

#if (defined(sun) && OS_MAJOR==4)
                                   /* Kluge to fix SunOS 4.1.3 bug */
int MAIN_;
#endif

int cuLastDrsErr;			     /* Most recent DRS error */
static int cuErrOptSet = 0;	             /* True iff user called cuseterropts */

					     /* Open a DRS file */
int cuopenread_drs(const char* controlpath, const char* datapath){
	CuFile* file;			     /* CDMS file struct */
	CuVar* var;			     /* CDMS var struct */
	CuDim* dim;			     /* CDMS dimension struct */
	CuAtt* att;			     /* CDMS attribute struct */
	const char *datafile;		     /* Datafile name for DRS */
	int dictlu, datalu;		     /* Dictionary/data logical units */
	int nvars;			     /* Number of variables */
	int err, reterr;		     /* DRS error returns */
	float version;			     /* DRS version number */
	DRS_FILENAME dataf;		     /* Name of DRS data file from inquiry */
	DRS_SOURCE source, dsource;	     /* Variable, dimension source */
	DRS_NAME name, dname;		     /* Variable, dimension name */
	DRS_TITLE title, dtitle;	     /* Variable, dimension title */
	DRS_UNITS units, dunits;	     /* Variable, dimension units */
	DRS_DATE date;			     /* Variable date written */
	DRS_TIME time;			     /* Variable time written */
	DRS_TYPE typed;			     /* Variable datatype (character string) */
	int ndims;			     /* Variable number of dimensions */
	int etype;			     /* Variable datatype (enum) */
	int bpe;			     /* Variable bytes per element */
	int dtype;			     /* Dimension is vector or first/last/len representation? */
	int dlen;			     /* Dimension length */
	float dfirst, dlast;		     /* Dimension first/last values */
	float vfirst;			     /* Dimension variable first value */
	double ddfirst;			     /* Dimension first value (double) */
	int i,j,k;			     /* Loop indices */
	int dimfound;			     /* Dimension lookup flag */

					     /* Set default error options */
	cuseterropts_drs(cuErrOpts);
	
					     /* Open the file */
	datafile = (datapath ? datapath : " ");
	if(cugetlu(&dictlu,&datalu)==-1)
		return -1;
	
	if(Drstest(cuLastDrsErr=Aslun(dictlu,(char*)controlpath,datalu,(char*)datafile,IDRS_READ))){
		CuError(CU_EOPEN,"Opening DRS file %s",controlpath);
		cufreelu(dictlu);
		cufreelu(datalu);
		return -1;
	}
					     /* Create the file struct */
	if((file = CuCreateFile(CuDrs))==(CuFile*)0){
		cufreelu(dictlu);
		cufreelu(datalu);
		return -1;
	}

					     /* Get the number of variables */
	if(Drstest(cuLastDrsErr=Inqlun(dictlu,dataf,&nvars,&version)))
		goto error;
	custrtrim(dataf);

					     /* Fill the file struct */
	strncpy(file->controlpath,controlpath,CU_MAX_PATH);
	strncpy(file->datapath,datafile,CU_MAX_PATH);
	file->internid1=dictlu;
	file->internid2=datalu;
	file->nvars=nvars;
					     /* Create the variable array */
	if((var = CuCreateVars(file,nvars))==(CuVar*)0)
		goto error;

					     /* Create the dimension array: */
					     /* Assume worst case of four dimensions per var*/
	if((dim = CuCreateDims(file,4*nvars))==(CuDim*)0)
		goto error;

					     /* For each variable ... */
	file->ndims = 0;
	Cluvdb();
					     /* Note:spurious no more variables warnings */
					     /* are suppressed by initial cuseterropts_drs call */
					     /* which disallows IDRS_WARNING report level for DRS */

	err = cuLastDrsErr=Inqdict(dictlu,IDRS_GETFIRSTVAR);
	for(i=0; (i<nvars) && (err!=IDRS_NOMOREVARS) && !(reterr=Drstest(err)) ; i++, var++){
		if(Drstest(cuLastDrsErr=Getname(source,name,title,units,date,time,typed,&ndims)))
			goto error;
		custrtrim(source);
		custrtrim(name);
		custrtrim(title);
		custrtrim(units);
		custrtrim(date);
		custrtrim(time);
		custrtrim(typed);
		if(Drstest(cuLastDrsErr=Getelemd(&etype,&bpe)))
			goto error;
		
					     /* Fill the variable struct */
		strncpy(var->name,name,CU_MAX_NAME);
		switch(etype){
		  case IDRS_IEEE_R4:
		  case IDRS_CRAY_R8:
			var->datatype = CuFloat;
			break;
		  case IDRS_I4:
#if defined(__alpha) || defined(__ia64) || defined(__x86_64__)
			var->datatype = CuInt;
			break;
#endif
		  case IDRS_I8:
			var->datatype = CuLong;
			break;
		  case IDRS_ASCII:
		  case IDRS_USER:
			var->datatype = CuByte;
			break;
		  default:
			CuError(CU_EINTERN,"Illegal datatype %d for variable %s, file %s",etype,var->name,file->controlpath);
			goto error;
		}
		var->ndims = ndims;
		var->natts = 5;
					     /* Create the attribute array */
		if((att = CuCreateAtts(file,var,var->natts))==(CuAtt*)0)
			goto error;

					     /* Fill the attribute array */
					     /* 0 = source */
					     /* 1 = title */
					     /* 2 = units */
					     /* 3 = date */
					     /* 4 = time */
		if(CuSetAtt(file,var,0,"source",CuChar,strlen(source)+1,source) != CU_SUCCESS)
			goto error;
		if(CuSetAtt(file,var,1,"title",CuChar,strlen(title)+1,title) != CU_SUCCESS)
			goto error;
		if(CuSetAtt(file,var,2,"units",CuChar,strlen(units)+1,units) != CU_SUCCESS)
			goto error;
		if(CuSetAtt(file,var,3,"date",CuChar,strlen(date)+1,date) != CU_SUCCESS)
			goto error;
		if(CuSetAtt(file,var,4,"time",CuChar,strlen(time)+1,time) != CU_SUCCESS)
			goto error;

					     /* Inquire dimension information */
		for(j=0; j<ndims; j++){
			if(Drstest(cuLastDrsErr=Getedim(j+1, dsource, dname, dtitle, dunits, &dtype, &dlen, &dfirst, &dlast)))
				goto error;
			custrtrim(dsource);
			custrtrim(dname);
			custrtrim(dtitle);
			custrtrim(dunits);

					     /* If variable is a dimension vector, add to dimensions list */
					     /* and cache pointer to associated coordinate variable */
					     /* NB!! Dimension vectors only have one dimensions, so */
					     /* this only gets executed once for the variable. */
			if(!strcmp(dname,"internal")){
				strncpy(dim->name, var->name, CU_MAX_NAME);
				strncpy(dim->units, units, CU_MAX_NAME);
				dim->var = (CuVar*)0;
				dim->coord = var;
				dim->len = dlen;
				dim->datatype = CuFloat;
				dim->dimtype = CuGlobalDim;
				dim->internid = 0;
				dim->spacing = IDRS_UNEQUALLY_SPACED;
				var->dims[0] = file->ndims++; /* Only one dimension for dimension variable */

					     /* Get the first value of the VARIABLE */
				if(Drstest(cuLastDrsErr=Setname(source,name,title,units," ")))
					return -1;
				if(Drstest(cuLastDrsErr=Setdim(1," "," ",0,(double)dfirst,(double)dfirst)))
					return -1;
				if(Drstest(cuLastDrsErr=Getdat(file->internid1,(int *)&vfirst,cutypelen(CuFloat))))
					return -1;
				dim->first = vfirst;

				dim++;
			}
					     /* If the dimension is global, it has already been */
					     /* added via the dimension variable, so just */
					     /* set the variable dimension index */
			else if(dtype == IDRS_UNEQUALLY_SPACED){
				for(k=0, dimfound=0; k<file->ndims; k++)
					if(file->dims[k].dimtype == CuGlobalDim
					   && (!strcmp(file->dims[k].name,dname))
					   && (!strcmp(file->dims[k].units,dunits))){
						dimfound=1;
						ddfirst = dfirst;
						if(ddfirst==file->dims[k].first && dlen==file->dims[k].len)
							var->dims[var->ndims-j-1]=k; /* C majority is canonical */
						else{
					     /* Dimension is local, equally-spaced */
							strncpy(dim->name,dname,CU_MAX_NAME);
							strncpy(dim->units,dunits,CU_MAX_NAME);
							dim->var = var;
							dim->coord = (CuVar*)0;
							dim->len = dlen;
							dim->datatype = CuFloat;
							dim->dimtype = CuLocalDim;
							dim->internid = j;
							dim->spacing = IDRS_UNEQUALLY_SPACED;
							dim->first = (double)dfirst;
							var->dims[var->ndims-j-1] = file->ndims++; /* C majority is canonical */
							dim++;
						}
						break;
					}
				if(!dimfound){
					CuError(CU_EINTERN,"Cannot find dimension %s for variable %s, file %s",dname,var->name,file->controlpath);
					goto error;
				}
			}
			else {
					     /* Dimension is local, equally-spaced */
				strncpy(dim->name,dname,CU_MAX_NAME);
				strncpy(dim->units,dunits,CU_MAX_NAME);
				dim->var = var;
				dim->coord = (CuVar*)0;
				dim->len = dlen;
				dim->datatype = CuFloat;
				dim->dimtype = CuLocalDim;
				dim->internid = j;
				dim->spacing = IDRS_EQUALLY_SPACED;
				dim->first = (double)dfirst;
				dim->interval = (dlen==1 ? 0.0 : ((double)dlast - (double)dfirst)/((double)dlen-1.0));
				var->dims[var->ndims-j-1] = file->ndims++; /* C majority is canonical */
				dim++;
			}
		}

					     /* End variable loop */
		err = cuLastDrsErr=Inqdict(dictlu,IDRS_GETNEXTVAR);
	}
					     /* Create the global attribute array */
	file->ngatts=3;
	if((att = CuCreateAtts(file,(CuVar*)0,file->ngatts))==(CuAtt*)0)
		goto error;

					     /* Set the global attributes */
					     /* 0 = format */
					     /* 1 = datafile */
					     /* 2 = version */
	if(CuSetAtt(file,(CuVar*)0,0,"format",CuChar,4,"DRS") != CU_SUCCESS)
		goto error;
	if(CuSetAtt(file,(CuVar*)0,1,"datafile",CuChar,strlen(dataf)+1,dataf) != CU_SUCCESS)
		goto error;
	if(CuSetAtt(file,(CuVar*)0,2,"version",CuFloat,1,(void*)&version) != CU_SUCCESS)
		goto error;
	
					     /* Successful return */
	return file->id;
					     /* Error return */
  error:
	cuLastDrsErr=Cllun(dictlu);
	cufreelu(dictlu);
	cufreelu(datalu);
	return -1;
}
int cuclose_drs(CuFile* file){
	if(Drstest(cuLastDrsErr=Cllun(file->internid1)))
		return -1;
	cufreelu(file->internid1);
	cufreelu(file->internid2);
	return CU_SUCCESS;
}
int cudimget_drs(CuFile* file, int dimid, void* value){
	CuDim* dim;
	CuVar* var;
	DRS_SOURCE source;
	DRS_NAME name;
	DRS_TITLE title;
	DRS_UNITS units;
	char source2[CU_MAX_NAME];
	char title2[CU_MAX_NAME];
	char units2[CU_MAX_NAME];
	int nbytes;
	int datatype;
	int retlen;
	
	if((dim=CuLookupDim(file,dimid))==(CuDim*)0)
		return -1;

	Cluvdb();
					     /* See comment below */
	if(dim->dimtype == CuGlobalDim)
		var = dim->coord;
	else
		var = dim->var;
		
	if(cuattget_gen(file,var->id,"source",source2))
		return -1;
	strncpy(source,source2,IDRS_SOURCELEN);
	if(source[0]=='\0') strcpy(source," ");
	
	strncpy(name,var->name,IDRS_NAMELEN);
	
	if(cuattget_gen(file,var->id,"title",title2))
		return -1;
	strncpy(title,title2,IDRS_TITLELEN);
	if(title[0]=='\0') strcpy(title," ");
	
	if(cuattget_gen(file,var->id,"units",units2))
		return -1;
	strncpy(units,units2,IDRS_UNITSLEN);
	if(units[0]=='\0') strcpy(units," ");

	nbytes=dim->len * cutypelen(dim->datatype);

	if(Drstest(cuLastDrsErr=Setname(source,name,title,units," ")))
		return -1;

					     /* If the dimension is global, read the associated variable, */
					     /* otherwise inquire the 'parent' variable */
	if(dim->dimtype == CuGlobalDim){
		var = dim->coord;
		if(Drstest(cuLastDrsErr=Getdat(file->internid1,(int*)value,nbytes)))
			return -1;
	}
	else{
		var = dim->var;
		if(Drstest(cuLastDrsErr=Inqdict(file->internid1,IDRS_GETFIRSTVAR)))
			return -1;
		if(Drstest(cuLastDrsErr=Getcdim(dim->internid+1,source,name,title,units,&datatype,nbytes,(float*)value,&retlen)))
			return -1;
	}

	return CU_SUCCESS;
			   
}
int cuvarget_drs(CuFile* file, int varid, const long start[], const long count[], void* value){
	CuDim* dim;
	CuVar* var;
	DRS_NAME name;
	DRS_SOURCE source;
	DRS_TITLE title;
	DRS_UNITS units;
	char source2[CU_MAX_NAME];
	char title2[CU_MAX_NAME];
	char units2[CU_MAX_NAME];
	float dfirst[4], dlast[4];
	float* dimvals;
	int istart, iend, i;
	int nbytes;

					     /* Lookup the variable */
	if((var = CuLookupVar(file,varid)) == (CuVar*)0)
		return -1;

					     /* For each dimension get corresponding */
					     /* first and last coordinate vector values */
					     /* Canonical C majority */
	nbytes = cutypelen(var->datatype);
	for(i=var->ndims-1; i>=0; i--){
		nbytes *= count[i];
		istart = start[i];
		iend = start[i]+count[i]-1;
		if((dim = CuLookupDim(file,var->dims[i]))==(CuDim*)0)
			return -1;
		
					     /* Check that index ranges are valid */
		if(istart<0 || istart>=dim->len){
			CuError(CU_EINVALCOORDS,"Invalid start index = %d, file %s, variable %s, dimension %d",
				istart,file->controlpath,var->name,i);
			return -1;
		}
		if(iend<istart || iend>=dim->len){
			CuError(CU_EINVALCOORDS,"Invalid count = %d, file %s, variable %s, dimension %d",
				count[i],file->controlpath,var->name,i);
			return -1;
		}

					     /* If the dimensions is equally-spaced, calculate first/last values */
		if(dim->spacing == IDRS_EQUALLY_SPACED){
			dfirst[i] = dim->first + istart * dim->interval;
			dlast[i] = dim->first + iend*dim->interval;
		}

					     /* Else read the dimension to get the first/last values */
		if(dim->spacing == IDRS_UNEQUALLY_SPACED){
			if((dimvals = (float*)(malloc(dim->len*cutypelen(dim->datatype))))==(float*)0){
				CuError(CU_SERROR,"Allocating %d bytes for dimension read, file %s, variable %s, dimension %d",
					dim->len*cutypelen(dim->datatype),file->controlpath,var->name,i);
				return -1;
			}
			if(cudimget_drs(file,var->dims[i],(void*)dimvals)==-1)
				return -1;
			dfirst[i] = dimvals[istart];
			dlast[i] = dimvals[iend];
			free(dimvals);
		}
	}
		
					     /* Clear the VDB and set names */

	Cluvdb();
	if(cuattget_gen(file,var->id,"source",source2))
		return -1;
	strncpy(source,source2,IDRS_SOURCELEN);
	if(source[0]=='\0') strcpy(source," ");
	
	strncpy(name,var->name,IDRS_NAMELEN);
	
	if(cuattget_gen(file,var->id,"title",title2))
		return -1;
	strncpy(title,title2,IDRS_TITLELEN);
	if(title[0]=='\0') strcpy(title," ");
	
	if(cuattget_gen(file,var->id,"units",units2))
		return -1;
	strncpy(units,units2,IDRS_UNITSLEN);
	if(units[0]=='\0') strcpy(units," ");

	if(Drstest(Setname(source,name,title,units," ")))
		return -1;

					     /* Set the dimension limits */
					     /* Note canonical C majority */
	for(i=var->ndims-1; i>=0; i--){
		if(Drstest(cuLastDrsErr=Setdim(var->ndims-i," "," ",0,dfirst[i],dlast[i]))){
			CuError(CU_DRIVER,"Setting dimension limits for file %s, variable %s",
				file->controlpath,var->name);
			return -1;
		}

	}
					     /* Read the data */
	if(Drstest(cuLastDrsErr=Getdat(file->internid1,(int*)value,nbytes))){
		CuError(CU_DRIVER,"Reading data for file %s, variable %s",file->controlpath,var->name);
		return -1;
	}
	return CU_SUCCESS;
}
void cuseterropts_drs(int erropts){
	
	if(erropts & CU_VERBOSE)
		Seterr(0,IDRS_FATAL);
	else
		Seterr(0,IDRS_NOREPORT);

	cuErrOptSet = 1;		     /* Don't reset DRS error option on openread */
	return;
}
#endif
					     /* Trim leading and trailing blanks from s, in place; */
					     /* s must already be null-terminated */
					     /* Characters are shifted left so that first */
					     /* nonblank character of s is the first character; */
					     /* Null is set after last nonblank character. */
char *custrtrim(char* s)
{
	char *t,*u;

				/* Trim leading blanks */
	for(t=u=s; *t==' '; t++);
	while(*u++ = *t++);
				/* Trim trailing blanks */
	for(u-=2; u>=s && *u==' '; u--);
	*++u='\0';
	return(s);
}
