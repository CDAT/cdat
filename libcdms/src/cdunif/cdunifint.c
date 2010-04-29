/* -*-Mode: C;-*-
 * Module:      cdunifint - cdunif internal functions
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
 * $Log: cdunifint.c,v $
 * Revision 1.3  1998/07/02 23:40:49  drach
 * - Added support for absolute time, via routines cdAbs2Comp, cdComp2Abs, cdDecodeRelativeTime, and cdDecodeAbsoluteTime
 * - Added support for the LANL POP ocean data format
 *
 * Revision 1.2  1998/01/15 20:51:49  drach
 * - Added missing_value attribute to GrADS driver
 * - Fixed a memory leak in GrADS driver
 *
 * Revision 1.1.1.1  1997/12/09 18:57:40  drach
 * Copied from cirrus
 *
 * Revision 1.9  1997/11/24  17:28:31  drach
 * - Added QL package to cdunif
 * - Added NdimIntersect function to CDMS
 *
 * Revision 1.8  1997/09/26  21:47:41  drach
 * - Added caching of netCDF unlimited dimension
 *
 * Revision 1.7  1997/01/06  17:47:38  drach
 * - Added HDF to cdunif
 *
 * Revision 1.6  1996/10/31  23:53:34  drach
 * - Cleaned up error returns
 *
 * Revision 1.5  1995/10/16  18:53:51  drach
 * - Modify mask on magic cookie, DEC Alpha version
 *
 * Revision 1.4  1995/09/15  21:08:27  drach
 * - Modified for Cray
 * - removed errno argument, to avoid possible conflict with external errno
 *
 * Revision 1.3  1994/12/17  00:38:40  drach
 * - Improve error messages
 *
 * Revision 1.2  1994/11/18  00:13:33  drach
 * Added error processing routines and externs.
 *
 * Revision 1.1  1994/11/17  19:58:42  drach
 * Initial CVS version
 *
 *
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <ctype.h>
#include <cdunifint.h>

#define CU_LINE_BUF_LEN 81

static CuFile* cuFileListHead = (CuFile *)0; /* Head of file list */
static int cuNextFileID = 0;		     /* File ID of next file created */
int cuLastError = CU_SUCCESS;	             /* Last error */

/* Create a file struct of the specified type and place on the file list */
CuFile* CuCreateFile(CuFileType filetype){
	CuFile *file;

	if((file = (CuFile*)malloc(sizeof(CuFile))) == (CuFile*)0){
		CuError(CU_SERROR,"Creating a file");
		return (CuFile *)0;
	}

					     /* Add file to head of list */
	file->next = cuFileListHead;
	cuFileListHead = file;
					     /* Initialize file */
	file->id = cuNextFileID++;
	file->filetype = filetype;
	file->ndims = file->nvars = file->ngatts = 0;
	file->dims = (CuDim*)0;
	file->vars = (CuVar*)0;
	file->atts = (CuAtt*)0;
	file->recdim = -1;
	file->recdimcache = (void *)0;
	
	return file;
}

/* Lookup a file in the file list */
CuFile* CuLookupFile(int fileid){
	CuFile *file;

	for(file=cuFileListHead; file; file = file->next){
		if(file->id == fileid)
			return file;
	}
	CuError(CU_EBADID,"Looking up file %d",fileid);
	return (CuFile*)0;
}

/* Delete a file created with CuCreateFile */
int CuDeleteFile(int fileid){
	CuFile *p, *file;
	int found;
					     /* Lookup the file and remove from list if found */
	found = 0;
	if(cuFileListHead && cuFileListHead->id == fileid){ /* 1 file open */
		file = cuFileListHead;
		cuFileListHead = file->next;
		found = 1;
	}
	else if(cuFileListHead){	     /* >1 file open */
		for(p=cuFileListHead; p->next; p = p->next){
			if(p->next->id == fileid){
				file = p->next;
				p->next = file->next;
				found = 1;
				break;
			}
		}
	}
					     /* Found == 1 iff lookup succeeded */
	if(found==0){
		CuError(CU_EBADID,"Deleting file %d",fileid);
		return -1;
	}
					     /* Delete dimensions if necessary */
	if(file->dims) free(file->dims);
					     /* Delete variables if necessary */
	if(file->vars)
		CuDeleteVars(file);
					     /* Delete global attributes if necessary */
	if(file->ngatts>0 && file->atts)
		CuDeleteAtts(file,(CuVar*)0);
					     /* Free memory */
	free(file);
	return CU_SUCCESS;
}

/* Create and initialize an array of nvars vars */
CuVar* CuCreateVars(CuFile* file, int nvars){
	CuVar *vars;
	int i, j;
	
	if(nvars>0 && (vars = (CuVar*)malloc(nvars*sizeof(CuVar))) == (CuVar*)0){
		CuError(CU_SERROR,"Creating array of variables for file %s",file->controlpath);
		return (CuVar*)0;
	}
	for(i=0;i<nvars;i++){
		vars[i].ndims = vars[i].natts = 0;
		for(j=0;j<CU_MAX_VAR_DIMS;j++) vars[i].dims[j]=-1;
		vars[i].atts=0;
		vars[i].file=file;
		vars[i].id=i;
	}
	file->vars = vars;
	return vars;
}

/* Lookup a variable by ID */
CuVar* CuLookupVar(CuFile* file, int varid){
	int found;

	if(varid<0 || varid>=file->nvars){
		CuError(CU_EBADID,"File %s, variable ID %d",file->controlpath, varid);
		return (CuVar*)0;
	}

	return ((file->vars)+varid);
}

/* Delete all variables (and variable attributes) in a file */
int CuDeleteVars(CuFile* file){
	CuVar* var;
	int i;

					     /* Delete the attribute list of each variable */
	for(i=0, var=file->vars; i<file->nvars && var; i++, var++){
		if(var->natts>0 && var->atts && CuDeleteAtts(file,var)==-1)
			return -1;
	}
	free(file->vars);
	return CU_SUCCESS;
}

/* Create a list of dimensions for a file */
CuDim* CuCreateDims(CuFile* file, int ndims){
	CuDim *dims;
	int i;

	if(ndims>0 && (dims = (CuDim*)malloc(ndims*sizeof(CuDim))) == (CuDim*)0){
		CuError(CU_SERROR,"Creating array of dimensions for file %s",file->controlpath);
		return (CuDim*)0;
	}
					     /* Initialize dimensions */
	for(i=0;i<ndims;i++)
		dims[i].len=0;

	file->dims = dims;
	return dims;
}
CuDim* CuLookupDim(CuFile* file, int dimid){
	int found;

	if(dimid<0 || dimid>=file->ndims){
		CuError(CU_EBADDIM,"File %s, dimension ID %d",file->controlpath, dimid);
		return (CuDim*)0;
	}

	return ((file->dims)+dimid);
}

/* Create an attribute list. natts must be >0 */
CuAtt* CuCreateAtts(CuFile* file, CuVar* var, int natts){
	CuAtt* atts;
	int i;
	
	if(natts>0 && (atts = (CuAtt*)malloc(natts*sizeof(CuAtt))) == (CuAtt*)0){
		if(var)
			CuError(CU_SERROR,"Creating array of attributes for file %s, variable %s",file->controlpath,var->name);
		else
			CuError(CU_SERROR,"Creating array of attributes for file %s",file->controlpath);
		return (CuAtt*)0;
	}
	else if (natts<=0) {
	    CuError(CU_SERROR,"Creating array of attributes: natts must be >0"); 
	    return (CuAtt*)0;
	}

	for(i=0;i<natts;i++)
		atts[i].len=0;

	if(var)				     /* Variable attributes */
		var->atts = atts;
	else				     /* Global attributes */
		file->atts = atts;

	return atts;
}

/* Set an attribute */
int CuSetAtt(CuFile* file, CuVar* var, int attnum, const char *name, CuType datatype, long len, void *values){
	CuAtt *att;
	char* c;

	if(var){			     /* Local attribute */
		if(attnum>=var->natts){
			CuError(CU_EBADID,"File %s, variable %s, attribute number %d",file->controlpath,var->name,attnum);
			return -1;
		}
		att = (var->atts)+attnum;
	}
	else{				     /* Global attributes */
		if(attnum>=file->ngatts){
			CuError(CU_EBADID,"File %s, attribute number %d",file->controlpath,attnum);
			return -1;
		}
		att = (file->atts)+attnum;
	}

					     /* Copy attribute to static storage */
	strncpy(att->name,name,CU_MAX_NAME);
	att->name[CU_MAX_NAME-1]='\0';
	att->datatype = datatype;
	att->len = len;

	if((att->val = malloc(len*cutypelen(datatype))) == (void*)0){
		CuError(CU_SERROR,"Creating attribute %s for file %s",name,file->controlpath);
		return -1;
	}
	memcpy(att->val, values, len*cutypelen(datatype));

	return CU_SUCCESS;
}
CuAtt* CuLookupAtt(CuFile* file, int varid, const char *name){
	CuVar* var;
	CuAtt* att;
	int i;

					     /* Lookup global attribute */
	if(varid==CU_GLOBAL){
		for(i=0, att=file->atts; i<file->ngatts && att; i++, att++)
			if(!strncmp(name,att->name,CU_MAX_NAME))
				return att;
/*		CuError(CU_ENOTATT,"No attribute %s found for file %s",name,file->controlpath);
*/		
	}
	else{
		if((var=CuLookupVar(file,varid))==(CuVar*)0)
			return (CuAtt*)0;

		for(i=0, att=var->atts; i<var->natts && att; i++, att++)
			if(!strncmp(name,att->name,CU_MAX_NAME))
				return att;

/*		CuError(CU_ENOTATT,"No attribute %s found for file %s, variable %s",name,file->controlpath,var->name);
*/		
	}
	return (CuAtt*)0;
}
int CuDeleteAtts(CuFile* file, CuVar* var){
	CuAtt* att, *atts;
	int natts, i;

	if(var){
		att=atts=var->atts;
		natts=var->natts;
	}
	else{
		att=atts=file->atts;
		natts=file->ngatts;
	}
	for(i=0; i<natts && att; i++, att++)
		if(att->val) free(att->val);

	if(natts>0)
	    free(atts);
	return CU_SUCCESS;
}
CuFileType CuGetFileType(const char *controlpath){
	FILE *fd;
	long magic;
	char line[CU_LINE_BUF_LEN];
	char pathcopy[CU_MAX_PATH];
	char *c;
	int comment, i, nread, len;



	/* mf 20001205
	   check for URL
	*/

	if( strncmp(controlpath,"http://",7) == 0) {
	  return CuNetcdf;
	}

	if( strncmp(controlpath,"gridftp:",8) == 0) {
	  return CuNetcdf;
	}

	if((fd=fopen(controlpath,"r"))==NULL){
		CuError(CU_SERROR,"Opening file %s",controlpath);
		return CuUnknown;
	}
					     /* Check for a DRS file */
	if(fseek(fd,48L,SEEK_SET) == 0)
		if((nread=fread(&magic,sizeof(long),1,fd))==1)
			if(magic==DRS_MAGIC_COOKIE){
				fclose(fd);
				return CuDrs;
			}

					     /* Check for a netCDF file */
	if(fseek(fd,0L,SEEK_SET) == 0)
		if((nread=fread(&magic,sizeof(long),1,fd))==1){
#ifdef cray
			magic &= 0xffffffff00000000;
#endif
#if defined(__alpha) || defined(__ia64) || defined(__x86_64__)
			magic &= 0x00000000ffffffff;
#endif
/*  			printf("magic is: magic, NC4, NC64, NC: %i, %i, %i\n",magic,NETCDF4_MAGIC_COOKIE,NETCDF4_64BIT_MAGIC_COOKIE,NETCDF_MAGIC_COOKIE ); */
/* 			printf("hex: %x, %x\n",magic,NETCDF4_MAGIC_COOKIE); */
			if((magic==NETCDF_MAGIC_COOKIE)|| (magic == NETCDF4_MAGIC_COOKIE) || (magic == NETCDF4_64BIT_MAGIC_COOKIE)|| (magic == NETCDF4_64BIT_MAGIC_COOKIE2)){
				fclose(fd);
				return CuNetcdf;
			}
		}
					     /* Check for a HDF file */
	if(fseek(fd,0L,SEEK_SET) == 0)
		if((nread=fread(&magic,sizeof(long),1,fd))==1){
#ifdef cray
			magic &= 0xffffffff00000000;
#endif
#if defined(__alpha) || defined(__ia64) || defined(__x86_64__)
			magic &= 0x00000000ffffffff;
#endif
			if(magic==HDF_MAGIC_COOKIE){
				fclose(fd);
				return CuHdf;
			}
		}
					     /* Check for a GrADS file: The first non-comment */
					     /* line contains the string "dset". A comment line */
					     /* has an asterisk in column 1. Case is ignored. */
	if(fseek(fd,0L,SEEK_SET) == 0){
		comment = 1;
		while(comment){
			if(fgets(line,CU_LINE_BUF_LEN,fd)==NULL){
				goto error;
			}
			else{
				for(i=0, c=line; i<CU_LINE_BUF_LEN && (*c!='\0'); i++, c++)
					*c = (char)tolower(*c);
				if(line[0] != '*')
					if (strstr(line,"dset") != NULL){
						fclose(fd);
						return CuGrads;
					}
					else
						comment = 0;
			}
		}
	}
					     /* Check for a QL (multi-file) metafile */
					     /* At the moment, just look for a .cdms file */
	strncpy(pathcopy, controlpath, CU_MAX_PATH);
	pathcopy[CU_MAX_PATH-1]='\0';
	custrtrim(pathcopy);
	len = strlen(pathcopy);
	if (len>=5 && !strcmp(pathcopy+len-5,".cdms")){
		fclose(fd);
		return CuQL;
	}
					     /* Check for a POP file */
					     /* At the moment, just look for a .pop file */
	strncpy(pathcopy, controlpath, CU_MAX_PATH);
	pathcopy[CU_MAX_PATH-1]='\0';
	custrtrim(pathcopy);
	len = strlen(pathcopy);
	if (len>=4 && !strcmp(pathcopy+len-4,".pop")){
		fclose(fd);
		return CuPop;
	}

#ifdef HAVE_PP
					     /* Check for a PP file */
	strncpy(pathcopy, controlpath, CU_MAX_PATH);
	pathcopy[CU_MAX_PATH-1]='\0';
	custrtrim(pathcopy);
	if (pp_is_ppum_file(pathcopy,fd)) {
		fclose(fd);
		return CuPP;
	}
#endif	
					     /* No CDMS format found */
  error:
	if(feof(fd))
		CuError(CU_ENOTCU,"End of file %s reached while determining file type, must specify dictionary file",controlpath);
	else
		CuError(CU_ENOTCU,"Determining type of file %s; must specify dictionary (control) file",controlpath);
	fclose(fd);
	return CuUnknown;
}
void CuError(int ierr, char *fmt, ...){
	va_list args;
	
	cuLastError = ierr;
	if(cuErrOpts & CU_VERBOSE){
		if(ierr==CU_SERROR)
			perror("CDMS system error");
		va_start(args,fmt);
		fprintf(stderr, "CDMS I/O error: ");
		vfprintf(stderr, fmt, args);
		fprintf(stderr, "\n");
		va_end(args);
	}
	if(cuErrOpts & CU_FATAL)
		exit(ierr);
	return;
}
