/* Test data read */
#include <stdio.h>
#include <string.h>
#include <cdunif.h>

main(int argc, char *argv[]){

	int fileid, varid;
	int i;
	int ndims, jdims;
	char* varname;
	char* filename, *varspec;
	int c;
	extern char *optarg;
	extern int optind;
	extern void usage();
	long start[CU_MAX_VAR_DIMS], count[CU_MAX_VAR_DIMS], ibegin[CU_MAX_VAR_DIMS], iend[CU_MAX_VAR_DIMS];
	long nelems;
	CuType datatype;
	int dims[CU_MAX_VAR_DIMS];
	void *v;
	char* tok;
	int cmajority=0;

	cuseterropts(CU_VERBOSE);
	filename=(char*)0;
	while((c = getopt(argc,argv,"ch")) != EOF)
		switch(c){
		  case 'c':
			cmajority = 1;
			break;
		  case 'v':
			varspec=optarg;
			break;
		  default:
			usage();
			exit(1);
		}

	varspec = argv[optind++];
	filename = argv[optind];
	
	if((filename==(char*)0) || (varspec==(char*)0)){
		usage();
		exit(1);
	}

	if((varname=strtok(varspec,"(,:)"))==NULL){
		usage();
		exit(1);
	}
	if((fileid=cuopenread(filename,(char*)0))==-1)
		exit(1);
	if((varid=cuvarid(fileid,varname))==-1)
		exit(1);
	if(cuvarinq(fileid,varid,0,&datatype,&ndims,dims,0)==-1)
		exit(1);

	for(i=0,jdims=0; tok=strtok(NULL,"(,:)"); i++){
		ibegin[i]=atol(tok);
		if((tok=strtok(NULL,"(,:)"))==NULL){
			usage();
			exit(1);
		}
		iend[i]=atol(tok);
		jdims++;
	}		

	if(jdims != ndims){
		fprintf(stderr,"Varspec should have %d dimensions.\n",ndims);
		exit(1);
	}
	nelems=1;
	for(i=0;i<ndims;i++){
		if(cmajority){
			start[i] = ibegin[i];
			count[i] = iend[i]-ibegin[i]+1;
			nelems *= count[i];
		}
		else{
			start[ndims-i-1] = ibegin[i]-1;
			count[ndims-i-1] = iend[i]-ibegin[i]+1;
			nelems *= count[ndims-i-1];
		}
	}

	if(nelems<1){
		printf("nelems = %d, should be positive\n",nelems);
		exit(1);
	}

	if(ndims>0)
		printf("Reading %s(%d:%d",varname,start[0],start[0]+count[0]-1);
	else
		printf("Reading %s(",varname);
	for(i=1;i<ndims;i++)
		printf(",%d:%d",start[i],start[i]+count[i]-1);
	printf(")\n");

	if((v=(void*)malloc(nelems*cutypelen(datatype)))==(void*)0)
		exit(1);
	if(cuvarget(fileid,varid,start,count,v)==-1)
		exit(1);
	if(printatt(v,datatype,nelems,ndims>0 ? count[ndims-1] : 1)==-1)
		exit(1);
	printf("\n");
	cuclose(fileid);
	exit(0);
}
int printatt(void *v, CuType atttype,int attlen,int strLength){
	char* cp;
	double* dp;
#if !defined(__alpha) && !defined(__sgi)
	long double *ldp;
#endif
	float* fp;
	int k;
	long* lp;
	short* sp;
	int* ip;
	char format[10];

	switch(atttype){
	  case CuByte:
		cp = (char*)v;
		for(k=0; k<attlen; k++){
			if(k>0) printf(",");
			printf("%u",cp[k]);
		}
		break;
	  case CuChar:
		cp = (char*)v;
		sprintf(format,"\"%%.%ds\"",strLength);
		for(k=0; k<attlen; k+=strLength, cp+=strLength){
			if(k>0) printf(",");
			printf(format,cp);
		}
		break;
	  case CuShort:
		sp = (short*)v;
		for(k=0; k<attlen; k++){
			if(k>0) printf(",");
			printf("%hd",sp[k]);
		}
		break;
	  case CuInt:
		ip = (int*)v;
		for(k=0; k<attlen; k++){
			if(k>0) printf(",");
			printf("%d",ip[k]);
		}
		break;
	  case CuLong:
		lp = (long*)v;
		for(k=0; k<attlen; k++){
			if(k>0) printf(",");
			printf("%ld",lp[k]);
		}
		break;
	  case CuFloat:
		fp = (float*)v;
		for(k=0; k<attlen; k++){
			if(k>0) printf(",");
			printf("%8.4g",fp[k]);
		}
		break;
	  case CuDouble:
		dp = (double*)v;
		for(k=0; k<attlen; k++){
			if(k>0) printf(",");
			printf("%.2f",dp[k]);
		}
		break;
#if !defined(__alpha) && !defined(__sgi)
	  case CuLongDouble:
		ldp = (long double*)v;
		for(k=0; k<attlen; k++){
			if(k>0) printf(",");
			printf("%.2Lf",ldp[k]);
		}
		break;
#endif
	  default:
		return -1;
	}
}
void
usage(){
	fprintf(stderr,"Usage: cuget [-c] [-h] varspec filename\n");
	fprintf(stderr,"   -c specifies C indexing (0-origin, last dimension varies fastest)\n");
	fprintf(stderr,"   -h option prints help message\n");
	fprintf(stderr,"      varspec is like ps(1:128,1:64,1:1)\n");
	return;
}
