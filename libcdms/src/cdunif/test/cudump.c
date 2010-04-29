					     /* Generate a metafile from a cdunif file */
#include <stdio.h>
#include <string.h>
#include <cdunif.h>

main(int argc, char *argv[]){
	CuDimType dimtype;
	CuType atttype;
	CuType datatype;
	CuType vartype;
	char attname[CU_MAX_NAME];
	char dimname[CU_MAX_NAME];
	char dimunits[CU_MAX_NAME];
	char fileroot[CU_MAX_NAME];
	char varname[CU_MAX_NAME];
	char* cp, *cp2;
	char* datapath;
	int argleft;
	int attlen;
	int c;
	int dimvarid, i, j;
	int fileid, dimid;
	int lenc;
	int ndims, nvars, natts, recdim;
	int printDimensions, printLocalDimensions;
	int varid,nvdims,nvatts;
	int vdims[CU_MAX_VAR_DIMS];
	long dimlen;
	void* v;
	extern char *optarg;
	extern int optind;
	extern int printatt(void *v, CuType atttype, int attlen);
	extern void printtype(CuType type);
	extern void usage();

	cuseterropts(CU_VERBOSE);
/*	if(cusetlu(7,8))
		exit(1);
*/
	printDimensions = printLocalDimensions = 0;
	while((c = getopt(argc,argv,"adh")) != EOF)
		switch(c){
		  case 'a':
			printLocalDimensions = 1;
		  case 'd':
			printDimensions = 1;
			break;
		  case 'h':
			usage();
			exit(0);
		  default:
			usage();
			exit(1);
		}
	
	argleft = argc-optind;
	if(argleft<1 || argleft>2){
		usage();
		exit(1);
	}

	datapath = (argc==2 ? (char*)0 : argv[optind+1]);
	if((fileid=cuopenread(argv[optind],datapath))==-1)
		exit(1);

	cp = strrchr(argv[optind],'.');
	cp2 = strrchr(argv[optind],'/');
	if(cp2)
		cp2++;
	else
		cp2 = argv[optind];
	if(cp){
		strncpy(fileroot,cp2,(lenc=cp-cp2));
		fileroot[lenc]='\0';
	}
	else
		strcpy(fileroot,cp2);
	printf("//cudump: %s\n",argv[optind]);
	printf("netcdf %s {\n",fileroot);
	if((cuinquire(fileid,&ndims,&nvars,&natts,&recdim)) != CU_SUCCESS)
		exit(1);

	printf("\ndimensions:\n");
	for(i=0; i<ndims; i++){
		for(j=0; j<CU_MAX_NAME; j++)
			dimunits[j] = '\0';
		if(cudiminq(fileid,i,dimname,dimunits,0,&dimtype,0,&dimlen)==-1)
			exit(1);
		if(dimtype == CuGlobalDim)
			printf("   %s = %d;",dimname,dimlen);
		else
			printf("   %s-%d = %d;",dimname,i,dimlen);
		if(dimunits[0])
			printf("\t// units = \"%s\"\n",dimunits);
		else
			printf("\n");
	}

	printf("\nvariables:\n");
	for(varid=0; varid<nvars; varid++){
		if((cuvarinq(fileid, varid, varname, &vartype, &nvdims, vdims, &nvatts))!=CU_SUCCESS)
			exit(1);
		printf("   ");
		printtype(vartype);
		printf(" %s(",varname);
		for(j=0; j<nvdims; j++){
			if(cudiminq(fileid,vdims[j],dimname,0,0,&dimtype,0,0)==-1)
				exit(1);
			if(j>0) printf(", ");
			if(dimtype==CuGlobalDim)
				printf("%s",dimname);
			else
				printf("%s-%d",dimname,vdims[j]);
		}
		printf(");\n");
		for(j=0; j<nvatts; j++){
			if(cuattname(fileid,varid,j,attname)!=CU_SUCCESS)
				exit(1);
			if(cuattinq(fileid,varid,attname,&atttype,&attlen)!=CU_SUCCESS)
				exit(1);
			if((v=(void*)malloc((atttype==CuChar? attlen+1 : attlen)*cutypelen(atttype)))==(void*)0)
				exit(1);
			if(cuattget(fileid,varid,attname,v)==-1)
				exit(1);
			printf("      %s:%s\t= ",varname,attname);
			if(printatt(v,atttype,attlen)==-1)
				exit(1);
			printf(";\n");
		}
	}
	
	printf("      // global attributes\n");
	for(i=0;i<natts;i++){
		if(cuattname(fileid,CU_GLOBAL,i,attname)!=CU_SUCCESS)
			exit(1);
		if(cuattinq(fileid,CU_GLOBAL,attname,&atttype,&attlen)!=CU_SUCCESS)
			exit(1);
		if((v=(void*)malloc((atttype==CuChar? attlen+1 : attlen)*cutypelen(atttype)))==(void*)0)
			exit(1);
		if(cuattget(fileid,CU_GLOBAL,attname,v)==-1)
			exit(1);
		printf("   :%s\t= ",attname);
		if(printatt(v,atttype,attlen)==-1)
			exit(1);
		printf(";\n");
	}
	
	if(printDimensions){
		printf("\ndata:\n");
		for(i=0; i<ndims; i++){
			if(cudiminq(fileid,i,dimname,0,&datatype,&dimtype,0,&dimlen)==-1)
				exit(1);
			if(dimtype==CuLocalDim && !printLocalDimensions)
				break;
			if((v=(void*)malloc(dimlen*cutypelen(datatype)))==(void*)0)
				exit(1);
			if(cudimget(fileid,i,v)==-1)
				exit(1);
			if(dimtype==CuGlobalDim)
				printf("   %s\t= ",dimname);
			else
				printf("   %s-%d\t= ",dimname,i);
			if(printatt(v,datatype,dimlen)==-1)
				exit(1);
			printf(";\n");
		}
	}
	
	printf("}\n");

	if(cuclose(fileid)==-1)
		exit(1);
	
	exit(0);
}

int printatt(void *v, CuType atttype,int attlen){
	char* cp;
	double* dp;
#if !defined(__alpha) && !defined(__sgi)
	long double* ldp;
#endif
	float* fp;
	int k;
	long* lp;
	short* sp;
	int* ip;

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
		cp[attlen]='\0';
		printf("\"%s\"",cp);
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
			printf("%.2f",fp[k]);
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
			printf("%.2lf",ldp[k]);
		}
		break;
#endif
	  default:
		return -1;
	}
}

void printtype(CuType type){
	switch(type){
	  case CuByte:
		printf("byte");
		break;
	  case CuChar:
		printf("char");
		break;
	  case CuShort:
		printf("short");
		break;
	  case CuInt:
		printf("int");
		break;
	  case CuLong:
		printf("long");
		break;
	  case CuFloat:
		printf("float");
		break;
	  case CuDouble:
		printf("double");
		break;
	  case CuLongDouble:
		printf("long double");
		break;
	  default:
		printf("<unknown>");
		break;
	}
}

void
usage(){
	fprintf(stderr,"Usage: cudump [-a] [-d] [-h] <control-file-path> [<data-file-path>]\n");
	fprintf(stderr,"   -a option prints all dimensions values, including local dimensions\n");
	fprintf(stderr,"   -d option prints global dimension values, but not local dimension values\n");
	fprintf(stderr,"   -h option prints help message\n");
	
	return;
}


