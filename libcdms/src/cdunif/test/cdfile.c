					     /* Determine CDMS file type: */
					     /* Print "drs", "grads", "netcdf", "unknown", ... */
					     /* to stdout */
#include <stdio.h>
#include <cdunif.h>
#include <string.h>
#include <stdlib.h>

main(int argc, char *argv[]){

	if(argc!=2){
		fprintf(stderr,"Usage: cdfile <control-file-path>\n");
		exit(1);
	}

	switch(CuGetFileType(argv[1])){
	  case CuDrs:
		printf("drs\n");
		break;
	  case CuNetcdf:
		printf("netcdf\n");
		break;
	  case CuGrads:
		printf("grads\n");
		break;
	  case CuHdf:
		printf("hdf\n");
		break;
	  case CuPP:
		printf("pp\n");
		break;
	  case CuUnknown:
	  default:
		exit(1);
	}
	exit(0);
}
