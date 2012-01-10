void read_3d_input_files(it, varname, field,n0,n1,n2)
     int it,n0,n1,n2;
     char *varname;
     double field[];
{
  int i,j,k;
  float factor,offset,max,min;
  min=-1.e20;
  max=1.e20;
   
  if (strcmp(varname,"CLOUD")==0) {
    factor = 0.02;
    offset = -20.;
  }
  else if (strcmp(varname,"U")==0) {
    factor = .08;
    offset = 45.;
  }
  else if (strcmp(varname,"T")==0) {
    factor = 1.2;
    offset = -25.;
    min=161.;
    max=320.;
  }
    
  for (k=0;k<n2;k++) {
    for (j=0;j<n1;j++) {
      for (i=0;i<n0;i++) {
        field[k*(n0*n1)+j*n0+i] = (k*64 + j*16 + i*4 + it)*factor - offset;
	if (field[k*(n0*n1)+j*n0+i]<min) field[k*(n0*n1)+j*n0+i]=min;
	if (field[k*(n0*n1)+j*n0+i]>max) field[k*(n0*n1)+j*n0+i]=max;
      }
    }
  }
}

void read_2d_input_files(it, varname, field, n0, n1)
  int it,n0,n1;
  char *varname;
  double field[];
{    
  int i, j,k;
  double factor, offset,min,max;
  double tmp;
  
  min=-1.e20;
  max=1.e20;

  if (strcmp(varname,"LATENT")==0){
    factor = 5.;
    offset = 0.;
    min=-65;
    max=65.;
  }
  else if (strcmp(varname,"TSURF")==0){
    factor = 2.1;
    offset = -230.;
    max=320.;
  }
  else if (strcmp(varname,"SOIL_WET")==0){
    factor = 4.;
    offset = 0.;
    max=130.;
  }
  else if (strcmp(varname,"PSURF")==0){
    factor = 1.;
    offset = -9.4e2;
  }
  else if (strcmp(varname,"htov")==0){
    factor = .5e14;
    offset = 2.e13;
    max = 1.492e14;
    min = 1.46e14;
  }

  for (j=0;j<n0;j++){
    for (i=0;i<n1;i++) {
      tmp = ((double)j*16. + (double)(i)*4. + (double)it)*factor - offset;
      k= (n0-1-j)*n1+i;
      field[k] = tmp;
      if (field[k]<min) field[k]=min;
      if (field[k]>max) field[k]=max;
    }
  }
}
