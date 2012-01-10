#include <time.h>
#include <stdio.h>
#include<string.h>
#include "cmor.h"
#include <stdlib.h>

void read_coords(alats, alons, plevs, bnds_lat, bnds_lon,lon,lat,lev)
     double *alats,*alons;
     int *plevs;
     double *bnds_lat,*bnds_lon;
     int lon,lat,lev;
{
  int i;
    
  for (i=0;i<lon;i++) {
    alons[i] = i*360./lon;
    bnds_lon[2*i] = (i - 0.5)*360./lon;
    bnds_lon[2*i+1] = (i + 0.5)*360./lon;
  };
  

  for (i=0;i<lat;i++) {
    alats[i] = (lat-i)*10;
    bnds_lat[2*i] = (lat-i)*10 + 5.;
    bnds_lat[2*i+1] = (lat-i)*10 - 5.;
  };
  

  for (i=0;i<lev;i++) {
    plevs[i] = (i+1)*100;
  }
}

void read_time(it, time, time_bnds)
     int it;
     double time[];
     double time_bnds[];
{    
  time[0] = (it-0.5)*30.;
  time_bnds[0] = (it-1)*30.;
  time_bnds[1] = it*30.;

  time[0]=it;
  time_bnds[0] = it;
  time_bnds[1] = it+1;

}
  
void read_3d_input_files(it, varname, field,n0,n1,n2)
     int it,n0,n1,n2;
     char *varname;
     double field[];
{
  int i,j,k;
  float factor,offset;
    
  if (strcmp(varname,"CLOUD")==0) {
    factor = 0.1;
    offset = -50.;
  }
  else if (strcmp(varname,"U")==0) {
    factor = 1.;
    offset = 100.;
  }
  else if (strcmp(varname,"T")==0) {
    factor = 0.5;
    offset = -150.;
  }
    
  for (k=0;k<n2;k++) {
    for (j=0;j<n1;j++) {
      for (i=0;i<n0;i++) {
        field[k*(n0*n1)+j*n0+i] = (k*64 + j*16 + i*4 + it)*factor - offset;
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
  double factor, offset;
  double tmp;
  
  if (strcmp(varname,"LATENT")==0){
    factor = 1.;
    offset = 120.;
  }
  else if (strcmp(varname,"TSURF")==0){
    factor = 2.0;
    offset = -230.;
  }
  else if (strcmp(varname,"SOIL_WET")==0){
    factor = 10.;
    offset = 0.;
  }
  else if (strcmp(varname,"PSURF")==0){
    factor = 1.;
    offset = -9.7e2;
  }

  for (j=0;j<n0;j++){
    for (i=0;i<n1;i++) {
      tmp = ((double)j*16. + (double)(i)*4. + (double)it)*factor - offset;
      k= (n0-1-j)*n1+i;
      field[k] = tmp;
    }
  }
}

int main()
     /*
/*   Purpose:   To serve as a generic example of an application that */
/*       uses the "Climate Model Output Rewriter" (CMOR) */

/*    CMOR writes CF-compliant netCDF files. */
/*    Its use is strongly encouraged by the IPCC and is intended for use  */
/*       by those participating in many community-coordinated standard  */
/*       climate model experiments (e.g., AMIP, CMIP, CFMIP, PMIP, APE, */
/*       etc.) */

/*   Background information for this sample code: */

/*      Atmospheric standard output requested by IPCC are listed in  */
/*   tables available on the web.  Monthly mean output is found in */
/*   tables A1a and A1c.  This sample code processes only two 3-d  */
/*   variables listed in table A1c ("monthly mean atmosphere 3-D data"  */
/*   and only four 2-d variables listed in table A1a ("monthly mean  */
/*   atmosphere + land surface 2-D (latitude, longitude) data").  The  */
/*   extension to many more fields is trivial. */

/*      For this example, the user must fill in the sections of code that  */
/*   extract the 3-d and 2-d fields from his monthly mean "history"  */
/*   files (which usually contain many variables but only a single time  */
/*   slice).  The CMOR code will write each field in a separate file, but  */
/*   many monthly mean time-samples will be stored together.  These  */
/*   constraints partially determine the structure of the code. */


/*   Record of revisions: */

/*       Date        Programmer(s)           Description of change */
/*       ====        ==========              ===================== */
/*      10/22/03     Rusty Koder              Original code */
/*       1/28/04     Les R. Koder             Revised to be consistent */
/*                                            with evolving code design */
{

  /*   dimension parameters: */
  /* --------------------------------- */
#define   ntimes  2    /* number of time samples to process */
#define   lon  4       /* number of longitude grid cells   */
#define   lat  3       /* number of latitude grid cells */
#define   lev  5       /* number of standard pressure levels */
#define   n2d  4       /* number of IPCC Table A1a fields to be */                                      /*     output. */
#define n3d 3       /* number of IPCC Table A1c fields to  */
                     /*                                be output.   */

  /*   Tables associating the user's variables with IPCC standard output  */
  /*   variables.  The user may choose to make this association in a  */
  /*   different way (e.g., by defining values of pointers that allow him  */
  /*   to directly retrieve data from a data record containing many  */
  /*   different variables), but in some way the user will need to map his  */
  /*   model output onto the Tables specifying the MIP standard output. */

  /* ---------------------------------- */

  /* My variable names for IPCC Table A1c fields */
  char varin3d[n3d][6]={"CLOUD", "U", "T" };
  
  /* Units appropriate to my data */
  char units3d[n3d][6]={"%", "m s-1", "K"};
  
  /* Corresponding IPCC Table A1c entry (variable name)  */
  char entry3d[n3d][3]={"cl","ua","ta"};

  /* My variable names for IPCC Table A1a fields */
  char varin2d[n2d][9]={ "LATENT","TSURF","SOIL_WET","PSURF" };
  
  /* Units appropriate to my data */
  char units2d[n2d][7]={ "W m-2","K","kg m-2","Pa"};
  
  char positive2d[n2d][4]={"down"," ", " ", " "};
  
  /* Corresponding IPCC Table A1a entry (variable name)  */
  char entry2d[n2d][6]={"hfls", "tas","mrsos","ps"};

/*  uninitialized variables used in communicating with CMOR: */
/*  --------------------------------------------------------- */

  int error_flag;
  int znondim_id, zfactor_id;
  int var2d_ids[n2d];
  int var3d_ids[n3d];
  double data2d[lat*lon];
  double data3d[lev*lat*lon];
  double alats[lat];
  double alons[lon];
 int ilats[lat];
  int ilons[lon];
  double   plevs[lev];
  int   iplevs[lev];
  long   lplevs[lev];
  float   fplevs[lev];
  double Time[2];
  double  bnds_time[4];
  double bnds_lat[lat*2];
  double bnds_lon[lon*2];
  double zlevs[lev];
  double zlev_bnds[lev+1];

  double a_coeff[lev]={ 0.1, 0.2, 0.3, 0.22, 0.1 };
  double b_coeff[lev]={ 0.0, 0.1, 0.2, 0.5, 0.8 };
  float p0= 1.e5;
  double a_coeff_bnds[lev+1]={0.,.15, .25, .25, .16, 0.};
  double b_coeff_bnds[lev+1]={0.,.05, .15, .35, .65, 1.};
  int ilon, ilat, ipres, ilev, itim;
  double dtmp,dtmp2;

  /*  Other variables: */
  /*  --------------------- */
  
  int it, m, i,ierr , j=400;
  int myaxes[410];
  int myaxes2[410];
  int myvars[400];
  char id[CMOR_MAX_STRING];
  char units[CMOR_MAX_STRING];
  char interval[CMOR_MAX_STRING];
  char anames[25][CMOR_MAX_STRING];
  char type;
  char regions[5][16] = { "atlantic_ocean", "indian_ocean", "pacific_ocean", "global_ocean", "sf_bay"};
  double timestest[5];
  /* Externals funcs */
  int tables[5];
  char msg[555];
  double bt=0.;
  /* ================================ */
  /*  Execution begins here: */
  /* ================================ */
  
  /* Read coordinate information from model into arrays that will be passed  */
  /*   to CMOR. */
  /* Read latitude, longitude, and pressure coordinate values into  */
  /*   alats, alons, and plevs, respectively.  Also generate latitude and  */
  /*   longitude bounds, and store in bnds_lat and bnds_lon, respectively. */
  /*   Note that all variable names in this code can be freely chosen by */
  /*   the user. */
  
  /*   The user must write the subroutine that fills the coordinate arrays  */
  /*   and their bounds with actual data.  The following line is simply a */
  /*   a place-holder for the user's code, which should replace it. */
  
  /*  *** possible user-written call *** */
  

  
  m = CMOR_EXIT_ON_MAJOR;
  j = CMOR_REPLACE;
  printf("ok mode is:%i\n",m);
  i=0;
  ierr = cmor_setup(NULL,&j,NULL,&m,NULL,&i);

  read_coords(&alats[0], &alons[0], &iplevs[0], &bnds_lat[0], &bnds_lon[0],lon,lat,lev);
  int tmpmo[12];
  printf("Test code: ok init cmor\n");
  char c1[CMOR_MAX_STRING];
  char c2[CMOR_MAX_STRING];
  strcpy(c1,"GICCM1\0");
  strcpy(c2,"Nat, TO");


  printf("yep: %s, %s\n",c1,c2);
  ierr = cmor_dataset(
       "Test",
       "abrupt 4XCO2",
       "GICC (Generic International Climate Center, Geneva, Switzerland)",
       "GICCM1 (2002): atmosphere:  GICAM3 (gicam_0_brnchT_itea_2, T63L32); ocean: MOM (mom3_ver_3.5.2, 2x3L15); sea ice: GISIM4; land: GILSM2.5",
       "standard",
       1,
       "Rusty Koder (koder@middle_earth.net)",
       "Output from archive/giccm_03_std_2xCO2_2256.",
       "Equilibrium reached after 30-year spin-up after which data were output starting with nominal date of January 2030",
       "Model described by Koder and Tolkien (J. Geophys. Res., 2001, 576-591).  Also see http://www.GICC.su/giccm/doc/index.html  2XCO2 simulation described in Dorkey et al. '(Clim. Dyn., 2003, 323-357.)",
       0,
       0,
       &tmpmo[0],&c1[0],&c2[0],3,0,"GICC","N/A",&bt,"r1i1p1");


/*   cmor_set_cur_dataset_attribute("initialization_method","3",1); */
/*  cmor_set_cur_dataset_attribute("physics_version","5",1); */
  printf("Test code: ok load cmor table(s)\n");
  ierr = cmor_load_table("Test/CMIP5_Amons",&tables[0]);
  strcpy(id,"time");
  strcpy(units,"months since 1980");
  strcpy(interval,"1 month");
  j = 400;
  for(i=0;i<j;i++) {
    read_time(0, &Time[0], &bnds_time[0]);
    read_time(1, &Time[1], &bnds_time[2]);
    sprintf(units,"months since %i",1900+i);
    ierr = cmor_axis(&myaxes[i],id,units,ntimes,&Time[0],'d',&bnds_time[0],2,interval);
  }
  
  strcpy(id,"latitude");
  strcpy(units,"degrees_north");
  strcpy(interval,"");
  ierr = cmor_axis(&myaxes[j],id,units,lat,&alats,'d',&bnds_lat,2,interval);
  strcpy(id,"longitude");
  strcpy(units,"degrees_east");
  ierr = cmor_axis(&myaxes[j+1],id,units,lon,&alons,'d',&bnds_lon,2,interval);



  dtmp = -999;
  dtmp2=1.e-4;
  myaxes2[1] = myaxes[j];
  myaxes2[2] = myaxes[j+1];

  printf("Test code: defining variables from table 1, %s\n",positive2d[0]);
  for(i=0;i<j;i++) {
    myaxes2[0] = myaxes[i];
    sprintf(id,"hfls%i",i);
    ierr = cmor_variable(&myvars[i],id,units2d[0],3,myaxes2,'d',NULL,&dtmp2,&positive2d[0][0],varin2d[0],"no history","no future");
  }

  for(m=0;m<j;m++) {
    for (i=0;i<ntimes;i++) {
      printf("Test code: writing time: %i of %i\n",i+1,ntimes);
      printf("2d\n");
      read_2d_input_files(i, varin2d[0], &data2d,lat,lon);
      sprintf(id,"%i",i);
      /*     ierr = cmor_write(myvars[0],&data2d,'d',id,1,&time,&bnds_time,NULL); */
      /*     read_3d_input_files(i, varin3d[2], &data3d,lev,lat,lon); */
      /*     ierr = cmor_write(myvars[1],&data3d,'d',id,1,&time,&bnds_time,NULL); */
      /*     read_3d_input_files(i, varin3d[0], &data3d,5,lat,lon); */
      /*     ierr = cmor_write(myvars[2],&data3d,'d',id,1,&time,&bnds_time,NULL); */
      /*     read_2d_input_files(i, varin2d[3], &data2d,lat,lon); */
      /*     ierr = cmor_write(myvars[3],&data2d,'d',id,1,&time,&bnds_time,&myvars[2]); */
      printf("ok writing time %i for variable %i\n",i,m);
      ierr = cmor_write(myvars[m],&data2d,'d',"",1,NULL,NULL,NULL);
    }
    ierr = cmor_close_variable(myvars[m],NULL,NULL);
  }
  ierr = cmor_close();
  return 0;
}
