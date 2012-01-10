#include <time.h>
#include <stdio.h>
#include<string.h>
#include "cmor.h"
#include <stdlib.h>
#include <math.h>

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
  
#include "reader_2D_3D.h"

int main()
{

  /*   dimension parameters: */
  /* --------------------------------- */
#define   ntimes  2    /* number of time samples to process */
#define   lon  3       /* number of longitude grid cells   */
#define   lat  4       /* number of latitude grid cells */
#define   lev  5       /* number of standard pressure levels */
#define nvert 6
  double x[lon];
  double y[lat];
  double lon_coords[lon*lat];
  double lat_coords[lon*lat];
  double lon_vertices[lon*lat*nvert];
  double lat_vertices[lon*lat*nvert];
 
  double data2d[lat*lon];
  double data3d[lev*lat*lon];

  int myaxes[10];
  int mygrids[10];
  int myvars[10];
  int tables[4];
  int axes_ids[CMOR_MAX_DIMENSIONS];
  int i,j,k,ierr;

  double Time[ntimes];
  double  bnds_time[ntimes*2];
  double tolerance=1.e-4;
  double lon0 = 280.;
  double lat0=0.;
  double delta_lon = 10.;
  double delta_lat = 10.;
  char id[CMOR_MAX_STRING];
  double tmpf=0.;

#define nparam 6 /* number of grid parameters */
#define lparam 40
#define lunits 14
  char params[nparam][lparam] = {"standard_parallel1","longitude_of_central_meridian","latitude_of_projection_origin","false_easting","false_northing","standard_parallel2"};
  char punits[nparam][lunits] = {"degrees_north","degrees_east","degrees_north","m","m","degrees_north"};
  //char punits[nparam][lunits] = {"","","","","",""};
  double pvalues[nparam] = {-20.,175.,13.,8.,0.,20};
  int exit_mode;
  /* first construct grid lon/lat */
  for (j=0;j<lat;j++) {
    y[j]=j;
    for (i=0;i<lon;i++) {
      x[i]=i;
      lon_coords[i+j*lon] = lon0+delta_lon*(j+1+i);
      lat_coords[i+j*lon] = lat0+delta_lat*(j+1-i);
      /* vertices lon*/
      k = i*nvert+j*lon*nvert+0;
      printf("i,j,k: %i, %i, %i\n",i,j,k);
      if (nvert==6) {
	lon_vertices[i*nvert+j*lon*nvert+0] = lon_coords[i+j*lon];
	lon_vertices[i*nvert+j*lon*nvert+1] = lon_coords[i+j*lon]+delta_lon;
	lon_vertices[i*nvert+j*lon*nvert+2] = lon_coords[i+j*lon]+delta_lon;
	lon_vertices[i*nvert+j*lon*nvert+3] = lon_coords[i+j*lon]+delta_lon/5.;
	lon_vertices[i*nvert+j*lon*nvert+4] = lon_coords[i+j*lon]+delta_lon/5.;
	lon_vertices[i*nvert+j*lon*nvert+5] = lon_coords[i+j*lon];
	/* vertices lat */
	lat_vertices[i*nvert+j*lon*nvert+0] = lat_coords[i+j*lon];
	lat_vertices[i*nvert+j*lon*nvert+1] = lat_coords[i+j*lon];
	lat_vertices[i*nvert+j*lon*nvert+2] = lat_coords[i+j*lon]+2.*delta_lat/3.;
	lat_vertices[i*nvert+j*lon*nvert+3] = lat_coords[i+j*lon]+2.*delta_lat/3.;
	lat_vertices[i*nvert+j*lon*nvert+4] = lat_coords[i+j*lon]+delta_lat;
	lat_vertices[i*nvert+j*lon*nvert+5] = lat_coords[i+j*lon]+delta_lat;
      }
      else {
	lon_vertices[i*4+j*lon*4+0] = lon_coords[i+j*lon]-delta_lon;
	lon_vertices[i*4+j*lon*4+1] = lon_coords[i+j*lon];
	lon_vertices[i*4+j*lon*4+2] = lon_coords[i+j*lon]+delta_lon;
	lon_vertices[i*4+j*lon*4+3] = lon_coords[i+j*lon];
	lat_vertices[i*4+j*lon*4+0] = lat_coords[i+j*lon];
	lat_vertices[i*4+j*lon*4+1] = lat_coords[i+j*lon]-delta_lat;
	lat_vertices[i*4+j*lon*4+2] = lat_coords[i+j*lon];
	lat_vertices[i*4+j*lon*4+3] = lat_coords[i+j*lon]+delta_lat;
      }
    }
  }

  exit_mode = CMOR_EXIT_ON_MAJOR;
  j = CMOR_REPLACE;
  printf("Test code: ok init cmor, %i\n",exit_mode);
  ierr = cmor_setup(NULL,&j,NULL,&exit_mode,NULL,NULL);
  printf("Test code: ok init cmor\n");
  int tmpmo[12];
  ierr = cmor_dataset(
       "Test",
       "amip",
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
       tmpmo,
       "GICCM1\0","N/A",0,0,"GICC","N/A",&tmpf,"r1i1p1");
  printf("Test code: ok load cmor table(s)\n");
  ierr = cmor_load_table("Tables/CMIP5_Amon",&tables[1]);
  printf("Test code: ok load cmor table(s)\n");
  //ierr = cmor_load_table("Test/IPCC_test_table_Grids",&tables[0]);
  ierr = cmor_load_table("Tables/CMIP5_grids",&tables[0]);
  printf("Test code: ok load cmor table(s)\n");
  ierr = cmor_set_table(tables[0]);

  /* first define grid axes (x/y/rlon/rlat,etc... */
  ierr = cmor_axis(&myaxes[0],"x","m",lon,&x[0],'d',NULL,0,NULL);
  printf("Test code: ok got axes id: %i for 'x'\n",myaxes[0]);
  ierr = cmor_axis(&myaxes[1],"y","m",lat,&y[0],'d',NULL,0,NULL);
  printf("Test code: ok got axes id: %i for 'y'\n",myaxes[1]);

  axes_ids[0] = myaxes[1];
  axes_ids[1] = myaxes[0];
  /*now defines the grid */
  printf("going to grid stuff \n");
    ierr = cmor_grid(&mygrids[0],2,&axes_ids[0],'d',&lat_coords[0],&lon_coords[0],nvert,&lat_vertices[0],&lon_vertices[0]);
    //ierr = cmor_grid(&mygrids[0],2,&axes_ids[0],'d',&lat_coords[0],&lon_coords[0],0,NULL,NULL);

  for (i=0;i<cmor_grids[0].ndims;i++) {
    printf("Dim : %i the grid has the follwoing axes on itself: %i (%s)\n",i,cmor_grids[0].axes_ids[i],cmor_axes[cmor_grids[0].axes_ids[i]].id);
  }



  /* ok puts some grid mappings in it,  not sure these parmeters make sens! */
  for(i=0;i<nparam;i++) printf("Test code: ok paramter: %i is: %s, with value %lf and units '%s'\n",i,params[i],pvalues[i],punits[i]);
  
  printf("back from grid going to mapping \n");
  ierr = cmor_set_grid_mapping(mygrids[0],"lambert_conformal_conic",nparam-1,&params[0][0],lparam,pvalues,&punits[0][0],lunits);


  for (i=0;i<cmor_grids[0].ndims;i++) {
    printf("New Dim : %i the grid has the follwoing axes on itself: %i (%s)\n",i,cmor_grids[0].axes_ids[i],cmor_axes[cmor_grids[0].axes_ids[i]].id);
  }

  /* ok sets back the vars table */
  cmor_set_table(tables[1]);


  for(i=0;i<ntimes;i++) read_time(i, &Time[i], &bnds_time[2*i]);
  ierr = cmor_axis(&myaxes[3],"time","months since 1980",2,&Time[0],'d',&bnds_time[0],2,NULL);

  printf("time axis id: %i\n",myaxes[3]);
  axes_ids[0]=myaxes[3]; /*time*/
  axes_ids[1]=mygrids[0]; /*grid */

  printf("Test code: sending axes_ids: %i %i\n",axes_ids[0],axes_ids[1]);

  ierr = cmor_variable(&myvars[0],"hfls","W m-2",2,axes_ids,'d',NULL,&tolerance,"down","HFLS","no history","no future");
  
  for (i=0;i<ntimes;i++) {
    printf("Test code: writing time: %i of %i\n",i+1,ntimes);
    
    printf("Test code: 2d\n");
    read_2d_input_files(i, "LATENT", &data2d[0],lat,lon);
    //for(j=0;j<10;j++) printf("Test code: %i out of %i : %lf\n",j,9,data2d[j]);
    printf("var id: %i\n",myvars[0]);
    ierr = cmor_write(myvars[0],&data2d,'d',NULL,1,NULL,NULL,NULL);
  }
  printf("ok loop done\n");
  ierr = cmor_close();
  printf("Test code: done\n");
  return 0;
}
