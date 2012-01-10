import numpy
# this test tries to mimic ippc_test_code.c but from python
# This one is using direct C calls from python not the python around it
ntimes=2
lon=4
lat=3
lev=5
lev2=17
varin3d=["CLOUD", "U", "T" ];
  
#  /* Units appropriate to my data */
units3d=["%", "m s-1", "K"];
  
#  /* Corresponding IPCC Table A1c entry (variable name)  */
entry3d=["cl","ua","ta"];

#  /* My variable names for IPCC Table A1a fields */
varin2d=[ "LATENT","TSURF","SOIL_WET","PSURF" ];
  
#  /* Units appropriate to my data */
units2d=[ "W m-2","K","kg m-2","Pa"];
  
positive2d=["down"," ", " ", " "];
  
#  /* Corresponding IPCC Table A1a entry (variable name)  */
entry2d=["hfls", "tas","mrsos","ps"];


def gen_irreg_grid(lon,lat):
    lon0 = 280.
    lat0=0.;
    delta_lon = 10.;
    delta_lat = 10.;
    y = numpy.arange(lat)
    x = numpy.arange(lon)
    lon_coords = numpy.zeros((lat,lon))
    lat_coords = numpy.zeros((lat,lon))
    lon_vertices = numpy.zeros((lat,lon,4))
    lat_vertices = numpy.zeros((lat,lon,4))

    for j in range(lat): # really porr coding i know
        for i in range(lon): # getting worse i know
            lon_coords[j,i] = lon0+delta_lon*(j+1+i);
            lat_coords[j,i] = lat0+delta_lat*(j+1-i);
            lon_vertices[j,i,0] = lon_coords[j,i]-delta_lon;
            lon_vertices[j,i,1] = lon_coords[j,i];
            lon_vertices[j,i,2] = lon_coords[j,i]+delta_lon;
            lon_vertices[j,i,3] = lon_coords[j,i];
## !!$      /* vertices lat */
            lat_vertices[j,i,0] = lat_coords[j,i];
            lat_vertices[j,i,1] = lat_coords[j,i]-delta_lat;
            lat_vertices[j,i,2] = lat_coords[j,i];
            lat_vertices[j,i,3] = lat_coords[j,i]+delta_lat;
    return x,y,lon_coords,lat_coords,lon_vertices,lat_vertices

# read_data funcs are highly unoptimzed....
def read_coords(lon,lat,lev):
    alons = numpy.zeros(lon)
    bnds_lon = numpy.zeros(2*lon)
    alats = numpy.zeros(lat)
    bnds_lat = numpy.zeros(2*lat)
    plevs = numpy.zeros(lev,dtype='i')
    for i in range(lon):
        alons[i] = i*360./lon
        bnds_lon[2*i] = (i - 0.5)*360./lon
        bnds_lon[2*i+1] = (i + 0.5)*360./lon
        
    for i in range(lat):
        alats[i] = (lat-i)*10
        bnds_lat[2*i] = (lat-i)*10 + 5.
        bnds_lat[2*i+1] = (lat-i)*10 - 5.
        

    plevs = numpy.array([100000., 92500., 85000., 70000.,
                         60000., 50000., 40000., 30000., 25000., 20000.,
                         15000., 10000., 7000., 5000., 3000., 2000., 1000.])

    return alats, alons, plevs, bnds_lat, bnds_lon

def read_time(it):
    time = [0]
    time_bnds=[0,0]
    time[0] = (it-0.5)*30.;
    time_bnds[0] = (it-1)*30.;
    time_bnds[1] = it*30.;

    time[0]=it;
    time_bnds[0] = it;
    time_bnds[1] = it+1;
    return time[0],numpy.array(time_bnds)

def read_3d_input_files(it, varname, n0, n1, n2, ntimes):
    
  if varname=="CLOUD":
      factor = 0.1;
      offset = -50.;
  elif varname=="U":
      factor = 1.
      offset = 100.
  elif varname=="T":
      factor = 0.5;
      offset = -150.;

  field = numpy.zeros((n2,n1,n0),dtype='d')
  for k in range(n2):
    for j in range(n1):
      for i in range(n0):
	field[k,j,i] = (k*64 + j*16 + i*4 + it)*factor - offset;
  return field


def read_2d_input_files(it, varname, n0, n1):

    if varname=="LATENT":
        factor = 1.25;
        offset = 100.;
    elif varname == "TSURF":
        factor = 2.0;
        offset = -230.;
    elif varname=="SOIL_WET":
        factor = 10.;
        offset = 0.;
    elif varname == "PSURF":
        factor = 1.;
        offset = -9.7e2;

    field = numpy.zeros((n0,n1),dtype='d')

    for j in range(n0):
        for i in range(n1):
            tmp = (j*16. + i*4. + it)*factor - offset;
            field[j,i] = tmp;
    return field

alats, alons, plevs, bnds_lat, bnds_lon = read_coords(lon,lat,lev);

Time = numpy.zeros(ntimes,dtype='d')
bnds_time = numpy.zeros(ntimes*2,dtype='d')
Time[0],bnds_time[0:2] = read_time(0)
Time[1],bnds_time[2:4] = read_time(1)

zlevs = numpy.zeros(5,dtype='d')
zlevs[0]=0.1999999999999999999;
zlevs[1]= 0.3;
zlevs[2]=0.55;
zlevs[3]= 0.7;
zlevs[4] =  0.99999999;

zlev_bnds    = numpy.zeros(6,dtype='d')
zlev_bnds[0] = 0.
zlev_bnds[1] = 0.2
zlev_bnds[2] = 0.42
zlev_bnds[3] = 0.62
zlev_bnds[4] = 0.8
zlev_bnds[5] = 1.

regions = numpy.array(["atlantic_arctic_ocean", "indian_pacific_ocean", "pacific_ocean", "global_ocean", "sf_bay"])

a_coeff=numpy.array([ 0.1, 0.2, 0.3, 0.22, 0.1 ])
b_coeff=numpy.array([ 0.0, 0.1, 0.2, 0.5, 0.8 ])
p0= numpy.array([1.e5,])
a_coeff_bnds=numpy.array([0.,.15, .25, .25, .16, 0.])
b_coeff_bnds=numpy.array([0.,.05, .15, .35, .65, 1.])

