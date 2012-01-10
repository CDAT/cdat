#!/usr/bin/env python

from test_python_common import * # common subroutines

import cmor._cmor
import os

pth = os.path.split(os.path.realpath(os.curdir))
if pth[-1]=='Test':
    ipth = opth = '.'
else:
    ipth = opth = 'Test'


myaxes=numpy.zeros(9,dtype='i')
myaxes2=numpy.zeros(9,dtype='i')
myvars=numpy.zeros(9,dtype='i')


cmor.setup(inpath=ipth,set_verbosity=cmor.CMOR_NORMAL, netcdf_file_action = cmor.CMOR_REPLACE, exit_control = cmor.CMOR_EXIT_ON_MAJOR);
cmor.dataset(
    outpath = opth,
    experiment_id = "historical",
    institution = "GICC (Generic International Climate Center, Geneva, Switzerland)",
    source = "GICCM1 2002: atmosphere:  GICAM3 (gicam_0_brnchT_itea_2, T63L32); ocean: MOM (mom3_ver_3.5.2, 2x3L15); sea ice: GISIM4; land: GILSM2.5",
    calendar = "standard",
    realization = 1,
    contact = "Rusty Koder (koder@middle_earth.net)",
    history = "Output from archive/giccm_03_std_2xCO2_2256.",
    comment = "Equilibrium reached after 30-year spin-up after which data were output starting with nominal date of January 2030",
    references = "Model described by Koder and Tolkien (J. Geophys. Res., 2001, 576-591).  Also see http://www.GICC.su/giccm/doc/index.html  2XCO2 simulation described in Dorkey et al. '(Clim. Dyn., 2003, 323-357.)",
    leap_year=0,
    leap_month=0,
    month_lengths=None,
    model_id="GICCM1",
    forcing="Ant, Nat",
    institute_id="pcmdi",
    parent_experiment_id="piControl",branch_time=18336.33,parent_experiment_rip='r1i1p1')

tables=[]
a = cmor.load_table("/git/cmip5-cmor-tables/Tables/CMIP5_grids")
tables.append(a)

t='CMIP5_Omon'
te = 'dissic'
u='mol m-3'
time='time'
ts='month'
tscl=1.

t='CMIP5_Lmon'
te = 'baresoilFrac'
u= ''
time='time'
ts='months'
tscl=3.5e-4

tables.append(cmor.load_table("/git/cmip5-cmor-tables/Tables/%s" % t))
print 'Tables ids:',tables

cmor.set_table(tables[0])

x,y,lon_coords,lat_coords,lon_vertices,lat_vertices = gen_irreg_grid(lon,lat)



myaxes[0] = cmor.axis(table_entry = 'y', 
                      units = 'm', 
                      coord_vals = y)
myaxes[1] = cmor.axis(table_entry = 'x', 
                      units = 'm', 
                      coord_vals = x)

grid_id = cmor.grid(axis_ids = myaxes[:2], 
                    latitude = lat_coords, 
                    longitude = lon_coords, 
                    latitude_vertices = lat_vertices, 
                    longitude_vertices = lon_vertices)
print 'got grid_id:',grid_id
myaxes[2] = grid_id

mapnm = 'lambert_conformal_conic'
params = [ "standard_parallel1",
           "longitude_of_central_meridian","latitude_of_projection_origin",
           "false_easting","false_northing","standard_parallel2" ]
punits = ["","","","","","" ]
pvalues = [-20.,175.,13.,8.,0.,20. ]
cmor.set_grid_mapping(grid_id=myaxes[2],
                      mapping_name = mapnm,
                      parameter_names = params,
                      parameter_values = pvalues,
                      parameter_units = punits)

cmor.set_table(tables[1])
myaxes[3] = cmor.axis(table_entry = time,
                      units = '%s since 1980' % ts)

pass_axes = [myaxes[3],myaxes[2]]

myvars[0] = cmor.variable( table_entry = te,
                           units = u,
                           axis_ids = pass_axes,
                           history = 'no history',
                           comment = 'no future'
                           )

ntimes=2
for i in range(ntimes):
    data2d = read_2d_input_files(i, varin2d[0], lat,lon)*1.E-6
    print 'writing time: ',i,data2d.shape#,data2d
    print Time[i],bnds_time[2*i:2*i+2]    
    cmor.write(myvars[0],data2d,1,time_vals=Time[i],time_bnds=bnds_time[2*i:2*i+2])
    print 'wrote'
cmor.close()
