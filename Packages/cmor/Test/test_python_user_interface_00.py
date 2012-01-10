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
    experiment_id = "lgm",
    institution = "GICC (Generic International Climate Center, Geneva, Switzerland)",
    source = "GICCM1 (2002): atmosphere:  GICAM3 (gicam_0_brnchT_itea_2, T63L32); ocean: MOM (mom3_ver_3.5.2, 2x3L15); sea ice: GISIM4; land: GILSM2.5",
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
    forcing="TO",
    institute_id="PCMDI",
    parent_experiment_id="N/A",
    parent_experiment_rip="N/A",
    branch_time=0)

tables=[]
a = cmor.load_table(os.path.join(ipth,"../Tables/CMIP5_Omon"))
tables.append(a)
tables.append(cmor.load_table("Tables/CMIP5_Amon"))
print 'Tables ids:',tables

axes=[]
id="time"
units="months since 1980"
print 'time bounds:',bnds_time
# ok we need to make the bounds 2D because the cmor module "undoes this"
bnds_time = numpy.reshape(bnds_time,(bnds_time.shape[0]/2,2))
bnds_lat = numpy.reshape(bnds_lat,(bnds_lat.shape[0]/2,2))
bnds_lon = numpy.reshape(bnds_lon,(bnds_lon.shape[0]/2,2))
myaxes[0] = cmor.axis(id,coord_vals=Time,units=units,cell_bounds=bnds_time,interval="1 month")
print 'time bounds:',bnds_time
id='latitude'
units="degrees_north"
interval=""
myaxes[1] = cmor.axis(id,coord_vals=alats,units=units,cell_bounds=bnds_lat)
id="longitude"
units="degrees_east"
myaxes[2] = cmor.axis(id,coord_vals=alons,units=units,cell_bounds=bnds_lon)
id="plevs"
units="Pa"
myaxes[3] = cmor.axis(id,coord_vals=plevs,units=units)

myaxes[4] = cmor.axis("alternate_hybrid_sigma",coord_vals=zlevs,units="1",cell_bounds=zlev_bnds)


cmor.set_table(tables[0])

myaxes[5] = cmor.axis( "basin",coord_vals=regions,units="")
id='time'
units='months since 1980'
myaxes[7] = cmor.axis(id,coord_vals=Time,units=units,cell_bounds=bnds_time,interval="1 month")
id="latitude"
units="degrees_north"
interval=""
myaxes[8] = cmor.axis(id,coord_vals=alats,units=units,cell_bounds=bnds_lat)

cmor.set_table(tables[1])


dtmp = -999;
dtmp2=1.e-4;
myaxes2[0] = myaxes[0];
myaxes2[1] = myaxes[3];
myaxes2[2] = myaxes[1];
myaxes2[3] = myaxes[2];

print 'ok doing the vars thing',positive2d[0]
myvars[0] = cmor.variable(entry2d[0],units2d[0],myaxes[:3],'d',missing_value=None,tolerance=dtmp2,positive=positive2d[0],original_name=varin2d[0],history="no history",comment="no future")
print 'vars 2'
myvars[1] = cmor.variable(entry3d[2],units3d[2],myaxes2[:4],'d',original_name = varin3d[2])
print 'vars 2'

myaxes2[1] = myaxes[4];
myvars[2] = cmor.variable(entry3d[0],units3d[0],myaxes2[:4],'d',original_name = varin3d[0])

print 'vars 2'


myvars[3] = cmor.zfactor(int(myaxes2[1]),"p0","Pa",None,'d',p0)
print 'zfact',myaxes2[1]
myvars[3] = cmor.zfactor(int(myaxes2[1]),"b","",myaxes2[1],'d',b_coeff,b_coeff_bnds)
print 'zfact',myaxes2[1]
myvars[3] = cmor.zfactor(int(myaxes2[1]),"a","",myaxes2[1],'d',a_coeff,a_coeff_bnds)
#/*   printf("defining ap\n"); */
#/*   for(i=0;i<5;i++) {a_coeff[i]*=1.e3;printf("sending acoef: %i, %lf\n",i,a_coeff[i]);} */
#/*   for(i=0;i<6;i++) {a_coeff_bnds[i]*=1.e5;printf("sending acoef: %i, %lf\n",i,a_coeff_bnds[i]);} */
#/*   ierr = cmor_zfactor(&myvars[3],myaxes2[1],"ap","hPa",1,&myaxes2[1],'d',&a_coeff,&a_coeff_bnds); */
print 'zfact before last'
myvars[3] = cmor.zfactor(zaxis_id = myaxes2[1],zfactor_name = "ps",units = "hPa",axis_ids = myaxes[:3],type = 'd')
print 'zfact last'

#  /* ok here we decalre a variable for region axis testing */
cmor.set_table(tables[0])
myaxes2[0] = myaxes[7]
myaxes2[1] = myaxes[5]
myaxes2[2] = myaxes[8]

myvars[4] = cmor.variable("htovgyre","W",myaxes2[:3],'d',positive=positive2d[0],original_name=varin2d[0])

cmor.set_table(tables[1])


for i in range(ntimes):
    data2d = read_2d_input_files(i, varin2d[0], lat,lon)
    print 'writing time: ',i,data2d.shape,data2d
    cmor.write(myvars[0],data2d,1)

cmor.close()


