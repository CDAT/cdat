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

cmor._cmor.setup(ipth,cmor.CMOR_REPLACE,cmor.CMOR_NORMAL,cmor.CMOR_EXIT_ON_MAJOR,"",1);
cmor._cmor.dataset( \
    ipth,\
    "lgm",\
    "GICC (Generic International Climate Center, Geneva, Switzerland)",\
    "GICCM1 (2002): atmosphere:  GICAM3 (gicam_0_brnchT_itea_2, T63L32); ocean: MOM (mom3_ver_3.5.2, 2x3L15); sea ice: GISIM4; land: GILSM2.5",\
    "standard",\
    1,\
    "Rusty Koder (koder@middle_earth.net)",\
    "Output from archive/giccm_03_std_2xCO2_2256.",\
    "Equilibrium reached after 30-year spin-up after which data were output starting with nominal date of January 2030",\
    "Model described by Koder and Tolkien (J. Geophys. Res., 2001, 576-591).  Also see http://www.GICC.su/giccm/doc/index.html  2XCO2 simulation described in Dorkey et al. '(Clim. Dyn., 2003, 323-357.)",\
    0,\
    0,\
    None,"GICCM1","Nat",0,0,"PCMDI","N/A",0,"r1i1p1")

tables=[]
a = cmor._cmor.load_table("Tables/CMIP5_Omon")
tables.append(a)
tables.append(cmor._cmor.load_table("Tables/CMIP5_Amon"))
print 'Tables ids:',tables


axes=[]
id="time"
units="months since 1980"
myaxes[0] = cmor._cmor.axis(id,units,ntimes,Time,'d',bnds_time,2,"1 month")
id='latitude'
units="degrees_north"
interval=""
myaxes[1] = cmor._cmor.axis(id,units,lat,alats,'d',bnds_lat,2,interval)
id="longitude"
units="degrees_east"
myaxes[2] = cmor._cmor.axis(id,units,lon,alons,'d',bnds_lon,2,interval)
id="plevs"
units="Pa"
print plevs.astype("d")
myaxes[3] = cmor._cmor.axis(id,units,lev2,plevs.astype("d"),'d',None,0,interval)


myaxes[4] = cmor._cmor.axis("alternate_hybrid_sigma","1",5,zlevs,'d',zlev_bnds,1,interval)


cmor.set_table(tables[0])
myaxes[5] = cmor._cmor.axis( "basin","",4,regions,'c',None,21,interval)
id='time'
units='months since 1980'
myaxes[7] = cmor._cmor.axis(id,units,ntimes,Time,'d',bnds_time,2,"1 month")
id="latitude"
units="degrees_north"
interval=""
myaxes[8] = cmor._cmor.axis(id,units,lat,alats,'d',bnds_lat,2,interval)

cmor._cmor.set_table(tables[1])


dtmp = -999;
dtmp2=1.e-4;
myaxes2[0] = myaxes[0];
myaxes2[1] = myaxes[3];
myaxes2[2] = myaxes[1];
myaxes2[3] = myaxes[2];

print 'ok doing the vars thing'
myvars[0] = cmor._cmor.variable(entry2d[0],units2d[0],3,myaxes,'d',None,dtmp2,positive2d[0],varin2d[0],"no history","no future")
print 'vars 2'
myvars[1] = cmor._cmor.variable(entry3d[2],units3d[2],4,myaxes2,'d',None,dtmp2,"",varin3d[2],"no history","no future")
print 'vars 2'

myaxes2[1] = myaxes[4];
myvars[2] = cmor._cmor.variable(entry3d[0],units3d[0],4,myaxes2,'d',None,dtmp2,"",varin3d[0],"no history","no future")

print 'vars 2'

print 'zfact',type(numpy.array(myaxes2[1])),type(myaxes2)

myvars[3] = cmor._cmor.zfactor(int(myaxes2[1]),"p0","Pa",0,None,'d',p0,None)
print 'zfact',myaxes2[1]
myvars[3] = cmor._cmor.zfactor(int(myaxes2[1]),"b","",1,myaxes2[1],'d',b_coeff,b_coeff_bnds)
print 'zfact',myaxes2[1]
myvars[3] = cmor._cmor.zfactor(int(myaxes2[1]),"a","",1,myaxes2[1],'d',a_coeff,a_coeff_bnds)
#/*   printf("defining ap\n"); */
#/*   for(i=0;i<5;i++) {a_coeff[i]*=1.e3;printf("sending acoef: %i, %lf\n",i,a_coeff[i]);} */
#/*   for(i=0;i<6;i++) {a_coeff_bnds[i]*=1.e5;printf("sending acoef: %i, %lf\n",i,a_coeff_bnds[i]);} */
#/*   ierr = cmor_zfactor(&myvars[3],myaxes2[1],"ap","hPa",1,&myaxes2[1],'d',&a_coeff,&a_coeff_bnds); */
print 'zfact before last'
myvars[3] = cmor._cmor.zfactor(int(myaxes2[1]),"ps","hPa",3,myaxes,'d',None,None)
print 'zfact last'

#  /* ok here we decalre a variable for region axis testing */
cmor.set_table(tables[0])
myaxes2[0] = myaxes[7]
myaxes2[1] = myaxes[5]
myaxes2[2] = myaxes[8]

myvars[4] = cmor._cmor.variable("htovgyre","W",3,myaxes2,'d',None,dtmp2,positive2d[0],varin2d[0],"no history","no future")

cmor.set_table(tables[1])


for i in range(ntimes):
    data2d = read_2d_input_files(i, varin2d[0], lat,lon)
    print 'writing time',i,data2d.shape,data2d,numpy.average(data2d)
    cmor._cmor.write(myvars[0],numpy.ravel(data2d),data2d.dtype.char,"",1,None,None,None);

cmor._cmor.close(None,0,0)
