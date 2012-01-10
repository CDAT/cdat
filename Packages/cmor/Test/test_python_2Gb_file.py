
import cmor,numpy





nlat = 360
dlat = 180./nlat
nlon = 720
dlon = 360./nlon
nlev = 17
ntimes = 12

lats = numpy.arange(-90+dlat/2.,90,dlat)
blats = numpy.arange(-90,90+dlat,dlat)
lons = numpy.arange(0+dlon/2.,360.,dlon)
blons = numpy.arange(0,360.+dlon,dlon)


cmor.setup(inpath='.',netcdf_file_action=cmor.CMOR_REPLACE)
cmor.dataset('historical', 'ukmo', 'HadCM3', 'gregorian',model_id='HadCM3',outpath='Test',forcing='N/A', parent_experiment_id="lgm", parent_experiment_rip="r1i1p1",branch_time=0,contact="Juan Uribe",institute_id="myinst")
table='Tables/CMIP5_Amon'
cmor.load_table(table)


ilat = cmor.axis(table_entry='latitude',coord_vals=lats,cell_bounds=blats,units='degrees_north')
ilon = cmor.axis(table_entry='longitude',coord_vals=lons,cell_bounds=blons,units='degrees_east')
itim = cmor.axis(table_entry='time',units='months since 2010')#,coord_vals=numpy.arange(ntimes,dtype=numpy.float),cell_bounds=numpy.arange(ntimes+1,dtype=float),units='months since 2000')
ilev = cmor.axis(table_entry='plevs',coord_vals=numpy.array([1000.,925,850,700,600,500,400,300,250,200,150,100,70,50,30,20,10]),units='hPa')
    
axes=[itim,ilev,ilat,ilon]

var = cmor.variable(table_entry='ta',units='K',axis_ids=axes)
ntimes=250

data = numpy.random.random((nlev,nlat,nlon))*30+273.15

for i in range(ntimes):
    if i%10==0 : print 'Writing time:',i
    cmor.write(var,data,time_vals=numpy.array([float(i),]),time_bnds=numpy.array([i,i+1.]))

print cmor.close(var_id=var,file_name=True)
cmor.close()



print 'hello'
