
import cmor,numpy





nlat = 3600
dlat = 180./nlat
nlon = 7200
dlon = 360./nlon
nlev = 24
dlev = 1000./nlev
ntimes = 1

lats = numpy.arange(-90+dlat/2.,90,dlat)
blats = numpy.arange(-90,90+dlat,dlat)
lons = numpy.arange(0+dlon/2.,360.,dlon)
blons = numpy.arange(0,360.+dlon,dlon)

levs = numpy.array([1000.,925,900,850,800,700,600,500,400,300,250,200,150,100,75,70,50,30,20,10,7.5,5,2.5,1])
alllevs = numpy.arange(1000,0,-dlev).tolist()
print len(alllevs)

cmor.setup(inpath='.',netcdf_file_action=cmor.CMOR_REPLACE)
cmor.dataset('historical', 'ukmo', 'pcmdi-10b HadCM3', 'gregorian',model_id='pcmdi-10b',outpath='Test',forcing='N/A', parent_experiment_id="lgm", parent_experiment_rip="r1i1p1",contact="Bruce Bochy",branch_time=0,institute_id="yep")
table='Tables/CMIP5_Amon'
cmor.load_table(table)


ilat = cmor.axis(table_entry='latitude',coord_vals=lats,cell_bounds=blats,units='degrees_north')
ilon = cmor.axis(table_entry='longitude',coord_vals=lons,cell_bounds=blons,units='degrees_east')
itim = cmor.axis(table_entry='time',units='months since 2010')#,coord_vals=numpy.arange(ntimes,dtype=numpy.float),cell_bounds=numpy.arange(ntimes+1,dtype=float),units='months since 2000')
ilev = cmor.axis(table_entry='plevs',coord_vals=levs,units='hPa')
    
axes=[itim,ilev,ilat,ilon]

var = cmor.variable(table_entry='ta',units='K',axis_ids=axes)

data = numpy.random.random((nlev,nlat,nlon))*30+273.15

for i in range(ntimes):
    if i%10==0 : print 'Writing time:',i
    cmor.write(var,data,time_vals=numpy.array([float(i),]),time_bnds=numpy.array([i,i+1.]))

print cmor.close(var_id=var,file_name=True)
cmor.close()



print 'hello'
