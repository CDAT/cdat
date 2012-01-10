import cmor,numpy

ipth="Test"
cmor.setup(inpath=ipth,
           set_verbosity=cmor.CMOR_NORMAL,
           netcdf_file_action = cmor.CMOR_REPLACE,
           logfile = None)

cmor.dataset(
    outpath = ipth,
    experiment_id = "lgm",
    institution = "PCMDI",
    source = "PCMDI",
    calendar = "standard",
    model_id="pcmdi-09a",forcing="forcing")

cmor.load_table("Tables/CMIP5_Omon")

nlat = 90
dlat = 180/nlat
nlon = 180
dlon = 360./nlon
nlev = 5

lats = numpy.arange(-90+dlat/2.,90,dlat)
blats = numpy.arange(-90,90+dlat,dlat)
lons = numpy.arange(0+dlon/2.,360.,dlon)
blons = numpy.arange(0,360.+dlon,dlon)

ntime = 12

data = numpy.random.random((ntime,nlat,nlev,nlon))*5+273.

itim = cmor.axis(table_entry='time',coord_vals=numpy.arange(0,ntime,1),units='month since 2008',cell_bounds=numpy.arange(0,ntime+1,1))
ilat = cmor.axis(table_entry='latitude',coord_vals=lats,units='degrees_north',cell_bounds=blats)
ilon = cmor.axis(table_entry='longitude',coord_vals=lons,units='degrees_east',cell_bounds=blons)
ilev = cmor.axis(table_entry='depth_coord',length=5,cell_bounds=numpy.arange(-12000,0,2000),coord_vals=numpy.arange(-10000,0,2000),units="m")

iv = cmor.variable(table_entry='thetao',axis_ids=numpy.array((itim,ilat,ilev,ilon)),units='K')

cmor.write(iv,data)

f1 = cmor.close(iv,file_name=True)
print f1
