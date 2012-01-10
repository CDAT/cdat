
import cmor,numpy



vars ={'hfls' : ['W.m-2',25.,40.],'tas':['K',25,268.15],'clt':['%',100.,0.],'ta':['K',25,273.15]}


nlat = 90
dlat = 180/nlat
nlon = 180
dlon = 360./nlon
nlev = 17
ntimes = 12

lats = numpy.arange(-90+dlat/2.,90,dlat)
blats = numpy.arange(-90,90+dlat,dlat)
lons = numpy.arange(0+dlon/2.,360.,dlon)
blons = numpy.arange(0,360.+dlon,dlon)


tvars= ['hfls','tas','clt','ta']

cmor.setup(inpath='.',netcdf_file_action=cmor.CMOR_REPLACE)
cmor.dataset('historical', 'ukmo', 'HadCM3', 'gregorian',
             model_id='HadCM3',outpath='Test',forcing='TO, Nat',
             institute_id="PCMDI",
             parent_experiment_rip="r1i3p2",
             contact="Matt Cain",parent_experiment_id="lgm",branch_time=0)
table='TestTables/CMIP5_Amon'
cmor.load_table(table)

for var in tvars:
    ilat = cmor.axis(table_entry='latitude',coord_vals=lats,cell_bounds=blats,units='degrees_north')
    ilon = cmor.axis(table_entry='longitude',coord_vals=lons,cell_bounds=blons,units='degrees_east')
    itim = cmor.axis(table_entry='time',coord_vals=numpy.arange(ntimes,dtype=numpy.float),cell_bounds=numpy.arange(ntimes+1,dtype=float),units='months since 2000')
    ilev = cmor.axis(table_entry='plevs',coord_vals=numpy.array([1000.,925,850,700,600,500,400,300,250,200,150,100,70,50,30,20,10]),units='hPa')
    
    if var!='ta':
        axes = [itim,ilat,ilon]
        data = numpy.random.random((ntimes,nlat,nlon))*vars[var][1]+vars[var][2]
    else:
        axes=[itim,ilev,ilat,ilon]
        data = numpy.random.random((ntimes,nlev,nlat,nlon))*vars[var][1]+vars[var][2]

    kw={}
    if var in ['hfss','hfls']:
        kw['positive']='up'
    var = cmor.variable(table_entry=var,units=vars[var][0],axis_ids=axes,**kw)

    cmor.write(var,data)
    path=cmor.close(var, file_name=True)
    print 'Saved in:',path

cmor.close()



print 'hello'
