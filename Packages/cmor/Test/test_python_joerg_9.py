import cmor,numpy,cdms2

f=cdms2.open("Test/GR30s_halo.nc")

nlon=f['grid_center_lat'].shape[1]
nlat=f['grid_center_lat'].shape[0]


error_flag = cmor.setup(inpath='Test', netcdf_file_action=cmor.CMOR_APPEND)
  
error_flag = cmor.dataset(                                   
       outpath='Joerg',                                         
       experiment_id='noVolc2000',
       institution= 'GICC (Generic International Climate Center, Geneva, Switzerland)',
       institute_id = "GICC",
       source='GICCM1 (2002): ',
       calendar='360_day',                                      
       realization=1,                                          
       contact = 'Rusty Koder (koder@middle_earth.net) ',      
       history='Output from archivcl_A1.nce/giccm_03_std_2xCO2_2256.', 
       comment='Equilibrium reached after 30-year spin-up ',                                 
       references='Model described by Koder and Tolkien ',
       model_id="GICCM1", 
       forcing="Ant",
       parent_experiment_id="lgm",branch_time=0)

ntables=[cmor.load_table("Tables/CMIP5_grids")]
ntables.append(cmor.load_table("Tables/CMIP5_OImon"))


cmor.set_table(ntables[0])
axes=numpy.zeros(2,numpy.int32)

axes[0] = cmor.axis(                            
    table_entry        = 'i_index',               
    length             = nlon,                    
    coord_vals         = numpy.arange(0,nlon,1,numpy.float32),                     
    units              = '1')
  
axes[1] = cmor.axis(                            
    table_entry        = 'j_index',               
    length             = nlat,                    
    coord_vals         = numpy.arange(0,nlat,1,numpy.float32),                     
    units              = '1')


olat_val  = f("grid_center_lat").filled().astype('f')
olon_val  = f("grid_center_lon").filled().astype('f')
bnds_olat = f("grid_corner_lat").filled().astype('f')
bnds_olon = f("grid_corner_lon").filled().astype('f')

grid_id = cmor.grid(                            
    axis_ids           = axes,                    
    latitude           = olat_val,                
    longitude          = olon_val,                
    latitude_vertices  = bnds_olat,               
    longitude_vertices = bnds_olon)

cmor.set_table(ntables[1])


ntimes=12

tim_id = cmor.axis( table_entry="time",
                    units="months since 2010")
time_vals = numpy.arange(ntimes).astype('f')
bnds_time = numpy.arange(ntimes+1).astype('f')

var_ids              = cmor.variable(           
    table_entry        = "sic",           
    units              = "%",                  
##     positive           = vartabin(3,i),           
    axis_ids           = [grid_id, tim_id ] 
    )

fnm=""
for i in range(ntimes):
    print 'writing time:',time_vals[i],bnds_time[i:i+2]
    data = numpy.random.random((nlon,nlat))
    error_flag = cmor.write(                        
        var_id            = var_ids,                 
        data              = data,
        ntimes_passed     = 1,                    
        file_suffix       = fnm,                  
        time_vals         = time_vals[i],                    
        time_bnds         = bnds_time[i:i+2])

    fnm = cmor.close(var_ids,file_name=True,preserve=True)
    print 'dumped to:',fnm

cmor.close()
