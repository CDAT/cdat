import cmor,numpy

error_flag = cmor.setup(inpath='Test', netcdf_file_action=cmor.CMOR_REPLACE)
  
error_flag = cmor.dataset(                                   
       outpath='Test',                                         
       experiment_id='noVolc2000',
       institution= 'GICC (Generic International Climate Center, Geneva, Switzerland)',                                 
       source='GICCM1 (2002): ',
       calendar='360_day',                                      
       realization=1,                                          
       contact = 'Rusty Koder (koder@middle_earth.net) ',      
       history='Output from archivcl_A1.nce/giccm_03_std_2xCO2_2256.', 
       comment='Equilibrium reached after 30-year spin-up ',                                 
       references='Model described by Koder and Tolkien ',
       model_id="GICCM1", 
       institute_id="PCMDI",
       forcing="Nat, SO",
       parent_experiment_id="lgm",branch_time=3.14159)
  

# creates 1 degree grid
nlat=18
nlon=36
alats = numpy.arange(180)-89.5
bnds_lat = numpy.arange(181)-90
alons=numpy.arange(360)+.5
bnds_lon=numpy.arange(361)
cmor.load_table("Tables/CMIP5_Amon")
ilat = cmor.axis(  
    table_entry='latitude',       
    units='degrees_north',          
    length=nlat,                   
    coord_vals=alats,              
    cell_bounds=bnds_lat)        

ilon = cmor.axis(  
    table_entry='longitude',      
    length=nlon,                   
    units='degrees_east',         
    coord_vals=alons,             
    cell_bounds=bnds_lon)      

ntimes=12
plevs = numpy.array([100000., 92500, 85000, 70000, 60000, 50000, 40000, 30000, 25000,
   20000, 15000, 10000, 7000, 5000, 3000, 2000, 1000, 999, 998, 997, 996,
   995, 994])


itim = cmor.axis(  
    table_entry='time',           
    units='months since 2030-1-1',  
    length=ntimes,                
    interval='1 month')

ilev = cmor.axis(  
        table_entry='plevs',       
        units='Pa',
        coord_vals=plevs,             
        cell_bounds=None)
    

var3d_ids = cmor.variable(    
    table_entry='ta',     
    units='K',           
    axis_ids=numpy.array((ilev, ilon, ilat, 1073743064)),
    missing_value=numpy.array([1.0e28,],dtype=numpy.float32)[0], 
    original_name='cloud')


  
for it in range(ntimes):

    time = numpy.array((it))
    bnds_time = numpy.array((it,it+1))
    data3d = numpy.random.random((len(plevs),nlon,nlat))*30.+265.
    data3d = data3d.astype('f')
    error_flag = cmor.write(                                  
        var_id        = var3d_ids,                        
        data          = data3d,                              
        ntimes_passed = 1,                                   
        time_vals     = time,                                
        time_bnds     = bnds_time   )

  
error_flag = cmor.close()  

