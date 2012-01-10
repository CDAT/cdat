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
       forcing="TO",
       parent_experiment_rip="r1i3p2",
       parent_experiment_id="lgm",branch_time=0)
  

# creates 1 degree grid
nlat=180
nlon=360
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

lev=5
ntimes=12
plevs = (numpy.arange(lev)+1)*1.E4


itim = cmor.axis(  
    table_entry='time',           
    units='months since 2030-1-1',  
    length=ntimes,                
    interval='1 month')

zlevs = numpy.array(( 0.1, 0.3, 0.55, 0.7, 0.9 ))
zlev_bnds=numpy.array(( 0.,.2, .42, .62, .8, 1. ))
table_entry='hybrid_height'
if table_entry == 'hybrid_height':
    ilev = cmor.axis(  
        table_entry='hybrid_height',       
        ##     table_entry='standard_sigma',       
                                        ##     table_entry='standard_hybrid_sigma',       
        units='m',
        length=lev,                   
        coord_vals=zlevs,             
        cell_bounds=zlev_bnds)
    
    p0 = 0.5e4
##   p0 = 1.e5
##   a_coeff = (/ 0.1, 0.2, 0.3, 0.2, 0.1 /)
    a_coeff = numpy.array(( 0.2, 0.4, 0.6, 0.8, 0.95 ))
    b_coeff = numpy.array(( 0.0, 0.1, 0.2, 0.5, 0.8 ))
    
##  a_coeff_bnds=(/0.,.15, .25, .25, .15, 0./)
    a_coeff_bnds=numpy.array((0.,.3, .5, .7, .9, 1.))
    b_coeff_bnds=numpy.array((0.,.05, .15, .35, .65, 1.))
    
## error_flag = cmor.zfactor(  
##     zaxis_id=ilev,                      
##     zfactor_name='ptop',                  
##     units='Pa',                         
##     zfactor_values = p0)
    
    error_flag = cmor.zfactor(  
        zaxis_id=ilev,                        
        zfactor_name='b',
        axis_ids= numpy.array( (ilev, )),                
        zfactor_values = b_coeff,            
        zfactor_bounds = b_coeff_bnds  )
    
##     error_flag = cmor.zfactor(  
##         zaxis_id=ilev,                       
##         zfactor_name='lev',                    
##         axis_ids= numpy.array(( ilev, )),
##         units='m',
##         zfactor_values = a_coeff,            
##         zfactor_bounds = a_coeff_bnds )
    
    data2d = numpy.random.random((180,360)).astype('f')*8000
    
    zfactor_id = cmor.zfactor(  
        zaxis_id=ilev,                         
        zfactor_name='orog',                     
        axis_ids=numpy.array(( ilon, ilat )),
        units='m' ,
        zfactor_values = data2d)
else:
    print 'yep working case'
    ilev = cmor.axis(  
        table_entry='standard_sigma',       
        units='1',
        length=lev,                   
        coord_vals=zlevs,             
        cell_bounds=zlev_bnds)
    
    p0 = 0.5E4
    a_coeff = numpy.array(( 0.2, 0.4, 0.6, 0.8, 0.95 ))
    b_coeff = numpy.array(( 0.0, 0.1, 0.2, 0.5, 0.8 ))
    
    a_coeff_bnds=numpy.array((0.,.3, .5, .7, .9, 1.))
    b_coeff_bnds=numpy.array((0.,.05, .15, .35, .65, 1.))
    
    error_flag = cmor.zfactor(  
        zaxis_id=ilev,                      
        zfactor_name='ptop',                  
        units='Pa',                         
        zfactor_values = p0)
    
    error_flag = cmor.zfactor(  
        zaxis_id=ilev,                       
        zfactor_name='sigma',                    
        axis_ids= numpy.array(( ilev, )),
        zfactor_values = a_coeff,            
        zfactor_bounds = a_coeff_bnds )
    
    data2d = numpy.random.random((180,360)).astype('f')-97000.
    
    zfactor_id = cmor.zfactor(  
        zaxis_id=ilev,                         
        zfactor_name='ps',                     
        axis_ids=numpy.array(( ilon, ilat, itim )),
        units='Pa')


var3d_ids = cmor.variable(    
    table_entry='cl',     
    units='%',           
    axis_ids=numpy.array((ilev, ilon, ilat, itim)),
    missing_value=1.0e28, 
    original_name='cloud')


  
for it in range(ntimes):

    time = numpy.array((it))
    bnds_time = numpy.array((it,it+1))
    data3d = numpy.random.random((5,360,180)).astype('f')*40.
    
    error_flag = cmor.write(                                  
        var_id        = var3d_ids,                        
        data          = data3d,                              
        ntimes_passed = 1,                                   
        time_vals     = time,                                
        time_bnds     = bnds_time   )

  
error_flag = cmor.close()  

