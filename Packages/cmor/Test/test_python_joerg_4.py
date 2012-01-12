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
       parent_experiment_rip="r1i3p2",
       parent_experiment_id="lgm",branch_time=3.14159)
  

# creates 1 degree grid
nlat=18
nlon=36
alats = numpy.arange(180)-89.5
bnds_lat = numpy.arange(181)-90
alons=numpy.arange(360)+.5
bnds_lon=numpy.arange(361)
cmor.load_table("Tables/CMIP5_Omon")
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
plevs = numpy.array([-6.0, -17.0, -27.0, -37.0, -47.0, -57.0, -68.0, -82.0, -100.0, -122.0, -150.0, -182.0, -220.0, -262.0, -310.0, -362.0, -420.0, -485.0, -560.0, -645.0, -740.0, -845.0, -960.0, -1085.0, -357.0, -382.0, -407.0, -434.0, -461.0, -490.0, -520.0, -551.0, -584.0, -619.0, -655.0, -693.0, -732.0, -773.0, -816.0, -861.0, -908.0, -957.0, -1009.0, -1063.0, -1119.0, -1178.0, -1240.0, -1304.0, -1372.0, -1442.0, -1516.0, -1594.0, -1675.0, -1760.0, -1849.0, -1942.0, -2039.0, -2140.0, -2246.0, -2357.0, -2473.0, -2594.0, -2721.0, -2854.0, -2993.0, -3139.0, -3291.0, -3450.0, -3616.0, -3790.0, -3972.0, -4163.0, -4362.0, -4571.0, -4789.0, -5017.0, -5255.0, -5505.0, -5766.0, -6039.0])

plevs_bnds = numpy.array([0.0, -11.5, -22.0, -32.0, -42.0, -52.0, -62.5, -75.0, -91.0, -111.0, -136.0, -166.0, -201.0, -241.0, -286.0, -336.0, -391.0, -452.5, -522.5, -602.5, -692.5, -792.5, -902.5, -1022.5, -721.0, -369.5, -394.5, -420.5, -447.5, -475.5, -505.0, -535.5, -567.5, -601.5, -637.0, -674.0, -712.5, -752.5, -794.5, -838.5, -884.5, -932.5, -983.0, -1036.0, -1091.0, -1148.5, -1209.0, -1272.0, -1338.0, -1407.0, -1479.0, -1555.0, -1634.5, -1717.5, -1804.5, -1895.5, -1990.5, -2089.5, -2193.0, -2301.5, -2415.0, -2533.5, -2657.5, -2787.5, -2923.5, -3066.0, -3215.0, -3370.5, -3533.0, -3703.0, -3881.0, -4067.5, -4262.5, -4466.5, -4680.0, -4903.0, -5136.0, -5380.0, -5635.5, -5902.5, -6175.5])

itim = cmor.axis(  
    table_entry='time',           
    units='months since 2030-1-1',  
    length=ntimes,                
    interval='1 month')

try:
    ilev = cmor.axis(  
            table_entry='depth_coord',       
            units='m',
            coord_vals=plevs,             
            cell_bounds=plevs_bnds)
except:
    pass

var3d_ids = cmor.variable(    
    table_entry='ta',     
    units='K',           
    axis_ids=numpy.array((ilev, ilon, ilat, itim)),
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

