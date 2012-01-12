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
cmor.load_table("Tables/CMIP5_cf3hr")
error_flag = cmor.close()  

