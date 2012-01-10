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
       parent_experiment_id="lgm",
       parent_experiment_rip="r1i1p1",
       branch_time=3.14159)
  

cmor.load_table("Tables/CMIP5_Omon")
itime = cmor.axis(table_entry="time",units='months since 2010',coord_vals=numpy.array([0,1,2,3,4.]),cell_bounds=numpy.array([0,1,2,3,4,5.]))
# creates 1 degree grid
nlat=18
nlon=36
alats = numpy.arange(180)-89.5
bnds_lat = numpy.arange(181)-90
alons=numpy.arange(360)+.5
bnds_lon=numpy.arange(361)
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
ivar = cmor.variable(table_entry="epc100",axis_ids=[itime,ilat,ilon],units='mol m-2 s-1',positive="up")

data=numpy.random.random((5,nlat,nlon))
for i in range(0,5):
    cmor.write(ivar,data[i:i])#,time_vals=numpy.array([i,]),time_bnds=numpy.array([i,i+1]))
error_flag = cmor.close()  

