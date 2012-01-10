import cmor

error_flag = cmor.setup(inpath='Test', netcdf_file_action=cmor.CMOR_REPLACE)

error_flag = cmor.dataset(                                   
       outpath='Test',                                         
       experiment_id='noVolc2000',
       institution= 'GICC (Generic International Climate Center, Geneva, Switzerland)',                                 
       source='pcmdi-10a GICCM1 (2002): ',
       calendar='standard',                                      
       realization=1,                                          
       contact = 'Rusty Koder (koder@middle_earth.net) ',      
       history='Output from archivcl_A1.nce/giccm_03_std_2xCO2_2256.', 
       comment='Equilibrium reached after 30-year spin-up ',                                 
       references='Model described by Koder and Tolkien ',
       model_id="pcmdi-10a", 
       institute_id="pcmdi-10a", 
       forcing="TO, SO, Nat",
       ## month_lengths=[30,28,30,30,30,30,30,31,30,31,30,30],
       ## leap_year=3,
       ## leap_month=1,
       parent_experiment_id="N/A",branch_time=0,
       parent_experiment_rip="N/A")

cmor.load_table("Tables/CMIP5_Omon")
itim = cmor.axis(  
    table_entry='time',           
    units='months since 2010-1-1',
    coord_vals=[0,1,2,3,4,5,6,7,8,9,10,11],
    cell_bounds=[0,1,2,3,4,5,6,7,8,9,10,11,12])

ivar = cmor.variable('thetaoga',units='K',axis_ids=[itim,])

data=[280.,]*12 # 12 months worth of data

cmor.write(ivar,data)

cmor.close()
