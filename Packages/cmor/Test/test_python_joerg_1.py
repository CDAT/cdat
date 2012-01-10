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
       model_id="pcmdi-10a", 
       forcing="co2",
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
#cmor.load_table("Test/IPCC_table_A1")
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


mlev_val= """
   0.000000    0.000000    0.000000    0.000000    0.000000
   0.000000    0.000000    0.000000    0.000000    0.000000
   0.000000    0.000000    0.000000    0.000000    0.000000
   0.000000    0.000000    0.000000    0.000000    0.000000
   0.000200    0.001650    0.006050    0.014750    0.028650
   0.048250    0.073700    0.104950    0.141700    0.183550
   0.229950    0.280200    0.333650    0.389650    0.447450
   0.506300    0.565500    0.624350    0.682100    0.738000
   0.791300    0.841100    0.886350    0.925950    0.958600
   0.982650    0.996150""".split()

levs=[]
for l in mlev_val:
    levs.append(float(l))
    
BS_bnds= """
   0.000000    0.000000    0.000000    0.000000    0.000000
   0.000000    0.000000    0.000000    0.000000    0.000000
   0.000000    0.000000    0.000000    0.000000    0.000000
   0.000000    0.000000    0.000000    0.000000    0.000000
   0.000000    0.000400    0.002900    0.009200    0.020300
   0.037000    0.059500    0.087900    0.122000    0.161400
   0.205700    0.254200    0.306200    0.361100    0.418200
   0.476700    0.535900    0.595100    0.653600    0.710600
   0.765400    0.817200    0.865000    0.907700    0.944200
   0.973000    0.992300    1.000000
""".split()
levs_bnds = []
for l in BS_bnds:
    levs_bnds.append(float(l))


levs=numpy.array(levs)
levs_bnds=numpy.array(levs_bnds)
nlevs = len(levs)

ntimes=12


itim = cmor.axis(  
    table_entry='time',           
    units='months since 2030-1-1',  
    length=ntimes,                
    interval='1 month')

zlevs = numpy.array(( 0.1, 0.3, 0.55, 0.7, 0.9 ))
zlev_bnds=numpy.array(( 0.,.2, .42, .62, .8, 1. ))
table_entry='alternate_hybrid_sigma'

## for i in range(nlevs):
##     print i,levs_bnds[i],levs[i],levs_bnds[i+1]
##     if not (levs_bnds[i]<=levs[i]<=levs_bnds[i+1]) :
##         print 'Yikes'

ilev = cmor.axis(  
    table_entry=table_entry,
    units='',
    length=nlevs,                   
    coord_vals=levs,             
    cell_bounds=levs_bnds)

