import cmor
import numpy

def cmor_initialisation():
    cmor.setup(inpath='Tables',
               netcdf_file_action = cmor.CMOR_REPLACE_3,
               create_subdirectories = 0)
    cmor.dataset('pre-industrial control', 'ukmo', 'HadCM3', '360_day',
                 institute_id = 'ukmo',
                 model_id = 'HadCM3',
                 history = 'some global history',
                 forcing = 'N/A',
                 parent_experiment_id = 'N/A',
                 parent_experiment_rip = 'N/A',
                 branch_time = 0.,
                 contact = 'bob',
                 outpath = 'Test')

def setup_data():
    ntimes=7200
    tvals = numpy.arange(ntimes)*6.
    tbnds = list(tvals)
    tbnds.append(43200)
    tbnds=numpy.array(tbnds)-3.
    print "tvals:",tvals
    print "tbnds:",tbnds
    import cdtime
    tunits='hours since 209-01-01 06:00:00'
    t1=cdtime.reltime(tvals[0],tunits)
    t2=cdtime.reltime(tvals[-1],tunits)
    t3=cdtime.reltime(tbnds[-1],tunits)
    print t1.tocomp(),t1.tocomp(cdtime.Calendar360)
    print t2.tocomp(),t2.tocomp(cdtime.Calendar360)
    print t3.tocomp(),t3.tocomp(cdtime.Calendar360)
    
    axes = [ {'table_entry': 'time1',
              'units': tunits,
              'coord_vals' : tvals,
              'cell_bounds' : tbnds,
              },
             {'table_entry': 'latitude',
              'units': 'degrees_north',
              'coord_vals': [0],
              'cell_bounds': [-5,5]},
             {'table_entry': 'longitude',
              'units': 'degrees_east',
              'coord_vals': [0],
              'cell_bounds':[-10,10]},
             ## {'table_entry': 'depth',
             ##  'units': 'm',
             ##  'coord_vals': [10],
             ##  'cell_bounds': [5,15]},
             ]

    values = numpy.ones(ntimes)*1013.
    return values.astype("f"), axes

def cmor_define_and_write(values, axes):
    table = '/git/cmip5-cmor-tables/Tables/CMIP5_6hrPlev'
    cmor.load_table(table)
    ## lev_axis_id = cmor.axis(**axes[2])
    lon_axis_id = cmor.axis(**axes[2])

    lat_axis_id = cmor.axis(**axes[1])

    time_axis_id = cmor.axis(**axes[0])

    #gid = cmor.grid([site_axis_id,],latitude=numpy.array([-20,]),longitude=numpy.array([150,]))


    axis_ids = [time_axis_id,lat_axis_id,lon_axis_id]
    varid = cmor.variable('psl',
                          'hPa',
                          axis_ids,
                          history = 'variable history',
                          missing_value = -99,
                          )

    cmor.write(varid, values)
    return varid
    
    
def main():
    
    cmor_initialisation()
    values,axes = setup_data()
    vid = cmor_define_and_write(values, axes)
    print cmor.close(var_id=vid,file_name=True)
    
if __name__ == '__main__':

    main()
