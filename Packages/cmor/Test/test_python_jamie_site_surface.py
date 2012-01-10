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
    axes = [ {'table_entry': 'time1',
              'units': 'days since 2000-01-01 00:00:00',
              },
             {'table_entry': 'site',
              'units': '',
              'coord_vals': [0]},
             ]

    values = numpy.array([215.], numpy.float32)
    return values, axes

def cmor_define_and_write(values, axes):
    table = 'CMIP5_cfSites'
    cmor.load_table(table)
    site_axis_id = cmor.axis(**axes[1])

    time_axis_id = cmor.axis(**axes[0])

    gid = cmor.grid([site_axis_id,],latitude=numpy.array([-20,]),longitude=numpy.array([150,]))


    axis_ids = [time_axis_id,gid]
    varid = cmor.variable('rlut',
                          'W m-2',
                          axis_ids,
                          history = 'variable history',
                          missing_value = -99,
                          positive = 'up'
                          )

    cmor.write(varid, values, time_vals = [15])
    
    
def main():
    
    cmor_initialisation()
    values, axes = setup_data()
    cmor_define_and_write(values, axes)
    print cmor.close(file_name=True)
    
if __name__ == '__main__':

    main()
