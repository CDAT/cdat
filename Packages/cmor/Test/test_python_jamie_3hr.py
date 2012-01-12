#!/usr/bin/env python
import cmor
import numpy

def main():
    
    cmor.setup(inpath='Tables',
               netcdf_file_action = cmor.CMOR_REPLACE_3)
    cmor.dataset('pre-industrial control', 'ukmo', 'HadCM3', '360_day',
                 institute_id = 'ukmo',
                 model_id = 'HadCM3',
                 history = 'some global history',
                 forcing = 'N/A',
                 parent_experiment_id = 'N/A',
                 branch_time = 0.,
                 contact = 'bob')
 
    table = 'CMIP5_3hr'
    cmor.load_table(table)
    axes = [ {'table_entry': 'time1',
              'units': 'hours since 2000-01-01 00:00:00',
              },
             {'table_entry': 'latitude',
              'units': 'degrees_north',
              'coord_vals': [0],
              'cell_bounds': [-1, 1]},             
             {'table_entry': 'longitude',
              'units': 'degrees_east',
              'coord_vals': [90],
              'cell_bounds': [89, 91]},
             ]

    values = numpy.array([1.], numpy.float32)+200
    axis_ids = list()
    for axis in axes:
        axis_id = cmor.axis(**axis)
        axis_ids.append(axis_id)

    varid = cmor.variable('tas',
                          'K',
                          axis_ids,
                          history = 'variable history',
                          missing_value = -99,
                          )
    cmor.write(varid, values, time_vals = [0.], time_bnds = [ [0,3.] ])

    cmor.close()
    
if __name__ == '__main__':

    main()
