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
                 parent_experiment_rip = 'N/A',
                 branch_time = 0.,
                 contact = 'bob')
 
    table = 'CMIP5_Amon'
    cmor.load_table(table)
    axes = [ {'table_entry': 'time',
              'units': 'days since 2000-01-01 00:00:00',
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

    for var, units, positive in (('ts', 'K', ''),
                                 ('rsut', 'W m-2', 'up'),
                                 ('rlut', 'W m-2', 'down'),):
        varid = cmor.variable(var,
                              units,
                              axis_ids,
                              history = 'variable history',
                              missing_value = -99,
                              positive = positive
                              )
        cmor.write(varid, values, time_vals = [15], time_bnds = [ [0,30] ])

    cmor.close()
    
if __name__ == '__main__':

    main()
