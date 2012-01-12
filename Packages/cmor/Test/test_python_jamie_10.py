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
                 branch_time = 0,
                 contact = 'brian clough')
 
    table = 'CMIP5_Amon'
    cmor.load_table(table)
    axes = [ {'table_entry': 'time',
              'units': 'days since 2000-01-01 00:00:00',
              },
             {'table_entry': 'plevs',
              'units': 'Pa',
              'coord_vals': [100000., 92500., 85000., 70000., 60000., 50000., 40000., 30000., 25000., 20000., 15000., 10000., 7000., 5000., 3000., 2000., 1000.]},
             {'table_entry': 'latitude',
              'units': 'degrees_north',
              'coord_vals': [0],
              'cell_bounds': [-1, 1]},             
             {'table_entry': 'longitude',
              'units': 'degrees_east',
              'coord_vals': [90],
              'cell_bounds': [89, 91]},
             ]


    axis_ids = list()
    for axis in axes:
        print 'doing:',axis
        axis_id = cmor.axis(**axis)
        axis_ids.append(axis_id)

    for var, units, value in (('ta', 'K', 274), ('ua', 'm s-1', 10)):
        values = numpy.array([value,]*len(axes[1]['coord_vals']), numpy.float32)
        varid = cmor.variable(var,
                              units,
                              axis_ids,
                              history = 'variable history',
                              missing_value = -99
                              )
        cmor.set_variable_attribute(varid, 'cell_measures', 'BLABLABLA')
        cmor.write(varid, values, time_vals = [15], time_bnds = [ [0,30] ])

    cmor.close()
    
if __name__ == '__main__':

    main()
