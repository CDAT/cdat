#!/usr/bin/env python
import cmor
import numpy

def main():
    
    cmor.setup(inpath='/git/cmip5-cmor-tables/Tables',
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
 
    table = 'CMIP5_Oclim'
    cmor.load_table(table)
    axes = [ {'table_entry': 'time2',
              'units': 'days since 1850-01-01 00:00:00',
              'coord_vals' : [15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5],
              'cell_bounds':[0,31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365],
              },
             {'table_entry': 'depth_coord',
              'units': 'm',
              'coord_vals': [5000., 3000., 2000., 1000.],
              'cell_bounds': [ 5000., 3000., 2000., 1000.,0]},
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

    for var, units, value in (('difvso', 'm2 s-1', 274.),):
        values = numpy.ones(map(lambda x: len(x["coord_vals"]),axes))*value
        varid = cmor.variable(var,
                              units,
                              axis_ids,
                              history = 'variable history',
                              missing_value = -99
                              )
        cmor.write(varid, values)

    cmor.close()
    
if __name__ == '__main__':

    main()
