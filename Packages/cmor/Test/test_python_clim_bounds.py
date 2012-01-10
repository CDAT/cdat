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
              'units': 'days since 2000-01-01 00:00:00',
              },
             {'table_entry': 'depth_coord',
              'units': 'm',
              'coord_vals': [ 500,1000.],
              'cell_bounds': [ 0.,750.,1200.]},
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

    for var, units, value in (('tnpeot', 'W m-2', 274),):
        values = numpy.array([value,]*len(axes[1]['coord_vals']), numpy.float32)
        varid = cmor.variable(var,
                              units,
                              axis_ids,
                              history = 'variable history',
                              missing_value = -99
                              )
        for i in range(12):
            cmor.write(varid, values, time_vals = [30*i+15], time_bnds = [ [30*i,360+30*(i+1)] ])

    cmor.close()
    
if __name__ == '__main__':

    main()
