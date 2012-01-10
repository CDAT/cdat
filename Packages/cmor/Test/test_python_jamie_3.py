#!/usr/bin/env python
import cmor
import numpy

def main():
    
    missing = -99.
    cmor.setup(inpath='Tables',
               netcdf_file_action = cmor.CMOR_REPLACE)
    cmor.dataset('historical', 'ukmo', 'HadCM3', '360_day',
                 institute_id = 'ukmo',
                 forcing = 'Nat',
                 model_id = 'HadCM3',
                 contact="Bleachers 140",
                 parent_experiment_rip="r1i3p2",
                 parent_experiment_id="lgm",branch_time=0.)

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

    values = numpy.array([missing], numpy.float32)
    myma = numpy.ma.masked_values(values, missing)
    axis_ids = list()
    for axis in axes:
        axis_id = cmor.axis(**axis)
        axis_ids.append(axis_id)
    varid = cmor.variable('ts', 'K', axis_ids, missing_value = myma.fill_value)

    cmor.write(varid, myma, time_vals = [15], time_bnds = [ [0,30] ])

    cmor.close(varid)
    
if __name__ == '__main__':

    main()
