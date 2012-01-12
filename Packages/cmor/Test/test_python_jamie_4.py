#!/usr/bin/env python
import cmor
import numpy

def main():
    
    cmor.setup(inpath='Tables',
               netcdf_file_action = cmor.CMOR_REPLACE)
    cmor.dataset('historical', 'ukmo', 'HadCM3', '360_day',
                 institute_id = 'ukmo',
                 forcing = 'SO',
                 model_id = 'HadCM3',contact="Dusty Baker (even though he was a Dodgers",
                 parent_experiment_rip="r1i3p2",
                 parent_experiment_id="lgm",branch_time=0)

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

    axis_ids = list()
    for axis in axes:
        axis_id = cmor.axis(**axis)
        axis_ids.append(axis_id)

    for var, units, val in (('ts', 'K',278),  ('ps', 'hPa',974.2)):
        varid = cmor.variable(var,
                              units,
                              axis_ids,
                              )

        values = numpy.array([val], numpy.float32)
        cmor.write(varid, values, time_vals = [15], time_bnds = [ [0,30] ])

    cmor.close()
    
if __name__ == '__main__':

    main()
