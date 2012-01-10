#!/usr/bin/env python
import cmor
import numpy

def main():
    
    cmor.setup(inpath='/git/cmip5-cmor-tables/Tables',
               netcdf_file_action = cmor.CMOR_REPLACE_3)
    cmor.dataset('pre-industrial control', 'ukmo', 'HadCM3', 'noleap',
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
              'units': 'days since 1861',
              'coord_vals': [48925.5, 48955, 48984.5, 49015, 49045.5, 49076, 49106.5, 49137.5, 49168, 49198.5, 49229, 49259.5 ],
              'cell_bounds': [ [ 45625, 52591],
                               [ 45656, 52619,],
                               [ 45684, 52650,],
                               [ 45715, 52680,],
                               [ 45745, 52711,],
                               [45776, 52741,],
                               [45806, 52772,],
                               [45837, 52803,],
                               [45868, 52833,],
                               [45898, 52864,],
                               [45929, 52894,],
                               [45959, 52925]],
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
            cmor.write(varid, values, ntimes_passed=1)

    cmor.close()
    
if __name__ == '__main__':

    main()
