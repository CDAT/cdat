#!/usr/bin/env python

import cmor
import numpy

def define_axes(axes):
    axis_ids = list()
    for axis in axes:
        axis_id = cmor.axis(**axis)
        axis_ids.append(axis_id)

    print 'cmor.axis calls complete'
    return axis_ids

def define_write_var(axis_ids, entry, unit, values):
    varid = cmor.variable(entry,
                          unit,
                          axis_ids,
                          missing_value = -99
                          )

    print 'cmor.variable call complete'
    
    cmor.write(varid, values, time_vals = [15.0], time_bnds = [0., 30.0])

    print 'cmor.write call complete'


def cmor_ini():
    cmor.setup(inpath='Tables',
               netcdf_file_action = cmor.CMOR_REPLACE)
    cmor.dataset('pre-industrial control', 'mohc', 'HadGEM2: source',
                 '360_day',
                 institute_id = 'ukmo',
                 model_id = 'HadGEM2',
                 history = 'some global history',
                 forcing = 'N/A',
                 parent_experiment_id = 'N/A',
                 parent_experiment_rip = 'N/A',
                 branch_time = 0.,
                 contact = 'bob')

def define_write_clisccp():
    cmor.load_table('CMIP5_cfMon')
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
             {'table_entry': 'plev7',
              'coord_vals': [90000., 74000., 62000., 50000., 37500., 24500., 9000.],
              'cell_bounds': [[100000., 80000.], [80000.,  68000.],  [68000.,  56000.],  [56000.,  44000.],  [44000.,  31000.],  [31000.,  18000.],  [18000.,   0.]],
              'units': 'Pa',
              },
             {'table_entry': 'tau',
              'coord_vals': [0.15, 0.8, 2.45, 6.5, 16.2, 41.5, 100.],
              'cell_bounds':[ [0.0,  0.3],  [0.3,  1.3],  [1.3,  3.6],  [3.6,  9.4], [9.4, 23.0], [23.0, 60.0], [60.0, 100000]],
              'units': '1'}
             ]

    axis_ids = define_axes(axes)


    values = numpy.array([0.0004,]*49, numpy.float32)
    values = numpy.reshape(values, (1, 1, 1, 7, 7))

    define_write_var(axis_ids, 'clisccp', '1', values)

def define_write_landcoverfrac():
    cmor.load_table('CMIP5_Lmon')
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
             {'table_entry': 'vegtype',
              'coord_vals': ['landcover'],
              'units': '1',
              },
             ]

    axis_ids = define_axes(axes)

    values = numpy.array([2.], numpy.float32)
    values = numpy.reshape(values, (1, 1, 1, 1))
             
    define_write_var(axis_ids, 'landCoverFrac', '1', values)
    
def main():

    cmor_ini()
    define_write_clisccp()
    define_write_landcoverfrac()
    cmor.close()
    
if __name__ == '__main__':

    main()
