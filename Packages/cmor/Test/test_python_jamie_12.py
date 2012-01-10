#!/usr/bin/env python

import cmor
import numpy

def define_axes(axes):
    axis_ids = list()
    for axis in axes:
        axis_id = cmor.axis(**axis)
        axis_ids.append(axis_id)

    print 'MY:cmor.axis calls complete'
    return axis_ids

def define_write_var(axis_ids, entry, unit, values):
    varid = cmor.variable(entry,
                          unit,
                          axis_ids,
                          missing_value = -99
                          )



    cmor.write(varid, values, time_vals = [15.0], time_bnds = [0., 30.0])
    cmor.close(varid, preserve = True)
    cmor.write(varid, values, time_vals = [45.0], time_bnds = [30., 60.0])
    cmor.close()


def cmor_ini():
    cmor.setup(inpath='/git/cmip5-cmor-tables/Tables',
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
                 contact = 'brian clough')


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
    define_write_landcoverfrac()
    
if __name__ == '__main__':

    main()
