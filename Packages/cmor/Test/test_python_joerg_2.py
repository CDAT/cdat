#!/usr/bin/env python
import cmor
import numpy


def test_mode(mode,i,suffix=''):
    cmor.setup(inpath='Tables',
               netcdf_file_action = mode)
    cmor.dataset('pre-industrial control', 'ukmo', 'HadCM3', '360_day',
                 institute_id = 'ukmo',
                 model_id = 'HadCM3',
                 forcing="TO",
                 contact="Derek Jeter",
                 history = 'some global history',
                 parent_experiment_rip="r1i3p2",
                 parent_experiment_id="lgm",branch_time=0)
    
    table = 'CMIP5_Amon'
    cmor.load_table(table)
    levels = [100000.,
              92500.,
              85000.,
              70000.,
              60000.,
              50000.,
              40000.,
              30000.,
              25000.,
              20000.,
              15000.,
              10000.,
              7000.,
              5000.,
              3000.,
              2000.,
              1000.,
              999,
              998,
              997,
              996,
              995,
              994]
    
    axes = [ {'table_entry': 'time',
              'units': 'months since 2000-01-01 00:00:00',
              },
             {'table_entry': 'latitude',
              'units': 'degrees_north',
              'coord_vals': [0],
              'cell_bounds': [-1, 1]},             
             {'table_entry': 'longitude',
              'units': 'degrees_east',
              'coord_vals': [90],
              'cell_bounds': [89, 91]},
             {'table_entry': 'plevs',
              'units': 'Pa',
              'coord_vals': levels},
             ]
    
    values = numpy.array(range(len(levels)), numpy.float32)+195
    axis_ids = list()
    for axis in axes:
        axis_id = cmor.axis(**axis)
        axis_ids.append(axis_id)

    for var, units in (('ta', 'K'),):
        varid = cmor.variable(var,
                              units,
                              axis_ids,
                              history = 'variable history',
                              missing_value = -99
                              )
        print 'suffix is:',suffix
        print "Sending time bounds:",[[i,i+1]]
        cmor.write(varid, values, time_vals = [i], time_bnds = [ [i,i+1] ],file_suffix=suffix)

    fnm = cmor.close(varid,file_name=True)
    cmor.close()
    return fnm
    
fnm=''
for i in range(5):
    print i,fnm
    if i==0:
        mode = cmor.CMOR_REPLACE
    else:
        mode = cmor.CMOR_APPEND
    fnm = test_mode(cmor.CMOR_APPEND,i,fnm)

