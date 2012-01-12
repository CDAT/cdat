import cmor
import numpy


def test_mode(mode):
    cmor.setup(inpath='Tables',
               netcdf_file_action = mode)
    cmor.dataset('pre-industrial control', 'ukmo', 'HadCM3', '360_day',
                 institute_id = 'ukmo',
                 model_id = 'HadCM3',
                 forcing="TO",
                 contact="Derek Jeter",
                 history = 'some global history',
                 parent_experiment_id="lgm",
                 parent_experiment_rip="r1i1p1",
                 branch_time=0)
    
    table = 'CMIP5_fx'
    cmor.load_table(table)
    axes = [ 
             {'table_entry': 'latitude',
              'units': 'degrees_north',
              'coord_vals': [0],
              'cell_bounds': [-1, 1]},             
             {'table_entry': 'longitude',
              'units': 'degrees_east',
              'coord_vals': [90],
              'cell_bounds': [89, 91]},
          
             ]
    values = numpy.array([5000],numpy.float32)
    axis_ids = list()


    for axis in axes:
        axis_id = cmor.axis(**axis)
        axis_ids.append(axis_id)
    for var, units in (('deptho', 'm'),):
        varid = cmor.variable(var,
                              units,
                              axis_ids,
                              history = 'variable history'
                              )
        cmor.write(varid, values)
    fnm = cmor.close(varid,file_name=True)
    cmor.close()
    return fnm
print test_mode(cmor.CMOR_REPLACE)
