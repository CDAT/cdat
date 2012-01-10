import cmor

def multi_call_test():
    cmor.setup(inpath='Tables',netcdf_file_action=cmor.CMOR_REPLACE)

    cmor.dataset('historical', 'ukmo', 'HadCM3', '360_day',
                 institute_id="PCMDI",
                 model_id='HadCM3',contact="Ron Nen",forcing='SO',
                 parent_experiment_id="lgm",
                 parent_experiment_rip="r1i3p2",
                 branch_time=0.)
    
    table='CMIP5_Amon'
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
    varid = cmor.variable('ts', 'K', axis_ids)
    cmor.write(varid, [275], time_vals = [15], time_bnds = [ [0,30] ])
    print 'First write worked as expected'
    try:
        cmor.write(varid, [275], time_vals = [15], time_bnds = [ [0], [30] ])
        raise Exception,"We shouldn't be getting in here"
    except:
        print 'Second write that should have failed did fail, good!'
        pass
    cmor.close(varid)
    print 'Success'


if __name__ == '__main__':
    multi_call_test()
