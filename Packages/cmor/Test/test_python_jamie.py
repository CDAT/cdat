import cmor

def path_test():
    cmor.setup(inpath='TestTables',netcdf_file_action=cmor.CMOR_REPLACE)

    cmor.dataset('historical', 'ukmo', 'HadCM3', '360_day',model_id='HadCM3',forcing='Nat',
                 contact="J.T. Snow",
                 institute_id="PCMDI",
                 parent_experiment_id="N/A",
                 parent_experiment_rip="N/A",
                 branch_time=0)
    
    table='CMIP5_Amon'
    cmor.load_table(table)
    axes = [ {'table_entry': 'time',
              'units': 'days since 2000-01-01 00:00:00',
              'coord_vals': [15],
              'cell_bounds': [0, 30]
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
    cmor.write(varid, [275])
    path=cmor.close(varid, file_name=True)

    print path

if __name__ == '__main__':
    path_test()
