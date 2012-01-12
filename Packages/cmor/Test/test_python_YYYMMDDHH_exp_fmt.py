import cmor

def path_test():
    cmor.setup(inpath='Test',netcdf_file_action=cmor.CMOR_REPLACE)

    cmor.dataset('mytest2010030812', 'ukmo', 'HadCM3', '360_day',
                 institute_id="PCMDI",
                 model_id='HadCM3',forcing='co2')
    
    table='CMIP5_Amon_YYYYMMDDHH'
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
    cmor.write(varid, [273])
    path=cmor.close(varid, file_name=True)

    print "Saved file: ",path

if __name__ == '__main__':
    path_test()
