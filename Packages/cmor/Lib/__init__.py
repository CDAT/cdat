from cmor_const import *

from pywrapper import axis,variable,write,setup,load_table,dataset,set_table,zfactor,close,grid,set_grid_mapping,time_varying_grid_coordinate,set_cur_dataset_attribute,get_cur_dataset_attribute,has_cur_dataset_attribute,create_output_path,set_variable_attribute,get_variable_attribute,has_variable_attribute

try:
  from check_CMOR_compliant import checkCMOR
except Exception,err:
  print err
  pass
