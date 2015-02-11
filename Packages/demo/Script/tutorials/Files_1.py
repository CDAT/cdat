# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms, sys
import cdat_info

file=cdat_info.get_prefix()+'/sample_data/clt.nc'

# Open the file via cdms
f=cdms.open(file)

# Now we are going to query the file
# Get the names of the variables in the file
variables = f.listvariables()

print variables        # ['clt', 'u', 'v']

# Get the names of the dimensions present in files
dims = f.listdimension()

# Get the global (file) attributes
glob = f.listglobal()
print glob             # ['center', 'comments', 'Conventions', 'model']

# Get attributes attached to a variable (here clt)
clt_att = f.listattribute('clt')
print clt_att
# ['units', 'time_statistic', 'long_name', 'grid_name', 'comments', 'missing_value', 'grid_type']
