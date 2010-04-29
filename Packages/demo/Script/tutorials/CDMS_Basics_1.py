# Adapted for numpy/ma/cdms2 by convertcdms.py
# Import the cdms module and the sys module
import cdms2 as cdms,sys

# Construct the string containing the path to the data_file
file_name=sys.prefix+'/sample_data/clt.nc'

# open the file with the cdms interface
file=cdms.open(file_name)

# Read the variable "clt" contained in the file
# and load it in memory
clt=file('clt')

# Close the file now that we don't need it anymore
file.close()
