# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,sys,os

path=sys.prefix+'/sample_data/'

# List files in sample_data directory
my_files=os.listdir(path)

# Now loops through values in my_files and assign them
# one after the other to file

for file in my_files:
    if file[-3:]=='.nc':     # Deal only with NetCDF files
        print 'Dealing with file:',file
        f=cdms.open(path+file)

        variables=f.listvariables()

        # Now loops through all the variables in the file and print their shape
        for v in variables:
            V=f[v]       # Create a reference to file variable
            print '\t',v,'shape is:',V.shape      # \t is to add a tabulation
            
        f.close()
        


