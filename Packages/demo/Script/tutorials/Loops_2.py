# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms, MV2 as MV
import sys

# Creates string for path to data
pth=sys.prefix+'/sample_data/'

# Creates list containg path to files
files = [ pth+'u_2000.nc',
          pth+'u_2001.nc',
          pth+'u_2002.nc',
          ]

# Now LOOP through the files
for file in files:
    # Open a file
    f=cdms.open(file)

    # Get the 'u' variable
    u=f('u')

    if file == files[0]:    # First file
        avg=MV.sum(u,0)     # Compute the time sum
        n=u.shape[0]        # Number of month in this file
        
    else:                   # Another file
        avg=avg+MV.sum(u,0) # Compute the time sum and add it to our final variable
        n=n+u.shape[0]
    f.close()
        
        
# Now divide by the total number of  times
avg=avg/n
