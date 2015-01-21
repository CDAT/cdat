# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms, MV2 as MV, genutil
import sys
import cdat_info

pth=cdat_info.get_prefix()+'/sample_data/'

files = [ pth+'u_2000.nc',
          pth+'u_2001.nc',
          pth+'u_2002.nc',
          ]

for file in files:
    f=cdms.open(file)
    u=f('u')
    
    if file == files[0]:          # First file
        sh=list(u.shape)          # Create a list with the shape of the data
        sh.insert(0,1)            # Insert value 1 in front of the list
        accumulation = u
        newdim = MV.reshape(u,sh) # Create a new 1D dimension
        
    else:
        # add u at the end of accumaltion on dimension 0
        accumulation = MV.concatenate((accumulation,u))
        tmp = MV.reshape(u,sh)                # Create a new 1D dimension
        newdim = MV.concatenate((newdim,tmp)) # Add u to the newdim over the new dimension
        
    f.close()
    
    
print accumulation.shape   # All time added over the same dimension
print newdim.shape         # Has a new dimension for years

avg = MV.average(accumulation)
std = genutil.statistics.std(newdim)

print avg.shape
print std.shape

