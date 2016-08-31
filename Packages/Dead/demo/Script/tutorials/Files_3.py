# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms
import sys
import cdat_info

path=cdat_info.get_sampledata_path()+'/'

files = [ path+'u_2000.nc',
	  path+'u_2001.nc',
	  path+'u_2002.nc',
	  ]

# First let's open the output file for writing (and possibly erase existing file)
fout=cdms.open('u_concatenated.nc','w')

for file in files:
	f=cdms.open(file)
	u=f('u')
	f.close()
	fout.write(u)
	
fout.close()
                    
