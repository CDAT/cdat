# Adapted for numpy/ma/cdms2 by convertcdms.py
import os, sys
import cdms2 as cdms, vcs
import EzTemplate

# Open file and retrieve data variable.
fname = os.path.join(vcs.prefix, 'sample_data/clt.nc')
cfile = cdms.open(fname)
data  = cfile('clt')

# Initialize vcs.
x = vcs.init()

# Configure the template.
M = EzTemplate.Multi(rows=4, columns=3)

# Plot data.
icol = 3
irow = 4
for i in range(12):
    if not i%3:
        irow -= 1
    icol -= 1
    t = M.get(column=icol, row=irow)
    x.plot(data[i], t)
    if not icol:
        icol = 3
        
    
