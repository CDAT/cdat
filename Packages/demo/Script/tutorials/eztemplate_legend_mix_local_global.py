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
for i in range(12):
    if i%2:
        if i%4 == 3:
            M.legend.direction = 'vertical'
        t = M.get(legend='local')
        M.legend.direction = 'horizontal'
    else:
        t = M.get()
    x.plot(data[i], t)
    
