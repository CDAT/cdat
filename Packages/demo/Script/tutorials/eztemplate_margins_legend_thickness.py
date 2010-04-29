# Adapted for numpy/ma/cdms2 by convertcdms.py
import os, sys
import cdms2 as cdms, vcs
import EzTemplate

# Open file and retrieve data variable.
fname = os.path.join(sys.prefix, 'sample_data/clt.nc')
cfile = cdms.open(fname)
data  = cfile('clt')

# Initialize vcs.
x = vcs.init()

# Configure the template.
M = EzTemplate.Multi(rows=4, columns=3)
M.margins.top    = .25
M.margins.bottom = .25
M.margins.left   = .25
M.margins.right  = .25

# Legend uses bottom margin for display. We need to shrink it.
M.legend.thickness = .1

# Plot data.
for i in range(12):
    t = M.get()
    x.plot(data[i], t)
    
