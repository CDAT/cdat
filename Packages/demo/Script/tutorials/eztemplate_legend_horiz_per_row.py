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
M.legend.stretch = 2.5  # 250% of width

# Plot data.
for i in range(12):
    t = M.get(legend='local')

    # Display legend every 3rd graph.
    if not i%3:
        t.legend.priority = 0
        
    x.plot(data[i], t)
    
