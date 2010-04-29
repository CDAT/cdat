# Adapted for numpy/ma/cdms2 by convertcdms.py
# First import necessary modules
import sys,os,thermo,vcs,cdms2 as cdms

# initialize the VCS Canvas and creates the "Thermodynamic Diagram" graphic method
x=vcs.init()
x.portrait()
th=thermo.Gth(x=x,name='test')

## Now open the sample dataset and reads in the data for temperature as a function of level
## Open the file, read the T
f=cdms.open(os.path.join(sys.prefix,'sample_data','thermo.nc'))
t=f('t')

# In this example we need to redefine the the "level" axis on "ta" because it needs to be in Pa
p=t.getLevel()
p=cdms.createAxis(p[:]*100)
p.id='level'
t.setAxis(1,p) ## Reset the axis on T

# Now we are good to go and plot t
th.plot_TP(t)
