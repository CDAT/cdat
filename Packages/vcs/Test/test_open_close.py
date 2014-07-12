import vcs
import sys
import cdms2
import vtk
import os

f=cdms2.open(os.path.join(sys.prefix,'sample_data','clt.nc'))
s=f("clt")
x=vcs.init()
x.open()
raw_input("Press enter")
x.close()
raw_input("Press enter")
bg = False
#bg = True
x.plot(s,bg=bg)
raw_input("Press enter")
x.close()
raw_input("Press enter")
x.open()
raw_input("Press enter")
