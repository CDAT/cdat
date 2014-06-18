'''
Created on Jun 18, 2014

@author: tpmaxwel
'''
import vcs
import cdms2
import sys
import os

f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt")

x=vcs.init()
dv3d=x.createdv3d()
x.plot(s,dv3d)
