import cdms2,vcs
import os,sys

f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))

s=f("clt",slice(0,1))

g=cdms2.createGaussianGrid(64)
gl = cdms2.createZonalGrid(g)
regridded = s.regrid(gl)
