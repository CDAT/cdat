import cdms2
import cdat_info
import os

f=cdms2.open(os.path.join(cdat_info.get_sampledata_path(),"clt.nc"))

s=f("clt",slice(0,1))

g=cdms2.createGaussianGrid(64)
gl = cdms2.createZonalGrid(g)
regridded = s.regrid(gl)
