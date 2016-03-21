import cdms2
import cdat_info
import os

pth = os.path.join(cdat_info.get_sampledata_path(),"clt.nc")

f=cdms2.open("file://"+pth)
