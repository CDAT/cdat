'''
Created on Jun 18, 2014

@author: tpmaxwel
'''
import vcs
import cdms2
import sys
import os

class DataType:
   STRUCTURED = 0
   UNSTRUCTURED = 1                           

data_type = DataType.STRUCTURED

if data_type == DataType.UNSTRUCTURED:
    testDataDir = '/Users/tpmaxwel/Data'
    testDataFile = os.path.join( testDataDir, 'WRF', 'wrfout_d03_2013-07-02_02-00-00.nc' )
    f = cdms2.open( testDataFile )
    u = f["U"] 
else:
    f = cdms2.open( os.path.join( sys.prefix, "sample_data", "geos5-sample.nc") )
    u = f["uwnd"] 

x = vcs.init()
dv3d = x.createdv3d()
x.plot( u, dv3d )
x.interact()
