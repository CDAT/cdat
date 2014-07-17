'''
Created on Jun 18, 2014

@author: tpmaxwel
'''
import vcs
import cdms2
import sys
import os
import subprocess, signal    

class DataType:
   STRUCTURED = 0
   UNSTRUCTURED = 1                           

data_type = DataType.STRUCTURED

if data_type == DataType.UNSTRUCTURED:
    
    proc_specs = subprocess.check_output('ps').split('\n')
    for proc_spec in proc_specs:
        if 'UVIS_DV3D' in proc_spec or 'uvcdat' in proc_spec:
            pid = int( proc_spec.split()[0] )
            if pid <> os.getpid():
                os.kill( pid, signal.SIGKILL )
                print "Killing proc: ", proc_spec

    testDataDir = '/Users/tpmaxwel/Data'
    testDataFile = os.path.join( testDataDir, 'WRF', 'wrfout_d03_2013-07-02_02-00-00.nc' )
    f = cdms2.open( testDataFile )
    u = f["U"] 
    v = f["V"] 
    
else:
    
    f = cdms2.open( os.path.join( sys.prefix, "sample_data", "geos5-sample.nc") )
    u = f["uwnd"] 
    v = f["vwnd"] 

dv3d = vcs.create3Dvector("stream")

x = vcs.init()
x.plot( u, v, dv3d )
x.interact()


