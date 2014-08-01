'''
Created on Jun 18, 2014

@author: tpmaxwel
'''
import vcs
import cdms2
import sys
import os, time
import subprocess, signal    

class DataType:
   STRUCTURED = 0
   UNSTRUCTURED = 1                           

data_type = DataType.UNSTRUCTURED

if data_type == DataType.STRUCTURED:
    
    proc_specs = subprocess.check_output('ps').split('\n')
    for proc_spec in proc_specs:
        if 'UVIS_DV3D' in proc_spec or 'uvcdat' in proc_spec:
            pid = int( proc_spec.split()[0] )
            if pid <> os.getpid():
                os.kill( pid, signal.SIGKILL )
                print "Killing proc: ", proc_spec

    testDataDir = '/Users/tpmaxwel/Data'
    datasetPath = os.path.join( testDataDir, 'WRF', 'wrfout_d03_2013-07-02_02-00-00.nc' )
    f = cdms2.open( datasetPath )
    varname = "U"
   
else:
    dataDir1 = "/Developer/Data/AConaty/comp-ECMWF"
    datasetPath = os.path.join( dataDir1, "ac-comp1-geos5.xml")
    f = cdms2.open( datasetPath )
    varname = "uwnd"
    
#    f = cdms2.open( os.path.join( sys.prefix, "sample_data", "geos5-sample.nc") )
#    u = f["uwnd"] 

# dv3d = vcs.create3d_scalar('hoffmuller','xyt')

u = f[varname] 
if not u is None:
#    dv3d = vcs.get3d_scalar('xyt')
    dv3d = vcs.get3d_scalar()
    
    x = vcs.init()
    print "3D_Scalar elements: ", str( x.listelements('3d_scalar') )
    x.plot( u, dv3d )
    x.interact()
else:
    print>>sys.stderr, "\n\n  Can't find variable %s in dataset %s" % ( varname, datasetPath )



