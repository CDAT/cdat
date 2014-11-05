'''
Created on Jun 18, 2014

@author: tpmaxwel
'''
import vcs
import cdms2
import sys
import os, time
import subprocess, signal 
from DV3D.TestManager import vcsTest  

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
    datasetPath = os.path.join( testDataDir, 'WRF', 'wrfout_d03_2013-07-02_02-00-00.nc' )
    f = cdms2.open( datasetPath )
    varname = "U"
   
else:
    dataDir1 = "/Developer/Data/AConaty/comp-ECMWF"
    datasetPath = os.path.join( dataDir1, "geos5.xml")
    f = cdms2.open( datasetPath )
    varname = "uwnd"
    
print "Reading variable %s in dataset %s " % ( varname, datasetPath )
    
#    f = cdms2.open( os.path.join( vcs.prefix, "sample_data", "geos5-sample.nc") )
#    u = f["uwnd"] 

# dv3d = vcs.create3d_scalar('hoffmuller','xyt')

test1 =  vcsTest( 'dv3d_volume_test', roi=( -105.0, -15.0, 7.0, 50.0 ), file=datasetPath, vars = [ varname ], 
                     parameters={'VerticalScaling': 3.0, 
                                 'ToggleVolumePlot': vcs.on,
                                 'ToggleSurfacePlot': vcs.off, 
                                 'ScaleOpacity': [0.0, 0.8],
                                 'ScaleColormap': [-15.0, 10.0, 1], 
                                 'ScaleTransferFunction':  [6.0, 12.0, 1], 
                                 'BasemapOpacity': [0.5],
                                 'XSlider': ( vcs.off ),
                                 'ZSlider': ( vcs.off ),
                                 'YSlider': ( vcs.off ), 
                                 }  )  

test1.interact()     



