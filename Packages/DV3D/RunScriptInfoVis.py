'''
Created on Feb 4, 2014

@author: tpmaxwel
'''

import os, os.path, sys, argparse, time, multiprocessing
from DistributedPointCollections import kill_all_zombies
from Application import DV3DApp
from MultiVarPointCollection import InterfaceType
from ConfigurationFunctions import PlotType

parser = argparse.ArgumentParser(description='DV3D Point Cloud Viewer')
parser.add_argument( 'PATH' )
parser.add_argument( '-d', '--data_dir', dest='data_dir', nargs='?', default="~/data", help='input data dir')
parser.add_argument( '-t', '--data_type', dest='data_type', nargs='?', default="GEOS5", help='input data type')
ns = parser.parse_args( sys.argv )

kill_all_zombies()
point_size = 1
n_overview_points = 500000
grid_coords = ( None, None, None, None )
data_dir = os.path.expanduser( ns.data_dir )
height_varnames = []
var_proc_op = None
interface = None
roi = None # ( 0, 0, 50, 50 )


if ns.data_type == "WRF":
    data_file = os.path.join( data_dir, "WRF/wrfout_d01_2013-05-11_00-00-00.nc" )
    grid_file = None
    varnames = ["U", "V" ]        
elif ns.data_type == "CAM":
    data_file = os.path.join( data_dir, "CAM/CAM_data.nc" )
    grid_file = os.path.join( data_dir, "CAM/ne120np4_latlon.nc" )
    varnames = ["U", "V" ]
    height_varnames = [ "Z3" ]
elif ns.data_type == "ECMWF":
    data_file = os.path.join( data_dir, "AConaty/comp-ECMWF/ecmwf.xml" )
    grid_file = None
    varnames = ["U_velocity", "V_velocity"  ]   
elif ns.data_type == "GEOS5":
    data_file = os.path.join( data_dir, "AConaty/comp-ECMWF/ac-comp1-geos5.xml" )
    grid_file = None
    varnames = ["uwnd", "vwnd"  ]   
elif ns.data_type == "MMF":
    data_file = os.path.join( data_dir, "MMF/diag_prs.20080101.nc" )
    grid_file = None
    varnames = ["U", "V" ]
elif ns.data_type == "GEOD":
    file_name =  "temperature_19010101_000000.nc" # "vorticity_19010102_000000.nc" # 
    data_file = os.path.join( data_dir, "GeodesicGrid", file_name )
    grid_file = os.path.join( data_dir, "GeodesicGrid", "grid.nc" )
    varnames = ["temperature_ifc", "vorticity" ] 
elif ns.data_type == "CubedSphere":
    file_name =  "vsnow00-10.cam.h1.2006-12-01-00000.nc" # "vorticity_19010102_000000.nc" # 
    data_file = os.path.join( data_dir, "CubedSphere/3d", file_name )
    grid_file = None
#    grid_coords = ( 'lon', 'lat', 'lev', None )
    varnames = ["U", "V" ]
elif ns.data_type == "CSU":
    file_name =  "psfc.nc" 
    data_file = os.path.join( data_dir, "ColoState", file_name )
    grid_file = os.path.join( data_dir, "ColoState", "grid.nc" )
    varnames = [ "pressure" ] 

g = DV3DApp() 
ncores = multiprocessing.cpu_count()
g.init( init = ( grid_file, data_file, interface, varnames, grid_coords, var_proc_op, roi, 'xyz' ), n_overview_points=n_overview_points, n_cores=ncores, blocking=True  ) #, plot_type = PlotType.List  ) 

rw = g.getRenderWindow() 
print "Got render window" ; sys.stdout.flush() 

g.start()


     
  
