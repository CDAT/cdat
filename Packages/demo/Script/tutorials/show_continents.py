# Adapted for numpy/ma/cdms2 by convertcdms.py
# Import the modules needed for the tuturial
import vcs, cdms2 as cdms, cdutil, time, os, sys

# Open data file:
filepath = os.path.join(sys.prefix, 'sample_data/clt.nc')
cdmsfile = cdms.open( filepath )

# Extract a 3 dimensional data set and get a subset of the time dimension
data = cdmsfile('clt', longitude=(-180, 180), latitude = (-90., 90.))

continent_type = { 0: "0 signifies - No Continents",
                   1: "1 signifies - Fine Continents",
                   2: "2 signifies - Coarse # Continents",
                   3: "3 signifies - United States Continents",
                   4: "4 signifies - Political Borders Continents",
                   5: "5 signifies - North American Rivers Continents",
                   6: "6 signifies - User continent file data_continent_other7",
                   7: "7 signifies - User continent file data_continent_other8",
                   8: "8 signifies - User continent file data_continent_other9",
                   9: "9 signifies - User continent file data_continent_other10",
                   10:  "10 signifies - User continent file data_continent_other11",
                   11:  "11 signifies - User continent file data_continent_other12"
            }

# Initial VCS:
v = vcs.init()

for i in range( 12 ):
 print "continent_type ", continent_type[i]
 v.plot(data, continents=i)
 time.sleep(2)
 v.clear()