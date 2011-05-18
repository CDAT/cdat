# Forecast test

print 'Test 19: Forecast I/O ... ',

import cdms2, os, numpy
import cdms2.forecast
import cdms2.tvariable
from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

# Generate some test data, then write it out.  This section doesn't test anything.
xaxis = cdms2.createAxis( [ 0.1, 0.2 ], id='x' )
taxis = cdms2.createAxis( [ 10.0, 20.0 ], id='t' )
taxis.units='seconds'    # required!
#...  Note that this is seconds since the forecast was generated, i.e. the appropriate
# standard_name is "forecast_period".
taxis.standard_name = 'forecast_period'

taxis.designateTime()
v1 = cdms2.tvariable.TransientVariable( [[1.1,1.2],[2.1,2.1]], axes=[taxis,xaxis], id='var' )
v2 = cdms2.tvariable.TransientVariable( [[3.1,3.2],[4.1,4.1]], axes=[taxis,xaxis], id='var' )
v3 = cdms2.tvariable.TransientVariable( [[5.1,5.2],[6.1,6.1]], axes=[taxis,xaxis], id='var' )
date1 = cdms2.tvariable.TransientVariable( 20100825, id='nbdate' )
sec1 = cdms2.tvariable.TransientVariable( 0, id='nbsec' )
date2 = cdms2.tvariable.TransientVariable( 20100826, id='nbdate' )
sec2 = cdms2.tvariable.TransientVariable( 0, id='nbsec' )
date3 = cdms2.tvariable.TransientVariable( 20100827, id='nbdate' )
sec3 = cdms2.tvariable.TransientVariable( 0, id='nbsec' )
f1 = cdms2.open('test_fc1','w')
f2 = cdms2.open('test_fc2','w')
f3 = cdms2.open('test_fc3','w')
f1.write(v1)
f1.write(date1)
f1.write(sec1)
f2.write(v2)
f2.write(date2)
f2.write(sec2)
f3.write(v3)
f3.write(date3)
f3.write(sec3)
f1.close()
f2.close()
f3.close()

os.system("../Script/cdscan -q --forecast -x 'test_fc.xml' test_fc?")

# Read in the data.

fcs = cdms2.forecast.forecasts( 'test_fc.xml', ("2010-08-25","2010-08-27") )
vin = fcs('var')
fcaxis = vin._TransientVariable__domain[0][0]
tinaxis = vin._TransientVariable__domain[1][0]
xinaxis = vin._TransientVariable__domain[2][0]

# Test vin against original data, looking especially at the forecast axis.
# The following should be True, error otherwise:
if not vin.id == v1.id : markError("wrong variable ID")
if not numpy.alltrue( vin[0,:,:]==v1[:,:] ) : markError("wrong variable data for fc1")
if not numpy.alltrue( vin[1,:,:]==v2[:,:] ) : markError("wrong variable data for fc2")
if not vin.shape == (2,2,2) : markError("wrong variable shape")
if not numpy.alltrue( tinaxis._data_ == taxis._data_ ) : markError( "wrong t axis data" )
if not numpy.alltrue( xinaxis._data_ == xaxis._data_ ) : markError( "wrong x axis data" )
if not tinaxis.isTime()==True : markError( "time axis isn't time" )
if not xinaxis.isTime()==False : markError( "non-time axis is time" )
if not fcaxis.isTime()==False : markError( "forecast axis is time" )
if not fcaxis.isForecast()==True : markError( "forecast axis isn't forecast" )
if not fcaxis.shape == (3,) : markError( "forecast axis shape is wrong" )
#...Note that fcaxis is an Axis so its shape comes from len(fcaxis.node) which is the
# number of forecasts described in test_fc.xml; in contrast for TransientAxis the shape
# would be len(fcaxis._data_) which is the number of forecasts that have been read.
if not fcaxis._data_ == [2010082500000L, 2010082600000L] : markError( "wrong forecast axis data" )
if not fcaxis.id == 'fctau0' : markError( "wrong forecast axis id" )

reportError()
