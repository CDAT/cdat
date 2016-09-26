import string, os
import cdms2
from analyzer import Analyzer
import numpy

def eof (v1, v2=None, nr=4, latweight_choice=None, mean_choice=None):
    g = v1.getGrid()
    if g is None:
        raise ValueError, "u does not have spatial dimensions."
    latw, longw = g.getWeights()
    latw = latw / numpy.maximum.reduce(latw)
    if latweight_choice is None:
        latweight = 1.0 + 0.0 * latw
    elif latweight_choice == 'latitude':
        latweight = latw 
    else:
        latweight = sqrt(latw)

    lat_axis = v1.getLatitude()
    long_axis = v1.getLongitude()
    time_axis = v1.getTime()
    if time_axis is None:
	   raise ValueError, "u has no time dimension" 
    nlat = len(lat_axis)
    nlong = len(long_axis)
    ntime = len(time_axis)
    nvar = nlat*nlong
    ax1 = v1.getAxisList(omit='time')
    if v2 is not None: 
        nvar = 2 * nvar
        ax2 = v2.getAxisList(omit='time')
    x = numpy.zeros((ntime, nvar), numpy.float)

    for ilat in range(nlat):
        udata = v1.getSlice(latitude=ilat)
        if udata.mask() is not None:
            raise ValueError, 'eof cannot operate on masked data'
        udata = udata.filled()
        x[:, ilat*nlong: (ilat+1)*nlong] = adjust(udata, ntime, mean_choice) * latweight[numpy.newaxis, ilat]
    
    del udata
    if v2 is not None:
        for ilat in range(nlat):
            udata = v1.getSlice(latitude=ilat)
            if udata.mask() is not None:
                raise ValueError, 'eof cannot operate on masked data'
            udata = udata.filled()
            x[:, nlat*nlong + ilat*nlong: nlat*nlong + (ilat+1)*nlong] = \
                adjust(udata[:, ilat, :], ntime, mean_choice) * latweight[numpy.newaxis, ilat]
        del udata
  
    a = Analyzer ()
    
    a.analyze (x, nr=nr)

# Compute weighted eigenvectors
    evs = a.evec
    pcs = numpy.matrix(x) * numpy.matrix(evs)
    number_of_components = len(a.eval)
    result = []
    for k in range(number_of_components):
        evs1 = numpy.reshape(evs[0:nlong*nlat, k], (nlat, nlong))
        evs1 = evs1 / latweight[:, numpy.newaxis]
        pc = cdms2.createVariable(evs1, copy=0, axes=ax1, id=v1.id+'_'+str(k+1),
                      attributes = v1.attributes)
        result.append(pc)
        if v2:
            evs1 = numpy.reshape(evs[nlong*nlat:2*nlong*nlat, k], (nlat, nlong))
            evs1 = evs1 / latweight[:, numpy.newaxis]
            pc = cdms2.createVariable(evs1, copy=0, axes=ax2, 
                      id=v2.id+'_'+str(k+1),
                      attributes = v2.attributes)
            result.append(pc)
    return a, result

    
def adjust(v, ntime, mean_choice):
    if mean_choice is None:
        return v
    elif mean_choice == 'mean':
        t1 = MA.filled(MA.average(v, 0))
        return v - t1[numpy.newaxis, :]
    else:
        interval = int(mean_choice)
        w = array(v)
        for j in range(ntime/interval):
            t1 =MA.filled(MA.average(v[j:ntime:interval,:], 0))
            w[j:ntime:interval,:] = v[j:ntime:interval] - t1
        return w
