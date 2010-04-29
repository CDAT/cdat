# Adapted for numpy/ma/cdms2 by convertcdms.py
import string, os, types
"Defines class Eof for EOF analysis."
import cdms2
from analyzer import Analyzer
import numpy

class Eof:
    """Eof(v1=None, v2=None, 
           number_of_components=4, 
           latweight_choice='default', 
           mean_choice='default'
           )
       Create Eof analyzer; if v1 and optionally v2 are given,
       calls analyze(v1, v2, number_of_components) as well.

       Calling method 'analyze' sets attributes:
           percent_explained
           eigenvectors
           principal_components (a list)

        Choices for mean correction:
         'none' -> use data as-is
         'default' -> subtract the time-mean of the data.
        
        Choices of latitude weights:
         'none' -> use data as-is
         'area' -> use latitude weights directly
         'default' -> use sqrt(latitude)
    """
    def __init__ (self, v1=None, v2=None, number_of_components=4, 
                  latweight_choice='default',
                  mean_choice='default'):
        self.set_latweight_choice(latweight_choice)
        self.set_mean_choice(mean_choice)
        self.set_number_of_components(number_of_components)
        self.percent_explained = None
        self.eigenvectors = None
        if v1 is not None:
            self.analyze(v1, v2)

    def latweight_choice (self):
        "The current latitude weighting choice."
        return self._latweight_choice

    def set_latweight_choice (self, choice):
        """Set choice of latitude weights:
         'none' -> use data as-is
         'area' -> use latitude weights directly
         'default' -> use sqrt(latitude)
        """
        if type(choice) is not types.StringType:
            raise ValueError, "choice must be a string."
        if choice == 'none' or choice == 'area' or choice == 'default':
            self._latweight_choice = choice
        else:
            raise ValueError, \
            "weighting choice must be 'none', 'area', or 'default'."

    def mean_choice (self):
        "The current value of the mean correction choice."
        return self._mean_choice
        
    def set_mean_choice (self, choice):
        """Set choice for mean correction:
         'none' -> use data as-is
         'default' -> subtract the time-mean of the data.
        """
        if type(choice) is types.StringType:
            if choice == 'none' or choice == 'default':
                self._mean_choice = choice
            else:
                raise ValueError, \
                "mean choice must be 'none', 'default', or a positive integer."
        elif isinstance(choice, types.IntType):
            if choice > 0:
                self._mean_choice = choice
            else:
                raise ValueError, \
                "mean choice must be 'none', 'default', or a positive integer."
        else:
            raise ValueError, \
            "mean choice must be 'none', 'default', or a positive integer."

    def number_of_components (self):
        "The number of components that will be calculated."
        return self._number_of_components

    def set_number_of_components (self, n):
        """set_number_of_components(n), n > 0"""
        if n <=0:
            raise ValueError, "Number of components must be positive."
        self._number_of_components = n

    def analyze (self, v1, v2=None):
        """analyze(v1, v2=None) 
           Calculates EOF principal components. 
           Sets the following attributes: 
              'principal_components' 
              'eigenvectors'
              'percent_explained' 
        """
        g = v1.getGrid()
        if g is None:
            raise ValueError, "u does not have spatial dimensions."
        latw, longw = g.getWeights()
        latw = latw / numpy.maximum.reduce(latw)
        if self.latweight_choice() == 'none':
            latweight = 1.0 + 0.0 * latw
        elif self.latweight_choice() == 'area':
            latweight = latw 
        else:
            latweight = numpy.sqrt(latw)
        mean_choice = self.mean_choice()
        nr = self.number_of_components()
    
        lat_axis = v1.getLatitude()
        long_axis = v1.getLongitude()
        time_axis = v1.getTime()
        if time_axis is None:
            raise ValueError, "v1 has no time dimension" 
        nlat = len(lat_axis)
        nlong = len(long_axis)
        ntime = len(time_axis)
        nvar = nlat*nlong
        ax1 = v1(order='...x').getAxisList(omit='time')
        
        if v2 is not None: 
            time_axis_2 = v2.getTime()
            if time_axis_2 is None:
                raise ValueError, 'v2 has no time dimension'
            if not numpy.allclose(time_axis, time_axis_2):
                raise ValueError, 'v1 and v2 have incompatible time axes'
            nvar = 2 * nvar
            ax2 = v2(order='...x').getAxisList(omit='time')
        x = numpy.zeros((ntime, nvar), numpy.float)
    
        for ilat in range(nlat):
            udata = v1.getSlice(latitude=ilat, 
                                required='time',
                                order='t...x', 
                                raw=1)
            if udata.mask is not numpy.ma.nomask:
                raise ValueError, 'eof cannot operate on masked data'
            if numpy.ma.rank(udata) != 2:
                raise ValueError, 'eof cannot handle extra dimension'
            udata = udata.filled()
            x[:, ilat*nlong: (ilat+1)*nlong] = \
                self.__adjust(udata, ntime, mean_choice) * \
                              latweight[numpy.newaxis, ilat]
        
        del udata
        if v2 is not None:
            for ilat in range(nlat):
                udata = v1.getSlice(latitude=ilat, 
                                    required='time',
                                    order='t...x', 
                                    raw=1)
                if udata.mask is not numpy.ma.nomask:
                    raise ValueError, 'eof cannot operate on masked data'
                if numpy.ma.rank(udata) != 2:
                    raise ValueError, 'eof cannot handle extra dimension'
                udata = udata.filled()
                x[:, nlat*nlong + ilat*nlong: nlat*nlong + (ilat+1)*nlong] = \
                    self.__adjust(udata[:, ilat, :], ntime, mean_choice) * \
                              latweight[numpy.newaxis, ilat]
            del udata
      
        a = Analyzer ()
        
        a.analyze (x, nr = nr)
    
    # Compute weighted eigenvectors
        evs = a.evec
        pcs = numpy.matrix(x)*numpy.matrix(evs)
        number_of_components = len(a.eval)
        result = []
        for k in range(number_of_components):
            evs1 = numpy.reshape(evs[0:nlong*nlat, k], (nlat, nlong))
            evs1 = evs1 / latweight[:, numpy.newaxis]
            pc = cdms2.createVariable(evs1, copy=0, axes=ax1, 
                          id=v1.id+'_'+str(k+1),
                          attributes = v1.attributes)
            result.append(pc)
            if v2:
                evs1 = numpy.reshape(evs[nlong*nlat:2*nlong*nlat, k], 
                                       (nlat, nlong))
                evs1 = evs1 / latweight[:, numpy.newaxis]
                pc = cdms2.createVariable(evs1, copy=0, axes=ax2, 
                          id=v2.id+'_'+str(k+1),
                          attributes = v2.attributes)
                result.append(pc)

        self.principal_components = result
        self.percent_explained = a.pct
        self.eigenvectors = a.evec
        
    def __adjust(self, v, ntime, mean_choice):
        if mean_choice == 'none':
            return v
        elif mean_choice == 'default':
            t1 = numpy.add.reduce(v, 0)/ntime
            return v - t1[numpy.newaxis, :]
        else:
            interval = mean_choice
            if ntime % interval <> 0:
                raise ValueError, "mean_choice must evenly divide # times."
            nv = ntime / interval
            w = numpy.array(v, numpy.float)
            for j in range(ntime/interval):
                t1 = numpy.add.reduce(v[j:ntime:interval,:])/nv
                w[j:ntime:interval,:] = v[j:ntime:interval] - t1
            return w
