import cdms2, numpy
# this version is not careful
class InterpreterComponent (cdms.selectors.SelectorComponent):
    "A component that interpolates to a specific location"
    def __init__ (self, lat, lon):
        self.lat = lat
        self.lon = lon
         
    def specify (self, slab, axes, specifications, confined_by, aux):
        aux[id(self)] = {}
        for i in range(len(axes)):
            axis = axes[i]
            if axis.isLatitude():
                if self.lat < axis[0] or self.lat > axis[-1]:
                    raise RuntimeError, 'Chosen latitude not in axis.'
                for low in range(1, len(axis)-1):
                    if self.lat <= axis[low]:
                        break
                else:
                    raise RuntimeError, 'Chosen latitude not in axis.'
                ix = low
                aux[id(self)]['latitude'] = i
            elif axis.isLongitude():
                if self.lon < axis[0] or self.lon > axis[-1]:
                    raise RuntimeError, 'Chosen latitude not in axis.'
                for low in range(1, len(axis)-1):
                    if self.lon <= axis[low]:
                        break
                else:
                    raise RuntimeError, 'Chosen latitude not in axis.'
                ix = low
                aux[id(self)]['longitude'] = i
            else:
                continue
            
            if confined_by[i] is None:
                specifications[i] = slice(ix-1, ix+1)
                confined_by[i] = self
            else:
                del aux[id(self)]
                return 1
        return 0

    def post (self, fetched, slab, axes, specifications, confined_by, aux, axismap):
        # incorrect, but for illustration just assume last two axes are yx.
        info = aux[id(self)]
        ilat = info['latitude']
        ilon = info['longitude']
        lataxis = fetched.getAxis(axismap[ilat])
        lonaxis = fetched.getAxis(axismap[ilon])
        f1 = (self.lat - lataxis[0])/(lataxis[1] - lataxis[0])
        f2 = (self.lon - lonaxis[0])/(lonaxis[1] - lonaxis[0])
        print f1, f2
        f = numpy.ma.average(fetched, axis = -1, weights=[1-f2, f2])
        f = numpy.ma.average(f, axis = -1, weights=[1-f1, f1])
        return cdms2.createVariable(f, copy=0, 
               axes=fetched.getAxisList(omit=['latitude', 'longitude'])
               )

def at (lat, lon):
    return cdms2.selectors.Selector(InterpreterComponent(lat, lon))

if __name__ == "__main__":
    f=cdms.open('clt.nc')
    t = f('clt', at(32., 98.),time=slice(0,10))
    print t
    t0 = f('clt', latitude=(32.,32.,'cc'), longitude=(98.,98., 'cc'), 
                  time=slice(0,10), squeeze=1)
    print t0
    
