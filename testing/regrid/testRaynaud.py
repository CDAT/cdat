"""
Testing CDAT regridding from curvilinear grid to rectangular grid
"""
import unittest
import cdms2
import ESMP
from cdms2.mvCdmsRegrid import CdmsRegrid
import numpy
import pylab
from genutil import minmax
from time import time
import operator
import sys

PLOT = True

def bounds1d(xx):
    """Compute bounds on a linear sequence or axis. 
    It is based on :func:`cdms2.genGenericBounds` of CDAT.
    
    :Example:
    
        >>> bounds1d(xx).shape
        360, 2

    :Returns: xx(nx,2)
    """

    try:
        xx = xx.getValues().astype('d')
    except:
        xx = numpy.asarray(xx[:],'d')

    if len(xx)>1:
        leftPoint = numpy.array([1.5*xx[0]-0.5*xx[1]])
        midArray = (xx[0:-1]+xx[1:])/2.0
        rightPoint = numpy.array([1.5*xx[-1]-0.5*xx[-2]])
        bnds = numpy.concatenate((leftPoint,midArray,rightPoint))
    else:
        delta = width/2.0
        bnds = numpy.array([self[0]-delta,self[0]+delta])

    # Transform to (n,2) array
    retbnds = numpy.zeros((len(xx),2),'d')
    retbnds[:,0] = bnds[:-1]
    retbnds[:,1] = bnds[1:]

    return retbnds

def bounds2d(*xyaxes):
    """Compute bounds on a rectangular grid.

    :Params:
    
        - **xyaxes**: 2D arrays(ny,nx) (or 2x1D arrays)

    :Example:
    
        >>> xx_bounds,yy_bounds = bounds2d(xx,yy)

    :Returns: ``xx(ny,nx,4),...``
    """

    results = []
    half = numpy.array(0.5,'d')
    quart = numpy.array(0.25,'d')


    for xy in xyaxes:

        assert xy.ndim == 2, 'You must pass 2d arrays as argument'
        ny,nx = xy.shape

        bounds = numpy.zeros((ny,nx,4),'d')

        inside = quart * (xy[0:-1,0:-1] + xy[0:-1,1:] +
                          xy[1:  ,0:-1] + xy[1:  ,1:])
        left  = half * (xy[0:-1, 0] - half * (xy[0:-1, 1] - xy[0:-1, 0]) + \
                        xy[1:  , 0] - half * (xy[1:  , 1] - xy[1:  , 0]))
        right = half * (xy[0:-1,-1] + half * (xy[0:-1,-1] - xy[0:-1,-2]) + \
                        xy[1:  ,-1] + half * (xy[1:  ,-1] - xy[1:  ,-2]))
        bot   = half * (xy[ 0,0:-1] - half * (xy[ 1,0:-1] - xy[ 0,0:-1]) + \
                        xy[ 0,1:  ] - half * (xy[ 1,1:  ] - xy[ 0,1:  ]))
        top   = half * (xy[-1,0:-1] + half * (xy[-1,0:-1] - xy[-2,0:-1]) + \
                        xy[-1,1:  ] + half * (xy[-1,1:  ] - xy[-2,1:  ]))
        left_bot  = xy[ 0, 0] + half * (xy[ 0, 0] - xy[ 1, 1])
        right_bot = xy[ 0,-1] + half * (xy[ 0,-1] - xy[ 1,-2])
        right_top = xy[-1,-1] + half * (xy[-1,-1] - xy[-2,-2])
        left_top  = xy[-1, 0] + half * (xy[-1, 0] - xy[-2, 1])


        bounds[1:  ,1:  ,0] = inside
        bounds[1:  ,0:-1,1] = inside
        bounds[0:-1,0:-1,2] = inside
        bounds[0:-1,1:  ,3] = inside

        bounds[1:  , 0,0] = left
        bounds[0:-1, 0,3] = left
        bounds[1:  ,-1,1] = right
        bounds[0:-1,-1,2] = right
        bounds[ 0,1:  ,0] = bot
        bounds[ 0,0:-1,1] = bot
        bounds[-1,0:-1,2] = top
        bounds[-1,1:  ,3] = top

        bounds[ 0, 0,0] = left_bot
        bounds[ 0,-1,1] = right_bot
        bounds[-1,-1,2] = right_top
        bounds[-1, 0,3] = left_top

        results.append(bounds)

    if len(results) == 1:
        return results[0]
    else:
        return results

class Test(unittest.TestCase):

    def setUp(self):
            pass

    def test0(self):
        """
        Test from Stephane Raynaud with a few modifications
        """

        toolsAndMethods = {
            'libcf': ['linear'],
            'esmf': ['linear', 'patch', 'conserve'],
            }
    
        f = cdms2.open(sys.prefix + '/sample_data/swan.four.nc')
        vari = f('HS')
        f.close()
        gridi = vari.getGrid()

        # add bounds to input grid
        lati = vari.getLatitude()
        loni = vari.getLongitude()
        xib, yib = bounds2d(loni, lati)
        loni.setBounds(xib)
        lati.setBounds(yib)
        self.assertNotEqual(gridi.getLatitude().getBounds(), None)
        self.assertNotEqual(gridi.getLongitude().getBounds(), None)
        
        # output grid
        nyo, nxo = 100, 200
        ymin, ymax = lati.min(), lati.max()
        xmin, xmax = loni.min(), loni.max()
        dy, dx = (ymax - ymin)/float(nyo), (xmax - xmin)/float(nxo)
        yo = numpy.array([ymin + dy*(j + 0.5) for j in range(nyo)])
        xo = numpy.array([xmin + dx*(i + 0.5) for i in range(nxo)])
        lato = cdms2.createAxis(yo)
        lato.designateLatitude() ; lato.units = 'degrees_north'
        lono = cdms2.createAxis(xo)
        lono.designateLongitude() ; lono.units= 'degrees_east'
        grido = cdms2.createRectGrid(lato, lono)
        self.assertNotEqual(grido.getLatitude().getBounds(), None)
        self.assertNotEqual(grido.getLongitude().getBounds(), None)
        
        for tool in toolsAndMethods:
            for met in toolsAndMethods[tool]:
                t0 = time()
                print tool.upper(), met, ':' 
                diag = {}
                varo = vari.regrid(grido,
                                   regridMethod = met, regridTool = tool,
                                   coordSys = 'cart', nitermax = 10, 
                                   diag = diag)
                print 'diag = ', diag
                met2 = diag['regridMethod']
                tool2 = diag['regridTool']
                self.assertEqual(met, met2)
                self.assertEqual(tool2, tool)
                self.assertGreater(varo.min(), -0.01)
                dt = time() - t0
                print tool.upper(), met, ':', dt, 'seconds'

                if PLOT:
                    pylab.figure(figsize=(12, 6))
                    pylab.subplots_adjust(right=0.9)
                    pylab.subplot(121)
                    pylab.pcolor(loni[:], lati[:], vari[0].asma(), vmin = 0, vmax = 2.5)
                    pylab.axis([xmin, xmax, ymin, ymax])
                    pylab.colorbar()
                    pylab.title('Original')
                    pylab.subplot(122)
                    pylab.pcolor(lono[:], lato[:], varo[0].asma(), vmin = 0, vmax = 2.5)
                    pylab.axis([xmin, xmax, ymin, ymax])
                    pylab.title(tool.upper()+' / '+met.upper())
                    pylab.colorbar()#cax=pylab.axes([0.92, 0.3, 0.02, 0.6]))
                    pylab.savefig('testRaynaud.%(tool)s.%(met)s.png'%vars())

    def test2_dstGridHasNoBounds(self):
        """
        Test that regrid will switch to linear when given regridMethod = 'conserve'
        when the dst grid has no bounds
        """

        toolsAndMethods = {
            'libcf': ['linear',],
            'esmf': ['conserve', 'patch', 'linear'],
            }
    
        f = cdms2.open(sys.prefix + '/sample_data/swan.four.nc')
        vari = f('HS')
        gridi = vari.getGrid()
        lati = vari.getLatitude()
        loni = vari.getLongitude()
        xib, yib = bounds2d(loni, lati)
        loni.setBounds(xib)
        lati.setBounds(yib)

        # output grid is input grid (without bounds)
        grido = f('HS').getGrid()

        for tool in toolsAndMethods:
            for met in toolsAndMethods[tool]:
                t0 = time()
                print tool.upper(), met, ':' 
                diag = {'numDstPoints': None, 'numValid': None}
                # should refuse to do conservative
                varo = vari.regrid(grido, 
                                   regridMethod = met, regridTool = tool,
                                   coordSys = 'cart',
                                   nitermax = 10, tolpos = 0.01,
                                   diag = diag)
                # cannot do conservative because the destination grid has no
                # bounds
                self.assertNotEqual(diag['regridMethod'], 'conserve')

    def test3_srcGridHasNoBounds(self):
        """
        Test that regridding will automatically choose libcf when there are
        no bounds in the src grid
        """

        toolsAndMethods = {
            'libcf': ['linear',],
            'esmf': ['conserve', 'patch', 'linear'],
            }
    
        f = cdms2.open(sys.prefix + '/sample_data/swan.four.nc')
        vari = f('HS')
        gridi = vari.getGrid()
        lati = vari.getLatitude()
        loni = vari.getLongitude()
        f.close()

        nyo, nxo = 100, 200
        ymin, ymax = lati.min(), lati.max()
        xmin, xmax = loni.min(), loni.max()
        dy, dx = (ymax - ymin)/float(nyo), (xmax - xmin)/float(nxo)
        yo = numpy.array([ymin + dy*(j + 0.5) for j in range(nyo)])
        xo = numpy.array([xmin + dx*(i + 0.5) for i in range(nxo)])
        lato = cdms2.createAxis(yo)
        lato.designateLatitude() ; lato.units = 'degrees_north'
        lono = cdms2.createAxis(xo)
        lono.designateLongitude() ; lono.units= 'degrees_east'
        grido = cdms2.createRectGrid(lato, lono) 

        for tool in toolsAndMethods:
            for met in toolsAndMethods[tool]:
                t0 = time()
                print tool.upper(), met, ':' 
                diag = {'numDstPoints': None, 'numValid': None}
                # although the user may choose esmf, we expect to interpolate
                # using libcf as the source grid has no bounds in this case
                varo = vari.regrid(grido, 
                                   regridMethod = met, regridTool = tool,
                                   coordSys = 'cart',
                                   nitermax = 10, tolpos = 0.01,
                                   diag = diag)
                # make sure libcf was selected
                self.assertEqual(diag['regridTool'], 'libcf')
                self.assertEqual(diag['regridMethod'], 'linear')


if __name__ == '__main__':
    print ""
    #ESMP.ESMP_Initialize()
    #ESMP.ESMP_LogSet(True)
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)

