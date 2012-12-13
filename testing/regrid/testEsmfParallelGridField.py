"""

$Id: testEsmfParallelGridField.py 2394 2012-07-26 17:23:11Z dkindig $

Test the order of grid and field creation relative to GridAddCoord such that all grids and
fields are decomposed.
"""


import ESMP
import numpy
from mpi4py import MPI

rootPe = 0
pe = MPI.COMM_WORLD.Get_rank()

COORDSYS = ESMP.ESMP_COORDSYS_CART
CENTER = ESMP.ESMP_STAGGERLOC_CENTER
CORNER = ESMP.ESMP_STAGGERLOC_CORNER
R4 = ESMP.ESMP_TYPEKIND_R4

ESMP.ESMP_Initialize()
ESMP.ESMP_LogSet(True)

def makeGrid2D(ny, nx):

    x = numpy.linspace(1, 4, nx, numpy.float32)
    y = numpy.linspace(0, 3, ny, numpy.float32)

    xx = numpy.outer(numpy.ones(ny), x)
    yy = numpy.outer(y ,numpy.ones(nx))
    
    xd, yd = abs(x[0]-x[1])/2., abs(y[0]-y[1])/2.
    xs, xe = x[0]-xd, x[-1]+xd
    ys, ye = y[0]-yd, y[-1]+yd
    xb = numpy.linspace(xs, xe, nx+1, numpy.float32)
    yb = numpy.linspace(ys, ye, ny+1, numpy.float32)

    xxb = numpy.outer(numpy.ones(ny+1), xb)
    yyb = numpy.outer(yb, numpy.ones(nx+1))

    return (yy,xx), (xxb,yyb)
def makeGrid3D(nx,ny,nz):
        dims = (nz, ny, nx)
        dimb = (nz+1, ny+1,nx+1)
        xbot, xtop, ybot, ytop, zbot, ztop = 1,4,1,5,.5,6
        xbob, xtob, ybob, ytob, zbob, ztob = .5,4.5,.5,5.5,0,6.5
        x = numpy.linspace(xbot, xtop, nx)
        y = numpy.linspace(ybot, ytop, ny)
        z = numpy.linspace(zbot, ztop, nz)

        xb = numpy.linspace(xbob, xtob, nx+1)
        yb = numpy.linspace(ybob, ytob, ny+1)
        zb = numpy.linspace(zbob, ztob, nz+1)
        
        xx = numpy.outer(numpy.ones(ny), x)
        yy = numpy.outer(y, numpy.ones(nx))
        ones = numpy.outer(numpy.ones(ny), numpy.ones(nx))
        xxx = numpy.outer(numpy.ones(nz), xx).reshape(dims)
        yyy = numpy.outer(numpy.ones(nz), yy).reshape(dims)
        zzz = numpy.outer(z, ones).reshape(dims)

        xxb = numpy.outer(numpy.ones(ny+1), xb)
        yyb = numpy.outer(yb, numpy.ones(nx+1))
        ones = numpy.outer(numpy.ones(ny+1), numpy.ones(nx+1))
        xxxb = numpy.outer(numpy.ones(nz+1), xxb).reshape(dimb)
        yyyb = numpy.outer(numpy.ones(nz+1), yyb).reshape(dimb)
        zzzb = numpy.outer(zb, ones).reshape(dimb)

        theVolume = [xxx, yyy, zzz]
        theBounds = [xxxb, yyyb, zzzb]

        theData = xxx * yyy + zzz

        return dims, theVolume, theData, theBounds

class createGridAndField:
  def __init__(self, maxIndex):
    

      self.grid = ESMP.ESMP_GridCreateNoPeriDim(maxIndex, coordSys = COORDSYS)
      ESMP.ESMP_GridAddCoord(self.grid, staggerloc = CENTER)

      self.field = ESMP.ESMP_FieldCreateGrid(self.grid, 'srcFeld1', 
                                        staggerloc = CENTER,
                                        typekind = R4)

  def getCoordPointer(self):
      return ESMP.ESMP_GridGetCoordPtr(self.grid, 0, staggerloc = CENTER)

  def getDataPointer(self):
      return ESMP.ESMP_FieldGetPtr(self.field)

#srcCrds, srcBnds = makeGrid2D(12, 24)
#dstCrds, dstBnds = makeGrid2D(12, 24)
sDims, srcCrds, srcData, srcBnds = makeGrid3D(6, 12, 24)
dDims, dstCrds, dstData, dstBnds = makeGrid3D(6, 12, 24)

maxIndex = numpy.array(srcCrds[0].shape[::-1], numpy.int32)

srcGrid1 = ESMP.ESMP_GridCreateNoPeriDim(maxIndex, coordSys = COORDSYS)
dstGrid1 = ESMP.ESMP_GridCreateNoPeriDim(maxIndex, coordSys = COORDSYS)

srcFeld1 = ESMP.ESMP_FieldCreateGrid(srcGrid1, 'srcFeld1', staggerloc = CENTER,
                                     typekind = R4)
dstFeld1 = ESMP.ESMP_FieldCreateGrid(dstGrid1, 'dstFeld', staggerloc = CENTER,
                                     typekind = R4)

srcPtr = ESMP.ESMP_FieldGetPtr(srcFeld1)
if pe == rootPe: print '1. Create Grid, Field then GridAddCoord()'
if pe == rootPe: print '2. GridCreate, GridAddCoord, FieldCreate'
if pe == rootPe: print '3. Use a class to create the grid addCoords then the field'
print '1. ', srcPtr.size, srcCrds[0].size
 
ESMP.ESMP_GridAddCoord(srcGrid1, staggerloc = CENTER)

srcPtr = ESMP.ESMP_FieldGetPtr(srcFeld1)
print '1. ', srcPtr.size, srcCrds[0].size

srcGrid2 = ESMP.ESMP_GridCreateNoPeriDim(maxIndex, coordSys = COORDSYS)
dstGrid2 = ESMP.ESMP_GridCreateNoPeriDim(maxIndex, coordSys = COORDSYS)

ESMP.ESMP_GridAddCoord(srcGrid2, staggerloc = CENTER)
srcFeld2 = ESMP.ESMP_FieldCreateGrid(srcGrid2, 'srcFeld2', staggerloc = CENTER,
                                    typekind = R4)
dstFeld2 = ESMP.ESMP_FieldCreateGrid(dstGrid2, 'dstFeld', staggerloc = CENTER,
                                    typekind = R4)

srcPtr = ESMP.ESMP_FieldGetPtr(srcFeld2)
print '2. ', srcPtr.size, srcCrds[0].size

srcStuff = createGridAndField(maxIndex)

aa = srcStuff.getDataPointer()
bb = srcStuff.getCoordPointer()
print '3. ', aa.size, bb.size

ESMP.ESMP_Finalize()
