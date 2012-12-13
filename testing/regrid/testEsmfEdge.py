"""

Test the new EDGE stagger locations

$Id: $
"""

import unittest
import ESMP
import cdms2
import numpy
from mpi4py import MPI

class TestESMPEDGE(unittest.TestCase):
    def setUp(self):

        self.pe = MPI.COMM_WORLD.Get_rank()
        self.nprocs = MPI.COMM_WORLD.Get_size()

        nxe, nye = 4, 3
        nxs, nys = 1, 1
        nx,  ny  = 8, 6
        y = numpy.linspace(nys, nye, ny)
        x = numpy.linspace(nxs, nxe, nx)
        y1 = numpy.linspace(nys-.5, nye+.5, ny+1)
        x1 = numpy.linspace(nxs-.5, nxe+.5, nx+1)

        # v
        dEdge1 = []
        dEdge1.append(numpy.outer(y1, numpy.ones(nx)))
        dEdge1.append(numpy.outer(numpy.ones(ny+1), x))
        self.dEdge1 = dEdge1

        # u
        dEdge2 = []
        dEdge2.append(numpy.outer(y, numpy.ones(nx+1)))
        dEdge2.append(numpy.outer(numpy.ones(ny), x1))
        self.dEdge2 = dEdge2

        # Cells
        dstGrid = []
        dstGrid.append(numpy.outer(y, numpy.ones(nx)))
        dstGrid.append(numpy.outer(numpy.ones(ny), x))
        self.dstGrid = dstGrid

        #  Corners
        dstCr = []
        dstCr.append(numpy.outer(y1, numpy.ones(nx+1)))
        dstCr.append(numpy.outer(numpy.ones(ny+1), x1))
        self.dstCr = dstCr

        self.dataEdge2 = numpy.array(dEdge2[0] + 2, numpy.float32)
        self.dataEdge1 = numpy.array(dEdge1[0] + 5, numpy.float32)
        self.datadstGrid = numpy.array(dstGrid[0] + 2, numpy.float32)
        self.datadstCr = numpy.array(dstCr[0] + 2, numpy.float32)

    def test1_EDGE1_linear_cart_nopd_native(self):
        # Cells
        maxIndexdstGrid = numpy.array(self.datadstGrid.shape[::-1], numpy.int32)
        grid = ESMP.ESMP_GridCreateNoPeriDim(maxIndexdstGrid, 
                                           coordSys = ESMP.ESMP_COORDSYS_CART)
        ESMP.ESMP_GridAddCoord(grid, staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)

        cGrdXPtr = ESMP.ESMP_GridGetCoordPtr(grid, 0, 
                              ESMP.ESMP_STAGGERLOC_CENTER)
        cGrdYPtr = ESMP.ESMP_GridGetCoordPtr(grid, 1, 
                              ESMP.ESMP_STAGGERLOC_CENTER)

        # Destination u and v fields in the cell centers
        cuFld = ESMP.ESMP_FieldCreateGrid(grid, 'cell_U_Fld',
                                typekind = ESMP.ESMP_TYPEKIND_R4,
                                staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        cvFld = ESMP.ESMP_FieldCreateGrid(grid, 'cell_V_Fld',
                                typekind = ESMP.ESMP_TYPEKIND_R4,
                                staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        cuFldPtr = ESMP.ESMP_FieldGetPtr(cuFld)
        cvFldPtr = ESMP.ESMP_FieldGetPtr(cvFld)

        # u Grid
        ESMP.ESMP_GridAddCoord(grid, staggerloc = ESMP.ESMP_STAGGERLOC_EDGE1)
        uGrdXPtr = ESMP.ESMP_GridGetCoordPtr(grid, 0, 
                              ESMP.ESMP_STAGGERLOC_EDGE1)
        uGrdYPtr = ESMP.ESMP_GridGetCoordPtr(grid, 1, 
                              ESMP.ESMP_STAGGERLOC_EDGE1)
        uFld = ESMP.ESMP_FieldCreateGrid(grid, 'srcEdge1', 
                                typekind = ESMP.ESMP_TYPEKIND_R4,
                                staggerloc = ESMP.ESMP_STAGGERLOC_EDGE1)
        uFldPtr = ESMP.ESMP_FieldGetPtr(uFld)

        # v Grid
        ESMP.ESMP_GridAddCoord(grid, staggerloc = ESMP.ESMP_STAGGERLOC_EDGE2)
        vGrdXPtr = ESMP.ESMP_GridGetCoordPtr(grid, 0, 
                              ESMP.ESMP_STAGGERLOC_EDGE2)
        vGrdYPtr = ESMP.ESMP_GridGetCoordPtr(grid, 1, 
                              ESMP.ESMP_STAGGERLOC_EDGE2)
        vFld = ESMP.ESMP_FieldCreateGrid(grid, 'srcEdge2', 
                                typekind = ESMP.ESMP_TYPEKIND_R4,
                                staggerloc = ESMP.ESMP_STAGGERLOC_EDGE2)
        vFldPtr = ESMP.ESMP_FieldGetPtr(vFld)

        # Get the indices
        ccLo, ccHi = ESMP.ESMP_GridGetCoord(grid, ESMP.ESMP_STAGGERLOC_CENTER)
        e1Lo, e1Hi = ESMP.ESMP_GridGetCoord(grid, ESMP.ESMP_STAGGERLOC_EDGE1)
        e2Lo, e2Hi = ESMP.ESMP_GridGetCoord(grid, ESMP.ESMP_STAGGERLOC_EDGE2)

        # Set the data
        cGrdXPtr[:] = self.dstGrid[1][ccLo[1]:ccHi[1], ccLo[0]:ccHi[0]].flat
        cGrdYPtr[:] = self.dstGrid[0][ccLo[1]:ccHi[1], ccLo[0]:ccHi[0]].flat
        uGrdXPtr[:] = self.dEdge2[1][e1Lo[1]:e1Hi[1], e1Lo[0]:e1Hi[0]].flat
        uGrdYPtr[:] = self.dEdge2[0][e1Lo[1]:e1Hi[1], e1Lo[0]:e1Hi[0]].flat
        vGrdXPtr[:] = self.dEdge1[1][e2Lo[1]:e2Hi[1], e2Lo[0]:e2Hi[0]].flat
        vGrdYPtr[:] = self.dEdge1[0][e2Lo[1]:e2Hi[1], e2Lo[0]:e2Hi[0]].flat
        cuFldPtr[:] = self.datadstGrid[ccLo[1]:ccHi[1], ccLo[0]:ccHi[0]].flat
        cvFldPtr[:] = self.datadstGrid[ccLo[1]:ccHi[1], ccLo[0]:ccHi[0]].flat
        uFldPtr[:] = self.dataEdge2[e1Lo[1]:e1Hi[1], e1Lo[0]:e1Hi[0]].flat
        vFldPtr[:] = self.dataEdge1[e2Lo[1]:e2Hi[1], e2Lo[0]:e2Hi[0]].flat

        # Regrid
        regridObj1 = ESMP.ESMP_FieldRegridStore(uFld, cuFld,
                              srcMaskValues = None, dstMaskValues = None,
                              regridmethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                              unmappedaction = ESMP.ESMP_UNMAPPEDACTION_IGNORE,
                              srcFracField = None, dstFracField = None)
        ESMP.ESMP_FieldRegrid(uFld, cuFld, regridObj1)
        regridObj2 = ESMP.ESMP_FieldRegridStore(vFld, cvFld,
                              srcMaskValues = None, dstMaskValues = None,
                              regridmethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                              unmappedaction = ESMP.ESMP_UNMAPPEDACTION_IGNORE,
                              srcFracField = None, dstFracField = None)
        ESMP.ESMP_FieldRegrid(vFld, cvFld, regridObj2)

        
        if self.pe == 0:
            print cuFldPtr.shape, self.datadstGrid.shape
            cuFldArr = numpy.reshape(cuFldPtr, self.datadstGrid.shape)
            cvFldArr = numpy.reshape(cvFldPtr, self.datadstGrid.shape)
    
            print
            print "U's"
            print self.dataEdge2
            print cuFldArr
            print
            print self.dataEdge1
            print cvFldArr

if __name__ == "__main__":
    ESMP.ESMP_Initialize()
    ESMP.ESMP_LogSet(False)
    suite = unittest.TestLoader().loadTestsFromTestCase(TestESMPEDGE)
    unittest.TextTestRunner(verbosity = 2).run(suite)

