#!/usr/bin/env python
#
# $Id: ESMP_GridToMeshRegridCsrv.py,v 1.5 2012/04/23 23:00:14 rokuingh Exp $
#===============================================================================
#             ESMP/examples/ESMP_GridToMeshRegrid.py
#===============================================================================

"""
ESMP_GridToMeshRegridCsrv.py

Two ESMP_Field objects are created, one on a Grid and the other on a Mesh.  The 
source Field is set to an analytic function, and a conservative regridding 
operation is performed from the source to the destination Field.  After 
the regridding is completed, the destination Field is compared to the 
exact solution over that domain.
"""

import cdms2
import ESMP
import numpy as _NP
import unittest

def grid_create():
  '''
  PRECONDITIONS: ESMP has been initialized.
  POSTCONDITIONS: A ESMP_Grid has been created.
  '''
  ub_x = float(4)
  ub_y = float(4)

  lb_x = float(0)
  lb_y = float(0)
  
  max_x = float(4)
  max_y = float(4)

  min_x = float(0)
  min_y = float(0)

  cellwidth_x = (max_x-min_x)/(ub_x-lb_x)
  cellwidth_y = (max_y-min_y)/(ub_y-lb_y)
  
  cellcenter_x = cellwidth_x/2
  cellcenter_y = cellwidth_y/2
  
  maxIndex = _NP.array([ub_x,ub_y], dtype=_NP.int32)

  grid = ESMP.ESMP_GridCreateNoPeriDim(maxIndex,
                                       coordSys=ESMP.ESMP_COORDSYS_CART)
  
  ##   CORNERS
  ESMP.ESMP_GridAddCoord(grid, staggerloc=ESMP.ESMP_STAGGERLOC_CORNER)

  exLB_corner, exUB_corner = ESMP.ESMP_GridGetCoord(grid, \
                                       ESMP.ESMP_STAGGERLOC_CORNER)

  # get the coordinate pointers and set the coordinates
  [x,y] = [0, 1]
  gridXCorner = ESMP.ESMP_GridGetCoordPtr(grid, x, ESMP.ESMP_STAGGERLOC_CORNER)
  gridYCorner = ESMP.ESMP_GridGetCoordPtr(grid, y, ESMP.ESMP_STAGGERLOC_CORNER)
  
  #print 'lower corner bounds = [{0},{1}]'.format(exLB_corner[0],exLB_corner[1])
  #print 'upper corner bounds = [{0},{1}]'.format(exUB_corner[0],exUB_corner[1])

  p = 0
  for i1 in range(exLB_corner[1], exUB_corner[1]):
    for i0 in range(exLB_corner[0], exUB_corner[0]):
      gridXCorner[p] = float(i0)*cellwidth_x
      gridYCorner[p] = float(i1)*cellwidth_y
      p = p + 1
 
  #print 'Grid corner coordinates:'
  p = 0
  for i1 in range(exLB_corner[1], exUB_corner[1]):
    for i0 in range(exLB_corner[0], exUB_corner[0]):
      #print '[{0},{1}]'.format(gridXCorner[p], gridYCorner[p])
      p = p + 1
  #print '\n'

  ##   CENTERS
  ESMP.ESMP_GridAddCoord(grid, staggerloc=ESMP.ESMP_STAGGERLOC_CENTER)

  exLB_center, exUB_center = ESMP.ESMP_GridGetCoord(grid, \
                                       ESMP.ESMP_STAGGERLOC_CENTER)

  # get the coordinate pointers and set the coordinates
  [x,y] = [0, 1]
  gridXCenter = ESMP.ESMP_GridGetCoordPtr(grid, x, ESMP.ESMP_STAGGERLOC_CENTER)
  gridYCenter = ESMP.ESMP_GridGetCoordPtr(grid, y, ESMP.ESMP_STAGGERLOC_CENTER)
  
  #print 'lower corner bounds = [{0},{1}]'.format(exLB_center[0],exLB_center[1])
  #print 'upper corner bounds = [{0},{1}]'.format(exUB_center[0],exUB_center[1])

  p = 0
  for i1 in range(exLB_center[1], exUB_center[1]):
    for i0 in range(exLB_center[0], exUB_center[0]):
      gridXCenter[p] = float(i0)*cellwidth_x + cellwidth_x/2.0
      gridYCenter[p] = float(i1)*cellwidth_y + cellwidth_y/2.0
      p = p + 1
  
  #print 'Grid center coordinates:'
  p = 0
  for i1 in range(exLB_center[1], exUB_center[1]):
    for i0 in range(exLB_center[0], exUB_center[0]):
      #print '[{0},{1}]'.format(gridXCenter[p], gridYCenter[p])
      p = p + 1
  #print '\n'

  return grid

def mesh_create_3x3(mesh):
  '''
  PRECONDITIONS: An ESMP_Mesh has been declared.
  POSTCONDITIONS: A 3x3 ESMP_Mesh has been created.
  
                 3x3 Mesh
   
  
   3.0  2.0   13 -------14 --------15--------16
              |         |          |         |
              |    7    |    8     |   9     |
              |         |          |         |
   2.5  1.5   9 ------- 10 --------11--------12
              |         |          |         |
              |    4    |    5     |   6     |
              |         |          |         |
   1.5  0.5   5 ------- 6 -------- 7-------- 8
              |         |          |         |
              |    1    |    2     |   3     |
              |         |          |         |
   1.0  0.0   1 ------- 2 -------- 3-------- 4
       
             0.0       0.5        1.5       2.0
             1.0       1.5        2.5       3.0
    
        Node Ids at corners
        Element Ids in centers
  
        (Everything owned by PET 0) 
  '''
  # set up a simple mesh
  num_node = 16
  num_elem = 9
  nodeId = _NP.array([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])
  '''
  # this is for grid to mesh
  nodeCoord = _NP.array([1.0,1.0, 1.5,1.0, 2.5,1.0, 3.0,1.0,
                          1.0,1.5, 1.5,1.5, 2.5,1.5, 3.0,1.5,
                          1.0,2.5, 1.5,2.5, 2.5,2.5, 3.0,2.5,
                          1.0,3.0, 1.5,3.0, 2.5,3.0, 3.0,3.0])
  '''
  # this is for mesh to grid
  nodeCoord = _NP.array([0.0,0.0, 1.5,0.0, 2.5,0.0, 4.0,0.0,
                          0.0,1.5, 1.5,1.5, 2.5,1.5, 4.0,1.5,
                          0.0,2.5, 1.5,2.5, 2.5,2.5, 4.0,2.5,
                          0.0,4.0, 1.5,4.0, 2.5,4.0, 4.0,4.0])
  
  nodeOwner = _NP.zeros(num_node, dtype=_NP.int32)
  elemId = _NP.array([1,2,3,4,5,6,7,8,9], dtype=_NP.int32)
  elemType = _NP.ones(num_elem, dtype=_NP.int32)
  elemType*=ESMP.ESMP_MESHELEMTYPE_QUAD
  elemConn = _NP.array([0,1,5,4,
                        1,2,6,5,
                        2,3,7,6,
                        4,5,9,8,
                        5,6,10,9,
                        6,7,11,10,
                        8,9,13,12,
                        9,10,14,13,
                        10,11,15,14], dtype=_NP.int32)

  ESMP.ESMP_MeshAddNodes(mesh,num_node,nodeId,nodeCoord,nodeOwner)
  
  ESMP.ESMP_MeshAddElements(mesh,num_elem,elemId,elemType,elemConn)
  
  #print 'Mesh coordinates:'
  for i in range(num_node):
    x = nodeCoord[2*i]
    y = nodeCoord[2*i+1]
    #print '[{0},{1}]'.format(x, y)
  #print '\n'
 
  return mesh, nodeCoord, elemType, elemConn

def create_ESMPmesh_3x3():  
  '''
  PRECONDITIONS: ESMP is initialized.
  POSTCONDITIONS: An ESMP_Mesh (3x3) has been created and returned as 'mesh'.
  '''
  # Two parametric dimensions, and three spatial dimensions
  mesh = ESMP.ESMP_MeshCreate(2,2)
  
  mesh, nodeCoord, elemType, elemConn = mesh_create_3x3(mesh)
  
  return mesh, nodeCoord, elemType, elemConn

def create_ESMPfieldgrid(grid, name):
  '''
  PRECONDITIONS: An ESMP_Grid has been created, and 'name' is a string that 
                 will be used to initialize the name of a new ESMP_Field.
  POSTCONDITIONS: An ESMP_Field has been created.
  '''
  # defaults to center staggerloc
  field = ESMP.ESMP_FieldCreateGrid(grid, name)

  return field

def build_analyticfieldgrid(field, grid):
  '''
  PRECONDITIONS: An ESMP_Field has been created.
  POSTCONDITIONS: The 'field' has been initialized to an analytic field.
  '''
  # get the field pointer first
  fieldPtr = ESMP.ESMP_FieldGetPtr(field) 
   
  # get the grid bounds and coordinate pointers
  exLB, exUB = ESMP.ESMP_GridGetCoord(grid, ESMP.ESMP_STAGGERLOC_CENTER)

  # get the coordinate pointers and set the coordinates
  [x,y] = [0, 1]
  gridXCoord = ESMP.ESMP_GridGetCoordPtr(grid, x, ESMP.ESMP_STAGGERLOC_CENTER)
  gridYCoord = ESMP.ESMP_GridGetCoordPtr(grid, y, ESMP.ESMP_STAGGERLOC_CENTER)

  #print "Grid center coordinates"
  p = 0 
  for i1 in range(exLB[1], exUB[1]): 
    for i0 in range(exLB[0], exUB[0]): 
      xc = gridXCoord[p]
      yc = gridYCoord[p]
      fieldPtr[p] = 20.0+xc+yc
      #fieldPtr[p] = 20.0+xc*yc+yc**2
      #print '[{0},{1}] = {2}'.format(xc,yc,fieldPtr[p]) 
      p = p + 1 
  #print "\n"

  return field

def create_ESMPfield(mesh, name):
  '''
  PRECONDITIONS: An ESMP_Mesh has been created, and 'name' is a string that 
                 will be used to initialize the name of a new ESMP_Field.
  POSTCONDITIONS: An ESMP_Field has been created.
  '''
  field = ESMP.ESMP_FieldCreate(mesh, name, meshloc=ESMP.ESMP_MESHLOC_ELEMENT)

  return field

def build_analyticfield(field, nodeCoord, elemType, elemConn):
  '''
  PRECONDITIONS: An ESMP_Field has been created.
  POSTCONDITIONS: The 'field' has been initialized to an analytic field.
  '''
  # get the field pointer first
  fieldPtr = ESMP.ESMP_FieldGetPtr(field, 0) 
  
  # set the field to a vanilla initial field for now
  #print "Mesh center coordinates"
  offset = 0
  for i in range(field.size):  # this routine assumes this field is on elements
    if (elemType[i] == ESMP.ESMP_MESHELEMTYPE_TRI):
      raise NameError("Cannot compute a non-constant analytic field for a mesh\
                       with triangular elements!")
    x1 = nodeCoord[(elemConn[offset])*2]
    x2 = nodeCoord[(elemConn[offset+1])*2]
    y1 = nodeCoord[(elemConn[offset+1])*2+1]
    y2 = nodeCoord[(elemConn[offset+3])*2+1]
    x = (x1+x2)/2.0
    y = (y1+y2)/2.0
    fieldPtr[i] = 20.0+x+y
    #fieldPtr[i] = 20.0+x*y+y**2
    #print '[{0},{1}] = {2}'.format(x,y,fieldPtr[i]) 
    offset = offset + 4
  #print "\n"
 
  return field

def run_regridding(srcfield, dstfield):
  '''
  PRECONDITIONS: Two ESMP_Fields have been created and a regridding operation 
                 is desired from 'srcfield' to 'dstfield'.
  POSTCONDITIONS: An ESMP regridding operation has set the data on 'dstfield'.
  '''
  # call the regridding functions
  routehandle = ESMP.ESMP_FieldRegridStore(srcfield, dstfield, 
                                      regridmethod=ESMP.ESMP_REGRIDMETHOD_CONSERVE, 
                                      unmappedaction=ESMP.ESMP_UNMAPPEDACTION_ERROR)
  ESMP.ESMP_FieldRegrid(srcfield, dstfield, routehandle)
  ESMP.ESMP_FieldRegridRelease(routehandle)
  
  return dstfield

def compare_fields(field1, field2):
  '''
  PRECONDITIONS: Two ESMP_Fields have been created and a comparison of the 
                 the values is desired between 'srcfield' and 'dstfield'.
  POSTCONDITIONS: The values on 'srcfield' and 'dstfield' are compared.

  returns True if the fileds are comparable (success)
  '''
  
  # get the data pointers for the fields
  field1ptr = ESMP.ESMP_FieldGetPtr(field1)
  field2ptr = ESMP.ESMP_FieldGetPtr(field2)
  
  # compare point values of field1 to field2
  # first verify they are the same size
  if (field1.size != field2.size): 
    raise NameError('compare_fields: Fields must be the same size!')
  
  # initialize to True, and check for False point values
  correct = True
  totalErr = 0.0
  for i in range(field1.size):
    err = abs(field1ptr[i] - field2ptr[i])/abs(field2ptr[i])
    if err > .06:
      correct = False
      print "ACCURACY ERROR - "+str(err)
      print "field1 = {0} : field2 = {1}\n".format(field1ptr[i], field2ptr[i])
    totalErr += err
  
  if correct:
    print " - PASS - Total Error = "+str(totalErr)
    return True
  else:
    print " - FAIL - Total Error = "+str(totalErr)
    return False

class TestESMP_GridToMeshRegridCsrv(unittest.TestCase):

  def setUp(self):
    pass

  def test_test1(self):
    # create two unique ESMP_Mesh objects
    grid = grid_create()
    mesh, nodeCoord, elemType, elemConn = create_ESMPmesh_3x3()
    '''
    # this is for grid to mesh
    # create ESMP_Field objects on the Meshes
    srcfield = create_ESMPfieldgrid(grid, 'srcfield')    
    dstfield = create_ESMPfield(mesh, 'dstfield')
    dstfield2 = create_ESMPfield(mesh, 'dstfield_exact')

    # initialize the Fields to an analytic function
    srcfield = build_analyticfieldgrid(srcfield, grid)
    dstfield2 = build_analyticfield(dstfield2, nodeCoord, elemType, elemConn)
    '''
    # this is for mesh to grid
    # create ESMP_Field objects on the Meshes
    srcfield = create_ESMPfield(mesh, 'srcfield')    
    dstfield = create_ESMPfieldgrid(grid, 'dstfield')
    dstfield2 = create_ESMPfieldgrid(grid, 'dstfield_exact')

    # initialize the Fields to an analytic function
    srcfield = build_analyticfield(srcfield, nodeCoord, elemType, elemConn)
    dstfield2 = build_analyticfieldgrid(dstfield2, grid)

    # run the ESMF regridding
    dstfield = run_regridding(srcfield, dstfield)

    # compare results and output PASS or FAIL
    ok = compare_fields(dstfield, dstfield2)

    # clean up
    ESMP.ESMP_FieldDestroy(srcfield)
    ESMP.ESMP_FieldDestroy(dstfield)
    ESMP.ESMP_FieldDestroy(dstfield2)
    ESMP.ESMP_GridDestroy(grid)
    ESMP.ESMP_MeshDestroy(mesh)

    self.assertEqual(ok, True)

if __name__ == '__main__':

    ESMP.ESMP_LogSet(True)
    print "" # Spacer
    suite = unittest.TestLoader().loadTestsFromTestCase(TestESMP_GridToMeshRegridCsrv)
    unittest.TextTestRunner(verbosity = 1).run(suite)

