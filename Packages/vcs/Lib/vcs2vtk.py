import vcs
import vtk
import numpy

## This module contains some convenience function from vcs2vtk

## Continents first
def continentsVCS2VTK(fnm):
  """ This converts vcs continents files to vtkpolydata
  Author: Charles Doutriaux
  Input: vcs continent file name
  """
  poly =vtk.vtkPolyData()
  cells = vtk.vtkCellArray()
  pts = vtk.vtkPoints()
  f=open(fnm)
  ln=f.readline()
  while ln.strip().split()!=["-99","-99"]:
    # Many lines, need to know number of points
    N = int(ln.split()[0])
    # Now create and store these points
    n=0
    npts = pts.GetNumberOfPoints()
    while n<N:
        ln=f.readline()
        while len(ln)>2:
          l,L=float(ln[:8]),float(ln[8:16])
          pts.InsertNextPoint(L,l,0.0001)
          ln=ln[16:]
          n+=2
    ln = vtk.vtkPolyLine()
    ln.GetPointIds().SetNumberOfIds(N/2)
    for i in range(N/2): ln.GetPointIds().SetId(i,i+npts)
    cells.InsertNextCell(ln)
    ln=f.readline()
  poly.SetPoints(pts)
  poly.SetLines(cells)
  return poly


#Geo projection
def project(pts,projection):
  if projection.type=="linear":
    return pts
  geo = vtk.vtkGeoTransform()
  ps = vtk.vtkGeoProjection()
  pd = vtk.vtkGeoProjection()
  projName = projection.type
  pd.SetName(projName)
  geo.SetSourceProjection(ps)
  geo.SetDestinationProjection(pd)
  geopts = vtk.vtkPoints()
  geo.TransformPoints(pts,geopts)
  return geopts


#Vtk dump
dumps=0
def dump2VTK(obj,fnm=None):
  global dumps
  if fnm is None:
    fnm="foo%.3i.vtk" % dumps
    dumps+=1
  dsw = vtk.vtkDataSetWriter()
  dsw.SetFileName(fnm)
  try:
    dsw.SetInputData(obj)
  except:
    dsw.SetInputConnection(obj.GetOutputPort())

  dsw.Write()


#Wrapping around
def doWrap(Mapper,Act,gm,wrap=[0.,360]):
  if wrap is None or numpy.allclose(wrap,0.):
    return
  A = vtk.vtkAssembly()
  A.AddPart(Act)
  mn=min(gm.datawc_x1,gm.datawc_x2)
  mx=max(gm.datawc_x1,gm.datawc_x2)
  print "XMIN XMAX:",mn,mx
  if numpy.allclose(mn,1.e20) or numpy.allclose(mx,1.e20):
    mx = abs(wrap[1])
    mn = -wrap[1]
  print "XMIN XMAX:",mn,mx
  if wrap[1]!=0.:
    i=0
    while A.GetXRange()[0]>mn:
      i+=1
      print A.GetXRange(),"left"
      act_left = vtk.vtkActor()
      act_left.SetMapper(Mapper)
      act_left.SetProperty(Act.GetProperty())
      T = vtk.vtkTransform()
      T.Translate(-i*wrap[1],0,0)
      act_left.SetUserTransform(T)
      A.AddPart(act_left)
    i=0
    while A.GetXRange()[1]<mx:
      i+=1
      print A.GetXRange(),"right"
      act_right = vtk.vtkActor()
      act_right.SetMapper(Mapper)
      T = vtk.vtkTransform()
      T.Translate(i*wrap[1],0,0)
      act_right.SetUserTransform(T)
      act_right.SetProperty(Act.GetProperty())
      A.AddPart(act_right)
  mn=min(gm.datawc_y1,gm.datawc_y2)
  mx=max(gm.datawc_y1,gm.datawc_y2)
  if numpy.allclose(mn,1.e20) or numpy.allclose(mx,1.e20):
    mx = abs(wrap[0])
    mn = -wrap[0]
  if wrap[0]!=0.:
    i=0
    while A.GetYrange()[0]>mn:
      i+=1
      act_up = vtk.vtkActor()
      act_up.SetMapper(Mapper)
      T = vtk.vtkTransform()
      T.Translate(0,i*wrap[0],0)
      act_up.SetUserTransform(T)
      act_up.SetProperty(Act.GetProperty())
      A.AddPart(act_up)
    i=0
    while A.GetYrange()[1]<mx:
      i+=1
      act_down = vtk.vtkActor()
      act_down.SetMapper(Mapper)
      T = vtk.vtkTransform()
      T.Translate(0,-i*wrap[0],0)
      act_down.SetUserTransform(T)
      act_down.SetProperty(Act.GetProperty())
      A.AddPart(act_down)
  return A

