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

def doClip(Actor):
    # We have the actor, do clipping
    mapper2 = vtk.vtkDataSetMapper()
    clpf = vtk.vtkPlane()
    clpf.SetOrigin(0,0,0)
    clpf.SetNormal(1,0,0)
    clp = vtk.vtkClipPolyData()
    clp.SetClipFunction(clpf)
    clp.SetInputData(Actor.GetMapper().GetInput())
    clp.Update()
    mapper2.SetInputConnection(clp.GetOutputPort())
    return mapper2
#Wrapping around
def doWrap(Act,gm,wrap=[0.,360]):
  Mapper = Act.GetMapper()
  if wrap is None or numpy.allclose(wrap,0.):
    A = Act
  else:
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
        act_left.SetProperty(Act.GetProperty())
        Tpf = vtk.vtkTransformPolyDataFilter()
        Tpf.SetInputData(Act.GetMapper().GetInput())
        T=vtk.vtkTransform()
        T.Translate(-i*wrap[1],0,0)
        Tpf.SetTransform(T)
        Tpf.Update()
        Mapper2 = vtk.vtkDataSetMapper()
        Mapper2.SetInputData(Tpf.GetOutput())
        Mapper2.SetLookupTable(Mapper.GetLookupTable())
        Mapper2.SetScalarRange(Mapper.GetScalarRange())
        act_left.SetMapper(Mapper2)
        A.AddPart(act_left)
      i=0
      while A.GetXRange()[1]<mx:
        i+=1
        print A.GetXRange(),"right"
        act_right = vtk.vtkActor()
        Tpf = vtk.vtkTransformPolyDataFilter()
        Tpf.SetInputData(Act.GetMapper().GetInput())
        T = vtk.vtkTransform()
        T.Translate(i*wrap[1],0,0)
        Tpf.SetTransform(T)
        Tpf.Update()
        Mapper2 = vtk.vtkDataSetMapper()
        Mapper2.SetInputData(Tpf.GetOutput())
        Mapper2.SetLookupTable(Mapper.GetLookupTable())
        Mapper2.SetScalarRange(Mapper.GetScalarRange())
        act_right.SetMapper(Mapper2)
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
        act_up.SetMapper(doClip(Act))
        T = vtk.vtkTransform()
        T.Translate(0,i*wrap[0],0)
        act_up.SetUserTransform(T)
        act_up.SetProperty(Act.GetProperty())
        A.AddPart(act_up)
      i=0
      while A.GetYrange()[1]<mx:
        i+=1
        act_down = vtk.vtkActor()
        act_down.SetMapper(doClip(Act))
        T = vtk.vtkTransform()
        T.Translate(0,-i*wrap[0],0)
        act_down.SetUserTransform(T)
        act_down.SetProperty(Act.GetProperty())
        A.AddPart(act_down)
    return A

def genTextActor(string,x=.5,y=.5,to='default',tt='default',cmap='default'):
  t = vtk.vtkTextActor()
  p=t.GetTextProperty()
  p.SetInput(string)
  if isinstance(to,str):
    to = vcs.elements["textorientation"][to]
  if isinstance(tt,str):
    tt = vcs.elements["texttable"][tt]
  if isinstance(cmap,str):
    cmap = vcs.elements["colormap"][cmap]
  c=cmap.index[tt.color]
  p.SetColor([x/100. for x in c])

  if to.halign=="left":
    p.SetJustificationToLeft()
  elif to.halign=="right":
    p.SetJustificationToRight()
  elif to.halign=="center":
    p.SetJustificationToCentered()

  if to.valign=='top':
    p.SetVerticalJustificationToTop()
  elif to.valign=='half':
    p.SetVerticalJustificationToCentered()
  elif to.valign=='bottom':
    p.SetVerticalJustificationToBottom()
  elif to.valign=='cap':
    warnings.warn("VTK does not support 'cap' align, using 'top'")
    p.SetVerticalJustificationToTop()
  elif to.valign=='base':
    warnings.warn("VTK does not support 'base' align, using 'bottom'")
    p.SetVerticalJustificationToBottom()

  p.SetOrientation(-to.angle)
  p.SetFontFamily(vtk.VTK_FONT_FILE)
  p.SetFontFile(vcs.elements["font"][vcs.elements["fontNumber"][tt.font]])
  p.SetPosition(tt.x,tt.y)

