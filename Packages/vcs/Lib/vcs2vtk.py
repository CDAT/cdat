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
def doWrap(Act,gm,wrap=[0.,360]):
  Mapper = Act.GetMapper()
  MB = vtk.vtkMultiBlockDataSet()
  mn=min(gm.datawc_x1,gm.datawc_x2)
  mx=max(gm.datawc_x1,gm.datawc_x2)
  MB.SetBlock(0,doClip(Mapper.GetInput(),mn,mx,0))
  Amn,Amx = Act.GetXRange()
  if numpy.allclose(mn,1.e20) or numpy.allclose(mx,1.e20):
    mx = abs(wrap[1])
    mn = -wrap[1]
  print "XMIN XMAX:",mn,mx
  if wrap[1]!=0.:
    i=0
    while Amn>mn:
      i+=1
      Amn-=wrap[1]
      print "left:",Amn
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(Act.GetMapper().GetInput())
      T=vtk.vtkTransform()
      T.Translate(-i*wrap[1],0,0)
      Tpf.SetTransform(T)
      Tpf.Update()
      out = doClip(Tpf.GetOutput(),mn,mx,0)
      MB.SetBlock(MB.GetNumberOfBlocks(),out)
    i=0
    while Amx<mx:
      i+=1
      Amx+=wrap[1]
      print Amx,"right"
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(Act.GetMapper().GetInput())
      T = vtk.vtkTransform()
      T.Translate(i*wrap[1],0,0)
      Tpf.SetTransform(T)
      Tpf.Update()
      MB.SetBlock(MB.GetNumberOfBlocks(),doClip(Tpf.GetOutput(),mn,mx,0))
  mn=min(gm.datawc_y1,gm.datawc_y2)
  mx=max(gm.datawc_y1,gm.datawc_y2)
  Amn,Amx = Act.GetYRange()
  if numpy.allclose(mn,1.e20) or numpy.allclose(mx,1.e20):
    mx = abs(wrap[0])
    mn = -wrap[0]
  if wrap[0]!=0.:
    i=0
    while Amn>mn:
      i+=1
      Amn-=wrap[0]
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(Act.GetMapper().GetInput())
      T = vtk.vtkTransform()
      T.Translate(0,i*wrap[0],0)
      Tpf.SetTransform(T)
      Tpf.Update()
      MB.SetBlock(MB.GetNumberOfBlocks(),doClip(Tpf.GetOutput()))
    i=0
    while Amx<mx:
      i+=1
      Amx+=wrap[0]
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(Act.GetMapper().GetInput())
      T = vtk.vtkTransform()
      T.Translate(0,-i*wrap[0],0)
      Tpf.SetTransform(T)
      Tpf.Update()
      MB.SetBlock(MB.GetNumberOfBlocks(),doClip(Tpf.GetOutput()))
  #data = doClip(MB)
  Actor = vtk.vtkActor()
  Actor.SetProperty(Act.GetProperty())
  Mapper2 = vtk.vtkDataSetMapper()
  Mapper2 = vtk.vtkCompositePolyDataMapper()
  Mapper2.SetInputDataObject(MB)
  Mapper2.SetLookupTable(Mapper.GetLookupTable())
  Mapper2.SetScalarRange(Mapper.GetScalarRange())
  Actor.SetMapper(Mapper2)
  #print "Returning :",Actor,MB.GetNumberOfBlocks()
  return Actor

def doClip(data,mn,mx,axis=0):
  minClip = doClip1(data,mn,1,0)
  minAndMaxClip = doClip1(minClip,mx,-1,0)
  return minAndMaxClip

def doClip1(data,value,normal,axis=0):
    # We have the actor, do clipping
    clpf = vtk.vtkPlane()
    print "Clipping:",value,normal,axis
    if axis == 0:
      clpf.SetOrigin(value,0,0)
      clpf.SetNormal(normal,0,0)
    clp = vtk.vtkClipPolyData()
    clp.SetClipFunction(clpf)
    clp.SetInputData(data)
    clp.Update()
    return clp.GetOutput()

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

