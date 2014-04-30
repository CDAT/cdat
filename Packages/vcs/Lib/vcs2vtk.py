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
  xmn=min(gm.datawc_x1,gm.datawc_x2)
  xmx=max(gm.datawc_x1,gm.datawc_x2)
  if numpy.allclose(xmn,1.e20) or numpy.allclose(xmx,1.e20):
    xmx = abs(wrap[1])
    xmn = -wrap[1]
  ymn=min(gm.datawc_y1,gm.datawc_y2)
  ymx=max(gm.datawc_y1,gm.datawc_y2)
  if numpy.allclose(ymn,1.e20) or numpy.allclose(ymx,1.e20):
    ymx = abs(wrap[0])
    ymn = -wrap[0]
  
  ## Prepare MultiBlock and puts in oriinal data
  MB = vtk.vtkMultiBlockDataSet()
  MB.SetBlock(0,doClip(Mapper.GetInput(),xmn,xmx,ymn,ymx))
  ## X axis wrappping
  Amn,Amx = Act.GetXRange()
  if wrap[1]!=0.:
    i=0
    while Amn>xmn:
      i+=1
      Amn-=wrap[1]
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(Act.GetMapper().GetInput())
      T=vtk.vtkTransform()
      T.Translate(-i*wrap[1],0,0)
      Tpf.SetTransform(T)
      Tpf.Update()
      out = doClip(Tpf.GetOutput(),xmn,xmx,ymn,ymx)
      MB.SetBlock(MB.GetNumberOfBlocks(),out)
    i=0
    while Amx<xmx:
      i+=1
      Amx+=wrap[1]
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(Act.GetMapper().GetInput())
      T = vtk.vtkTransform()
      T.Translate(i*wrap[1],0,0)
      Tpf.SetTransform(T)
      Tpf.Update()
      MB.SetBlock(MB.GetNumberOfBlocks(),doClip(Tpf.GetOutput(),xmn,xmx,ymn,ymx))

  # Y axis wrapping
  Amn,Amx = Act.GetYRange()
  if wrap[0]!=0.:
    i=0
    while Amn>ymn:
      i+=1
      Amn-=wrap[0]
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(Act.GetMapper().GetInput())
      T = vtk.vtkTransform()
      T.Translate(0,i*wrap[0],0)
      Tpf.SetTransform(T)
      Tpf.Update()
      MB.SetBlock(MB.GetNumberOfBlocks(),doClip(Tpf.GetOutput(),xmn,xmx,ymn,ymx))
    i=0
    while Amx<ymx:
      i+=1
      Amx+=wrap[0]
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(Act.GetMapper().GetInput())
      T = vtk.vtkTransform()
      T.Translate(0,-i*wrap[0],0)
      Tpf.SetTransform(T)
      Tpf.Update()
      MB.SetBlock(MB.GetNumberOfBlocks(),doClip(Tpf.GetOutput(),xmn,xmx,ymn,ymx))
  Actor = vtk.vtkActor()
  Actor.SetProperty(Act.GetProperty())
  Mapper2 = vtk.vtkDataSetMapper()
  Mapper2 = vtk.vtkCompositePolyDataMapper()
  Mapper2.SetInputDataObject(MB)
  Mapper2.SetLookupTable(Mapper.GetLookupTable())
  Mapper2.SetScalarRange(Mapper.GetScalarRange())
  Actor.SetMapper(Mapper2)
  return Actor

def doClip(data,xmin,xmax,ymin,ymax):
  if xmin!=xmax:
    xminClip = doClip1(data,xmin,1,0)
    xfullClip = doClip1(xminClip,xmax,-1,0)
  else:
    xfullClip = data
  if ymin!=ymax:
    yminClip  = doClip1(xfullClip,ymin,1,1)
    xyClip  = doClip1(yminClip,ymax,-1,1)
  else:
    xyClip = xfullClip
  return xyClip

def doClip1(data,value,normal,axis=0):
    # We have the actor, do clipping
    clpf = vtk.vtkPlane()
    if axis == 0:
      clpf.SetOrigin(value,0,0)
      clpf.SetNormal(normal,0,0)
    else:
      clpf.SetOrigin(0,value,0)
      clpf.SetNormal(0,normal,0)
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

