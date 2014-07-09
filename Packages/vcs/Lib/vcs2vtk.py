## This module contains some convenience function from vcs2vtk
import vcs
import vtk
import numpy
import sys
import json
import os
import meshfill
from vtk.util import numpy_support as VN
import cdms2

f = open(os.path.join(sys.prefix,"share","vcs","wmo_symbols.json"))
wmo = json.load(f)


def genUnstructuredGrid(data1,data2,gm):
  continents = False
  wrap = None
  m3 = None
  g = None
  try: #First try to see if we can get a mesh out of this
    g=data1.getGrid()
    if isinstance(g,cdms2.gengrid.AbstractGenericGrid): # Ok need unstrctured grid
      m=g.getMesh()
      xm = m[:,1].min()
      xM = m[:,1].max()
      ym = m[:,0].min()
      yM = m[:,0].max()
      N=m.shape[0]
      #For vtk we need to reorder things
      m2 = numpy.ascontiguousarray(numpy.transpose(m,(0,2,1)))
      m2.resize((m2.shape[0]*m2.shape[1],m2.shape[2]))
      m2=m2[...,::-1]
      # here we add dummy levels, might want to reconsider converting "trimData" to "reOrderData" and use actual levels?
      m3=numpy.concatenate((m2,numpy.zeros((m2.shape[0],1))),axis=1)
      continents = True
      wrap = [0.,360.]
  except Exception,err: # Ok no mesh on file, will do with lat/lon
    ## Could still be meshfill with mesh data
    if isinstance(gm,meshfill.Gfm) and data2 is not None:
      N = data2.shape[0]
      m2 = numpy.ascontiguousarray(numpy.transpose(data2,(0,2,1)))
      m2.resize((m2.shape[0]*m2.shape[1],m2.shape[2]))
      m2=m2[...,::-1]
      # here we add dummy levels, might want to reconsider converting "trimData" to "reOrderData" and use actual levels?
      m3=numpy.concatenate((m2,numpy.zeros((m2.shape[0],1))),axis=1)
      if gm.wrap[1]==360.:
        continents = True
      wrap = gm.wrap
  if m3 is not None:
    #Create unstructured grid points
    vg = vtk.vtkUnstructuredGrid()
    for i in range(N):
      lst = vtk.vtkIdList()
      for j in range(4):
        lst.InsertNextId(i*4+j)
      ## ??? TODO ??? when 3D use CUBE?
      vg.InsertNextCell(vtk.VTK_QUAD,lst)
  else:
    #Ok a simple structured grid is enough
    vg = vtk.vtkStructuredGrid()
    if g is not None:
      # Ok we have grid
      lat = g.getLatitude()
      lon = g.getLongitude()
      continents = True
      wrap = [0.,360.]
      if not isinstance(g,cdms2.hgrid.AbstractCurveGrid):
        lat = lat[:,numpy.newaxis]*numpy.ones(lon.shape)[numpy.newaxis,:]
        lon = lon[numpy.newaxis,:]*numpy.ones(lat.shape)[:,numpy.newaxis]
    else:
      data1=cdms2.asVariable(data1)
      lon=data1.getAxis(-1)[:]
      lat=data1.getAxis(-2)[:]
      lat = lat[:,numpy.newaxis]*numpy.ones(lon.shape)[numpy.newaxis,:]
      lon = lon[numpy.newaxis,:]*numpy.ones(lat.shape)[:,numpy.newaxis]
    vg.SetDimensions(lat.shape[1],lat.shape[0],1)
    lon = numpy.ma.ravel(lon)
    lat = numpy.ma.ravel(lat)
    sh = list(lat.shape)
    sh.append(1)
    lon = numpy.ma.reshape(lon,sh)
    lat = numpy.ma.reshape(lat,sh)
    z = numpy.zeros(lon.shape)
    m3 = numpy.concatenate((lon,lat),axis=1)
    m3 = numpy.concatenate((m3,z),axis=1)
    xm=lon.min()
    xM=lon.max()
    ym=lat.min()
    yM=lat.max()
  # First create the points/vertices (in vcs terms)
  deep = True
  pts = vtk.vtkPoints()
  ## Convert nupmy array to vtk ones
  ppV = VN.numpy_to_vtk(m3,deep=deep)
  pts.SetData(ppV)

  projection = vcs.elements["projection"][gm.projection]
  geopts = project(pts,projection)
  ## Sets the vertics into the grid
  vg.SetPoints(geopts)



  return vg,xm,xM,ym,yM,continents,wrap

def getRange(gm,xm,xM,ym,yM):
    # Also need to make sure it fills the whole space
    if not numpy.allclose([gm.datawc_x1,gm.datawc_x2],1.e20):
      x1,x2 = gm.datawc_x1,gm.datawc_x2
    else:
      x1,x2 = xm,xM
    if not numpy.allclose([gm.datawc_y1,gm.datawc_y2],1.e20):
      y1,y2 = gm.datawc_y1,gm.datawc_y2
    else:
      y1,y2 = ym,yM
    return x1,x2,y1,y2

## Continents first
def prepContinents(fnm):
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
        sp=ln.split()
        sn = len(sp)
        didIt = False
        if sn%2 == 0:
          try:
            spts = []
            for i in range(sn/2):
              l,L = float(sp[i*2]),float(sp[i*2+1])
              spts.append([l,L])
            for p in spts:
              pts.InsertNextPoint(p[1],p[0],0.0001)
            n+=sn
            didIt = True
          except:
            didIt = False
        if didIt is False: 
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
dumps={}
def dump2VTK(obj,fnm=None):
  global dumps
  if fnm[:-4].lower()!=".vtk":
    fnm+=".vtk"
  if fnm is None:
    fnm="foo.vtk" % dumps
  if fnm in dumps:
    dumps[fnm]+=1
    fnm=fnm[:-4]+"%.3i.vtk" % dumps[fnm]
  else:
    dumps[fnm]=0
  dsw = vtk.vtkDataSetWriter()
  dsw.SetFileName(fnm)
  try:
    dsw.SetInputData(obj)
  except:
    dsw.SetInputConnection(obj.GetOutputPort())

  dsw.Write()


#Wrapping around
def doWrap(Act,wc,wrap=[0.,360]):
  Mapper = Act.GetMapper()
  data = Mapper.GetInput()
  xmn=min(wc[0],wc[1])
  xmx=max(wc[0],wc[1])
  if numpy.allclose(xmn,1.e20) or numpy.allclose(xmx,1.e20):
    xmx = abs(wrap[1])
    xmn = -wrap[1]
  ymn=min(wc[2],wc[3])
  ymx=max(wc[2],wc[3])
  if numpy.allclose(ymn,1.e20) or numpy.allclose(ymx,1.e20):
    ymx = abs(wrap[0])
    ymn = -wrap[0]
  
  ## Prepare MultiBlock and puts in oriinal data
  appendFilter =vtk.vtkAppendPolyData()
  appendFilter.AddInputData(data)
  ## X axis wrappping
  Amn,Amx = Act.GetXRange()
  if wrap[1]!=0.:
    i=0
    while Amn>xmn:
      i+=1
      Amn-=wrap[1]
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(data)
      T=vtk.vtkTransform()
      T.Translate(-i*wrap[1],0,0)
      Tpf.SetTransform(T)
      Tpf.Update()
      appendFilter.AddInputData(Tpf.GetOutput())
    i=0
    while Amx<xmx:
      i+=1
      Amx+=wrap[1]
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(data)
      T = vtk.vtkTransform()
      T.Translate(i*wrap[1],0,0)
      Tpf.SetTransform(T)
      Tpf.Update()
      appendFilter.AddInputData(Tpf.GetOutput())

  # Y axis wrapping
  Amn,Amx = Act.GetYRange()
  if wrap[0]!=0.:
    i=0
    while Amn>ymn:
      i+=1
      Amn-=wrap[0]
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(data)
      T = vtk.vtkTransform()
      T.Translate(0,i*wrap[0],0)
      Tpf.SetTransform(T)
      Tpf.Update()
      appendFilter.AddInputData(Tpf.GetOutput())
    i=0
    while Amx<ymx:
      i+=1
      Amx+=wrap[0]
      Tpf = vtk.vtkTransformPolyDataFilter()
      Tpf.SetInputData(data)
      T = vtk.vtkTransform()
      T.Translate(0,-i*wrap[0],0)
      Tpf.SetTransform(T)
      Tpf.Update()
      appendFilter.AddInputData(Tpf.GetOutput())
  appendFilter.Update()
  Actor = vtk.vtkActor()
  Actor.SetProperty(Act.GetProperty())
  #Mapper2 = vtk.vtkDataSetMapper()
  #Mapper2 = vtk.vtkCompositePolyDataMapper()
  Mapper2 = vtk.vtkPolyDataMapper()
  Mapper2.SetInputData(doClip(appendFilter.GetOutput(),xmn,xmx,ymn,ymx))
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

def prepTextProperty(p,to="default",tt="default",cmap=None):
  if isinstance(to,str):
    to = vcs.elements["textorientation"][to]
  if isinstance(tt,str):
    tt = vcs.elements["texttable"][tt]
  
  if cmap is None:
    if tt.colormap is not None:
      cmap = tt.colormap
    else:
      cmap = 'default'
  if isinstance(cmap,str):
    cmap = vcs.elements["colormap"][cmap]
  c=cmap.index[tt.color]
  p.SetColor([C/100. for C in c])
  if to.halign in [0, 'left']:
    p.SetJustificationToLeft()
  elif to.halign in [2, 'right']:
    p.SetJustificationToRight()
  elif to.halign in [1,'center']:
    p.SetJustificationToCentered()

  if to.valign in [0,'top']:
    p.SetVerticalJustificationToTop()
  elif to.valign in [2, 'half']:
    p.SetVerticalJustificationToCentered()
  elif to.valign in [4,'bottom']:
    p.SetVerticalJustificationToBottom()
  elif to.valign in [1,'cap']:
    warnings.warn("VTK does not support 'cap' align, using 'top'")
    p.SetVerticalJustificationToTop()
  elif to.valign in [3,'base']:
    warnings.warn("VTK does not support 'base' align, using 'bottom'")
    p.SetVerticalJustificationToBottom()
  p.SetOrientation(-to.angle)
  p.SetFontFamily(vtk.VTK_FONT_FILE)
  p.SetFontFile(vcs.elements["font"][vcs.elements["fontNumber"][tt.font]])

def genTextActor(renderer,string=None,x=None,y=None,to='default',tt='default',cmap=None):
  if isinstance(to,str):
    to = vcs.elements["textorientation"][to]
  if isinstance(tt,str):
    tt = vcs.elements["texttable"][tt]
  if tt.priority==0:
    return
  if string is None:
    string = tt.string
  if x is None:
    x = tt.x
  if y is None:
    y = tt.y
  if x is None or y is None or string in [['',],[]]:
    return
  
  n = max(len(x),len(y),len(string))
  for a in [x,y,string]:
    while len(a)<n:
      a.append(a[-1])

  for i in range(n):
    t = vtk.vtkTextActor()
    p=t.GetTextProperty()
    prepTextProperty(p,to,tt,cmap)
    t.SetInput(string[i])
    X,Y = world2Renderer(renderer,x[i],y[i],tt.viewport,tt.worldcoordinate)
    t.SetPosition(X,Y)
    renderer.AddActor(t)
  return 

def prepPrimitive(prim):
  if prim.x is None or prim.y is None:
    return 0
  if not isinstance(prim.x[0],(list,tuple)):
    prim.x = [prim.x,]
  if not isinstance(prim.y[0],(list,tuple)):
    prim.y = [prim.y,]
  if vcs.isfillarea(prim):
    atts = ["x","y","color","style","index"]
  elif vcs.ismarker(prim):
    atts = ["x","y","color","size","type"]
  elif vcs.isline(prim):
    atts = ["x","y","color","width","type"]
  n=0
  for a in atts:
    n = max(n,len(getattr(prim,a)))
  for a in atts:
    v = getattr(prim,a)
    while len(v)<n:
      v.append(v[-1])
    setattr(prim,a,v)
  return n

def prepFillarea(renWin,ren,farea,cmap=None):
  n = prepPrimitive(farea)
  if n==0:
    return
  for i in range(n):
    x   = farea.x[i]
    y   = farea.y[i]
    c   = farea.color[i]
    st  = farea.style[i]
    idx = farea.index[i]
    N = max(len(x),len(y))
    for a in [x,y]:
      while len(a)<n:
        a.append(a[-1])
    #Create points
    pts = vtk.vtkPoints()
    for j in range(N):
      pts.InsertNextPoint(x[j],y[j],0.)
    #Create polygon out of these points
    polygon = vtk.vtkPolygon()
    pid = polygon.GetPointIds()
    pid.SetNumberOfIds(N)
    for j in range(N):
      pid.SetId(j,j)
    polygons = vtk.vtkCellArray()
    polygons.InsertNextCell(polygon)

    polygonPolyData = vtk.vtkPolyData()
    polygonPolyData.SetPoints(pts)
    polygonPolyData.SetPolys(polygons)

    a = vtk.vtkActor()
    m = vtk.vtkPolyDataMapper()
    m.SetInputData(polygonPolyData)
    a.SetMapper(m)
    p = a.GetProperty()
   
    if cmap is None:
      if farea.colormap is not None:
        cmap = farea.colormap
      else:
        cmap = 'default'
    if isinstance(cmap,str):
      cmap = vcs.elements["colormap"][cmap]
    color = cmap.index[c]
    p.SetColor([C/100. for C in color])
    ren.AddActor(a)
    fitToViewport(a,ren,farea.viewport,farea.worldcoordinate)
  return 

def genPoly(coords,pts,filled=True):
  N = pts.GetNumberOfPoints()
  if filled:
    poly = vtk.vtkPolygon()
  else:
    poly = vtk.vtkPolyLine()
  pid = poly.GetPointIds()
  n = len(coords)
  pid.SetNumberOfIds(n)
  for j in range(n):
    c = list(coords[j])
    if len(c)==2:
      c.append(0)
    pts.InsertNextPoint(*c)
    pid.SetId(j,j+N)
  return poly

def prepMarker(renWin,ren,marker,cmap=None):
  n=prepPrimitive(marker)
  if n==0:
    return
  for i in range(n):
    ## Creates the glyph
    g = vtk.vtkGlyph2D()
    markers = vtk.vtkPolyData()
    x = marker.x[i]
    y=marker.y[i]
    c=marker.color[i]
    s=marker.size[i]/float(max(marker.worldcoordinate))*10.
    t=marker.type[i]
    N = max(len(x),len(y))
    for a in [x,y]:
      while len(a)<n:
        a.append(a[-1])
    pts = vtk.vtkPoints()
    for j in range(N):
      pts.InsertNextPoint(x[j],y[j],0.)
    markers.SetPoints(pts)

    #  Type
    ## Ok at this point generates the source for glpyh
    gs = vtk.vtkGlyphSource2D()
    pd = None
    if t=='dot':
      gs.SetGlyphTypeToCircle()
      gs.FilledOn()
    elif t=='circle':
      gs.SetGlyphTypeToCircle()
      gs.FilledOff()
    elif t=='plus':
      gs.SetGlyphTypeToCross()
      gs.FilledOff()
    elif t=='cross':
      gs.SetGlyphTypeToCross()
      gs.SetRotationAngle(45)
      gs.FilledOff()
    elif t[:6]=='square':
      gs.SetGlyphTypeToSquare()
      gs.FilledOff()
    elif t[:7]=='diamond':
      gs.SetGlyphTypeToDiamond()
      gs.FilledOff()
    elif t[:8]=='triangle':
      gs.SetGlyphTypeToTriangle()
      if t[9]=="d":
        gs.SetRotationAngle(180)
      elif t[9]=="l":
        gs.SetRotationAngle(90)
      elif t[9]=="r":
        gs.SetRotationAngle(-90)
      elif t[9]=="u":
        gs.SetRotationAngle(0)
    elif t == "hurricane":
      s =s/100.
      ds = vtk.vtkDiskSource()
      ds.SetInnerRadius(.55*s)
      ds.SetOuterRadius(1.01*s)
      ds.SetCircumferentialResolution(90)
      ds.SetRadialResolution(30)
      gf = vtk.vtkGeometryFilter()
      gf.SetInputConnection(ds.GetOutputPort())
      gf.Update()
      pd1 = gf.GetOutput()
      apd = vtk.vtkAppendPolyData()
      apd.AddInputData(pd1)
      pts = vtk.vtkPoints()
      pd = vtk.vtkPolyData()
      polygons = vtk.vtkCellArray()
      add_angle = numpy.pi/360.
      coords = []
      angle1 = .6*numpy.pi
      angle2 = .88*numpy.pi
      while angle1<=angle2:
        coords.append([s*2+2*s*numpy.cos(angle1),2*s*numpy.sin(angle1)])
        angle1+=add_angle
      angle1=.79*numpy.pi
      angle2=.6*numpy.pi
      while angle1>=angle2:
        coords.append([s*2.25+s*4*numpy.cos(angle1),-s*2+s*4*numpy.sin(angle1)])
        angle1-=add_angle
      poly = genPoly(coords,pts,filled=True)
      polygons.InsertNextCell(poly)
      coords=[]
      angle1 = 1.6*numpy.pi
      angle2 = 1.9*numpy.pi
      while angle1 <= angle2:
        coords.append( [- s*2 + s*2*numpy.cos(angle1),s*2*numpy.sin(angle1)])
        angle1 += add_angle
      angle1 = 1.8*numpy.pi
      angle2 = 1.6*numpy.pi
      while angle1 >= angle2:
        coords.append( [- s*2.27 + s*4*numpy.cos(angle1), s*2 + s*4*numpy.sin(angle1)])
        angle1 -= add_angle
      poly = genPoly(coords,pts,filled=True)
      polygons.InsertNextCell(poly)
      pd.SetPoints(pts)
      pd.SetPolys(polygons)
      apd.AddInputData(pd)
      apd.Update()
      g.SetSourceData(apd.GetOutput())
    elif t in ["w%.2i" % x for x in range(203)]:
      ## WMO marker
      params = wmo[t]
      pts = vtk.vtkPoints()
      pd = vtk.vtkPolyData()
      polys = vtk.vtkCellArray()
      lines = vtk.vtkCellArray()
      #Lines first
      for l in params["line"]:
        coords = numpy.array(zip(*l))*s/30.
        line = genPoly(coords.tolist(),pts,filled=False)
        lines.InsertNextCell(line)
      for l in params["poly"]:
        coords = numpy.array(zip(*l))*s/30.
        line = genPoly(coords.tolist(),pts,filled=True)
        polys.InsertNextCell(line)
      pd.SetPoints(pts)
      pd.SetPolys(polys)
      pd.SetLines(lines)
      g.SetSourceData(pd)
    else:
      warnings.warn("unknown marker type: %s, using dot" % t)
      gs.SetGlyphTypeToCircle()
      gs.FilledOn()
    if t[-5:]=="_fill":
      gs.FilledOn()
    gs.SetScale(s)
    gs.Update()


    if pd is None:
      g.SetSourceConnection(gs.GetOutputPort())
    g.SetInputData(markers)

    a = vtk.vtkActor()
    m = vtk.vtkPolyDataMapper()
    m.SetInputConnection(g.GetOutputPort())
    m.Update()
    a.SetMapper(m)
    p = a.GetProperty()
    #Color
    if cmap is None:
      if marker.colormap is not None:
        cmap = marker.colormap
      else:
        cmap = 'default'
    if isinstance(cmap,str):
      cmap = vcs.elements["colormap"][cmap]
    color = cmap.index[c]
    p.SetColor([C/100. for C in color])
    ren.AddActor(a)
    fitToViewport(a,ren,marker.viewport,marker.worldcoordinate)
  return 

def prepLine(renWin,ren,line,cmap=None):
  n = prepPrimitive(line)
  if n==0:
    return
  for i in range(n):
    l = vtk.vtkLine()
    lines = vtk.vtkCellArray()
    x = line.x[i]
    y=line.y[i]
    c=line.color[i]
    w=line.width[i]
    t=line.type[i]
    N = max(len(x),len(y))
    for a in [x,y]:
      while len(a)<n:
        a.append(a[-1])
    pts = vtk.vtkPoints()
    for j in range(N):
      pts.InsertNextPoint(x[j],y[j],0.)
    for j in range(N-1):
      l.GetPointIds().SetId(0,j)
      l.GetPointIds().SetId(1,j+1)
      lines.InsertNextCell(l)
    linesPoly = vtk.vtkPolyData()
    linesPoly.SetPoints(pts)
    linesPoly.SetLines(lines)
    a = vtk.vtkActor()
    m = vtk.vtkPolyDataMapper()
    m.SetInputData(linesPoly)
    a.SetMapper(m)
    p = a.GetProperty()
    p.SetLineWidth(w)
   
    if cmap is None:
      if line.colormap is not None:
        cmap = line.colormap
      else:
        cmap = 'default'
    if isinstance(cmap,str):
      cmap = vcs.elements["colormap"][cmap]
    color = cmap.index[c]
    p.SetColor([C/100. for C in color])
    # stipple
    if t == 'long-dash':
      p.SetLineStipplePattern(int('1111111100000000',2))
      p.SetLineStippleRepeatFactor(1)
    elif t == 'dot':
      p.SetLineStipplePattern(int('1010101010101010',2))
      p.SetLineStippleRepeatFactor(1)
    elif t == 'dash':
      p.SetLineStipplePattern(int('1111000011110000',2))
      p.SetLineStippleRepeatFactor(1)
    elif t == 'dash-dot':
      p.SetLineStipplePattern(int('0011110000110011',2))
      p.SetLineStippleRepeatFactor(1)
    elif t == 'solid':
      p.SetLineStipplePattern(int('1111111111111111',2))
      p.SetLineStippleRepeatFactor(1)
    else:
      raise Exception,"Unkonw line type: '%s'" % t
    ren.AddActor(a)
    fitToViewport(a,ren,line.viewport,line.worldcoordinate)
  return 

def getRendererCorners(Renderer,vp=[0.,1.,0.,1.]):
  sz = Renderer.GetSize()
  origin = Renderer.GetOrigin()
  opposite = origin[0]+sz[0]*vp[1],origin[1]+sz[1]*vp[3]
  origin2 = origin[0]+sz[0]*vp[0],origin[1]+sz[1]*vp[2]
  return origin2,opposite

def world2Renderer(ren,x,y,vp=[0.,1.,0.,1.],wc=[0.,1.,0.,1.]):
  origin,opposite = getRendererCorners(ren,vp)
  X = origin[0]+ (opposite[0]-origin[0] )*(x-wc[0])/(wc[1]-wc[0])
  Y = origin[1]+ (opposite[1]-origin[1] )*(y-wc[2])/(wc[3]-wc[2])
  return X,Y

def R2World(ren,x,y):
  """Converts renderer's x/y to WorldCoordinate for a given Renderer"""
  #print "ok X and Y:",x,y
  ren.SetDisplayPoint(x,y,0)
  ren.DisplayToWorld()
  ren.ViewToWorld()
  wp = ren.GetWorldPoint()
  return wp

def vtkWorld2Renderer(ren,x,y):
  ren.SetWorldPoint(x,y,0,0)
  ren.WorldToDisplay()
  renpts = ren.GetDisplayPoint()
  return renpts

def fitToViewport(Actor,Renderer,vp,wc=None):
  return fitToViewportNew(Actor,Renderer,vp,wc)

def fitToViewportNew(Actor,Renderer,vp,wc=None):
  ## Data range in World Coordinates
  if wc is None:
    Xrg = Actor.GetXRange()
    Yrg = Actor.GetYRange()
  else:
    Xrg=float(wc[0]),float(wc[1])
    Yrg=float(wc[2]),float(wc[3])

  Renderer.SetViewport(vp[0],vp[2],vp[1],vp[3])
  rw = Renderer.GetRenderWindow()
  sc = rw.GetSize()
  wRatio = float(sc[0])/float(sc[1])
  dRatio = (Xrg[1]-Xrg[0])/(Yrg[1]-Yrg[0])
  vRatio = float(vp[1]-vp[0])/float(vp[3]-vp[2])


  if wRatio>1.: #landscape orientated window
      yScale = 1.
      xScale = vRatio*wRatio/dRatio
  else:
      xScale = 1.
      yScale = dRatio/(vRatio*wRatio)


  T = vtk.vtkTransform()
  T.Scale(xScale,yScale,1.)

  Actor.SetUserTransform(T)


  xc = xScale*float(Xrg[1]+Xrg[0])/2.
  yc = yScale*float(Yrg[1]+Yrg[0])/2.
  xd = xScale*float(Xrg[1]-Xrg[0])/2.
  yd = yScale*float(Yrg[1]-Yrg[0])/2.
  cam = Renderer.GetActiveCamera()
  cam.ParallelProjectionOn()
  cam.SetParallelScale(yd)
  cd = cam.GetDistance()
  cam.SetPosition(xc,yc,cd)
  cam.SetFocalPoint(xc,yc,0.)


def fitToViewportSlow(Actor,Renderer,vp,wc=None):
  ## Data range in World Coordinates
  if wc is None:
    Xrg = Actor.GetXRange()
    Yrg = Actor.GetYRange()
  else:
    Xrg=float(wc[0]),float(wc[1])
    Yrg=float(wc[2]),float(wc[3])
  
  #print "VIEWPORT:",vp
  #print "XrgYrg:",Xrg,Yrg
  ## Where they are in term of pixels
  oll = vtkWorld2Renderer(Renderer,Xrg[0],Yrg[0])
  our = vtkWorld2Renderer(Renderer,Xrg[1],Yrg[1])
  
  #print "oll,our:",oll,our
  # Where they should be in term of pixel
  ll = world2Renderer(Renderer,Xrg[0],Yrg[0],
      vp,
      [Xrg[0],Xrg[1],Yrg[0],Yrg[1]])
  ur = world2Renderer(Renderer,Xrg[1],Yrg[1],
      vp,
      [Xrg[0],Xrg[1],Yrg[0],Yrg[1]])
  
  #print "ll,ur:",ll,ur
  # How much does it needs to be scaled by?
  xScale = (ur[0]-ll[0])/(our[0]-oll[0])
  yScale = (ur[1]-ll[1])/(our[1]-oll[1])
  #print xScale,yScale

  #World coordinates of where they need to be
  LL = R2World(Renderer,*ll)
  #print "LL:",LL

  # Move it to the correct bottom left corner
  dX = LL[0]-Xrg[0]
  dY = LL[1]-Yrg[0]
  #print "dX,dY:",dX,dY


  # transformation are applied in reverse order
  T = vtk.vtkTransform()
  ## After scaling move it back to lower left corner
  T.Translate(LL[0]*(1.-xScale),LL[1]*(1.-yScale),0)
  ## scale
  T.Scale(xScale,yScale,1)
  # Move it to the correct bottom left corner
  T.Translate(dX,dY,0)

  Actor.SetUserTransform(T)
  return Actor

