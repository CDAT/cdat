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
def doWrap(Act,gm,wrap=[0.,360]):
  Mapper = Act.GetMapper()
  data = Mapper.GetInput()
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
  if to.halign==0:
    p.SetJustificationToLeft()
  elif to.halign ==2:
    p.SetJustificationToRight()
  elif to.halign==1:
    p.SetJustificationToCentered()

  print to.valign
  if to.valign==0:
    p.SetVerticalJustificationToTop()
  elif to.valign==2:
    p.SetVerticalJustificationToCentered()
  elif to.valign==4:
    p.SetVerticalJustificationToBottom()
  elif to.valign==1:
    warnings.warn("VTK does not support 'cap' align, using 'top'")
    p.SetVerticalJustificationToTop()
  elif to.valign==3:
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
  return t

def fillareaVCS2VTK(renWin,ren,farea,cmap=None):
  if farea.x is None or farea.y is None:
    return
  if not isinstance(farea.x[0],(list,tuple)):
    farea.x = [farea.x,]
  if not isinstance(farea.y[0],(list,tuple)):
    farea.y = [farea.y,]
  n = max(len(farea.style),len(farea.x),len(farea.y),len(farea.color),len(farea.index))
  for a in ["x","y","color","style","index"]:
    v = getattr(farea,a)
    while len(v)<n:
      v.append(v[-1])
    setattr(farea,a,v)

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
    if i==0:
      ren.AddActor(a)
      renWin.Render()
    b = fitToViewport(a,ren,farea.viewport,farea.worldcoordinate)
    if i==0:
      ren.RemoveActor(a)
    ren.AddActor(b)
  return 

def markerVCS2VTK(renWin,ren,marker,cmap=None):
  if marker.x is None or marker.y is None:
    return
  if not isinstance(marker.x[0],(list,tuple)):
    marker.x = [marker.x,]
  if not isinstance(marker.y[0],(list,tuple)):
    marker.y = [marker.y,]
  n = max(len(marker.type),len(marker.x),len(marker.y),len(marker.color),len(marker.size))
  for a in ["x","y","color","size","type"]:
    v = getattr(marker,a)
    while len(v)<n:
      v.append(v[-1])
    setattr(marker,a,v)
  for i in range(n):
    print "I:",i,n
    ## Creates the glyph
    g = vtk.vtkGlyph2D()
    markers = vtk.vtkPolyData()
    x = marker.x[i]
    y=marker.y[i]
    c=marker.color[i]
    s=marker.size[i]
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
    ### TODO Need to add custom glyphs from vcs ones
    gs = vtk.vtkGlyphSource2D()
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
    else:
      warnings.warn("unknown marker type: %s, using dot" % t)
      gs.SetGlyphTypeToCircle()
      gs.FilledOn()
    if t[-5:]=="_fill":
      gs.FilledOn()
    gs.SetScale(s/100.)


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
    if i==0:
      ren.AddActor(a)
      renWin.Render()
    b = fitToViewport(a,ren,marker.viewport,marker.worldcoordinate)
    if i==0:
      ren.RemoveActor(a)
    ren.AddActor(b)
  return 

def lineVCS2VTK(renWin,ren,line,cmap=None):
  if line.x is None or line.y is None:
    return
  if not isinstance(line.x[0],(list,tuple)):
    line.x = [line.x,]
  if not isinstance(line.y[0],(list,tuple)):
    line.y = [line.y,]
  n = max(len(line.type),len(line.x),len(line.y),len(line.color),len(line.width))
  for a in ["x","y","color","width","type"]:
    v = getattr(line,a)
    while len(v)<n:
      v.append(v[-1])
    setattr(line,a,v)
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
    dump2VTK(linesPoly,"linesPoly")
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
    if i==0:
      ren.AddActor(a)
      renWin.Render()
    b = fitToViewport(a,ren,line.viewport,line.worldcoordinate)
    if i==0:
      ren.RemoveActor(a)
    ren.AddActor(b)
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
  ## Data range in World Coordinates
  if wc is None:
    Xrg = Actor.GetXRange()
    Yrg = Actor.GetYRange()
  else:
    Xrg=float(wc[0]),float(wc[1])
    Yrg=float(wc[2]),float(wc[3])
  
  print "VIEWPORT:",vp
  print "XrgYrg:",Xrg,Yrg
  ## Where they are in term of pixels
  oll = vtkWorld2Renderer(Renderer,Xrg[0],Yrg[0])
  our = vtkWorld2Renderer(Renderer,Xrg[1],Yrg[1])
  
  print "oll,our:",oll,our
  # Where they should be in term of pixel
  ll = world2Renderer(Renderer,Xrg[0],Yrg[0],
      vp,
      [Xrg[0],Xrg[1],Yrg[0],Yrg[1]])
  ur = world2Renderer(Renderer,Xrg[1],Yrg[1],
      vp,
      [Xrg[0],Xrg[1],Yrg[0],Yrg[1]])
  
  print "ll,ur:",ll,ur
  # How much does it needs to be scaled by?
  xScale = (ur[0]-ll[0])/(our[0]-oll[0])
  yScale = (ur[1]-ll[1])/(our[1]-oll[1])
  print xScale,yScale

  #World coordinates of where they need to be
  LL = R2World(Renderer,*ll)
  print "LL:",LL

  # Move it to the correct bottom left corner
  dX = LL[0]-Xrg[0]
  dY = LL[1]-Yrg[0]
  print "dX,dY:",dX,dY


  # transformation are applied in reverse order
  T = vtk.vtkTransform()
  ## After scaling move it back to lower left corner
  T.Translate(LL[0]*(1.-xScale),LL[1]*(1.-yScale),0)
  ## scale
  T.Scale(xScale,yScale,1)
  # Move it to the correct bottom left corner
  T.Translate(dX,dY,0)

  Actor.SetUserTransform(T)
  Renderer.Render()
  print "oll,our:",oll,our
  # Where they should be in term of pixel
  ll = world2Renderer(Renderer,Xrg[0],Yrg[0],
      vp,
      [Xrg[0],Xrg[1],Yrg[0],Yrg[1]])
  print "ll,ur:",ll,ur
  #World coordinates of where they need to be
  LL = R2World(Renderer,*ll)
  print "LL2:",LL

  return Actor

