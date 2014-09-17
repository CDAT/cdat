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
import warnings
import cdtime

f = open(os.path.join(sys.prefix,"share","vcs","wmo_symbols.json"))
wmo = json.load(f)

def putMaskOnVTKGrid(data,grid,actorColor=None,cellData=True,deep=True):
  #Ok now looking
  msk = data.mask
  imsk =  VN.numpy_to_vtk(msk.astype(numpy.int).flat,deep=deep)
  mapper = None
  if msk is not numpy.ma.nomask and not numpy.allclose(msk,False):
      msk =  VN.numpy_to_vtk(numpy.logical_not(msk).astype(numpy.uint8).flat,deep=deep)
      if actorColor is not None:
          if grid.IsA("vtkStructuredGrid"):
            grid2 = vtk.vtkStructuredGrid()
          else:
            grid2 = vtk.vtkUnstructuredGrid()
          grid2.CopyStructure(grid)
          geoFilter = vtk.vtkDataSetSurfaceFilter()
          if not cellData:
              grid2.GetPointData().SetScalars(imsk)
              #grid2.SetCellVisibilityArray(imsk)
              p2c = vtk.vtkPointDataToCellData()
              p2c.SetInputData(grid2)
              geoFilter.SetInputConnection(p2c.GetOutputPort())
          else:
              grid2.GetCellData().SetScalars(imsk)
              geoFilter.SetInputData(grid)
          geoFilter.Update()
          mapper = vtk.vtkPolyDataMapper()
          mapper.SetInputData(geoFilter.GetOutput())
          lut = vtk.vtkLookupTable()
          lut.SetNumberOfTableValues(1)
          r,g,b = actorColor
          lut.SetNumberOfTableValues(2)
          lut.SetTableValue(0,r/100.,g/100.,b/100.)
          lut.SetTableValue(1,r/100.,g/100.,b/100.)
          mapper.SetLookupTable(lut)
          mapper.SetScalarRange(1,1)
      if grid.IsA("vtkStructuredGrid"):
        if not cellData:
          grid.SetPointVisibilityArray(msk)
        else:
          grid.SetCellVisibilityArray(msk)
  return mapper

def genGrid(data1,data2,gm):
  continents = False
  wrap = None
  m3 = None
  g = None
  cellData = True
  xm,xM,ym,yM = None, None, None, None
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
    ## Could still be meshfill with mesh data
    ## Ok probably should do a test for hgrid before sending data2
    if isinstance(gm,meshfill.Gfm) and data2 is not None:
      xm = data2[:,1].min()
      xM = data2[:,1].max()
      ym = data2[:,0].min()
      yM = data2[:,0].max()
      N = data2.shape[0]
      m2 = numpy.ascontiguousarray(numpy.transpose(data2,(0,2,1)))
      nVertices = m2.shape[-2]
      m2.resize((m2.shape[0]*m2.shape[1],m2.shape[2]))
      m2=m2[...,::-1]
      # here we add dummy levels, might want to reconsider converting "trimData" to "reOrderData" and use actual levels?
      m3=numpy.concatenate((m2,numpy.zeros((m2.shape[0],1))),axis=1)
      if gm.wrap[1]==360.:
        continents = True
      wrap = gm.wrap
  except Exception,err: # Ok no mesh on file, will do with lat/lon
    print "No mesh data found"
  if m3 is not None:
    #Create unstructured grid points
    vg = vtk.vtkUnstructuredGrid()
    lst = vtk.vtkIdTypeArray()
    cells = vtk.vtkCellArray()
    numberOfCells = N
    lst.SetNumberOfComponents(nVertices + 1)
    lst.SetNumberOfTuples(numberOfCells)
    for i in range(N):
      tuple = [None] * (nVertices + 1)
      tuple[0] = nVertices
      for j in range(nVertices):
        tuple[j + 1] = i * nVertices + j
      lst.SetTuple(i, tuple)
      ## ??? TODO ??? when 3D use CUBE?
    cells.SetCells(numberOfCells, lst)
    vg.SetCells(vtk.VTK_POLYGON, cells)
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
          lon=data1.getAxis(-1)
          lat=data1.getAxis(-2)
          xm=lon[0]
          xM=lon[-1]
          ym=lat[0]
          yM=lat[-1]
          lat2 = numpy.zeros(len(lat)+1)
          lon2 = numpy.zeros(len(lon)+1)
          # Ok let's try to get the bounds
          try:
            blat = lat.getBounds()
            blon = lon.getBounds()
            lat2[:len(lat)]=blat[:,0]
            lat2[len(lat)]=blat[-1,1]
            lon2[:len(lon)]=blon[:,0]
            lon2[len(lon)]=blon[-1,1]
            xm=blon[0][0]
            xM=blon[-1][1]
            ym=blat[0][0]
            yM=blat[-1][1]
          except Exception,err:
            ## No luck we have to generate bounds ourselves
            lat2[1:-1]=(lat[:-1]+lat[1:])/2.
            lat2[0]=lat[0]-(lat[1]-lat[0])/2.
            lat2[-1]=lat[-1]+(lat[-1]-lat[-2])/2.
            lon2[1:-1]=(lon[:-1]+lon[1:])/2.
            lon2[0]=lon[0]-(lon[1]-lon[0])/2.
            lon2[-1]=lat[-1]+(lat[-1]-lat[-2])/2.
          lat = lat2[:,numpy.newaxis]*numpy.ones(lon2.shape)[numpy.newaxis,:]
          lon = lon2[numpy.newaxis,:]*numpy.ones(lat2.shape)[:,numpy.newaxis]
      else:
        #Ok in this case it is a structured grid we have no bounds, using ponitData
        ## ??? We should probably revist and see if we can use the "bounds" attribute
        ## to generate cell instead of points
        cellData = False
    else:
      data1=cdms2.asVariable(data1)
      lon=data1.getAxis(-1)
      lat=data1.getAxis(-2)
      xm=lon[0]
      xM=lon[-1]
      ym=lat[0]
      yM=lat[-1]
      lat2 = numpy.zeros(len(lat)+1)
      lon2 = numpy.zeros(len(lon)+1)
      # Ok let's try to get the bounds
      try:
        blat = lat.getBounds()
        blon = lon.GetBounds()
        lat2[:len(lat)]=blat[:][0]
        lat2[len(lat2)]=blat[-1][1]
        lon2[:len(lon)]=blon[:][0]
        lon2[len(lon2)]=blon[-1][1]
        xm=blon[0][0]
        xM=blon[-1][1]
        ym=blat[0][0]
        yM=blat[-1][1]
      except:
        ## No luck we have to generate bounds ourselves
        lat2[1:-1]=(lat[:-1]+lat[1:])/2.
        lat2[0]=lat[0]-(lat[1]-lat[0])/2.
        lat2[-1]=lat[-1]+(lat[-1]-lat[-2])/2.
        lon2[1:-1]=(lon[:-1]+lon[1:])/2.
        lon2[0]=lon[0]-(lon[1]-lon[0])/2.
        lon2[-1]=lon[-1]+(lon[-1]-lon[-2])/2.
      lat = lat2[:,numpy.newaxis]*numpy.ones(lon2.shape)[numpy.newaxis,:]
      lon = lon2[numpy.newaxis,:]*numpy.ones(lat2.shape)[:,numpy.newaxis]
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
    if xm is None:
      try:
        xm = lon[0]
        xM = lon[-1]
        ym = lat[0]
        yM = lat[-1]
      except:
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
  xm,xM,ym,yM = getRange(gm,xm,xM,ym,yM)
  geo, geopts = project(pts,projection,[xm,xM,ym,yM])
  ## Sets the vertics into the grid
  vg.SetPoints(geopts)
  return vg,xm,xM,ym,yM,continents,wrap,geo,cellData

def getRange(gm,xm,xM,ym,yM):
    # Also need to make sure it fills the whole space
    rtype= type(cdtime.reltime(0,"days since 2000"))
    X1,X2 = gm.datawc_x1,gm.datawc_x2
    if isinstance(X1,rtype) or isinstance(X2,rtype):
      X1=X1.value
      X2=X2.value
    if not numpy.allclose([X1,X2],1.e20):
      x1,x2 = X1,X2
    else:
      x1,x2 = xm,xM
    Y1,Y2 = gm.datawc_y1,gm.datawc_y2
    if isinstance(Y1,rtype) or isinstance(Y2,rtype):
      Y1=Y1.value
      Y2=Y2.value
    if not numpy.allclose([Y1,Y2],1.e20):
      y1,y2 = Y1,Y2
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
def project(pts,projection,wc):
  xm,xM,ym,yM= wc
  if isinstance(projection,(str,unicode)):
    projection = vcs.elements["projection"][projection]
  if projection.type=="linear":
    return None,pts
  geo = vtk.vtkGeoTransform()
  ps = vtk.vtkGeoProjection()
  #for i in range(ps.GetNumberOfProjections()):
  #  print i, ps.GetProjectionName(i)
  pd = vtk.vtkGeoProjection()
  names = ["linear","utm","state","aea","lcc","merc","stere","poly","eqdc","tmerc","stere","lcca","azi","gnom","ortho","vertnearper","sinu","eqc","mill","vandg","omerc","robin","somerc","alsk","goode","moll","imoll","hammer","wag4","wag7","oea"]
  proj_dic = {"polar stereographic":"stere",
      -3:"aeqd",
      }
  for i in range(len(names)):
    proj_dic[i]=names[i]

  pname = proj_dic.get(projection._type,projection.type)
  projName = pname
  #for i in range(0,184,2):
  #  pd.SetName(pd.GetProjectionName(i))
  #  print i,":",pd.GetProjectionName(i),"(",pd.GetNumberOfOptionalParameters(),") --"
  #  pd.SetName(pd.GetProjectionName(i+1))
  #  print i+1,":",pd.GetProjectionName(i+1),"(",pd.GetNumberOfOptionalParameters(),")"

  pd.SetName(projName)
  if projection.type == "polar (non gctp)":
    if ym<yM:
      pd.SetOptionalParameter("lat_0","-90.")
      pd.SetCentralMeridian(xm)
    else:
      pd.SetOptionalParameter("lat_0","90.")
      pd.SetCentralMeridian(xm+180.)
  else:
    setProjectionParameters(pd,projection)
  geo.SetSourceProjection(ps)
  geo.SetDestinationProjection(pd)
  geopts = vtk.vtkPoints()
  geo.TransformPoints(pts,geopts)
  return geo,geopts

def setProjectionParameters(pd,proj):
    if proj._type>200:
      proj4 = proj.parameters
      if numpy.allclose(proj4,1.e20):
        proj4={}
    else:
        p=proj.parameters
        proj4={}
        if proj._type in [3,4]:
             proj4["a"]=proj.smajor
             proj4["b"]=proj.sminor
             proj4["lat_1"]=proj.standardparallel1
             proj4["lat_2"]=proj.standardparallel2
             proj4["lon_0"]=proj.centralmeridian
             proj4["lat_0"]=proj.originlatitude
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
        elif proj._type==5:
             proj4["a"]=proj.smajor
             proj4["b"]=proj.sminor
             proj4["lon_0"]=proj.centralmeridian
             proj4["lat_ts"]=proj.truescale
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
        elif proj._type==6:
             proj4["a"]=proj.smajor
             proj4["b"]=proj.sminor
             proj4["lon_wrap"]=proj.centerlongitude # MAP NAME ?????
             proj4["lat_ts"]=proj.truescale
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
        elif proj._type==7:
             proj4["a"]=proj.smajor
             proj4["b"]=proj.sminor
             proj4["lon_0"]=proj.centralmeridian
             proj4["lat_0"]=proj.originlatitude
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
        elif proj._type==8:
             proj4["a"]=proj.smajor
             proj4["b"]=proj.sminor
             proj4["lon_0"]=proj.centralmeridian
             proj4["lat_0"]=proj.originlatitude
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
             if (p[8]==0 or p[8]>9.9E19):
                  proj4["subtype"]=proj.subtype=0
                  proj4["lat_1"]=proj.standardparallel # MAP NAME ?????
                  proj4["lat_2"]=proj.standardparallel # MAP NAME ?????
                  proj4["lat_ts"]=proj.standardparallel # MAP NAME ?????
             else:
                  proj4["subtype"]=proj.subtype=1
                  proj4["lat_1"]=proj.standardparallel1
                  proj4["lat_2"]=proj.standardparallel2
        elif proj._type==9:
             proj4["a"]=proj.smajor
             proj4["b"]=proj.sminor
             proj4["k_0"]=proj.factor
             proj4["lon_0"]=proj.centralmeridian
             proj4["lat_0"]=proj.originlatitude
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
        elif proj._type in [10,11,12,13,14]:
             proj4["a"]=proj.sphere # MAP NAME ?????
             proj4["b"]=proj.sphere # MAP NAME ?????
             proj4["lon_0"]=proj.centerlongitude
             proj4["lat_0"]=proj.centerlatitude
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
        elif proj._type==15:
             proj4["a"]=proj.sphere # MAP NAME ?????
             proj4["b"]=proj.sphere # MAP NAME ?????
             proj4["height"]=proj.height # MAP NAME ?????
             proj4["lon_0"]=proj.centerlongitude
             proj4["lat_0"]=proj.centerlatitude
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
        elif proj._type in [16,18,21,25,27,28,29]:
             proj4["a"]=proj.sphere # MAP NAME ?????
             proj4["b"]=proj.sphere # MAP NAME ?????
             proj4["lon_0"]=proj.centralmeridian
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
        elif proj._type==17:
             proj4["a"]=proj.sphere
             proj4["b"]=proj.sphere
             proj4["lon_0"]=proj.centralmeridian
             proj4["lat_ts"]=proj.truescale
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
        elif proj._type==19:
             proj4["a"]=proj.sphere # MAP NAME ?????
             proj4["b"]=proj.sphere # MAP NAME ?????
             proj4["lon_0"]=proj.centralmeridian
             proj4["lat_0"]=proj.originlatitude
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
        elif proj._type==20:
             proj4["a"]=proj.smajor
             proj4["b"]=proj.sminor
             proj4["k_0"]=proj.factor
             proj4["lat_0"]=proj.originlatitude
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
             if (p[12]==0 or p[12]>9.9E19):
                  proj4["subtype"]=proj.subtype
                  proj4["lon_0"]=proj.longitude1 # MAP NAME ?????
                  proj4["lat_1"]=proj.latitude1
                  proj4["lonc"]=proj.longitude2 # MAP NAME ?????
                  proj4["lat_2"]=proj.latitude2
             else:
                  proj4["subtype"]=proj.subtype
                  proj4["azi"]=proj.azimuthalangle # MAP NAME ?????
                  proj4["lon_0"]=proj.azimuthallongitude # MAP NAME ?????
        elif proj._type==22:
             proj4["a"]=proj.smajor
             proj4["b"]=proj.sminor
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
             if (p[12]==0 or p[12]>9.9E19):
                  proj4["subtype"]=proj.subtype
                  proj4["???"]=proj.orbitinclination # MAP NAME ?????
                  proj4["???"]=proj.orbitlongitude # MAP NAME ?????
                  proj4["???"]=proj.satelliterevolutionperiod # MAP NAME ?????
                  proj4["???"]=proj.landsatcompensationratio # MAP NAME ?????
                  proj4["???"]=proj.pathflag # MAP NAME ?????
             else:
                  proj4["subtype"]=proj.subtype
                  proj4["???"]=proj.satellite # MAP NAME ?????
                  proj4["???"]=proj.path # MAP NAME ?????
        elif proj._type==23:
             proj4["a"]=proj.smajor
             proj4["b"]=proj.sminor
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing
        elif proj._type in [24,26]:
             proj4["a"]=proj.sphere # MAP NAME ?????
             proj4["b"]=proj.sphere # MAP NAME ?????
        elif proj._type==30:
             proj4["a"]=proj.sphere # MAP NAME ?????
             proj4["b"]=proj.sphere # MAP NAME ?????
             proj4["???"]=proj.shapem # MAP NAME ?????
             proj4["???"]=proj.shapen # MAP NAME ?????
             proj4["lon_0"]=proj.centerlongitude
             proj4["lat_0"]=proj.centerlatitude
             proj4["x_0"]=proj.falseeasting
             proj4["y_0"]=proj.falsenorthing

    if proj._type==6:
      pd.SetOptionalParameter("lat_0","90")
    for k in proj4:
      if not numpy.allclose(proj4[k],1.e20):
        if k=="lon_0":
          pd.SetCentralMeridian(proj4[k])
        elif k!="???":
          pd.SetOptionalParameter(k,str(proj4[k]))

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
  if wrap is None:
    return Act
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
  appendFilter.Update()
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
      appendFilter.Update()
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
      appendFilter.Update()

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
      appendFilter.Update()
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
  appendFilter.Update()
  Actor = vtk.vtkActor()
  Actor.SetProperty(Act.GetProperty())
  #Mapper2 = vtk.vtkDataSetMapper()
  #Mapper2 = vtk.vtkCompositePolyDataMapper()
  Mapper2 = vtk.vtkPolyDataMapper()
  Mapper2.SetInputData(appendFilter.GetOutput())
  Mapper2.SetLookupTable(Mapper.GetLookupTable())
  Mapper2.SetScalarRange(Mapper.GetScalarRange())
  Mapper2.SetScalarMode(Mapper.GetScalarMode())
  setClipPlanes(Mapper2, xmn, xmx, ymn, ymx)
  Mapper2.Update()
  Actor.SetMapper(Mapper2)
  return Actor

def setClipPlanes(mapper, xmin, xmax, ymin, ymax):
    clipPlaneCollection = vtk.vtkPlaneCollection()

    if xmin != xmax:
      clipPlaneXMin = vtk.vtkPlane()
      clipPlaneXMin.SetOrigin(xmin, 0.0, 0.0)
      clipPlaneXMin.SetNormal(1.0, 0.0, 0.0)

      clipPlaneXMax = vtk.vtkPlane()
      clipPlaneXMax.SetOrigin(xmax, 0.0, 0.0)
      clipPlaneXMax.SetNormal(-1.0, 0.0, 0.0)

      clipPlaneCollection.AddItem(clipPlaneXMin)
      clipPlaneCollection.AddItem(clipPlaneXMax)

    if ymin != ymax:
      clipPlaneYMin = vtk.vtkPlane()
      clipPlaneYMin.SetOrigin(0.0, ymin, 0.0)
      clipPlaneYMin.SetNormal(0.0, 1.0, 0.0)

      clipPlaneYMax = vtk.vtkPlane()
      clipPlaneYMax.SetOrigin(0.0, ymax, 0.0)
      clipPlaneYMax.SetNormal(0.0, -1.0, 0.0)

      clipPlaneCollection.AddItem(clipPlaneYMin)
      clipPlaneCollection.AddItem(clipPlaneYMax)

    if clipPlaneCollection.GetNumberOfItems() > 0:
        mapper.SetClippingPlanes(clipPlaneCollection)

# The code is replaced by the setClipPlanes above
# def doClip(data, xmin,xmax,ymin,ymax):
#   if xmin!=xmax:
#     xminClip = doClip1(data,xmin,1,0)
#     xfullClip = doClip1(xminClip,xmax,-1,0)
#   else:
#     xfullClip = data
#   if ymin!=ymax:
#     yminClip  = doClip1(xfullClip,ymin,1,1)
#     xyClip  = doClip1(yminClip,ymax,-1,1)
#   else:
#     xyClip = xfullClip
#   return xyClip

# def doClip1(data,value,normal,axis=0):
#     return data
#     # We have the actor, do clipping
#     clpf = vtk.vtkPlane()
#     if axis == 0:
#       clpf.SetOrigin(value,0,0)
#       clpf.SetNormal(normal,0,0)
#     else:
#       clpf.SetOrigin(0,value,0)
#       clpf.SetNormal(0,normal,0)
#     clp = vtk.vtkClipPolyData()
#     clp.SetClipFunction(clpf)
#     clp.SetInputData(data)
#     clp.Update()
#     return clp.GetOutput()

def prepTextProperty(p,winSize,to="default",tt="default",cmap=None):
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
  p.SetFontSize(int(to.height*winSize[1]/800.))


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

  sz = renderer.GetRenderWindow().GetSize()
  for i in range(n):
    t = vtk.vtkTextActor()
    p=t.GetTextProperty()
    prepTextProperty(p,sz,to,tt,cmap)
    t.SetInput(string[i])
    X,Y = world2Renderer(renderer,x[i],y[i],tt.viewport,tt.worldcoordinate)
    t.SetPosition(X,Y)
    #T=vtk.vtkTransform()
    #T.Scale(1.,sz[1]/606.,1.)
    #T.RotateY(to.angle)
    #t.SetUserTransform(T)
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
    geo,pts = project(pts,farea.projection,farea.worldcoordinate)
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
    fitToViewport(a,ren,farea.viewport,wc=farea.worldcoordinate,geo=geo)
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
    s=marker.size[i]*.5
    t=marker.type[i]
    N = max(len(x),len(y))
    for a in [x,y]:
      while len(a)<n:
        a.append(a[-1])
    pts = vtk.vtkPoints()
    geo,pts = project(pts,marker.projection,marker.worldcoordinate)
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
      s =s/5.
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
      s*=3
      #Lines first
      for l in params["line"]:
        coords = numpy.array(zip(*l))*s/30.
        line = genPoly(coords.tolist(),pts,filled=False)
        lines.InsertNextCell(line)
      for l in params["poly"]:
        coords = numpy.array(zip(*l))*s/30.
        line = genPoly(coords.tolist(),pts,filled=True)
        polys.InsertNextCell(line)
      geo,pts = project(pts,marker.projection,marker.worldcoordinate)
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
      
    if pd is None:
      s/=float(max(marker.worldcoordinate))
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
    fitToViewport(a,ren,marker.viewport,wc=marker.worldcoordinate,geo=geo)

    # Add a transform to correct the glyph's aspect ratio:

    if pd is None and a.GetUserTransform():
      # Invert the scale of the actor's transform.
      glyphTransform = vtk.vtkTransform()
      scale = a.GetUserTransform().GetScale()
      xComp = scale[0]
      scale = [xComp / float(val) for val in scale]
      glyphTransform.Scale(scale)

      glyphFixer = vtk.vtkTransformPolyDataFilter()
      glyphFixer.SetTransform(glyphTransform)

      if pd is None:
        glyphFixer.SetInputConnection(gs.GetOutputPort())
      else:
        glyphFixer.SetInputData(pd)
        g.SetSourceData(None)

      g.SetSourceConnection(glyphFixer.GetOutputPort())

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
    geo,pts=project(pts,line.projection,line.worldcoordinate)
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
    fitToViewport(a,ren,line.viewport,wc=line.worldcoordinate,geo=geo)
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
  return wp

def vtkWorld2Renderer(ren,x,y):
  ren.SetWorldPoint(x,y,0,0)
  ren.WorldToDisplay()
  renpts = ren.GetDisplayPoint()
  return renpts

def fitToViewport(Actor,Renderer,vp,wc=None,geo=None):
  T = vtk.vtkTransform()
  ## Data range in World Coordinates
  if wc is None:
    Xrg = list(Actor.GetXRange())
    Yrg = list(Actor.GetYRange())
  else:
    Xrg=[float(wc[0]),float(wc[1])]
    Yrg=[float(wc[2]),float(wc[3])]
  if Yrg[0]>Yrg[1]:
    #Yrg=[Yrg[1],Yrg[0]]
    #T.RotateY(180)
    Yrg=[Yrg[1],Yrg[0]]
    flipY = True
  else:
    flipY = False
  if Xrg[0]>Xrg[1]:
    Xrg=[Xrg[1],Xrg[0]]
    flipX=True
  else:
    flipX=False

  if geo is not None:
   pt = vtk.vtkPoints()
   pt.SetNumberOfPoints(1)
   Xrg2 = [1.e20,-1.e20]
   Yrg2 = [1.e20,-1.e20]
   Npts=50.
   for x in numpy.arange(Xrg[0],Xrg[1],(Xrg[1]-Xrg[0])/Npts):
     for y in numpy.arange(Yrg[0],Yrg[1],(Yrg[1]-Yrg[0])/Npts):
       pt.SetPoint(0,x,y,0)
       pts = vtk.vtkPoints()
       geo.TransformPoints(pt,pts)
       b = pts.GetBounds()
       xm,xM,ym,yM=b[:4]
       if xm!=-numpy.inf:
         Xrg2[0]=min(Xrg2[0],xm)
       if xM!=numpy.inf:
         Xrg2[1]=max(Xrg2[1],xM)
       if ym!=-numpy.inf:
         Yrg2[0]=min(Yrg2[0],ym)
       if yM!=numpy.inf:
         Yrg2[1]=max(Yrg2[1],yM)
   Xrg=Xrg2
   Yrg=Yrg2
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


  T.Scale(xScale,yScale,1.)

  Actor.SetUserTransform(T)

  mapper = Actor.GetMapper()
  planeCollection = mapper.GetClippingPlanes()

  # We have to transform the hardware clip planes as well
  if (planeCollection is not None):
      planeCollection.InitTraversal()
      plane = planeCollection.GetNextItem()
      while (plane):
          origin = plane.GetOrigin()
          inOrigin = [origin[0], origin[1], origin[2], 1.0]
          outOrigin = [origin[0], origin[1], origin[2], 1.0]

          normal = plane.GetNormal()
          inNormal = [normal[0], normal[1], normal[2], 0.0]
          outNormal = [normal[0], normal[1], normal[2], 0.0]

          T.MultiplyPoint(inOrigin, outOrigin)
          if (outOrigin[3] != 0.0):
              outOrigin[0] /= outOrigin[3]
              outOrigin[1] /= outOrigin[3]
              outOrigin[2] /= outOrigin[3]
          plane.SetOrigin(outOrigin[0], outOrigin[1], outOrigin[2])

          # For normal matrix, compute the transpose of inverse
          normalTransform = vtk.vtkTransform()
          normalTransform.DeepCopy(T)
          mat = vtk.vtkMatrix4x4()
          normalTransform.GetTranspose(mat)
          normalTransform.GetInverse(mat)
          normalTransform.SetMatrix(mat)
          normalTransform.MultiplyPoint(inNormal, outNormal)
          if (outNormal[3] != 0.0):
              outNormal[0] /= outNormal[3]
              outNormal[1] /= outNormal[3]
              outNormal[2] /= outNormal[3]
          plane.SetNormal(outNormal[0], outNormal[1], outNormal[2])

          plane = planeCollection.GetNextItem()

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
  if geo is None:
    if flipY:
      cam.Elevation(180.)
      cam.Roll(180.)
      pass
    if flipX:
      cam.Azimuth(180.)

p=vtk.vtkGeoProjection()
vtkProjections = [ p.GetProjectionName(i) for i in range(p.GetNumberOfProjections()) ]
def checkProjType(self,name,value):
  if value in vtkProjections:
    warnings.warn("%s is a VTK backend specific projection, it might not work if you are not using the VTK backend" % value)
    return 200+vtkProjections.index(value)
  raise Exception("%s is not a known VTK projection" % value)
def checkProjParameters(self,name,value):
  if not isinstance(value,dict):
    raise ValueError("VTK specific projections parameters attribute needs to be a dictionary")
  return value
def getProjType(value):
  return vtkProjections[value-200]
