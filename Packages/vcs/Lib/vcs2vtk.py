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
from projection import round_projections

f = open(os.path.join(vcs.prefix,"share","vcs","wmo_symbols.json"))
wmo = json.load(f)

def applyAttributesFromVCStmpl(tmpl,tmplattribute,txtobj=None):
    tatt = getattr(tmpl,tmplattribute)
    if txtobj is None:
        txtobj = vcs.createtext(To_source=tatt.textorientation,Tt_source=tatt.texttable)
    for att in ["x","y","priority"]:
        setattr(txtobj,att,getattr(tatt,att))
    return txtobj

def numpy_to_vtk_wrapper(numpyArray, deep=False, array_type=None):
    result = VN.numpy_to_vtk(numpyArray, deep, array_type)
    # Prevent garbage collection on shallow copied data:
    if not deep:
        result.numpyArray = numpyArray
    return result

def putMaskOnVTKGrid(data,grid,actorColor=None,cellData=True,deep=True):
  #Ok now looking
  msk = data.mask
  imsk =  numpy_to_vtk_wrapper(msk.astype(numpy.int).flat,deep=deep)
  mapper = None
  if msk is not numpy.ma.nomask and not numpy.allclose(msk,False):
      msk =  numpy_to_vtk_wrapper(numpy.logical_not(msk).astype(numpy.uint8).flat,deep=deep)
      nomsk = numpy_to_vtk_wrapper(numpy.ones(data.mask.shape,numpy.uint8).flat,deep=deep)
      if actorColor is not None:
          if grid.IsA("vtkStructuredGrid"):
            grid2 = vtk.vtkStructuredGrid()
          else:
            grid2 = vtk.vtkUnstructuredGrid()
          grid2.CopyStructure(grid)
          geoFilter = vtk.vtkDataSetSurfaceFilter()
          lut = vtk.vtkLookupTable()
          r,g,b = actorColor
          lut.SetNumberOfTableValues(2)
          if not cellData:
              if grid2.IsA("vtkStructuredGrid"):
                  grid2.SetPointVisibilityArray(nomsk)
              grid2.GetPointData().SetScalars(imsk)
              #grid2.SetCellVisibilityArray(imsk)
              #p2c = vtk.vtkPointDataToCellData()
              #p2c.SetInputData(grid2)
              #geoFilter.SetInputConnection(p2c.GetOutputPort())
              geoFilter.SetInputData(grid2)
              lut.SetTableValue(0,r/100.,g/100.,b/100.,1.)
              lut.SetTableValue(1,r/100.,g/100.,b/100.,1.)
          else:
              if grid2.IsA("vtkStructuredGrid"):
                  grid2.SetCellVisibilityArray(nomsk)
              grid2.GetCellData().SetScalars(imsk)
              geoFilter.SetInputData(grid2)
              lut.SetTableValue(0,r/100.,g/100.,b/100.,0.)
              lut.SetTableValue(1,r/100.,g/100.,b/100.,1.)
          geoFilter.Update()
          mapper = vtk.vtkPolyDataMapper()
          mapper.SetInputData(geoFilter.GetOutput())
          mapper.SetLookupTable(lut)
          mapper.SetScalarRange(0,1)
      if grid.IsA("vtkStructuredGrid"):
        if not cellData:
          grid.SetPointVisibilityArray(msk)
        else:
          grid.SetCellVisibilityArray(msk)
  return mapper

def genGridOnPoints(data1,gm,deep=True,grid=None,geo=None):
  continents = False
  xm,xM,ym,yM = None, None, None, None
  useStructuredGrid = True
  try:
    g=data1.getGrid()
    if grid is None:
      x = g.getLongitude()[:]
      y = g.getLatitude()[:]
    continents=True
    wrap=[0,360]
    if isinstance(g,cdms2.gengrid.AbstractGenericGrid): # Ok need unstrctured grid
      useStructuredGrid = False
  except:
    #hum no grid that's much easier
    wrap=None
    if grid is None:
      x=data1.getAxis(-1)[:]
      y=data1.getAxis(-2)[:]

  if grid is None:
    if x.ndim==1:
      y = y[:,numpy.newaxis]*numpy.ones(x.shape)[numpy.newaxis,:]
      x = x[numpy.newaxis,:]*numpy.ones(y.shape)
    x=x.flatten()
    y=y.flatten()
    sh =list(x.shape)
    sh.append(1)
    x=numpy.reshape(x,sh)
    y=numpy.reshape(y,sh)
    #Ok we have our points in 2D let's create unstructured points grid
    xm=x.min()
    xM=x.max()
    ym=y.min()
    yM=y.max()
    z = numpy.zeros(x.shape)
    m3 = numpy.concatenate((x,y),axis=1)
    m3 = numpy.concatenate((m3,z),axis=1)
    deep = True
    pts = vtk.vtkPoints()
    ## Convert nupmy array to vtk ones
    ppV = numpy_to_vtk_wrapper(m3,deep=deep)
    pts.SetData(ppV)
  else:
    xm,xM,ym,yM,tmp,tmp2 = grid.GetPoints().GetBounds()
    vg = grid
  projection = vcs.elements["projection"][gm.projection]
  xm,xM,ym,yM = getRange(gm,xm,xM,ym,yM)
  if geo is None:
    geo, geopts = project(pts,projection,[xm,xM,ym,yM])
  ## Sets the vertices into the grid
  if grid is None:
    if useStructuredGrid:
      vg = vtk.vtkStructuredGrid()
      vg.SetDimensions(data1.shape[1],data1.shape[0],1)
    else:
      vg = vtk.vtkUnstructuredGrid()
    vg.SetPoints(geopts)
  else:
    vg=grid
  out={"vtk_backend_grid":vg,
      "xm":xm,
      "xM":xM,
      "ym":ym,
      "yM":yM,
      "continents":continents,
      "wrap":wrap,
      "geo":geo,
      }
  return out
  
def genGrid(data1,data2,gm,deep=True,grid=None,geo=None):
  continents = False
  wrap = None
  m3 = None
  g = None
  cellData = True
  xm,xM,ym,yM = None, None, None, None
  try: #First try to see if we can get a mesh out of this
    g=data1.getGrid()
    if isinstance(g,cdms2.gengrid.AbstractGenericGrid): # Ok need unstrctured grid
      continents = True
      wrap = [0.,360.]
      if grid is None:
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
    ## Could still be meshfill with mesh data
    ## Ok probably should do a test for hgrid before sending data2
    if isinstance(gm,meshfill.Gfm) and data2 is not None:
      wrap = gm.wrap
      if gm.wrap[1]==360.:
        continents = True
      if grid is None:
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
  except Exception,err: # Ok no mesh on file, will do with lat/lon
    pass
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
    if grid is None:
      vg = vtk.vtkStructuredGrid()
    if g is not None:
      # Ok we have grid
      continents = True
      wrap = [0.,360.]
      if grid is None:
        lat = g.getLatitude()
        lon = g.getLongitude()
      if isinstance(g,cdms2.hgrid.AbstractCurveGrid):
        #Ok in this case it is a structured grid we have no bounds, using ponitData
        ## ??? We should probably revist and see if we can use the "bounds" attribute
        ## to generate cell instead of points
        cellData = False
      elif grid is None:
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
    elif grid is None:
      ## No grid info from data, making one up
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
    if grid is None:
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
  if grid is None:
    # First create the points/vertices (in vcs terms)
    pts = vtk.vtkPoints()
    ## Convert nupmy array to vtk ones
    ppV = numpy_to_vtk_wrapper(m3,deep=deep)
    pts.SetData(ppV)
  else:
     xm,xM,ym,yM,tmp,tmp2 = grid.GetPoints().GetBounds()
  projection = vcs.elements["projection"][gm.projection]
  xm,xM,ym,yM = getRange(gm,xm,xM,ym,yM)
  if grid is None:
    geo, geopts = project(pts,projection,[xm,xM,ym,yM],geo)
    ## Sets the vertics into the grid
    vg.SetPoints(geopts)
  else:
    vg=grid
  out={"vtk_backend_grid":vg,
      "xm":xm,
      "xM":xM,
      "ym":ym,
      "yM":yM,
      "continents":continents,
      "wrap":wrap,
      "geo":geo,
      "cellData":cellData,
      }
  return out

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
## Try to save time and memorize these continents
vcsContinents= {}
def prepContinents(fnm):
  """ This converts vcs continents files to vtkpolydata
  Author: Charles Doutriaux
  Input: vcs continent file name
  """
  if vcsContinents.has_key(fnm):
    return vcsContinents[fnm]
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
              pts.InsertNextPoint(p[1],p[0],0.)
            n+=sn
            didIt = True
          except:
            didIt = False
        if didIt is False:
          while len(ln)>2:
            l,L=float(ln[:8]),float(ln[8:16])
            pts.InsertNextPoint(L,l,0.)
            ln=ln[16:]
            n+=2
    ln = vtk.vtkPolyLine()
    ln.GetPointIds().SetNumberOfIds(N/2)
    for i in range(N/2): ln.GetPointIds().SetId(i,i+npts)
    cells.InsertNextCell(ln)
    ln=f.readline()
  poly.SetPoints(pts)
  poly.SetLines(cells)

  # The dataset has some duplicate lines that extend outside of x=[-180, 180],
  # which will cause wrapping artifacts for certain projections (e.g.
  # Robinson). Clip out the duplicate data:
  box = vtk.vtkBox()
  box.SetXMin(-180., -90., 0.)
  box.SetXMax(180., 90., 1.)
  clipper = vtk.vtkClipPolyData()
  clipper.SetInputData(poly)
  clipper.InsideOutOn()
  clipper.SetClipFunction(box)
  clipper.Update()
  poly = clipper.GetOutput()

  vcsContinents[fnm]=poly
  return poly


#Geo projection
def project(pts,projection,wc,geo=None):
  xm,xM,ym,yM= wc
  if isinstance(projection,(str,unicode)):
    projection = vcs.elements["projection"][projection]
  if projection.type=="linear":
    return None,pts
  if geo is None:
    geo = vtk.vtkGeoTransform()
    ps = vtk.vtkGeoProjection()
    pd = vtk.vtkGeoProjection()
    names = ["linear","utm","state","aea","lcc","merc","stere","poly","eqdc","tmerc","stere","lcca","azi","gnom","ortho","vertnearper","sinu","eqc","mill","vandg","omerc","robin","somerc","alsk","goode","moll","imoll","hammer","wag4","wag7","oea"]
    proj_dic = {"polar stereographic":"stere",
        -3:"aeqd",
        }
    for i in range(len(names)):
      proj_dic[i]=names[i]

    pname = proj_dic.get(projection._type,projection.type)
    projName = pname
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
  if fnm is None:
    fnm="foo.vtk" % dumps
  if fnm[:-4].lower()!=".vtk":
    fnm+=".vtk"
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
def doWrap(Act,wc,wrap=[0.,360], fastClip=True):
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

  # Clip the data to the final window:
  clipBox = vtk.vtkBox()
  clipBox.SetXMin(xmn, ymn, -1.0)
  clipBox.SetXMax(xmx, ymx,  1.0)
  if fastClip:
      clipper = vtk.vtkExtractPolyDataGeometry()
      clipper.ExtractInsideOn()
      clipper.SetImplicitFunction(clipBox)
      clipper.ExtractBoundaryCellsOn()
      clipper.PassPointsOff()
  else:
      clipper = vtk.vtkClipPolyData()
      clipper.InsideOutOn()
      clipper.SetClipFunction(clipBox)
  clipper.SetInputConnection(appendFilter.GetOutputPort())
  clipper.Update()

  Mapper.SetInputData(clipper.GetOutput())
  return Act

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

def prepTextProperty(p,winSize,to="default",tt="default",cmap=None,
                     overrideColorIndex = None):
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
  colorIndex = overrideColorIndex if overrideColorIndex else tt.color
  c=cmap.index[colorIndex]
  p.SetColor([C/100. for C in c])
  if to.halign in [0, 'left']:
    p.SetJustificationToLeft()
  elif to.halign in [2, 'right']:
    p.SetJustificationToRight()
  elif to.halign in [1,'center']:
    p.SetJustificationToCentered()

  p.SetOrientation(-to.angle)

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
  p.SetFontFamily(vtk.VTK_FONT_FILE)
  p.SetFontFile(vcs.elements["font"][vcs.elements["fontNumber"][tt.font]])
  p.SetFontSize(int(to.height*winSize[1]/800.))


def genTextActor(renderer,string=None,x=None,y=None,to='default',tt='default',cmap=None):
  if isinstance(to,str):
    to = vcs.elements["textorientation"][to]
  if isinstance(tt,str):
    tt = vcs.elements["texttable"][tt]
  if tt.priority==0:
    return []
  if string is None:
    string = tt.string
  if x is None:
    x = tt.x
  if y is None:
    y = tt.y
  if x is None or y is None or string in [['',],[]]:
    return []

  n = max(len(x),len(y),len(string))
  for a in [x,y,string]:
    while len(a)<n:
      a.append(a[-1])

  sz = renderer.GetRenderWindow().GetSize()
  actors=[]
  pts = vtk.vtkPoints()
  geo = None
  if vcs.elements["projection"][tt.projection].type!="linear":
      # Need to figure out new WC
      Npts = 20
      for i in range(Npts+1):
          X = tt.worldcoordinate[0]+float(i)/Npts*(tt.worldcoordinate[1]-tt.worldcoordinate[0])
          for j in range(Npts+1):
              Y = tt.worldcoordinate[2]+float(j)/Npts*(tt.worldcoordinate[3]-tt.worldcoordinate[2])
              pts.InsertNextPoint(X,Y,0.)
      geo,pts = project(pts,tt.projection,tt.worldcoordinate,geo=None)
      wc = pts.GetBounds()[:4]
      #renderer.SetViewport(tt.viewport[0],tt.viewport[2],tt.viewport[1],tt.viewport[3])
      renderer.SetWorldPoint(wc)


  for i in range(n):
    t = vtk.vtkTextActor()
    p=t.GetTextProperty()
    prepTextProperty(p,sz,to,tt,cmap)
    pts = vtk.vtkPoints()
    pts.InsertNextPoint(x[i],y[i],0.)
    if geo is not None:
        geo,pts = project(pts,tt.projection,tt.worldcoordinate,geo=geo)
        X,Y,tz=pts.GetPoint(0)
        X,Y = world2Renderer(renderer,X,Y,tt.viewport,wc)
    else:
        X,Y = world2Renderer(renderer,x[i],y[i],tt.viewport,tt.worldcoordinate)
    t.SetPosition(X,Y)
    t.SetInput(string[i])
    #T=vtk.vtkTransform()
    #T.Scale(1.,sz[1]/606.,1.)
    #T.RotateY(to.angle)
    #t.SetUserTransform(T)
    renderer.AddActor(t)
    actors.append(t)
  return actors

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

def prepFillarea(renWin,farea,cmap=None):
  n = prepPrimitive(farea)
  if n==0:
    return []
  actors =[]

  # Find color map:
  if cmap is None:
    if farea.colormap is not None:
      cmap = farea.colormap
    else:
      cmap = 'default'
  if isinstance(cmap,str):
    cmap = vcs.elements["colormap"][cmap]

  # Create data structures:
  pts = vtk.vtkPoints()
  polygons = vtk.vtkCellArray()
  colors = vtk.vtkUnsignedCharArray()
  colors.SetNumberOfComponents(3)
  colors.SetNumberOfTuples(n)
  polygonPolyData = vtk.vtkPolyData()
  polygonPolyData.SetPoints(pts)
  polygonPolyData.SetPolys(polygons)
  polygonPolyData.GetCellData().SetScalars(colors)

  # Reuse this temporary container to avoid reallocating repeatedly:
  polygon = vtk.vtkPolygon()

  # Iterate through polygons:
  for i in range(n):
    x   = farea.x[i]
    y   = farea.y[i]
    c   = farea.color[i]
    st  = farea.style[i]
    idx = farea.index[i]
    N = max(len(x),len(y))

    for a in [x,y]:
      assert(len(a) == N)

    # Add current polygon
    pid = polygon.GetPointIds()
    pid.SetNumberOfIds(N)
    for j in range(N):
      pid.SetId(j, pts.InsertNextPoint(x[j],y[j],0.))
    cellId = polygons.InsertNextCell(polygon)

    # Add the color to the color array:
    color = cmap.index[c]
    colors.SetTupleValue(cellId, [int((C/100.) * 255) for C in color])

  # Transform points:
  geo,pts = project(pts,farea.projection,farea.worldcoordinate)

  # Setup rendering
  m = vtk.vtkPolyDataMapper()
  m.SetInputData(polygonPolyData)
  a = vtk.vtkActor()
  a.SetMapper(m)
  actors.append((a,geo))
  return actors


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

def prepGlyph(g, marker, index=0):
  t, s = marker.type[index], marker.size[index] * .5
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
    gs.FilledOff()
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
  elif t[:4] == "star":
    np = 5
    points = starPoints(.001 * s, 0, 0, np)

    pts = vtk.vtkPoints()
    # Add all perimeter points
    for point in points:
      pts.InsertNextPoint((point[0], point[1], 0))

    center_id = len(points)

    # Add center point
    pts.InsertNextPoint((0,0,0))

    polygons = vtk.vtkCellArray()
    for ind in range(0, np*2, 2):
      poly = vtk.vtkPolygon()
      pid = poly.GetPointIds()
      pid.SetNumberOfIds(4)
      pid.SetId(0, ind)
      pid.SetId(1, (ind - 1) % len(points))
      pid.SetId(2, center_id)
      pid.SetId(3, (ind + 1) % len(points))
      polygons.InsertNextCell(poly)

    pd = vtk.vtkPolyData()

    pd.SetPoints(pts)
    pd.SetPolys(polygons)

    g.SetSourceData(pd)
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
    # Use the difference in x to scale the point, as later we'll use the
    # x range to correct the aspect ratio:
    dx = marker.worldcoordinate[1] - marker.worldcoordinate[0]
    s *= abs(float(dx))/500.
    gs.SetScale(s)
    gs.Update()
    g.SetSourceConnection(gs.GetOutputPort())
  return gs, pd

def setMarkerColor(p, marker, c, cmap=None):
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

def scaleMarkerGlyph(g, gs, pd, a):
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

def prepMarker(renWin,marker,cmap=None):
  n=prepPrimitive(marker)
  if n==0:
    return []
  actors=[]
  for i in range(n):
    g = vtk.vtkGlyph2D()
    markers = vtk.vtkPolyData()
    x = marker.x[i]
    y = marker.y[i]
    c = marker.color[i]
    N = max(len(x),len(y))
    for a in [x,y]:
      while len(a)<n:
        a.append(a[-1])
    pts = vtk.vtkPoints()
    for j in range(N):
      pts.InsertNextPoint(x[j],y[j],0.)
    geo,pts = project(pts,marker.projection,marker.worldcoordinate)
    markers.SetPoints(pts)

    #  Type
    ## Ok at this point generates the source for glpyh
    gs, pd = prepGlyph(g, marker, index=i)
    g.SetInputData(markers)

    a = vtk.vtkActor()
    m = vtk.vtkPolyDataMapper()
    m.SetInputConnection(g.GetOutputPort())
    m.Update()
    a.SetMapper(m)
    p = a.GetProperty()
    setMarkerColor(p, marker, c, cmap)
    actors.append((g, gs, pd, a, geo))

  return actors

def prepLine(renWin,line,cmap=None):
  number_lines = prepPrimitive(line)

  if number_lines == 0:
    return []

  actors = []

  for i in range(number_lines):
    l = vtk.vtkLine()
    lines = vtk.vtkCellArray()
    x = line.x[i]
    y = line.y[i]
    c = line.color[i]
    w = line.width[i]
    t = line.type[i]
    number_points = max(len(x),len(y))

    # Extend x or y to the length of the other by duplicating the last coord.
    for a in [x,y]:
      while len(a)<number_points:
        a.append(a[-1])

    pts = vtk.vtkPoints()

    if vcs.elements["projection"][line.projection].type=="linear":
        for j in range(number_points):
          pts.InsertNextPoint(x[j],y[j],0.)
        n2 = number_points - 1
    else:
        pts.InsertNextPoint(x[0],y[0],0.)
        n2=0
        for j in range(1,number_points):
            if vcs.elements["projection"][line.projection].type in round_projections:
                NPointsInterp = 50
            else:
                NPointsInterp = 25
            for i in range(1,NPointsInterp+1):
                if x[j]!=x[j-1]:
                    tmpx = x[j-1]+float(i)/NPointsInterp*(x[j]-x[j-1])
                else:
                    tmpx=x[j]
                if y[j]!=y[j-1]:
                    tmpy = y[j-1]+float(i)/NPointsInterp*(y[j]-y[j-1])
                else:
                    tmpy=y[j]
                pts.InsertNextPoint(tmpx,tmpy,0.)
                n2+=1
    for j in range(n2):
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
      raise Exception,"Unknown line type: '%s'" % t
    actors.append((a,geo))
  return actors

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
  ren.SetDisplayPoint(x,y,0)
  ren.DisplayToWorld()
  return wp

def vtkWorld2Renderer(ren,x,y):
  ren.SetWorldPoint(x,y,0,0)
  ren.WorldToDisplay()
  renpts = ren.GetDisplayPoint()
  return renpts

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

def starPoints(radius_outer, x, y, number_points = 5):
  """Defines coordinate points for an arbitrary star"""
  # Point at the top of the star
  theta = numpy.pi / 2.0

  # Angular distance from one point to the next
  delta_theta = 2 * numpy.pi / float(number_points)

  # Size of the interior circle
  radius_inner = radius_outer / 2.0

  points = []
  for point_index in range(number_points * 2):
    # Outer point
    dx = radius_outer * numpy.cos(theta)
    dy = radius_outer * numpy.sin(theta)

    points.append((x + dx, y + dy))

    # Inner point
    dx = radius_inner * numpy.cos(theta + delta_theta / 2.0)
    dy = radius_inner * numpy.sin(theta + delta_theta / 2.0)
    points.append((x + dx, y + dy))

    theta += delta_theta
  return points

def generateVectorArray(data1,data2,vtk_grid):
    u=numpy.ma.ravel(data1)
    v=numpy.ma.ravel(data2)
    sh = list(u.shape)
    sh.append(1)
    u = numpy.reshape(u,sh)
    v = numpy.reshape(v,sh)
    z = numpy.zeros(u.shape)
    w = numpy.concatenate((u,v),axis=1)
    w = numpy.concatenate((w,z),axis=1)

    # HACK The grid returned by vtk2vcs.genGrid is not the same size as the
    # data array. I'm not sure where the issue is...for now let's just zero-pad
    # data array so that we can at least test rendering until Charles gets
    # back from vacation:
    wLen = len(w)
    numPts = vtk_grid.GetNumberOfPoints()
    if wLen != numPts:
        warnings.warn("!!! Warning during vector plotting: Number of points does not "\
              "match the number of vectors to be glyphed (%s points vs %s "\
              "vectors). The vectors will be padded/truncated to match for "\
              "rendering purposes, but the resulting image should not be "\
              "trusted."%(numPts, wLen))
        newShape = (numPts,) + w.shape[1:]
        w = numpy.ma.resize(w, newShape)

    w = numpy_to_vtk_wrapper(w,deep=False)
    w.SetName("vectors")
    return w

def stripGrid(vtk_grid):
    # Strip out masked points.
    if vtk_grid.IsA("vtkStructuredGrid"):
        if vtk_grid.GetCellBlanking():
            visArray = vtk_grid.GetCellVisibilityArray()
            visArray.SetName("BlankingArray")
            vtk_grid.GetCellData().AddArray(visArray)
            thresh = vtk.vtkThreshold()
            thresh.SetInputData(vtk_grid)
            thresh.ThresholdByUpper(0.5)
            thresh.SetInputArrayToProcess(0, 0, 0,
                                          "vtkDataObject::FIELD_ASSOCIATION_CELLS",
                                          "BlankingArray")
            thresh.Update()
            vtk_grid = thresh.GetOutput()
        elif vtk_grid.GetPointBlanking():
            visArray = vtk_grid.GetPointVisibilityArray()
            visArray.SetName("BlankingArray")
            vtk_grid.GetPointData().AddArray(visArray)
            thresh = vtk.vtkThreshold()
            thresh.SetInputData(vtk_grid)
            thresh.SetUpperThreshold(0.5)
            thresh.SetInputArrayToProcess(0, 0, 0,
                                          "vtkDataObject::FIELD_ASSOCIATION_POINTS",
                                          "BlankingArray")
            thresh.Update()
            vtk_grid = thresh.GetOutput()
    return vtk_grid

def vtkIterate(iterator):
    iterator.InitTraversal()
    obj = iterator.GetNextItem()
    while obj is not None:
        yield obj
        obj = iterator.GetNextItem()
