import warnings
import vtk
import vcs
import vcs2vtk
import numpy
from vtk.util import numpy_support as VN
import meshfill,boxfill,isofill,isoline
import os
import cdms2

   
def smooth(x,beta,window_len=11):
   """ kaiser window smoothing """
   # extending the data at beginning and at the end
   # to apply the window at the borders
   s = numpy.r_[x[window_len-1:0:-1],x,x[-1:-window_len:-1]]
   w = numpy.kaiser(window_len,beta)
   y = numpy.convolve(w/w.sum(),s,mode='valid')
   return y[(window_len/2):-(window_len/2)]

class VTKVCSBackend(object):
  def __init__(self,canvas,renWin=None, debug=False,bg=None):
    self.canvas = canvas
    self.renWin = renWin
    self.debug = debug
    self.bg = bg
    self.type = "vtk"
    self._plot_keywords = []
    self.numberOfPlotCalls = 0 

  def clear(self):
    if self.renWin is None:
      return
    renderers = self.renWin.GetRenderers()
    ren = renderers.GetFirstRenderer()
    while ren is not None:
      ren.RemoveAllViewProps()
      ren.Render()
      self.renWin.RemoveRenderer(ren)
      ren = renderers.GetNextItem()
    self.renWin.Render()

  def createRenWin(self,*args,**kargs):
    if self.renWin is None:
      # Create the usual rendering stuff.
      self.renWin = vtk.vtkRenderWindow()
      self.renWin.SetWindowName("VCS Canvas")
      self.renWin.SetAlphaBitPlanes(1)
      ren = vtk.vtkRenderer()
      r,g,b = self.canvas.backgroundcolor
      ren.SetBackground(r/255.,g/255.,b/255.)
      print "BACKGROUND IS SET TO RGB"
      self.renWin.AddRenderer(ren)
      return True
    else:
      return False
    
  def initialSize(self):
      #screenSize = self.renWin.GetScreenSize()
      self.renWin.SetSize(814,606)

  def open(self):
    if self.createRenWin():
      self.initialSize()
    self.renWin.Render()

  def close(self):
    if self.renWin is None:
      return
    pass

  def plot(self,data1,data2,template,gtype,gname,bg,*args,**kargs):
    self.numberOfPlotCalls+=1
    created = self.createRenWin(**kargs)
    if self.bg is None:
      if bg:
        self.bg= True
        self.renWin.SetOffScreenRendering(True)
        self.renWin.SetSize(814,606)
      else:
        self.bg= False
        if created:
          self.initialSize()
    if kargs.get("renderer",None) is None:
        ren = vtk.vtkRenderer()
        ren.SetPreserveDepthBuffer(True)
    else:
      ren = kargs["renderer"]
    self.renWin.AddRenderer(ren)

    #screenSize = self.renWin.GetScreenSize()
    if gtype in ["boxfill","meshfill","isoline","isofill","vector"]:
      data1 = self.trimData2D(data1) # Ok get only the last 2 dims
      data2 = self.trimData2D(data2)
    #elif vcs.isgraphicsmethod(vcs.elements[gtype][gname]):
      ## oneD
    #  data1 = self.trimData1D(data1)
    #  data2 = self.trimData1D(data2)
    if gtype == "text":
      tt,to = gname.split(":::")
      tt = vcs.elements["texttable"][tt]
      to = vcs.elements["textorientation"][to]
    else:
      gm = vcs.elements[gtype][gname]
    tpl = vcs.elements["template"][template]
    # ok for now let's assume it is 2D...
    if gtype in ["boxfill","meshfill","isofill","isoline"]:
      self.plot2D(data1,data2,tpl,gm,ren)
    elif gtype in ["text"]:
      if tt.priority!=0:
        self.setLayer(ren,tt.priority)
        vcs2vtk.genTextActor(ren,to=to,tt=tt)
    elif gtype=="line":
      if gm.priority!=0:
        self.setLayer(ren,gm.priority)
        vcs2vtk.prepLine(self.renWin,ren,gm)
    elif gtype=="marker":
      if gm.priority!=0:
        self.setLayer(ren,gm.priority)
        vcs2vtk.prepMarker(self.renWin,ren,gm)
    elif gtype=="fillarea":
      if gm.priority!=0:
        self.setLayer(ren,gm.priority)
        vcs2vtk.prepFillarea(self.renWin,ren,gm)
    elif gtype=="oned":
      self.plot1D(data1,data2,tpl,gm,ren)
    else:
      raise Exception,"Graphic type: '%s' not re-implemented yet" % gtype
    self.renWin.Render()

  def plot1D(self,data1,data2,tmpl,gm,ren):
    self.setLayer(ren,tmpl.data.priority)
    Y = data1
    if data2 is None:
      X=Y.getAxis(0)[:]
    else:
      X=data2

    if gm.flip:
      tmp = Y
      Y = X
      X = tmp

    if gm.smooth is not None:
        Y = smooth(Y,gm.smooth)
    l = self.canvas.createline()
    l.x = X.tolist()
    l.y = Y.tolist()
    l.color=gm.linecolor
    l.width = gm.linewidth
    l.type = gm.line
    l.viewport = [tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2]
    # Also need to make sure it fills the whole space
    if not numpy.allclose([gm.datawc_x1,gm.datawc_x2],1.e20):
      x1,x2 = gm.datawc_x1,gm.datawc_x2
    else:
      x1,x2 = X.min(),X.max()
    if not numpy.allclose([gm.datawc_y1,gm.datawc_y2],1.e20):
      y1,y2 = gm.datawc_y1,gm.datawc_y2
    else:
      y1,y2 = Y.min(),Y.max()
    l.worldcoordinate = [x1,x2,y1,y2]

    m=self.canvas.createmarker()
    m.type = gm.marker
    m.color = gm.markercolor
    m.size = gm.markersize
    m.x = l.x
    m.y=l.y
    m.viewport=l.viewport
    m.worldcoordinate = l.worldcoordinate

    self.canvas.plot(l,renderer=ren)
    self.canvas.plot(m,renderer=ren)
    tmpl.plot(self.canvas,data1,gm,bg=self.bg,renderer=ren,X=X,Y=Y)
    
    legd = self.canvas.createline()
    legd.x = [tmpl.legend.x1, tmpl.legend.x2]
    legd.y = [tmpl.legend.y1, tmpl.legend.y1]
    legd.color = l.color
    legd.width = l.width
    legd.type  = l.type


    t=self.canvas.createtext(To_source=tmpl.legend.textorientation,Tt_source=tmpl.legend.texttable)
    t.x=tmpl.legend.x2
    t.y=tmpl.legend.y2
    t.string=data1.id
    self.canvas.plot(t,renderer=ren)
    self.canvas.plot(legd,renderer=ren)
    legd.list()
  
  def setLayer(self,renderer,priority):
    n = self.numberOfPlotCalls + (priority-1)*1000000 
    nMax = max(self.renWin.GetNumberOfLayers(),n+1)
    self.renWin.SetNumberOfLayers(nMax)
    renderer.SetLayer(n)
    pass

  def plot2D(self,data1,data2,tmpl,gm,ren):
    self.setLayer(ren,tmpl.data.priority)
    continents = False
    wrap = None
    try: #First try to see if we can get a mesh out of this
      g=data1.getGrid()
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
      else:
        data1=cdms2.asVariable(data1)
        #Ok no mesh info
        # first lat/lon case
        if data1.getLatitude() is not None and data1.getLongitude() is not None and data1.getAxis(-1).isLongitude():
          continents = True
          wrap = [0.,360.]
        x=data1.getAxis(-1)
        y=data1.getAxis(-2)
        xm=x.min()
        xM=x.max()
        ym=y.min()
        yM=y.max()
        # make it 2D
        x = x[numpy.newaxis,:]*numpy.ones(y.shape)[:,numpy.newaxis]
        y = y[:,numpy.newaxis]*numpy.ones(x.shape)[numpy.newaxis,:]
        z = numpy.zeros(x.shape)
        m3=numpy.concatenate((x,y,z),axis=1)



    #Create unstructured grid points
    ug = vtk.vtkUnstructuredGrid()

    # First create the points/vertices (in vcs terms)
    deep = False
    pts = vtk.vtkPoints()
    ## Convert nupmy array to vtk ones
    ppV = VN.numpy_to_vtk(m3,deep=deep)
    pts.SetData(ppV)

    projection = vcs.elements["projection"][gm.projection]
    geopts = vcs2vtk.project(pts,projection)
    ## Sets the vertics into the grid
    ug.SetPoints(geopts)

    #Now applies the actual data on each cell
    data = VN.numpy_to_vtk(data1.filled().flat,deep=True)

    for i in range(N):
      lst = vtk.vtkIdList()
      for j in range(4):
        lst.InsertNextId(i*4+j)
      ## ??? TODO ??? when 3D use CUBE?
      ug.InsertNextCell(vtk.VTK_QUAD,lst)

    ug.GetCellData().SetScalars(data)
    if self.debug:
      vcs2vtk.dump2VTK(ug)

    lut = vtk.vtkLookupTable()
    #lut.SetTableRange(0,Nlevs)
    ## Following assumes contiguous levels for now
    mn,mx=vcs.minmax(data1)
    #Ok now we have grid and data let's use the mapper
    mapper = vtk.vtkPolyDataMapper()
    if isinstance(gm,(isofill.Gfi,isoline.Gi,meshfill.Gfm)) or \
        (isinstance(gm,boxfill.Gfb) and gm.boxfill_type=="custom"):
      
      # Sets data to point instead of just cells
      c2p = vtk.vtkCellDataToPointData()
      c2p.SetInputData(ug)
      c2p.Update()
      if self.debug:
        vcs2vtk.dump2VTK(c2p)
      #For contouring duplicate points seem to confuse it
      cln = vtk.vtkCleanUnstructuredGrid()
      cln.SetInputConnection(c2p.GetOutputPort())
      if self.debug:
        vcs2vtk.dump2VTK(cln)
      #Now this filter seems to create the good polydata
      sFilter = vtk.vtkDataSetSurfaceFilter()
      sFilter.SetInputConnection(cln.GetOutputPort())
      sFilter.Update()
      if self.debug:
        vcs2vtk.dump2VTK(sFilter)
      if isinstance(gm,isoline.Gi):
        cot = vtk.vtkContourFilter()
      else:
        cot = vtk.vtkBandedPolyDataContourFilter()
      cot.SetInputData(sFilter.GetOutput())
      if self.debug:
        vcs2vtk.dump2VTK(cot)


      levs = gm.levels
      if (isinstance(gm,isoline.Gi) and numpy.allclose( levs[0],[0.,1.e20])) or numpy.allclose(levs,1.e20):
        levs = vcs.mkscale(mn,mx)
      else:
        if isinstance(gm.levels[0],(list,tuple)):
          if isinstance(gm,isoline.Gi):
            levs = [x[0] for x in gm.levels]
          else:
            raise Exception, "Cannot handle non contiguous levels yet: %s" % gm.levels
        else:
          levs=gm.levels
          if numpy.allclose(levs[0],1.e20):
            levs[0]=-1.e20
      Nlevs=len(levs)
      ## Figure out colors
      if isinstance(gm,boxfill.Gfb):
        cols = gm.fillareacolors 
        if cols is None:
          cols = vcs.getcolors(levs,split=0)
      elif isinstance(gm,isofill.Gfi):
        cols = gm.fillareacolors
        if cols==[1,]:
          cols = vcs.getcolors(levs,split=0)
      elif isinstance(gm,isoline.Gi):
        cols = gm.linecolors

      cot.SetNumberOfContours(Nlevs+1)
      if levs[0]==1.e20:
        levs[0]=-1.e20
      for i in range(Nlevs):
        cot.SetValue(i,levs[i])
      cot.SetValue(Nlevs,levs[-1])
      cot.Update()
      if self.debug:
        vcs2vtk.dump2VTK(cot,"cot")
      mapper.SetInputConnection(cot.GetOutputPort())
    else: #Boxfill/Meshfill
      geoFilter = vtk.vtkGeometryFilter()
      geoFilter.SetInputData(ug)
      geoFilter.Update()
      mapper.SetInputData(geoFilter.GetOutput())
      if isinstance(gm,boxfill.Gfb):
        if numpy.allclose(gm.level_1,1.e20) or numpy.allclose(gm.level_2,1.e20):
          levs = vcs.mkscale(mn,mx)
        else:
          levs = numpy.arange(gm.level_1,gm.level_2,(gm.level_2-gm.level_1)/(gm.color_2-gm.color_1+1))
        cols = vcs.getcolors(levs,range(gm.color_1,gm.color_2+1))
      else:
        if numpy.allclose(gm.levels,1.e20):
          levs = vcs.mkscale(mn,mx)
        else:
          levs = gm.levels
          if numpy.allclose(levs[0],1.e20):
            levs[0]=-1.e20
        cols = gm.fillareacolors
        if cols==[1,]:
          cols = vcs.getcolors(levs)
      Nlevs = len(levs)

    ## Colortable bit
    # make sure length match
    while len(cols)<Nlevs:
      cols.append(cols[-1])
    
    try:
      cmap = vcs.elements["colormap"][cmap]
    except:
      cmap = vcs.elements["colormap"][self.canvas.getcolormapname()]
    lut.SetNumberOfTableValues(Nlevs)
    for i in range(Nlevs):
      r,g,b = cmap.index[cols[i]]
      lut.SetTableValue(i,r/100.,g/100.,b/100.)

    mapper.SetLookupTable(lut)
    mapper.SetScalarRange(levs[0],levs[-1])

    # And now we need actors to actually render this thing
    act = vtk.vtkActor()
    act.SetMapper(mapper)

    # Also need to make sure it fills the whole space
    if not numpy.allclose([gm.datawc_x1,gm.datawc_x2],1.e20):
      x1,x2 = gm.datawc_x1,gm.datawc_x2
    else:
      x1,x2 = xm,xM
    if not numpy.allclose([gm.datawc_y1,gm.datawc_y2],1.e20):
      y1,y2 = gm.datawc_y1,gm.datawc_y2
    else:
      y1,y2 = ym,yM

    act = vcs2vtk.doWrap(act,[x1,x2,y1,y2],wrap)
    if tmpl.data.priority != 0:
      ren.AddActor(act)
      self.renWin.Render()
      tmp = vcs2vtk.fitToViewport(act,ren,[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2],[x1,x2,y1,y2])
      ren.RemoveActor(act)
      ren.AddActor(tmp)
    
    self.renderTemplate(ren,tmpl,data1,gm)
    if isinstance(gm,(isofill.Gfi,meshfill.Gfm,boxfill.Gfb)):
      self.renderColorBar(ren,tmpl,levs,cols)
    if continents:
      contData = vcs2vtk.prepContinents(os.environ["HOME"]+"/.uvcdat/data_continent_political")
      contMapper = vtk.vtkPolyDataMapper()
      contMapper.SetInputData(contData)
      contActor = vtk.vtkActor()
      contActor.SetMapper(contMapper)
      contActor.GetProperty().SetColor(0.,0.,0.)
      cpts = contData.GetPoints()
      gcpts = vcs2vtk.project(cpts,projection)
      contData.SetPoints(gcpts)
      contActor = vcs2vtk.doWrap(contActor,[x1,x2,y1,y2],wrap)
      tmp = vcs2vtk.fitToViewport(contActor,ren,[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2],[x1,x2,y1,y2])
      if tmpl.data.priority!=0:
        ren.AddActor(tmp)

  def renderTemplate(self,renderer,tmpl,data,gm):
    tmpl.plot(self.canvas,data,gm,bg=self.bg,renderer=renderer)

  def renderColorBar(self,renderer,tmpl,levels,colors):
    if tmpl.legend.priority>0:
      tmpl.drawColorBar(colors,levels,x=self.canvas,renderer=renderer)

  def trimData1D(self,data):
    if data is None:
      return None
    while len(data.shape)>1:
      data = data[0]
    return data

  #ok now trying to figure the actual data to plot
  def trimData2D(self,data):
    if data is None:
      return None
    try:
      g=data.getGrid()
      gaxes=list(g.getAxisList())
      daxes=list(data.getAxisList())
      if daxes[len(daxes)-len(gaxes):] == gaxes:
        # Ok it is gridded and the grid axes are last
        return data(*(slice(0,1),)*(len(daxes)-len(gaxes)))
      else:
        # Ok just return the last two dims
        return data(*(slice(0,1),)*(len(daxes)-2))
    except Exception,err: # ok no grid info
      print "Got exception",err
      daxes=list(data.getAxisList())
      if cdms2.isVariable(data):
        return data(*(slice(0,1),)*(len(daxes)-2))
      else: #numpy arrays are not callable
        op = ()
        for i in range(numpy.rank(data)-2):
          op.append(slice(0,1))
        return data[op]

  def png(self, file, width=None,height=None,units=None,draw_white_background = 0):
        
        if self.renWin is None:
          raise Exception,"Nothing to dump aborting"
            
        if not file.split('.')[-1].lower() in ['png']:
            file+='.png'

        try:
          os.remove(file)
        except:
          pass

        if width is not None and height is not None:
          self.renWin.SetSize(width,height)
          self.renWin.Render()
        imgfiltr = vtk.vtkWindowToImageFilter()
        imgfiltr.SetInput(self.renWin)
#        imgfiltr.SetMagnification(3)
        imgfiltr.SetInputBufferTypeToRGBA()
        imgfiltr.Update()
        writer = vtk.vtkPNGWriter()
        writer.SetInputConnection(imgfiltr.GetOutputPort())
        writer.SetFileName(file)
        writer.Write()

  def cgm(self,file):
        if self.renWin is None:
          raise Exception,"Nothing to dump aborting"
            
        if not file.split('.')[-1].lower() in ['cgm']:
            file+='.cgm'

        try:
          os.remove(file)
        except:
          pass

        writer = vtk.vtkIOCGM.vtkCGMWriter()
        writer.SetFileName(file)
        R = self.renWin.GetRenderers()
        r=R.GetFirstRenderer()
        A = r.GetActors()
        A.InitTraversal()
        a = A.GetNextActor()
        while a is not None:
          m = a.GetMapper()
          m.Update()
          writer.SetInputData(m.GetInput())
          writer.Write()
          a=A.GetNextActor()


