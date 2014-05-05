import warnings
import vtk
import vcs
import vcs2vtk
import numpy
from vtk.util import numpy_support as VN
import meshfill,boxfill,isofill,isoline
import os

class VTKVCSBackend(object):
  def __init__(self,canvas):
    self.canvas = canvas
    self.renWin = None
    self.debug = True
    self.bg = None
    self.type = "vtk"
    self._plot_keywords = []

  def plot(self,data1,data2,template,gtype,gname,bg,*args,**kargs):
    #print "OK VTK RECEIVED:",template,gtype,gname
    #print "OK VTK BG:",bg
    #print "OK VTK RECEIVED ARG:",args
    #print "OK VTK RECEIVED KARG:",kargs
    if self.renWin is None:
      # Create the usual rendering stuff.
      self.renWin = vtk.vtkRenderWindow()
      self.renWin.SetWindowName("VCS Canvas")
    if self.bg is None:
      if bg:
        self.bg= True
        self.renWin.SetOffScreenRendering(True)
        self.renWin.SetSize(814,606)
      else:
        self.bg= False
        screenSize = self.renWin.GetScreenSize()
        self.renWin.SetSize(814,606)
    if kargs.get("renderer",None) is None:
      print "NEW RENDERER!!!!!"
      ren = vtk.vtkRenderer()
      ren.SetBackground(1,1,1)
    else:
      ren = kargs["renderer"]
    self.renWin.AddRenderer(ren)

    #screenSize = self.renWin.GetScreenSize()
    data1 = self.trimData(data1) # Ok get only the last 2 dims
    data2 = self.trimData(data2)
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
      vcs2vtk.genTextActor(ren,to=to,tt=tt)
    elif gtype=="line":
      warnings.warn("Please implement line prmary objects in vtk backend")
    else:
      raise Exception,"Graphic type: '%s' not re-implemented yet" % gtype
    
  def plot2D(self,data1,data2,tmpl,gm,ren):
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

    if continents:
        contData = vcs2vtk.continentsVCS2VTK(os.environ["HOME"]+"/.uvcdat/data_continent_political")
        contMapper = vtk.vtkPolyDataMapper()
        contMapper.SetInputData(contData)
        contActor = vtk.vtkActor()
        contActor.SetMapper(contMapper)
        contActor.GetProperty().SetColor(0.,0.,0.)


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
    if continents:
      cpts = contData.GetPoints()
      gcpts = vcs2vtk.project(cpts,projection)
      contData.SetPoints(gcpts)
      contActor = vcs2vtk.doWrap(contActor,gm,wrap)
      ren.AddActor(contActor)
      self.renWin.Render()
      tmp = vcs2vtk.fitToViewport(contActor,ren,[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2])
      ren.RemoveActor(contActor)
      ren.AddActor(tmp)

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
    mapper = vtk.vtkDataSetMapper()
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
        print "Autogen:",levs
        levs = vcs.mkscale(mn,mx)
      else:
        if isinstance(gm.levels[0],(list,tuple)):
          if isinstance(gm,isoline.Gi):
            levs = [x[0] for x in gm.levels]
          else:
            raise Exception, "Cannot handle non contiguous levels yet: %s" % gm.levels
        else:
          levs=gm.levels
          print "Came back with:",levs
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
      c2p = vtk.vtkCellDataToPointData()
      c2p.SetInputData(ug)
      c2p.Update()
      sFilter = vtk.vtkDataSetSurfaceFilter()
      sFilter.SetInputDataObject(ug)
      mapper.SetInputData(sFilter.GetOutput())
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
      print "COLORMAP:",self.canvas.getcolormapname()
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
      x1,x2 = xM,xm
    if not numpy.allclose([gm.datawc_y1,gm.datawc_y2],1.e20):
      y1,y2 = gm.datawc_y1,gm.datawc_y2
    else:
      y1,y2 = yM,ym


    act = vcs2vtk.doWrap(act,gm,wrap)
    ren.AddActor(act)
    self.renWin.Render()
    tmp = vcs2vtk.fitToViewport(act,ren,[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2])
    ren.RemoveActor(act)
    ren.AddActor(tmp)
    
    self.renderTemplate(ren,tmpl,data1,gm)
    self.renderColorBar(ren,mapper,tmpl,data1)

    self.renWin.Render()

  def renderTemplate(self,ren,tmpl,data,gm):
    tmpl.plot(self.canvas,data,gm,bg=self.bg,renderer=ren)
  def renderColorBar(self,ren,mapper,tmpl,data):
    if tmpl.legend.priority>0:
      #Now let's have colorbar
      clr = vtk.vtkScalarBarActor()
      lut = mapper.GetLookupTable()
      n = lut.GetNumberOfTableValues()
      clr.SetNumberOfLabels(n)
      clr.SetLookupTable(lut)
      clr.SetTitle("")
      clr.GetPositionCoordinate().SetCoordinateSystemToNormalizedViewport()
      clr.SetOrientationToHorizontal()
      p = clr.GetLabelTextProperty()
      vcs2vtk.prepTextProperty(p,to=tmpl.legend.textorientation,tt=tmpl.legend.texttable,cmap="default")
      X = tmpl.legend.x1
      Y=tmpl.legend.y1
      clr.SetPosition(X,Y)
      print clr.GetPosition()
      w = tmpl.legend.x2-tmpl.legend.x1
      h = tmpl.legend.y2-tmpl.legend.y1
      #h = 1.
      #w=1.
      clr.SetWidth(w)
      clr.SetHeight(1.8*h)
      print clr.GetBounds()
      ren.AddActor(clr)
      self.renWin.Render()
    pass


  #ok now trying to figure the actual data to plot
  def trimData(self,data):
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
    except: # ok no grid info
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


