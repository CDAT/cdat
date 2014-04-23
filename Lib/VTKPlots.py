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
  def plot(self,data1,data2,template,gtype,gname,bg,*args,**kargs):
    print "OK VTK RECEIVED:",data1.id, data2, template,gtype,gname
    print "OK VTK BG:",bg
    print "OK VTK RECEIVED ARG:",args
    print "OK VTK RECEIVED KARG:",kargs
    if self.renWin is None:
      # Create the usual rendering stuff.
      self.renWin = vtk.vtkRenderWindow()
      self.renWin.SetWindowName("VCS Canvas")
      if "bg" in kargs:
        self.bg= True
        self.renWin.SetOffScreenRendering(True)
      else:
        self.bg= False
        screenSize = self.renWin.GetScreenSize()
        self.renWin.SetSize(814,606)
    ren = vtk.vtkRenderer()
    ren.SetBackground(1,1,1)
    self.renWin.AddRenderer(ren)
    #screenSize = self.renWin.GetScreenSize()
    data1 = self.trimData(data1) # Ok get only the last 2 dims
    data2 = self.trimData(data2)
    gm = vcs.elements[gtype][gname]
    tpl = vcs.elements["template"][template]
    # ok for now let's assume it is 2D...
    if gtype in ["boxfill","meshfill","isofill","isoline"]:
      self.plot2D(data1,data2,tpl,gm,ren)
    else:
      raise Exception,"Graphic type: '%s' not re-implemented yet" % gtype
    
  def plot2D(self,data1,data2,tmpl,gm,ren):
    continents = False
    try: #First try to see if we can get a mesh out of this
      m=data1.getGrid().getMesh()
      N=m.shape[0]
      #For vtk we need to reorder things
      m2 = numpy.ascontiguousarray(numpy.transpose(m,(0,2,1)))
      m2.resize((m2.shape[0]*m2.shape[1],m2.shape[2]))
      m2=m2[...,::-1]
      # here we add dummy levels, might want to reconsider converting "trimData" to "reOrderData" and use actual levels?
      m3=numpy.concatenate((m2,numpy.zeros((m2.shape[0],1))),axis=1)
      continents = True
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
      else:
        data1=cdms2.asVariable(data1)
        #Ok no mesh info
        # first lat/lon case
        if data1.getLatitude() is not None and data1.getLongitude() is not None and data1.getAxis(-1).isLongitude():
          continents = True
        x=data1.getAxis(-1)
        y=data1.getAxis(-2)
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
      ren.AddActor(contActor)

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

    #Ok now we have grid and data let's use the mapper
    mapper = vtk.vtkDataSetMapper()
    if isinstance(gm,(isofill.Gfi,isoline.Gi)) or \
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


      ## Following assumes contiguous levels for now
      if gm.levels[0]==[0.,1.e20] or numpy.allclose(gm.levels,1.e20):
        mn,mx=vcs.minmax(data1)
        levs = vcs.mkscale(mn,mx)
        Nlevs = len(levs)
      else:
        Nlevs = len(gm.levels)
        if isinstance(gm.levels[0],(list,tuple)):
          if isinstance(gm,isoline.Gi):
            levs = [x[0] for x in gm.levels]
          else:
            raise Exception, "Cannot handle non contiguous levels yet: %s" % gm.levels
        else:
          levs=gm.levels
      ## Figure out colors
      if isinstance(gm,boxfill.Gfb):
        cols = gm.fillareacolors 
        if cols is None:
          cols = vcs.getcolors(levs)
      elif isinstance(gm,isofill.Gfi):
        cols = gm.fillareacolors
        print "COLORS FROM GM:",cols
        if cols==[1,]:
          cols = vcs.getcolors(levs)
      elif isinstance(gm,isoline.Gi):
        cols = gm.linecolors
        if cols==[1,]:
          cols = vcs.getcolors(levs)

      # make sure length match
      while len(cols)<len(levs):
        cols.append(cols[-1])
      
      ##Now sets up the colors


      cot.SetNumberOfContours(Nlevs+1)
      #At that point let's try to tweak color table
      lut = vtk.vtkLookupTable()
      lut.SetNumberOfTableValues(Nlevs)
      #lut.SetTableRange(0,Nlevs)
      try:
        cmap = vcs.elements["colormap"][cmap]
      except:
        cmap = self.canvas.getcolormap()

      if levs[0]==1.e20:
        levs[0]=-1.e20
      for i in range(Nlevs):
        cot.SetValue(i,levs[i])
        r,g,b = cmap.index[cols[i]]
        lut.SetTableValue(i,r/100.,g/100.,b/100.)
      cot.SetValue(Nlevs,levs[-1])
      cot.Update()
      mapper.SetInputConnection(cot.GetOutputPort())
      mapper.SetLookupTable(lut)
    else: #end isoline/isofill/boxfill-custom
      mapper.SetInputData(ug)
    mapper.SetScalarRange(levs[0],levs[-1])
    if self.debug:
      vcs2vtk.dump2VTK(cot,"cot")

    # And now we need actors to actually render this thing
    act = vtk.vtkActor()
    act.SetMapper(mapper)

    # Trying to do some positioning here
    ren.SetViewport(tmpl.data.x1,tmpl.data.y1,tmpl.data.x2,tmpl.data.y2)
    ren.AddActor(act)
    # Also need to make sure it fills the whole space
    if not numpy.allclose([gm.datawc_x1,gm.datawc_x2],1.e20):
      x1,x2 = gm.datawc_x1,gm.datawc_x2
    else:
      x1,x2 = xM,xm
    if not numpy.allclose([gm.datawc_y1,gm.datawc_y2],1.e20):
      y1,y2 = gm.datawc_y1,gm.datawc_y2
    else:
      y1,y2 = yM,ym

    
    self.renderTemplate(data1,tmpl,mapper)
    if not self.bg:
      self.renWin.Render()

  def renderTemplate(self,data1,tmpl,mapper):
    if tmpl.legend.priority>0:
      #Now let's have colorbar
      ## ??? different renderer for this one? so it doesn't zoo in/out
      clr = vtk.vtkScalarBarActor()
      clr.SetLookupTable(mapper.GetLookupTable())
      clr.SetTitle(data1.id)
      clr.GetPositionCoordinate().SetCoordinateSystemToNormalizedViewport()
      clr.GetPositionCoordinate().SetValue(.1,.01)
      clr.SetOrientationToHorizontal()
      clr.SetWidth(tmpl.data.x2-tmpl.data.x1)
      clr.SetHeight(tmpl.data.y2-tmpl.data.y2)
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

        imgfiltr = vtk.vtkWindowToImageFilter()
        imgfiltr.SetInput(self.renWin)
#        imgfiltr.SetMagnification(3)
        imgfiltr.SetInputBufferTypeToRGBA()
        imgfiltr.Update()
        writer = vtk.vtkPNGWriter()
        writer.SetInputConnection(imgfiltr.GetOutputPort())
        writer.SetFileName(file)
        writer.Write()

