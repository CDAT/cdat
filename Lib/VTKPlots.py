import animate_helper
import warnings
import vtk
import vcs
import vcs2vtk
import numpy
from vtk.util import numpy_support as VN
import meshfill,boxfill,isofill,isoline
import os, traceback, sys
import cdms2
import DV3D
import MV2
import cdtime
import inspect
import VTKAnimate

def smooth(x,beta,window_len=11):
   """ kaiser window smoothing """
   # extending the data at beginning and at the end
   # to apply the window at the borders
   s = numpy.r_[x[window_len-1:0:-1],x,x[-1:-window_len:-1]]
   w = numpy.kaiser(window_len,beta)
   y = numpy.convolve(w/w.sum(),s,mode='valid')
   return y[(window_len/2):-(window_len/2)]

class VCSInteractorStyle(vtk.vtkInteractorStyleUser):

  def __init__(self,parent):
      self.AddObserver("LeftButtonPressEvent", parent.leftButtonPressEvent )
      self.AddObserver("LeftButtonReleaseEvent", parent.leftButtonReleaseEvent )
      self.AddObserver( "ModifiedEvent", parent.configureEvent )
      self.AddObserver( "ConfigureEvent", parent.configureEvent )
      if sys.platform == "darwin":
          self.AddObserver( "RenderEvent", parent.renderEvent )

class VTKVCSBackend(object):
  def __init__(self,canvas,renWin=None, debug=False,bg=None):
    self._lastSize = None
    self.canvas = canvas
    self.renWin = renWin
    self.debug = debug
    self.bg = bg
    self.type = "vtk"
    self.plotApps = {}
    self.plotRenderers = set()
    self.logoRenderer = None
    self.logoRepresentation = None
    self.renderer = None
    self._renderers = {}
    self._plot_keywords = [ 'renderer','vtk_backend_grid','vtk_backend_geo', 'cdmsfile', 'cell_coordinates' ]
    self.numberOfPlotCalls = 0
    self.renderWindowSize=None
    self.clickRenderer = None

    if renWin is not None:
      self.renWin = renWin
      if renWin.GetInteractor() is None and self.bg is False:
        self.createDefaultInteractor()

    if sys.platform == "darwin":
        self.reRender = False
        self.oldCursor = None


  def setAnimationStepper( self, stepper ):
      for plot in self.plotApps.values():
        plot.setAnimationStepper( stepper )

  def interact(self,*args,**kargs):
      if self.renWin is None:
          warnings.warn("Cannot interact if you did not open the canvas yet")
          return
      interactor = self.renWin.GetInteractor()
      ## Mac seems to handle events a bit differently
      ## Need to add observers on renWin
      ## Linux is fine w/o it so no need to do it
      if sys.platform == "darwin":
          self.renWin.AddObserver( "RenderEvent", self.renderEvent )
          self.renWin.AddObserver("LeftButtonPressEvent", self.leftButtonPressEvent )
          self.renWin.AddObserver("LeftButtonReleaseEvent", self.leftButtonReleaseEvent )
          self.renWin.AddObserver( "ModifiedEvent", self.configureEvent )
          self.renWin.AddObserver( "ConfigureEvent", self.configureEvent )
          self.renWin.AddObserver( "EndEvent",self.endEvent)
      if interactor is None:
          warnings.warn("Cannot start interaction. Blank plot?")
          return
      warnings.warn("Press 'Q' to exit interactive mode and continue script execution")
      interactor.Start()

  def endEvent(self,obj,event):
    if self.renWin is not None:
      if self.reRender:
        self.reRender = False
        #self._lastSize = None
        self.renWin.Render()

  def renderEvent(self,caller,evt):
    renwin = self.renWin if (caller == None) else caller
    window_size = renwin.GetSize()
    if ( window_size <> self.renderWindowSize ):
      self.configureEvent(caller,evt)
      self.renderWindowSize = window_size

  def leftButtonPressEvent(self,obj,event):
    xy = self.renWin.GetInteractor().GetEventPosition()
    sz = self.renWin.GetSize()
    x = float(xy[0])/sz[0]
    y = float(xy[1])/sz[1]
    st = ""
    for dnm in self.canvas.display_names:
      d=vcs.elements["display"][dnm]
      if d.array[0] is None:
        continue
      t=vcs.elements["template"][d.template]
      gm = vcs.elements[d.g_type][d.g_name]
      if t.data.x1 <= x <= t.data.x2 and t.data.y1 <= y <= t.data.y2:
        ## Ok we clicked within template
        if numpy.allclose(gm.datawc_x1,1.e20):
          x1 = d.array[0].getAxis(-1)[0]
        else:
          x1 = gm.datawc_x1
        if numpy.allclose(gm.datawc_x2,1.e20):
          x2 = d.array[0].getAxis(-1)[-1]
        else:
          x2 = gm.datawc_x2
        if numpy.allclose(gm.datawc_y1,1.e20):
          y1 = d.array[0].getAxis(-2)[0]
        else:
          y1 = gm.datawc_y1
        if numpy.allclose(gm.datawc_y2,1.e20):
          y2 = d.array[0].getAxis(-2)[-1]
        else:
          y2 = gm.datawc_y2

        X = (x-t.data.x1)/(t.data.x2-t.data.x1)*(x2-x1)+x1
        Y = (y-t.data.y1)/(t.data.y2-t.data.y1)*(y2-y1)+y1
        # Ok we now have the X/Y values we need to figure out the indices
        try:
            I = d.array[0].getAxis(-1).mapInterval((X,X,'cob'))[0]
            try:
                J = d.array[0].getAxis(-2).mapInterval((Y,Y,'cob'))[0]
                # Values at that point
                V = d.array[0][...,J,I]
            except:
                V = d.array[0][...,I]
            if isinstance(V,numpy.ndarray):
              V=V.flat[0]
            try:
                st+="Var: %s\nX[%i] = %g\nY[%i] = %g\nValue: %g" % (d.array[0].id,I,X,J,Y,V)
            except:
                st+="Var: %s\nX = %g\nY[%i] = %g\nValue: %g" % (d.array[0].id,X,I,Y,V)
        except:
            st+="Var: %s\nX=%g\nY=%g\nValue = N/A" % (d.array[0].id,X,Y)
    if st == "":
        return
    ren = vtk.vtkRenderer()
    ren.SetBackground(.96,.96,.86)
    ren.SetViewport(x,y,min(x+.2,1.),min(y+.2,1))
    ren.SetLayer(self.renWin.GetNumberOfLayers()-1)
    a=vtk.vtkTextActor()
    a.SetInput(st)
    p=a.GetProperty()
    p.SetColor(0,0,0)
    bb = [0,0,0,0]
    a.GetBoundingBox(ren,bb)
    ps=vtk.vtkPlaneSource()
    ps.SetCenter(bb[0],bb[2],0.)
    ps.SetPoint1(bb[1],bb[2],0.)
    ps.SetPoint2(bb[0],bb[3],0.)
    ps.Update()
    m2d=vtk.vtkPolyDataMapper2D()
    m2d.SetInputConnection(ps.GetOutputPort())
    a2d=vtk.vtkActor2D()
    a2d.SetMapper(m2d)
    a2d.GetProperty().SetColor(.93,.91,.67)
    ren.AddActor(a2d)
    ren.AddActor(a)
    ren.ResetCamera()
    self.clickRenderer = ren
    self.renWin.AddRenderer(ren)
    self.renWin.Render()

  def leftButtonReleaseEvent(self,obj,event):
    if self.clickRenderer is not None:
      self.clickRenderer.RemoveAllViewProps()
      self.renWin.RemoveRenderer(self.clickRenderer)
      self.renWin.Render()
      self.clickRenderer = None

  def configureEvent(self,obj,ev):
    cursor = self.renWin.GetCurrentCursor()
    if sys.platform == "darwin" and ev == "ModifiedEvent" and cursor != self.oldCursor:
      self.oldCursor = cursor
      return

    if self.get3DPlot() is not None:
        return

    sz = self.renWin.GetSize()
    if self._lastSize == sz:
      # We really only care about resize event
      # this is mainly to avoid segfault vwith Vistraisl which does
      # not catch configure Events but only modifiedEvents....
      return

    self._lastSize = sz
    plots_args = []
    key_args =[]

    for dnm in self.canvas.display_names:
      d=vcs.elements["display"][dnm]
      parg = []
      for a in d.array:
        if a is not None:
          parg.append(a)
      parg.append(d._template_origin)
      parg.append(d.g_type)
      parg.append(d.g_name)
      plots_args.append(parg)
      if d.ratio is not None:
          key_args.append({"ratio":d.ratio})
      else:
          key_args.append({})

    # Have to pull out the UI layer so it doesn't get borked by the clear
    self.hideGUI()

    self.canvas.clear(render=False)

    for i, pargs in enumerate(plots_args):
      self.canvas.plot(*pargs, render = False, **key_args[i])

    if self.canvas.animate.created() and self.canvas.animate.frame_num != 0:
      self.canvas.animate.draw_frame(allow_static = False, render_offscreen=False)

    self.showGUI()

    if self.renWin.GetSize()!=(0,0):
      self.scaleLogo()

    self.renWin.Render()

  def clear(self, render=True):
    if self.renWin is None: #Nothing to clear
          return
    renderers = self.renWin.GetRenderers()
    renderers.InitTraversal()
    ren = renderers.GetNextItem()
    hasValidRenderer = True if ren is not None else False

    for gm in self.plotApps:
      app = self.plotApps[gm]
      app.plot.quit()

    self.hideGUI()
    while ren is not None:
        ren.RemoveAllViewProps()
        if not ren.GetLayer()==0:
          self.renWin.RemoveRenderer(ren)
        else:
          #Update background color
          r,g,b = [c / 255. for c in self.canvas.backgroundcolor]
          ren.SetBackground(r,g,b)
        ren = renderers.GetNextItem()
    self.showGUI()
    if hasValidRenderer and self.renWin.IsDrawable() and render:
        self.renWin.Render()
    self.numberOfPlotCalls = 0
    self.logoRenderer = None
    self.createLogo()
    self._renderers = {}

  def createDefaultInteractor( self, ren=None ):
    defaultInteractor = self.renWin.GetInteractor()
    if defaultInteractor is None:
      defaultInteractor = vtk.vtkRenderWindowInteractor()
    self.vcsInteractorStyle = VCSInteractorStyle(self)
    if ren:
      self.vcsInteractorStyle.SetCurrentRenderer( ren )
    defaultInteractor.SetInteractorStyle( self.vcsInteractorStyle )
    defaultInteractor.SetRenderWindow(self.renWin)
    self.vcsInteractorStyle.On()

  def createRenWin(self,*args,**kargs):
    if self.renWin is None:
      # Create the usual rendering stuff.
      self.renWin = vtk.vtkRenderWindow()
      self.renWin.SetWindowName("VCS Canvas %i" % self.canvas._canvas_id)
      self.renWin.SetAlphaBitPlanes(1)
      ## turning on Stencil for Labels on iso plots
      self.renWin.SetStencilCapable(1)
      ## turning off antialiasing by default
      ## mostly so that pngs are same accross platforms
      self.renWin.SetMultiSamples(0)
      self.initialSize()

    if self.renderer == None:
      self.renderer = self.createRenderer()
      if self.bg is False:
          self.createDefaultInteractor(self.renderer)
      self.renWin.AddRenderer(self.renderer)

  def createRenderer(self, *args, **kargs):
      # For now always use the canvas background
      ren = vtk.vtkRenderer()
      r,g,b = self.canvas.backgroundcolor
      ren.SetBackground(r/255., g/255., b/255.)
      return ren

  def update(self, *args, **kargs):
    self._lastSize=-1
    if self.renWin:
      if self.get3DPlot():
        plots_args = []
        key_args =[]
        for dnm in self.canvas.display_names:
          d=vcs.elements["display"][dnm]
          parg = []
          for a in d.array:
            if a is not None:
              parg.append(a)
          parg.append(d._template_origin)
          parg.append(d.g_type)
          parg.append(d.g_name)
          plots_args.append(parg)
          if d.ratio is not None:
              key_args.append({"ratio":d.ratio})
          else:
              key_args.append({})
        for i, args in enumerate(plots_args):
            self.canvas.plot(*args, **key_args[i])
      else:
        self.configureEvent(None,None)

  def canvasinfo(self):
    if self.renWin is None:
      mapstate = False
      height = self.canvas.bgY
      width = self.canvas.bgX
      depth = None
      x=0
      y=0
    else:
      try: #mac but not linux
        mapstate = self.renWin.GetWindowCreated()
      except:
        mapstate = True
      width, height = self.renWin.GetSize()
      depth=self.renWin.GetDepthBufferSize()
      try: #mac not linux
        x,y = self.renWin.GetPosition()
      except:
        x,y = 0,0
    info = {
        "mapstate":mapstate,
        "height":height,
        "width":width,
        "depth":depth,
        "x":x,
        "y":y,
        }
    return info

  def orientation(self,*args,**kargs):
    if self.renWin is None:
      return "landscape"
    w,h = self.renWin.GetSize()
    if w>h:
      return "landscape"
    else:
      return "portrait"

  def portrait(self,W,H,x,y,clear):
      if clear:
          self.clear()
      if self.renWin is None:
          if W!=-99:
              self.canvas.bgX = W
              self.canvas.bgY = H
          else:
              W = self.canvas.bgX
              self.canvas.bgX = self.canvas.bgY
              self.canvas.bgY = W
      else:
          self.renWin.SetSize(W,H)

  def initialSize(self):
      #screenSize = self.renWin.GetScreenSize()
      self.renWin.SetSize(self.canvas.bgX,self.canvas.bgY)
      self._lastSize = (self.canvas.bgX, self.canvas.bgY)

  def open(self):
    self.createRenWin( open=True )

  def close(self):
    if self.renWin is None:
      return
    self.clear()
    self.renWin.Finalize()
    self.renWin = None

  def geometry(self,x,y,*args):
      self.renWin.SetSize(x,y)

  def flush(self):
      if self.renWin is not None:
          self.renWin.Render()

  def plot(self,data1,data2,template,gtype,gname,bg,*args,**kargs):
    self.numberOfPlotCalls+=1
    ## these are keyargs that can be reused later by the backend.
    returned = {}
    if self.bg is None:
      if bg:
        self.bg= True
      else:
        self.bg= False
    self.createRenWin(**kargs)
    if self.bg:
        self.renWin.SetOffScreenRendering(True)
        self.renWin.SetSize(self.canvas.bgX,self.canvas.bgY)
    self.cell_coordinates=kargs.get( 'cell_coordinates', None )
    self.canvas.initLogoDrawing()
    if gtype == "text":
      tt,to = gname.split(":::")
      tt = vcs.elements["texttable"][tt]
      to = vcs.elements["textorientation"][to]
      gm=tt
    else:
      gm = vcs.elements[gtype][gname]
    tpl = vcs.elements["template"][template]

    if kargs.get("renderer",None) is None:
        if ( gtype in ["3d_scalar", "3d_dual_scalar", "3d_vector"] ) and (self.renderer <> None):
            ren = self.renderer
        else:
            #ren = self.createRenderer()
            #if not (vcs.issecondaryobject(gm) and gm.priority==0):
            #    self.setLayer(ren,tpl.data.priority)
            #    self.renderer = ren
            #    self.renWin.AddRenderer(ren)
            pass
        #ren.SetPreserveDepthBuffer(True)
    else:
      ren = kargs["renderer"]

    vtk_backend_grid = kargs.get("vtk_backend_grid",None)
    vtk_backend_geo = kargs.get("vtk_backend_geo",None)
    if gtype in ["boxfill","meshfill","isofill","isoline"]:
      returned.update(self.plot2D(data1,data2,tpl,gm,vtk_backend_grid=vtk_backend_grid,vtk_backend_geo=vtk_backend_geo))
    elif gtype in ["3d_scalar", "3d_dual_scalar", "3d_vector"]:
      cdms_file = kargs.get( 'cdmsfile', None )
      cdms_var = kargs.get( 'cdmsvar', None )
      if not cdms_var is None:
          raise Exception()
      if not cdms_file is None:
          gm.addPlotAttribute( 'file', cdms_file )
          gm.addPlotAttribute( 'filename', cdms_file )
          gm.addPlotAttribute( 'url', cdms_file )
      returned.update(self.plot3D(data1,data2,tpl,gm,ren,**kargs))
    elif gtype in ["text"]:
      if tt.priority!=0:
        #if not (None,None,None) in self._renderers.keys():
        ren = self.createRenderer()
        self.renWin.AddRenderer(ren)
        self.setLayer(ren,1)
        #    self._renderers[(None,None,None)]=ren
        #else:
        #    ren = self._renderers[(None,None,None)]
        returned["vtk_backend_text_actors"] = vcs2vtk.genTextActor(ren,to=to,tt=tt)
        self.setLayer(ren,tt.priority)
    elif gtype=="line":
      if gm.priority!=0:
        actors = vcs2vtk.prepLine(self.renWin,gm)
        returned["vtk_backend_line_actors"]=actors
        for act,geo in actors:
            ren = self.fitToViewport(act,gm.viewport,wc=gm.worldcoordinate,geo=geo,priority=gm.priority)
    elif gtype=="marker":
      if gm.priority!=0:
        actors = vcs2vtk.prepMarker(self.renWin,gm)
        returned["vtk_backend_marker_actors"]=actors
        for g,gs,pd,act,geo in actors:
            ren = self.fitToViewport(act,gm.viewport,wc=gm.worldcoordinate,geo=geo,priority=gm.priority)
            if pd is None and act.GetUserTransform():
              vcs2vtk.scaleMarkerGlyph(g, gs, pd, act)

    elif gtype=="fillarea":
      if gm.priority!=0:
        actors = vcs2vtk.prepFillarea(self.renWin,gm)
        returned["vtk_backend_fillarea_actors"]=actors
        for act,geo in actors:
            ren = self.fitToViewport(act,gm.viewport,wc=gm.worldcoordinate,geo=geo,priority=gm.priority)
    elif gtype=="1d":
      #self.renWin.AddRenderer(ren)
      returned.update(self.plot1D(data1,data2,tpl,gm))
    elif gtype=="vector":
      returned.update(self.plotVector(data1,data2,tpl,gm,vtk_backend_grid=vtk_backend_grid,vtk_backend_geo=vtk_backend_geo))
    else:
      raise Exception,"Graphic type: '%s' not re-implemented yet" % gtype
    self.scaleLogo()
    if not kargs.get("donotstoredisplay",False) and kargs.get("render", True):
      self.renWin.Render()
    return returned


  def plot1D(self,data1,data2,tmpl,gm):
    Y = self.trimData1D(data1)
    if data2 is None:
      X=Y.getAxis(0)
    else:
      X=Y
      data1._yname = data2.id
      Y=self.trimData1D(data2)

    if gm.flip:
      tmp = Y
      Y = X
      X = tmp

    if gm.smooth is not None:
        Y = smooth(Y,gm.smooth)
    l = self.canvas.createline()
    Xs = X[:].tolist()
    Ys = Y[:].tolist()
    xs = []
    ys = []
    prev = None
    for i,v in enumerate(Ys):
        if v is not None and Xs[i] is not None: # Valid data
            if prev is None:
                prev=[]
                prev2 = []
            prev.append(Xs[i])
            prev2.append(v)
        else:
            if prev is not None:
                xs.append(prev)
                ys.append(prev2)
                prev = None
    if prev is not None:
        xs.append(prev)
        ys.append(prev2)
    l._x = xs
    l._y = ys
    l.color=gm.linecolor
    if gm.linewidth>0:
        l.width = gm.linewidth
    else:
        l.priority=0
    l.type = gm.line
    l._viewport = [tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2]
    # Also need to make sure it fills the whole space
    x1,x2,y1,y2 = vcs.utils.getworldcoordinates(gm,X,Y)
    if numpy.allclose(y1,y2):
        y1-=.0001
        y2+=.0001
    if numpy.allclose(x1,x2):
        x1-=.0001
        x2+=.0001
    l._worldcoordinate = [x1,x2,y1,y2]
    if gm.marker is not None:
        m=self.canvas.createmarker()
        m.type = gm.marker
        m.color = gm.markercolor
        if gm.markersize>0:
            m.size = gm.markersize
        else:
            m.priority=0
        m._x = l.x
        m._y = l.y
        m._viewport=l.viewport
        m._worldcoordinate = l.worldcoordinate

    if not (Y[:].min()>max(y1,y2) or Y[:].max()<min(y1,y2) \
            or X[:].min()>max(x1,x2) or X[:].max()<min(x1,x2)):
    	if l.priority>0:
            self.canvas.plot(l,donotstoredisplay=True)
        if gm.marker is not None and m.priority>0:
            self.canvas.plot(m,donotstoredisplay=True)
    ren2 = self.createRenderer()
    self.renWin.AddRenderer(ren2)
    tmpl.plot(self.canvas,data1,gm,bg=self.bg,renderer=ren2,X=X,Y=Y)
    if hasattr(data1,"_yname"):
      del(data1._yname)
    del(vcs.elements["line"][l.name])
    if gm.marker is not None:
        del(vcs.elements["marker"][m.name])

    if tmpl.legend.priority>0:
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
        self.canvas.plot(t,donotstoredisplay=True)
        sp = t.name.split(":::")
        del(vcs.elements["texttable"][sp[0]])
        del(vcs.elements["textorientation"][sp[1]])
        del(vcs.elements["textcombined"][t.name])
        self.canvas.plot(legd,donotstoredisplay=True)
        del(vcs.elements["line"][legd.name])
    return {}

  def setLayer(self,renderer,priority):
    n = self.numberOfPlotCalls + (priority-1)*200 + 1
    nMax = max(self.renWin.GetNumberOfLayers(),n+1)
    self.renWin.SetNumberOfLayers(nMax)
    renderer.SetLayer(n)

  def plot3D(self,data1,data2,tmpl,gm,ren,**kargs):
      from DV3D.Application import DV3DApp
      requiresFileVariable = True
      self.canvas.drawLogo = False
      if ( data1 is None ) or ( requiresFileVariable and not ( isinstance(data1, cdms2.fvariable.FileVariable ) or isinstance(data1, cdms2.tvariable.TransientVariable ) ) ):
          traceback.print_stack()
          raise Exception, "Error, must pass a cdms2 variable object as the first input to the dv3d gm ( found '%s')" % ( data1.__class__.__name__ )
      g = self.plotApps.get( gm, None )
      if g == None:
          g = DV3DApp( self.canvas, self.cell_coordinates )
          n_overview_points = 500000
          grid_coords = ( None, None, None, None )
          var_proc_op = None
          interface = None
          roi = None # ( 0, 0, 50, 50 )
          g.gminit( data1, data2, roi=roi, axes=gm.axes, n_overview_points=n_overview_points, n_cores=gm.NumCores, renwin=ren.GetRenderWindow(), plot_attributes=gm.getPlotAttributes(), gmname=gm.g_name, cm=gm.cfgManager, **kargs  ) #, plot_type = PlotType.List  )
          self.plotApps[ gm ] = g
          self.plotRenderers.add( g.plot.renderer )
      else:
          g.update( tmpl )
      return {}

  def onClosing( self, cell ):
      for plot in self.plotApps.values():
          if hasattr( plot, 'onClosing' ):
              plot.onClosing( cell )

  def plotVector(self,data1,data2,tmpl,gm,vtk_backend_grid=None,vtk_backend_geo=None):
    #Preserve time and z axis for plotting these inof in rendertemplate
    returned = {}
    taxis = data1.getTime()
    if data1.ndim>2:
        zaxis = data1.getAxis(-3)
    else:
        zaxis = None
    data1 = self.trimData2D(data1) # Ok get3 only the last 2 dims
    data2 = self.trimData2D(data2)
    gridGenDict = vcs2vtk.genGridOnPoints(data1,gm,deep=False,grid=vtk_backend_grid,geo=vtk_backend_geo)
    for k in ['vtk_backend_grid','xm','xM','ym','yM','continents','wrap','geo']:
        exec("%s = gridGenDict['%s']" % (k,k))
    returned["vtk_backend_grid"]=vtk_backend_grid
    returned["vtk_backend_geo"]=geo
    missingMapper = vcs2vtk.putMaskOnVTKGrid(data1,vtk_backend_grid,None,False,deep=False)
    #None/False are for color and cellData (sent to vcs2vtk.putMaskOnVTKGrid)
    returned["vtk_backend_missing_mapper"]=missingMapper,None,False

    w=vcs2vtk.generateVectorArray(data1,data2,vtk_backend_grid)

    vtk_backend_grid.GetPointData().AddArray(w)

    ## Vector attempt
    l = gm.line
    if l is None:
        l = "default"
    try:
      l = vcs.getline(l)
      lwidth = l.width[0]
      lcolor = l.color[0]
      lstyle = l.type[0]
    except:
      lstyle = "solid"
      lwidth = 1.
      lcolor = 0
    if gm.linewidth is not None:
        lwidth = gm.linewidth
    if gm.linecolor is not None:
        lcolor = gm.linecolor

    vtk_backend_grid = vcs2vtk.stripGrid(vtk_backend_grid)

    arrow = vtk.vtkGlyphSource2D()
    arrow.SetGlyphTypeToArrow()
    arrow.FilledOff()

    glyphFilter = vtk.vtkGlyph2D()
    glyphFilter.SetSourceConnection(arrow.GetOutputPort())
    glyphFilter.SetVectorModeToUseVector()

    # Rotate arrows to match vector data:
    glyphFilter.OrientOn()

    # Scale to vector magnitude:
    glyphFilter.SetScaleModeToScaleByVector()

    # These are some unfortunately named methods. It does *not* clamp the scale
    # range to [min, max], but rather remaps the range [min, max]-->[0,1]. Bump
    # up min so that near-zero vectors will not be rendered, as these tend to
    # come out randomly oriented.
    glyphFilter.ClampingOn()
    glyphFilter.SetRange(0.01, 1.0)

    glyphFilter.SetInputArrayToProcess(1,0,0,0,"vectors")
    glyphFilter.SetScaleFactor(2.*gm.scale)

    glyphFilter.SetInputData(vtk_backend_grid)

    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInputConnection(glyphFilter.GetOutputPort())
    act = vtk.vtkActor()
    act.SetMapper(mapper)
    try:
      cmap = vcs.elements["colormap"][cmap]
    except:
      cmap = vcs.elements["colormap"][self.canvas.getcolormapname()]
    r,g,b = cmap.index[lcolor]
    act.GetProperty().SetColor(r/100.,g/100.,b/100.)
    x1,x2,y1,y2 = vcs.utils.getworldcoordinates(gm,data1.getAxis(-1),data1.getAxis(-2))
    act = vcs2vtk.doWrap(act,[x1,x2,y1,y2],wrap)
    ren = self.fitToViewport(act,[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2],[x1,x2,y1,y2],priority=tmpl.data.priority)
    returned.update(self.renderTemplate(tmpl,data1,gm,taxis,zaxis))
    if self.canvas._continents is None:
      continents = False
    if continents:
        projection = vcs.elements["projection"][gm.projection]
        self.plotContinents(x1,x2,y1,y2,projection,wrap,tmpl)
    returned["vtk_backend_actors"] = [[act,[x1,x2,y1,y2]],]
    returned["vtk_backend_glyphfilters"]=[glyphFilter,]
    returned["vtk_backend_luts"]=[[None,None],]
    return returned

  def plot2D(self,data1,data2,tmpl,gm,vtk_backend_grid=None,vtk_backend_geo=None):
    #Preserve time and z axis for plotting these inof in rendertemplate
    returned = {}
    t = data1.getTime()
    if data1.ndim>2:
        z = data1.getAxis(-3)
    else:
        z = None
    wmn,wmx = vcs.minmax(data1)
    data1 = self.trimData2D(data1) # Ok get3 only the last 2 dims
    if gm.g_name!="Gfm":
      data2 = self.trimData2D(data2)
    if isinstance(gm,(vcs.isofill.Gfi,vcs.isoline.Gi)):
        gridGenDict = vcs2vtk.genGridOnPoints(data1,gm,deep=False,grid=vtk_backend_grid,geo=vtk_backend_geo)
        gridGenDict["cellData"]=False
    else:
        gridGenDict = vcs2vtk.genGrid(data1,data2,gm,deep=False,grid=vtk_backend_grid,geo=vtk_backend_geo)
    for k in ['vtk_backend_grid','xm','xM','ym','yM','continents','wrap','geo','cellData']:
        exec("%s = gridGenDict['%s']" % (k,k))
    returned["vtk_backend_grid"]=vtk_backend_grid
    returned["vtk_backend_geo"]=geo
    returned["vtk_backend_wrap"]=wrap
    #Now applies the actual data on each cell
    if isinstance(gm,boxfill.Gfb) and gm.boxfill_type=="log10":
        data1=numpy.ma.log10(data1)
    data = vcs2vtk.numpy_to_vtk_wrapper(data1.filled(0.).flat, deep=False)
    if cellData:
        vtk_backend_grid.GetCellData().SetScalars(data)
    else:
        vtk_backend_grid.GetPointData().SetScalars(data)

    try:
      cmap = vcs.elements["colormap"][cmap]
    except:
      cmap = vcs.elements["colormap"][self.canvas.getcolormapname()]

    color = getattr(gm,"missing",None)
    if color is not None:
        color = cmap.index[color]
    missingMapper = vcs2vtk.putMaskOnVTKGrid(data1,vtk_backend_grid,color,cellData,deep=False)
    returned["vtk_backend_missing_mapper"]=missingMapper,color,cellData
    lut = vtk.vtkLookupTable()
    mn,mx=vcs.minmax(data1)
    #Ok now we have grid and data let's use the mapper
    mapper = vtk.vtkPolyDataMapper()
    legend = None
    if isinstance(gm,(meshfill.Gfm,boxfill.Gfb)):
      geoFilter = vtk.vtkDataSetSurfaceFilter()
      if cellData:
          p2c = vtk.vtkPointDataToCellData()
          p2c.SetInputData(vtk_backend_grid)
          geoFilter.SetInputConnection(p2c.GetOutputPort())
      else:
        geoFilter.SetInputData(vtk_backend_grid)
      geoFilter.Update()

    if isinstance(gm,(isofill.Gfi,isoline.Gi,meshfill.Gfm)) or \
        (isinstance(gm,boxfill.Gfb) and gm.boxfill_type=="custom"):

      #Now this filter seems to create the good polydata
      sFilter = vtk.vtkDataSetSurfaceFilter()
      if cellData:
          # Sets data to point instead of just cells
          c2p = vtk.vtkCellDataToPointData()
          c2p.SetInputData(vtk_backend_grid)
          c2p.Update()
          #For contouring duplicate points seem to confuse it
          if vtk_backend_grid.IsA("vtkUntructuredGrid"):
              cln = vtk.vtkCleanUnstructuredGrid()
              cln.SetInputConnection(c2p.GetOutputPort())
              sFilter.SetInputConnection(cln.GetOutputPort())
          else:
              sFilter.SetInputConnection(c2p.GetOutputPort())
      else:
          sFilter.SetInputData(vtk_backend_grid)
      sFilter.Update()
      returned["vtk_backend_filter"]=sFilter
      if isinstance(gm,isoline.Gi):
        cot = vtk.vtkContourFilter()
        if cellData:
          cot.SetInputData(sFilter.GetOutput())
        else:
          cot.SetInputData(vtk_backend_grid)

      levs = gm.levels
      ## Apparently in some cases
      if numpy.allclose( levs[0],[0.,1.e20]) or numpy.allclose(levs,1.e20):
        if isinstance(gm,isoline.Gi):
            levs = vcs.mkscale(mn,mx)
            if len(levs)==1: # constant value ?
              levs = [levs[0],levs[0]+.00001]
            Ncolors = len(levs)
        else:
          levs2 = vcs.mkscale(mn,mx)
          if len(levs2)==1: # constant value ?
            levs2 = [levs2[0],levs2[0]+.00001]
          levs=[]
          for i in range(len(levs2)-1):
            levs.append([levs2[i],levs2[i+1]])
      else:
        if isinstance(gm.levels[0],(list,tuple)):
          if isinstance(gm,isoline.Gi):
            levs = [x[0] for x in gm.levels]
          else:
            levs = gm.levels
        else:
          levs = []
          levs2=gm.levels
          if numpy.allclose(levs2[0],1.e20):
            levs2[0]=-1.e20
          for i in range(len(levs2)-1):
            levs.append([levs2[i],levs2[i+1]])
          if isinstance(gm,isoline.Gi):
            levs = levs2
      print "LEVELS:",levs
      print "LEVELS:",levs2
      Nlevs=len(levs)
      Ncolors = Nlevs
      ## Figure out colors
      if isinstance(gm,boxfill.Gfb):
        cols = gm.fillareacolors
        if cols is None:
          cols = vcs.getcolors(levs2,split=0)
      elif isinstance(gm,(isofill.Gfi,meshfill.Gfm)):
        cols = gm.fillareacolors
        if cols==[1,]:
          cols = vcs.getcolors(levs2,split=0)
          if isinstance(cols,(int,float)):
              cols=[cols,]
      elif isinstance(gm,isoline.Gi):
        cols = gm.linecolors

      if isinstance(gm, isoline.Gi):
        cot.SetNumberOfContours(Nlevs)
        if levs[0]==1.e20:
          levs[0]=-1.e20
        for i in range(Nlevs):
          cot.SetValue(i,levs[i])
        cot.SetValue(Nlevs,levs[-1])
        cot.Update()
        mappers = []
        if gm.label=="y":
            mapper = vtk.vtkLabeledContourMapper()
        else:
            mapper = vtk.vtkPolyDataMapper()
        lut = vtk.vtkLookupTable()
        lut.SetNumberOfTableValues(1)
        lut.SetTableValue(0, 0, 0, 0)
        if gm.label=="y":
            mapper.GetPolyDataMapper().SetLookupTable(lut)
            mapper.SetLabelVisibility(1)
        else:
            mapper.SetLookupTable(lut)

        # Create text properties.
        if gm.label=="y":
         if gm.text or gm.textcolors:
          colorOverrides = gm.textcolors if gm.textcolors else [None] * len(gm.text)
          texts = gm.text if gm.text else [None] * len(gm.textcolors)
          while len(texts)<Nlevs:
              texts.append(texts[-1])
          while len(colorOverrides)<Nlevs:
              colorOverrides.append(colorOverrides[-1])
          tprops = vtk.vtkTextPropertyCollection()
          for tc,colorOverride in zip(texts, colorOverrides):
              if vcs.queries.istextcombined(tc):
                  tt,to = tuple(tc.name.split(":::"))
              elif tc is None:
                  tt="default"
                  to="default"
              elif vcs.queries.istexttable(tc):
                  tt=tc.name
                  to="default"
              elif vcs.queries.istextorientation(tc):
                  to=tc.name
                  tt="default"
              if colorOverride is not None:
                  tt=vcs.createtexttable(None,tt)
                  tt.color = colorOverride
                  tt=tt.name
              tprop = vtk.vtkTextProperty()
              vcs2vtk.prepTextProperty(tprop, self.renWin.GetSize(), to, tt)
              tprops.AddItem(tprop)
              if colorOverride is not None:
                  del(vcs.elements["texttable"][tt])

              mapper.SetTextProperties(tprops)
         else:
            # No text properties specified. Use the default:
            tprop = vtk.vtkTextProperty()
            vcs2vtk.prepTextProperty(tprop, self.renWin.GetSize())
            mapper.SetTextProperty(tprop)

        stripper = vtk.vtkStripper()
        stripper.SetInputConnection(cot.GetOutputPort())
        mapper.SetInputConnection(stripper.GetOutputPort())
        stripper.Update()
        mappers.append([mapper,])
        returned["vtk_backend_contours"]=[cot,]
      else:
        mappers = []
        LEVS = []
        INDX = []
        COLS = []
        indices = gm.fillareaindices
        if indices is None:
            indices=[1,]
        while len(indices)<len(cols):
            indices.append(indices[-1])
        print "levs:",levs
        print "cols:",cols
        if len(levs)>len(cols):
            raise RuntimeError("You asked for %i levels but provided only %i colors\n\
            Graphic Method: %s of type %s" % (len(levs),len(cols),gm.name,gm.g_name))
        elif len(levs)<len(cols)-1:
            warnings.warn("You asked for %i levels but provided %i colors, extra ones will be ignored\n\
            Graphic Method: %s of type %s" % (len(levs),len(cols),gm.name,gm.g_name))
        for i,l in enumerate(levs):
            if i==0:
                C = [cols[i],]
                if numpy.allclose(levs[0][0],-1.e20):
                    ## ok it's an extension arrow
                    L=[mn-1.,levs[0][1]]
                else:
                    L = list(levs[i])
                I = [indices[i],]
            else:
                if l[0] == L[-1] and I[-1]==indices[i]:
                    # Ok same type lets keep going
                    if numpy.allclose(l[1],1.e20):
                        L.append(mx+1.)
                    else:
                        L.append(l[1])
                    C.append(cols[i])
                else: # ok we need new contouring
                    LEVS.append(L)
                    COLS.append(C)
                    INDX.append(I)
                    C = [cols[i],]
                    L = levs[i]
                    I = [indices[i],]
        LEVS.append(L)
        COLS.append(C)
        INDX.append(I)


        luts=[]
        cots=[]
        geos =[]
        for i,l in enumerate(LEVS):
          # Ok here we are trying to group together levels can be, a join will happen if:
          # next set of levels contnues where one left off AND pattern is identical

          if isinstance(gm,isofill.Gfi):
              mapper = vtk.vtkPolyDataMapper()
              lut = vtk.vtkLookupTable()
              cot = vtk.vtkBandedPolyDataContourFilter()
              cot.ClippingOn()
              cot.SetInputData(sFilter.GetOutput())
              cot.SetNumberOfContours(len(l))
              cot.SetClipTolerance(0.)
              for j,v in enumerate(l):
                cot.SetValue(j,v)
              #cot.SetScalarModeToIndex()
              cot.Update()
              cots.append(cot)
              mapper.SetInputConnection(cot.GetOutputPort())
              lut.SetNumberOfTableValues(len(COLS[i]))
              for j,color in enumerate(COLS[i]):
                  r,g,b = cmap.index[color]
                  lut.SetTableValue(j,r/100.,g/100.,b/100.)
              luts.append([lut,[0,len(l)-1,True]])
              mapper.SetLookupTable(lut)
              mapper.SetScalarRange(0,len(l)-1)
              mapper.SetScalarModeToUseCellData()
              mappers.append([mapper,])
          else:
              for j,color in enumerate(COLS[i]):
                  mapper = vtk.vtkPolyDataMapper()
                  lut = vtk.vtkLookupTable()
                  th = vtk.vtkThreshold()
                  th.ThresholdBetween(l[j],l[j+1])
                  th.SetInputConnection(geoFilter.GetOutputPort())
                  geoFilter2 = vtk.vtkDataSetSurfaceFilter()
                  geoFilter2.SetInputConnection(th.GetOutputPort())
                  geos.append(geoFilter2)
                  mapper.SetInputConnection(geoFilter2.GetOutputPort())
                  lut.SetNumberOfTableValues(1)
                  r,g,b = cmap.index[color]
                  lut.SetTableValue(0,r/100.,g/100.,b/100.)
                  mapper.SetLookupTable(lut)
                  mapper.SetScalarRange(l[j],l[j+1])
                  luts.append([lut,[l[j],l[j+1],False]])
                  ## Store the mapper only if it's worth it?
                  ## Need to do it with the whole slab min/max for animation purposes
                  if not(l[j+1]<wmn or l[j]>wmx):
                      mappers.append([mapper,])


          #png = vtk.vtkPNGReader()
          #png.SetFileName("/git/uvcdat/Packages/vcs/Share/uvcdat_texture.png")
          #T=vtk.vtkTexture()
          #T.SetInputConnection(png.GetOutputPort())
          #if isinstance(gm,isofill.Gfi):
          #    mappers.append([mapper,])
        returned["vtk_backend_luts"]=luts
        if len(cots)>0:
           returned["vtk_backend_contours"]=cots
        if len(geos)>0:
           returned["vtk_backend_geofilters"]=geos

    else: #Boxfill (non custom)/Meshfill
      if isinstance(gm,boxfill.Gfb):
        if numpy.allclose(gm.level_1,1.e20) or numpy.allclose(gm.level_2,1.e20):
          levs = vcs.mkscale(mn,mx)
          if len(levs)==1: # constant value ?
              levs = [levs[0],levs[0]+.00001]
          legend = vcs.mklabels(levs)
          dx = (levs[-1]-levs[0])/(gm.color_2-gm.color_1+1)
          levs = numpy.arange(levs[0],levs[-1]+dx,dx)
        else:
          if gm.boxfill_type=="log10":
              levslbls = vcs.mkscale(numpy.ma.log10(gm.level_1),numpy.ma.log10(gm.level_2))
              levs = vcs.mkevenlevels(numpy.ma.log10(gm.level_1),
                      numpy.ma.log10(gm.level_2),
                      nlev=(gm.color_2-gm.color_1)+1)
          else:
              levslbls = vcs.mkscale(gm.level_1,gm.level_2)
              levs = vcs.mkevenlevels(gm.level_1,gm.level_2,nlev=(gm.color_2-gm.color_1)+1)
          if len(levs)>25:
              ## Too many colors/levels need to prettyfy this for legend
              legend = vcs.mklabels(levslbls)
              ## Make sure extremes are in
              legd2=vcs.mklabels([levs[0],levs[-1]])
              legend.update(legd2)
          else:
              legend = vcs.mklabels(levs)
          if gm.boxfill_type=="log10":
              for k in legend.keys():
                  legend[float(numpy.ma.log10(legend[k]))] = legend[k]
                  del(legend[k])

        cols = range(gm.color_1,gm.color_2+1)
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
      Ncolors = Nlevs-1
      #Prep mapper
      mappers=[]
      mapper = vtk.vtkPolyDataMapper()
      thr = vtk.vtkThreshold()
      thr.SetInputConnection(geoFilter.GetOutputPort())
      if not gm.ext_1 in ["y",1,True]  and not gm.ext_2 in ["y",1,True] :
          thr.ThresholdBetween(levs[0],levs[-1])
      elif gm.ext_1 in ["y",1,True]  and not gm.ext_2 in ["y",1,True] :
          thr.ThresholdByLower(levs[-1])
      elif not gm.ext_1 in ["y",1,True]  and gm.ext_2 in ["y",1,True] :
          thr.ThresholdByUpper(levs[0])
      thr.Update()
      geoFilter2 = vtk.vtkDataSetSurfaceFilter()
      geoFilter2.SetInputConnection(thr.GetOutputPort())
      if gm.ext_1 in ["y",1,True]  and gm.ext_2 in ["y",1,True] :
          mapper.SetInputConnection(geoFilter.GetOutputPort())
          returned["vtk_backend_geofilters"]=[geoFilter,]
      else:
          mapper.SetInputConnection(geoFilter2.GetOutputPort())
          returned["vtk_backend_geofilters"]=[geoFilter2,]

    if mappers == []: # ok didn't need to have special banded contours
      mappers=[mapper,]
      ## Colortable bit
      # make sure length match
      while len(cols)<Ncolors:
        cols.append(cols[-1])

      lut.SetNumberOfTableValues(Ncolors)
      for i in range(Ncolors):
        r,g,b = cmap.index[cols[i]]
        lut.SetTableValue(i,r/100.,g/100.,b/100.)

      mapper.SetLookupTable(lut)
      if numpy.allclose(levs[0],-1.e20):
        lmn = mn-1.
      else:
        lmn= levs[0]
      if numpy.allclose(levs[-1],1.e20):
        lmx = mx+1.
      else:
        lmx= levs[-1]
      mapper.SetScalarRange(lmn,lmx)
      returned["vtk_backend_luts"]=[[lut,[lmn,lmx,False]],]

    if missingMapper is not None:
      if isinstance(gm,meshfill.Gfm):
        mappers.append(missingMapper)
      else:
        mappers.insert(0,missingMapper)

    if isinstance(gm,meshfill.Gfm):
        x1,x2,y1,y2 = vcs2vtk.getRange(gm,xm,xM,ym,yM)
    else:
        x1,x2,y1,y2 = vcs.utils.getworldcoordinates(gm,data1.getAxis(-1),data1.getAxis(-2))

    # Add a second mapper for wireframe meshfill:
    if isinstance(gm, meshfill.Gfm) and gm.mesh:
      lineMappers = []
      wireLUT = vtk.vtkLookupTable()
      wireLUT.SetNumberOfTableValues(1)
      wireLUT.SetTableValue(0,0,0,0)
      for polyMapper in mappers:
        if isinstance(polyMapper, list):
            polyMapper = polyMapper[0]
        lineMapper = vtk.vtkPolyDataMapper()
        lineMapper.SetInputConnection(polyMapper.GetInputConnection(0, 0))
        #lineMapper.SetInputConnection(geoFilter.GetOutputPort())
        lineMapper._useWireFrame = True

        # Setup depth resolution so lines stay above points:
        polyMapper.SetResolveCoincidentTopologyPolygonOffsetParameters(0, 1)
        polyMapper.SetResolveCoincidentTopologyToPolygonOffset()
        lineMapper.SetResolveCoincidentTopologyPolygonOffsetParameters(1, 1)
        lineMapper.SetResolveCoincidentTopologyToPolygonOffset()
        lineMapper.SetLookupTable(wireLUT)

        lineMappers.append(lineMapper)
      mappers.extend(lineMappers)

    if tmpl.data.priority != 0:
      # And now we need actors to actually render this thing
      actors = []
      for mapper in mappers:
        act = vtk.vtkActor()
        if isinstance(mapper,list):
          act.SetMapper(mapper[0])
        else:
          mapper.Update()
          act.SetMapper(mapper)
          if hasattr(mapper, "_useWireFrame"):
            prop = act.GetProperty()
            # Makes wireframed
            prop.SetRepresentationToWireframe()
        if geo is None:
          #If using geofilter on wireframed does not get wrppaed not sure why so sticking to many mappers
          act = vcs2vtk.doWrap(act,[x1,x2,y1,y2],wrap)
        if mapper is missingMapper:
            actors.append([act,missingMapper,[x1,x2,y1,y2]])
        else:
            actors.append([act,[x1,x2,y1,y2]])
        if isinstance(mapper,list):
          ## This is the sport to add patterns
          #act.GetMapper().ScalarVisibilityOff()
          #act.SetTexture(mapper[1])
          pass
        # create a new renderer for this mapper
        # (we need one for each mapper because of cmaera flips)
        ren = self.fitToViewport(act,[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2],
                wc=[x1,x2,y1,y2],geo=geo,priority=tmpl.data.priority)
      returned["vtk_backend_actors"] = actors

    if isinstance(gm,meshfill.Gfm):
      tmpl.plot(self.canvas,data1,gm,
                bg=self.bg,
                X=numpy.arange(xm,xM*1.1,(xM-xm)/10.),
                Y=numpy.arange(ym,yM*1.1,(yM-ym)/10.))
    else:
      returned.update(self.renderTemplate(tmpl,data1,gm,t,z))
    if isinstance(gm,(isofill.Gfi,meshfill.Gfm,boxfill.Gfb)):
      if getattr(gm,"legend",None) is not None:
        legend = gm.legend
      if gm.ext_1 in ["y",1,True] and not numpy.allclose(levs[0],-1.e20):
          if isinstance(levs,numpy.ndarray):
              levs=levs.tolist()
          if not (isinstance(levs[0],list) and numpy.less_equal(levs[0][0],-1.e20)):
            levs.insert(0,-1.e20)
      if gm.ext_2 in ["y",1,True] and not numpy.allclose(levs[-1],1.e20):
          if isinstance(levs,numpy.ndarray):
              levs=levs.tolist()
          if not (isinstance(levs[-1],list) and numpy.greater_equal(levs[-1][-1],1.e20)):
            levs.append(1.e20)

      returned.update(self.renderColorBar(tmpl,levs,cols,legend,cmap))
    if self.canvas._continents is None:
      continents = False
    if continents:
        projection = vcs.elements["projection"][gm.projection]
        self.plotContinents(x1,x2,y1,y2,projection,wrap,tmpl)
    return returned

  def plotContinents(self,x1,x2,y1,y2,projection,wrap,tmpl):
      contData = vcs2vtk.prepContinents(self.canvas._continents)
      contMapper = vtk.vtkPolyDataMapper()
      contMapper.SetInputData(contData)
      contActor = vtk.vtkActor()
      contActor.SetMapper(contMapper)
      contActor.GetProperty().SetColor(0.,0.,0.)
      contActor = vcs2vtk.doWrap(contActor,[x1,x2,y1,y2],wrap,fastClip=False)
      if projection.type!="linear":
          contData=contActor.GetMapper().GetInput()
          cpts = contData.GetPoints()
          geo, gcpts = vcs2vtk.project(cpts,projection,[x1,x2,y1,y2])
          contData.SetPoints(gcpts)
          contMapper = vtk.vtkPolyDataMapper()
          contMapper.SetInputData(contData)
          contActor = vtk.vtkActor()
          contActor.SetMapper(contMapper)
          contActor.GetProperty().SetColor(0.,0.,0.)
      else:
          geo=None

      ren = self.fitToViewport(contActor,[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2],
              wc=[x1,x2,y1,y2],geo=geo,priority=tmpl.data.priority)
      return {}

  def renderTemplate(self,tmpl,data,gm,taxis,zaxis):
    ## ok first basic template stuff, let's store the displays
    ## because we need to return actors for min/max/mean
    displays = tmpl.plot(self.canvas,data,gm,bg=self.bg)
    returned = {}
    for d in displays:
        if d is None:
          continue
        texts=d.backend.get("vtk_backend_text_actors",[])
        for t in texts:
          ## ok we had a text actor, let's see if it's min/max/mean
          txt = t.GetInput()
          s0=txt.split()[0]
          if s0 in ["Min","Max","Mean"]:
              returned["vtk_backend_%s_text_actor" % s0] = t
          else:
              returned["vtk_backend_%s_text_actor" % d.backend["vtk_backend_template_attribute"]] = t
        self.canvas.display_names.remove(d.name)
        del(vcs.elements["display"][d.name])
    if taxis is not None:
      try:
        tstr = str(cdtime.reltime(taxis[0],taxis.units).tocomp(taxis.getCalendar()))
        #ok we have a time axis let's display the time
        crdate = vcs2vtk.applyAttributesFromVCStmpl(tmpl,"crdate")
        crdate.string = tstr.split()[0].replace("-","/")
        crtime = vcs2vtk.applyAttributesFromVCStmpl(tmpl,"crtime")
        crtime.string = tstr.split()[1]
        if not (None,None,None) in self._renderers.keys():
            ren = self.createRenderer()
            self.renWin.AddRenderer(ren)
            self.setLayer(ren,1)
            self._renderers[(None,None,None)]=ren
        else:
            ren = self._renderers[(None,None,None)]
        tt,to = crdate.name.split(":::")
        tt = vcs.elements["texttable"][tt]
        to = vcs.elements["textorientation"][to]
        if crdate.priority>0:
            actors = vcs2vtk.genTextActor(ren,to=to,tt=tt)
            returned["vtk_backend_crdate_text_actor"]=actors[0]
        del(vcs.elements["texttable"][tt.name])
        del(vcs.elements["textorientation"][to.name])
        del(vcs.elements["textcombined"][crdate.name])
        tt,to = crtime.name.split(":::")
        tt = vcs.elements["texttable"][tt]
        to = vcs.elements["textorientation"][to]
        if crtime.priority>0:
            actors = vcs2vtk.genTextActor(ren,to=to,tt=tt)
            returned["vtk_backend_crtime_text_actor"]=actors[0]
        del(vcs.elements["texttable"][tt.name])
        del(vcs.elements["textorientation"][to.name])
        del(vcs.elements["textcombined"][crtime.name])
      except:
          pass
    if zaxis is not None:
      try:
        # ok we have a zaxis to draw
        zname = vcs2vtk.applyAttributesFromVCStmpl(tmpl,"zname")
        zname.string=zaxis.id
        zvalue = vcs2vtk.applyAttributesFromVCStmpl(tmpl,"zvalue")
        if zaxis.isTime():
            zvalue.string = str(zaxis.asComponentTime()[0])
        else:
            zvalue.string= "%g" % zaxis[0]
        if not (None,None,None) in self._renderers.keys():
            ren = self.createRenderer()
            self.renWin.AddRenderer(ren)
            self.setLayer(ren,1)
            self._renderers[(None,None,None)]=ren
        else:
            ren = self._renderers[(None,None,None)]
        tt,to = zname.name.split(":::")
        tt = vcs.elements["texttable"][tt]
        to = vcs.elements["textorientation"][to]
        if zname.priority>0:
            vcs2vtk.genTextActor(ren,to=to,tt=tt)
        del(vcs.elements["texttable"][tt.name])
        del(vcs.elements["textorientation"][to.name])
        del(vcs.elements["textcombined"][zname.name])
        if hasattr(zaxis,"units"):
            zunits = vcs2vtk.applyAttributesFromVCStmpl(tmpl,"zunits")
            zunits.string=zaxis.units
            if zunits.priority>0:
                tt,to = zunits.name.split(":::")
                tt = vcs.elements["texttable"][tt]
                to = vcs.elements["textorientation"][to]
                vcs2vtk.genTextActor(ren,to=to,tt=tt)
                del(vcs.elements["texttable"][tt.name])
                del(vcs.elements["textorientation"][to.name])
                del(vcs.elements["textcombined"][zunits.name])
        tt,to = zvalue.name.split(":::")
        tt = vcs.elements["texttable"][tt]
        to = vcs.elements["textorientation"][to]
        if zvalue.priority>0:
            actors = vcs2vtk.genTextActor(ren,to=to,tt=tt)
            returned["vtk_backend_zvalue_text_actor"]=actors[0]
        del(vcs.elements["texttable"][tt.name])
        del(vcs.elements["textorientation"][to.name])
        del(vcs.elements["textcombined"][zvalue.name])
      except:
          pass
    return returned


  def renderColorBar(self,tmpl,levels,colors,legend,cmap):
    if tmpl.legend.priority>0:
      tmpl.drawColorBar(colors,levels,x=self.canvas,legend=legend,cmap=cmap)
    return {}

  def cleanupData(self,data):
      data[:] = numpy.ma.masked_invalid(data,numpy.nan)
      return data

  def trimData1D(self,data):
    if data is None:
      return None
    while len(data.shape)>1:
      data = data[0]
    return self.cleanupData(data)

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
        return self.cleanupData(data(*(slice(0,1),)*(len(daxes)-len(gaxes)),squeeze=1))
      else:
        # Ok just return the last two dims
        return self.cleanupData(data(*(slice(0,1),)*(len(daxes)-2),squeeze=1))
    except Exception,err: # ok no grid info
      daxes=list(data.getAxisList())
      if cdms2.isVariable(data):
        return self.cleanupData( data(*(slice(0,1),)*(len(daxes)-2)))
      else: #numpy arrays are not callable
        op = ()
        for i in range(numpy.rank(data)-2):
          op.append(slice(0,1))
        return self.cleanupData(data[op])

  def put_png_on_canvas(self,filename,zoom=1,xOffset=0,yOffset=0,*args,**kargs):
      return self.put_img_on_canvas(filename,zoom,xOffset,yOffset,*args,**kargs)

  def put_img_on_canvas(self,filename,zoom=1,xOffset=0,yOffset=0,*args,**kargs):
    self.hideGUI()
    readerFactory = vtk.vtkImageReader2Factory()
    reader = readerFactory.CreateImageReader2(filename)
    reader.SetFileName(filename)
    reader.Update()
    imageData = reader.GetOutput()
    a = vtk.vtkImageActor()
    a.GetMapper().SetInputConnection(reader.GetOutputPort())
    origin = imageData.GetOrigin()
    spc = imageData.GetSpacing()
    ext = imageData.GetExtent()
    ren = self.createRenderer()
    cam = ren.GetActiveCamera()
    cam.ParallelProjectionOn()
    width = (ext[1]-ext[0])*spc[0]
    height = (ext[3]-ext[2])*spc[1]
    xoff = width*xOffset/zoom/200.
    yoff = height*yOffset/zoom/200.
    xc  = origin[0] + .5*(ext[0]+ext[1])*spc[0]
    yc  = origin[1] + .5*(ext[2]+ext[3])*spc[1]
    yd = (ext[3]-ext[2])*spc[1]
    d = cam.GetDistance()
    cam.SetParallelScale(.5*yd/zoom)
    cam.SetFocalPoint(xc+xoff,yc+yoff,0.)
    cam.SetPosition(xc+xoff,yc+yoff,d)
    ren.AddActor(a)
    layer = max(self.renWin.GetNumberOfLayers() - 2, 0)
    ren.SetLayer(layer)
    self.renWin.AddRenderer(ren)
    self.showGUI(render=False)
    self.renWin.Render()
    return

  def hideGUI(self):
    plot = self.get3DPlot()

    if plot:
        plot.hideWidgets()
    elif self.bg is False:
      from vtk_ui.manager import get_manager, manager_exists
      if manager_exists(self.renWin.GetInteractor()):
          manager = get_manager(self.renWin.GetInteractor())
          self.renWin.RemoveRenderer(manager.renderer)

  def showGUI(self, render=True):
    plot = self.get3DPlot()

    if plot:
        plot.showWidgets()
    elif self.bg is False:
      from vtk_ui.manager import get_manager, manager_exists
      if manager_exists(self.renWin.GetInteractor()):
          manager = get_manager(self.renWin.GetInteractor())
          self.renWin.AddRenderer(manager.renderer)
          # Bring the manager's renderer to the top of the stack
          manager.elevate()
      if render:
          self.renWin.Render()

  def get3DPlot(self):
    from dv3d import Gfdv3d
    plot = None
    for key in self.plotApps.keys():
        if isinstance( key, Gfdv3d ):
            plot = self.plotApps[key]
            break
    return plot

  def vectorGraphics(self, output_type, file, width=None, height=None, units=None):
    if self.renWin is None:
      raise Exception("Nothing on Canvas to dump to file")

    self.hideGUI()

    gl  = vtk.vtkGL2PSExporter()

    # This is the size of the initial memory buffer that holds the transformed
    # vertices produced by OpenGL. If you start seeing a lot of warnings:
    # GL2PS info: OpenGL feedback buffer overflow
    # increase it to save some time.
    # ParaView lags so we need a try/except around this
    # in case it is a ParaView build
    try:
      gl.SetBufferSize(50*1024*1024) # 50MB
    except:
      pass

    # Since the vcs layer stacks renderers to manually order primitives, sorting
    # is not needed and will only slow things down and introduce artifacts.
    gl.SetSortToOff()

    gl.SetInput(self.renWin)
    gl.SetCompress(0) # Do not compress
    gl.SetFilePrefix(".".join(file.split(".")[:-1]))
    gl.TextAsPathOn()
    if output_type=="svg":
        gl.SetFileFormatToSVG()
    elif output_type == "ps":
        gl.SetFileFormatToPS()
    elif output_type=="pdf":
        gl.SetFileFormatToPDF()
    else:
        raise Exception("Unknown format: %s" % output_type)
    gl.Write()
    plot = self.get3DPlot()
    if plot: plot.showWidgets()

    self.showGUI()

  def postscript(self, file, width=None, height=None, units=None,left=None,right=None,top=None,bottom=None):
      if right is not None:
          warnings.warn("the right_margin keyword for postscript has been deprecated in 2.0 and is being ignored")
      if left is not None:
          warnings.warn("the left_margin keyword for postscript has been deprecated in 2.0 and is being ignored")
      if top is not None:
          warnings.warn("the top_margin keyword for postscript has been deprecated in 2.0 and is being ignored")
      if bottom is not None:
          warnings.warn("the bottom_margin keyword for postscript has been deprecated in 2.0 and is being ignored")

      return self.vectorGraphics("ps", file, width, height, units)

  def pdf(self, file, width=None, height=None, units=None):
      return self.vectorGraphics("pdf", file, width, height, units)

  def svg(self, file, width=None, height=None, units=None):
      return self.vectorGraphics("svg", file, width, height, units)

  def gif(self,filename='noname.gif', merge='r', orientation=None, geometry='1600x1200'):
    raise RuntimeError("gif method not implemented in VTK backend yet")

  def png(self, file, width=None,height=None,units=None,draw_white_background = True, **args ):

        if self.renWin is None:
          raise Exception,"Nothing to dump aborting"

        if not file.split('.')[-1].lower() in ['png']:
            file+='.png'

        try:
          os.remove(file)
        except:
          pass


        #if width is not None and height is not None:
        #  self.renWin.SetSize(width,height)
          #self.renWin.Render()

        imgfiltr = vtk.vtkWindowToImageFilter()
        imgfiltr.SetInput(self.renWin)
#        imgfiltr.SetMagnification(3)
        ignore_alpha = args.get( 'ignore_alpha', False )
        if ignore_alpha or draw_white_background:
          imgfiltr.SetInputBufferTypeToRGB()
        else:
          imgfiltr.SetInputBufferTypeToRGBA()

        self.hideGUI()
        imgfiltr.Update()
        self.showGUI()
        self.renWin.Render()

        writer = vtk.vtkPNGWriter()
        writer.SetInputConnection(imgfiltr.GetOutputPort())
        writer.SetFileName(file)
        writer.Write()

  def cgm(self,file):
        if self.renWin is None:
          raise Exception,"Nothing to dump aborting"

        self.hideGUI()

        if not file.split('.')[-1].lower() in ['cgm']:
            file+='.cgm'

        try:
          os.remove(file)
        except:
          pass

        plot = self.get3DPlot()
        if plot: plot.hideWidgets()

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

        self.showGUI()

  def Animate(self,*args,**kargs):
     return VTKAnimate.VTKAnimate(*args,**kargs)

  def gettextextent(self,textorientation,texttable):
      warnings.warn("Please implement gettextextent for VTK Backend")

  def getantialiasing(self):
    if self.renWin is None:
      return 0
    else:
      return self.renWin.GetMultiSamples()

  def setantialiasing(self,antialiasing):
    if self.renWin is None:
      warnings.warn("no RenderWindow ready, skipping setantialiasing call, please reissue at a later time")
    else:
      self.renWin.SetMultiSamples(antialiasing)

  def createLogo(self):
    if self.canvas.drawLogo:
        if self.logoRepresentation == None:
            defaultLogoFile = os.path.join(sys.prefix,"share","vcs","uvcdat.png")
            reader = vtk.vtkPNGReader()
            reader.SetFileName( defaultLogoFile )
            reader.Update()
            logo_input = reader.GetOutput()
            self.logoRepresentation = vtk.vtkLogoRepresentation()
            self.logoRepresentation.SetImage(logo_input)
            self.logoRepresentation.ProportionalResizeOn ()
            self.logoRepresentation.SetPosition( 0.882, 0.0 )
            self.logoRepresentation.SetPosition2( 0.10, 0.05 )
            self.logoRepresentation.GetImageProperty().SetOpacity( .8 )
            self.logoRepresentation.GetImageProperty().SetDisplayLocationToBackground()
        if (self.logoRenderer == None):
            self.logoRenderer = vtk.vtkRenderer()
            self.logoRenderer.AddViewProp(self.logoRepresentation)
        self.logoRepresentation.SetRenderer(self.logoRenderer)

  def scaleLogo(self):
    if self.canvas.drawLogo:
        if self.renWin is not None:
            self.createLogo()
            self.setLayer(self.logoRenderer,1)
            self.renWin.AddRenderer(self.logoRenderer)

  def fitToViewport(self,Actor,vp,wc=None,geo=None,priority=None):
      ## Data range in World Coordinates
      if priority==0:
          return None
      vp=tuple(vp)
      if wc is None:
        Xrg = list(Actor.GetXRange())
        Yrg = list(Actor.GetYRange())
      else:
        Xrg=[float(wc[0]),float(wc[1])]
        Yrg=[float(wc[2]),float(wc[3])]

      wc_used = (float(Xrg[0]),float(Xrg[1]),float(Yrg[0]),float(Yrg[1]))
      sc = self.renWin.GetSize()

      # Ok at this point this is all the info we need
      # we can determine if it's a unique renderer or not
      # let's see if we did this already.
      if (vp,wc_used,sc,priority) in self._renderers.keys():
        #yep already have one, we will use this Renderer
        Renderer,xScale,yScale = self._renderers[(vp,wc_used,sc,priority)]
        didRenderer = True
      else:
        Renderer = self.createRenderer()
        self.renWin.AddRenderer(Renderer)
        Renderer.SetViewport(vp[0],vp[2],vp[1],vp[3])
        didRenderer = False

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
         Xrg2 = [1.e20,-1.e20]
         Yrg2 = [1.e20,-1.e20]
         if geo.GetDestinationProjection().GetName() in ["aeqd",]:
             ## These need more precision to compute actual range
             Npts=250
         else:
             Npts=50
         NGridCover=0
         pt.SetNumberOfPoints(Npts*Npts)
         for x in numpy.arange(Xrg[0],Xrg[1],(Xrg[1]-Xrg[0])/Npts):
           for y in numpy.arange(Yrg[0],Yrg[1],(Yrg[1]-Yrg[0])/Npts):
             pt.InsertPoint(NGridCover,x,y,0)
             NGridCover+=1
         pts = vtk.vtkPoints()
         #pts.SetNumberOfPoints(Npts*Npts)
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
        wRatio = float(sc[0])/float(sc[1])
        dRatio = (Xrg[1]-Xrg[0])/(Yrg[1]-Yrg[0])
        vRatio = float(vp[1]-vp[0])/float(vp[3]-vp[2])


        if wRatio>1.: #landscape orientated window
            yScale = 1.
            xScale = vRatio*wRatio/dRatio
        else:
            xScale = 1.
            yScale = dRatio/(vRatio*wRatio)
        self.setLayer(Renderer,priority)
        self._renderers[(vp,wc_used,sc,priority)] = Renderer,xScale,yScale

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

      T = vtk.vtkTransform()
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

      Renderer.AddActor(Actor)
      return Renderer

  def update_input(self,vtkobjects,array1,array2=None,update=True):
      if vtkobjects.has_key("vtk_backend_grid"):
          ## Ok ths is where we update the input data
          vg=vtkobjects["vtk_backend_grid"]
          data = vcs2vtk.numpy_to_vtk_wrapper(array1.filled(0.).flat, deep=False)
          pData= vg.GetPointData().GetScalars()
          if pData is not None:
              vg.GetPointData().SetScalars(data)
          else:
              vg.GetCellData().SetScalars(data)
          if vtkobjects.has_key("vtk_backend_filter"):
            vtkobjects["vtk_backend_filter"].Update()
          if vtkobjects.has_key("vtk_backend_missing_mapper"):
              missingMapper,color,cellData = vtkobjects["vtk_backend_missing_mapper"]
              missingMapper2 = vcs2vtk.putMaskOnVTKGrid(array1,vg,color,cellData,deep=False)
          else:
              missingMapper = None
          if vtkobjects.has_key("vtk_backend_contours"):
            for c in vtkobjects["vtk_backend_contours"]:
              c.Update()
            ports=vtkobjects["vtk_backend_contours"]
          elif vtkobjects.has_key("vtk_backend_geofilters"):
            ports=vtkobjects["vtk_backend_geofilters"]
          else:
            # Vector plot
            ports=vtkobjects["vtk_backend_glyphfilters"]
            w = vcs2vtk.generateVectorArray(array1,array2,vg)
            vg.GetPointData().AddArray(w)
            vg = vcs2vtk.stripGrid(vg)
            ports[0].SetInputData(vg)

          if vtkobjects.has_key("vtk_backend_actors"):
              i=0
              for a in vtkobjects["vtk_backend_actors"]:
                  act = a[0]
                  wrp = a[1]
                  if a[1] is missingMapper:
                      i-=1
                      mapper = missingMapper2
                      wrp = a[2]
                  else:
                      mapper = vtk.vtkPolyDataMapper()
                      mapper.SetInputConnection(ports[i].GetOutputPort())
                      lut,rg = vtkobjects["vtk_backend_luts"][i]
                      if lut is not None:
                          mapper.SetLookupTable(lut)
                          if rg[2]:
                              mapper.SetScalarModeToUseCellData()
                          mapper.SetScalarRange(rg[0],rg[1])
                  act.SetMapper(mapper)
                  act = vcs2vtk.doWrap(a[0],wrp)
                  a[0].SetMapper(act.GetMapper())
                  i+=1

      taxis = array1.getTime()
      if taxis is not None:
          tstr = str(cdtime.reltime(taxis[0],taxis.units).tocomp(taxis.getCalendar()))
      else:
          tstr = None
      ## Min/Max/Mean
      for att in ["Min","Max","Mean","crtime","crdate","zvalue"]:
          if vtkobjects.has_key("vtk_backend_%s_text_actor" % att):
              t = vtkobjects["vtk_backend_%s_text_actor" % att]
              if att == "Min":
                  t.SetInput("Min %g" % array1.min())
              elif att == "Max":
                  t.SetInput("Max %g" % array1.max())
              elif att == "Mean":
                if not inspect.ismethod(getattr(array1,'mean')):
                    meanstring = "Mean: %s" % getattr(array1,"mean")
                else:
                    try:
                     meanstring='Mean %.4g'% float(cdutil.averager(array1,
                             axis = " ".join(["(%s)" % S for S in array1.getAxisIds()])))
                    except Exception,err:
                     meanstring='Mean %.4g'%array1.mean()
                t.SetInput(meanstring)
              elif att=="crdate" and tstr is not None:
                  t.SetInput(tstr.split()[0].replace("-","/"))
              elif att=="crtime" and tstr is not None:
                  t.SetInput(tstr.split()[1])
              elif att=="zvalue":
                  if len(array1.shape)>2:
                      l=array1.getAxis(-3)
                      if l.isTime():
                          t.SetInput(str(l.asComponentTime()[0]))
                      else:
                          t.SetInput("%g" % l[0])

      if update:
        self.renWin.Render()
