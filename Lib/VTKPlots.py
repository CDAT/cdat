import cdutil
import warnings
import vtk
import vcs
import vcs2vtk
import numpy
import os
import traceback
import sys
import cdms2
import cdtime
import inspect
import VTKAnimate
import vcsvtk


class VCSInteractorStyle(vtk.vtkInteractorStyleUser):

    def __init__(self, parent):
        self.AddObserver("LeftButtonPressEvent", parent.leftButtonPressEvent)
        self.AddObserver(
            "LeftButtonReleaseEvent",
            parent.leftButtonReleaseEvent)
        self.AddObserver("ModifiedEvent", parent.configureEvent)
        self.AddObserver("ConfigureEvent", parent.configureEvent)
        if sys.platform == "darwin":
            self.AddObserver("RenderEvent", parent.renderEvent)


class VTKVCSBackend(object):

    def __init__(self, canvas, renWin=None, debug=False, bg=None):
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
        self._plot_keywords = [
            'renderer',
            'vtk_backend_grid',
            'vtk_backend_geo',
            'cdmsfile',
            'cell_coordinates']
        self.numberOfPlotCalls = 0
        self.renderWindowSize = None
        self.clickRenderer = None
        # Turn on anti-aliasing by default
        # Initially set to 16x Multi-Sampled Anti-Aliasing
        self.antialiasing = 8
        self._rasterPropsInVectorFormats = False

        if renWin is not None:
            self.renWin = renWin
            if renWin.GetInteractor() is None and self.bg is False:
                self.createDefaultInteractor()

        if sys.platform == "darwin":
            self.reRender = False
            self.oldCursor = None

    def setAnimationStepper(self, stepper):
        for plot in self.plotApps.values():
            plot.setAnimationStepper(stepper)

    def interact(self, *args, **kargs):
        if self.renWin is None:
            warnings.warn("Cannot interact if you did not open the canvas yet")
            return
        interactor = self.renWin.GetInteractor()
        # Mac seems to handle events a bit differently
        # Need to add observers on renWin
        # Linux is fine w/o it so no need to do it
        if sys.platform == "darwin":
            self.renWin.AddObserver("RenderEvent", self.renderEvent)
            self.renWin.AddObserver(
                "LeftButtonPressEvent",
                self.leftButtonPressEvent)
            self.renWin.AddObserver(
                "LeftButtonReleaseEvent",
                self.leftButtonReleaseEvent)
            self.renWin.AddObserver("ModifiedEvent", self.configureEvent)
            self.renWin.AddObserver("ConfigureEvent", self.configureEvent)
            self.renWin.AddObserver("EndEvent", self.endEvent)
        if interactor is None:
            warnings.warn("Cannot start interaction. Blank plot?")
            return
        warnings.warn(
            "Press 'Q' to exit interactive mode and continue script execution")
        self.showGUI()
        interactor.Start()

    def endEvent(self, obj, event):
        if self.renWin is not None:
            if self.reRender:
                self.reRender = False
                self.renWin.Render()

    def renderEvent(self, caller, evt):
        renwin = self.renWin if (caller is None) else caller
        window_size = renwin.GetSize()
        if (window_size != self.renderWindowSize):
            self.configureEvent(caller, evt)
            self.renderWindowSize = window_size

    def leftButtonPressEvent(self, obj, event):
        xy = self.renWin.GetInteractor().GetEventPosition()
        sz = self.renWin.GetSize()
        x = float(xy[0]) / sz[0]
        y = float(xy[1]) / sz[1]
        st = ""
        for dnm in self.canvas.display_names:
            d = vcs.elements["display"][dnm]
            if d.array[0] is None:
                continue
            t = vcs.elements["template"][d.template]
            gm = vcs.elements[d.g_type][d.g_name]
            if t.data.x1 <= x <= t.data.x2 and t.data.y1 <= y <= t.data.y2:
                # Ok we clicked within template
                if numpy.allclose(gm.datawc_x1, 1.e20):
                    x1 = d.array[0].getAxis(-1)[0]
                else:
                    x1 = gm.datawc_x1
                if numpy.allclose(gm.datawc_x2, 1.e20):
                    x2 = d.array[0].getAxis(-1)[-1]
                else:
                    x2 = gm.datawc_x2
                if numpy.allclose(gm.datawc_y1, 1.e20):
                    y1 = d.array[0].getAxis(-2)[0]
                else:
                    y1 = gm.datawc_y1
                if numpy.allclose(gm.datawc_y2, 1.e20):
                    y2 = d.array[0].getAxis(-2)[-1]
                else:
                    y2 = gm.datawc_y2

                X = (x - t.data.x1) / (t.data.x2 - t.data.x1) * (x2 - x1) + x1
                Y = (y - t.data.y1) / (t.data.y2 - t.data.y1) * (y2 - y1) + y1
                # Ok we now have the X/Y values we need to figure out the
                # indices
                try:
                    I = d.array[0].getAxis(-1).mapInterval((X, X, 'cob'))[0]
                    try:
                        J = d.array[
                            0].getAxis(-2).mapInterval((Y, Y, 'cob'))[0]
                        # Values at that point
                        V = d.array[0][..., J, I]
                    except:
                        V = d.array[0][..., I]
                    if isinstance(V, numpy.ndarray):
                        V = V.flat[0]
                    try:
                        st += "Var: %s\nX[%i] = %g\nY[%i] = %g\nValue: %g" % (
                            d.array[0].id, I, X, J, Y, V)
                    except:
                        st += "Var: %s\nX = %g\nY[%i] = %g\nValue: %g" % (
                            d.array[0].id, X, I, Y, V)
                except:
                    st += "Var: %s\nX=%g\nY=%g\nValue = N/A" % (
                        d.array[0].id, X, Y)
        if st == "":
            return
        ren = vtk.vtkRenderer()
        ren.SetBackground(.96, .96, .86)
        ren.SetViewport(x, y, min(x + .2, 1.), min(y + .2, 1))
        ren.SetLayer(self.renWin.GetNumberOfLayers() - 1)
        self.renWin.AddRenderer(ren)
        a = vtk.vtkTextActor()
        a.SetInput(st)
        p = a.GetProperty()
        p.SetColor(0, 0, 0)
        bb = [0, 0, 0, 0]
        a.GetBoundingBox(ren, bb)
        ps = vtk.vtkPlaneSource()
        ps.SetCenter(bb[0], bb[2], 0.)
        ps.SetPoint1(bb[1], bb[2], 0.)
        ps.SetPoint2(bb[0], bb[3], 0.)
        ps.Update()
        m2d = vtk.vtkPolyDataMapper2D()
        m2d.SetInputConnection(ps.GetOutputPort())
        a2d = vtk.vtkActor2D()
        a2d.SetMapper(m2d)
        a2d.GetProperty().SetColor(.93, .91, .67)
        ren.AddActor(a2d)
        ren.AddActor(a)
        ren.ResetCamera()
        self.clickRenderer = ren
        self.renWin.Render()

    def leftButtonReleaseEvent(self, obj, event):
        if self.clickRenderer is not None:
            self.clickRenderer.RemoveAllViewProps()
            self.renWin.RemoveRenderer(self.clickRenderer)
            self.renWin.Render()
            self.clickRenderer = None

    def configureEvent(self, obj, ev):
        if not self.renWin:
            return
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
        key_args = []

        for dnm in self.canvas.display_names:
            d = vcs.elements["display"][dnm]
            parg = []
            for a in d.array:
                if a is not None:
                    parg.append(a)
            parg.append(d._template_origin)
            parg.append(d.g_type)
            parg.append(d.g_name)
            plots_args.append(parg)
            if d.ratio is not None:
                key_args.append({"ratio": d.ratio})
            else:
                key_args.append({})

        # Have to pull out the UI layer so it doesn't get borked by the clear
        self.hideGUI()

        self.canvas.clear(render=False)

        for i, pargs in enumerate(plots_args):
            self.canvas.plot(*pargs, render=False, **key_args[i])

        if self.canvas.animate.created(
        ) and self.canvas.animate.frame_num != 0:
            self.canvas.animate.draw_frame(
                allow_static=False,
                render_offscreen=False)

        self.showGUI(render=False)

        if self.renWin.GetSize() != (0, 0):
            self.scaleLogo()

        self.renWin.Render()

    def clear(self, render=True):
        if self.renWin is None:  # Nothing to clear
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
            if not ren.GetLayer() == 0:
                self.renWin.RemoveRenderer(ren)
            else:
                # Update background color
                r, g, b = [c / 255. for c in self.canvas.backgroundcolor]
                ren.SetBackground(r, g, b)
            ren = renderers.GetNextItem()
        self.showGUI(render=False)

        if hasValidRenderer and self.renWin.IsDrawable() and render:
            self.renWin.Render()
        self.numberOfPlotCalls = 0
        self.logoRenderer = None
        self.createLogo()
        self._renderers = {}

    def createDefaultInteractor(self, ren=None):
        defaultInteractor = self.renWin.GetInteractor()
        if defaultInteractor is None:
            defaultInteractor = vtk.vtkRenderWindowInteractor()
        self.vcsInteractorStyle = VCSInteractorStyle(self)
        if ren:
            self.vcsInteractorStyle.SetCurrentRenderer(ren)
        defaultInteractor.SetInteractorStyle(self.vcsInteractorStyle)
        defaultInteractor.SetRenderWindow(self.renWin)
        self.vcsInteractorStyle.On()

    def createRenWin(self, *args, **kargs):
        if self.renWin is None:
            # Create the usual rendering stuff.
            self.renWin = vtk.vtkRenderWindow()
            self.renWin.SetWindowName("VCS Canvas %i" % self.canvas._canvas_id)
            self.renWin.SetAlphaBitPlanes(1)
            # turning on Stencil for Labels on iso plots
            self.renWin.SetStencilCapable(1)
            # turning off antialiasing by default
            # mostly so that pngs are same accross platforms
            self.renWin.SetMultiSamples(self.antialiasing)
            self.initialSize()

        if self.renderer is None:
            self.renderer = self.createRenderer()
            if not self.bg:
                self.createDefaultInteractor(self.renderer)
            self.renWin.AddRenderer(self.renderer)
        if "open" in kargs and kargs["open"]:
            self.renWin.Render()

    def createRenderer(self, *args, **kargs):
        # For now always use the canvas background
        ren = vtk.vtkRenderer()
        r, g, b = self.canvas.backgroundcolor
        ren.SetBackground(r / 255., g / 255., b / 255.)
        return ren

    def update(self, *args, **kargs):
        self._lastSize = -1
        if self.renWin:
            if self.get3DPlot():
                plots_args = []
                key_args = []
                for dnm in self.canvas.display_names:
                    d = vcs.elements["display"][dnm]
                    parg = []
                    for a in d.array:
                        if a is not None:
                            parg.append(a)
                    parg.append(d._template_origin)
                    parg.append(d.g_type)
                    parg.append(d.g_name)
                    plots_args.append(parg)
                    if d.ratio is not None:
                        key_args.append({"ratio": d.ratio})
                    else:
                        key_args.append({})
                for i, args in enumerate(plots_args):
                    self.canvas.plot(*args, **key_args[i])
            else:
                self.configureEvent(None, None)

    def canvasinfo(self):
        if self.renWin is None:
            mapstate = False
            height = self.canvas.bgY
            width = self.canvas.bgX
            depth = None
            x = 0
            y = 0
        else:
            try:  # mac but not linux
                mapstate = self.renWin.GetWindowCreated()
            except:
                mapstate = True
            width, height = self.renWin.GetSize()
            depth = self.renWin.GetDepthBufferSize()
            try:  # mac not linux
                x, y = self.renWin.GetPosition()
            except:
                x, y = 0, 0
        info = {
            "mapstate": mapstate,
            "height": height,
            "width": width,
            "depth": depth,
            "x": x,
            "y": y,
        }
        return info

    def orientation(self, *args, **kargs):
        canvas_info = self.canvasinfo()
        w = canvas_info["width"]
        h = canvas_info["height"]
        if w > h:
            return "landscape"
        else:
            return "portrait"

    def resize_or_rotate_window(self, W=-99, H=-99, x=-99, y=-99, clear=0):
        # Resize and position window to the provided arguments except when the
        # values are default and negative. In the latter case, it should just
        # rotate the window.
        if clear:
            self.clear()
        if self.renWin is None:
            if W != -99:
                self.canvas.bgX = W
                self.canvas.bgY = H
            else:
                W = self.canvas.bgX
                self.canvas.bgX = self.canvas.bgY
                self.canvas.bgY = W
        else:
            self.renWin.SetSize(W, H)
            self.canvas.bgX = W
            self.canvas.bgY = H

    def portrait(self, W=-99, H=-99, x=-99, y=-99, clear=0):
        self.resize_or_rotate_window(W, H, x, y, clear)

    def landscape(self, W=-99, H=-99, x=-99, y=-99, clear=0):
        self.resize_or_rotate_window(W, H, x, y, clear)

    def initialSize(self):
        # Gets user physical screen dimensions
        screenSize = self.renWin.GetScreenSize()
        try:
            # following works on some machines but not all
            # Creates the window to be 60% of user's screen's width
            bgX = int(screenSize[0] * .6)
        except:
            bgX = self.canvas.bgX
        # Respect user chosen aspect ratio
        bgY = int(bgX / self.canvas.size)
        # Sets renWin dimensions
        self.renWin.SetSize(bgX, bgY)
        self._lastSize = (bgX, bgY)

    def open(self):
        self.createRenWin(open=True)

    def close(self):
        if self.renWin is None:
            return
        self.clear()
        self.renWin.Finalize()
        self.renWin = None

    def geometry(self, x, y, *args):
        self.renWin.SetSize(x, y)

    def flush(self):
        if self.renWin is not None:
            self.renWin.Render()

    def plot(self, data1, data2, template, gtype, gname, bg, *args, **kargs):
        self.numberOfPlotCalls += 1
        # these are keyargs that can be reused later by the backend.
        returned = {}
        if self.bg is None:
            if bg:
                self.bg = True
            else:
                self.bg = False
        self.createRenWin(**kargs)
        if self.bg:
            self.renWin.SetOffScreenRendering(True)
            self.renWin.SetSize(self.canvas.bgX, self.canvas.bgY)
        self.cell_coordinates = kargs.get('cell_coordinates', None)
        self.canvas.initLogoDrawing()
        if gtype == "text":
            tt, to = gname.split(":::")
            tt = vcs.elements["texttable"][tt]
            to = vcs.elements["textorientation"][to]
            gm = tt
        else:
            gm = vcs.elements[gtype][gname]
        tpl = vcs.elements["template"][template]

        if kargs.get("renderer", None) is None:
            if (gtype in ["3d_scalar", "3d_dual_scalar", "3d_vector"]) and (
                    self.renderer is not None):
                ren = self.renderer
        else:
            ren = kargs["renderer"]

        vtk_backend_grid = kargs.get("vtk_backend_grid", None)
        vtk_backend_geo = kargs.get("vtk_backend_geo", None)

        pipeline = vcsvtk.createPipeline(gm, self)
        if pipeline is not None:
            returned.update(pipeline.plot(data1, data2, tpl,
                                          vtk_backend_grid, vtk_backend_geo))
        elif gtype in ["3d_scalar", "3d_dual_scalar", "3d_vector"]:
            cdms_file = kargs.get('cdmsfile', None)
            cdms_var = kargs.get('cdmsvar', None)
            if cdms_var is not None:
                raise Exception()
            if cdms_file is not None:
                gm.addPlotAttribute('file', cdms_file)
                gm.addPlotAttribute('filename', cdms_file)
                gm.addPlotAttribute('url', cdms_file)
            returned.update(self.plot3D(data1, data2, tpl, gm, ren, **kargs))
        elif gtype in ["text"]:
            if tt.priority != 0:
                # if not (None,None,None) in self._renderers.keys():
                ren = self.createRenderer()
                self.renWin.AddRenderer(ren)
                self.setLayer(ren, 1)
                #    self._renderers[(None,None,None)]=ren
                # else:
                #    ren = self._renderers[(None,None,None)]
                returned["vtk_backend_text_actors"] = vcs2vtk.genTextActor(
                    ren,
                    to=to,
                    tt=tt,
                    cmap=self.canvas.colormap)
                self.setLayer(ren, tt.priority)
        elif gtype == "line":
            if gm.priority != 0:
                actors = vcs2vtk.prepLine(self.renWin, gm,
                                          cmap=self.canvas.colormap)
                returned["vtk_backend_line_actors"] = actors
                create_renderer = True
                for act, geo in actors:
                    ren = self.fitToViewport(
                        act,
                        gm.viewport,
                        wc=gm.worldcoordinate,
                        geo=geo,
                        priority=gm.priority,
                        create_renderer=create_renderer)
                    create_renderer = False
        elif gtype == "marker":
            if gm.priority != 0:
                actors = vcs2vtk.prepMarker(self.renWin, gm,
                                            cmap=self.canvas.colormap)
                returned["vtk_backend_marker_actors"] = actors
                create_renderer = True
                for g, gs, pd, act, geo in actors:
                    ren = self.fitToViewport(
                        act,
                        gm.viewport,
                        wc=gm.worldcoordinate,
                        geo=geo,
                        priority=gm.priority,
                        create_renderer=True)
                    create_renderer = False
                    if pd is None and act.GetUserTransform():
                        vcs2vtk.scaleMarkerGlyph(g, gs, pd, act)

        elif gtype == "fillarea":
            if gm.priority != 0:
                actors = vcs2vtk.prepFillarea(self.renWin, gm,
                                              cmap=self.canvas.colormap)
                returned["vtk_backend_fillarea_actors"] = actors
                create_renderer = True
                for act, geo in actors:
                    ren = self.fitToViewport(
                        act,
                        gm.viewport,
                        wc=gm.worldcoordinate,
                        geo=geo,
                        priority=gm.priority,
                        create_renderer=create_renderer)
                    create_renderer = False
        else:
            raise Exception(
                "Graphic type: '%s' not re-implemented yet" %
                gtype)
        self.scaleLogo()

        # Decide whether to rasterize background in vector outputs
        # Current criteria to rasterize:
        #       * if fillarea style is either pattern or hatch
        #       * if fillarea opacity is less than 100 for solid fill
        try:
            if gm.style and all(style != 'solid' for style in gm.style):
                self._rasterPropsInVectorFormats = True
            elif gm.opacity and not all(o == 100 for o in gm.opacity):
                self._rasterPropsInVectorFormats = True
        except:
            pass
        try:
            if gm.fillareastyle in ['pattern', 'hatch']:
                self._rasterPropsInVectorFormats = True
            elif not all(o == 100 for o in gm.fillareaopacity):
                self._rasterPropsInVectorFormats = True
        except:
            pass

        if not kargs.get("donotstoredisplay", False) and kargs.get(
                "render", True):
            self.renWin.Render()
        return returned

    def setLayer(self, renderer, priority):
        n = self.numberOfPlotCalls + (priority - 1) * 200 + 1
        nMax = max(self.renWin.GetNumberOfLayers(), n + 1)
        self.renWin.SetNumberOfLayers(nMax)
        renderer.SetLayer(n)

    def plot3D(self, data1, data2, tmpl, gm, ren, **kargs):
        from DV3D.Application import DV3DApp
        requiresFileVariable = True
        self.canvas.drawLogo = False
        if (data1 is None) or (requiresFileVariable and not (isinstance(
                data1, cdms2.fvariable.FileVariable) or isinstance(data1, cdms2.tvariable.TransientVariable))):
            traceback.print_stack()
            raise Exception(
                "Error, must pass a cdms2 variable object as the first input to the dv3d gm ( found '%s')" %
                (data1.__class__.__name__))
        g = self.plotApps.get(gm, None)
        if g is None:
            g = DV3DApp(self.canvas, self.cell_coordinates)
            n_overview_points = 500000
            roi = None  # ( 0, 0, 50, 50 )
            g.gminit(
                data1,
                data2,
                roi=roi,
                axes=gm.axes,
                n_overview_points=n_overview_points,
                n_cores=gm.NumCores,
                renwin=ren.GetRenderWindow(),
                plot_attributes=gm.getPlotAttributes(),
                gmname=gm.g_name,
                cm=gm.cfgManager,
                **kargs)  # , plot_type = PlotType.List  )
            self.plotApps[gm] = g
            self.plotRenderers.add(g.plot.renderer)
        else:
            g.update(tmpl)
        return {}

    def onClosing(self, cell):
        for plot in self.plotApps.values():
            if hasattr(plot, 'onClosing'):
                plot.onClosing(cell)

    def plotContinents(self, x1, x2, y1, y2, projection, wrap, tmpl):
        contData = vcs2vtk.prepContinents(self.canvas._continents)
        contMapper = vtk.vtkPolyDataMapper()
        contMapper.SetInputData(contData)
        contActor = vtk.vtkActor()
        contActor.SetMapper(contMapper)
        contActor.GetProperty().SetColor(0., 0., 0.)
        contActor = vcs2vtk.doWrap(
            contActor, [
                x1, x2, y1, y2], wrap, fastClip=False)
        if projection.type != "linear":
            contData = contActor.GetMapper().GetInput()
            cpts = contData.GetPoints()
            geo, gcpts = vcs2vtk.project(cpts, projection, [x1, x2, y1, y2])
            contData.SetPoints(gcpts)
            contMapper = vtk.vtkPolyDataMapper()
            contMapper.SetInputData(contData)
            contActor = vtk.vtkActor()
            contActor.SetMapper(contMapper)
            contActor.GetProperty().SetColor(0., 0., 0.)
        else:
            geo = None

        self.fitToViewport(contActor,
                           [tmpl.data.x1, tmpl.data.x2,
                            tmpl.data.y1, tmpl.data.y2],
                           wc=[x1, x2, y1, y2], geo=geo,
                           priority=tmpl.data.priority,
                           create_renderer=True)
        return {}

    def renderTemplate(self, tmpl, data, gm, taxis, zaxis):
        # ok first basic template stuff, let's store the displays
        # because we need to return actors for min/max/mean
        displays = tmpl.plot(self.canvas, data, gm, bg=self.bg)
        returned = {}
        for d in displays:
            if d is None:
                continue
            texts = d.backend.get("vtk_backend_text_actors", [])
            for t in texts:
                # ok we had a text actor, let's see if it's min/max/mean
                txt = t.GetInput()
                s0 = txt.split()[0]
                if s0 in ["Min", "Max", "Mean"]:
                    returned["vtk_backend_%s_text_actor" % s0] = t
                else:
                    returned[
                        "vtk_backend_%s_text_actor" %
                        d.backend["vtk_backend_template_attribute"]] = t
            self.canvas.display_names.remove(d.name)
            del(vcs.elements["display"][d.name])
        # Sometimes user passes "date" as an attribute to replace date
        if hasattr(data, "user_date"):
            taxis = cdms2.createAxis(
                [cdtime.s2r(data.user_date, "days since 1900").value])
            taxis.designateTime()
            taxis.units = "days since 1900"
            if zaxis is not None and zaxis.isTime():
                zaxis = taxis
        if taxis is not None:
            try:
                tstr = str(
                    cdtime.reltime(
                        taxis[0],
                        taxis.units).tocomp(
                        taxis.getCalendar()))
                # ok we have a time axis let's display the time
                crdate = vcs2vtk.applyAttributesFromVCStmpl(tmpl, "crdate")
                crdate.string = tstr.split()[0].replace("-", "/")
                crtime = vcs2vtk.applyAttributesFromVCStmpl(tmpl, "crtime")
                crtime.string = tstr.split()[1]
                if not (None, None, None) in self._renderers.keys():
                    ren = self.createRenderer()
                    self.renWin.AddRenderer(ren)
                    self.setLayer(ren, 1)
                    self._renderers[(None, None, None)] = ren
                else:
                    ren = self._renderers[(None, None, None)]
                tt, to = crdate.name.split(":::")
                tt = vcs.elements["texttable"][tt]
                to = vcs.elements["textorientation"][to]
                if crdate.priority > 0:
                    actors = vcs2vtk.genTextActor(ren, to=to, tt=tt)
                    returned["vtk_backend_crdate_text_actor"] = actors[0]
                del(vcs.elements["texttable"][tt.name])
                del(vcs.elements["textorientation"][to.name])
                del(vcs.elements["textcombined"][crdate.name])
                tt, to = crtime.name.split(":::")
                tt = vcs.elements["texttable"][tt]
                to = vcs.elements["textorientation"][to]
                if crtime.priority > 0:
                    actors = vcs2vtk.genTextActor(ren, to=to, tt=tt)
                    returned["vtk_backend_crtime_text_actor"] = actors[0]
                del(vcs.elements["texttable"][tt.name])
                del(vcs.elements["textorientation"][to.name])
                del(vcs.elements["textcombined"][crtime.name])
            except:
                pass
        if zaxis is not None:
            try:
                # ok we have a zaxis to draw
                zname = vcs2vtk.applyAttributesFromVCStmpl(tmpl, "zname")
                zname.string = zaxis.id
                zvalue = vcs2vtk.applyAttributesFromVCStmpl(tmpl, "zvalue")
                if zaxis.isTime():
                    zvalue.string = str(zaxis.asComponentTime()[0])
                else:
                    zvalue.string = "%g" % zaxis[0]
                if not (None, None, None) in self._renderers.keys():
                    ren = self.createRenderer()
                    self.renWin.AddRenderer(ren)
                    self.setLayer(ren, 1)
                    self._renderers[(None, None, None)] = ren
                else:
                    ren = self._renderers[(None, None, None)]
                tt, to = zname.name.split(":::")
                tt = vcs.elements["texttable"][tt]
                to = vcs.elements["textorientation"][to]
                if zname.priority > 0:
                    vcs2vtk.genTextActor(ren, to=to, tt=tt)
                del(vcs.elements["texttable"][tt.name])
                del(vcs.elements["textorientation"][to.name])
                del(vcs.elements["textcombined"][zname.name])
                if hasattr(zaxis, "units"):
                    zunits = vcs2vtk.applyAttributesFromVCStmpl(tmpl, "zunits")
                    zunits.string = zaxis.units
                    if zunits.priority > 0:
                        tt, to = zunits.name.split(":::")
                        tt = vcs.elements["texttable"][tt]
                        to = vcs.elements["textorientation"][to]
                        vcs2vtk.genTextActor(ren, to=to, tt=tt)
                        del(vcs.elements["texttable"][tt.name])
                        del(vcs.elements["textorientation"][to.name])
                        del(vcs.elements["textcombined"][zunits.name])
                tt, to = zvalue.name.split(":::")
                tt = vcs.elements["texttable"][tt]
                to = vcs.elements["textorientation"][to]
                if zvalue.priority > 0:
                    actors = vcs2vtk.genTextActor(ren, to=to, tt=tt)
                    returned["vtk_backend_zvalue_text_actor"] = actors[0]
                del(vcs.elements["texttable"][tt.name])
                del(vcs.elements["textorientation"][to.name])
                del(vcs.elements["textcombined"][zvalue.name])
            except:
                pass
        return returned

    def renderColorBar(self, tmpl, levels, colors, legend, cmap,
                       style=['solid'], index=[1], opacity=[]):
        if tmpl.legend.priority > 0:
            tmpl.drawColorBar(
                colors,
                levels,
                x=self.canvas,
                legend=legend,
                cmap=cmap,
                style=style,
                index=index,
                opacity=opacity)
        return {}

    def cleanupData(self, data):
        data[:] = numpy.ma.masked_invalid(data, numpy.nan)
        return data

    def trimData1D(self, data):
        if data is None:
            return None
        while len(data.shape) > 1:
            data = data[0]
        return self.cleanupData(data)

    # ok now trying to figure the actual data to plot
    def trimData2D(self, data):
        if data is None:
            return None
        try:
            g = data.getGrid()
            gaxes = list(g.getAxisList())
            daxes = list(data.getAxisList())
            if daxes[len(daxes) - len(gaxes):] == gaxes:
                # Ok it is gridded and the grid axes are last
                return self.cleanupData(
                    data(*(slice(0, 1),) * (len(daxes) - len(gaxes)), squeeze=1))
            else:
                # Ok just return the last two dims
                return self.cleanupData(
                    data(*(slice(0, 1),) * (len(daxes) - 2), squeeze=1))
        except:
            daxes = list(data.getAxisList())
            if cdms2.isVariable(data):
                return self.cleanupData(
                    data(*(slice(0, 1),) * (len(daxes) - 2)))
            else:  # numpy arrays are not callable
                op = ()
                for i in range(numpy.rank(data) - 2):
                    op.append(slice(0, 1))
                return self.cleanupData(data[op])

    def put_png_on_canvas(
            self, filename, zoom=1, xOffset=0, yOffset=0, *args, **kargs):
        return self.put_img_on_canvas(
            filename, zoom, xOffset, yOffset, *args, **kargs)

    def put_img_on_canvas(
            self, filename, zoom=1, xOffset=0, yOffset=0, *args, **kargs):
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
        width = (ext[1] - ext[0]) * spc[0]
        height = (ext[3] - ext[2]) * spc[1]
        xoff = width * xOffset / zoom / 200.
        yoff = height * yOffset / zoom / 200.
        xc = origin[0] + .5 * (ext[0] + ext[1]) * spc[0]
        yc = origin[1] + .5 * (ext[2] + ext[3]) * spc[1]
        yd = (ext[3] - ext[2]) * spc[1]
        d = cam.GetDistance()
        cam.SetParallelScale(.5 * yd / zoom)
        cam.SetFocalPoint(xc + xoff, yc + yoff, 0.)
        cam.SetPosition(xc + xoff, yc + yoff, d)
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
        elif not self.bg:
            from vtk_ui.manager import get_manager, manager_exists
            if manager_exists(self.renWin.GetInteractor()):
                manager = get_manager(self.renWin.GetInteractor())
                self.renWin.RemoveRenderer(manager.renderer)
                self.renWin.RemoveRenderer(manager.actor_renderer)

    def showGUI(self, render=True):
        plot = self.get3DPlot()

        if plot:
            plot.showWidgets()
        elif not self.bg:
            from vtk_ui.manager import get_manager, manager_exists
            if manager_exists(self.renWin.GetInteractor()):
                manager = get_manager(self.renWin.GetInteractor())
                self.renWin.AddRenderer(manager.renderer)
                self.renWin.AddRenderer(manager.actor_renderer)
                # Bring the manager's renderer to the top of the stack
                manager.elevate()
            if render:
                self.renWin.Render()

    def get3DPlot(self):
        from dv3d import Gfdv3d
        plot = None
        for key in self.plotApps.keys():
            if isinstance(key, Gfdv3d):
                plot = self.plotApps[key]
                break
        return plot

    def vectorGraphics(
            self, output_type, file, width=None, height=None, units=None):
        if self.renWin is None:
            raise Exception("Nothing on Canvas to dump to file")

        self.hideGUI()

        gl = vtk.vtkGL2PSExporter()

        # This is the size of the initial memory buffer that holds the transformed
        # vertices produced by OpenGL. If you start seeing a lot of warnings:
        # GL2PS info: OpenGL feedback buffer overflow
        # increase it to save some time.
        # ParaView lags so we need a try/except around this
        # in case it is a ParaView build
        try:
            gl.SetBufferSize(50 * 1024 * 1024)  # 50MB
        except:
            pass

        # Since the vcs layer stacks renderers to manually order primitives, sorting
        # is not needed and will only slow things down and introduce artifacts.
        gl.SetSortToOff()

        # Since the patterns are applied as textures on vtkPolyData, enabling
        # background rasterization is required to write them out
        if self._rasterPropsInVectorFormats:
            gl.Write3DPropsAsRasterImageOn()

        gl.SetInput(self.renWin)
        gl.SetCompress(0)  # Do not compress
        gl.SetFilePrefix(".".join(file.split(".")[:-1]))
        gl.TextAsPathOn()
        if output_type == "svg":
            gl.SetFileFormatToSVG()
        elif output_type == "ps":
            gl.SetFileFormatToPS()
        elif output_type == "pdf":
            gl.SetFileFormatToPDF()
        else:
            raise Exception("Unknown format: %s" % output_type)
        gl.Write()
        plot = self.get3DPlot()
        if plot:
            plot.showWidgets()

        self.showGUI()

    def postscript(self, file, width=None, height=None,
                   units=None, left=None, right=None, top=None, bottom=None):
        if right is not None:
            warnings.warn(
                "the right_margin keyword for postscript has been deprecated in 2.0 and is being ignored")
        if left is not None:
            warnings.warn(
                "the left_margin keyword for postscript has been deprecated in 2.0 and is being ignored")
        if top is not None:
            warnings.warn(
                "the top_margin keyword for postscript has been deprecated in 2.0 and is being ignored")
        if bottom is not None:
            warnings.warn(
                "the bottom_margin keyword for postscript has been deprecated in 2.0 and is being ignored")

        return self.vectorGraphics("ps", file, width, height, units)

    def pdf(self, file, width=None, height=None, units=None):
        return self.vectorGraphics("pdf", file, width, height, units)

    def svg(self, file, width=None, height=None, units=None):
        return self.vectorGraphics("svg", file, width, height, units)

    def gif(self, filename='noname.gif', merge='r', orientation=None,
            geometry='1600x1200'):
        raise RuntimeError("gif method not implemented in VTK backend yet")

    def png(self, file, width=None, height=None,
            units=None, draw_white_background=True, **args):

        if self.renWin is None:
            raise Exception("Nothing to dump aborting")

        if not file.split('.')[-1].lower() in ['png']:
            file += '.png'

        try:
            os.remove(file)
        except:
            pass

        # if width is not None and height is not None:
        #  self.renWin.SetSize(width,height)
            # self.renWin.Render()

        imgfiltr = vtk.vtkWindowToImageFilter()
        imgfiltr.SetInput(self.renWin)
#        imgfiltr.SetMagnification(3)
        ignore_alpha = args.get('ignore_alpha', False)
        if ignore_alpha or draw_white_background:
            imgfiltr.SetInputBufferTypeToRGB()
        else:
            imgfiltr.SetInputBufferTypeToRGBA()

        self.hideGUI()
        imgfiltr.Update()
        self.showGUI(render=False)
        self.renWin.Render()

        writer = vtk.vtkPNGWriter()
        writer.SetInputConnection(imgfiltr.GetOutputPort())
        writer.SetFileName(file)
        writer.Write()

    def cgm(self, file):
        if self.renWin is None:
            raise Exception("Nothing to dump aborting")

        self.hideGUI()

        if not file.split('.')[-1].lower() in ['cgm']:
            file += '.cgm'

        try:
            os.remove(file)
        except:
            pass

        plot = self.get3DPlot()
        if plot:
            plot.hideWidgets()

        writer = vtk.vtkIOCGM.vtkCGMWriter()
        writer.SetFileName(file)
        R = self.renWin.GetRenderers()
        r = R.GetFirstRenderer()
        A = r.GetActors()
        A.InitTraversal()
        a = A.GetNextActor()
        while a is not None:
            m = a.GetMapper()
            m.Update()
            writer.SetInputData(m.GetInput())
            writer.Write()
            a = A.GetNextActor()

        self.showGUI()

    def Animate(self, *args, **kargs):
        return VTKAnimate.VTKAnimate(*args, **kargs)

    def gettextextent(self, textorientation, texttable):
        warnings.warn("Please implement gettextextent for VTK Backend")

    def getantialiasing(self):
        if self.renWin is None:
            return self.antialiasing
        else:
            return self.renWin.GetMultiSamples()

    def setantialiasing(self, antialiasing):
        self.antialiasing = antialiasing
        if self.renWin is not None:
            self.renWin.SetMultiSamples(antialiasing)

    def createLogo(self):
        if self.canvas.drawLogo:
            if self.logoRepresentation is None:
                defaultLogoFile = os.path.join(
                    sys.prefix,
                    "share",
                    "vcs",
                    "uvcdat.png")
                reader = vtk.vtkPNGReader()
                reader.SetFileName(defaultLogoFile)
                reader.Update()
                logo_input = reader.GetOutput()
                self.logoRepresentation = vtk.vtkLogoRepresentation()
                self.logoRepresentation.SetImage(logo_input)
                self.logoRepresentation.ProportionalResizeOn()
                self.logoRepresentation.SetPosition(0.882, 0.0)
                self.logoRepresentation.SetPosition2(0.10, 0.05)
                self.logoRepresentation.GetImageProperty().SetOpacity(.8)
                self.logoRepresentation.GetImageProperty(
                ).SetDisplayLocationToBackground()
            if (self.logoRenderer is None):
                self.logoRenderer = vtk.vtkRenderer()
                self.logoRenderer.AddViewProp(self.logoRepresentation)
            self.logoRepresentation.SetRenderer(self.logoRenderer)

    def scaleLogo(self):
        if self.canvas.drawLogo:
            if self.renWin is not None:
                self.createLogo()
                self.setLayer(self.logoRenderer, 1)
                self.renWin.AddRenderer(self.logoRenderer)

    def fitToViewport(self, Actor, vp, wc=None, geo=None, priority=None,
                      create_renderer=False):

        # Data range in World Coordinates
        if priority == 0:
            return None
        vp = tuple(vp)
        if wc is None:
            Xrg = list(Actor.GetXRange())
            Yrg = list(Actor.GetYRange())
        else:
            Xrg = [float(wc[0]), float(wc[1])]
            Yrg = [float(wc[2]), float(wc[3])]

        wc_used = (float(Xrg[0]), float(Xrg[1]), float(Yrg[0]), float(Yrg[1]))
        sc = self.renWin.GetSize()

        # Ok at this point this is all the info we need
        # we can determine if it's a unique renderer or not
        # let's see if we did this already.
        if not create_renderer and\
                (vp, wc_used, sc, priority) in self._renderers.keys():
            # yep already have one, we will use this Renderer
            Renderer, xScale, yScale = self._renderers[
                (vp, wc_used, sc, priority)]
        else:
            Renderer = self.createRenderer()
            self.renWin.AddRenderer(Renderer)
            Renderer.SetViewport(vp[0], vp[2], vp[1], vp[3])

            if Yrg[0] > Yrg[1]:
                # Yrg=[Yrg[1],Yrg[0]]
                # T.RotateY(180)
                Yrg = [Yrg[1], Yrg[0]]
                flipY = True
            else:
                flipY = False
            if Xrg[0] > Xrg[1]:
                Xrg = [Xrg[1], Xrg[0]]
                flipX = True
            else:
                flipX = False

            if geo is not None:
                pt = vtk.vtkPoints()
                Xrg2 = [1.e20, -1.e20]
                Yrg2 = [1.e20, -1.e20]
                if geo.GetDestinationProjection().GetName() in ["aeqd", ]:
                    # These need more precision to compute actual range
                    Npts = 250
                else:
                    Npts = 50
                NGridCover = 0
                pt.SetNumberOfPoints(Npts * Npts)
                for x in numpy.arange(
                        Xrg[0], Xrg[1], (Xrg[1] - Xrg[0]) / Npts):
                    for y in numpy.arange(
                            Yrg[0], Yrg[1], (Yrg[1] - Yrg[0]) / Npts):
                        pt.InsertPoint(NGridCover, x, y, 0)
                        NGridCover += 1
                pts = vtk.vtkPoints()
                # pts.SetNumberOfPoints(Npts*Npts)
                geo.TransformPoints(pt, pts)
                b = pts.GetBounds()
                xm, xM, ym, yM = b[:4]
                if xm != -numpy.inf:
                    Xrg2[0] = min(Xrg2[0], xm)
                if xM != numpy.inf:
                    Xrg2[1] = max(Xrg2[1], xM)
                if ym != -numpy.inf:
                    Yrg2[0] = min(Yrg2[0], ym)
                if yM != numpy.inf:
                    Yrg2[1] = max(Yrg2[1], yM)
                Xrg = Xrg2
                Yrg = Yrg2
            wRatio = float(sc[0]) / float(sc[1])
            dRatio = (Xrg[1] - Xrg[0]) / (Yrg[1] - Yrg[0])
            vRatio = float(vp[1] - vp[0]) / float(vp[3] - vp[2])

            if wRatio > 1.:  # landscape orientated window
                yScale = 1.
                xScale = vRatio * wRatio / dRatio
            else:
                xScale = 1.
                yScale = dRatio / (vRatio * wRatio)
            self.setLayer(Renderer, priority)
            self._renderers[
                (vp, wc_used, sc, priority)] = Renderer, xScale, yScale

            xc = xScale * float(Xrg[1] + Xrg[0]) / 2.
            yc = yScale * float(Yrg[1] + Yrg[0]) / 2.
            yd = yScale * float(Yrg[1] - Yrg[0]) / 2.
            cam = Renderer.GetActiveCamera()
            cam.ParallelProjectionOn()
            cam.SetParallelScale(yd)
            cd = cam.GetDistance()
            cam.SetPosition(xc, yc, cd)
            cam.SetFocalPoint(xc, yc, 0.)
            if geo is None:
                if flipY:
                    cam.Elevation(180.)
                    cam.Roll(180.)
                    pass
                if flipX:
                    cam.Azimuth(180.)

        T = vtk.vtkTransform()
        T.Scale(xScale, yScale, 1.)

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

    def update_input(self, vtkobjects, array1, array2=None, update=True):
        if "vtk_backend_grid" in vtkobjects:
            # Ok ths is where we update the input data
            vg = vtkobjects["vtk_backend_grid"]
            data = vcs2vtk.numpy_to_vtk_wrapper(
                array1.filled(0.).flat,
                deep=False)
            pData = vg.GetPointData().GetScalars()
            if pData is not None:
                vg.GetPointData().SetScalars(data)
            else:
                vg.GetCellData().SetScalars(data)
            if "vtk_backend_filter" in vtkobjects:
                vtkobjects["vtk_backend_filter"].Update()
            if "vtk_backend_missing_mapper" in vtkobjects:
                missingMapper, color, cellData = vtkobjects[
                    "vtk_backend_missing_mapper"]
                missingMapper2 = vcs2vtk.putMaskOnVTKGrid(
                    array1,
                    vg,
                    color,
                    cellData,
                    deep=False)
            else:
                missingMapper = None
            if "vtk_backend_contours" in vtkobjects:
                for c in vtkobjects["vtk_backend_contours"]:
                    c.Update()
                ports = vtkobjects["vtk_backend_contours"]
            elif "vtk_backend_geofilters" in vtkobjects:
                ports = vtkobjects["vtk_backend_geofilters"]
            else:
                # Vector plot
                ports = vtkobjects["vtk_backend_glyphfilters"]
                w = vcs2vtk.generateVectorArray(array1, array2, vg)
                vg.GetPointData().AddArray(w)
                ports[0].SetInputData(vg)

            if "vtk_backend_actors" in vtkobjects:
                i = 0
                for a in vtkobjects["vtk_backend_actors"]:
                    act = a[0]
                    wrp = a[1]
                    if a[1] is missingMapper:
                        i -= 1
                        mapper = missingMapper2
                        wrp = a[2]
                    else:
                        # Labeled contours are a different kind
                        if "vtk_backend_luts" in vtkobjects:
                            lut, rg = vtkobjects["vtk_backend_luts"][i]
                            mapper = vtk.vtkPolyDataMapper()
                        elif "vtk_backend_labeled_luts" in vtkobjects:
                            lut, rg = vtkobjects["vtk_backend_labeled_luts"][i]
                            mapper = vtk.vtkLabeledContourMapper()
                        if lut is None:
                            mapper.SetInputConnection(ports[i].GetOutputPort())
                        else:
                            if mapper.IsA("vtkPolyDataMapper"):
                                mapper.SetInputConnection(
                                    ports[i].GetOutputPort())
                                mapper.SetLookupTable(lut)
                                mapper.SetScalarModeToUsePointData()
                            else:
                                stripper = vtk.vtkStripper()
                                stripper.SetInputConnection(
                                    ports[i].GetOutputPort())
                                mapper.SetInputConnection(
                                    stripper.GetOutputPort())
                                stripper.Update()
                                tprops = vtkobjects[
                                    "vtk_backend_contours_labels_text_properties"]
                                mapper.GetPolyDataMapper().SetLookupTable(lut)
                                mapper.GetPolyDataMapper(
                                ).SetScalarModeToUsePointData()
                                mapper.GetPolyDataMapper().SetScalarRange(
                                    rg[0],
                                    rg[1])
                                mapper.SetLabelVisibility(1)
                                mapper.SetTextProperties(tprops)
                            if rg[2]:
                                mapper.SetScalarModeToUseCellData()
                            mapper.SetScalarRange(rg[0], rg[1])
                    act.SetMapper(mapper)
                    act = vcs2vtk.doWrap(a[0], wrp)
                    a[0].SetMapper(act.GetMapper())
                    i += 1

        taxis = array1.getTime()
        if taxis is not None:
            tstr = str(
                cdtime.reltime(
                    taxis[0],
                    taxis.units).tocomp(
                    taxis.getCalendar()))
        else:
            tstr = None
        # Min/Max/Mean
        for att in ["Min", "Max", "Mean", "crtime", "crdate", "zvalue"]:
            if "vtk_backend_%s_text_actor" % att in vtkobjects:
                t = vtkobjects["vtk_backend_%s_text_actor" % att]
                if att == "Min":
                    t.SetInput("Min %g" % array1.min())
                elif att == "Max":
                    t.SetInput("Max %g" % array1.max())
                elif att == "Mean":
                    if not inspect.ismethod(getattr(array1, 'mean')):
                        meanstring = "Mean: %s" % getattr(array1, "mean")
                    else:
                        try:
                            meanstring = 'Mean %.4g' % \
                                float(cdutil.averager(array1, axis=" ".join(["(%s)" %
                                                                             S for S in array1.getAxisIds()])))
                        except:
                            meanstring = 'Mean %.4g' % array1.mean()
                    t.SetInput(meanstring)
                elif att == "crdate" and tstr is not None:
                    t.SetInput(tstr.split()[0].replace("-", "/"))
                elif att == "crtime" and tstr is not None:
                    t.SetInput(tstr.split()[1])
                elif att == "zvalue":
                    if len(array1.shape) > 2:
                        l = array1.getAxis(-3)
                        if l.isTime():
                            t.SetInput(str(l.asComponentTime()[0]))
                        else:
                            t.SetInput("%g" % l[0])

        if update:
            self.renWin.Render()
