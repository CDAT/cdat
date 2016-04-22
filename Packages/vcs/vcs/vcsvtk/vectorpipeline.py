from .pipeline import Pipeline

import vcs
from vcs import vcs2vtk
import vtk


class VectorPipeline(Pipeline):

    """Implementation of the Pipeline interface for VCS vector plots."""

    def __init__(self, gm, context_):
        super(VectorPipeline, self).__init__(gm, context_)

    def plot(self, data1, data2, tmpl, grid, transform, **kargs):
        """Overrides baseclass implementation."""
        # Preserve time and z axis for plotting these inof in rendertemplate
        geo = None  # to make flake8 happy
        projection = vcs.elements["projection"][self._gm.projection]
        returned = {}
        taxis = data1.getTime()
        if data1.ndim > 2:
            zaxis = data1.getAxis(-3)
        else:
            zaxis = None

        # Ok get3 only the last 2 dims
        data1 = self._context().trimData2D(data1)
        data2 = self._context().trimData2D(data2)

        scale = 1.0
        lat = None
        lon = None

        latAccessor = data1.getLatitude()
        lonAccessor = data1.getLongitude()
        if latAccessor:
            lat = latAccessor[:]
        if lonAccessor:
            lon = lonAccessor[:]

        plotBasedDualGrid = kargs.get('plot_based_dual_grid', True)
        if (plotBasedDualGrid):
            hasCellData = data1.hasCellData()
            dualGrid = hasCellData
        else:
            dualGrid = False
        gridGenDict = vcs2vtk.genGrid(data1, data2, self._gm, deep=False, grid=grid,
                                      geo=transform, genVectors=True,
                                      dualGrid=dualGrid)

        data1 = gridGenDict["data"]
        data2 = gridGenDict["data2"]
        geo = gridGenDict["geo"]

        grid = gridGenDict['vtk_backend_grid']
        xm = gridGenDict['xm']
        xM = gridGenDict['xM']
        ym = gridGenDict['ym']
        yM = gridGenDict['yM']
        continents = gridGenDict['continents']
        self._dataWrapModulo = gridGenDict['wrap']
        geo = gridGenDict['geo']
        cellData = gridGenDict['cellData']

        if geo is not None:
            newv = vtk.vtkDoubleArray()
            newv.SetNumberOfComponents(3)
            newv.InsertTupleValue(0, [lon.min(), lat.min(), 0])
            newv.InsertTupleValue(1, [lon.max(), lat.max(), 0])

            vcs2vtk.projectArray(newv, projection, [xm, xM, ym, yM])
            dimMin = [0, 0, 0]
            dimMax = [0, 0, 0]

            newv.GetTupleValue(0, dimMin)
            newv.GetTupleValue(1, dimMax)

            maxDimX = max(dimMin[0], dimMax[0])
            maxDimY = max(dimMin[1], dimMax[1])

            if lat.max() != 0.0:
                scale = abs((maxDimY / lat.max()))

            if lon.max() != 0.0:
                temp = abs((maxDimX / lon.max()))
                if scale < temp:
                    scale = temp
        else:
            scale = 1.0

        returned["vtk_backend_grid"] = grid
        returned["vtk_backend_geo"] = geo
        missingMapper = vcs2vtk.putMaskOnVTKGrid(data1, grid, actorColor=None,
                                                 cellData=cellData, deep=False)

        # None/False are for color and cellData
        # (sent to vcs2vtk.putMaskOnVTKGrid)
        returned["vtk_backend_missing_mapper"] = (missingMapper, None, False)

        # convert to point data
        if cellData:
            c2p = vtk.vtkCellDataToPointData()
            c2p.SetInputData(grid)
            c2p.Update()
            grid = c2p.GetOutput()

        # Vector attempt
        l = self._gm.line
        if l is None:
            l = "default"
        try:
            l = vcs.getline(l)
            lwidth = l.width[0]  # noqa
            lcolor = l.color[0]
            lstyle = l.type[0]  # noqa
        except:
            lstyle = "solid"  # noqa
            lwidth = 1.  # noqa
            lcolor = 0
        if self._gm.linewidth is not None:
            lwidth = self._gm.linewidth  # noqa
        if self._gm.linecolor is not None:
            lcolor = self._gm.linecolor

        arrow = vtk.vtkGlyphSource2D()
        arrow.SetGlyphTypeToArrow()
        arrow.SetOutputPointsPrecision(vtk.vtkAlgorithm.DOUBLE_PRECISION)
        arrow.FilledOff()

        glyphFilter = vtk.vtkGlyph2D()
        glyphFilter.SetInputData(grid)
        glyphFilter.SetInputArrayToProcess(1, 0, 0, 0, "vector")
        glyphFilter.SetSourceConnection(arrow.GetOutputPort())
        glyphFilter.SetVectorModeToUseVector()

        # Rotate arrows to match vector data:
        glyphFilter.OrientOn()

        # Scale to vector magnitude:
        glyphFilter.SetScaleModeToScaleByVector()
        glyphFilter.SetScaleFactor(scale * 2.0 * self._gm.scale)

        # These are some unfortunately named methods. It does *not* clamp the
        # scale range to [min, max], but rather remaps the range
        # [min, max] --> [0, 1].
        glyphFilter.ClampingOn()
        glyphFilter.SetRange(0.01, 1.0)

        mapper = vtk.vtkPolyDataMapper()

        glyphFilter.Update()
        data = glyphFilter.GetOutput()

        mapper.SetInputData(data)
        mapper.ScalarVisibilityOff()
        act = vtk.vtkActor()
        act.SetMapper(mapper)

        cmap = self.getColorMap()
        r, g, b, a = cmap.index[lcolor]
        act.GetProperty().SetColor(r / 100., g / 100., b / 100.)

        plotting_dataset_bounds = vcs2vtk.getPlottingBounds(
            vcs.utils.getworldcoordinates(self._gm,
                                          data1.getAxis(-1),
                                          data1.getAxis(-2)),
            [xm, xM, ym, yM], geo)
        x1, x2, y1, y2 = plotting_dataset_bounds
        if geo is None:
            wc = plotting_dataset_bounds
        else:
            xrange = list(act.GetXRange())
            yrange = list(act.GetYRange())
            wc = [xrange[0], xrange[1], yrange[0], yrange[1]]

        if (transform and kargs.get('ratio', '0') == 'autot'):
            returned['ratio_autot_viewport'] = self._processRatioAutot(tmpl, grid)

        vp = returned.get('ratio_autot_viewport',
                          [tmpl.data.x1, tmpl.data.x2, tmpl.data.y1, tmpl.data.y2])
        # look for previous dataset_bounds different than ours and
        # modify the viewport so that the datasets are alligned
        # Hack to fix the case when the user does not specify gm.datawc_...
        # if geo is None:
        #     for dp in vcs.elements['display'].values():
        #         if (hasattr(dp, 'backend')):
        #             prevWc = dp.backend.get('dataset_bounds', None)
        #             if (prevWc):
        #                 middleX = float(vp[0] + vp[1]) / 2.0
        #                 middleY = float(vp[2] + vp[3]) / 2.0
        #                 sideX = float(vp[1] - vp[0]) / 2.0
        #                 sideY = float(vp[3] - vp[2]) / 2.0
        #                 ratioX = float(prevWc[1] - prevWc[0]) / float(wc[1] - wc[0])
        #                 ratioY = float(prevWc[3] - prevWc[2]) / float(wc[3] - wc[2])
        #                 sideX = sideX / ratioX
        #                 sideY = sideY / ratioY
        #                 vp = [middleX - sideX, middleX + sideX, middleY - sideY, middleY + sideY]

        dataset_renderer, xScale, yScale = self._context().fitToViewport(
            act, vp,
            wc=wc,
            priority=tmpl.data.priority,
            create_renderer=True)
        returned['dataset_renderer'] = dataset_renderer
        returned['dataset_scale'] = (xScale, yScale)
        bounds = [min(xm, xM), max(xm, xM), min(ym, yM), max(ym, yM)]
        kwargs = {'vtk_backend_grid': grid,
                  'dataset_bounds': bounds,
                  'plotting_dataset_bounds': plotting_dataset_bounds}
        if ('ratio_autot_viewport' in returned):
            kwargs["ratio_autot_viewport"] = vp
        returned.update(self._context().renderTemplate(
            tmpl, data1,
            self._gm, taxis, zaxis, **kwargs))

        if self._context().canvas._continents is None:
            continents = False
        if continents:
            continents_renderer, xScale, yScale = self._context().plotContinents(
                plotting_dataset_bounds, projection,
                self._dataWrapModulo, vp, tmpl.data.priority,
                vtk_backend_grid=grid,
                dataset_bounds=bounds)
            returned["continents_renderer"] = continents_renderer
        returned["vtk_backend_actors"] = [[act, plotting_dataset_bounds]]
        returned["vtk_backend_glyphfilters"] = [glyphFilter]
        returned["vtk_backend_luts"] = [[None, None]]

        return returned
