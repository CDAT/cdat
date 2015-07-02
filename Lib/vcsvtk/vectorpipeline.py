from .pipeline import Pipeline

import vcs
from vcs import vcs2vtk
import vtk


class VectorPipeline(Pipeline):

    """Implementation of the Pipeline interface for VCS vector plots."""

    def __init__(self, context_):
        super(VectorPipeline, self).__init__(context_)

    def plot(self, data1, data2, tmpl, gm, grid, transform):
        """Overrides baseclass implementation."""
        # Preserve time and z axis for plotting these inof in rendertemplate
        geo = None  # to make flake8 happy
        returned = {}
        taxis = data1.getTime()
        if data1.ndim > 2:
            zaxis = data1.getAxis(-3)
        else:
            zaxis = None

        # Ok get3 only the last 2 dims
        data1 = self._context().trimData2D(data1)
        data2 = self._context().trimData2D(data2)

        gridGenDict = vcs2vtk.genGridOnPoints(data1, gm, deep=False, grid=grid,
                                              geo=transform)
        for k in ['vtk_backend_grid', 'xm', 'xM', 'ym', 'yM', 'continents',
                  'wrap', 'geo']:
            exec("%s = gridGenDict['%s']" % (k, k))
        grid = gridGenDict['vtk_backend_grid']
        self._dataWrapModulo = gridGenDict['wrap']

        returned["vtk_backend_grid"] = grid
        returned["vtk_backend_geo"] = geo
        missingMapper = vcs2vtk.putMaskOnVTKGrid(data1, grid, None, False,
                                                 deep=False)

        # None/False are for color and cellData
        # (sent to vcs2vtk.putMaskOnVTKGrid)
        returned["vtk_backend_missing_mapper"] = (missingMapper, None, False)

        w = vcs2vtk.generateVectorArray(data1, data2, grid)

        grid.GetPointData().AddArray(w)

        # Vector attempt
        l = gm.line
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
        if gm.linewidth is not None:
            lwidth = gm.linewidth  # noqa
        if gm.linecolor is not None:
            lcolor = gm.linecolor

        arrow = vtk.vtkGlyphSource2D()
        arrow.SetGlyphTypeToArrow()
        arrow.FilledOff()

        glyphFilter = vtk.vtkGlyph2D()
        glyphFilter.SetInputData(grid)
        glyphFilter.SetInputArrayToProcess(1, 0, 0, 0, "vectors")
        glyphFilter.SetSourceConnection(arrow.GetOutputPort())
        glyphFilter.SetVectorModeToUseVector()

        # Rotate arrows to match vector data:
        glyphFilter.OrientOn()

        # Scale to vector magnitude:
        glyphFilter.SetScaleModeToScaleByVector()
        glyphFilter.SetScaleFactor(2. * gm.scale)

        # These are some unfortunately named methods. It does *not* clamp the
        # scale range to [min, max], but rather remaps the range
        # [min, max] --> [0, 1].
        glyphFilter.ClampingOn()
        glyphFilter.SetRange(0.01, 1.0)

        mapper = vtk.vtkPolyDataMapper()
        mapper.SetInputConnection(glyphFilter.GetOutputPort())
        act = vtk.vtkActor()
        act.SetMapper(mapper)

        cmap = self._context().canvas.getcolormapname()
        cmap = vcs.elements["colormap"][cmap]
        r, g, b = cmap.index[lcolor]
        act.GetProperty().SetColor(r / 100., g / 100., b / 100.)

        x1, x2, y1, y2 = vcs.utils.getworldcoordinates(gm, data1.getAxis(-1),
                                                       data1.getAxis(-2))

        act = vcs2vtk.doWrap(act, [x1, x2, y1, y2], self._dataWrapModulo)
        self._context.fitToViewport(act, [tmpl.data.x1, tmpl.data.x2,
                                          tmpl.data.y1, tmpl.data.y2],
                                    [x1, x2, y1, y2],
                                    priority=tmpl.data.priority,
                                    create_renderer=True)

        returned.update(
            self._context().renderTemplate(tmpl, data1, gm, taxis, zaxis))

        if self._context().canvas._continents is None:
            continents = False
        if continents:
            projection = vcs.elements["projection"][gm.projection]
            self._context().plotContinents(x1, x2, y1, y2, projection,
                                           self._dataWrapModulo, tmpl)

        returned["vtk_backend_actors"] = [[act, [x1, x2, y1, y2]]]
        returned["vtk_backend_glyphfilters"] = [glyphFilter]
        returned["vtk_backend_luts"] = [[None, None]]

        return returned
