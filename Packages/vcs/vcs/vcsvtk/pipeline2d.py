from .pipeline import Pipeline
from .. import vcs2vtk

import fillareautils
import numpy
import vcs
import vtk
import warnings


class IPipeline2D(Pipeline):

    """Interface class for Pipeline2D.

    Defines the virtual method API for subclasses of Pipeline2D.

    This class is an interface. It only contains stubs defining the expected
    methods for concrete subclasses. Any shared implementations should go into
    Pipeline2D.

    Internal methods:
        Documented inline.

    Internal variables:
        - _resultDict: The 'returned' dictionary from the original
            VTKPlots.plot2D method. This should be refactored out soon and use
            a documented API for what this needs.
        - _gm: The graphics method object.
        - _template: The vcs template object.
        - _originalData1: The original data1 object.
        - _originalData2: The original data2 object.
        - _data1: The _originalData1 object modified for this pipeline.
        - _data2: The _originalData2 object modified for this pipeline.
        - _contourLevels: List of contour levels.
        - _contourColors: List of contour colors.
        - _vtkDataSet: The vtkDataSet object with _trimmedData[1|2] set as
            point or cell scalars. The datset is already transformed through
            the geographic projection if there is one.
        - _vtkGeoTransform: The vtkGeoTransform object associated with this
            pipeline.
        - _vtkDataSetBounds: The bounds of _vtkDataSet, in lon/lat space, as
            tuple(float xMin, float xMax, float yMin, float yMax)
        - _vtkPolyDataFilter: A vtkAlgorithm that produces a polydata
            representation of the data.
        - _colorMap: The vcs colormap object used to color the scalar data.
        - _useContinents: Whether or not to plot continents.
        - _dataWrapModulo: Wrap modulo as [YMax, XMax], in degrees. 0 means
            'no wrapping'.
        - _hasCellData: True if data is applied to cell, false if data is
            applied to points.
        - _needsCellData: True if the plot needs cell scalars, false if
            the plot needs point scalars
        - _needsVectors: True if the plot needs vectors, false if it needs scalars
        - _scalarRange: The range of _data1 as tuple(float min, float max)
        - _maskedDataMapper: The mapper used to render masked data.
    """

    def __init__(self, gm, context_):
        super(IPipeline2D, self).__init__(gm, context_)

        # TODO This should be replaced by getters that retrieve the info
        # needed, or document the members of the map somewhere. Much of this
        # map can be replaced by setting up and maintaining a pipeline, as most
        # objects in this map are used by VTKPlots.update_input to manually
        # reexecute visualization operations.
        self._resultDict = None

        self._template = None
        self._originalData1 = None
        self._originalData2 = None
        self._data1 = None
        self._data2 = None
        self._contourLevels = None
        self._contourColors = None
        self._vtkDataSet = None
        self._vtkGeoTransform = None
        self._vtkDataSetBounds = None
        self._colorMap = None
        self._useContinents = None
        self._dataWrapModulo = None
        self._hasCellData = None
        self._needsCellData = None
        self._needsVectors = False
        self._scalarRange = None
        self._maskedDataMapper = None

    def _updateScalarData(self):
        """Create _data1 and _data2 from _originalData1 and _originalData2."""
        raise NotImplementedError("Missing override.")

    def _updateVTKDataSet(self, plotBasedDualGrid):
        """Apply the vcs data to _vtkDataSet, creating it if necessary."""
        raise NotImplementedError("Missing override.")

    def _updateFromGenGridDict(self, genGridDict):
        """Use the vcs2vtk.genGrid(...) result to update this pipeline.

        This method should be called from the _updateVTKDataSet override.
        """
        raise NotImplementedError("Missing override.")

    def _updateContourLevelsAndColorsGeneric(self):
        # Contour values:
        self._contourLevels = self._gm.levels
        if numpy.allclose(self._contourLevels[0], [0., 1.e20]) or \
                numpy.allclose(self._contourLevels, 1.e20):
            levs2 = vcs.mkscale(self._scalarRange[0],
                                self._scalarRange[1])
            if len(levs2) == 1:  # constant value ?
                levs2 = [levs2[0], levs2[0] + .00001]
            self._contourLevels = []
            if self._gm.ext_1:
                # user wants arrow at the end
                levs2[0] = -1.e20
            if self._gm.ext_2:
                # user wants arrow at the end
                levs2[-1] = 1.e20
            for i in range(len(levs2) - 1):
                self._contourLevels.append([levs2[i], levs2[i + 1]])
        else:
            if not isinstance(self._gm.levels[0], (list, tuple)):
                self._contourLevels = []
                levs2 = self._gm.levels
                if numpy.allclose(levs2[0], 1.e20):
                    levs2[0] = -1.e20
                for i in range(len(levs2) - 1):
                    self._contourLevels.append([levs2[i], levs2[i + 1]])
            else:
                levs2 = self._gm.levels

        if isinstance(self._contourLevels, numpy.ndarray):
            self._contourLevels = self._contourLevels.tolist()

        # Figure out colors
        self._contourColors = self._gm.fillareacolors
        if self._contourColors in [[1], None, [[0.0, 0.0, 0.0, 100.0]]]:
            # TODO BUG It's possible that levs2 may not exist here...
            self._contourColors = vcs.getcolors(levs2, split=0)
            if isinstance(self._contourColors, (int, float)):
                self._contourColors = [self._contourColors]

    def _updateContourLevelsAndColors(self):
        """This method prepares the _contourLevels and _contourColors variables.
        """
        raise NotImplementedError("Missing override.")

    def _createPolyDataFilter(self):
        """Create and initialize _vtkPolyDataFilter."""
        raise NotImplementedError("Missing override.")

    def _createMaskedDataMapper(self):
        """Create _maskedDataMapper for rendering masked data.

        The mapper may be None if not needed.
        """
        raise NotImplementedError("Missing override.")

    def _plotInternal(self):
        """Used by subclasses to carry out plot-specific rendering."""
        raise NotImplementedError("Missing override.")


class Pipeline2D(IPipeline2D):

    """Common VTK pipeline functionality for 2D VCS plot."""

    def __init__(self, gm, context_):
        super(Pipeline2D, self).__init__(gm, context_)

    def _patternCreation(self, vtkFilter, color, style, index, opacity):
        """ Creates pattern things """
        c = [val * 255 / 100.0 for val in color]
        if opacity is None:
            opacity = c[-1]
        else:
            opacity = opacity * 255 / 100.
        act = fillareautils.make_patterned_polydata(vtkFilter.GetOutput(),
                                                    fillareastyle=style,
                                                    fillareaindex=index,
                                                    fillareacolors=c,
                                                    fillareaopacity=opacity)
        if act is not None:
            self._patternActors.append(act)
        return

    def _prepContours(self):
        """ Prep contours bands"""
        tmpLevels = []
        tmpColors = []
        tmpIndices = []
        tmpOpacities = []

        indices = self._gm.fillareaindices
        opacities = self._gm.fillareaopacity
        style = self._gm.fillareastyle

        if indices is None:
            indices = [1]
        while len(indices) < len(self._contourColors):
            indices.append(indices[-1])
        if len(self._contourLevels) > len(self._contourColors):
            raise RuntimeError(
                "You asked for %i levels but provided only %i colors\n"
                "Graphic Method: %s of type %s\nLevels: %s"
                % (len(self._contourLevels), len(self._contourColors),
                   self._gm.name, self._gm.g_name,
                   repr(self._contourLevels)))
        elif len(self._contourLevels) < len(self._contourColors) - 1:
            warnings.warn(
                "You asked for %i lgridevels but provided %i colors, "
                "extra ones will be ignored\nGraphic Method: %s of type %s"
                % (len(self._contourLevels), len(self._contourColors),
                   self._gm.name, self._gm.g_name))
        if len(opacities) < len(self._contourColors):
            # fill up the opacity values
            opacities += [None] * (len(self._contourColors) - len(opacities))

        # The following loop attempts to group isosurfaces based on their
        # attributes. Isosurfaces grouped if and only if all properties match.
        for i, l in enumerate(self._contourLevels):
            if i == 0:
                C = [self._contourColors[i]]
                if style == "pattern":
                    C = [(0., 0., 0., 100.)]
                if numpy.allclose(self._contourLevels[0][0], -1.e20):
                    # ok it's an extension arrow
                    L = [self._scalarRange[0] - 1., self._contourLevels[0][1]]
                else:
                    L = list(self._contourLevels[i])
                I = indices[i]
                O = opacities[i]
            else:
                if l[0] == L[-1] and\
                        ((style == 'solid') or
                            (I == indices[i] and C[-1] == self._contourColors[i] and
                             O == opacities[i])):
                    # Ok same type lets keep going
                    if numpy.allclose(l[1], 1.e20):
                        L.append(self._scalarRange[1] + 1.)
                    else:
                        L.append(l[1])
                    C.append(self._contourColors[i])
                    tmpOpacities.append(O)
                    O = opacities[i]
                else:  # ok we need new contouring
                    tmpLevels.append(L)
                    tmpColors.append(C)
                    tmpIndices.append(I)
                    tmpOpacities.append(O)
                    C = [self._contourColors[i]]
                    L = self._contourLevels[i]
                    I = indices[i]
                    O = opacities[i]
        tmpLevels.append(L)
        tmpColors.append(C)
        tmpIndices.append(I)
        tmpOpacities.append(O)

        result = {
            "tmpLevels": tmpLevels,
            "tmpColors": tmpColors,
            "tmpIndices": tmpIndices,
            "tmpOpacities": tmpOpacities,
        }

        return result

    def plot(self, data1, data2, tmpl, grid, transform, **kargs):
        """Overrides baseclass implementation."""
        # Clear old results:
        self._resultDict = {}

        self._template = tmpl
        self._originalData1 = data1
        self._originalData2 = data2
        self._vtkDataSet = grid
        self._vtkGeoTransform = transform

        # Preprocess the input scalar data:
        self._updateScalarData()
        self._min = self._data1.min()
        self._max = self._data1.max()
        self._scalarRange = vcs.minmax(self._data1)

        # Create/update the VTK dataset.
        plotBasedDualGrid = kargs.get('plot_based_dual_grid', True)
        self._updateVTKDataSet(plotBasedDualGrid)

        # Update the results:
        self._resultDict["vtk_backend_grid"] = self._vtkDataSet
        self._resultDict["vtk_backend_geo"] = self._vtkGeoTransform
        self._resultDict["vtk_backend_wrap"] = self._dataWrapModulo
        self._resultDict["dataset_bounds"] = self._vtkDataSetBounds

        # Determine and format the contouring information:
        self._updateContourLevelsAndColors()

        # Generate a mapper to render masked data:
        self._createMaskedDataMapper()

        # Create the polydata filter:
        self._createPolyDataFilter()

        if (kargs.get('ratio', '0') == 'autot' and
                # atot is implemented for linear plots at vcs level
                # for geographic projections we implement it here.
                self._vtkGeoTransform):
            self._resultDict['ratio_autot_viewport'] = self._processRatioAutot(
                self._template, self._vtkDataSet)

        # Plot specific rendering:
        self._plotInternal()

        return self._resultDict

    def _updateScalarData(self):
        """Overrides baseclass implementation."""
        self._data1 = self._context().trimData2D(self._originalData1)
        self._data2 = self._context().trimData2D(self._originalData2)

    def _updateVTKDataSet(self, plotBasedDualGrid):
        """
        """
        if (plotBasedDualGrid):
            hasCellData = self._data1.hasCellData()
            dualGrid = (hasCellData != self._needsCellData)
        else:
            dualGrid = False
        genGridDict = vcs2vtk.genGrid(self._data1, self._data2, self._gm,
                                      deep=False,
                                      grid=self._vtkDataSet,
                                      geo=self._vtkGeoTransform, genVectors=self._needsVectors,
                                      dualGrid=dualGrid)
        self._data1 = genGridDict["data"]
        self._data2 = genGridDict["data2"]
        self._updateFromGenGridDict(genGridDict)

    def _createPolyDataFilter(self):
        """This is only used when we use the grid stored in the file for all plots."""
        self._vtkPolyDataFilter = vtk.vtkDataSetSurfaceFilter()
        if self._hasCellData == self._needsCellData:
            self._vtkPolyDataFilter.SetInputData(self._vtkDataSet)
        elif self._hasCellData:
            # use cells but needs points
            c2p = vtk.vtkCellDataToPointData()
            c2p.PassCellDataOn()
            c2p.SetInputData(self._vtkDataSet)
            self._vtkPolyDataFilter.SetInputConnection(c2p.GetOutputPort())
        else:
            # use points but needs cells
            p2c = vtk.vtkPointDataToCellData()
            p2c.SetInputData(self._vtkDataSet)
            # For contouring duplicate points seem to confuse it
            self._vtkPolyDataFilter.SetInputConnection(p2c.GetOutputPort())
        self._vtkPolyDataFilter.Update()
        self._resultDict["vtk_backend_filter"] = self._vtkPolyDataFilter
        # create an actor and a renderer for the surface mesh.
        # this is used for displaying point information using the hardware selection
        mapper = vtk.vtkPolyDataMapper()
        mapper.SetInputConnection(self._vtkPolyDataFilter.GetOutputPort())
        act = vtk.vtkActor()
        act.SetMapper(mapper)
        vp = self._resultDict.get(
            'ratio_autot_viewport',
            [self._template.data.x1, self._template.data.x2,
             self._template.data.y1, self._template.data.y2])
        plotting_dataset_bounds = self.getPlottingBounds()
        surface_renderer, xScale, yScale = self._context().fitToViewport(
            act, vp,
            wc=plotting_dataset_bounds, geoBounds=self._vtkDataSet.GetBounds(),
            geo=self._vtkGeoTransform,
            priority=self._template.data.priority,
            create_renderer=True)
        self._resultDict['surface_renderer'] = surface_renderer
        self._resultDict['surface_scale'] = (xScale, yScale)
        if (surface_renderer):
            surface_renderer.SetDraw(False)

    def _updateFromGenGridDict(self, genGridDict):
        """Overrides baseclass implementation."""
        self._vtkDataSet = genGridDict['vtk_backend_grid']
        self._vtkDataSetBounds = (genGridDict['xm'], genGridDict['xM'],
                                  genGridDict['ym'], genGridDict['yM'])
        self._useContinents = genGridDict['continents']
        self._dataWrapModulo = genGridDict['wrap']
        self._vtkGeoTransform = genGridDict['geo']
        self._hasCellData = genGridDict['cellData']

    def _createMaskedDataMapper(self):
        """Overrides baseclass implementation."""
        color = getattr(self._gm, "missing", None)
        _colorMap = self.getColorMap()
        if color is not None:
            color = self.getColorIndexOrRGBA(_colorMap, color)
        self._maskedDataMapper = vcs2vtk.putMaskOnVTKGrid(
            self._data1, self._vtkDataSet, color, self._hasCellData,
            deep=False)

        self._resultDict["vtk_backend_missing_mapper"] = (
            self._maskedDataMapper, color, self._hasCellData)

    def getPlottingBounds(self):
        """gm.datawc if it is set or dataset_bounds if there is not geographic projection
           wrapped bounds otherwise
        """
        if (self._vtkGeoTransform):
            return vcs2vtk.getWrappedBounds(
                vcs.utils.getworldcoordinates(self._gm,
                                              self._data1.getAxis(-1),
                                              self._data1.getAxis(-2)),
                self._vtkDataSetBounds, self._dataWrapModulo)
        else:
            return vcs2vtk.getPlottingBounds(
                vcs.utils.getworldcoordinates(self._gm,
                                              self._data1.getAxis(-1),
                                              self._data1.getAxis(-2)),
                self._vtkDataSetBounds, self._vtkGeoTransform)
