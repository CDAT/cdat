from .pipeline import Pipeline
from .. import vcs2vtk

import vcs


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
            point or cell scalars.
        - _vtkGeoTransform: The vtkGeoTransform object associated with this
            pipeline.
        - _vtkDataSetBounds: The bounds of _vtkDataSet as
            tuple(float xMin, float xMax, float yMin, float yMax)
        - _vtkPolyDataFilter: A vtkAlgorithm that produces a polydata
            representation of the data.
        - _colorMap: The vcs colormap object used to color the scalar data.
        - _useContinents: Whether or not to plot continents.
        - _dataWrapModulo: Wrap modulo as [YMax, XMax], in degrees. 0 means
            'no wrapping'.
        - _useCellScalars: True if data is applied to cell, false if data is
            applied to points.
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
        self._useCellScalars = None
        self._scalarRange = None
        self._maskedDataMapper = None

    def _updateScalarData(self):
        """Create _data1 and _data2 from _originalData1 and _originalData2."""
        raise NotImplementedError("Missing override.")

    def _updateVTKDataSet(self):
        """Apply the vcs data to _vtkDataSet, creating it if necessary."""
        raise NotImplementedError("Missing override.")

    def _updateFromGenGridDict(self, genGridDict):
        """Use the vcs2vtk.genGrid(...) result to update this pipeline.

        This method should be called from the _updateVTKDataSet override.
        """
        raise NotImplementedError("Missing override.")

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

    def plot(self, data1, data2, tmpl, grid, transform):
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
        self._scalarRange = vcs.minmax(self._data1)

        # Create/update the VTK dataset.
        self._updateVTKDataSet()

        # Update the results:
        self._resultDict["vtk_backend_grid"] = self._vtkDataSet
        self._resultDict["vtk_backend_geo"] = self._vtkGeoTransform
        self._resultDict["vtk_backend_wrap"] = self._dataWrapModulo

        # Determine and format the contouring information:
        self._updateContourLevelsAndColors()

        # Generate a mapper to render masked data:
        self._createMaskedDataMapper()

        # Create the polydata filter:
        self._createPolyDataFilter()

        # Plot specific rendering:
        self._plotInternal()

        return self._resultDict

    def _updateScalarData(self):
        """Overrides baseclass implementation."""
        self._data1 = self._context().trimData2D(self._originalData1)
        self._data2 = self._context().trimData2D(self._originalData2)
        self._min = self._data1.min()
        self._max = self._data1.max()

    def _updateVTKDataSet(self):
        """Overrides baseclass implementation."""
        genGridDict = vcs2vtk.genGrid(self._data1, self._data2, self._gm,
                                      deep=False,
                                      grid=self._vtkDataSet,
                                      geo=self._vtkGeoTransform)
        self._updateFromGenGridDict(genGridDict)

        data = vcs2vtk.numpy_to_vtk_wrapper(self._data1.filled(0.).flat,
                                            deep=False)
        if self._useCellScalars:
            self._vtkDataSet.GetCellData().SetScalars(data)
        else:
            self._vtkDataSet.GetPointData().SetScalars(data)

    def _updateFromGenGridDict(self, genGridDict):
        """Overrides baseclass implementation."""
        self._vtkDataSet = genGridDict['vtk_backend_grid']
        self._vtkDataSetBounds = (genGridDict['xm'], genGridDict['xM'],
                                  genGridDict['ym'], genGridDict['yM'])
        self._useContinents = genGridDict['continents']
        self._dataWrapModulo = genGridDict['wrap']
        self._vtkGeoTransform = genGridDict['geo']
        self._useCellScalars = genGridDict['cellData']

    def _createMaskedDataMapper(self):
        """Overrides baseclass implementation."""
        color = getattr(self._gm, "missing", None)
        _colorMap = self.getColorMap()
        if color is not None:
            color = _colorMap.index[color]
        self._maskedDataMapper = vcs2vtk.putMaskOnVTKGrid(
            self._data1, self._vtkDataSet, color, self._useCellScalars,
            deep=False)

        self._resultDict["vtk_backend_missing_mapper"] = (
            self._maskedDataMapper, color, self._useCellScalars)
