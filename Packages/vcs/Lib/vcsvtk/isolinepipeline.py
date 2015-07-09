from .pipeline2d import Pipeline2D
from .. import vcs2vtk

import numpy
import vcs
import vtk


class IsolinePipeline(Pipeline2D):
    """Implementation of the Pipeline interface for VCS isoline plots."""

    def __init__(self, context_):
        super(IsolinePipeline, self).__init__(context_)

    def _updateVTKDataSet(self):
        """Overrides baseclass implementation."""
        # Force point data for isoline/isofill
        genGridDict = vcs2vtk.genGridOnPoints(self._data1, self._gm,
                                              deep=False,
                                              grid=self._vtkDataSet,
                                              geo=self._vtkGeoTransform)
        genGridDict["cellData"] = False
        self._updateFromGenGridDict(genGridDict)

        data = vcs2vtk.numpy_to_vtk_wrapper(self._data1.filled(0.).flat,
                                            deep=False)
        self._vtkDataSet.GetPointData().SetScalars(data)

    def _updateContourLevelsAndColors(self):
        """Overrides baseclass implementation."""
        # Contour values:
        self._contourLevels = self._gm.levels
        if numpy.allclose(self._contourLevels[0], [0., 1.e20]) or \
           numpy.allclose(self._contourLevels, 1.e20):
            self._contourLevels = vcs.mkscale(self._scalarRange[0],
                                              self._scalarRange[1])
            if len(self._contourLevels) == 1:  # constant value ?
                self._contourLevels = [self._contourLevels[0],
                                       self._contourLevels[0] + .00001]
        else:
            if isinstance(self._gm.levels[0], (list, tuple)):
                self._contourLevels = [x[0] for x in self._gm.levels]
            else:
                if numpy.allclose(self._contourLevels[0], 1.e20):
                    self._contourLevels[0] = -1.e20

        # Contour colors:
        self._contourColors = self._gm.linecolors

    def _createPolyDataFilter(self):
        """Overrides baseclass implementation."""
        self._vtkPolyDataFilter = vtk.vtkDataSetSurfaceFilter()
        if self._useCellScalars:
            # Sets data to point instead of just cells
            c2p = vtk.vtkCellDataToPointData()
            c2p.SetInputData(self._vtkDataSet)
            c2p.Update()
            # For contouring duplicate points seem to confuse it
            self._vtkPolyDataFilter.SetInputConnection(c2p.GetOutputPort())
        else:
            self._vtkPolyDataFilter.SetInputData(self._vtkDataSet)
        self._resultDict["vtk_backend_filter"] = self._vtkPolyDataFilter

    def _plotInternal(self):
        """Overrides baseclass implementation."""
        numLevels = len(self._contourLevels)

        cot = vtk.vtkContourFilter()
        if self._useCellScalars:
            cot.SetInputConnection(self._vtkPolyDataFilter.GetOutputPort())
        else:
            cot.SetInputData(self._vtkDataSet)
        cot.SetNumberOfContours(numLevels)

        if self._contourLevels[0] == 1.e20:
            self._contourLevels[0] = -1.e20
        for i in range(numLevels):
            cot.SetValue(i, self._contourLevels[i])
        cot.SetValue(numLevels, self._contourLevels[-1])
        # TODO remove update
        cot.Update()

        mappers = []

        lut = vtk.vtkLookupTable()
        lut.SetNumberOfTableValues(len(self._contourColors))
        cmap = vcs.elements["colormap"][self._context.canvas.getcolormapname()]
        for i, col in enumerate(self._contourColors):
            r, g, b = cmap.index[col]
            lut.SetTableValue(i, r/100., g/100., b/100.)

        # Setup isoline labels
        if self._gm.label:
            # Setup label mapping array:
            tpropMap = vtk.vtkDoubleArray()
            tpropMap.SetNumberOfComponents(1)
            tpropMap.SetNumberOfTuples(numLevels)
            for i, val in enumerate(self._contourLevels):
                tpropMap.SetTuple(i, [val])

            # Prep text properties:
            tprops = vtk.vtkTextPropertyCollection()
            if self._gm.text or self._gm.textcolors:
                # Text objects:
                if self._gm.text:
                    texts = self._gm.text
                    while len(texts) < numLevels:
                        texts.append(texts[-1])
                else:
                    texts = [None] * len(self._gm.textcolors)

                # Custom colors:
                if self._gm.textcolors:
                    colorOverrides = self._gm.textcolors
                    while len(colorOverrides) < numLevels:
                        colorOverrides.append(colorOverrides[-1])
                else:
                    colorOverrides = [None] * len(self._gm.text)

                for tc, colorOverride in zip(texts, colorOverrides):
                    if vcs.queries.istextcombined(tc):
                        tt, to = tuple(tc.name.split(":::"))
                    elif tc is None:
                        tt = "default"
                        to = "default"
                    elif vcs.queries.istexttable(tc):
                        tt = tc.name
                        to = "default"
                    elif vcs.queries.istextorientation(tc):
                        to = tc.name
                        tt = "default"
                    if colorOverride is not None:
                        tt = vcs.createtexttable(None, tt)
                        tt.color = colorOverride
                        tt = tt.name
                    tprop = vtk.vtkTextProperty()
                    vcs2vtk.prepTextProperty(tprop,
                                             self._context.renWin.GetSize(),
                                             to, tt)
                    tprops.AddItem(tprop)
                    if colorOverride is not None:
                        del(vcs.elements["texttable"][tt])
            else:  # No text properties specified. Use the default:
                tprop = vtk.vtkTextProperty()
                vcs2vtk.prepTextProperty(tprop, self._context.renWin.GetSize())
                tprops.AddItem(tprop)
            self._resultDict["vtk_backend_contours_labels_text_properties"] = \
                tprops

            mapper = vtk.vtkLabeledContourMapper()
            mapper.SetTextProperties(tprops)
            mapper.SetTextPropertyMapping(tpropMap)
            mapper.SetLabelVisibility(1)

            pdMapper = mapper.GetPolyDataMapper()

            self._resultDict["vtk_backend_labeled_luts"] = [
                  [lut,
                   [self._contourLevels[0], self._contourLevels[-1], False]]]
        else:  # No isoline labels:
            mapper = vtk.vtkPolyDataMapper()
            pdMapper = mapper
            self._resultDict["vtk_backend_luts"] = \
                [[lut, [self._contourLevels[0],
                        self._contourLevels[-1], False]]]
        pdMapper.SetLookupTable(lut)
        pdMapper.SetScalarRange(self._contourLevels[0],
                                self._contourLevels[-1])
        pdMapper.SetScalarModeToUsePointData()

        stripper = vtk.vtkStripper()
        stripper.SetInputConnection(cot.GetOutputPort())
        mapper.SetInputConnection(stripper.GetOutputPort())
        # TODO remove update, make pipeline
        stripper.Update()
        mappers.append(mapper)
        self._resultDict["vtk_backend_contours"] = [cot]

        if self._maskedDataMapper is not None:
            mappers.insert(0, self._maskedDataMapper)

        x1, x2, y1, y2 = vcs.utils.getworldcoordinates(self._gm,
                                                       self._data1.getAxis(-1),
                                                       self._data1.getAxis(-2))

        # And now we need actors to actually render this thing
        actors = []
        for mapper in mappers:
            act = vtk.vtkActor()
            act.SetMapper(mapper)

            if self._vtkGeoTransform is None:
                # If using geofilter on wireframed does not get wrppaed not
                # sure why so sticking to many mappers
                act = vcs2vtk.doWrap(act, [x1, x2, y1, y2],
                                     self._dataWrapModulo)

            # TODO See comment in boxfill.
            if mapper is self._maskedDataMapper:
                actors.append([act, self._maskedDataMapper, [x1, x2, y1, y2]])
            else:
                actors.append([act, [x1, x2, y1, y2]])

            # create a new renderer for this mapper
            # (we need one for each mapper because of cmaera flips)
            ren = self._context.fitToViewport(
                  act, [self._template.data.x1, self._template.data.x2,
                        self._template.data.y1, self._template.data.y2],
                  wc=[x1, x2, y1, y2], geo=self._vtkGeoTransform,
                  priority=self._template.data.priority,
                  create_renderer=True)

        self._resultDict["vtk_backend_actors"] = actors

        t = self._originalData1.getTime()
        if self._originalData1.ndim > 2:
            z = self._originalData1.getAxis(-3)
        else:
            z = None

        self._resultDict.update(self._context.renderTemplate(self._template,
                                                             self._data1,
                                                             self._gm, t, z))

        if self._context.canvas._continents is None:
            self._useContinents = False
        if self._useContinents:
            projection = vcs.elements["projection"][self._gm.projection]
            self._context.plotContinents(x1, x2, y1, y2, projection,
                                         self._dataWrapModulo, self._template)
