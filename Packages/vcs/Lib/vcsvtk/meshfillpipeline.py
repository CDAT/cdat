from .pipeline2d import Pipeline2D
from .. import vcs2vtk
import fillareautils

import numpy
import vcs
import vtk


class MeshfillPipeline(Pipeline2D):

    """Implementation of the Pipeline interface for VCS meshfill plots."""

    def __init__(self, gm, context_):
        super(MeshfillPipeline, self).__init__(gm, context_)

        self._patternActors = []

    def _updateScalarData(self):
        """Overrides baseclass implementation."""
        # We don't trim _data2 for meshfill:
        self._data1 = self._context().trimData2D(self._originalData1)
        self._data2 = self._originalData2

    def _updateContourLevelsAndColors(self):
        self._updateContourLevelsAndColorsGeneric()

    def _createPolyDataFilter(self):
        """Overrides baseclass implementation."""
        self._vtkPolyDataFilter = vtk.vtkDataSetSurfaceFilter()
        if self._useCellScalars:
            p2c = vtk.vtkPointDataToCellData()
            p2c.SetInputData(self._vtkDataSet)
            self._vtkPolyDataFilter.SetInputConnection(p2c.GetOutputPort())
        else:
            self._vtkPolyDataFilter.SetInputData(self._vtkDataSet)

    def _plotInternal(self):

        prepedContours = self._prepContours()
        tmpLevels = prepedContours["tmpLevels"]
        tmpIndices = prepedContours["tmpIndices"]
        tmpColors = prepedContours["tmpColors"]
        tmpOpacities = prepedContours["tmpOpacities"]

        style = self._gm.fillareastyle
        # self._patternActors = []

        mappers = []
        luts = []
        geos = []
        wholeDataMin, wholeDataMax = vcs.minmax(self._originalData1)
        plotting_dataset_bounds = self.getPlottingBounds()
        x1, x2, y1, y2 = plotting_dataset_bounds
        _colorMap = self.getColorMap()
        self._patternActors = []
        for i, l in enumerate(tmpLevels):
            # Ok here we are trying to group together levels can be, a join
            # will happen if: next set of levels contnues where one left off
            # AND pattern is identical
            # TODO this should really just be a single polydata that is
            # colored by scalars:
            for j, color in enumerate(tmpColors[i]):
                mapper = vtk.vtkPolyDataMapper()
                lut = vtk.vtkLookupTable()
                th = vtk.vtkThreshold()
                th.ThresholdBetween(l[j], l[j + 1])
                th.SetInputConnection(self._vtkPolyDataFilter.GetOutputPort())
                geoFilter2 = vtk.vtkDataSetSurfaceFilter()
                geoFilter2.SetInputConnection(th.GetOutputPort())
                # Make the polydata output available here for patterning later
                geoFilter2.Update()
                geos.append(geoFilter2)
                mapper.SetInputConnection(geoFilter2.GetOutputPort())
                lut.SetNumberOfTableValues(1)
                r, g, b, a = self.getColorIndexOrRGBA(_colorMap, color)
                if style == 'solid':
                    tmpOpacity = tmpOpacities[j]
                    if tmpOpacity is None:
                        tmpOpacity = a / 100.
                    else:
                        tmpOpacity = tmpOpacities[j] / 100.
                    lut.SetTableValue(
                        0, r / 100., g / 100., b / 100., tmpOpacity)
                else:
                    lut.SetTableValue(0, 1., 1., 1., 0.)
                mapper.SetLookupTable(lut)
                mapper.SetScalarRange(l[j], l[j + 1])
                luts.append([lut, [l[j], l[j + 1], True]])
                # Store the mapper only if it's worth it?
                # Need to do it with the whole slab min/max for animation
                # purposes
                if not (l[j + 1] < wholeDataMin or l[j] > wholeDataMax):
                    mappers.append(mapper)

            # Since pattern creation requires a single color, assuming the
            # first
            c = self.getColorIndexOrRGBA(_colorMap, tmpColors[i][0])
            act = fillareautils.make_patterned_polydata(geoFilter2.GetOutput(),
                                                        fillareastyle=style,
                                                        fillareaindex=tmpIndices[i],
                                                        fillareacolors=c,
                                                        fillareaopacity=tmpOpacities[i],
                                                        size=(x2 - x1, y2 - y1))
            if act is not None:
                self._patternActors.append(act)

        self._resultDict["vtk_backend_luts"] = luts
        if len(geos) > 0:
            self._resultDict["vtk_backend_geofilters"] = geos

        """
        numLevels = len(self._contourLevels)
        if mappers == []:  # ok didn't need to have special banded contours
            mapper = vtk.vtkPolyDataMapper()
            mappers = [mapper]
            # Colortable bit
            # make sure length match
            while len(self._contourColors) < numLevels:
                self._contourColors.append(self._contourColors[-1])

            lut = vtk.vtkLookupTable()
            lut.SetNumberOfTableValues(numLevels)
            for i in range(numLevels):
                r, g, b, a = self._colorMap.index[self._contourColors[i]]
                lut.SetTableValue(i, r / 100., g / 100., b / 100., a / 100.)

            mapper.SetLookupTable(lut)
            if numpy.allclose(self._contourLevels[0], -1.e20):
                lmn = self._min - 1.
            else:
                lmn = self._contourLevels[0]
            if numpy.allclose(self._contourLevels[-1], 1.e20):
                lmx = self._max + 1.
            else:
                lmx = self._contourLevels[-1]
            mapper.SetScalarRange(lmn, lmx)
            self._resultDict["vtk_backend_luts"] = [[lut, [lmn, lmx, True]]]
            """

        if self._maskedDataMapper is not None:
            # Note that this is different for meshfill -- others prepend.
            mappers.append(self._maskedDataMapper)

        # Add a second mapper for wireframe meshfill:
        if self._gm.mesh:
            lineMappers = []
            wireLUT = vtk.vtkLookupTable()
            wireLUT.SetNumberOfTableValues(1)
            wireLUT.SetTableValue(0, 0, 0, 0)
            for polyMapper in mappers:
                lineMapper = vtk.vtkPolyDataMapper()
                lineMapper.SetInputConnection(
                    polyMapper.GetInputConnection(0, 0))
                lineMapper._useWireFrame = True

                # 'noqa' comments disable pep8 checking for these lines. There
                # is not a readable way to shorten them due to the unwieldly
                # method name.
                #
                # Setup depth resolution so lines stay above points:
                polyMapper.SetResolveCoincidentTopologyPolygonOffsetParameters(0, 1)  # noqa
                polyMapper.SetResolveCoincidentTopologyToPolygonOffset()
                lineMapper.SetResolveCoincidentTopologyPolygonOffsetParameters(1, 1)  # noqa
                lineMapper.SetResolveCoincidentTopologyToPolygonOffset()
                lineMapper.SetLookupTable(wireLUT)

                lineMappers.append(lineMapper)
            mappers.extend(lineMappers)

        # And now we need actors to actually render this thing
        actors = []
        vp = [self._template.data.x1, self._template.data.x2,
              self._template.data.y1, self._template.data.y2]
        for mapper in mappers:
            act = vtk.vtkActor()
            act.SetMapper(mapper)

            if hasattr(mapper, "_useWireFrame"):
                prop = act.GetProperty()
                prop.SetRepresentationToWireframe()

            if self._vtkGeoTransform is None:
                # If using geofilter on wireframed does not get wrppaed not
                # sure why so sticking to many mappers
                act = vcs2vtk.doWrap(act, plotting_dataset_bounds,
                                     self._dataWrapModulo)

            # TODO See comment in boxfill.
            if mapper is self._maskedDataMapper:
                actors.append([act, self._maskedDataMapper, plotting_dataset_bounds])
            else:
                actors.append([act, plotting_dataset_bounds])

            # create a new renderer for this mapper
            # (we need one for each mapper because of cmaera flips)
            self._context().fitToViewportBounds(
                act, vp,
                wc=plotting_dataset_bounds, geoBounds=self._vtkDataSet.GetBounds(),
                geo=self._vtkGeoTransform,
                priority=self._template.data.priority,
                create_renderer=True)

        for act in self._patternActors:
            if self._vtkGeoTransform is None:
                # If using geofilter on wireframed does not get wrapped not sure
                # why so sticking to many mappers
                self._context().fitToViewportBounds(
                    act, vp,
                    wc=plotting_dataset_bounds, geoBounds=self._vtkDataSet.GetBounds(),
                    geo=self._vtkGeoTransform,
                    priority=self._template.data.priority,
                    create_renderer=True)
                actors.append([act, plotting_dataset_bounds])

        self._resultDict["vtk_backend_actors"] = actors

        self._template.plot(self._context().canvas, self._data1, self._gm,
                            bg=self._context().bg,
                            X=numpy.arange(min(x1, x2),
                                           max(x1, x2) * 1.1,
                                           abs(x2 - x1) / 10.),
                            Y=numpy.arange(min(y1, y2),
                                           max(y1, y2) * 1.1,
                                           abs(y2 - y1) / 10.),
                            vtk_backend_grid=self._vtkDataSet,
                            dataset_bounds=self._vtkDataSetBounds,
                            plotting_dataset_bounds=plotting_dataset_bounds)

        legend = getattr(self._gm, "legend", None)

        if self._gm.ext_1:
            if isinstance(self._contourLevels[0], list):
                if numpy.less(abs(self._contourLevels[0][0]), 1.e20):
                    # Ok we need to add the ext levels
                    self._contourLevels.insert(
                        0, [-1.e20, self._contourLevels[0][0]])
            else:
                if numpy.less(abs(self._contourLevels[0]), 1.e20):
                    # need to add an ext
                    self._contourLevels.insert(0, -1.e20)
        if self._gm.ext_2:
            if isinstance(self._contourLevels[-1], list):
                if numpy.less(abs(self._contourLevels[-1][1]), 1.e20):
                    # need ext
                    self._contourLevels.append([self._contourLevels[-1][1],
                                                1.e20])
            else:
                if numpy.less(abs(self._contourLevels[-1]), 1.e20):
                    # need exts
                    self._contourLevels.append(1.e20)

        patternArgs = {}
        patternArgs['style'] = self._gm.fillareastyle
        patternArgs['index'] = self._gm.fillareaindices
        if patternArgs['index'] is None:
            patternArgs['index'] = [1, ]
        patternArgs['opacity'] = self._gm.fillareaopacity
        self._resultDict.update(
            self._context().renderColorBar(self._template, self._contourLevels,
                                           self._contourColors,
                                           legend,
                                           self.getColorMap(),
                                           **patternArgs))

        if self._context().canvas._continents is None:
            self._useContinents = False
        if self._useContinents:
            projection = vcs.elements["projection"][self._gm.projection]
            self._context().plotContinents(plotting_dataset_bounds, projection,
                                           self._dataWrapModulo,
                                           vp, self._template.data.priority,
                                           vtk_backend_grid=self._vtkDataSet,
                                           dataset_bounds=self._vtkDataSetBounds)

    def getPlottingBounds(self):
        """gm.datawc if it is set or dataset_bounds
        """
        if (self._vtkGeoTransform):
            return vcs2vtk.getWrappedBounds(
                [self._gm.datawc_x1, self._gm.datawc_x2, self._gm.datawc_y1, self._gm.datawc_y2],
                self._vtkDataSetBounds, self._dataWrapModulo)
        else:
            return vcs2vtk.getPlottingBounds(
                [self._gm.datawc_x1, self._gm.datawc_x2, self._gm.datawc_y1, self._gm.datawc_y2],
                self._vtkDataSetBounds, self._vtkGeoTransform)
