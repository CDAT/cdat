from .pipeline2d import Pipeline2D
import fillareautils

import numpy
import vcs
import vtk


class BoxfillPipeline(Pipeline2D):

    """Implementation of the Pipeline interface for VCS boxfill plots.

    Internal variables:
        - self._contourLabels: Contour labels.
        - self._mappers: Mappers produced by this pipeline.
            TODO _mappers should be removed and replaced by a more specific
            set of ivars (at minimum, identify what the mappers are rendering).
    """

    def __init__(self, gm, context_):
        super(BoxfillPipeline, self).__init__(gm, context_)

        self._contourLabels = None
        self._mappers = None
        self._customBoxfillArgs = {}
        self._needsCellData = True

    def _updateScalarData(self):
        """Overrides baseclass implementation."""
        # Update data1 if this is a log10 boxfill:
        data = self._originalData1
        if self._gm.boxfill_type == "log10":
            data = numpy.ma.log10(data)

        self._data1 = self._context().trimData2D(data)
        self._data2 = self._context().trimData2D(self._originalData2)

    def _updateContourLevelsAndColors(self):
        """Overrides baseclass implementation."""
        if self._gm.boxfill_type != "custom":
            self._updateContourLevelsAndColorsForBoxfill()
        else:
            self._updateContourLevelsAndColorsGeneric()

        if isinstance(self._contourLevels, numpy.ndarray):
            self._contourLevels = self._contourLevels.tolist()

    def _updateContourLevelsAndColorsForBoxfill(self):
        """Set contour information for a standard boxfill."""
        self._contourLevels = self._gm.getlevels(self._scalarRange[0], self._scalarRange[1])
        self._contourLabels = self._gm.getlegendlabels(self._contourLevels)
        # Use consecutive colors:
        self._contourColors = range(self._gm.color_1, self._gm.color_2 + 1)

    def _plotInternal(self):
        """Overrides baseclass implementation."""
        # Special case for custom boxfills:
        if self._gm.boxfill_type != "custom":
            self._plotInternalBoxfill()
        else:
            self._plotInternalCustomBoxfill()

        if self._maskedDataMapper is not None:
            self._mappers.insert(0, self._maskedDataMapper)

        plotting_dataset_bounds = self.getPlottingBounds()
        x1, x2, y1, y2 = plotting_dataset_bounds

        # And now we need actors to actually render this thing
        actors = []
        patternActors = []
        cti = 0
        ctj = 0
        _colorMap = self.getColorMap()
        _style = self._gm.fillareastyle
        vp = self._resultDict.get(
            'ratio_autot_viewport',
            [self._template.data.x1, self._template.data.x2,
             self._template.data.y1, self._template.data.y2])
        dataset_renderer = None
        xScale, yScale = (1, 1)
        for mapper in self._mappers:
            act = vtk.vtkActor()
            act.SetMapper(mapper)

            # TODO We shouldn't need this conditional branch, the 'else' body
            # should be used and GetMapper called to get the mapper as needed.
            # If this is needed for other reasons, we need a comment explaining
            # why.
            if mapper is self._maskedDataMapper:
                actors.append([act, self._maskedDataMapper, plotting_dataset_bounds])
            else:
                actors.append([act, plotting_dataset_bounds])

                if self._gm.boxfill_type == "custom":
                    # Patterns/hatches creation for custom boxfill plots
                    patact = None

                    tmpColors = self._customBoxfillArgs["tmpColors"]
                    if ctj >= len(tmpColors[cti]):
                        ctj = 0
                        cti += 1
                    # Since pattern creation requires a single color, assuming the first
                    c = self.getColorIndexOrRGBA(_colorMap, tmpColors[cti][ctj])
                    patact = fillareautils.make_patterned_polydata(
                        mapper.GetInput(),
                        fillareastyle=_style,
                        fillareaindex=self._customBoxfillArgs["tmpIndices"][cti],
                        fillareacolors=c,
                        fillareaopacity=self._customBoxfillArgs["tmpOpacities"][cti],
                        size=(x2 - x1, y2 - y1))

                    ctj += 1

                    if patact is not None:
                        patternActors.append(patact)

            # create a new renderer for this mapper
            # (we need one for each mapper because of camera flips)
            dataset_renderer, xScale, yScale = self._context().fitToViewport(
                act, vp,
                wc=plotting_dataset_bounds, geoBounds=self._vtkDataSet.GetBounds(),
                geo=self._vtkGeoTransform,
                priority=self._template.data.priority,
                create_renderer=(dataset_renderer is None))

        for act in patternActors:
            if self._vtkGeoTransform is None:
                # If using geofilter on wireframed does not get wrapped not sure
                # why so sticking to many mappers
                self._context().fitToViewport(
                    act, vp,
                    wc=plotting_dataset_bounds, geoBounds=self._vtkDataSet.GetBounds(),
                    geo=self._vtkGeoTransform,
                    priority=self._template.data.priority,
                    create_renderer=True)
                actors.append([act, plotting_dataset_bounds])

        self._resultDict["vtk_backend_actors"] = actors

        t = self._originalData1.getTime()
        if self._originalData1.ndim > 2:
            z = self._originalData1.getAxis(-3)
        else:
            z = None
        kwargs = {"vtk_backend_grid": self._vtkDataSet,
                  "dataset_bounds": self._vtkDataSetBounds,
                  "plotting_dataset_bounds": plotting_dataset_bounds,
                  "vtk_backend_geo": self._vtkGeoTransform}
        if ("ratio_autot_viewport" in self._resultDict):
            kwargs["ratio_autot_viewport"] = vp
        self._resultDict.update(self._context().renderTemplate(
            self._template,
            self._data1,
            self._gm, t, z, **kwargs))

        if getattr(self._gm, "legend", None) is not None:
            self._contourLabels = self._gm.legend

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

        # Do not pass patterning parameters for color bar rendering if the
        # boxfill type is non-custom
        patternArgs = {}
        if self._gm.boxfill_type == "custom":
            patternArgs['style'] = self._gm.fillareastyle
            patternArgs['index'] = self._gm.fillareaindices
            patternArgs['opacity'] = self._gm.fillareaopacity

        self._resultDict.update(
            self._context().renderColorBar(self._template, self._contourLevels,
                                           self._contourColors,
                                           self._contourLabels,
                                           self.getColorMap(),
                                           **patternArgs))

        if self._context().canvas._continents is None:
            self._useContinents = False
        if self._useContinents:
            projection = vcs.elements["projection"][self._gm.projection]
            continents_renderer, xScale, yScale = self._context().plotContinents(
                plotting_dataset_bounds, projection,
                self._dataWrapModulo,
                vp, self._template.data.priority,
                vtk_backend_grid=self._vtkDataSet,
                dataset_bounds=self._vtkDataSetBounds)

    def _plotInternalBoxfill(self):
        """Implements the logic to render a non-custom boxfill."""
        # Prep mapper
        mapper = vtk.vtkPolyDataMapper()
        self._mappers = [mapper]

        if self._gm.ext_1 and self._gm.ext_2:
            mapper.SetInputConnection(self._vtkPolyDataFilter.GetOutputPort())
            self._resultDict["vtk_backend_geofilters"] = \
                [self._vtkPolyDataFilter]
        else:
            thr = vtk.vtkThreshold()
            thr.SetInputConnection(self._vtkPolyDataFilter.GetOutputPort())
            if not self._gm.ext_1 and not self._gm.ext_2:
                thr.ThresholdBetween(self._contourLevels[0],
                                     self._contourLevels[-1])
            elif self._gm.ext_1 and not self._gm.ext_2:
                thr.ThresholdByLower(self._contourLevels[-1])
            elif not self._gm.ext_1 and self._gm.ext_2:
                thr.ThresholdByUpper(self._contourLevels[0])

            geoFilter2 = vtk.vtkDataSetSurfaceFilter()
            geoFilter2.SetInputConnection(thr.GetOutputPort())
            mapper.SetInputConnection(geoFilter2.GetOutputPort())
            self._resultDict["vtk_backend_geofilters"] = [geoFilter2]

        # Colortable bit
        # make sure length match
        numLevels = len(self._contourLevels) - 1
        while len(self._contourColors) < numLevels:
            self._contourColors.append(self._contourColors[-1])

        lut = vtk.vtkLookupTable()
        lut.SetNumberOfTableValues(numLevels)
        _colorMap = self.getColorMap()
        for i in range(numLevels):
            r, g, b, a = self.getColorIndexOrRGBA(_colorMap, self._contourColors[i])
            lut.SetTableValue(i, r / 100., g / 100., b / 100., a / 100.)

        mapper.SetLookupTable(lut)
        if numpy.allclose(self._contourLevels[0], -1.e20):
            lmn = self._min - 1.
        else:
            lmn = self._contourLevels[0]
        if numpy.allclose(self._contourLevels[-1], 1.e20):
            lmx = self._mx + 1.
        else:
            lmx = self._contourLevels[-1]
        mapper.SetScalarRange(lmn, lmx)
        self._resultDict["vtk_backend_luts"] = [[lut, [lmn, lmx, True]]]

    def _plotInternalCustomBoxfill(self):
        """Implements the logic to render a custom boxfill."""
        self._mappers = []

        self._customBoxfillArgs = self._prepContours()
        tmpLevels = self._customBoxfillArgs["tmpLevels"]
        tmpColors = self._customBoxfillArgs["tmpColors"]
        tmpOpacities = self._customBoxfillArgs["tmpOpacities"]

        style = self._gm.fillareastyle

        luts = []
        geos = []
        wholeDataMin, wholeDataMax = vcs.minmax(self._originalData1)
        _colorMap = self.getColorMap()
        for i, l in enumerate(tmpLevels):
            # Ok here we are trying to group together levels can be, a join
            # will happen if: next set of levels continues where one left off
            # AND pattern is identical

            # TODO this should really just be a single polydata/mapper/actor:
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
                    lut.SetTableValue(0, r / 100., g / 100., b / 100., tmpOpacity)
                else:
                    lut.SetTableValue(0, 1., 1., 1., 0.)
                mapper.SetLookupTable(lut)
                mapper.SetScalarRange(l[j], l[j + 1])
                luts.append([lut, [l[j], l[j + 1], False]])
                # Store the mapper only if it's worth it?
                # Need to do it with the whole slab min/max for animation
                # purposes
                if not (l[j + 1] < wholeDataMin or l[j] > wholeDataMax):
                    self._mappers.append(mapper)

        self._resultDict["vtk_backend_luts"] = luts
        if len(geos) > 0:
            self._resultDict["vtk_backend_geofilters"] = geos
