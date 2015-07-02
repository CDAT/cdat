from .pipeline2d import Pipeline2D
from .. import vcs2vtk

import numpy
import vcs
import vtk
import warnings


class BoxfillPipeline(Pipeline2D):

    """Implementation of the Pipeline interface for VCS boxfill plots.

    Internal variables:
        - self._contourLabels: Contour labels.
        - self._mappers: Mappers produced by this pipeline.
            TODO _mappers should be removed and replaced by a more specific
            set of ivars (at minimum, identify what the mappers are rendering).
    """

    def __init__(self, context_):
        super(BoxfillPipeline, self).__init__(context_)

        self._contourLabels = None
        self._mappers = None

    def _updateScalarData(self):
        """Overrides baseclass implementation."""
        # Update data1 if this is a log10 boxfill:
        data = self._originalData1
        if self._gm.boxfill_type == "log10":
            data = numpy.ma.log10(data)

        self._data1 = self._context.trimData2D(data)
        self._data2 = self._context.trimData2D(self._originalData2)

    def _updateContourLevelsAndColors(self):
        """Overrides baseclass implementation."""
        if self._gm.boxfill_type != "custom":
            self._updateContourLevelsAndColorsForBoxfill()
        else:
            self._updateContourLevelsAndColorsForCustomBoxfill()

        if isinstance(self._contourLevels, numpy.ndarray):
            self._contourLevels = self._contourLevels.tolist()

    def _updateContourLevelsAndColorsForBoxfill(self):
        """Set contour information for a standard boxfill."""
        # Compute levels
        nlev = (self._gm.color_2 - self._gm.color_1) + 1
        if numpy.allclose(self._gm.level_1, 1.e20) or \
           numpy.allclose(self._gm.level_2, 1.e20):
            self._contourLevels = vcs.mkscale(self._scalarRange[0],
                                              self._scalarRange[1])
            if len(self._contourLevels) == 1:  # constant value ?
                self._contourLevels = [self._contourLevels[0],
                                       self._contourLevels[0] + .00001]
            self._contourLabels = vcs.mklabels(self._contourLevels)
            dx = (self._contourLevels[-1] - self._contourLevels[0]) / nlev
            self._contourLevels = numpy.arange(self._contourLevels[0],
                                               self._contourLevels[-1] + dx,
                                               dx)
        else:
            if self._gm.boxfill_type == "log10":
                levslbls = vcs.mkscale(numpy.ma.log10(self._gm.level_1),
                                       numpy.ma.log10(self._gm.level_2))
                self._contourLevels = vcs.mkevenlevels(
                    numpy.ma.log10(self._gm.level_1),
                    numpy.ma.log10(self._gm.level_2), nlev=nlev)
            else:
                levslbls = vcs.mkscale(self._gm.level_1, self._gm.level_2)
                self._contourLevels = vcs.mkevenlevels(self._gm.level_1,
                                                       self._gm.level_2,
                                                       nlev=nlev)
            if len(self._contourLevels) > 25:
                # Too many colors/levels need to prettyfy this for legend
                self._contourLabels = vcs.mklabels(levslbls)
                # Make sure extremes are in
                legd2 = vcs.mklabels([self._contourLevels[0],
                                      self._contourLevels[-1]])
                self._contourLabels.update(legd2)
            else:
                self._contourLabels = vcs.mklabels(self._contourLevels)
            if self._gm.boxfill_type == "log10":
                logLabels = {}
                for key in self._contourLabels.keys():
                    value = self._contourLabels[key]
                    newKey = float(numpy.ma.log10(value))
                    logLabels[newKey] = value
                self._contourLabels = logLabels

        # Use consecutive colors:
        self._contourColors = range(self._gm.color_1, self._gm.color_2 + 1)

    def _updateContourLevelsAndColorsForCustomBoxfill(self):
        """Set contour information for a custom boxfill."""
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
                    levs2[0] = 0
                for i in range(len(levs2) - 1):
                    self._contourLevels.append([levs2[i], levs2[i + 1]])

        # Contour colors:
        self._contourColors = self._gm.fillareacolors
        if self._contourColors is None:
            # TODO BUG levs2 may not be defined here...
            self._contourColors = vcs.getcolors(levs2, split=0)

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
        """Overrides baseclass implementation."""
        # Special case for custom boxfills:
        if self._gm.boxfill_type != "custom":
            self._plotInternalBoxfill()
        else:
            self._plotInternalCustomBoxfill()

        if self._maskedDataMapper is not None:
            self._mappers.insert(0, self._maskedDataMapper)

        x1, x2, y1, y2 = vcs.utils.getworldcoordinates(self._gm,
                                                       self._data1.getAxis(-1),
                                                       self._data1.getAxis(-2))

        # And now we need actors to actually render this thing
        actors = []
        for mapper in self._mappers:
            act = vtk.vtkActor()
            act.SetMapper(mapper)

            if self._vtkGeoTransform is None:
                # If using geofilter on wireframed does not get wrppaed not
                # sure why so sticking to many mappers
                act = vcs2vtk.doWrap(act, [x1, x2, y1, y2],
                                     self._dataWrapModulo)

            # TODO We shouldn't need this conditional branch, the 'else' body
            # should be used and GetMapper called to get the mapper as needed.
            # If this is needed for other reasons, we need a comment explaining
            # why.
            if mapper is self._maskedDataMapper:
                actors.append([act, self._maskedDataMapper, [x1, x2, y1, y2]])
            else:
                actors.append([act, [x1, x2, y1, y2]])

            # create a new renderer for this mapper
            # (we need one for each mapper because of camera flips)
            self._context.fitToViewport(
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

        self._resultDict.update(
            self._context.renderColorBar(self._template, self._contourLevels,
                                         self._contourColors,
                                         self._contourLabels,
                                         self._colorMap))

        if self._context.canvas._continents is None:
            self._useContinents = False
        if self._useContinents:
            projection = vcs.elements["projection"][self._gm.projection]
            self._context.plotContinents(x1, x2, y1, y2, projection,
                                         self._dataWrapModulo, self._template)

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
        numLevels = len(self._contourLevels)
        while len(self._contourColors) < numLevels:
            self._contourColors.append(self._contourColors[-1])

        lut = vtk.vtkLookupTable()
        lut.SetNumberOfTableValues(numLevels)
        for i in range(numLevels):
            r, g, b = self._colorMap.index[self._contourColors[i]]
            lut.SetTableValue(i, r / 100., g / 100., b / 100.)

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
        tmpLevels = []
        tmpColors = []
        indices = self._gm.fillareaindices
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

        for i, l in enumerate(self._contourLevels):
            if i == 0:
                C = [self._contourColors[i]]
                if numpy.allclose(self._contourLevels[0][0], -1.e20):
                    # ok it's an extension arrow
                    L = [self._scalarRange[0] - 1., self._contourLevels[0][1]]
                else:
                    L = list(self._contourLevels[i])
                I = [indices[i]]
            else:
                if l[0] == L[-1] and I[-1] == indices[i]:
                    # Ok same type lets keep going
                    if numpy.allclose(l[1], 1.e20):
                        L.append(self._scalarRange[1] + 1.)
                    else:
                        L.append(l[1])
                    C.append(self._contourColors[i])
                else:  # ok we need new contouring
                    tmpLevels.append(L)
                    tmpColors.append(C)
                    C = [self._contourColors[i]]
                    L = self._contourLevels[i]
                    I = [indices[i]]
        tmpLevels.append(L)
        tmpColors.append(C)

        luts = []
        geos = []
        wholeDataMin, wholeDataMax = vcs.minmax(self._originalData1)
        for i, l in enumerate(tmpLevels):
            # Ok here we are trying to group together levels can be, a join
            # will happen if: next set of levels contnues where one left off
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
                geos.append(geoFilter2)
                mapper.SetInputConnection(geoFilter2.GetOutputPort())
                lut.SetNumberOfTableValues(1)
                r, g, b = self._colorMap.index[color]
                lut.SetTableValue(0, r / 100., g / 100., b / 100.)
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
