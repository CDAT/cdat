from .pipeline2d import Pipeline2D
from .vcswrapfilter import VCSWrapFilter
from .. import vcs2vtk

import numpy
import vcs
import vtk
import warnings


class MeshfillPipeline(Pipeline2D):
    """Implementation of the Pipeline interface for VCS meshfill plots."""

    def __init__(self, context_):
        super(MeshfillPipeline, self).__init__(context_)

    def _updateScalarData(self):
        """Overrides baseclass implementation."""
        # We don't trim _data2 for meshfill:
        self._data1 = self._context().trimData2D(self._originalData1)
        self._data2 = self._originalData2

    def _updateContourLevelsAndColors(self):
        """Overrides baseclass implementation."""
        # Contour values:
        self._contourLevels = self._gm.levels
        if numpy.allclose(self._contourLevels[0], [0., 1.e20]) or \
           numpy.allclose(self._contourLevels, 1.e20):
            levs2 = vcs.mkscale(self._scalarRange[0], self._scalarRange[1])
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
                self._contourLevels.append([levs2[i], levs2[i+1]])
        else:
            if not isinstance(self._contourLevels[0], (list, tuple)):
                self._contourLevels = []
                levs2 = self._gm.levels
                if numpy.allclose(levs2[0], 1.e20):
                    levs2[0] = -1.e20
                for i in range(len(levs2) - 1):
                    self._contourLevels.append([levs2[i], levs2[i+1]])

        # Contour colors:
        self._contourColors = self._gm.fillareacolors
        if self._contourColors == [1]:
            # TODO BUG levs2 may be uninitialized here
            self._contourColors = vcs.getcolors(levs2, split=0)
            if isinstance(self._contourColors, (int, float)):
                self._contourColors = [self._contourColors]

        if isinstance(self._contourLevels, numpy.ndarray):
            self._contourLevels = self._contourLevels.tolist()

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
                  "You asked for %i lgridevels but provided %i colors, extra "
                  "ones will be ignored\nGraphic Method: %s of type %s"
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

        mappers = []
        luts = []
        geos = []
        for i, l in enumerate(tmpLevels):
            # Ok here we are trying to group together levels can be, a join
            # will happen if: next set of levels contnues where one left off
            # AND pattern is identical
            wholeDataMin, wholeDataMax = vcs.minmax(self._originalData1)
            # TODO this should really just be a single polydata that is
            # colored by scalars:
            for j, color in enumerate(tmpColors[i]):
                mapper = vtk.vtkPolyDataMapper()
                lut = vtk.vtkLookupTable()
                th = vtk.vtkThreshold()
                th.ThresholdBetween(l[j], l[j+1])
                th.SetInputConnection(self._vtkPolyDataFilter.GetOutputPort())
                geoFilter2 = vtk.vtkDataSetSurfaceFilter()
                geoFilter2.SetInputConnection(th.GetOutputPort())
                geos.append(geoFilter2)
                mapper.SetInputConnection(geoFilter2.GetOutputPort())
                lut.SetNumberOfTableValues(1)
                r, g, b = self._colorMap.index[color]
                lut.SetTableValue(0, r/100., g/100., b/100.)
                mapper.SetLookupTable(lut)
                mapper.SetScalarRange(l[j], l[j+1])
                luts.append([lut, [l[j], l[j+1], True]])
                # Store the mapper only if it's worth it?
                # Need to do it with the whole slab min/max for animation
                # purposes
                if not (l[j+1] < wholeDataMin or l[j] > wholeDataMax):
                    mappers.append(mapper)

        self._resultDict["vtk_backend_luts"] = luts
        if len(geos) > 0:
            self._resultDict["vtk_backend_geofilters"] = geos

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
                r, g, b = self._colorMap.index[self._contourColors[i]]
                lut.SetTableValue(i, r / 100., g / 100., b / 100.)

            mapper.SetLookupTable(lut)
            if numpy.allclose(self._contourLevels[0], -1.e20):
                lmn = mn - 1.
            else:
                lmn = self._contourLevels[0]
            if numpy.allclose(self._contourLevels[-1], 1.e20):
                lmx = mx + 1.
            else:
                lmx = self._contourLevels[-1]
            mapper.SetScalarRange(lmn, lmx)
            self._resultDict["vtk_backend_luts"] = [[lut, [lmn, lmx, True]]]

        if self._maskedDataMapper is not None:
            # Note that this is different for meshfill -- others prepend.
            mappers.append(self._maskedDataMapper)

        # This is also different for meshfill, others use
        # vcs.utils.getworldcoordinates
        x1, x2, y1, y2 = vcs2vtk.getRange(self._gm,
                                          self._vtkDataSetBounds[0],
                                          self._vtkDataSetBounds[1],
                                          self._vtkDataSetBounds[2],
                                          self._vtkDataSetBounds[3])

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
        for mapper in mappers:
            act = vtk.vtkActor()
            act.SetMapper(mapper)

            if hasattr(mapper, "_useWireFrame"):
                prop = act.GetProperty()
                prop.SetRepresentationToWireframe()

            # create a new renderer for this mapper
            # (we need one for each mapper because of cmaera flips)
            ren = self._context().fitToViewport(
                  act, [self._template.data.x1,
                        self._template.data.x2,
                        self._template.data.y1,
                        self._template.data.y2],
                  wc=[x1, x2, y1, y2], geo=self._vtkGeoTransform,
                  priority=self._template.data.priority)

            if self._vtkGeoTransform is None:
                wrapFilter = VCSWrapFilter([x1, x2, y1, y2],
                                           self._dataWrapModulo,
                                           transform=act.GetMatrix())
                wrapFilter.SetInputConnection(mapper.GetInputConnection(0, 0))
                mapper.SetInputConnection(wrapFilter.GetOutputPort())

            # TODO See comment in boxfill.
            if mapper is self._maskedDataMapper:
                actors.append([act, self._maskedDataMapper, [x1, x2, y1, y2]])
                self._maskedDataActor = act
            else:
                actors.append([act, [x1, x2, y1, y2]])

        self._resultDict["vtk_backend_actors"] = actors

        self._template.plot(self._context().canvas, self._data1, self._gm,
                            bg=self._context().bg,
                            X=numpy.arange(self._vtkDataSetBounds[0],
                                           self._vtkDataSetBounds[1] * 1.1,
                                           (self._vtkDataSetBounds[1] -
                                            self._vtkDataSetBounds[0]) / 10.),
                            Y=numpy.arange(self._vtkDataSetBounds[2],
                                           self._vtkDataSetBounds[3] * 1.1,
                                           (self._vtkDataSetBounds[3] -
                                            self._vtkDataSetBounds[2]) / 10.))

        legend = getattr(self._gm, "legend", None)

        if self._gm.ext_1:
            if isinstance(self._contourLevels[0], list):
                if numpy.less(abs(self._contourLevels[0][0]), 1.e20):
                    # Ok we need to add the ext levels
                    self._contourLevels.insert(0, [-1.e20, levs[0][0]])
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
            self._context().renderColorBar(self._template, self._contourLevels,
                                           self._contourColors, legend,
                                           self._colorMap))

        if self._context().canvas._continents is None:
            self._useContinents = False
        if self._useContinents:
            projection = vcs.elements["projection"][self._gm.projection]
            self._context().plotContinents(x1, x2, y1, y2, projection,
                                           self._dataWrapModulo,
                                           self._template)
