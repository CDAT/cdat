from .pipeline2d import Pipeline2D
from .. import vcs2vtk
import fillareautils

import numpy
import vcs
import vtk


class IsofillPipeline(Pipeline2D):

    """Implementation of the Pipeline interface for VCS isofill plots."""

    def __init__(self, gm, context_):
        super(IsofillPipeline, self).__init__(gm, context_)

    def _updateVTKDataSet(self):
        """Overrides baseclass implementation."""
        # Force point data for isoline/isofill
        genGridDict = vcs2vtk.genGridOnPoints(self._data1, self._gm,
                                              deep=False,
                                              grid=self._vtkDataSet,
                                              geo=self._vtkGeoTransform)
        genGridDict["cellData"] = False
        self._data1 = genGridDict["data"]
        self._updateFromGenGridDict(genGridDict)

        data = vcs2vtk.numpy_to_vtk_wrapper(self._data1.filled(0.).flat,
                                            deep=False)

        self._vtkDataSet.GetPointData().SetScalars(data)

    def _updateContourLevelsAndColors(self):
        self._updateContourLevelsAndColorsGeneric()

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
        self._vtkPolyDataFilter.Update()
        self._resultDict["vtk_backend_filter"] = self._vtkPolyDataFilter

    def _plotInternal(self):
        """Overrides baseclass implementation."""

        prepedContours = self._prepContours()
        tmpLevels = prepedContours["tmpLevels"]
        tmpIndices = prepedContours["tmpIndices"]
        tmpColors = prepedContours["tmpColors"]
        tmpOpacities = prepedContours["tmpOpacities"]
        style = self._gm.fillareastyle

        luts = []
        cots = []
        mappers = []
        _colorMap = self.getColorMap()

        x1, x2, y1, y2 = vcs.utils.getworldcoordinates(self._gm,
                                                       self._data1.getAxis(-1),
                                                       self._data1.getAxis(-2))
        for i, l in enumerate(tmpLevels):
            # Ok here we are trying to group together levels can be, a join
            # will happen if: next set of levels continues where one left off
            # AND pattern is identical
            mapper = vtk.vtkPolyDataMapper()
            lut = vtk.vtkLookupTable()
            cot = vtk.vtkBandedPolyDataContourFilter()
            cot.ClippingOn()
            cot.SetInputData(self._vtkPolyDataFilter.GetOutput())
            cot.SetNumberOfContours(len(l))
            cot.SetClipTolerance(0.)
            for j, v in enumerate(l):
                cot.SetValue(j, v)
            cot.Update()

            cots.append(cot)
            mapper.SetInputConnection(cot.GetOutputPort())
            lut.SetNumberOfTableValues(len(tmpColors[i]))
            for j, color in enumerate(tmpColors[i]):
                r, g, b, a = self.getColorIndexOrRGBA(_colorMap, color)
                if style == 'solid':
                    tmpOpacity = tmpOpacities[j]
                    if tmpOpacity is None:
                        tmpOpacity = a / 100.
                    else:
                        tmpOpacity = tmpOpacities[j] / 100.
                    lut.SetTableValue(j, r / 100., g / 100., b / 100., tmpOpacity)
                else:
                    lut.SetTableValue(j, 1., 1., 1., 0.)
            luts.append([lut, [0, len(l) - 1, True]])
            mapper.SetLookupTable(lut)
            mapper.SetScalarRange(0, len(l) - 1)
            mapper.SetScalarModeToUseCellData()
            mappers.append(mapper)

        self._resultDict["vtk_backend_luts"] = luts
        if len(cots) > 0:
            self._resultDict["vtk_backend_contours"] = cots

        numLevels = len(self._contourLevels)
        if mappers == []:  # ok didn't need to have special banded contours
            mapper = vtk.vtkPolyDataMapper()
            mappers = [mapper]
            # Colortable bit
            # make sure length match
            while len(self._contourColors) < len(self._contourLevels):
                self._contourColors.append(self._contourColors[-1])

            lut = vtk.vtkLookupTable()
            lut.SetNumberOfTableValues(numLevels)
            for i in range(numLevels):
                r, g, b, a = self.getColorIndexOrRGBA(_colorMap, self._contourColors[i])
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

        if self._maskedDataMapper is not None:
            mappers.insert(0, self._maskedDataMapper)

        # And now we need actors to actually render this thing
        actors = []
        patternActors = []
        ct = 0
        for mapper in mappers:
            act = vtk.vtkActor()
            act.SetMapper(mapper)

            if self._vtkGeoTransform is None:
                # If using geofilter on wireframed does not get wrppaed not
                # sure why so sticking to many mappers
                act = vcs2vtk.doWrap(act, [x1, x2, y1, y2],
                                     self._dataWrapModulo)

            patact = None
            # TODO see comment in boxfill.
            if mapper is self._maskedDataMapper:
                actors.append([act, self._maskedDataMapper, [x1, x2, y1, y2]])
            else:
                actors.append([act, [x1, x2, y1, y2]])

                # Since pattern creation requires a single color, assuming the first
                c = self.getColorIndexOrRGBA(_colorMap, tmpColors[ct][0])

                # The isofill actor is scaled by the camera, so we need to use this size
                # instead of window size for scaling the pattern.
                viewsize = (x2 - x1, y2 - y1)
                patact = fillareautils.make_patterned_polydata(mapper.GetInput(),
                                                               fillareastyle=style,
                                                               fillareaindex=tmpIndices[ct],
                                                               fillareacolors=c,
                                                               fillareaopacity=tmpOpacities[ct],
                                                               size=viewsize)

                if patact is not None:
                    patternActors.append(patact)

                # increment the count
                ct += 1

            # create a new renderer for this mapper
            # (we need one for each mapper because of cmaera flips)
            self._context().fitToViewport(
                act, [self._template.data.x1, self._template.data.x2,
                      self._template.data.y1, self._template.data.y2],
                wc=[x1, x2, y1, y2], geo=self._vtkGeoTransform,
                priority=self._template.data.priority,
                create_renderer=True)

        for act in patternActors:
            self._context().fitToViewport(
                act, [self._template.data.x1, self._template.data.x2,
                      self._template.data.y1, self._template.data.y2],
                wc=[x1, x2, y1, y2], geo=self._vtkGeoTransform,
                priority=self._template.data.priority,
                create_renderer=True)
            actors.append([act, [x1, x2, y1, y2]])

        self._resultDict["vtk_backend_actors"] = actors

        t = self._originalData1.getTime()
        if self._originalData1.ndim > 2:
            z = self._originalData1.getAxis(-3)
        else:
            z = None

        self._resultDict.update(self._context().renderTemplate(self._template,
                                                               self._data1,
                                                               self._gm, t, z))

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

        self._resultDict.update(
            self._context().renderColorBar(self._template, self._contourLevels,
                                           self._contourColors, legend,
                                           self.getColorMap(),
                                           style=style,
                                           index=self._gm.fillareaindices,
                                           opacity=self._gm.fillareaopacity))

        if self._context().canvas._continents is None:
            self._useContinents = False
        if self._useContinents:
            projection = vcs.elements["projection"][self._gm.projection]
            self._context().plotContinents(x1, x2, y1, y2, projection,
                                           self._dataWrapModulo,
                                           self._template)
