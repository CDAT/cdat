from .pipeline2d import Pipeline2D
from .. import vcs2vtk

import numpy
import vcs
import vtk
import warnings
import fillareautils


class IsofillPipeline(Pipeline2D):

    """Implementation of the Pipeline interface for VCS isofill plots."""

    def __init__(self, context_):
        super(IsofillPipeline, self).__init__(context_)

        self._patternActors = None

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

        if isinstance(self._contourLevels, numpy.ndarray):
            self._contourLevels = self._contourLevels.tolist()

        # Figure out colors
        self._contourColors = self._gm.fillareacolors
        if self._contourColors == [1]:
            # TODO BUG It's possible that levs2 may not exist here...
            self._contourColors = vcs.getcolors(levs2, split=0)
            if isinstance(self._contourColors, (int, float)):
                self._contourColors = [self._contourColors]

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
        tmpLevels = []
        tmpColors = []
        tmpIndices = []
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
#            print "levels: ", l
            if i == 0:
                C = [self._contourColors[i]]
                if numpy.allclose(self._contourLevels[0][0], -1.e20):
                    # ok it's an extension arrow
                    L = [self._scalarRange[0] - 1., self._contourLevels[0][1]]
                else:
                    L = list(self._contourLevels[i])
                I = indices[i]
            else:
                if l[0] == L[-1] and I == indices[i]:
                    # Ok same type lets keep going
                    if numpy.allclose(l[1], 1.e20):
                        L.append(self._scalarRange[1] + 1.)
                    else:
                        L.append(l[1])
                    C.append(self._contourColors[i])
                else:  # ok we need new contouring
                    tmpLevels.append(L)
                    tmpColors.append(C)
                    tmpIndices.append(I)
                    C = [self._contourColors[i]]
                    L = [L[-1], l[1]]
                    I = indices[i]
        tmpLevels.append(L)
        tmpColors.append(C)
        tmpIndices.append(I)
#        print "tmpLevels", tmpLevels
#        print "tmpColors", tmpColors
#        print "tmpIndices", tmpIndices

        luts = []
        cots = []
        geos = []
        mappers = []
        self._patternActors = []
        for i, l in enumerate(tmpLevels):
            # Ok here we are trying to group together levels can be, a join
            # will happen if: next set of levels contnues where one left off
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

#            vtp = vtk.vtkXMLPolyDataWriter()
#            vtp.SetInputConnection(cot.GetOutputPort())
#            s = "cot_" + str(i) + ".vtp"
#            vtp.SetFileName(s)
#            vtp.Write()

            cots.append(cot)
            mapper.SetInputConnection(cot.GetOutputPort())
            lut.SetNumberOfTableValues(len(tmpColors[i]))
            for j, color in enumerate(tmpColors[i]):
                r, g, b = self._colorMap.index[color]
                lut.SetTableValue(j, r / 100., g / 100., b / 100.)
            luts.append([lut, [0, len(l) - 1, True]])
            mapper.SetLookupTable(lut)
            mapper.SetScalarRange(0, len(l) - 1)
            mapper.SetScalarModeToUseCellData()
            mappers.append(mapper)

            act = fillareautils.make_patterned_polydata(cot.GetOutput(),
                                                        fillareastyle=self._gm.fillareastyle,
                                                        fillareaindex=tmpIndices[i],
                                                        applystencil=True)
            if act is not None:
                self._patternActors.append(act)

        self._resultDict["vtk_backend_luts"] = luts
        if len(cots) > 0:
            self._resultDict["vtk_backend_contours"] = cots
        if len(geos) > 0:
            self._resultDict["vtk_backend_geofilters"] = geos

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
                r, g, b = self._colorMap.index[self._contourColors[i]]
                lut.SetTableValue(i, r / 100., g / 100., b / 100.)

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

            # TODO see comment in boxfill.
            if mapper is self._maskedDataMapper:
                actors.append([act, self._maskedDataMapper, [x1, x2, y1, y2]])
            else:
                actors.append([act, [x1, x2, y1, y2]])

            # create a new renderer for this mapper
            # (we need one for each mapper because of cmaera flips)
            self._context.fitToViewport(
                act, [self._template.data.x1, self._template.data.x2,
                      self._template.data.y1, self._template.data.y2],
                wc=[x1, x2, y1, y2], geo=self._vtkGeoTransform,
                priority=self._template.data.priority,
                create_renderer=True)

        for act in self._patternActors:
            self._context.fitToViewport(
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

        self._resultDict.update(self._context.renderTemplate(self._template,
                                                             self._data1,
                                                             self._gm, t, z))

        legend = getattr(self._gm, "legend", None)

        if self._gm.ext_1:
            if isinstance(self._contourLevels[0], list):
                if numpy.less(abs(self._contourLevels[0][0]), 1.e20):
                    # Ok we need to add the ext levels
                    self._contourLevels.insert(0, [-1.e20, self._contourLevels[0][0]])
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
                                         self._contourColors, legend,
                                         self._colorMap, self._gm.fillareastyle,
                                         self._gm.fillareaindices))

        if self._context.canvas._continents is None:
            self._useContinents = False
        if self._useContinents:
            projection = vcs.elements["projection"][self._gm.projection]
            self._context.plotContinents(x1, x2, y1, y2, projection,
                                         self._dataWrapModulo, self._template)
