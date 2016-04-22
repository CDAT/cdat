from .pipeline2d import Pipeline2D
from .. import vcs2vtk

import numpy
import vcs
import vtk


class IsolinePipeline(Pipeline2D):

    """Implementation of the Pipeline interface for VCS isoline plots."""

    def __init__(self, gm, context_):
        super(IsolinePipeline, self).__init__(gm, context_)
        self._needsCellData = False

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

    def _plotInternal(self):
        """Overrides baseclass implementation."""
        tmpLevels = []
        tmpColors = []
        tmpLineWidths = []
        tmpLineStyles = []

        linewidth = self._gm.linewidths
        linestyle = self._gm.line

        if len(linewidth) < len(self._contourLevels):
            # fill up the line width values
            linewidth += [1.0] * (len(self._contourLevels) - len(linewidth))

        if len(linestyle) < len(self._contourLevels):
            # fill up the line style values
            linestyle += ['solid'] * (len(self._contourLevels) - len(linestyle))

        plotting_dataset_bounds = self.getPlottingBounds()
        x1, x2, y1, y2 = plotting_dataset_bounds

        for i, l in enumerate(self._contourLevels):
            if i == 0:
                W = linewidth[i]
                S = linestyle[i]
                C = [self._contourColors[i]]
                if l == 1.e20:
                    L = [-1.e20]
                else:
                    L = [l]
            else:
                if W == linewidth[i] and S == linestyle[i]:
                    # Ok same style and width, lets keep going
                    L.append(l)
                    if i >= len(self._contourColors):
                        C.append(self._contourColors[-1])
                    else:
                        C.append(self._contourColors[i])
                else:
                    tmpLevels.append(L)
                    tmpColors.append(C)
                    tmpLineWidths.append(W)
                    tmpLineStyles.append(S)
                    L = [l]
                    if i >= len(self._contourColors):
                        C = [self._contourColors[-1]]
                    else:
                        C = [self._contourColors[i]]
                    W = linewidth[i]
                    S = linestyle[i]

        tmpLevels.append(L)
        tmpColors.append(C)
        tmpLineWidths.append(W)
        tmpLineStyles.append(S)

        cots = []
        textprops = []
        luts = []

        actors = []
        mappers = []

        if self._gm.label and (self._gm.text or self._gm.textcolors):
            # Text objects:
            if self._gm.text:
                texts = self._gm.text
                while len(texts) < len(self._contourLevels):
                    texts.append(texts[-1])
            else:
                texts = [None] * len(self._contourLevels)

            # Custom colors:
            if self._gm.textcolors:
                colorOverrides = self._gm.textcolors
                while len(colorOverrides) < len(self._contourLevels):
                    colorOverrides.append(colorOverrides[-1])
            else:
                colorOverrides = [None] * len(self._gm.text)

            # Custom background colors and opacities:
            backgroundColors = self._gm.labelbackgroundcolors
            if backgroundColors:
                while len(backgroundColors) < len(self._contourLevels):
                    backgroundColors.append(backgroundColors[-1])
            backgroundOpacities = self._gm.labelbackgroundopacities
            if backgroundOpacities:
                while len(backgroundOpacities) < len(self._contourLevels):
                    backgroundOpacities.append(backgroundOpacities[-1])

        countLevels = 0
        vp = self._resultDict.get(
            'ratio_autot_viewport',
            [self._template.data.x1, self._template.data.x2,
             self._template.data.y1, self._template.data.y2])
        dataset_renderer = None
        xScale, yScale = (1, 1)
        for i, l in enumerate(tmpLevels):
            numLevels = len(l)

            cot = vtk.vtkContourFilter()
            if self._hasCellData:
                cot.SetInputConnection(self._vtkPolyDataFilter.GetOutputPort())
            else:
                cot.SetInputData(self._vtkDataSet)
            cot.SetNumberOfContours(numLevels)

            for n in range(numLevels):
                cot.SetValue(n, l[n])
            cot.SetValue(numLevels, l[-1])
            # TODO remove update
            cot.Update()

            lut = vtk.vtkLookupTable()
            lut.SetNumberOfTableValues(len(tmpColors[i]))
            cmap = self.getColorMap()
            for n, col in enumerate(tmpColors[i]):
                r, g, b, a = self.getColorIndexOrRGBA(cmap, col)
                lut.SetTableValue(n, r / 100., g / 100., b / 100., a / 100.)

            # Setup isoline labels
            if self._gm.label:
                # Setup label mapping array:
                tpropMap = vtk.vtkDoubleArray()
                tpropMap.SetNumberOfComponents(1)
                tpropMap.SetNumberOfTuples(numLevels)
                for n, val in enumerate(l):
                    tpropMap.SetTuple(n, [val])

                # Prep text properties:
                tprops = vtk.vtkTextPropertyCollection()
                if self._gm.text or self._gm.textcolors:
                    ttexts = texts[countLevels:(countLevels + len(l))]

                    for idx, tc in enumerate(ttexts):
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
                        elif isinstance(tc, (str, unicode)):
                            sp = tc.split(":::")
                            if len(sp) == 2:
                                tt = sp[0]
                                to = sp[1]
                            else:  # Hum don't know what do do with this
                                if sp[0] in vcs.listelements("textcombined"):
                                    tc = vcs.gettextcombined(tc)
                                    tt, to = tuple(tc.name.split(":::"))
                                elif sp[0] in vcs.listelements("textorientation"):
                                    to = sp[0]
                                    tt = "default"
                                elif sp[0] in vcs.listelements("texttable"):
                                    tt = sp[0]
                                    to = "default"

                        colorOverride = colorOverrides[countLevels + idx]
                        if colorOverride is not None:
                            tt = vcs.createtexttable(None, tt)
                            tt.color = colorOverride
                            tt = tt.name
                        if backgroundColors is not None:
                            texttbl = vcs.gettexttable(tt)
                            texttbl.backgroundcolor = backgroundColors[countLevels + idx]
                        if backgroundOpacities is not None:
                            texttbl = vcs.gettexttable(tt)
                            texttbl.backgroundopacity = backgroundOpacities[countLevels + idx]
                        tprop = vtk.vtkTextProperty()
                        vcs2vtk.prepTextProperty(tprop,
                                                 self._context().renWin.GetSize(),
                                                 to, tt, cmap=cmap)
                        tprops.AddItem(tprop)
                        if colorOverride is not None:
                            del(vcs.elements["texttable"][tt])
                else:  # No text properties specified. Use the default:
                    tprop = vtk.vtkTextProperty()
                    vcs2vtk.prepTextProperty(tprop,
                                             self._context().renWin.GetSize(),
                                             cmap=cmap)
                    tprops.AddItem(tprop)
                textprops.append(tprops)

                mapper = vtk.vtkLabeledContourMapper()
                mapper.SetTextProperties(tprops)
                mapper.SetTextPropertyMapping(tpropMap)
                mapper.SetLabelVisibility(1)
                mapper.SetSkipDistance(self._gm.labelskipdistance)

                pdMapper = mapper.GetPolyDataMapper()

                luts.append([lut, [l[0], l[-1], False]])
            else:  # No isoline labels:
                mapper = vtk.vtkPolyDataMapper()
                pdMapper = mapper
                luts.append([lut, [l[0], l[-1], False]])
            pdMapper.SetLookupTable(lut)
            pdMapper.SetScalarRange(l[0], l[-1])
            pdMapper.SetScalarModeToUsePointData()

            stripper = vtk.vtkStripper()
            stripper.SetInputConnection(cot.GetOutputPort())
            mapper.SetInputConnection(stripper.GetOutputPort())
            # TODO remove update, make pipeline
            stripper.Update()
            mappers.append(mapper)
            cots.append(cot)

            # Create actor to add to scene
            act = vtk.vtkActor()
            act.SetMapper(mapper)
            # Set line properties here
            p = act.GetProperty()
            p.SetLineWidth(tmpLineWidths[i])
            vcs2vtk.stippleLine(p, tmpLineStyles[i])

            if self._vtkGeoTransform is None:
                # If using geofilter on wireframed does not get wrppaed not
                # sure why so sticking to many mappers
                act = vcs2vtk.doWrap(act, plotting_dataset_bounds,
                                     self._dataWrapModulo)
            actors.append([act, plotting_dataset_bounds])

            # create a new renderer for this mapper
            # (we need one for each mapper because of cmaera flips)
            dataset_renderer, xScale, yScale = self._context().fitToViewport(
                act, vp,
                wc=plotting_dataset_bounds, geoBounds=self._vtkDataSet.GetBounds(),
                geo=self._vtkGeoTransform,
                priority=self._template.data.priority,
                create_renderer=(dataset_renderer is None))

            countLevels += len(l)
        self._resultDict['dataset_renderer'] = dataset_renderer
        self._resultDict['dataset_scale'] = (xScale, yScale)
        if len(textprops) > 0:
            self._resultDict["vtk_backend_contours_labels_text_properties"] = \
                textprops
        if len(luts) > 0:
            if self._gm.label:
                self._resultDict["vtk_backend_labeled_luts"] = luts
            else:
                self._resultDict["vtk_backend_luts"] = luts
        if len(cots) > 0:
            self._resultDict["vtk_backend_contours"] = cots

        if self._maskedDataMapper is not None:
            mappers.insert(0, self._maskedDataMapper)
            act = vtk.vtkActor()
            act.SetMapper(self._maskedDataMapper)
            if self._vtkGeoTransform is None:
                # If using geofilter on wireframed does not get wrppaed not
                # sure why so sticking to many mappers
                act = vcs2vtk.doWrap(act, plotting_dataset_bounds,
                                     self._dataWrapModulo)
            actors.append([act, self._maskedDataMapper, plotting_dataset_bounds])
            # create a new renderer for this mapper
            # (we need one for each mapper because of cmaera flips)
            self._context().fitToViewport(
                act, vp,
                wc=plotting_dataset_bounds, geoBounds=self._vtkDataSet.GetBounds(),
                geo=self._vtkGeoTransform,
                priority=self._template.data.priority,
                create_renderer=True)

        self._resultDict["vtk_backend_actors"] = actors

        t = self._originalData1.getTime()
        if self._originalData1.ndim > 2:
            z = self._originalData1.getAxis(-3)
        else:
            z = None
        kwargs = {"vtk_backend_grid": self._vtkDataSet,
                  "dataset_bounds": self._vtkDataSetBounds,
                  "plotting_dataset_bounds": plotting_dataset_bounds}
        if ("ratio_autot_viewport" in self._resultDict):
            kwargs["ratio_autot_viewport"] = vp
        self._resultDict.update(self._context().renderTemplate(
            self._template,
            self._data1,
            self._gm, t, z, **kwargs))

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
            self._resultDict['continents_renderer'] = continents_renderer
