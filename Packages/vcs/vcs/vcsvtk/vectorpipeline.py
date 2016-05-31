from .pipeline2d import Pipeline2D

import vcs
from vcs import vcs2vtk
import vtk


class VectorPipeline(Pipeline2D):

    """Implementation of the Pipeline interface for VCS vector plots."""

    def __init__(self, gm, context_):
        super(VectorPipeline, self).__init__(gm, context_)
        self._needsCellData = False
        self._needsVectors = True

    def _plotInternal(self):
        """Overrides baseclass implementation."""
        # Preserve time and z axis for plotting these inof in rendertemplate
        projection = vcs.elements["projection"][self._gm.projection]
        taxis = self._originalData1.getTime()
        scaleFactor = 1.0

        if self._originalData1.ndim > 2:
            zaxis = self._originalData1.getAxis(-3)
        else:
            zaxis = None

        scale = 1.0
        lat = None
        lon = None

        latAccessor = self._data1.getLatitude()
        lonAccessor = self._data1.getLongitude()
        if latAccessor:
            lat = latAccessor[:]
        if lonAccessor:
            lon = lonAccessor[:]

        if self._vtkGeoTransform is not None:
            newv = vtk.vtkDoubleArray()
            newv.SetNumberOfComponents(3)
            newv.InsertTupleValue(0, [lon.min(), lat.min(), 0])
            newv.InsertTupleValue(1, [lon.max(), lat.max(), 0])

            vcs2vtk.projectArray(newv, projection, self._vtkDataSetBounds)
            dimMin = [0, 0, 0]
            dimMax = [0, 0, 0]

            newv.GetTupleValue(0, dimMin)
            newv.GetTupleValue(1, dimMax)

            maxDimX = max(dimMin[0], dimMax[0])
            maxDimY = max(dimMin[1], dimMax[1])

            if lat.max() != 0.0:
                scale = abs((maxDimY / lat.max()))

            if lon.max() != 0.0:
                temp = abs((maxDimX / lon.max()))
                if scale < temp:
                    scale = temp
        else:
            scale = 1.0

        # Vector attempt
        l = self._gm.line
        if l is None:
            l = "default"
        try:
            l = vcs.getline(l)
            lwidth = l.width[0]  # noqa
            lcolor = l.color[0]
            lstyle = l.type[0]  # noqa
        except:
            lstyle = "solid"  # noqa
            lwidth = 1.  # noqa
            lcolor = 0
        if self._gm.linewidth is not None:
            lwidth = self._gm.linewidth  # noqa
        if self._gm.linecolor is not None:
            lcolor = self._gm.linecolor

        arrow = vtk.vtkGlyphSource2D()
        arrow.SetGlyphTypeToArrow()
        arrow.SetOutputPointsPrecision(vtk.vtkAlgorithm.DOUBLE_PRECISION)
        arrow.FilledOff()

        polydata = self._vtkPolyDataFilter.GetOutput()
        vectors = polydata.GetPointData().GetVectors()

        if self._gm.scaletype == 'constant' or\
           self._gm.scaletype == 'constantNNormalize' or\
           self._gm.scaletype == 'constantNLinear':
            scaleFactor = scale * 2.0 * self._gm.scale
        else:
            scaleFactor = 1.0

        glyphFilter = vtk.vtkGlyph2D()
        glyphFilter.SetInputData(polydata)
        glyphFilter.SetInputArrayToProcess(1, 0, 0, 0, "vector")
        glyphFilter.SetSourceConnection(arrow.GetOutputPort())
        glyphFilter.SetVectorModeToUseVector()

        # Rotate arrows to match vector data:
        glyphFilter.OrientOn()
        glyphFilter.ScalingOn()

        glyphFilter.SetScaleModeToScaleByVector()

        if self._gm.scaletype == 'normalize' or self._gm.scaletype == 'linear' or\
           self._gm.scaletype == 'constantNNormalize' or self._gm.scaletype == 'constantNLinear':

            # Find the min and max vector magnitudes
            maxNorm = vectors.GetMaxNorm()

            if maxNorm == 0:
                maxNorm = 1.0

            if self._gm.scaletype == 'normalize' or self._gm.scaletype == 'constantNNormalize':
                scaleFactor /= maxNorm

            if self._gm.scaletype == 'linear' or self._gm.scaletype == 'constantNLinear':
                minNorm = None
                maxNorm = None

                noOfComponents = vectors.GetNumberOfComponents()
                for i in range(0, vectors.GetNumberOfTuples()):
                    norm = vtk.vtkMath.Norm(vectors.GetTuple(i), noOfComponents)

                    if (minNorm is None or norm < minNorm):
                        minNorm = norm
                    if (maxNorm is None or norm > maxNorm):
                        maxNorm = norm

                if maxNorm == 0:
                    maxNorm = 1.0

                scalarArray = vtk.vtkDoubleArray()
                scalarArray.SetNumberOfComponents(1)
                scalarArray.SetNumberOfValues(vectors.GetNumberOfTuples())

                oldRange = maxNorm - minNorm
                oldRange = 1.0 if oldRange == 0.0 else oldRange

                # New range min, max.
                newRangeValues = self._gm.scalerange
                newRange = newRangeValues[1] - newRangeValues[0]

                for i in range(0, vectors.GetNumberOfTuples()):
                    norm = vtk.vtkMath.Norm(vectors.GetTuple(i), noOfComponents)
                    newValue = (((norm - minNorm) * newRange) / oldRange) + newRangeValues[0]
                    scalarArray.SetValue(i, newValue)
                    polydata.GetPointData().SetScalars(scalarArray)

                # Scale to vector magnitude:
                # NOTE: Currently we compute our own scaling factor since VTK does
                # it by clamping the values > max to max  and values < min to min
                # and not remap the range.
                glyphFilter.SetScaleModeToScaleByScalar()

        glyphFilter.SetScaleFactor(scaleFactor)

        mapper = vtk.vtkPolyDataMapper()

        glyphFilter.Update()
        data = glyphFilter.GetOutput()

        mapper.SetInputData(data)
        mapper.ScalarVisibilityOff()
        act = vtk.vtkActor()
        act.SetMapper(mapper)

        cmap = self.getColorMap()
        r, g, b, a = cmap.index[lcolor]
        act.GetProperty().SetColor(r / 100., g / 100., b / 100.)

        plotting_dataset_bounds = vcs2vtk.getPlottingBounds(
            vcs.utils.getworldcoordinates(self._gm,
                                          self._data1.getAxis(-1),
                                          self._data1.getAxis(-2)),
            self._vtkDataSetBounds, self._vtkGeoTransform)
        x1, x2, y1, y2 = plotting_dataset_bounds
        if self._vtkGeoTransform is None:
            wc = plotting_dataset_bounds
        else:
            xrange = list(act.GetXRange())
            yrange = list(act.GetYRange())
            wc = [xrange[0], xrange[1], yrange[0], yrange[1]]

        vp = self._resultDict.get('ratio_autot_viewport',
                                  [self._template.data.x1, self._template.data.x2,
                                   self._template.data.y1, self._template.data.y2])
        # look for previous dataset_bounds different than ours and
        # modify the viewport so that the datasets are alligned
        # Hack to fix the case when the user does not specify gm.datawc_...
        # if geo is None:
        #     for dp in vcs.elements['display'].values():
        #         if (hasattr(dp, 'backend')):
        #             prevWc = dp.backend.get('dataset_bounds', None)
        #             if (prevWc):
        #                 middleX = float(vp[0] + vp[1]) / 2.0
        #                 middleY = float(vp[2] + vp[3]) / 2.0
        #                 sideX = float(vp[1] - vp[0]) / 2.0
        #                 sideY = float(vp[3] - vp[2]) / 2.0
        #                 ratioX = float(prevWc[1] - prevWc[0]) / float(wc[1] - wc[0])
        #                 ratioY = float(prevWc[3] - prevWc[2]) / float(wc[3] - wc[2])
        #                 sideX = sideX / ratioX
        #                 sideY = sideY / ratioY
        #                 vp = [middleX - sideX, middleX + sideX, middleY - sideY, middleY + sideY]

        dataset_renderer, xScale, yScale = self._context().fitToViewport(
            act, vp,
            wc=wc,
            priority=self._template.data.priority,
            create_renderer=True)
        kwargs = {'vtk_backend_grid': self._vtkDataSet,
                  'dataset_bounds': self._vtkDataSetBounds,
                  'plotting_dataset_bounds': plotting_dataset_bounds}
        if ('ratio_autot_viewport' in self._resultDict):
            kwargs["ratio_autot_viewport"] = vp
        self._resultDict.update(self._context().renderTemplate(
            self._template, self._data1,
            self._gm, taxis, zaxis, **kwargs))

        if self._context().canvas._continents is None:
            self._useContinents = False
        if self._useContinents:
            continents_renderer, xScale, yScale = self._context().plotContinents(
                plotting_dataset_bounds, projection,
                self._dataWrapModulo, vp, self._template.data.priority,
                vtk_backend_grid=self._vtkDataSet,
                dataset_bounds=self._vtkDataSetBounds)
        self._resultDict["vtk_backend_actors"] = [[act, plotting_dataset_bounds]]
        self._resultDict["vtk_backend_glyphfilters"] = [glyphFilter]
        self._resultDict["vtk_backend_luts"] = [[None, None]]

    def _updateContourLevelsAndColors(self):
        """Overrides baseclass implementation."""
        pass
