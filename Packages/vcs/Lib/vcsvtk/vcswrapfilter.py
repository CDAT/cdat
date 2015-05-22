from .. import vcs2vtk

import numpy
import vcs
import vtk
from vtk.util.vtkAlgorithm import VTKPythonAlgorithmBase


class VCSWrapFilter(VTKPythonAlgorithmBase):
    """Reproduces the vcs2vtk.doWrap logics as a VTK filter.

    The doWrap arguments are implemented as the following filter attributes:
        - wc --> world_coordinate
        - wrap --> wrap_modulo
        - fastClip --> fast_clip

    world_coordinate is ??? TODO Charles, I need a docstring on this.

    wrap_modulo is [YMax, XMax], in degrees. 0 means 'no wrapping'.

    fast_clip controls how the translated and expanded dataset is clipped to
    its final output dimension:
        - If True, vtkExtractPolyDataGeometry is used. This is faster, but will
          not cut cells that cross the boundary -- such cells are completely
          omitted from the output.
        - If False, vtkClipPolyData is used. This is slower, but will cut cells
          at the boundaries to preserve as much data as possible.
    """

    def __init__(self, worldCoord, wrapModulo=[0., 360.], fastClip=True):
        VTKPythonAlgorithmBase.__init__(
              self, nInputPorts=1, inputType='vtkPolyData',
              nOutputPorts=1, outputType='vtkPolyData')
        self._world_coordinate = worldCoord
        self._wrap_modulo = wrapModulo
        self._fast_clip = fastClip

        print "Created new filter!"

        import weakref
        weakFilter = weakref.ref(self)
        def printRefs():
            filter = weakFilter()
            if filter is None:
                print "Searching for filter ref holders: Already gc'd!"
            else:
                print "\n\nFilter ref holders:"
                import gc
                for i,ref in enumerate(gc.get_referrers(filter)):
                    print "\n############%d###########\n%s"%(i, str(ref))
        import atexit
        atexit.register(printRefs)

    @property
    def world_coordinate(self):
        return self._world_coordinate

    @world_coordinate.setter
    def world_coordinate(self, value):
        if value != self._world_coordinate:
            self._world_coordinate = value
            self.Modified()

    @property
    def wrap_modulo(self):
        return self._wrap_modulo

    @wrap_modulo.setter
    def wrap_modulo(self, value):
        if value != self._wrap_modulo:
            self._wrap_modulo = value
            self.Modified()

    @property
    def fast_clip(self):
        return self._fast_clip

    @fast_clip.setter
    def fast_clip(self, value):
        if value != self._fast_clip:
            self._fast_clip = value
            self.Modified()

    def RequestData(self, request, inInfo, outInfo):
        """Reimplemented from base class."""
        input = vtk.vtkPolyData.GetData(inInfo[0].GetInformationObject(0))
        output = vtk.vtkPolyData.GetData(outInfo.GetInformationObject(0))

        # DL: The following is a copy/paste of vcs2vtk.doWrap, with some style
        # and variable name refactoring.

        xmn = min(self._world_coordinate[0], self._world_coordinate[1])
        xmx = max(self._world_coordinate[0], self._world_coordinate[1])
        if numpy.allclose(xmn, 1.e20) or numpy.allclose(xmx, 1.e20):
            xmx = abs(self._wrap_modulo[1])
            xmn = -self._wrap_modulo[1]
        ymn = min(self._world_coordinate[2], self._world_coordinate[3])
        ymx = max(self._world_coordinate[2], self._world_coordinate[3])
        if numpy.allclose(ymn, 1.e20) or numpy.allclose(ymx, 1.e20):
            ymx = abs(self._wrap_modulo[0])
            ymn = -self._wrap_modulo[0]

        # Prepare MultiBlock and puts in oriinal data
        appendFilter = vtk.vtkAppendPolyData()
        appendFilter.AddInputData(input)
        appendFilter.Update()

        # X axis wrappping
        Amn, Amx = input.GetBounds()[0:2]
        if self._wrap_modulo[1] != 0.:
            i = 0
            while Amn > xmn:
                i += 1
                Amn -= self._wrap_modulo[1]
                Tpf = vtk.vtkTransformPolyDataFilter()
                Tpf.SetInputData(input)
                T = vtk.vtkTransform()
                T.Translate(-i * self._wrap_modulo[1], 0, 0)
                Tpf.SetTransform(T)
                Tpf.Update()
                appendFilter.AddInputData(Tpf.GetOutput())
                appendFilter.Update()
            i = 0
            while Amx < xmx:
                i += 1
                Amx += self._wrap_modulo[1]
                Tpf = vtk.vtkTransformPolyDataFilter()
                Tpf.SetInputData(input)
                T = vtk.vtkTransform()
                T.Translate(i * self._wrap_modulo[1], 0, 0)
                Tpf.SetTransform(T)
                Tpf.Update()
                appendFilter.AddInputData(Tpf.GetOutput())
                appendFilter.Update()

        # Y axis wrapping
        Amn, Amx = input.GetBounds()[2:4]
        if self._wrap_modulo[0] != 0.:
            i = 0
            while Amn > ymn:
                i += 1
                Amn -= self._wrap_modulo[0]
                Tpf = vtk.vtkTransformPolyDataFilter()
                Tpf.SetInputData(input)
                T = vtk.vtkTransform()
                T.Translate(0, i * self._wrap_modulo[0], 0)
                Tpf.SetTransform(T)
                Tpf.Update()
                appendFilter.AddInputData(Tpf.GetOutput())
                appendFilter.Update()
            i = 0
            while Amx < ymx:
                i += 1
                Amx += self._wrap_modulo[0]
                Tpf = vtk.vtkTransformPolyDataFilter()
                Tpf.SetInputData(input)
                T = vtk.vtkTransform()
                T.Translate(0, -i * self._wrap_modulo[0], 0)
                Tpf.SetTransform(T)
                Tpf.Update()
                appendFilter.AddInputData(Tpf.GetOutput())
                appendFilter.Update()

        # Clip the data to the final window:
        clipBox = vtk.vtkBox()
        clipBox.SetXMin(xmn, ymn, -1.0)
        clipBox.SetXMax(xmx, ymx,  1.0)
        if self._fast_clip:
            clipper = vtk.vtkExtractPolyDataGeometry()
            clipper.ExtractInsideOn()
            clipper.SetImplicitFunction(clipBox)
            clipper.ExtractBoundaryCellsOn()
            clipper.PassPointsOff()
        else:
            clipper = vtk.vtkClipPolyData()
            clipper.InsideOutOn()
            clipper.SetClipFunction(clipBox)
        clipper.SetInputConnection(appendFilter.GetOutputPort())
        clipper.Update()

        output.ShallowCopy(clipper.GetOutput())

        return 1
