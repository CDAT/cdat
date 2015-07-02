from .. import vcs2vtk

import numpy
import vcs
import vtk
from vtk.util.vtkAlgorithm import VTKPythonAlgorithmBase


class VCSWrapFilter(VTKPythonAlgorithmBase):
    """Wraps the vcs2vtk.doWrap logic in a VTK filter.

    world_coordinate is passed to the wc argument of doWrap.

    wrap_modulo is [YMax, XMax], in degrees. 0 means 'no wrapping'.

    fast_clip controls how the translated and expanded dataset is clipped to
    its final output dimension:
        - If True, vtkExtractPolyDataGeometry is used. This is faster, but will
          not cut cells that cross the boundary -- such cells are completely
          omitted from the output.
        - If False, vtkClipPolyData is used. This is slower, but will cut cells
          at the boundaries to preserve as much data as possible.

    The transform object is a vtkMatrix4x4 that is used to transform the input
    dataset prior to wrapping.
    """

    def __init__(self, worldCoord, wrapModulo=[0., 360.], fastClip=True,
                 transform=None):
        VTKPythonAlgorithmBase.__init__(
              self, nInputPorts=1, inputType='vtkPolyData',
              nOutputPorts=1, outputType='vtkPolyData')
        self._world_coordinate = worldCoord
        self._wrap_modulo = wrapModulo
        self._fast_clip = fastClip
        self._transform = transform

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

    @property
    def transform(self):
        return self._transform

    @transform.setter
    def transform(self, value):
        if value != self._transform:
            self._transform = value
            self.Modified()

    def RequestData(self, request, inInfo, outInfo):
        """Calls vcs2vtk.doWrap to produce the output dataset."""
        input = vtk.vtkPolyData.GetData(inInfo[0].GetInformationObject(0))
        output = vtk.vtkPolyData.GetData(outInfo.GetInformationObject(0))

        # vcs2vtk.doWrap uses rendering objects as data containers:
        actor = vtk.vtkActor()
        mapper = vtk.vtkPolyDataMapper()

        actor.SetMapper(mapper)
        mapper.SetInputData(input)

        if self._transform is not None:
            actor.SetUserMatrix(self._transform)

        vcs2vtk.doWrap(actor, self._world_coordinate, self._wrap_modulo,
                       self._fast_clip)

        output.ShallowCopy(mapper.GetInput())

        return 1
