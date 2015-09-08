import weakref
import vcs


class Pipeline(object):

    """Base class for a VTK pipeline implementation of a VCS plot command.

    The Pipeline class defines an interface for creating VTK pipeline from a
    VTK plot command. Refer to the method documentation for details.
    """

    def __init__(self, graphics_method, context_):
        """Initialize the pipeline object.

        _gm is a vcs graphics method
        _context is a weakref of the VTKVCSBackend object that created this
            Pipeline.
        """
        self._context = weakref.ref(context_)
        self._gm = graphics_method

    # For now, we'll just throw everything at plot. This might need to be
    # broken up into set_data, set_template, etc methods...
    def plot(self, data1, data2, template, graphics_method, grid, transform):
        raise NotImplementedError("Missing override.")

    def getcolormap(self):
        _colorMap = self._gm.colormap
        if _colorMap is None:
            _colorMap = \
                _colorMap = self._context().canvas.getcolormapname()
        if _colorMap is None:
            _colorMap = vcs._colorMap
        if isinstance(_colorMap, str):
            _colorMap = vcs.elements["colormap"][_colorMap]
        return _colorMap
