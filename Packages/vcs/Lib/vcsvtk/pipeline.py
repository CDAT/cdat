import weakref


class Pipeline(object):
    """Base class for a VTK pipeline implementation of a VCS plot command.

    The Pipeline class defines an interface for creating VTK pipeline from a
    VTK plot command. Refer to the method documentation for details.
    """

    def __init__(self, context_):
        """Initialize the pipeline object.

        _context is a weakref of the VTKVCSBackend object that created this
            Pipeline.
        """
        self._context = weakref.ref(context_)

    # For now, we'll just throw everything at plot. This might need to be
    # broken up into set_data, set_template, etc methods...
    def plot(self, data1, data2, template, graphics_method, grid, transform):
        raise NotImplementedError("Missing override.")

    def update_input(self, data1, data2):
        """Update the input to this pipeline."""
        raise NotImplementedError("Missing override.")
