import vcs


def createPipeline(graphics_method, context):
    """Create and initialize a Pipeline subclass from a graphics method.

    Returns None if the graphics method is not recognized.
    """
    if not vcs.isgraphicsmethod(graphics_method):
        return None

    if graphics_method.g_name == "Gfb":
        from .boxfillpipeline import BoxfillPipeline
        return BoxfillPipeline(context)
    elif graphics_method.g_name == "Gfi":
        from .isofillpipeline import IsofillPipeline
        return IsofillPipeline(context)
    elif graphics_method.g_name == "Gi":
        from .isolinepipeline import IsolinePipeline
        return IsolinePipeline(context)
    elif graphics_method.g_name == "Gfm":
        from .meshfillpipeline import MeshfillPipeline
        return MeshfillPipeline(context)
    elif graphics_method.g_name == "G1d":
        from .pipeline1d import Pipeline1D
        return Pipeline1D(context)
    elif graphics_method.g_name == "Gv":
        from .vectorpipeline import VectorPipeline
        return VectorPipeline(context)

    return None
