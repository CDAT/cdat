import cdms2
from .node import GetVariableOperation, SubsetVariableOperation, TransformOperation, MetadataOperation, AxisOperation
import operator
import cdutil
import numpy


def open_file(uri):
    return cdms2.open(uri)


def close_file(f):
    f.close()


class NumpyGetVariableOperation(GetVariableOperation):
    """
    Implements the retrieval of a variable from a file.

    Expects just an "id" argument, and the parent should be a file object (as defined by the backend, not a node).
    """
    def evaluate(self, values):
        # At some point we should include support for numpy.zeroes/numpy.ones
        var_id = self._arguments["id"]
        return values[0](var_id)


class NumpySubsetVariableOperation(SubsetVariableOperation):
    """
    Reduces one or more axes of a variable.

    Expects a dictionary of axes and the selectors to use to reduce those axes (axes specified by ID)
    """
    def evaluate(self, values):
        variable = values[0]
        axes = self._arguments["axes"]

        # Validate that the axes exist
        var_axis_ids = variable.getAxisIds()
        for ax in axes:
            if ax not in var_axis_ids:
                raise ValueError("No axis '%s' found." % (ax))

        kwargs = axes.copy()
        kwargs["squeeze"] = self._arguments.get("squeeze", False)
        kwargs["order"] = self._arguments.get('order', None)
        kwargs["grid"] = self._arguments.get('grid', None)

        return variable(**kwargs)


class NumpyTransformVariableOperation(TransformOperation):
    """
    Calls the appropriate function on a variable.
    """
    func_map = {
        "remainder": cdms2.MV2.remainder,
        "hypot": cdms2.MV2.hypot,
        "arctan2": cdms2.MV2.arctan2,
        "outerproduct": cdms2.MV2.outerproduct,
        "log": cdms2.MV2.log,
        "log10": cdms2.MV2.log10,
        "conjugate": cdms2.MV2.conjugate,
        "sin": cdms2.MV2.sin,
        "cos": cdms2.MV2.cos,
        "tan": cdms2.MV2.tan,
        "arcsin": cdms2.MV2.arcsin,
        "arccos": cdms2.MV2.arccos,
        "arctan": cdms2.MV2.arctan,
        "sinh": cdms2.MV2.sinh,
        "cosh": cdms2.MV2.cosh,
        "tanh": cdms2.MV2.tanh,
        "fabs": cdms2.MV2.fabs,
        "nonzero": cdms2.MV2.nonzero,
        "around": cdms2.MV2.around,
        "floor": cdms2.MV2.floor,
        "ceil": cdms2.MV2.ceil,
        "sqrt": cdms2.MV2.sqrt,
        "absolute": cdms2.MV2.absolute,
        "sometrue": cdms2.MV2.sometrue,
        "alltrue": cdms2.MV2.alltrue,
        "max": cdms2.MV2.max,
        "min": cdms2.MV2.min,
        "sort": cdms2.MV2.sort,
        "count": cdms2.MV2.count,
        "product": cdms2.MV2.product,
        "sum": cdms2.MV2.sum,
        "average": cdms2.MV2.average,
        "choose": cdms2.MV2.choose,
        "take": cdms2.MV2.take,
        "transpose": cdms2.MV2.transpose,
        "argsort": cdms2.MV2.argsort,
        "repeat": cdms2.MV2.repeat,
        "reshape": cdms2.MV2.reshape,
        "resize": cdms2.MV2.resize,
        "diagonal": cdms2.MV2.diagonal,
        "add": cdms2.MV2.add,
        "sub": cdms2.MV2.subtract,
        "mul": cdms2.MV2.multiply,
        "div": cdms2.MV2.divide,
        "eq": cdms2.MV2.equal,
        "le": cdms2.MV2.less_equal,
        "ge": cdms2.MV2.greater_equal,
        "lt": cdms2.MV2.less,
        "gt": cdms2.MV2.greater,
        "ne": cdms2.MV2.not_equal,
        "not": cdms2.MV2.logical_not,
        "and": cdms2.MV2.bitwise_and,
        "or": cdms2.MV2.bitwise_or,
        "xor": cdms2.MV2.bitwise_xor,
        "pow": cdms2.MV2.power,
        "neg": cdms2.MV2.negative,
    }

    def execute_transform(self, func_name, args, kwargs):
        return self.func_map[func_name](*args, **kwargs)


class NumpyMetadataOperation(MetadataOperation):
    """
    Updates the variables metadata.

    Expects a dictionary of attributes and the new values to assign to them.
    """
    def evaluate(self, values):
        variable = values[0]
        for k, v in self._arguments["attributes"].iteritems():
            variable.attributes[k] = v

        if "id" in self._arguments:
            variable.id = self._arguments["id"]

        return variable


class NumpyAxisOperation(AxisOperation):
    """
    Updates the metadata of variable axes.

    Expects an axis identifier, and an attributes dictionary.
    """
    def evaluate(self, values):
        variable = values[0]

        axis = self._arguments["axis"]
        if axis not in variable.getAxisIds():
            raise ValueError("No axis '%s' available." % axis)

        axis = variable.getAxis(variable.getAxisIndex(axis))

        attributes = self._arguments["attributes"]

        for attr in attributes:
            if attr == "bounds":
                if axis.isTime():
                    if attributes["bounds"] == "daily":
                        cdutil.setAxisTimeBoundsDaily(axis)
                        continue
                    elif attributes["bounds"] == "monthly":
                        cdutil.setAxisTimeBoundsMonthly(axis)
                        continue
                    elif attributes["bounds"] == "yearly":
                        cdutil.setAxisTimeBoundsYearly(axis)
                        continue

                axis.setBounds(numpy.array(attributes["bounds"]))
            else:
                setattr(axis, attr, attributes[attr])

        return variable
