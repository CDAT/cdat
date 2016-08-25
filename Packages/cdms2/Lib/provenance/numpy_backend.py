import cdms2
from .node import GetVariableOperation, SubsetVariableOperation, TransformVariableOperation, OperatorOperation, MetadataOperation, AxisOperation
import operator


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
        return variable(**axes)


class NumpyTransformVariableOperation(TransformVariableOperation):
    """
    Calls the appropriate function on a variable.
    """
    binary_functions = {
        "remainder": cdms2.MV2.remainder,
        "hypot": cdms2.MV2.hypot,
        "arctan2": cdms2.MV2.arctan2,
        "outerproduct": cdms2.MV2.outerproduct,
    }

    unary_functions = {
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
        "absolute": cdms2.MV2.absolute
    }

    unary_with_axis = {
        "sometrue": cdms2.MV2.sometrue,
        "alltrue": cdms2.MV2.alltrue,
        "max": cdms2.MV2.max,
        "min": cdms2.MV2.min,
        "sort": cdms2.MV2.sort,
        "count": cdms2.MV2.count
    }

    misc_functions = {
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
        "diagonal": cdms2.MV2.diagonal
    }

    def evaluate(self, values):
        function = self._arguments["function"]

        # Binary operations need two arrays
        if function in self.binary_functions:
            if len(values) != 2:
                raise ValueError("Function %s requires two variables. %d provided." % (self._arguments["function"], len(values)))
            return self.binary_functions[function](values[0], values[1])

        if function == "average":
            if len(values) == 2:
                # Second array is the weights
                weights = values[1]
            else:
                weights = None
            axis = self._arguments.get("axis", None)
            returned = self._arguments.get("returned", False)
            return cdms2.MV2.average(values[0], axis=axis, weights=weights, returned=returned)
        if function == "compress":
            if "mask" in self._arguments:
                mask = self._arguments["mask"]
            else:
                if len(values) == 2:
                    mask = values[1]
                else:
                    raise ValueError("Function compress requires two variables or a mask argument.")
            return cdms2.MV2.compress(values[0], mask)

        # All of the rest only require a single array
        if len(values) != 1:
            raise ValueError("Function %s requires one variable. %d provided." % (self._arguments["function"], len(values)))

        if function in self.unary_functions:
            return self.unary_functions[function](values[0])

        if function in self.unary_with_axis:
            axis = self._arguments.get("axis", None)
            if axis is None:
                return self.unary_with_axis[function](values[0])
            else:
                return self.unary_with_axis[function](values[0], axis=axis)

        # args will be an index into values or a string to fetch from _arguments
        # kwargs is a dict of default values that will be fetched from _arguments if given.
        # Most just use the array as the only positional arg
        args = (0,)
        kwargs = {"axis": None}
        if function == "product":
            kwargs = {"axis": 0, dtype: None}
        elif function == "sum":
            kwargs = {"axis": None, "fill_value": 0, "dtype": None}
        elif function == "choose":
            args = ("indices", 0)
            kwargs = {}
        elif function == "take":
            args = (0, "indices")
        elif function == "transpose":
            # Defaults are good
            pass
        elif function == "argsort":
            kwargs = {"axis": -1, "fill_value": None}
        elif function == "repeat":
            args = (0, "repeats")
        elif function == "reshape":
            args = (0, "shape")
            kwargs = {"axes": None, "attributes": None, "id": None, "grid": None}
        elif function == "resize":
            args = (0, "shape")
            kwargs = {"axes": None, "attributes": None, "id": None, "grid": None}
        elif function == "diagonal":
            kwargs = {"offset": 0, "axis1": 0, "axis2": 1}
        else:
            raise ValueError("No function '%s' defined for Numpy backend." % function)

        real_args = []
        for arg in args:
            if isinstance(arg, int):
                real_args.append(values[arg])
            elif isinstance(arg, (str, unicode)):
                real_args.append(self._arguments.get(arg, None))

        real_kwargs = {}
        for key, default in kwargs.iteritems():
            if key in self._arguments:
                real_kwargs[key] = self._arguments[key]
            else:
                real_kwargs[key] = default

        return misc_functions[function](*real_args, **real_kwargs)


class NumpyOperatorOperation(OperatorOperation):
    """
    Performs the appropriate mathematical operator on the parent variables.
    """
    binary_operators = {
        "add": operator.add,
        "concat": operator.concat,
        "contains": operator.contains,
        "div": operator.div,
        "truediv": operator.truediv,
        "floordiv": operator.floordiv,
        "and": operator.and_,
        "xor": operator.xor,
        "invert": operator.invert,
        "or": operator.or_,
        "pow": operator.pow,
        "is": operator.is_,
        "is_not": operator.is_not,
        "setitem": operator.setitem,
        "delitem": operator.delitem,
        "getitem": operator.getitem,
        "lshift": operator.lshift,
        "mod": operator.mod,
        "mul": operator.mul,
        "rshift": operator.rshift,
        "repeat": operator.repeat,
        "sub": operator.sub,
        "lt": operator.lt,
        "le": operator.le,
        "eq": operator.eq,
        "ne": operator.ne,
        "ge": operator.ge,
        "gt": operator.gt,
    }

    unary_operators = {
        "neg": operator.neg,
        "not": operator.not_,
        "pos": operator.pos,
        "truth": operator.truth,
    }

    def evaluate(self, values):
        oper = self._arguments["operator"]
        if oper in self.binary_operators:
            if len(values) != 2:
                raise ValueError("Operator %s requires 2 variables." % (oper))
            return self.binary_operators[oper](values[0], values[1])
        elif oper in self.unary_operators:
            if len(values) != 1:
                raise ValueError("Operator %s requires 1 variable." % oper)
            return self.unary_operators[oper](values[0])
        else:
            raise ValueError("Invalid operator %s." % oper)


class NumpyMetadataOperation(MetadataOperation):
    """
    Updates the variables metadata.

    Expects a dictionary of attributes and the new values to assign to them.
    """
    pass


class NumpyAxisOperation(AxisOperation):
    """
    Updates the metadata of variable axes.

    Expects an axis identifier, and an attributes dictionary.
    """
    pass
