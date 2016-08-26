import weakref
import logging


class Node(object):
    def __init__(self):
        self.__cache__ = None

    def get_value(self):
        """
        Retrieves the cached version of the value, or calculates it if no cache is available.

        Uses a weakreference to avoid memory pressure, recalculates if necessary.
        """
        if self.__cache__ is None or self.__cache__() is None:
            v = self.derive()
            try:
                self.__cache__ = weakref.ref(v)
            except TypeError:
                self.__cache__ = lambda: v
        return self.__cache__()

    def derive(self):
        raise NotImplementedError("Please implement derive for node type %s" % (type(self)))


class FileNode(Node):
    def __init__(self, uri, backend):
        super(FileNode, self).__init__()
        self._uri = uri
        self.backend = backend

    def derive(self):
        return self.backend.open_file(self._uri)

    def finalize(self, f):
        self.backend.close_file(f)
        self.__cache__ = None


class VariableNode(Node):
    def __init__(self, operation, parents):
        super(VariableNode, self).__init__()
        self._oper = operation
        self._parents = parents

    def derive(self):
        parent_values = []
        files = []
        for p in self._parents:
            parent_values.append(p.get_value())
            if isinstance(p, FileNode):
                files.append((p, parent_values[-1]))
        value = self._oper.evaluate(parent_values)
        for node, file in files:
            # Makes sure files get closed properly.
            # This is incredibly wasteful. Should really just open/close once.
            node.finalize(file)
        return value


class OperationNode(object):
    def __init__(self, spec):
        for k in self.required_arguments:
            if k not in spec:
                raise ValueError("Missing required argument: %s" % k)
            if not isinstance(spec[k], self.required_arguments[k]):
                raise TypeError("Argument '%s' should be of type %s." % (k, self.required_arguments[k]))
        self._arguments = spec

    def evaluate(self, values):
        raise NotImplementedError("Please implement evaluate for operation type: %s" % (type(self)))


class GetVariableOperation(OperationNode):
    """
    Implements the retrieval of a variable from a file.

    Expects just an "id" argument, and the parent should be a file object (as defined by the backend, not a node).
    """
    required_arguments = {"id": (str, unicode)}


class SubsetVariableOperation(OperationNode):
    """
    Reduces one or more axes of a variable.

    Expects a dictionary of axes and the selectors to use to reduce those axes (axes specified by ID)
    """
    required_arguments = {"axes": dict}


class TransformOperation(OperationNode):
    """
    Calls the appropriate function on a variable.
    """
    required_arguments = {"function": (str, unicode)}
    argument_specs = {
        "remainder": ([0, 1], {}),
        "hypot": ([0, 1], {}),
        "arctan2": ([0, 1], {}),
        "outerproduct": ([0, 1], {}),
        "log": ([0], {}),
        "log10": ([0], {}),
        "conjugate": ([0], {}),
        "sin": ([0], {}),
        "cos": ([0], {}),
        "tan": ([0], {}),
        "arcsin": ([0], {}),
        "arccos": ([0], {}),
        "arctan": ([0], {}),
        "sinh": ([0], {}),
        "cosh": ([0], {}),
        "tanh": ([0], {}),
        "fabs": ([0], {}),
        "nonzero": ([0], {}),
        "around": ([0], {}),
        "floor": ([0], {}),
        "ceil": ([0], {}),
        "sqrt": ([0], {}),
        "absolute": ([0], {}),
        "sometrue": ([0], {"axis": None}),
        "alltrue": ([0], {"axis": None}),
        "max": ([0], {"axis": None}),
        "min": ([0], {"axis": None}),
        "sort": ([0], {"axis": None}),
        "count": ([0], {"axis": None}),
        "product": ([0], {"axis": None, "dtype": None}),
        "sum": ([0], {"axis": None, "fill_value": 0, "dtype": None}),
        "average": ([0], {"axis": None, "returned": False, "weights": None}),
        "choose": (["indices", 0], {}),
        "take": ([0, "indices"], {"axis": None}),
        "transpose": ([0], {"axis": None}),
        "argsort": ([0], {"axis": -1, "fill_value": None}),
        "repeat": ([0, "count"], {"axis": None}),
        "reshape": ([0, "shape"], {"axis": None, "attributes": None, "id": None, "grid": None}),
        "resize": ([0, "shape"], {"axis": None, "attributes": None, "id": None, "grid": None}),
        "diagonal": ([0], {"axis1": 0, "axis2": 1, "offset": 0}),
        "add": ([0, 1], {}),
        "sub": ([0, 1], {}),
        "mul": ([0, 1], {}),
        "div": ([0, 1], {}),
        "eq": ([0, 1], {}),
        "le": ([0, 1], {}),
        "ge": ([0, 1], {}),
        "lt": ([0, 1], {}),
        "gt": ([0, 1], {}),
        "ne": ([0, 1], {}),
        "and": ([0, 1], {}),
        "or": ([0, 1], {}),
        "xor": ([0, 1], {}),
        "pow": ([0, 1], {}),
        "neg": ([0], {}),
        "not": ([0], {}),
    }

    def evaluate(self, values):
        func = self._arguments["function"]
        if func not in self.argument_specs:
            raise ValueError("Unknown transform %s." % func)

        args, kwargs = self.argument_specs[func]
        sub_args, sub_kwargs = [], {}
        for a in args:
            if isinstance(a, int):
                sub_args.append(values[a])
            if isinstance(a, (str, unicode)):
                sub_args.append(self._arguments[a])
        for kw, default in kwargs.iteritems():
            val = self._arguments.get(kw, default)
            # grid and weights are special ones that can use
            # parent values, so check if they're ints.
            if kw in ("grid", "weights") and isinstance(val, int):
                val = values[val]
            sub_kwargs[kw] = val
        return self.execute_transform(func, sub_args, sub_kwargs)


class MetadataOperation(OperationNode):
    """
    Updates the variables metadata.

    Expects a dictionary of attributes and the new values to assign to them.
    """
    required_arguments = {"attributes": dict}


class AxisOperation(OperationNode):
    """
    Updates the metadata of variable axes.

    Expects an axis identifier, and an attributes dictionary.
    """
    required_arguments = {"axis": (str, unicode), "attributes": dict}


def create_operation(oper_spec, backend):
    oper_dict = {
        "get": GetVariableOperation,
        "subset": SubsetVariableOperation,
        "transform": TransformOperation,
        "metadata": MetadataOperation,
        "axis": AxisOperation
    }

    oper_class = oper_dict.get(oper_spec["type"].lower(), None)

    if oper_class is None:
        raise ValueError("No operation of type '%s' defined." % oper_spec["type"])

    for sc in oper_class.__subclasses__():
        if sc.__module__ == backend.__name__:
            return sc(oper_spec)

    raise NotImplementedError("No implementation of operation type %s found in module %s." % (oper_spec["type"], backend.__name__))
