import weakref


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
            self.__cache__ = weakref.ref(v)
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


class TransformVariableOperation(OperationNode):
    """
    Calls the appropriate function on a variable.
    """
    required_arguments = {"function": (str, unicode)}


class OperatorOperation(OperationNode):
    """
    Performs the appropriate mathematical operator on the parent variables.
    """
    required_arguments = {"operator": (str, unicode)}


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
        "transform": TransformVariableOperation,
        "operator": OperatorOperation,
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
