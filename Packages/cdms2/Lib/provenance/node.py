import inspect
import importlib
import operator


class VariableNode(object):
    def __init__(self, operation, arguments, keyword_arguments, parents):
        self._oper = operation
        self._args = arguments
        self._kwargs = keyword_arguments
        self._parents = parents
        self._result = None

    @property
    def result(self):
        if self._result is None:
            self._result = self.calculate()
        return self._result

    def calculate(self):
        return self._oper.calculate(*self._args, **self._kwargs)

    def format(self):
        return {"type": "variable", "id": self.id()}

    def dump(self, dictionary=None):
        if dictionary is None:
            dictionary = {
                "files": [],
                "variables": []
            }

        for dep in self.parents:
            dep.serialize(dictionary)

        dictionary["variables"].append({
            "id": self.id(),
            "arguments": [a.format() for a in self._args],
            "operation": self._oper.format()
        })

        return dictionary

    def id(self):
        if self._result is not None:
            return self._result.id
        else:
            return self._oper.id() + "_".join([p.id() for p in self.parents])


class TransformNode(object):

    key = None

    def __init__(self, value):
        self.value = value

    def format(self):
        return {"key": self.key, "value": self.value}

    def calculate(self, args, kwargs):
        raise NotImplementedError("You need to implement calculate on %s." % (type(self)))


class FunctionTransform(TransformNode):
    key = "func"

    def __init__(self, func):
        if callable(func):
            self.value = func.__name__
            self.module = inspect.getmodulename(func)
        else:
            if "." in func:
                parts = func.split(".")
                self.value = parts[-1]
                self.module = ".".join(parts[:-1])
            else:
                raise ValueError("Argument should be either a function object or the complete module/function name of some function.")

    def calculate(self, args, kwargs):
        module = importlib.import_module(self.module)
        func = getattr(module, self.value)
        return func(*args, **kwargs)

    def format(self):
        d = super(FunctionOperation, self).format()
        d["module"] = self.module
        return d

    def id(self):
        return self.value


class OperatorTransform(TransformNode):
    key = "operator"
    unary_operations = {
        "-": operator.neg,
        "~": operator.invert,
        "+": operator.pos,
        "not": operator.not_
    }
    binary_operations = {
        "+": operator.add,
        "/": operator.div,
        "&": opeartor.and_,
        "^": operator.xor,
        "|": opeartor.or_,
        "**": operator.pow,
        "is": operator.is_,
        "is not": operator.is_not,
        "<<": operator.lshift,
        "%": operator.mod,
        "*": operator.mul,
        ">>": operator.rshift,
        "-": operator.sub,
        "<": operator.lt,
        ">": operator.gt,
        ">=": operator.gte,
        "<=": operator.lte,
        "==": operator.eq,
        "!=": operator.ne,
        "in": operator.contains
    }

    def __init__(self, oper, binary=True):
        self.value = oper
        oper_dict = self.binary_operations if binary else self.unary_operations
        self.operation = oper_dict[oper]

    def calculate(self, args):
        return self.operation(*args)

    def id(self):
        return self.operation.__name__.rstrip("_")


def load(dictionary):
    for f in dictionary["files"]:
        pass
    for v in dictionary["variables"]:
        pass
