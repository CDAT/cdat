import json
import node
import numpy_backend


def derive_variable(json_spec, backend=numpy_backend):
    if isinstance(json_spec, (str, unicode)):
        definition = json.loads(json_spec)
    elif isinstance(json_spec, dict):
        definition = json_spec
    else:
        raise ValueError("json_spec must be a dictionary or JSON-encoded text.")

    variable_id = definition.get("id", None)
    derivation = definition.get("derivation", [])

    derivation_results = []
    for step in derivation:
        if "type" not in step:
            raise ValueError("No derivation step type provided.")

        result = None

        if step["type"].lower() == "file":
            result = node.FileNode(step["uri"], backend)

        if step["type"].lower() == "variable":
            operation = node.create_operation(step["operation"], backend)
            result = node.VariableNode(operation, [derivation_results[i] for i in step["parents"]])

        if result is None:
            raise ValueError("Unsupported derivation step type: %s" % (step["type"]))

        derivation_results.append(result)

    var = derivation_results[-1].get_value()
    if "id" in definition:
        var.id = str(definition["id"])
    return var
