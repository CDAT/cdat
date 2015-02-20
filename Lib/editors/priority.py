from vcs.vtk_ui.behaviors import KeyableMixin

class PriorityEditor(KeyableMixin):
    def key_pressed(self, key, shift=False, alt=False, control=False):
        if key == "Up":
            self.raise_priority()
        elif key == "Down":
            self.lower_priority()
        elif (len(key) == 1 and ord(key) == 127) or key == "Delete":
            self.delete()

    def get_object(self):
        raise NotImplementedError("Implement get_object to use the PriorityEditor")

    def raise_priority(self):
        obj = self.get_object()
        obj.priority += 1

    def lower_priority(self):
        obj = self.get_object()
        if obj.priority > 1:
            obj.priority -= 1

    def delete(self):
        obj = self.get_object()
        obj.priority = 0