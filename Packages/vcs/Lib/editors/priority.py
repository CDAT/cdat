from vcs.vtk_ui.behaviors import KeyableMixin

class PriorityEditor(KeyableMixin):
    """
    Provides basic keyboard manipulation of object's priority– up / down to raise / lower priority, delete/backspace to set it to 0
    """
    def key_pressed(self, key, shift=False, alt=False, control=False):
        if key == "Up":
            self.raise_priority()
        elif key == "Down":
            self.lower_priority()
        elif (len(key) == 1 and ord(key) == 127) or key in ("Delete", "Backspace"):
            self.delete()

    def get_object(self):
        raise NotImplementedError("Implement get_object to use the PriorityEditor")

    def update_priority(self):
        raise NotImplementedError("Implement update_priority to use the PriorityEditor")

    def raise_priority(self):
        obj = self.get_object()
        obj.priority += 1
        self.update_priority()

    def lower_priority(self):
        obj = self.get_object()
        if obj.priority > 1:
            obj.priority -= 1
            self.update_priority()

    def delete(self):
        obj = self.get_object()
        obj.priority = 0