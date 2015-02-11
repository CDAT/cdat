import priority
import vcs.vtk_ui

class GroupEditor(priority.PriorityEditor, vcs.vtk_ui.behaviors.ClickableMixin, vcs.vtk_ui.behaviors.DraggableMixin):
    def __init__(self, interactor, targets):

        self.targets = []
        for target in targets:
            if target is None:
                continue
            target.unregister()
            self.targets.append(target)

        self.interactor = interactor
        super(GroupEditor, self).__init__()

    def is_object(self, obj):
        return self.contains_object(obj)

    def in_bounds(self, x, y):
        return True

    def handle_click(self, point):
        return True

    def add_target(self, target):
        target.unregister()
        self.targets.append(target)

    def remove_target(self, target):
        self.targets.remove(target)
        target.detach()

    def multiplex(self, action, *args, **kwargs):
        for target in self.targets:
            try:
                func = getattr(target, action)
                func(*args, **kwargs)
            except AttributeError:
                pass

    def remove_object(self, obj):
        for target in self.targets:
            if target.is_object(obj):
                self.remove_target(target)

    def contains_object(self, obj):
        for target in self.targets:
            if target.is_object(obj):
                return True
        return False

    def place(self):
        self.multiplex("place")

    def deactivate(self):
        self.multiplex("deactivate")

    def click_press(self):
        self.multiplex("click_press")

    def click_release(self):
        self.multiplex("click_release")

    def double_press(self):
        self.multiplex("double_press")

    def double_release(self):
        self.multiplex("double_release")

    def right_press(self):
        self.multiplex("right_press")

    def right_release(self):
        self.multiplex("right_release")

    def key_pressed(self, key, shift=False, alt=False, control=False):
        self.multiplex("key_pressed", key, shift=False, alt=False, control=False)

    def key_released(self, key, shift=False, alt=False, control=False):
        self.multiplex("key_released", key, shift=False, alt=False, control=False)

    def drag_start(self):
        self.multiplex("drag_start")

    def drag_move(self, delta_x, delta_y):
        self.multiplex("drag_move", delta_x, delta_y)

    def drag_stop(self):
        self.multiplex("drag_stop")