from vcs.vtk_ui import behaviors
import priority


class PointEditor(behaviors.ClickableMixin, behaviors.DraggableMixin, priority.PriorityEditor):
    """
    Base editor for anything with a single "x" and "y" coordinate (so mostly labels)

    Draggable, deactivates if double clicked, priority editing.
    """
    def __init__(self, interactor, point, configurator):
        self.point = point
        self.interactor = interactor
        self.configurator = configurator
        super(PointEditor, self).__init__()
        self.register()

    def get_object(self):
        return self.point

    def drag_stop(self):
        self.save()

    def double_release(self):
        x, y = self.event_position()
        if self.in_bounds(x, y):
            self.deactivate()

    def is_object(self, point):
        return point == self.point

    def handle_click(self, point):
        x, y = point
        w, h = self.interactor.GetRenderWindow().GetSize()

        adjusted_x, adjusted_y = float(x) / w, float(y) / h
        try:
            return self.in_bounds(adjusted_x, adjusted_y) or self.toolbar.in_toolbar(x, y)
        except AttributeError:
            return self.in_bounds(adjusted_x, adjusted_y)

    def render(self):
        from vcs.vtk_ui.manager import get_manager
        m = get_manager(self.interactor)
        m.queue_render()

    def drag_move(self, d_x, d_y):
        self.point.x += d_x
        self.point.y += d_y
        try:
            w, h = self.interactor.GetRenderWindow().GetSize()
            x, y = self.actor.GetPosition()
            self.actor.SetPosition(x + w * d_x, y + h * d_y)
            self.render()
        except AttributeError:
            self.configurator.changed = True

    def in_bounds(self, x, y):
        return in_point(self.point, x, y)

    def save(self):
        self.configurator.save()

    def detach(self):
        self.unregister()

    def deactivate(self):
        self.configurator.deactivate(self)


def in_point(point, x, y):
    if x < point.x + .001 and x > point.x - .001 and y > point.y - .001 and y < point.y + .001:
        return True
    else:
        return False
