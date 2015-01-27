from vcs.vtk_ui import behaviors

class PointEditor(behaviors.ClickableMixin, behaviors.DraggableMixin):
    def __init__(self, interactor, point, configurator):
        self.point = point
        self.interactor = interactor
        self.configurator = configurator
        super(PointEditor, self).__init__()
        self.register()

    def double_release(self):
        x, y = self.event_position()
        if self.in_bounds(x, y):
            self.deactivate()

    def click_release(self):
        x, y = self.event_position()
        if not self.in_bounds(x, y):
            self.deactivate()

    def drag_move(self, d_x, d_y):
        self.point.x += d_x
        self.point.y += d_y
        self.save()

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