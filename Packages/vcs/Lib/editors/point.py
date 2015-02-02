from vcs.vtk_ui import behaviors, Handle

class PointEditor(behaviors.ClickableMixin, behaviors.DraggableMixin):
    def __init__(self, interactor, point, configurator):
        self.point = point
        self.interactor = interactor
        self.handle = Handle(self.interactor, (point.x, point.y), dragged=self.drag_handle, released=self.adjust, color=(0,0,0), normalize=True)
        self.handle.show()
        self.configurator = configurator
        super(PointEditor, self).__init__()
        self.register()

    def adjust(self, handle):
        self.point.x = self.handle.x
        self.point.y = self.handle.y
        self.save()

    def drag_handle(self, handle, x, y):
        self.point.x = x
        self.point.y = y

    def drag_stop(self):
        self.handle.place()
        self.save()

    def double_release(self):
        x, y = self.event_position()
        if self.in_bounds(x, y):
            self.deactivate()

    def is_object(self, point):
        return point == self.point

    def click_release(self):
        x, y = self.event_position()
        if not self.in_bounds(x, y):
            self.deactivate()

    def drag_move(self, d_x, d_y):
        self.point.x += d_x
        self.point.y += d_y
        self.handle.x = self.point.x
        self.handle.y = self.point.y
        self.handle.place()

    def in_bounds(self, x, y):
        return in_point(self.point, x, y)

    def save(self):
        self.configurator.save()

    def detach(self):
        self.handle.hide()
        self.handle.detach()
        self.unregister()

    def deactivate(self):
        self.configurator.deactivate(self)

def in_point(point, x, y):
    if x < point.x + .001 and x > point.x - .001 and y > point.y - .001 and y < point.y + .001:
        return True
    else:
        return False