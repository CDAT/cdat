from vcs.vtk_ui import behaviors, Handle
import priority

class PointEditor(behaviors.ClickableMixin, behaviors.DraggableMixin, priority.PriorityEditor):
    def __init__(self, interactor, point, configurator):
        self.point = point
        self.interactor = interactor
        self.handle = Handle(self.interactor, (point.x, point.y), dragged=self.drag_handle, released=self.adjust, color=(0,0,0), normalize=True)
        self.handle.show()
        self.configurator = configurator
        super(PointEditor, self).__init__()
        self.register()

    def get_object(self):
        return self.point

    def adjust(self, handle):
        self.point.x = self.handle.x
        self.point.y = self.handle.y
        try:
            w, h = self.interactor.GetRenderWindow().GetSize()
            self.actor.SetPosition(w * self.handle.x, h * self.handle.y)
        except AttributeError:
            self.configurator.changed = True
        self.save()

    def drag_handle(self, handle, x, y):
        self.point.x = x
        self.point.y = y
        try:
            w, h = self.interactor.GetRenderWindow().GetSize()
            self.actor.SetPosition(w * x, h * y)
        except AttributeError:
            self.configurator.changed = True

    def drag_stop(self):
        self.handle.place()
        self.save()

    def double_release(self):
        x, y = self.event_position()
        if self.in_bounds(x, y):
            self.deactivate()

    def is_object(self, point):
        return point == self.point

    def handle_click(self, point):
        x, y = point
        return self.in_bounds(x, y) or self.toolbar.in_toolbar(x, y)

    def drag_start(self):
        return
        """
        try:
            print self.actor
        except AttributeError:
            pass
        """

    def drag_move(self, d_x, d_y):
        self.point.x += d_x
        self.point.y += d_y
        self.handle.x = self.point.x
        self.handle.y = self.point.y
        self.handle.place()
        try:
            w, h = self.interactor.GetRenderWindow().GetSize()
            x, y = self.actor.GetPosition()
            self.actor.SetPosition(x + w * d_x, y + h * d_y)
            self.actor.GetMapper().Update()
        except AttributeError:
            self.configurator.changed = True

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