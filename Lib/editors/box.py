from vcs import vtk_ui
from vcs.vtk_ui import behaviors

class BoxEditor(behaviors.ClickableMixin, behaviors.DraggableMixin):
    def __init__(self, interactor, box, configurator):
        self.box = box
        self.interactor = interactor
        self.configurator = configurator

        self.top_left = vtk_ui.Handle(self.interactor, (box.x1, box.y1), dragged=self.drag_handle, released=self.adjust, color=(0,0,0), normalize=True)
        self.top_right = vtk_ui.Handle(self.interactor, (box.x2, box.y1), dragged=self.drag_handle, released=self.adjust, color=(0,0,0), normalize=True)
        self.bottom_left = vtk_ui.Handle(self.interactor, (box.x1, box.y2), dragged=self.drag_handle, released=self.adjust, color=(0,0,0), normalize=True)
        self.bottom_right = vtk_ui.Handle(self.interactor, (box.x2, box.y2), dragged=self.drag_handle, released=self.adjust, color=(0,0,0), normalize=True)
        self.drag_buffer = 3
        self.top_left.show()
        self.top_right.show()
        self.bottom_left.show()
        self.bottom_right.show()

        super(BoxEditor, self).__init__()
        self.register()

    def in_bounds(self, x, y):
        x1, y1, x2, y2 = min(self.box.x1, self.box.x2), min(self.box.y1, self.box.y2), max(self.box.x1, self.box.x2), max(self.box.y1, self.box.y2)
        return x > x1 and x < x2 and y > y1 and y < y2

    def drag_handle(self, handle, x, y):
        if handle in (self.top_left, self.top_right):
            self.top_left.y, self.top_right.y = handle.y, handle.y
        else:
            self.bottom_right.y, self.bottom_left.y = handle.y, handle.y

        if handle in (self.top_left, self.bottom_left):
            self.top_left.x, self.bottom_left.x = handle.x, handle.x
        else:
            self.bottom_right.x, self.top_right.x = handle.x, handle.x

        handles = [self.top_left, self.top_right, self.bottom_left, self.bottom_right]
        handles.remove(handle)

        for h in handles:
            h.place()

    def adjust(self, handle):
        self.box.x1, self.box.y1 = self.top_left.x, self.top_left.y
        self.box.x2, self.box.y2 = self.bottom_right.x, self.bottom_right.y
        self.save()

    def drag_move(self, d_x, d_y):

        self.top_left.x += d_x
        self.top_right.x += d_x
        self.bottom_left.x += d_x
        self.bottom_right.x += d_x

        self.top_left.y += d_y
        self.top_right.y += d_y
        self.bottom_left.y += d_y
        self.bottom_right.y += d_y

        self.top_left.place()
        self.top_right.place()
        self.bottom_left.place()
        self.bottom_right.place()
        self.box.x1 = self.top_left.x
        self.box.y1 = self.top_left.y
        self.box.x2 = self.bottom_right.x
        self.box.y2 = self.bottom_right.y
        self.save()

    def click_release(self):
        x, y = self.event_position()
        if self.in_bounds(x, y):
            pass
        else:
            self.configurator.deactivate(self)

    def save(self):
        self.configurator.save()

    def detach(self):
        self.top_left.detach()
        self.top_right.detach()
        self.bottom_left.detach()
        self.bottom_right.detach()

        self.unregister()