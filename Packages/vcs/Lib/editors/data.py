from vcs import vtk_ui
from vcs.vtk_ui import behaviors

class DataEditor(behaviors.DraggableMixin):
    def __init__(self, interactor, data, configurator):
        self.data = data
        self.interactor = interactor
        self.configurator = configurator

        self.top_left = vtk_ui.Handle(self.interactor, (data.x1, data.y1), dragged=self.drag_handle, released=self.adjust, color=(0,0,0), normalize=True)
        self.top_right = vtk_ui.Handle(self.interactor, (data.x2, data.y1), dragged=self.drag_handle, released=self.adjust, color=(0,0,0), normalize=True)
        self.bottom_left = vtk_ui.Handle(self.interactor, (data.x1, data.y2), dragged=self.drag_handle, released=self.adjust, color=(0,0,0), normalize=True)
        self.bottom_right = vtk_ui.Handle(self.interactor, (data.x2, data.y2), dragged=self.drag_handle, released=self.adjust, color=(0,0,0), normalize=True)

        self.top_left.show()
        self.top_right.show()
        self.bottom_left.show()
        self.bottom_right.show()

        super(DataEditor, self).__init__()

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
        self.data.x1, self.data.y1 = self.top_left.x, self.top_left.y
        self.data.x2, self.data.y2 = self.bottom_right.x, self.bottom_right.y
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

    def drag_stop(self):
        self.save()

    def save(self):
        self.configurator.save()