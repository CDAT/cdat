from vcs import vtk_ui
from vcs.color_picker import ColorPicker
from vcs.vtk_ui import behaviors

class MarkerEditor(behaviors.ClickableMixin):
    def __init__(self, interactor, marker, index, configurator):
        self.interactor = interactor
        self.marker = marker
        self.index = index
        self.configurator = configurator

        self.handles = []

        for ind, x in enumerate(marker.x[index]):
            y = marker.y[index][ind]
            h = vtk_ui.Handle(self.interactor, (x, y), released=self.adjust, color=(0,0,0), normalize=True)
            h.show()
            self.handles.append(h)

        self.toolbar = vtk_ui.toolbar.Toolbar(self.interactor, "Marker Options")
        self.toolbar.show()

        self.toolbar.add_button(["Change Color"], action=self.change_color)
        self.toolbar.add_slider_button(marker.size[index], 1, 300, "Marker Size", end=self.set_size)
        # Used to store the color picker when it's active
        self.picker = None

        super(MarkerEditor, self).__init__()
        self.register()

    def click_release(self):
        x, y = self.event_position()

        if self.in_bounds(x, y):
            pass
        else:
            self.deactivate()

    def set_size(self, size):
        self.marker.size[self.index] = size
        self.save()

    def change_color(self, state):
        if self.picker:
            self.picker.make_current()
        else:
            self.picker = ColorPicker(500, 500, self.marker.colormap, self.marker.color[self.index], on_save=self.set_color, on_cancel=self.cancel_color)

    def set_color(self, colormap, color):
        self.marker.colormap = colormap
        self.marker.color[self.index] = color
        del self.picker
        self.picker = None
        self.save()

    def cancel_color(self):
        del self.picker
        self.picker = None

    def double_release(self):
        x, y = self.event_position()

        if self.in_bounds(x, y):
            self.deactivate()
        else:
            h = vtk_ui.Handle(self.interactor, (x, y), released=self.adjust, color=(0,0,0), normalize=True)
            h.show()
            self.handles.append(h)
            self.marker.x[self.index].append(x)
            self.marker.y[self.index].append(y)
            self.save()

    def adjust(self, handle):
        ind = self.handles.index(handle)
        self.marker.x[self.index][ind] = handle.x
        self.marker.y[self.index][ind] = handle.y
        self.save()

    def in_bounds(self, x, y):
        w, h = self.interactor.GetRenderWindow().GetSize()
        return inside_marker(self.marker, x, y, w, h, index=self.index) is not None

    def right_release(self):
        x, y = self.event_position()
        if self.in_bounds(x, y):
            points = zip(self.marker.x[self.index], self.marker.y[self.index])

            size = self.marker.size[self.index]
            screen_width, screen_height = self.interactor.GetRenderWindow().GetSize()

            w, h = float(size) / screen_width, float(size) / screen_height

            for ind, point in enumerate(points):
                m_x, m_y = point
                if x > m_x - w and x < m_x + w and y > m_y - h and y < m_y + h:
                    break

            del self.marker.x[self.index][ind]
            del self.marker.y[self.index][ind]
            self.handles[ind].detach()
            del self.handles[ind]
            self.save()

    def detach(self):
        self.unregister()

        if self.picker:
            self.picker.close()
            self.picker = None
        self.toolbar.detach()

        for h in self.handles:
            h.detach()

    def deactivate(self):
        self.configurator.deactivate(self)

    def save(self):
        self.configurator.save()

def inside_marker(marker, x, y, screen_width, screen_height, index=None):
    if index is None:
        index = range(len(marker.x))
    else:
        index = [index]

    for ind in index:
        marker_x, marker_y = marker.x[ind], marker.y[ind]
        coords = zip(marker_x, marker_y)
        size = marker.size[ind]
        w, h = float(size) / screen_width, float(size) / screen_height

        for m_x, m_y in coords:
            if x > m_x - w and x < m_x + w and y > m_y - h and y < m_y + h:
                return ind

    return None