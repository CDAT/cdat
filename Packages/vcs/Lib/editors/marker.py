from vcs import vtk_ui
from vcs.colorpicker import ColorPicker
from vcs.vtk_ui import behaviors
from vcs.VCS_validation_functions import checkMarker

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

        self.type_bar = self.toolbar.add_toolbar("Marker Type", open_label="Change")

        shapes = marker_shapes()

        shapes.insert(0, "Select Shape")
        self.shape_button = self.type_bar.add_button(shapes, action=self.change_shape)

        wmos = wmo_shapes()
        wmos.insert(0, "Select WMO Marker")

        self.wmo_button = self.type_bar.add_button(wmos, action=self.change_wmo)

        if self.marker.type[self.index] in shapes:
            self.shape_button.set_state(shapes.index(self.marker.type[self.index]))
        else:
            self.wmo_button.set_state(wmos.index(self.marker.type[self.index]))

        # Used to store the color picker when it's active
        self.picker = None

        super(MarkerEditor, self).__init__()
        self.register()

    def click_release(self):
        x, y = self.event_position()

        if self.in_bounds(x, y):
            pass
        else:
            self.configurator.deactivate(self)

    def change_shape(self, index):
        if index != 0:
            self.marker.type[self.index] = marker_shapes()[index - 1]
            self.wmo_button.set_state(0)
            self.save()
        else:
            self.change_wmo(1)

    def change_wmo(self, index):
        if index != 0:
            self.marker.type[self.index] = wmo_shapes()[index - 1]
            self.shape_button.set_state(0)
            self.save()
        else:
            self.change_shape(1)

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
            self.configurator.deactivate(self)
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

            if len(self.marker.x[self.index]) == 0:
                del self.marker.x[self.index]
                del self.marker.y[self.index]
                del self.marker.type[self.index]
                del self.marker.color[self.index]

            if len(self.marker.x) == 0:
                self.delete()
                return

            self.save()

    def detach(self):
        self.unregister()

        if self.picker:
            self.picker.close()
            self.picker = None
        self.toolbar.detach()

        for h in self.handles:
            h.detach()

    def delete(self):
        self.configurator.delete(self.marker, self.index)
        self.configurator.deactivate(self)

    def save(self):
        self.configurator.save()

def marker_shapes():
    # Returns all shapes that are supported (skips star for now), indexed numerically
    shapes = []
    for i in xrange(1, 20):
        try:
            val = checkMarker(None, "type", i)
            # Star is busted right now, add it back in once it's fixed
            shapes.append(val)
        except ValueError:
            pass
    return shapes

def wmo_shapes():
    wmo = []
    for i in xrange(100, 203):
        try:
            val = checkMarker(None, "type", i)
            wmo.append(val)
        except ValueError:
            pass
    return wmo



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
