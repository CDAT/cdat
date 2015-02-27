from vcs import vtk_ui
from vcs.colorpicker import ColorPicker
from vcs.vtk_ui import behaviors
from vcs.VCS_validation_functions import checkMarker
import vtk
import vcs.vcs2vtk
import priority

class MarkerEditor(behaviors.ClickableMixin, behaviors.DraggableMixin, priority.PriorityEditor):
    """
    Editor for marker objects

    Ctrl + click to drop a new marker, toolbar to configure, priority, draggable + handles on each marker.
    """
    def __init__(self, interactor, marker, index, display, configurator):
        self.interactor = interactor
        self.marker = marker
        self.index = index
        self.configurator = configurator

        actors = display.backend["vtk_backend_marker_actors"][index]

        self.glyph, self.glyph_source, self.polydata, self.actor, self.geo = actors
        
        self.display = display

        self.handles = []

        for ind, x in enumerate(marker.x[index]):
            y = marker.y[index][ind]
            h = vtk_ui.Handle(self.interactor, (x, y), dragged=self.adjust, color=(0,0,0), normalize=True)
            h.show()
            self.handles.append(h)

        self.toolbar = vtk_ui.toolbar.Toolbar(self.interactor, "Marker Options")
        self.toolbar.show()

        self.toolbar.add_button(["Change Color"], action=self.change_color)
        self.toolbar.add_slider_button(marker.size[index], 1, 300, "Marker Size", update=self.set_size)

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
        prop = vtk.vtkTextProperty()
        prop.SetBackgroundColor(.87, .79, .55)
        prop.SetBackgroundOpacity(1)
        prop.SetColor(0, 0, 0)
        self.tooltip = vtk_ui.Label(self.interactor, "Ctrl + Click to place new markers.", textproperty=prop)
        self.tooltip.left = 0
        self.tooltip.top = self.interactor.GetRenderWindow().GetSize()[1] - self.tooltip.get_dimensions()[1]
        self.tooltip.show()
        super(MarkerEditor, self).__init__()
        self.register()

    def get_object(self):
        return self.marker

    def handle_click(self, point):
        x, y = point
        # Alt drops a new instance
        return self.in_bounds(x, y) or self.toolbar.in_toolbar(x, y) or self.current_modifiers()["control"]

    def is_object(self, marker):
        return self.marker == marker

    def place(self):
        for h in self.handles:
            h.place()
        self.toolbar.place()

    def render(self):
        from vcs.vtk_ui.manager import get_manager
        m = get_manager(self.interactor)
        m.queue_render()

    def update_shape(self):
        # Update the glyph for the marker to reflect the new shape
        self.glyph_source, self.polydata = vcs.vcs2vtk.prepGlyph(self.glyph, self.marker, self.index)
        self.display.backend["vtk_backend_marker_actors"][self.index] = (self.glyph, self.glyph_source, self.polydata, self.actor, self.geo)
        # Have to rescale the glyph now... work that out later with charles
        self.render()

    def change_shape(self, index):
        if index != 0:
            self.marker.type[self.index] = marker_shapes()[index - 1]
            self.wmo_button.set_state(0)
            self.update_shape()
        else:
            self.change_wmo(1)

    def change_wmo(self, index):
        if index != 0:
            self.marker.type[self.index] = wmo_shapes()[index - 1]
            self.shape_button.set_state(0)
            self.update_shape()
        else:
            self.change_shape(1)

    def set_size(self, size):
        self.marker.size[self.index] = size
        self.update_shape()

    def change_color(self, state):
        if self.picker:
            self.picker.make_current()
        else:
            self.picker = ColorPicker(500, 500, self.marker.colormap, self.marker.color[self.index], parent_interactor=self.interactor,  on_save=self.set_color, on_cancel=self.cancel_color)

    def set_color(self, colormap, color):
        self.marker.colormap = colormap
        self.marker.color[self.index] = color
        del self.picker
        self.picker = None
        vcs.vcs2vtk.setMarkerColor(self.actor.GetProperty(), self.marker, self.marker.color[self.index])
        self.render()

    def cancel_color(self):
        del self.picker
        self.picker = None

    def click_release(self):
        x, y = self.event_position()

        if self.current_modifiers()["control"]:
            h = vtk_ui.Handle(self.interactor, (x, y), dragged=self.adjust, color=(0,0,0), normalize=True)
            h.show()
            self.handles.append(h)
            self.marker.x[self.index].append(x)
            self.marker.y[self.index].append(y)
            self.sync_positions()

    def adjust(self, handle, dx, dy):
        ind = self.handles.index(handle)
        self.marker.x[self.index][ind] += dx
        self.marker.y[self.index][ind] += dy
        self.sync_positions()

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

            self.sync_positions()

    def detach(self):
        self.unregister()

        if self.picker:
            self.picker.close()
            self.picker = None
        self.toolbar.detach()

        for h in self.handles:
            h.detach()
        self.tooltip.detach()

    def delete(self):
        del self.marker.x[self.index]
        del self.marker.y[self.index]
        del self.marker.type[self.index]
        del self.marker.color[self.index]
        self.actor.SetVisibility(0)
        self.configurator.deactivate(self)

    def update_priority(self):
        maxLayers = self.interactor.GetRenderWindow().GetNumberOfLayers()
        new_layer = self.marker.priority * 10000 + 1 + self.configurator.displays.index(self.display)
        if new_layer + 1 > maxLayers:
            self.interactor.GetRenderWindow().SetNumberOfLayers(new_layer + 1)

        self.actor.SetLayerNumber(new_layer)

        self.render()

    def sync_positions(self):
        # Sync all points
        points = self.glyph.GetInput().GetPoints()
        for i, (x, y) in enumerate(zip(self.marker.x[self.index], self.marker.y[self.index])):
            if i == points.GetNumberOfPoints():
                points.InsertNextPoint(x, y, 0)
            else:
                points.SetPoint(i, x, y, 0)
        self.glyph.GetInput().Modified()
        self.render()

__shape_cache = {}

def marker_shapes():
    # Returns all shapes that are supported (skips star for now), indexed numerically
    shapes = []
    for i in xrange(1, 20):
        if i in __shape_cache:
            shapes.append(__shape_cache[i])
        else:
            try:
                val = checkMarker(None, "type", i)
                shapes.append(val)
                __shape_cache[i] = val
            except ValueError:
                pass
    return shapes

def wmo_shapes():
    wmo = []
    for i in xrange(100, 203):
        if i in __shape_cache:
            wmo.append(__shape_cache[i])
        else:
            try:
                val = checkMarker(None, "type", i)
                wmo.append(val)
                __shape_cache[i] = val
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
