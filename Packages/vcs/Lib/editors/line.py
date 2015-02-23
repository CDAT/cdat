from vcs import vtk_ui
from vcs.vtk_ui import behaviors
from vcs.colorpicker import ColorPicker
import priority

class LineEditor(behaviors.ClickableMixin, behaviors.DraggableMixin, priority.PriorityEditor):
    styles = ["solid", "dash", "dot", "dash-dot", "long-dash"]
    def __init__(self, interactor, line, index, configurator):
        self.index = index
        self.line = line
        self.interactor = interactor

        self.handles = []

        self.configurator = configurator
        self.rebuild()

        self.toolbar = vtk_ui.toolbar.Toolbar(self.interactor, "Line Options")
        self.toolbar.show()

        self.toolbar.add_button(["Change Color"], action=self.change_color)

        # Used to store the color picker when it's active
        self.picker = None

        b = self.toolbar.add_button([style[0].upper() + style[1:] for style in LineEditor.styles], action=self.change_type)
        style = line.type[index]
        if style == "solid":
            b.set_state(0)
        elif style == "dash":
            b.set_state(1)
        elif style == "dot":
            b.set_state(2)
        elif style == "dash-dot":
            b.set_state(3)
        elif style == "long-dash":
            b.set_state(4)
        self.slider_button = self.toolbar.add_slider_button(self.line.width[self.index], 1, 300, "Width", update=self.set_width)

        super(LineEditor, self).__init__()
        # Register mixins' events
        self.register()

    def get_object(self):
        return self.line

    def handle_click(self, point):
        x, y = point
        return self.in_bounds(x, y) or self.toolbar.in_toolbar(x, y)

    def is_object(self, line):
        return self.line == line

    def set_width(self, width):
        self.line.width[self.index] = int(width)
        self.slider_button.set_value(int(width))
        self.configurator.changed = True
        self.save()

    def place(self):
        for h in self.handles:
            h.place()
        self.toolbar.place()

    def change_type(self, index):
        self.line.type[self.index] = index
        self.configurator.changed = True
        self.save()

    def change_color(self, state):
        if self.picker:
            self.picker.make_current()
        else:
            self.picker = ColorPicker(500, 500, self.line.colormap, self.line.color[self.index], on_save=self.set_color, on_cancel=self.cancel_color)

    def set_color(self, colormap, color):
        self.line.colormap = colormap
        self.line.color[self.index] = color
        self.configurator.changed = True
        self.save()

    def right_release(self):
        x, y = self.event_position()

        # Check each vertex to see if the rightclick was on it
        for ind, x1 in enumerate(self.line.x[self.index]):
            if x1 - .01 < x and x1 + .01 > x:
                y1 = self.line.y[self.index][ind]
                if y1 - .01 < y and y1 + .01 > y:
                    del self.line.x[self.index][ind]
                    del self.line.y[self.index][ind]
                    self.configurator.changed = True
                    self.rebuild()
                    self.save()
                    break

    def cancel_color(self):
        self.picker = None

    def rebuild(self):
        for h in self.handles:
            h.detach()

        self.handles = []

        points = zip(self.line.x[self.index], self.line.y[self.index])

        for point in points:
            h = vtk_ui.Handle(self.interactor, point, released=self.adjust, color=(0,0,0), normalize=True)
            h.show()
            self.handles.append(h)

    def adjust(self, handle):
        ind = self.handles.index(handle)
        self.line.x[self.index][ind], self.line.y[self.index][ind] = handle.x, handle.y
        self.configurator.changed = True
        self.save()

    def drag_move(self, delta_x, delta_y):
        for h in self.handles:
            h.x += delta_x
            h.y += delta_y
            h.place()

    def drag_stop(self):
        for ind, h in enumerate(self.handles):
            if self.line.x[self.index][ind] != h.x and self.line.y[self.index][ind] != h.y:
                self.configurator.changed = True
            self.line.x[self.index][ind] = h.x
            self.line.y[self.index][ind] = h.y

        self.save()

    def save(self):
        self.configurator.save()

    def in_bounds(self, x, y):
        _, h = self.interactor.GetRenderWindow().GetSize()
        return inside_line(self.line, x, y, h, index=self.index) == self.index

    def detach(self):
        for h in self.handles:
            h.detach()

        self.toolbar.detach()

        self.unregister()

    def double_release(self):
        x, y = self.event_position()

        # Hack to make draggable work a little better with double click
        self.drag_origin = None
        if self.in_bounds(x, y):
            points = zip(self.line.x[self.index], self.line.y[self.index])
            for ind, point in enumerate(points):
                x1, _ = point
                x2, _ = points[ind + 1]

                # We know that we're inside the line, so we'll exit this loop
                # before we hit the end
                if min(x1, x2) < x and max(x1, x2) > x:
                    break
            # insert after ind
            self.line.x[self.index].insert(ind + 1, x)
            self.line.y[self.index].insert(ind + 1, y)
            self.rebuild()
            self.configurator.changed = True
            self.save()
        else:
            self.configurator.deactivate(self)


def inside_line(line, x, y, screen_height, index=None):
    if index is None:
        indices = range(len(line.x))
    else:
        indices = [index]

    for ind in indices:
        if index is not None:
            if ind != index:
                continue

        points = zip(line.x[ind], line.y[ind])
        width = line.width[ind]
        offset = width / float(screen_height) * 2
        for p_ind, point in enumerate(points):
            if p_ind + 1 == len(points):
                break

            x1, y1 = point

            x2, y2 = points[p_ind + 1]

            if max(x1, x2) > x and min(x1, x2) < x:

                m = (y2 - y1) / float(x2 - x1)

                b = y2 - m * x2

                line_y = m * x + b

                if offset > abs(line_y - y):
                    return ind
    return None