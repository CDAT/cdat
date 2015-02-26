from vcs import vtk_ui
from vcs.vtk_ui import behaviors
from vcs.colorpicker import ColorPicker
import priority

class FillEditor(behaviors.ClickableMixin, behaviors.DraggableMixin, priority.PriorityEditor):
    """
    Editor for vcs `fillarea` object

    Sticks a handle at each vertex for manipulating the x/y coordinates of that vertex,
    draggable as a whole, double click on an edge to add a new vertex, toolbar to configure options.
    """
    def __init__(self, interactor, fillarea, index, configurator):
        self.index = index
        self.fill = fillarea
        self.interactor = interactor

        self.handles = []

        self.configurator = configurator
        self.rebuild()

        self.toolbar = vtk_ui.toolbar.Toolbar(self.interactor, "Fill Area Options")
        self.toolbar.show()

        self.toolbar.add_button(["Change Color"], action=self.change_color)

        # Used to store the color picker when it's active
        self.picker = None

        b = self.toolbar.add_button(["Solid", "Hatch", "Pattern"], action=self.change_style)
        style = fillarea.style[index]
        if style == "solid":
            b.set_state(0)
        elif style == "hatch":
            b.set_state(1)
        elif style == "pattern":
            b.set_state(2)

        super(FillEditor, self).__init__()
        # Register mixins' events
        self.register()

    def get_object(self):
        return self.fill

    def is_object(self, fill):
        return fill == self.fill

    def place(self):

        for h in self.handles:
            h.place()

        self.toolbar.place()

    def change_style(self, state):
        if state == 0:
            self.fill.style[self.index] = "solid"
        elif state == 1:
            self.fill.style[self.index] = "hatch"
        elif state == 2:
            self.fill.style[self.index] = "pattern"
        self.configurator.changed = True
        self.save()

    def change_color(self, state):
        if self.picker:
            self.picker.make_current()
        else:
            self.picker = ColorPicker(500, 500, self.fill.colormap, self.fill.color[self.index], on_save=self.set_color, on_cancel=self.cancel_color)

    def set_color(self, colormap, color):
        self.fill.colormap = colormap
        self.fill.color[self.index] = color
        self.configurator.changed = True
        self.picker = None
        self.save()

    def cancel_color(self):
        self.picker = None

    def rebuild(self):
        for h in self.handles:
            h.detach()

        self.handles = []

        points = zip(self.fill.x[self.index], self.fill.y[self.index])

        for point in points:
            h = vtk_ui.Handle(self.interactor, point, released=self.adjust, color=(0,0,0), normalize=True)
            h.show()
            self.handles.append(h)

    def in_side(self, x, y):
        for ind, x1 in enumerate(self.fill.x[self.index]):
            x2 = self.fill.x[self.index][ind - 1]
            y1 = self.fill.y[self.index][ind]
            y2 = self.fill.y[self.index][ind - 1]

            left = min(x1, x2)
            bottom = min(y1, y2)
            right = max(x1, x2)
            top = max(y1, y2)

            if left < x and right > x and top > y and bottom < y:
                return ind
        return None

    def right_release(self):

        x, y = self.event_position()

        # Check each vertex to see if the rightclick was on it
        for ind, x1 in enumerate(self.fill.x[self.index]):
            if x1 - .01 < x and x1 + .01 > x:
                y1 = self.fill.y[self.index][ind]
                if y1 - .01 < y and y1 + .01 > y:
                    del self.fill.x[self.index][ind]
                    del self.fill.y[self.index][ind]
                    self.rebuild()
                    if len(self.fill.x[self.index]) < 2:
                        del self.fill.x[self.index]
                        del self.fill.y[self.index]
                        del self.fill.color[self.index]
                        self.configurator.changed = True
                        if len(self.fill.x) == 0:
                            self.delete()

                        self.configurator.deactivate(self)
                        return


                    self.save()
                    break
    def delete(self):
        self.configurator.delete(self.fill, self.index)

    def drag_move(self, delta_x, delta_y):
        for h in self.handles:
            h.x += delta_x
            h.y += delta_y
            h.place()

    def drag_stop(self):
        for ind, h in enumerate(self.handles):
            if self.fill.x[self.index][ind] != h.x or self.fill.y[self.index][ind] != h.y:
                self.configurator.changed = True
            self.fill.x[self.index][ind] = h.x
            self.fill.y[self.index][ind] = h.y


        self.save()

    def click_release(self):

        x, y = self.event_position()

        if self.drag_origin != (x, y) and self.drag_position is not None:
            # Update vertex positions
            for ind, h in enumerate(self.handles):
                self.fill.x[self.index][ind], self.fill.y[self.index][ind] = h.x, h.y

            self.configurator.changed = True
            self.save()
        self.drag_position = None
        self.drag_origin = None

    def double_release(self):
        point = self.event_position()
        x, y = point

        # Check if there was a click in or around a line.
        line_click = self.in_side(x, y)
        if line_click is not None:
            self.fill.x[self.index].insert(line_click, x)
            self.fill.y[self.index].insert(line_click, y)
            self.configurator.changed = True
            self.rebuild()
            self.save()
        else:
            self.configurator.deactivate(self)
        self.drag_position = None
        self.drag_origin = None

    def adjust(self, handle):
        ind = self.handles.index(handle)
        self.fill.x[self.index][ind], self.fill.y[self.index][ind] = handle.x, handle.y
        self.configurator.changed = True
        self.save()

    def save(self):
        self.configurator.save()
        for h in self.handles:
            h.show()

    def in_bounds(self, x, y):
        return inside_fillarea(self.fill, x, y, self.index) is not None

    def handle_click(self, point):
        x, y = point
        return self.in_bounds(x, y) or self.toolbar.in_toolbar(x, y)

    def detach(self):
        for h in self.handles:
            h.detach()

        self.unregister()

        self.toolbar.detach()


def inside_fillarea(fillarea, x, y, index=None):
    """
    Determines if a point is inside a fillarea; returns the index of the
    fill shape selected or None.
    """
    for ind, xcoords in enumerate(fillarea.x):
        if index is not None:
            if ind != index:
                continue
        # Points are stored as a list of lists, once the fill has been rendered
        points = zip(xcoords, fillarea.y[ind])

        if len(points) == 1:
            if points[0][0] == x and points[0][1] == y:
                # If you magically click the exact point that this is, sure, you can edit it.
                return ind
            continue
        elif len(points) == 0:
            continue

        # Test if point is inside bounding box
        xmin, xmax = min(xcoords), max(xcoords)
        ymin, ymax = min(fillarea.y[ind]), max(fillarea.y[ind])

        if not  xmin <= x and xmax >= x and ymin <= y and ymax >= y:
            continue

        sides = [ (point, points[point_ind - 1]) for point_ind, point in enumerate(points) ]

        # We're going to cast a ray straight to the right from x,y
        # Every side that we intersect will get added here.
        # If intersected is even, we're outside the shape.
        # If intersected is odd, we're inside the shape.
        intersected = []
        for side_ind, side in enumerate(sides):
            x1, y1 = side[0]
            x2, y2 = side[1]

            if (x > x1 and x > x2) or (y < min(y1, y2) or y > max(y1, y2)):
                continue

            m = (y1 - y2) / float(x1 - x2)
            b = y1 - m * x1

            if (y - b) / float(m) > x:
                intersected.append(side_ind)

        if len(intersected) % 2 == 1:
            return ind

    return None