from vcs import vtk_ui
from vcs.vtk_ui import behaviors

class LineEditor(behaviors.ClickableMixin, behaviors.DraggableMixin):
    styles = ["solid", "dash", "dot", "dash-dot", "long-dash"]
    def __init__(self, interactor, line, index, configurator):
        self.index = index
        self.line = line
        self.interactor = interactor

        self.handles = []

        self.configurator = configurator
        self.rebuild()

        self.toolbar = vtk_ui.toolbar.Toolbar(self.interactor, "Line %s" % line.name, open_label="Configure")
        self.toolbar.show()
        self.toolbar.add_slider_button(line.color[index], 0, 255, "Color", end=self.change_color)

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

        super(LineEditor, self).__init__()
        # Register mixins' events
        self.register()

    def change_color(self, value):
        self.line.color[self.index] = int(value)
        self.save()

    def change_type(self, index):
        self.line.type[self.index] = index

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

    def click_release(self):
        x, y = self.event_position()
        if not self.in_bounds(x, y):
            self.configurator.deactivate(self)

    def double_release(self):
        x, y = self.event_position()

        if self.in_bounds(x, y):
            print ""
        else:
            print "Missed the line :("


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
        offset = width / float(screen_height)
        for p_ind, point in enumerate(points):
            if p_ind + 1 == len(points):
                break

            x1, y1 = point

            x2, y2 = points[p_ind + 1]

            if max(x1, x2) > x and min(x1, x2) < x:

                m = (y2 - y1) / float(x2 - x1)

                b = y2 - m * x2

                line_y = m * x + b
                print x, y, line_y

                if offset > abs(line_y - y):
                    print p_ind, ind
                    return ind
    return None