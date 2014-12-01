from datetime import datetime, timedelta
from vcs import vtk_ui

class FillEditor(object):
    def __init__(self, interactor, fillarea, index, configurator):
        self.index = index
        self.fill = fillarea
        self.interactor = interactor
        self.actions = []
        self.actions.append(self.interactor.AddObserver("LeftButtonPressEvent", self.clicked))
        self.actions.append(self.interactor.AddObserver("RightButtonPressEvent", self.rightclick))

        self.clicked_on = None
        self.handles = []
        self.configurator = configurator
        self.rebuild()

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


    def rightclick(self, obj, event):

        point = self.interactor.GetEventPosition()
        w, h = self.interactor.GetRenderWindow().GetSize()
        x, y = point
        x = x / float(w)
        y = y / float(h)
        point = (x, y)

        # Check each vertex to see if the rightclick was on it
        for ind, x1 in enumerate(self.fill.x[self.index]):
            if x1 - .01 < x and x1 + .01 > x:
                y1 = self.fill.y[self.index][ind]
                if y1 - .01 < y and y1 + .01 > y:
                    del self.fill.x[self.index][ind]
                    del self.fill.y[self.index][ind]
                    self.rebuild()
                    self.save()
                    break


    def clicked(self, obj, event):
        point = self.interactor.GetEventPosition()
        w, h = self.interactor.GetRenderWindow().GetSize()
        x, y = point
        x = x / float(w)
        y = y / float(h)
        point = (x, y)

        # Check if there was a click in or around a line.
        line_click = self.in_side( x, y)

        # Add a new point on double clicks
        if line_click is not None:

            if self.clicked_on is not None and (datetime.now() - self.clicked_on) < timedelta(0, .5):
                self.fill.x[self.index].insert(line_click, x)
                self.fill.y[self.index].insert(line_click, y)
                self.rebuild()
                self.save()
                self.clicked_on = None
            else:
                self.clicked_on = datetime.now()

            return

        if inside_fillarea(self.fill, *point) == self.index:
            if self.clicked_on is not None:
                if (datetime.now() - self.clicked_on) < timedelta(0, .5):
                    self.configurator.deactivate(self)
        else:
            self.configurator.deactivate(self)

        self.clicked_on = datetime.now()

    def adjust(self, handle):
        ind = self.handles.index(handle)
        self.fill.x[self.index][ind], self.fill.y[self.index][ind] = handle.x, handle.y
        self.save()

    def save(self):
        self.configurator.save()
        for h in self.handles:
            h.show()

    def detach(self):
        for h in self.handles:
            h.detach()

        for action in self.actions:
            self.interactor.RemoveObserver(action)


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
        intersected = 0
        for side in sides:
            x1, y1 = side[0]
            x2, y2 = side[1]

            if (x1 > x or x2 > x) and min(y1, y2) < y and max(y1, y2) > y:
                intersected += 1

        if intersected % 2 == 1:
            return ind

    return None