import vcs
import datetime

class Configurator(object):
    def __init__(self, backend):
        self.backend = backend
        self.interactor = None
        self.displays = []
        self.clicked = None

    def update(self, displays):
        if self.backend.renWin and self.interactor is None:
            self.interactor = self.backend.renWin.GetInteractor()
            self.interactor.AddObserver("LeftButtonPressEvent", self.click)
        self.displays = [vcs.elements["display"][display] for display in displays]

    def click(self, object, event):
        now = datetime.datetime.now()

        point = self.interactor.GetEventPosition()

        for display in self.displays:
            obj = self.in_display_plot(point, display)
            if obj is not None:
                break

        if obj:
            if self.clicked and now - self.clicked[0] < datetime.timedelta(0, .5) and self.clicked[1] == obj:
                self.clicked = None
                self.activate(obj)
            else:
                self.clicked = (now, obj)

    def activate(self, obj):
        print "Activating %s" % obj.name

    def in_display_plot(self, point, dp):
        #Normalize the point
        x, y = point
        w, h = self.interactor.GetRenderWindow().GetSize()
        if x > 1 or y > 1:
            point = (x / float(w), y / float(h))
            x, y = point

        if dp.g_type == "fillarea":
            fill = vcs.getfillarea(dp.g_name)
            if fillarea_intersection(fill, *point) is not None:
                return fill
        else:
            fudge = 5 / float(w)
            return in_template(point, t(dp.template), fudge=fudge)


def t(name):
    return vcs.gettemplate(name)

def in_template(point, template, fudge=None):
    x, y = point

    attrs = [
        "file",
        "function",
        "logicalmask",
        "transformation",
        "source",
        "dataname",
        "title",
        "units",
        "crdate",
        "crtime",
        "comment1",
        "comment2",
        "comment3",
        "comment4",
        "xname",
        "yname",
        "zname",
        "tname",
        "xunits",
        "yunits",
        "zunits",
        "tunits",
        "xvalue",
        "yvalue",
        "zvalue",
        "tvalue",
        "mean",
        "min",
        "max",
        "xtic1",
        "xtic2",
        "xmintic1",
        "xmintic2",
        "ytic1",
        "ytic2",
        "ymintic1",
        "ymintic2",
        "xlabel1",
        "xlabel2",
        "ylabel1",
        "ylabel2",
        "box1",
        "box2",
        "box3",
        "box4",
        "line1",
        "line2",
        "line3",
        "line4",
        "legend",
        "data",
    ]

    intersecting = None

    for attr in attrs:
        attribute = getattr(template, attr)
        if attribute.priority == 0 or (intersecting is not None and intersecting.priority > attribute.priority):
            # 0 is turned off
            continue
        t_x = safe_get(attribute, "x")
        t_y = safe_get(attribute, "y")

        if t_x is not None or t_y is not None:
            if t_x is not None and t_y is not None:
                # It's probably a text blob
                if is_point_in_box((x, y), ((t_x - fudge, t_y - fudge), (t_x + fudge, t_y + fudge))):
                    intersecting = attribute
            else:
                # It's probably the labels for an axis
                if t_x is not None and t_x < x + fudge and t_x > x - fudge:
                    intersecting = attribute
                elif t_y is not None and t_y < y + fudge and t_y > y - fudge:
                    intersecting = attribute
        else:
            x1 = safe_get(attribute, "x1")
            y1 = safe_get(attribute, "y1")
            x2 = safe_get(attribute, "x2")
            y2 = safe_get(attribute, "y2")

            if None in (x1, y1, x2, y2):
                continue
            else:
                if is_point_in_box((x, y), ((min(x1, x2), min(y1, y2)), (max(x1, x2), max(y1, y2)))):
                    intersecting = attribute

    return intersecting

def is_point_in_box(point, box):
    """
    Box should be provided as ((xmin, ymin), (xmax, ymax))
    """
    x, y = point
    (x1, y1), (x2, y2) = box
    return x1 <= x and x2 >= x and y1 <= y and y2 >= y

def fillarea_intersection(fillarea, x, y):
    """
    Determines if a point is inside a fillarea; returns the index of the
    fill shape selected or None.
    """

    for ind, xcoords in enumerate(fillarea.x):

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

        if not is_point_in_box((x,y), ((xmin, ymin), (xmax, ymax))):
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




def safe_get(obj, attr, sentinel=None):
    """
    Returns sentinel value if attr isn't in obj, otherwise attr's value
    """
    try:
        return getattr(obj, attr)
    except AttributeError:
        return sentinel