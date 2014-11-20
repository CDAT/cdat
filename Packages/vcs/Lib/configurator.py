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
        #Normalize the point
        x, y = point
        w, h = self.interactor.GetRenderWindow().GetSize()
        point = (x / float(w), y / float(h))

        fudge = 5 / float(w)

        for display in self.displays:
            attr = in_template(point, t(display.template), fudge=fudge)
            if attr is not None:
                print "In %s.%s" % (display.template, attr.member)
                break

        if attr:
            if self.clicked and now - self.clicked[0] < datetime.timedelta(0, .5) and self.clicked[1] == attr:
                self.clicked = None
                self.activate(attr)
            else:
                self.clicked = (now, attr)

    def activate(self, attr):
        print "Activating %s" % attr.member



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
        if attribute.priority == 0 or intersecting is not None and intersecting.priority > attribute.priority:
            # 0 is turned off
            continue
        t_x = safe_get(attribute, "x")
        t_y = safe_get(attribute, "y")

        if t_x is not None or t_y is not None:
            if t_x is not None and t_y is not None:
                # It's probably a text blob
                if t_x < x + fudge and t_x > x - fudge and t_y < y + fudge and t_y > y - fudge:
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
                if min(x1, x2) < x and max(x1, x2) > x and min(y1, y2) < y and max(y1, y2) > y:
                    intersecting = attribute

    return intersecting


def safe_get(obj, attr, sentinel=None):
    """
    Returns sentinel value if attr isn't in obj, otherwise attr's value
    """
    try:
        return getattr(obj, attr)
    except AttributeError:
        return sentinel