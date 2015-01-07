import vcs
import datetime
from editors import box, fillarea, line, legend

class Configurator(object):
    def __init__(self, canvas):
        self.canvas = canvas
        self.backend = canvas.backend
        self.interactor = None
        self.displays = []
        self.clicked = None
        self.clicked_info = None
        self.target = None

    def update(self, displays):
        if self.backend.renWin and self.interactor is None:
            self.interactor = self.backend.renWin.GetInteractor()
            self.interactor.AddObserver("LeftButtonPressEvent", self.click)

        self.displays = [vcs.elements["display"][display] for display in displays]

    def click(self, object, event):
        if self.target is not None:
            # Target should register for own events; don't want to step on toes
            return

        now = datetime.datetime.now()

        point = self.interactor.GetEventPosition()

        for display in self.displays:
            obj = self.in_display_plot(point, display)
            if obj is not None:
                break

        if obj:
            if self.clicked and now - self.clicked[0] < datetime.timedelta(0, .5) and self.clicked[1] == obj:
                self.activate(obj, display)
                self.clicked = None
                self.clicked_info = None
            else:
                self.clicked = (now, obj)

    def deactivate(self, obj):
        if self.target == obj:
            self.target.detach()
            self.target = None
        self.save()

    def activate(self, obj, display):
        if display.g_type == "fillarea":
            editor = fillarea.FillEditor(self.interactor, obj, self.clicked_info, self)
            self.target = editor
        elif display.g_type == "line":
            editor = line.LineEditor(self.interactor, obj, self.clicked_info, self)
            self.target = editor
        else:
            if is_box(obj):
                if obj.member == "legend":
                    editor = legend.LegendEditor(self.interactor, t(display.template), self)
                    self.target = editor
                else:
                    editor = box.BoxEditor(self.interactor, obj, self)
                    self.target = editor


    def in_display_plot(self, point, dp):
        #Normalize the point
        x, y = point
        w, h = self.interactor.GetRenderWindow().GetSize()
        if x > 1 or y > 1:
            point = (x / float(w), y / float(h))
            x, y = point

        if dp.g_type == "fillarea":
            fill = vcs.getfillarea(dp.g_name)
            info = fillarea.inside_fillarea(fill, *point)
            if info is not None:
                self.clicked_info = info
                return fill
        elif dp.g_type == "line":
            line = vcs.getline(dp.g_name)
            # Uses screen_height to determine how much buffer space there is around the line
            info = line.inside_line(line, *point, screen_height=h)
            if info is not None:
                self.clicked_info = info
                return line
        else:
            fudge = 5 / float(w)
            return in_template(point, t(dp.template), fudge=fudge)

    def save(self):
        self.canvas.update()

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

def is_box(member):
    x1 = safe_get(member, "x1")
    y1 = safe_get(member, "y1")
    x2 = safe_get(member, "x2")
    y2 = safe_get(member, "y2")

    if None in (x1, y1, x2, y2):
        return False
    else:
        return True

def is_point_in_box(point, box):
    """
    Box should be provided as ((xmin, ymin), (xmax, ymax))
    """
    x, y = point
    (x1, y1), (x2, y2) = box
    return x1 <= x and x2 >= x and y1 <= y and y2 >= y

def safe_get(obj, attr, sentinel=None):
    """
    Returns sentinel value if attr isn't in obj, otherwise attr's value
    """
    try:
        return getattr(obj, attr)
    except AttributeError:
        return sentinel