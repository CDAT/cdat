import vcs
import datetime
import editors
import vtk_ui.button
import os, sys

CREATING_FILL = "fill"
CREATING_LINE = "line"
CREATING_MARKER = "marker"
CREATING_TEXT = "text"

CLICKS_TO_CREATE = {
    CREATING_FILL: 3,
    CREATING_LINE: 2,
    CREATING_MARKER: 1,
    CREATING_TEXT: 1,
}

def array_strings(template, array):
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

    template = t(template)

    strings = {}
    for attr in attrs:
        try:
            attribute = getattr(template, attr)

            if is_label(attribute):
                strings[attr] = editors.label.get_label_text(attribute, array)
        except AttributeError:
            pass

    return strings

class Configurator(object):
    def __init__(self, canvas):
        self.canvas = canvas
        self.backend = canvas.backend
        self.interactor = None
        self.display_strings = {}
        self.displays = []
        self.clicked = None
        self.clicked_info = None
        self.target = None

        self.fill_button = None
        self.text_button = None
        self.line_button = None
        self.marker_button = None

        self.creating = False
        self.click_locations = None


    def update(self, displays):
        if self.backend.renWin and self.interactor is None:
            self.interactor = self.backend.renWin.GetInteractor()
            self.interactor.AddObserver("LeftButtonPressEvent", self.click)
            self.init_buttons()

        self.place()

        self.displays = [vcs.elements["display"][display] for display in self.canvas.display_names]

        # Add new arrays
        matched = set()
        for d in self.displays:
            for a in d.array:
                if a is not None:
                    if a.id not in self.display_strings:
                        self.display_strings[a.id] = array_strings(d.template, a)
                        matched.add(a.id)
                    elif a.id in self.display_strings:
                        matched.add(a.id)

        # Figure out which arrays to remove
        to_remove = set()
        for array in self.display_strings:
            if array not in matched:
                to_remove.add(array)

        # Remove the missing arrays
        for array in to_remove:
            del self.display_strings[array]

    def click(self, object, event):

        point = self.interactor.GetEventPosition()

        if self.creating:
            self.click_locations.append(point)
            if len(self.click_locations) == CLICKS_TO_CREATE[self.creating]:
                self.create()
            return


        clicked = None
        display_clicked = None
        for display in self.displays:
            obj = self.in_display_plot(point, display)
            if obj is not None and (clicked is None or obj.priority >= clicked.priority):
                clicked = obj
                display_clicked = display

        if clicked:
            if self.target is None or self.target.is_object(clicked) == False:
                self.activate(clicked, display_clicked)


    def deactivate(self, obj):
        if self.target == obj:
            self.target.detach()
            self.target = None
        self.save()

    def delete(self, obj, index):
        obj.priority = 0
        self.save()

    def place(self):
        self.fill_button.place()
        self.line_button.place()
        self.marker_button.place()
        self.text_button.place()
        if self.target:
            self.target.place()

    def activate(self, obj, display):
        if self.target is not None:
            self.deactivate(self.target)

        if display.g_type == "fillarea":
            editor = editors.fillarea.FillEditor(self.interactor, obj, self.clicked_info, self)
        elif display.g_type == "line":
            editor = editors.line.LineEditor(self.interactor, obj, self.clicked_info, self)
        elif display.g_type == "marker":
            editor = editors.marker.MarkerEditor(self.interactor, obj, self.clicked_info, self)
        elif display.g_type == "text":
            editor = editors.text.TextEditor(self.interactor, obj, self.clicked_info, self)
        else:
            if is_box(obj):
                if obj.member == "legend":
                    editor = editors.legend.LegendEditor(self.interactor, t(display.template), self)
                elif obj.member == "data":
                    editor = editors.data.DataEditor(self.interactor, vcs.getgraphicsmethod(display.g_type, display.g_name), t(display.template), self)
                else:
                    editor = editors.box.BoxEditor(self.interactor, obj, self)
            else:
                if is_label(obj):
                    editor = editors.label.LabelEditor(self.interactor, obj, display, self)
                else:
                    editor = editors.point.PointEditor(self.interactor, obj, self)
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
            info = editors.fillarea.inside_fillarea(fill, *point)
            if info is not None:
                self.clicked_info = info
                return fill
        elif dp.g_type == "line":
            l = vcs.getline(dp.g_name)
            # Uses screen_height to determine how much buffer space there is around the line
            info = editors.line.inside_line(l, *point, screen_height=h)
            if info is not None:
                self.clicked_info = info
                return l
        elif dp.g_type == "marker":
            m = vcs.getmarker(dp.g_name)
            info = editors.marker.inside_marker(m, point[0], point[1], w, h)
            if info is not None:
                self.clicked_info = info
                return m
        elif dp.g_type == "text":
            tc = vcs.gettextcombined(dp.g_name)
            info = editors.text.inside_text(tc, point[0], point[1], w, h)
            if info is not None:
                self.clicked_info = info
                return tc
        else:
            fudge = 5 / float(w)
            return in_template(point, t(dp.template), dp, (w, h), self.display_strings[dp.array[0].id], fudge=fudge)

    def save(self):
        self.canvas.update()

    def init_buttons(self):
        # An "off" and "on" state
        states = [vtk_ui.button.ButtonState(bgcolor=x) for x in ((.5, .5, .5), (.75, .75, .75))]

        self.fill_button = vtk_ui.button.Button(self.interactor, states=states, image=os.path.join(sys.prefix,"share","vcs","fill_icon.png"), top=10, left=10, halign=vtk_ui.button.RIGHT_ALIGN, action=self.fill_click)
        self.fill_button.show()

        self.text_button = vtk_ui.button.Button(self.interactor, states=states, image=os.path.join(sys.prefix, "share", "vcs", "text_icon.png"), top=10, left=63, halign=vtk_ui.button.RIGHT_ALIGN, action=self.text_click)
        self.text_button.show()

        self.line_button = vtk_ui.button.Button(self.interactor, states=states, image=os.path.join(sys.prefix, "share", "vcs", "line_icon.png"), top=10, left=116, halign=vtk_ui.button.RIGHT_ALIGN, action=self.line_click)
        self.line_button.show()

        self.marker_button = vtk_ui.button.Button(self.interactor, states=states, image=os.path.join(sys.prefix, "share", "vcs", "marker_icon.png"), top=10, left=169, halign=vtk_ui.button.RIGHT_ALIGN, action=self.marker_click)
        self.marker_button.show()


    def creator_enabled(self, button):
        if self.target is not None:
            self.deactivate(self.target)

        buttons = [self.fill_button, self.text_button, self.line_button, self.marker_button]
        buttons.remove(button)

        if self.creating:
            for button in buttons:
                button.set_state(0)

        self.click_locations = []

        if button == self.fill_button:
            self.creating = CREATING_FILL
        elif button == self.text_button:
            self.creating = CREATING_TEXT
        elif button == self.line_button:
            self.creating = CREATING_LINE
        elif button == self.marker_button:
            self.creating = CREATING_MARKER

    def creator_disabled(self, button):
        if self.target is not None:
            self.deactivate(self.target)

        self.click_locations = None
        self.creating = None

    def marker_click(self, index):
        if index == 1:
            self.creator_enabled(self.marker_button)
        else:
            self.creator_disabled(self.marker_button)

    def line_click(self, index):
        if index == 1:
            self.creator_enabled(self.line_button)
        else:
            self.creator_disabled(self.line_button)

    def text_click(self, index):
        if index == 1:
            self.creator_enabled(self.text_button)
        else:
            self.creator_disabled(self.text_button)

    def fill_click(self, index):
        if index == 1:
            self.creator_enabled(self.fill_button)
        else:
            self.creator_disabled(self.fill_button)

    def create(self):
        w, h = self.interactor.GetRenderWindow().GetSize()

        x = []
        y = []

        for point in self.click_locations:
            x.append(point[0] / float(w))
            y.append(point[1] / float(h))

        created = None

        if self.creating == CREATING_FILL:
            fill = self.canvas.createfillarea()
            fill.x = x
            fill.y = y
            created = fill
            self.fill_button.set_state(0)
        elif self.creating == CREATING_TEXT:
            t = self.canvas.createtextcombined()
            t.x = x
            t.y = y
            t.string = ["New Text"]
            created = t
            self.text_button.set_state(0)
        elif self.creating == CREATING_MARKER:
            m = self.canvas.createmarker()
            m.x = x
            m.y = y
            m.size = [10]
            created = m
            self.marker_button.set_state(0)
        elif self.creating == CREATING_LINE:
            l = self.canvas.createline()
            l.x = x
            l.y = y
            created = l
            self.line_button.set_state(0)

        dp = self.canvas.plot(created)

        # Activate an editor for the new object
        self.clicked_info = 0
        self.activate(created, dp)

        self.creating = False
        self.click_locations = None


def t(name):
    return vcs.gettemplate(name)

def is_label(obj):
    return type(obj) in (vcs.Pformat.Pf, vcs.Ptext.Pt)

def in_template(point, template, dp, window_size, strings, fudge=None):
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
                if is_label(attribute):
                    text = strings[attr]
                    if text == '':
                        continue
                    if editors.label.inside_label(attribute, text, x, y, *window_size):
                        intersecting = attribute
                elif is_point_in_box((x, y), ((t_x - fudge, t_y - fudge), (t_x + fudge, t_y + fudge))):
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