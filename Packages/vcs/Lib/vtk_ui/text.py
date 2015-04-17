from vtk import vtkTextActor, vtkTextWidget, vtkTextRenderer, vtkTextProperty, vtkPropPicker
import datetime
import math
def __set_font(font, text_props):
    """
    Font selection logic for text properties
    """
    from vtk import VTK_FONT_FILE

    if font == "Arial":
        text_props.SetFontFamilyToArial()
    elif font == "Courier":
        text_props.SetFontFamilyToCourier()
    elif font == "Times":
        text_props.SetFontFamilyToTimes()
    else:
        # Assume it's a font file
        text_props.SetFontFamily(VTK_FONT_FILE)
        text_props.SetFontFile(font)

def lum_normalize(component):
    if component <= .03928:
        return component / 12.92
    else:
        return ((component + .055) / 1.055) ** 2.4

def luminance(color):
    r, g, b = [lum_normalize(c) for c in color]
    lum = .2126 * r + .7152 * g + .0722 * b
    return lum

def contrast_ratio(fg, bg):
    lum_fg = luminance(fg)
    lum_bg = luminance(bg)
    l1 = max(lum_fg, lum_bg)
    l2 = min(lum_fg, lum_bg)

    return (l1 + .05) / (l2 + .05)
def white_or_black(red, green, blue):
    """ Returns white or black to choose most contrasting color for provided color """
    # Convert to YIQ colorspace
    lum = luminance((red, green, blue))
    return (0,0,0) if lum >= .5 else (1, 1, 1)

def contrasting_color(red, green, blue):
    hue, saturation, value = rgb_to_hsv(red, green, blue)

    #saturation /= 2.
    r, g, b = red, green, blue
    phi = .61803398875

    iterations = 0
    max_iters = 5

    hsv = {"hue": hue, "value": value, "saturation":saturation}
    var_keys = hsv.keys()
    key = "value"

    while contrast_ratio((red, green, blue), (r, g, b)) < 4.5:
        iterations += 1
        if iterations == max_iters:
            key = var_keys[(var_keys.index(key) + 1) % 3]
            iterations = 0
        var_value = hsv[key] - phi
        if var_value < 0:
            var_value += 1
        hsv[key] = var_value
        r, g, b = hsv_to_rgb(hsv["hue"], hsv["saturation"], hsv["value"])

    return hsv_to_rgb(hsv["hue"], hsv["saturation"], hsv["value"])

def hsv_to_rgb(h, s, v):
    if s == 0:
        # grayscale
        return v, v, v

    h = h / 60.
    i = math.floor(h)
    f = h - i
    p = v * (1 - s)
    q = v * ( 1 - s * f)
    t = v * (1 - s * (1 - f))

    if i == 0:
        r, g, b = v, t, p
    elif i == 1:
        r, g, b = q, v, p
    elif i == 2:
        r, g, b = p, v, t
    elif i == 3:
        r, g, b = p, q, v
    elif i == 4:
        r, g, b = t, p, v
    else:
        r, g, b = v, p, q
    return r, g, b

def rgb_to_hsv(r, g, b):

    minimum = min(r, g, b)
    maximum = max(r, g, b)
    v = maximum

    delta = float(maximum - minimum)

    if r == g == b:
        s = 0
        h = -1
        return h, s, v
    else:
        s = delta / maximum

    if r == maximum:
        h = (g - b) / delta
    elif g == maximum:
        h = 2 + (b - r) / delta
    else:
        h = 4 + (r - g) / delta

    h = h * 60
    if h < 0:
        h += 360
    return h, s, v


def text_actor(string, fgcolor, size, font):
    """
    Build a text actor with some sane defaults
    """

    actor = vtkTextActor()
    actor.SetInput(string)
    props = actor.GetTextProperty()

    __set_font(font, props)
    props.SetFontSize(size)
    props.SetColor(*fgcolor)
    props.SetBackgroundColor(white_or_black(*fgcolor))
    props.SetBackgroundOpacity(0)

    # Sane defaults.
    props.SetJustificationToCentered()
    props.SetVerticalJustificationToCentered()

    if string.find("\n") != -1:
        lines = string.split("\n")
        width = size * max([len(s) for s in lines])
        actor.SetPosition(width / 2.0, 0)

    return actor

def text_dimensions(text, text_prop):
    ren = vtkTextRenderer()
    bounds = [0,0,0,0]
    ren.GetBoundingBox(text_prop, text, bounds)
    return bounds[1] - bounds[0], bounds[3] - bounds[2]

def baseline_offsets(origin, new_string, text_prop):
    ren = vtkTextRenderer()

    bounds_origin = [0,0,0,0]
    ren.GetBoundingBox(text_prop, origin, bounds_origin)

    bounds_new = [0,0,0,0]
    ren.GetBoundingBox(text_prop, new_string, bounds_new)

    below_offset = bounds_origin[2] - bounds_new[2]

    above_offset = bounds_origin[3] - bounds_new[3]

    return below_offset, above_offset

from widget import Widget, WidgetReprShim
from behaviors import DraggableMixin

class Label(Widget, DraggableMixin):

    def __init__(self, interactor, string, movable=False, on_move=None, on_drag=None, on_click=None, on_release=None, fgcolor=(1,1,1), size=24, font="Arial", left=0, top=0, textproperty=None):

        if textproperty is not None:
            self.actor = vtkTextActor()
            self.actor.SetInput(string)
            self.actor.SetTextProperty(textproperty)
            if textproperty.GetBackgroundColor() == textproperty.GetColor():
                textproperty.SetBackgroundColor(*white_or_black(*textproperty.GetColor()))
        else:
            self.actor = text_actor(string, fgcolor, size, font)

        widget = WidgetReprShim(interactor, self.actor)

        super(Label, self).__init__(interactor, widget)

        self.movable = movable
        self.action = on_click
        self.release_action = on_release
        self.move_action = on_move
        self.dragged = on_drag

        self.left, self.top = left, top
        self.top_offset = 0

        self.actor.SetTextScaleModeToNone()
        self.actor.SetUseBorderAlign(False)
        self.actor.VisibilityOff()
        self.register()

    def __repr__(self):
        return "<Label Widget: %s>" % self.get_text()

    def get_text(self):
        return self.actor.GetInput()

    def set_text(self, string):

        below, above = baseline_offsets(self.actor.GetInput(), string, self.actor.GetTextProperty())

        self.top_offset += above
        self.actor.SetInput(string)
        self.place()
        self.render()

    def set_font_size(self, size):
        prop = self.actor.GetTextProperty()
        prop.SetFontSize(size)

    def set_font_color(self, color):
        prop = self.actor.GetTextProperty()
        prop.SetColor(*color)
        prop.SetBackgroundColor(white_or_black(*color))

    def get_dimensions(self):
        bbox = [0,0]
        self.actor.GetSize(self.repr.GetRenderer(), bbox)
        return bbox[0], bbox[1]

    def show(self):
        if self.showing() == False:
            self.place()
            self.actor.VisibilityOn()
            self.actor.Modified()
            self.render()

    def hide(self):
        if self.showing() == True:
            self.actor.VisibilityOff()
            self.actor.Modified()
            self.render()

    def place(self):
        w, h = self.interactor.GetRenderWindow().GetSize()
        widget_w, widget_h = self.get_dimensions()

        x, y = self.left, h - self.top - self.top_offset

        p = self.actor.GetTextProperty()

        just = p.GetJustificationAsString()
        if just == "Centered":
            x += widget_w / 2.
        elif just == "Right":
            x += widget_w

        vjust = p.GetVerticalJustificationAsString()
        if vjust == "Centered":
            y -= widget_h / 2.
        elif vjust == "Bottom":
            y -= widget_h

        x, y = int(x), int(y)
        self.actor.SetPosition(x, y)

    def release(self, obj, event):
        if self.release_action is not None:
            self.release_action(self.interactor.GetEventPosition())

    def render(self):
        self.manager.queue_render()

    def click(self, obj, event):
        self.drag_origin = self.event_position()
        self.drag_started = datetime.datetime.now()
        # Pass this to self.action
        if self.action is not None:
            self.action(self.interactor.GetEventPosition())

    def in_bounds(self, x, y):
        picker = vtkPropPicker()
        ren = self.manager.actor_renderer
        if picker.PickProp(x, y, ren) and picker.GetViewProp() == self.actor:
            return True
        else:
            return False

    def drag_start(self):
        self.log("Start drag")

    def drag_stop(self):
        if self.movable and self.move_action:
            self.move_action()

    def detach(self):
        self.unsubscribe(*self.subscriptions.keys())
        self.manager.remove_widget(self)
        self.repr.GetRenderer().RemoveActor(self.actor)
        self.interactor = None

    def drag_move(self, dx, dy):
        if self.movable:
            w, h = self.interactor.GetRenderWindow().GetSize()
            dx, dy = dx * w, dy * h
            self.left += dx
            self.top -= dy
            self.dragged(self, dx/w, dy/h)
            self.place()