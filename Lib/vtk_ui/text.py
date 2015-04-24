from vtk import vtkTextActor, vtkTextRenderer, vtkTextProperty, vtkPropPicker
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
    return (0, 0, 0) if lum >= .5 else (1, 1, 1)


def contrasting_color(red, green, blue):
    hue, saturation, value = rgb_to_hsv(red, green, blue)

    #saturation /= 2.
    r, g, b = red, green, blue
    phi = .61803398875

    iterations = 0
    max_iters = 5

    hsv = {"hue": hue, "value": value, "saturation": saturation}
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
    q = v * (1 - s * f)
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
        h = 0
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
    props.SetVerticalJustificationToTop()

    if string.find("\n") != -1:
        lines = string.split("\n")
        width = size * max([len(s) for s in lines])
        actor.SetPosition(width / 2.0, 0)

    return actor


def text_dimensions(text, text_prop, at_angle=0):
    ren = vtkTextRenderer()
    bounds = [0, 0, 0, 0]
    p = vtkTextProperty()
    p.ShallowCopy(text_prop)
    p.SetOrientation(at_angle)
    ren.GetBoundingBox(p, text, bounds)
    return bounds[1] - bounds[0], bounds[3] - bounds[2]


from widget import Widget, WidgetReprShim
from behaviors import DraggableMixin, ClickableMixin


class Label(Widget, DraggableMixin, ClickableMixin):

    def __init__(self, interactor, string, movable=False, on_move=None, on_drag=None, on_click=None, fgcolor=(1, 1, 1), size=24, font="Arial", left=0, top=0, textproperty=None):

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
        self.move_action = on_move
        self.dragged = on_drag

        self.actor.SetTextScaleModeToNone()
        self.actor.SetUseBorderAlign(False)
        self.actor.VisibilityOff()

        self.left = left
        self.top = top

        self.register()

    def text():
        doc = "The text property."

        def fget(self):
            return self.get_text()

        def fset(self, value):
            self.set_text(value)

        return locals()
    text = property(**text())

    def x():
        doc = "The x coordinate of the text actor."

        def fget(self):
            return self.actor.GetPosition()[0]

        def fset(self, value):
            self.actor.SetPosition(value, self.y)
        return locals()
    x = property(**x())

    def y():
        doc = "The y coordinate of the text actor."

        def fget(self):
            return self.actor.GetPosition()[1]

        def fset(self, value):
            self.actor.SetPosition(self.x, value)
        return locals()
    y = property(**y())

    def left():
        doc = "The left property."

        def fget(self):
            halign = self.actor.GetTextProperty().GetJustificationAsString()
            if halign == "Left":
                return self.x

            w, h = text_dimensions(self.text, self.actor.GetTextProperty())
            if halign == "Centered":
                return self.x - math.floor(w / 2.)

            if halign == "Right":
                return self.x - w

        def fset(self, l):

            halign = self.actor.GetTextProperty().GetJustificationAsString()
            if halign == "Left":
                self.x = l

            w, h = text_dimensions(self.text, self.actor.GetTextProperty())
            if halign == "Centered":
                self.x = l + math.floor(w / 2.)

            if halign == "Right":
                self.x = l + w
        return locals()
    left = property(**left())

    def top():
        """
        Provides support for the old-style of coordinates; left + top, vs x + y
        """
        doc = "Top coordinate"

        def fget(self):
            """
            Returns distance in pixels from top of window to top of actor
            """
            w, h = text_dimensions(self.text, self.actor.GetTextProperty())
            valign = self.actor.GetTextProperty().GetVerticalJustificationAsString()
            y = self.y
            # Adjust from alignment point to top of the actor
            if valign == "Top":
                pass
            if valign == "Centered":
                y += math.floor(h / 2.) + 1
            if valign == "Bottom":
                y += h
            # Transform from y position to distance from top of screen to top of actor
            w, h = self.interactor.GetRenderWindow().GetSize()
            return h - y

        def fset(self, t):
            """
            Sets actor y using distance in pixels from top of window to top of actor
            """
            # Get the text's size to adjust for alignment
            w, h = text_dimensions(self.text, self.actor.GetTextProperty())

            valign = self.actor.GetTextProperty().GetVerticalJustificationAsString()
            # Adjust position based on alignment
            if valign == "Top":
                y = t
            # Since it's not top-aligned, alignment point will be lower (and we're in units from top)
            elif valign == "Centered":
                y = t + math.floor(h / 2.) + 1
            elif valign == "Bottom":
                y = t + h
            # Convert the y from pixels from top to pixels from bottom
            w, h = self.interactor.GetRenderWindow().GetSize()
            self.y = h - y

        return locals()
    top = property(**top())

    def __repr__(self):
        return "<Label Widget: %s>" % self.get_text()

    def get_text(self):
        return self.actor.GetInput()

    def set_text(self, string):
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
        bbox = [0, 0]
        self.actor.GetSize(self.repr.GetRenderer(), bbox)
        return bbox[0], bbox[1]

    def show(self):
        if self.showing() is False:
            self.place()
            self.actor.VisibilityOn()
            self.actor.Modified()
            self.render()

    def hide(self):
        if self.showing():
            self.actor.VisibilityOff()
            self.actor.Modified()
            self.render()

    def place(self):
        """
        No-op, now that left and top auto-update the actor,
        but useful for subclasses to move accessories
        """
        pass

    def render(self):
        self.manager.queue_render()

    def in_bounds(self, x, y):
        if x < 1 and y < 1:
            w, h = self.interactor.GetRenderWindow().GetSize()
            x, y = x * w, y * h
        picker = vtkPropPicker()
        ren = self.manager.actor_renderer
        if picker.PickProp(x, y, ren) and picker.GetViewProp() == self.actor:
            return True
        else:
            return False

    def drag_stop(self):
        if self.movable and self.move_action:
            self.move_action()

    def detach(self):
        self.unsubscribe(*self.subscriptions.keys())
        self.manager.remove_widget(self)
        self.repr.GetRenderer().RemoveActor(self.actor)

    def click_release(self):
        if self.action:
            pos = self.event_position()
            if self.in_bounds(*pos):
                self.action(pos)

    def drag_move(self, dx, dy):
        if self.movable:
            w, h = self.interactor.GetRenderWindow().GetSize()
            dx, dy = dx * w, dy * h
            self.x = dx + self.x
            self.y = dy + self.y
            self.place()
            self.dragged(self, dx/w, dy/h)
            self.manager.queue_render()
