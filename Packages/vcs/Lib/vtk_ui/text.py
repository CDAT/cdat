from vtk import vtkTextActor, vtkTextWidget, vtkTextRenderer

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
        # Guess that it's a font file
        text_props.SetFontFile(font)
        # If GetFontFamily returns VTK_FONT_FILE, then it worked
        if text_props.GetFontFamily() != VTK_FONT_FILE:
            # Ugly default!
            text_props.SetFontFamilyToCourier()


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

from widget import Widget

class Label(Widget):

    def __init__(self, interactor, string, action=None, fgcolor=(1,1,1), size=24, font="Arial", left=0, top=0, textproperty=None):
        widget = vtkTextWidget()
        self.actor = text_actor(string, fgcolor, size, font)

        if textproperty is not None:
            self.actor.SetTextProperty(textproperty)

        widget.SetTextActor(self.actor)

        super(Label, self).__init__(interactor, widget)

        self.widget.ResizableOff()

        self.action = action
        self.left, self.top = left, top
        self.top_offset = 0

        # Assigned by Widget.__init__
        self.repr.MovingOff()
        self.repr.PickableOff()
        self.repr.SetShowBorderToOff()

        self.actor.SetTextScaleModeToNone()
        self.subscribe("WidgetActivateEvent", self.click)

    def set_text(self, string):

        below, above = baseline_offsets(self.actor.GetInput(), string, self.actor.GetTextProperty())

        self.top_offset += above
        self.repr.SetText(string)
        self.place()
        self.widget.Render()

    def set_font_size(self, size):
        prop = self.actor.GetTextProperty()
        prop.SetFontSize(size)

    def set_font_color(self, color):
        prop = self.actor.GetTextProperty()
        prop.SetColor(*color)

    def get_dimensions(self):
        bbox = [0,0]
        self.actor.GetSize(self.repr.GetRenderer(), bbox)
        return bbox[0], bbox[1]

    def show(self):
        self.place()
        self.widget.On()

    def hide(self):
        self.widget.Off()

    def place(self):
        w, h = self.interactor.GetRenderWindow().GetSize()
        _, widget_h = self.get_dimensions()
        self.repr.SetPosition(self.left / float(w), (h - self.top - widget_h - self.top_offset ) / float(h))

    def click(self, obj, event):
        # Pass this to self.action
        if self.action is not None:
            self.action(self.interactor.GetEventPosition())