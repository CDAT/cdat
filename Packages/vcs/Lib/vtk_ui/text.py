from vtk import vtkTextActor, vtkTextWidget, vtkTextRenderer
import datetime
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

def white_or_black(red, green, blue):
    """ Returns white or black to choose most contrasting color for provided color """
    # Convert to YIQ colorspace
    yiq = (red * 255 * 299 + green * 255 * 587 + blue * 255 * 114) / 1000
    return (0,0,0) if yiq >= 128 else (1, 1, 1)

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

from widget import Widget
from behaviors import DraggableMixin

class Label(Widget, DraggableMixin):

    def __init__(self, interactor, string, movable=False, on_move=None, on_drag=None, on_click=None, on_release=None, fgcolor=(1,1,1), size=24, font="Arial", left=0, top=0, textproperty=None):
        widget = vtkTextWidget()

        if textproperty is not None:
            self.actor = vtkTextActor()
            self.actor.SetInput(string)
            textproperty.SetBackgroundColor(white_or_black(*textproperty.GetColor()))
            textproperty.SetBackgroundOpacity(0)
            self.actor.SetTextProperty(textproperty)
        else:
            self.actor = text_actor(string, fgcolor, size, font)

        widget.SetTextActor(self.actor)

        super(Label, self).__init__(interactor, widget)

        #self.widget.ResizableOff()
        self.movable = movable
        self.action = on_click
        self.release_action = on_release
        self.move_action = on_move
        self.drag_action = on_drag

        self.left, self.top = left, top
        self.top_offset = 0

        # Assigned by Widget.__init__
        self.repr.MovingOff()
        self.repr.PickableOff()
        self.repr.SetShowBorderToOff()

        self.actor.SetTextScaleModeToNone()

        # Map events to draggable actions, because standard events aren't propagated
        self.add_event_handler("StartInteractionEvent", self.drag_clicked)
        self.add_event_handler("InteractionEvent", self.drag_moved)
        self.add_event_handler("EndInteractionEvent", self.drag_released)
        self.add_event_handler("StartInteractionEvent", self.click)
        self.add_event_handler("EndInteractionEvent", self.release)

        self.register()

    def get_text(self):
        return self.repr.GetText()

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
        prop.SetBackgroundColor(white_or_black(*color))

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

    def release(self, obj, event):
        if self.release_action is not None:
            self.release_action(self.interactor.GetEventPosition())

    def render(self):
        self.widget.Render()

    def click(self, obj, event):
        self.drag_origin = self.event_position()
        self.drag_started = datetime.datetime.now()
        # Pass this to self.action
        if self.action is not None:
            self.action(self.interactor.GetEventPosition())

    def in_bounds(self, x, y):
        w, h = self.get_dimensions()
        if x < self.left + w and x > self.left and y < self.top and y > self.top - h:
            return True
        return False

    def drag_stop(self):
        if self.movable and self.move_action:
            self.move_action()

    def drag_move(self, dx, dy):
        if self.movable:
            w, h = self.interactor.GetRenderWindow().GetSize()
            dx, dy = dx * w, dy * h
            self.left += dx
            self.top -= dy
            self.drag_action(dx, dy)
            self.place()