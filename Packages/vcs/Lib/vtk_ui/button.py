import vtk
from image_utils import *
from text import Label, text_dimensions
from slider import Slider
from widget import Widget
from datetime import datetime, timedelta

BUTTON_MARGIN = 3

def __kwargs_to_dict__(**kwargs):
    return kwargs

class ButtonState(object):
    def __init__(self, label = '', image = None, bgcolor = None, fgcolor = None, opacity = None):
        self.label = label
        self.image = image
        self.bgcolor = bgcolor
        self.fgcolor = fgcolor
        self.opacity = opacity

    def __getattribute__(self, attr):
        result = super(ButtonState, self).__getattribute__(attr)
        # Autoload the image when it's needed
        if attr == "image" and result is not None:
            self.image = load_image(result)
            return super(ButtonState, self).__getattribute__(attr)
        return result

LEFT_ALIGN = "LEFT"
RIGHT_ALIGN = "RIGHT"
CENTER_ALIGN = "CENTER"
TOP_ALIGN = "TOP"
BOTTOM_ALIGN = "BOTTOM"

class Button(Widget):
    def __init__(self, interactor, action=None, corner_radius=5, width=None, font="Arial",
                 height=None, left=0, top=0, image=None, label="", bgcolor=(.5, .5, .5), fgcolor=(1,1,1),
                 opacity=1, size=14, states = None, halign=LEFT_ALIGN, valign=CENTER_ALIGN, tooltip=None, tooltip_property=None):
        """
        @kwargs:
            action: A callback function that will receive the current state ID when the button is clicked.
            width: if width is None, use the size of the label to determine width
            height: if height is None, use the size of the label to determine height
            left: Distance from the left of the window to place the button
            top: Distance from the top of the window to place the button
            image: Icon to place on top of the background
            label: Default label to use for all states (if no states are provided, creates a state using defaults)
            bgcolor: Default background color of the button (if states do not provide a bgcolor, this one is used)
            fgcolor: Default font color of the label (if states do not provide an fgcolor, this one is used)
            opacity: Default opacity of button & label (if states do not provide an opacity, this one is used)
            size: Default font size of the label (if states do not provide a font size, this one is used)
            halign: If the button states have multiple widths (different labels/images), this will align them horizontally as specified
            valign: If the button states have multiple heights (labels with newlines, images), this will align them vertically as specified
        """

        self.width = width
        self.height = height
        self.left = left
        self.top = top
        self.radius = corner_radius
        self.action = action

        if halign not in (LEFT_ALIGN, RIGHT_ALIGN, CENTER_ALIGN):
            raise TypeError("halign must be one of LEFT_ALIGN, RIGHT_ALIGN, or CENTER_ALIGN")
        self.halign = halign

        if valign not in (TOP_ALIGN, BOTTOM_ALIGN, CENTER_ALIGN):
            raise TypeError("valign must be one of TOP_ALIGN, BOTTOM_ALIGN, or CENTER_ALIGN")
        self.valign = valign
        if image:
            self.image = load_image(image)
        else:
            self.image = None

        self.__placing__ = False

        text = states[0].label if states else label
        # Text widget will be placed over the button; clicks on it have to propogate down
        self.text_widget = Label(interactor, text, on_click = self.__advance__, size=size, font=font)

        self.label = label
        self.size = size
        self.opacity = opacity
        self.fgcolor = fgcolor
        self.bgcolor = bgcolor

        if states:
            self.states = states
        else:
            self.states = [ButtonState(label=label)]

        widget = vtk.vtkButtonWidget()
        widget.SetRepresentation(vtk.vtkTexturedButtonRepresentation2D())

        super(Button, self).__init__(interactor, widget)

        if tooltip:
            self.tooltip_label = Label(interactor, tooltip, textproperty=tooltip_property)
            self.hover_handler = self.interactor.AddObserver("MouseMoveEvent", self.hover)
            self.hover_timer = None
            self.timer_handler = self.interactor.AddObserver("TimerEvent", self.still_hovering)

        self.update()
        self.subscribe( 'StateChangedEvent', self.clicked)

    def hover(self, obj, event):
        if self.widget.GetEnabled() == 0:
            return

        x, y = self.interactor.GetEventPosition()

        if self.hover_timer is None and self.tooltip_label.showing() == False:
            if self.in_bounds(x, y):
                self.hover_timer = self.interactor.CreateOneShotTimer(300)

        if self.in_bounds(x, y) == False:
            if self.hover_timer is not None:
                self.interactor.DestroyTimer(self.hover_timer)
                self.hover_timer = None
            if self.tooltip_label.showing():
                self.tooltip_label.hide()


    def still_hovering(self, obj, event):
        if self.hover_timer:
            self.tooltip_label.place()
            self.tooltip_label.show()
            self.hover_timer = None

    def get_text(self):
        return self.text_widget.get_text()

    def add_state(self, label=None, image=None, bgcolor=None, fgcolor=None, opacity=None):
        self.states.append(ButtonState(label=label, image=image, bgcolor=bgcolor, fgcolor=fgcolor, opacity=opacity))

    def place(self):
        width, height = self.get_dimensions()
        x, y = self.get_position()
        bounds = (x, x + width, y - height, y, 0, 0)

        self.repr.SetPlaceFactor(1)
        self.repr.PlaceWidget(bounds)

        text_width, text_height = self.text_widget.get_dimensions()
        swidth, sheight = self.interactor.GetRenderWindow().GetSize()

        self.text_widget.left = x + (width - text_width) / 2.0
        self.text_widget.top = sheight - y + BUTTON_MARGIN

        self.text_widget.place()

        try:
            w, h = self.tooltip_label.get_dimensions()

            if x + 5 + w < swidth:
                self.tooltip_label.left = x + 5
            else:
                self.tooltip_label.left = swidth - w - 5

            self.tooltip_label.top = sheight - (y - height)
            self.tooltip_label.place()
        except AttributeError:
            pass


    def get_dimensions(self):
        image = self.repr.GetButtonTexture(self.repr.GetState())
        width, height, _ = image.GetDimensions()

        return width, height

    def update(self):
        self.repr.SetNumberOfStates(len(self.states))

        max_width = 0
        max_height = 0

        for index, state in enumerate(self.states):
            # Set up attributes with defaults if nothing is set
            label_text = state.label if state.label else self.label
            image = state.image if state.image else self.image

            if image:
                # Image supersedes label
                w, h, _ = image.GetDimensions()
                # Use a 3 px padding for now
                max_height = max(max_height, h)
                max_width = max(max_width, w)

            elif label_text:
                l_w, l_h = text_dimensions(label_text, self.text_widget.actor.GetTextProperty())

                max_height = max(max_height, l_h)
                max_width = max(max_width, l_w)

        # Pad the text
        max_width += 2 * BUTTON_MARGIN
        max_height += 2 * BUTTON_MARGIN

        for index, state in enumerate(self.states):

            image = state.image if state.image else self.image
            bgcolor = state.bgcolor if state.bgcolor else self.bgcolor
            # Opacity not yet supported by this code
            #opacity = state.opacity if state.opacity else self.opacity

            # Something weird happens when images of drastically different sizes are passed in;
            # not hunting down that fix right now.
            if image is not None:
                width, height, _ = image.GetDimensions()
            else:
                width = self.width if self.width else int(max_width)
                height = self.height if self.height else int(max_height)

            # Optimization can be done here; can use the same image for everything with same bgcolor + h/w
            bg_image = rounded_rect(width, height, self.radius, bgcolor)
            if image is not None:
                image = pad_image(image, max_width, max_height)
                bg_image = combine_images(bg_image, image)

            # Should deal with opacity here-ish
            self.repr.SetButtonTexture(index, bg_image)

    def get_position(self):
        default_texture = self.repr.GetButtonTexture(0)
        dwidth, dheight, _ = default_texture.GetDimensions()
        width, height = self.get_dimensions()

        window = self.interactor.GetRenderWindow()
        size = window.GetSize()

        if self.halign == LEFT_ALIGN:
            left = self.left
        elif self.halign == CENTER_ALIGN:
            left = (self.left - (width - dwidth) / 2)
        elif self.halign == RIGHT_ALIGN:
            left = size[0] - self.left - width

        if dheight == height or self.valign == TOP_ALIGN:
            top = self.top
        elif self.valign == CENTER_ALIGN:
            top = (self.top - (height - dheight) / 2)
        elif self.valign == BOTTOM_ALIGN:
            top = size[1] - (self.top - (height - dheight))

        return left, size[1] - top

    def get_state(self):
        return self.repr.GetState()

    def set_state(self, state):
        new_state = self.states[state]
        label = self.label if new_state.label is None else new_state.label
        self.text_widget.set_text(label)
        self.repr.SetState(state)
        self.place()

    def show(self):
        super(Button, self).show()
        self.text_widget.show()
        self.place()

    def detach(self):
        self.text_widget.detach()
        self.text_widget = None
        try:
            self.tooltip_label.detach()
            self.tooltip_label = None
            self.interactor.RemoveObserver(self.hover_handler)
        except:
            pass
        self.action = None
        super(Button, self).detach()

    def hide(self):
        super(Button, self).hide()
        try:
            self.tooltip_label.hide()
        except AttributeError:
            pass
        self.text_widget.hide()

    def in_bounds(self, x, y):
        w, h = self.get_dimensions()
        box_x, box_y = self.get_position()
        return x < box_x + w and x > box_x and y > box_y - h and y < box_y

    def __advance__(self, point):
        state = self.repr.GetState()
        self.set_state( (state + 1) % len(self.states) )
        #self.clicked(self.widget, "StateChangedEvent") Do we need to call this? I bet we don't.

    def clicked(self, obj, event):
        state = self.get_state()
        button_state = self.states[state]

        self.text_widget.set_text( button_state.label if button_state.label else self.label )
        self.text_widget.set_font_color( button_state.fgcolor if button_state.fgcolor else self.fgcolor )
        self.place()
        if self.action:
            self.action(state)

    def copy(self, interactor, button_type=None, button_args=None, button_kwargs=None, skip_args=None):
        # In the future, we'll want to do some optimization with states and images here.
        b = Button(interactor, action=self.action, corner_radius=self.radius, width=self.width,
             height=self.height, left=self.left, top=self.top, image=self.image, label=self.label, bgcolor=self.bgcolor, fgcolor=self.fgcolor,
             opacity=self.opacity, size=self.size, states = self.states, halign=self.halign, valign=self.valign)

        state = self.get_state()
        if state != 0:
            b.set_state(state)
            b.action(state)

        return b


class ToggleButton(Button):
    """
    Displays a button with 2 states, that will call different callbacks when clicked based on current state.
    """
    def __init__(self, interactor, label, on=None, off=None, corner_radius=5, width=None,
                 height=None, left=0, top=0, image=None, bgcolor=(.5, .5, .5), fgcolor=(1,1,1), font="Arial",
                 opacity=1, size=14, states=None, halign=LEFT_ALIGN, valign=CENTER_ALIGN, on_prefix="Enable", off_prefix="Disable"):

        super(ToggleButton, self).__init__(interactor, action=self.toggle, corner_radius=corner_radius, width=width,
                 height=height, left=left, top=top, image=image, bgcolor=bgcolor, fgcolor=fgcolor, font=font,
                 opacity=opacity, size=size, states=states if states else [ButtonState(label="%s %s" % (on_prefix, label)), ButtonState(label="%s %s" % (off_prefix, label))], halign=halign, valign=valign)

        self.on = on
        self.off = off
        self.label = label

    def get_text(self):
        if self.get_state() == 0:
            return self.label.get_text()[len(on_prefix):]
        else:
            return self.label.get_text()[len(off_prefix):]

    def toggle(self, state):
        if state == 1:
            self.on()
        else:
            self.off()

    def detach(self):
        super(ToggleButton, self).detach()
        self.on = None
        self.off = None

    def copy(self, interactor):
        b = ToggleButton(interactor, self.label, on=self.on, off=self.off, corner_radius=self.radius, width=self.width,
                 height=self.height, left=self.left, top=self.top, image=self.image, bgcolor=self.bgcolor, fgcolor=self.fgcolor,
                 opacity=self.opacity, size=self.size, states = self.states, halign=self.halign, valign=self.valign)

        state = self.repr.GetState()

        if state != 0:
            b.set_state(state)
            b.on()

        return b


class SliderButton(ToggleButton):
    """
    Displays a button that will show a slider when toggled on, and hide it when toggled off.
    Provides a callback to receive the value of the slider, and one for when the sliding has stopped.
    """
    def __init__(self, interactor, value, min_val, max_val, label, on_show_slider=None, update=None, end=None, corner_radius=5, width=None,
                 height=None, left=0, top=0, image=None, bgcolor=(.5, .5, .5), fgcolor=(1,1,1), font="Arial",
                 opacity=1, size=14, states=None, halign=LEFT_ALIGN, valign=CENTER_ALIGN, point1=(0,.1), point2=(1,.1)):

        self.slider = Slider(interactor, update=update, end=end, title= label, value=value, min_val=min_val, max_val=max_val, point1=point1, point2=point2)

        def _show_slider():
            if on_show_slider:
                on_show_slider()
            self.slider.show()

        super(SliderButton, self).__init__(interactor, label, on=_show_slider, off=self.slider.hide, corner_radius=corner_radius, width=width,
                 height=height, left=left, top=top, image=image, bgcolor=bgcolor, fgcolor=fgcolor, font=font,
                 opacity=opacity, size=size, states= states, on_prefix="Show", off_prefix="Hide", halign=halign, valign=valign)

    def get_value(self):
        return self.slider.repr.GetValue()

    def set_value(self, value):
        self.slider.repr.SetValue(value)

    def show(self):
        state = self.repr.GetState()
        if state == 1:
            self.slider.show()

        super(SliderButton, self).show()

    def hide(self):
        self.slider.hide()
        super(SliderButton, self).hide()

    def detach(self):
        super(SliderButton, self).detach()
        self.slider.detach()

    def copy(self, interactor):
        value = self.slider.repr.GetValue()
        min_val = self.slider.repr.GetMinimumValue()
        max_val = self.slider.repr.GetMaximumValue()
        p1x, p1y, _ = self.slider.repr.GetPoint1Coordinate().GetValue()
        p2x, p2y, _ = self.slider.repr.GetPoint2Coordinate().GetValue()

        b = SliderButton(interactor, value, min_val, max_val, self.label, update=self.slider.update_callback, end=self.slider.end_callback,
                          corner_radius=self.radius, width=self.width, height=self.height, left=self.left, top=self.top, image=self.image,
                         bgcolor=self.bgcolor, fgcolor=self.fgcolor, opacity=self.opacity, size=self.size, states=self.states, halign=self.halign, valign=self.valign,
                         point1=(p1x,p1y), point2=(p2x, p2y))

        state = self.repr.GetState()
        if state != 0:
            b.set_state(state)
            b.action(state)

        return b
