from vtk import vtkTextActor, vtkTextWidget, vtkTextRepresentation, vtkTextRenderer, vtkInteractorStyle

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

    def __init__(self, interactor, string, action=None, fgcolor=(1,1,1), size=24, font="Arial", left=0, top=0):
        widget = vtkTextWidget()
        self.actor = text_actor(string, fgcolor, size, font)
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

class Textbox(Label):
    def __init__(self, interactor, string, fgcolor=(0, 0, 0), size=24, font="Arial", left=0, top=0):
        super(Textbox, self).__init__(interactor, string, action=self.clicked, fgcolor=fgcolor, size=size, font=font, left=left, top=top)
        self.editing = False
        self.edit_indicator = None
        self.column = 0
        self.row = 0
        self.text = string
        # OK, nevermind; we're going to need to swap out the interactor style when editing is enabled, so we can grab the keys.
        # We'll initialize that here, then pass the keys into the typed function here.
        # Since we're not subscribing to a notification from the widget, we need to do this manually
        self.keyboard_observer = self.interactor.AddObserver("KeyPressEvent", self.typed, 1.0)

    def get_char(self):
        keycode = self.interactor.GetKeyCode()
        if len(keycode) == 0 or ord(keycode) > 126 or ord(keycode) < 33:
            keycode = self.interactor.GetKeySym()
        return keycode

    def add_character(self, character):
        rows = self.text.split("\n")

        row = rows[self.row]

        if self.column >= len(row):
            row += character
            rows[self.row] = row
            if character == "\n":
                self.column = 0
                self.row += 1
            else:
                self.column = len(row)
        else:
            row = row[:self.column] + character + row[self.column:]
            rows[self.row] = row
            if character != "\n":
                self.column += 1
            else:
                self.row += 1
                self.column = 0

        self.text = "\n".join(rows)


    def delete_character(self):
        rows = self.text.split("\n")

        if self.column == 0 and self.row > 0:
            row = rows[self.row - 1]
            self.column = len(row)
            row += rows[self.row]
            rows[self.row - 1] = row
            del rows[self.row]
            self.row -= 1
        elif self.column == 0:
            return
        else:
            row = rows[self.row]
            if self.column < len(row):
                print row, row[:self.column - 1], row[self.column:]
                rows[self.row] = row[:self.column - 1] + row[self.column:]

                self.column -= 1
            else:
                rows[self.row] = row[:-1]
                self.column = len(row)

        self.text = "\n".join(rows)


    def typed(self, obj, event):
        if self.editing:
            c = self.get_char()

            if c in "qQeEjJ3cCtTaAsS":
                # Prevents VTK from doing stuff with these keys
                self.interactor.SetKeyCode("`")

            if c == "Backspace":
                self.delete_character()
            elif c == "Return":
                self.add_character("\n")
            elif len(c) == 1:
                self.add_character(c)
            elif c == "space":
                self.add_character(" ")
            elif c == "Escape":
                self.editing = False
            elif c[:5] == "Shift":
                pass
            elif c == "Tab":
                self.add_character("\t")

            if c in ("Left", "Right", "Up", "Down"):
                rows = self.text.split("\n")

                if c == "Left":
                    if self.column == 0 and self.row > 0:
                        self.row -= 1
                        self.column = len(rows[self.row])
                    else:
                        self.column = max(0, self.column - 1)
                elif c == "Right":
                    if self.column == len(rows[self.row]) and self.row < len(rows) - 1:
                        self.column = 0
                        self.row += 1
                    else:
                        self.column = min(self.column + 1, len(rows[self.row]))
                elif c == "Up":
                    self.row = max(0, self.row - 1)
                    self.column = min(len(rows[self.row]), self.columnQ)
                elif c == "Down":
                    if self.row == len(rows) - 1:
                        self.column = len(rows[self.row])
                    else:
                        self.row += 1

            if self.text[-2:] == "QQ":
                self.editing = False
                self.text = self.text[:-2]

            self.update()

    def set_text(self, text):
        super(Textbox, self).set_text(text)
        self.text = text

    def update(self):
        if self.repr.GetText() != self.text:
            self.widget.On()
            self.set_text(self.text)
            self.widget.On()

    def clicked(self, point):

        self.editing = not self.editing
        if self.editing:
            rows = self.text.split("\n")
            # This should get changed to figuring out which row and column based on where the click was
            self.row = len(rows) - 1
            self.column = len(rows[-1])

    def detach(self):
        print "detaching"
        super(Textbox, self).detach()