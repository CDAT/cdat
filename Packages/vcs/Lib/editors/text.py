from vcs.vtk_ui import Textbox, Toolbar
import vcs.vtk_ui.text
from vcs.colorpicker import ColorPicker
from vtk import vtkTextProperty
from vcs.vtk_ui.behaviors import ClickableMixin
import vcs

class TextEditor(ClickableMixin):
    def __init__(self, interactor, text, index, configurator):

        self.interactor = interactor
        self.text = text
        self.index = index
        self.configurator = configurator

        # We're going to insert textboxes at each of the locations
        # That way we can edit the text.
        self.old_priority = text.priority
        text.priority = 0
        configurator.save()
        self.textboxes = None

        self.toolbar = Toolbar(self.interactor, "Text Options")
        self.toolbar.add_slider_button(text.height, 1, 100, "Height", update=self.update_height, end=self.save_height)
        halign_bar = self.toolbar.add_toolbar("Horizontal Align")

        self.left_align_button = halign_bar.add_toggle_button("Left Align", on=self.align_left, off=self.dealign_left)
        self.center_align_button = halign_bar.add_toggle_button("Center Align", on=self.align_center, off=self.dealign_center)
        self.right_align_button = halign_bar.add_toggle_button("Right Align", on=self.align_right, off=self.dealign_right)

        valign_bar = self.toolbar.add_toolbar("Vertical Align")

        self.top_align_button = valign_bar.add_toggle_button("Top Align", on=self.align_top, off=self.dealign_top)
        self.half_align_button = valign_bar.add_toggle_button("Half Align", on=self.align_half, off=self.dealign_half)
        self.bottom_align_button = valign_bar.add_toggle_button("Bottom Align", on=self.align_bottom, off=self.dealign_bottom)

        self.picker = None
        self.toolbar.add_button(["Change Color"], action=self.change_color)

        super(TextEditor, self).__init__()
        self.register()
        self.toggle_halign_buttons()
        self.toggle_valign_buttons()

    def update(self):

        if self.textboxes:

            for box in self.textboxes:
                box.stop_editing()
                box.detach()
            del self.textboxes

        self.textboxes = []
        w, h = self.interactor.GetRenderWindow().GetSize()
        cmap = vcs.getcolormap()

        prop = vtkTextProperty()
        vcs.vcs2vtk.prepTextProperty(prop, (w, h), self.text, self.text, cmap)

        for ind, x in enumerate(self.text.x):
            y = self.text.y[ind]
            string = self.text.string[ind]

            text_width, text_height = text_dimensions(self.text, ind, (w, h))
            x = x * w
            y = h - y * h
            if self.text.valign in ("half", 2):
                y -= text_height / 2.0
            elif self.text.valign in ("bottom", "base", 3, 4):
                y -= text_height

            if self.text.halign in ("right", 2):
                x -= text_width
            elif self.text.halign in ("center", 1):
                x -= text_width / 2.0

            textbox = Textbox(self.interactor, string, left=x, top=y, movable=True, on_editing_end=self.finished_editing, on_move=self.moved_textbox, textproperty=prop, on_click=self.textbox_clicked)
            textbox.show()

            if ind == self.index:
                textbox.start_editing()

            self.textboxes.append(textbox)

    def finished_editing(self, textbox):
        self.text.string[self.textboxes.index(textbox)] = textbox.text

    def in_bounds(self, x, y):
        return inside_text(self.text, x, y, *self.interactor.GetRenderWindow().GetSize(), index=self.index) is not None

    def click_release(self):
        x, y = self.event_position()
        text_index = inside_text(self.text, x, y, *self.interactor.GetRenderWindow().GetSize())

        self.handle_click(text_index, x, y)

    def double_release(self):
        x, y = self.event_position()

        text_index = inside_text(self.text, x, y, *self.interactor.GetRenderWindow().GetSize())

        if text_index is None:

            self.textboxes[self.index].stop_editing()

            # Add a new text item to self.text, update, and start editing
            new_index = len(self.text.x)

            self.text.x.append(x)
            self.text.y.append(y)
            self.text.string.append("New Text")

            self.index = new_index
            self.update()

    def moved_textbox(self):
        box = self.textboxes[self.index]
        w, h = self.interactor.GetRenderWindow().GetSize()
        self.text.x[self.index] = box.left / float(w)
        self.text.y[self.index] = (h - box.top - box.get_dimensions()[1] / 2.0) / float(h)

    def handle_click(self, text_index, x, y):

        if text_index == self.index:
            # Adjust cursor position
            self.textboxes[self.index].start_editing((x, y))
            return
        else:
            self.textboxes[self.index].stop_editing()

            if text_index is None:
                self.deactivate()
            else:
                # Change which one we're editing
                self.index = text_index
                self.textboxes[self.index].start_editing((x, y))

    def textbox_clicked(self, point):
        x, y = point

        winsize = self.interactor.GetRenderWindow().GetSize()

        clicked_on = inside_text(self.text, x, y, *winsize)
        self.handle_click(clicked_on, x, y)

    def save(self):
        self.configurator.save()

    def deactivate(self):
        self.text.priority = self.old_priority
        self.configurator.deactivate(self)

    def update_height(self, value):
        self.text.height = value
        self.update()

    def save_height(self, value):
        self.text.height = value
        self.save()

    def change_color(self, state):
        if self.picker:
            self.picker.make_current()
        else:
            self.picker = ColorPicker(500, 500, vcs.getcolormap(), self.text.color, on_save=self.set_color, on_cancel=self.cancel_color)

    def set_color(self, cmap, color):
        self.text.color = color
        self.update()
        self.picker = None
        #text colormap is currently not in place, will be later.
        #self.text.colormap = cmap

    def cancel_color(self):
        self.picker = None

    def detach(self):
        self.unregister()
        for box in self.textboxes:
            box.detach()
        del self.textboxes
        self.toolbar.detach()

    def toggle_halign_buttons(self):
        halign = self.text.halign
        buttons = [self.left_align_button, self.right_align_button, self.center_align_button]

        if halign in ("left", 0):
            states = [1, 0, 0]
        elif halign in ("right", 2):
            states = [0, 1, 0]
        else:
            states = [0, 0, 1]

        for state, button in zip(states, buttons):
            button.set_state(state)

        self.update()

    def toggle_valign_buttons(self):
        valign = self.text.valign
        buttons = [self.top_align_button, self.bottom_align_button, self.half_align_button]

        if valign in ("top", 0, 'cap', 1):
            states = [1, 0, 0]
        elif valign in ("bottom", 'base', 3, 4):
            states = [0, 1, 0]
        else:
            states = [0, 0, 1]

        for state, button in zip(states, buttons):
            button.set_state(state)

        self.update()


    def align_left(self):
        self.text.halign = "left"
        self.toggle_halign_buttons()
    def dealign_left(self):
        self.toggle_halign_buttons()

    def align_center(self):
        self.text.halign = "center"
        self.toggle_halign_buttons()

    def dealign_center(self):
        self.text.halign = "left"
        self.toggle_halign_buttons()

    def align_right(self):
        self.text.halign = "right"
        self.toggle_halign_buttons()
    def dealign_right(self):
        self.text.halign = "left"
        self.toggle_halign_buttons()

    def align_top(self):
        self.text.valign = "top"
        self.toggle_valign_buttons()
    def dealign_top(self):
        self.toggle_valign_buttons()

    def align_half(self):
        self.text.valign = "half"
        self.toggle_valign_buttons()
    def dealign_half(self):
        self.text.valign = "top"
        self.toggle_valign_buttons()

    def align_bottom(self):
        self.text.valign = "bottom"
        self.toggle_valign_buttons()
    def dealign_bottom(self):
        self.text.valign = "top"
        self.toggle_valign_buttons()

def text_dimensions(text, index, winsize):
    prop = vtkTextProperty()
    vcs.vcs2vtk.prepTextProperty(prop, winsize, text, text, vcs.getcolormap())
    return vcs.vtk_ui.text.text_dimensions(text.string[index], prop)

def inside_text(text, x, y, screen_width, screen_height, index=None):

    winsize = (screen_width, screen_height)

    if x > 1:
        x = x / float(screen_width)
    if y > 1:
        y = y / float(screen_height)

    for ind, xcoord in enumerate(text.x):
        if index is not None:
            if ind != index:
                continue

        ycoord = text.y[ind]
        text_width, text_height = text_dimensions(text, ind, winsize)
        text_width = text_width / float(screen_width)
        text_height = text_height / float(screen_height)


        if text.valign in ("half", 2):
            ycoord -= text_height / 2.0
        elif text.valign in ("top", 0):
            ycoord -= text_height

        if text.halign in ("right", 2):
            xcoord -= text_width
        elif text.halign in ("center", 1):
            xcoord -= text_width / 2.0

        if x > xcoord and x < xcoord + text_width and y < ycoord + text_height and y > ycoord:
            return ind

    return None