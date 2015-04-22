from vcs.vtk_ui import Textbox, Toolbar, Label
import vcs.vtk_ui.text
from vcs.colorpicker import ColorPicker
from vtk import vtkTextProperty, vtkPropPicker, vtkPropCollection
from vcs.vtk_ui.behaviors import ClickableMixin
import priority
import vcs
from vcs.vcs2vtk import genTextActor, vtkIterate
from font import FontEditor
import sys

__valign_map__ = {
    0: 0,
    1: 0,
    2: 1,
    3: 2,
    4: 2,
}


class TextEditor(ClickableMixin, priority.PriorityEditor):
    """
    Editor for `textcombined` objects

    Click a text box to edit the text, config toolbar, draggable textboxes (using the vtk_ui.textbox widget).
    """
    def __init__(self, interactor, text, index, dp, configurator):

        self.interactor = interactor
        self.text = text

        self.display = dp
        self.actors = dp.backend["vtk_backend_text_actors"]

        self.index = index
        self.configurator = configurator

        for actor in self.actors:
            actor.SetVisibility(0)

        self.textboxes = None

        self.toolbar = Toolbar(self.interactor, "Text Options")
        self.toolbar.add_slider_button(text.height, 1, 100, "Height", update=self.update_height)

        halign = self.toolbar.add_button(["Left Align", "Center Align", "Right Align"], action=self.halign)
        valign = self.toolbar.add_button(["Top Align", "Half Align", "Bottom Align"], action=self.valign)
        halign.set_state(self.text.halign)
        valign.set_state(__valign_map__[self.text.valign])

        self.toolbar.add_slider_button(text.angle, 0, 360, "Angle", update=self.update_angle)

        font_editor = FontEditor(self.toolbar, self.set_font, vcs.elements["fontNumber"][self.text.font])

        self.picker = None
        self.toolbar.add_button(["Change Color"], action=self.change_color)
        self.toolbar.show()

        prop = vtkTextProperty()
        prop.SetBackgroundColor(.87, .79, .55)
        prop.SetBackgroundOpacity(1)
        prop.SetColor(0, 0, 0)
        prop.SetVerticalJustificationToTop()
        self.tooltip = Label(self.interactor, "%s + Click to place new text." % ("Cmd" if sys.platform == "darwin" else "Ctrl"), textproperty=prop)
        self.tooltip.left = 0
        self.tooltip.top = self.interactor.GetRenderWindow().GetSize()[1] - self.tooltip.get_dimensions()[1]
        self.tooltip.show()
        super(TextEditor, self).__init__()
        self.register()
        self.update()

    def set_font(self, font):
        self.text.font = font
        self.update()

    def get_object(self):
        return self.text

    def is_object(self, text):
        return self.text == text

    def place(self):
        self.toolbar.place()
        for box in self.textboxes:
            box.place()

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
        vcs.vcs2vtk.prepTextProperty(prop, (w, h), to=self.text, tt=self.text, cmap=cmap)
        prop.SetOrientation(-1 * self.text.angle)

        for ind, x in enumerate(self.text.x):
            self.actors[ind].SetTextProperty(prop)

            y = self.text.y[ind]
            string = self.text.string[ind]

            text_width, text_height = text_dimensions(self.text, ind, (w, h))
            x = x * w
            y = h - y * h  # mirror the y axis for widgets

            if self.text.halign in ("right", 2):
                x -= text_width
            elif self.text.halign in ("center", 1):
                x -= text_width / 2.0

            if self.text.valign in ("half", 2):
                y -= text_height / 2.0
            elif self.text.valign in ("bottom", 4):
                y -= text_height

            box_prop = vtkTextProperty()
            vcs.vcs2vtk.prepTextProperty(box_prop, (w, h), to=self.text, tt=self.text, cmap=cmap)
            box_prop.SetOrientation(-1 * self.text.angle)
            text_color = box_prop.GetColor()
            highlight_color = vcs.vtk_ui.text.contrasting_color(*text_color)

            textbox = Textbox(self.interactor, string, left=x, top=y, highlight_color=highlight_color, highlight_opacity=.8, movable=True, on_editing_end=self.finished_editing, on_drag=self.moved_textbox, textproperty=box_prop, on_click=self.textbox_clicked)
            textbox.show()
            textbox.show_highlight()

            if ind == self.index:
                textbox.start_editing()

            self.textboxes.append(textbox)

    def finished_editing(self, textbox):
        index = self.textboxes.index(textbox)
        self.text.string[index] = textbox.text
        self.actors[index].SetInput(textbox.text)

    def get_box_at_point(self, x, y):
        for box in self.textboxes:
            if box.in_bounds(x, y):
                return box
        return None

    def in_bounds(self, x, y):
        return self.get_box_at_point(x, y) is not None

    def click_release(self):
        x, y = self.event_position()
        w, h = self.interactor.GetRenderWindow().GetSize()
        box = self.get_box_at_point(x * w, y * h)

        text_index = None if box is None else self.textboxes.index(box)

        self.process_click(text_index, x, y)

    def moved_textbox(self, box, dx, dy):
        self.text.x[self.index] += dx
        self.text.y[self.index] += dy
        w, h = self.interactor.GetRenderWindow().GetSize()
        self.actors[self.index].SetPosition(w * self.text.x[self.index], h * self.text.y[self.index])

    def handle_click(self, point):
        x, y = point
        return self.in_bounds(x, y) or self.toolbar.in_toolbar(x, y) or self.current_modifiers()["control"]

    def process_click(self, text_index, x, y):

        if text_index == self.index:
            # Adjust cursor position
            self.textboxes[self.index].start_editing((x, y))
            return
        else:
            self.textboxes[self.index].stop_editing()

            if text_index is not None:
                # Change which one we're editing
                self.index = text_index
                self.textboxes[self.index].start_editing((x, y))
            else:
                if self.current_modifiers()["control"]:

                    self.textboxes[self.index].stop_editing()

                    # Add a new text item to self.text, update, and start editing
                    new_index = max(len(self.text.x), len(self.text.y), len(self.text.string))

                    self.text.x.append(x)
                    self.text.y.append(y)
                    self.text.string.append("Click to Edit")

                    new_actor = genTextActor(self.actors[0].GetConsumer(0), string=["Click to Edit"], x=[x], y=[y],to=self.text,tt=self.text,cmap=vcs.getcolormap())[0]
                    new_actor.SetVisibility(0)
                    self.actors.append(new_actor)
                    self.index = new_index

                    self.update()

    def textbox_clicked(self, point):
        x, y = point

        winsize = self.interactor.GetRenderWindow().GetSize()

        clicked_on = inside_text(self.text, x, y, *winsize)
        self.process_click(clicked_on, x, y)

    def deactivate(self):
        self.configurator.deactivate(self)

    def update_height(self, value):
        self.text.height = value
        self.update()

    def change_color(self, state):
        if self.picker:
            self.picker.make_current()
        else:
            self.picker = ColorPicker(500, 500, vcs.getcolormap(), self.text.color, parent_interactor=self.interactor, on_save=self.set_color, on_cancel=self.cancel_color)

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
            if box.editing:
                box.stop_editing()
            box.detach()
        del self.textboxes

        if self.text.priority > 0:
            for actor in self.actors:
                actor.SetVisibility(1)
        self.tooltip.detach()
        self.toolbar.detach()

    def halign(self, state):
        self.text.halign = state
        self.update()

    def valign(self, state):
        if state == 0:
            self.text.valign = 0
        elif state == 1:
            self.text.valign = 2
        elif state == 2:
            self.text.valign = 4
        self.update()

    def update_angle(self, value):
        self.text.angle = int(value)
        self.update()

    def change_font(self, state):
        self.text.font = self.fonts[state]
        self.update()

    def delete(self):
        """Overriding PriorityEditor.delete to make this behave intelligently"""
        if not self.textboxes[self.index].editing:
            self.text.priority = 0
            self.configurator.deactivate(self)

    def render(self):
        from vcs.vtk_ui.manager import get_manager
        m = get_manager(self.interactor)
        m.queue_render()

    def update_priority(self):
        maxLayers = self.interactor.GetRenderWindow().GetNumberOfLayers()
        new_layer = self.text.priority * 10000 + 1 + self.configurator.displays.index(self.display)
        if new_layer + 1 > maxLayers:
            self.interactor.GetRenderWindow().SetNumberOfLayers(new_layer + 1)

        for actor in self.actors:
            actor.SetLayerNumber(new_layer)

        self.render()


def text_dimensions(text, index, winsize):
    prop = vtkTextProperty()
    vcs.vcs2vtk.prepTextProperty(prop, winsize, text, text, vcs.getcolormap())
    return vcs.vtk_ui.text.text_dimensions(text.string[index], prop)
