import point
import vcs
import inspect
import text
#import vtk
import vcs.vcs2vtk
from functools import partial

__valign_map__ = {
    0: 0,
    1: 0,
    2: 1,
    3: 2,
    4: 2,
}

__backend_actor_names__ = {
    "mean": "vtk_backend_Mean_text_actor",
    "min": "vtk_backend_Min_text_actor",
    "max": "vtk_backend_Max_text_actor",
}

def get_actor(member, dp):
    if member.member in __backend_actor_names__:
        actor = dp.backend[__backend_actor_names__[member.member]]
    elif "vtk_backend_%s_text_actor" % member.member in dp.backend:
        actor = dp.backend["vtk_backend_%s_text_actor" % member.member]
    else:
        actor = None
    return actor

class LabelEditor(point.PointEditor):
    def __init__(self, interactor, label, dp, configurator):
        self.label = label
        self.display = dp
        super(LabelEditor, self).__init__(interactor, label, configurator)

        self.toolbar = vcs.vtk_ui.Toolbar(self.interactor, "%s Options" % label.member)
        template = vcs.gettemplate(dp.template)

        self.actor = get_actor(self.label, self.display)


        text_types_name = template.name + "_" + label.member

        try:
            self.tt = vcs.gettexttable(text_types_name)
            self.to = vcs.gettextorientation(text_types_name)
        except ValueError:
            self.tt = vcs.createtexttable(text_types_name, label.texttable)
            self.to = vcs.createtextorientation(text_types_name, label.textorientation)

        self.height_button = self.toolbar.add_slider_button(self.to.height, 1, 100, "Height", update=self.update_height)

        halign = self.toolbar.add_button(["Left Align", "Center Align", "Right Align"], action=self.halign)
        valign = self.toolbar.add_button(["Top Align", "Half Align", "Bottom Align"], action=self.valign)

        halign.set_state(self.to.halign)
        valign.set_state(__valign_map__[self.to.valign])

        self.angle_button = self.toolbar.add_slider_button(self.to.angle, 0, 360, "Angle", update=self.update_angle)
        self.fonts = sorted(vcs.elements["font"].keys())

        font_toolbar = self.toolbar.add_toolbar("Fonts")

        self.font_buttons = {}

        def font_setter(font):
            return partial(self.set_font, font)

        deactivate = font_setter("default")

        for ind, font in enumerate(self.fonts):

            if font[:4] != "Math":
                button = font_toolbar.add_toggle_button(font, on=font_setter(font), off=deactivate, font=vcs.elements["font"][font])
            else:
                button = font_toolbar.add_toggle_button(font, on=font_setter(font), off=deactivate)

            if vcs.elements["fontNumber"][self.tt.font] == font:
                button.set_state(1)
            self.font_buttons[font] = button

        self.picker = None
        self.toolbar.add_button(["Change Color"], action=self.change_color)

        self.label.texttable = self.tt.name
        self.label.textorientation = self.to.name

    def save(self):
        if self.actor:
            self.sync_actor()
        else:
            self.configurator.changed = True
            self.configurator.save()

    def sync_actor(self):
        if self.actor:
            p = self.actor.GetTextProperty()
            winSize = self.interactor.GetRenderWindow().GetSize()
            vcs.vcs2vtk.prepTextProperty(p,winSize,to=self.to,tt=self.tt,cmap=None)

    def set_font(self, font):
        current_font = vcs.getfontname(self.tt.font)
        if font != current_font:
            self.font_buttons[current_font].set_state(0)
        self.tt.font = font
        self.font_buttons[font].set_state(1)
        self.save()

    def halign(self, state):
        self.to.halign = state
        self.save()

    def valign(self, state):
        if state == 0:
            self.to.valign = 0
        elif state == 1:
            self.to.valign = 2
        elif state == 2:
            self.to.valign = 3

        self.save()

    def update_height(self, value):
        self.to.height = int(value)
        self.height_button.set_value(int(value))
        self.save()

    def update_angle(self, value):
        self.to.angle = int(value)
        self.angle_button.set_value(int(value))
        self.save()

    def change_color(self, state):
        if self.picker:
            self.picker.make_current()
        else:
            self.picker = vcs.colorpicker.ColorPicker(500, 500, vcs.getcolormap(), self.tt.color, on_save=self.set_color, on_cancel=self.cancel_color)

    def set_color(self, cmap, color):
        self.tt.color = color
        self.picker = None
        self.save()
        #text colormap is currently not in place, will be later.
        #self.text.colormap = cmap

    def cancel_color(self):
        self.picker = None


    def get_text(self):
        return get_label_text(self.label, self.display)

    def place(self):
        pass

    def is_object(self, label):
        return self.label == label

    def in_bounds(self, x, y):
        t = self.get_text()
        swidth, sheight = self.interactor.GetRenderWindow().GetSize()
        return inside_label(self.label, t, x, y, swidth, sheight)
    
    def delete(self):
        super(LabelEditor, self).delete()
        self.actor.SetVisibility(0)
        self.configurator.deactivate(self)

    def detach(self):
        super(LabelEditor, self).detach()
        self.toolbar.detach()

    def update_priority(self):
        maxLayers = self.interactor.GetRenderWindow().GetNumberOfLayers()
        new_layer = self.label.priority * 10000 + 1 + self.configurator.displays.index(self.display)
        if new_layer + 1 > maxLayers:
            self.interactor.GetRenderWindow().SetNumberOfLayers(new_layer + 1)

        self.actor.SetLayerNumber(new_layer)

        self.interactor.GetRenderWindow().Render()


def get_label_text(label, display):
    actor = get_actor(label, display)

    if actor is not None:
        return actor.GetInput()
    else:
        s = label.member

        array = display.array[0]

        try:
            t = getattr(array, s)
        except AttributeError:
            t = ''
        return t

def inside_label(label, t, x, y, screen_width, screen_height):
    tt = label.texttable
    to = label.textorientation

    tc = vcs.createtextcombined(Tt_source=tt, To_source=to)
    tc.string = [t]
    tc.x = [label.x]
    tc.y = [label.y]

    return text.inside_text(tc, x, y, screen_width, screen_height) is not None
