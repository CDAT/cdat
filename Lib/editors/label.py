import point
import vcs
import text
import vtk
import vcs.vcs2vtk
from font import FontEditor
from vcs.vtk_ui.text import contrasting_color
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
    """
    An editor for text items that have data-provided text (template.min, template.max, etc.)

    Draggable, provides a toolbar for config options.
    """
    def __init__(self, interactor, label, dp, configurator):
        self.label = label
        self.display = dp
        super(LabelEditor, self).__init__(interactor, label, configurator)

        self.toolbar = vcs.vtk_ui.Toolbar(self.interactor, "%s Options" % label.member)
        template = vcs.gettemplate(dp.template)

        self.actor = get_actor(self.label, self.display)

        tprop = self.actor.GetTextProperty()
        self.real_bg = tprop.GetBackgroundColor()
        self.real_bg_opacity = tprop.GetBackgroundOpacity()

        tprop.SetBackgroundColor(contrasting_color(*tprop.GetColor()))
        tprop.SetBackgroundOpacity(.85)

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

        font_editor = FontEditor(self.toolbar, self.set_font, vcs.elements["fontNumber"][self.tt.font])

        self.picker = None
        self.toolbar.add_button(["Change Color"], action=self.change_color)
        self.toolbar.show()
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
        self.tt.font = font
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
            self.picker = vcs.colorpicker.ColorPicker(500, 500, vcs.getcolormap(), self.tt.color, parent_interactor=self.interactor, on_save=self.set_color, on_cancel=self.cancel_color)

    def set_color(self, cmap, color):
        self.tt.color = color
        self.picker = None
        self.save()
        tprop = self.actor.GetTextProperty()
        tprop.SetBackgroundColor(contrasting_color(*tprop.GetColor()))
        tprop.SetBackgroundOpacity(.85)
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
        w, h = self.interactor.GetRenderWindow().GetSize()
        x, y = x * w, y * h
        picker = vtk.vtkPropPicker()
        for ren in vcs.vcs2vtk.vtkIterate(self.interactor.GetRenderWindow().GetRenderers()):
            if ren.HasViewProp(self.actor):
                break
        else:
            return False

        if picker.PickProp(x, y, ren) and picker.GetViewProp() == self.actor:
            return True
        else:
            return False

    def delete(self):
        super(LabelEditor, self).delete()
        self.actor.SetVisibility(0)
        self.configurator.deactivate(self)

    def detach(self):
        super(LabelEditor, self).detach()
        self.toolbar.detach()
        tprop = self.actor.GetTextProperty()
        tprop.SetBackgroundColor(*self.real_bg)
        tprop.SetBackgroundOpacity(self.real_bg_opacity)

    def update_priority(self):
        maxLayers = self.interactor.GetRenderWindow().GetNumberOfLayers()
        new_layer = self.label.priority * 10000 + 1 + self.configurator.displays.index(self.display)
        if new_layer + 1 > maxLayers:
            self.interactor.GetRenderWindow().SetNumberOfLayers(new_layer + 1)

        self.actor.SetLayerNumber(new_layer)

        self.render()

    def render(self):
        from vcs.vtk_ui.manager import get_manager
        m = get_manager(self.interactor)
        m.queue_render()


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
