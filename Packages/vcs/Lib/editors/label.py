import point
import vcs
import inspect
import text
#import vtk
import vcs.vcs2vtk

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
    "zvalue": "vtk_backend_zvalue_text_actor",
    "crtime": "vtk_backend_crtime_text_actor",
    "crdate": "vtk_backend_crdate_text_actor",
}

class LabelEditor(point.PointEditor):
    def __init__(self, interactor, label, dp, configurator):
        self.label = label
        self.display = dp
        super(LabelEditor, self).__init__(interactor, label, configurator)

        self.toolbar = vcs.vtk_ui.Toolbar(self.interactor, "Label Options")
        template = vcs.gettemplate(dp.template)


        self.actor = None
        if self.label.member in __backend_actor_names__:
            self.actor = dp.backend[__backend_actor_names__[self.label.member]]

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

        font_buttons = {}

        def font_setter(font):
            def set_font():
                current_font = vcs.getfontname(self.tt.font)
                if font != current_font:
                    font_buttons[current_font].set_state(0)
                self.tt.font = font
                font_buttons[font].set_state(1)

                self.save()

            return set_font

        deactivate = font_setter("default")

        for ind, font in enumerate(self.fonts):

            if font[:4] != "Math":
                button = font_toolbar.add_toggle_button(font, on=font_setter(font), off=deactivate, font=vcs.elements["font"][font])
            else:
                button = font_toolbar.add_toggle_button(font, on=font_setter(font), off=deactivate)

            if vcs.elements["fontNumber"][self.tt.font] == font:
                button.set_state(1)
            font_buttons[font] = button

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
        return get_label_text(self.label, self.display.array[0])

    def place(self):
        pass

    def is_object(self, label):
        return self.label == label

    def in_bounds(self, x, y):
        t = self.get_text()
        swidth, sheight = self.interactor.GetRenderWindow().GetSize()
        return inside_label(self.label, t, x, y, swidth, sheight)

    def detach(self):
        super(LabelEditor, self).detach()
        self.toolbar.detach()


def get_label_text(label, array):
    s = label.member

    smn, smx = vcs.minmax(array)

    if s == 'min':
        t = 'Min %g' % (smn)
    elif s == 'max':
        t = 'Max %g' % smx
    elif s == 'mean':
        if not inspect.ismethod(getattr(array,'mean')):
            t = float(getattr(array,s))
        else:
            t = array.mean()

        t = "Mean %f" % t
    else:
        # General slab attributes
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