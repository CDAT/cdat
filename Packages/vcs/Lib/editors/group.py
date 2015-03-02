import priority
import vcs
from vcs.colorpicker import ColorPicker
import text, label, marker


def extract_widgets(editor):
    try:
        widgets = editor.handles
    except AttributeError:
        try:
            widgets = [editor.handle]
        except AttributeError:
            try:
                widgets = editor.textboxes
            except AttributeError:
                widgets = []
    return widgets

class GroupEditor(priority.PriorityEditor, vcs.vtk_ui.behaviors.ClickableMixin, vcs.vtk_ui.behaviors.DraggableMixin):
    """
    Editor for multiple targets at once

    Multiplexes events to all targets, provides a toolbar to edit like items.
    """
    def __init__(self, interactor, targets):

        self.targets = []
        self.types = set()

        self.toolbar = vcs.vtk_ui.Toolbar(interactor, "Group Options")
        self.picker = None
        self.color_button = self.toolbar.add_button(["Change Color"], action=self.change_color)
        self.toolbar.show()
        self.widgets = []
        self.widget_dragged_methods = {}

        for target in targets:
            if target is None:
                continue
            self.add_target(target)

        self.interactor = interactor
        super(GroupEditor, self).__init__()
        self.register()

    def set_up_toolbar(self):
        # Strip existing items
        for w in self.toolbar.widgets:
            if w != self.color_button:
                w.detach()
        self.toolbar.widgets = [self.color_button]

        if self.types <= set((text.TextEditor, label.LabelEditor)):
            self.toolbar.add_slider_button(12, 1, 100, "Height", update=self.update_height)
            self.toolbar.add_slider_button(0, 0, 360, "Angle", update=self.update_angle)
            self.fonts = sorted(vcs.elements["font"].keys())

            self.toolbar.add_button(["Left Align", "Center Align", "Right Align"], action=self.halign)
            self.toolbar.add_button(["Top Align", "Half Align", "Bottom Align"], action=self.valign)

            font_toolbar = self.toolbar.add_toolbar("Fonts")

            font_buttons = {}

            def font_setter(font):
                def set_font():
                    self.multiplex("set_font", font)
                return set_font

            deactivate = font_setter("default")

            for ind, font in enumerate(self.fonts):
                # Math fonts render unintelligbly
                if font[:4] != "Math":
                    button = font_toolbar.add_toggle_button(font, on=font_setter(font), off=deactivate, font=vcs.elements["font"][font])
                else:
                    button = font_toolbar.add_toggle_button(font, on=font_setter(font), off=deactivate)

                font_buttons[font] = button
        elif self.types <= set((marker.MarkerEditor,)):
            self.toolbar.add_slider_button(10, 1, 300, "Marker Size", update=self.set_size)

            type_bar = self.toolbar.add_toolbar("Marker Type", open_label="Change")

            shapes = marker.marker_shapes()

            shapes.insert(0, "Select Shape")
            self.shape_button = type_bar.add_button(shapes, action=self.change_shape)

            wmos = marker.wmo_shapes()
            wmos.insert(0, "Select WMO Marker")

            self.wmo_button = type_bar.add_button(wmos, action=self.change_wmo)

    def change_color(self, state):
        if self.picker:
            self.picker.make_current()
        else:
            self.picker = ColorPicker(500, 500, vcs.getcolormap(), 0, on_save=self.set_color, on_cancel=self.cancel_color)

    def set_color(self, colormap, color):
        self.multiplex("set_color", colormap, color)
        self.picker = None

    def cancel_color(self):
        self.picker = None

    def is_object(self, obj):
        return self.contains_object(obj)

    def in_bounds(self, x, y):
        for target in self.targets:
            if target.in_bounds(x, y):
                return True
        return False

    def handle_click(self, point):
        for target in self.targets:
            if target.handle_click(point):
                return True
        return False

    def add_target(self, target):
        target.unregister()

        # Extract all handles and textboxes, swap out their drag methods with something that multiplexes
        new_widgets = extract_widgets(target)
        for widget in new_widgets:
            self.widget_dragged_methods[widget] = widget.dragged
            widget.dragged = self.dragged
        self.widgets.extend(new_widgets)

        if type(target) not in self.types:
            self.types.add(type(target))
            self.set_up_toolbar()

        target.toolbar.detach()
        self.targets.append(target)

    def remove_target(self, target):
        self.targets.remove(target)

        widgets = extract_widgets(target)
        for w in widgets:
            self.widgets.remove(w)

        for t in self.targets:
            if type(t) == type(target):
                break
        else:
            self.types.remove(type(target))
            self.set_up_toolbar()
        target.detach()

    def multiplex(self, action, *args, **kwargs):
        for target in self.targets:
            try:
                func = getattr(target, action)
                func(*args, **kwargs)
            except AttributeError:
                pass

    def remove_object(self, obj):
        for target in self.targets:
            if target.is_object(obj):
                self.remove_target(target)

    def contains_object(self, obj):
        for target in self.targets:
            if target.is_object(obj):
                return True
        return False

    def detach(self):
        self.multiplex("detach")
        self.toolbar.detach()
        self.targets = None
        self.unregister()

    def place(self):
        self.multiplex("place")

    def raise_priority(self):
        self.multiplex("raise_priority")

    def lower_priority(self):
        self.multiplex("lower_priority")

    def delete(self):
        self.multiplex("delete")

    def deactivate(self):
        self.multiplex("deactivate")

    def click_press(self):
        self.multiplex("click_press")

    def click_release(self):
        self.multiplex("click_release")

    def double_press(self):
        self.multiplex("double_press")

    def double_release(self):
        self.multiplex("double_release")

    def right_press(self):
        self.multiplex("right_press")

    def right_release(self):
        self.multiplex("right_release")

    def key_pressed(self, key, shift=False, alt=False, control=False):
        self.multiplex("key_pressed", key, shift=False, alt=False, control=False)

    def key_released(self, key, shift=False, alt=False, control=False):
        self.multiplex("key_released", key, shift=False, alt=False, control=False)

    def drag_start(self):
        self.multiplex("drag_start")

    def drag_move(self, delta_x, delta_y):
        self.multiplex("drag_move", delta_x, delta_y)
        self.dragged(None, delta_x, delta_y)

    def drag_stop(self):
        self.multiplex("drag_stop")

    def dragged(self, obj, dx, dy):
        for widget in self.widgets:
            if widget != obj:
                try:
                    widget.x += dx
                    widget.y += dy
                except AttributeError:
                    w, h = self.interactor.GetRenderWindow().GetSize()
                    widget.left += w * dx
                    widget.top -= h * dy
                widget.place()
            self.widget_dragged_methods[widget](widget, dx, dy)

    # Text toolbar multiplexed methods
    def update_height(self, value):
        self.multiplex("update_height", value)
    def update_angle(self, value):
        self.multiplex("update_angle", value)
    def halign(self, value):
        self.multiplex("halign", value)
    def valign(self, value):
        self.multiplex("valign", value)
    # Marker toolbar multiplexed methods
    def change_shape(self, state):
        self.multiplex("change_shape", state)
        if state == 0:
            self.wmo_button.set_state(1)
        else:
            self.wmo_button.set_state(0)
    def change_wmo(self, state):
        self.multiplex("change_wmo", state)
        if state == 0:
            self.shape_button.set_state(1)
        else:
            self.shape_button.set_state(0)
    def set_size(self, size):
        self.multiplex("set_size", size)