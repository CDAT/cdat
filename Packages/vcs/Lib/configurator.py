import vcs
import datetime
import editors
import vtk_ui
import os, sys
import vtk

CREATING_FILL = "fill"
CREATING_LINE = "line"
CREATING_MARKER = "marker"
CREATING_TEXT = "text"

CLICKS_TO_CREATE = {
    CREATING_FILL: 3,
    CREATING_LINE: 2,
    CREATING_MARKER: 1,
    CREATING_TEXT: 1,
}

import copy

def sync_template(src, target):
    target.orientation=src.orientation
    target.file=copy.copy(src.file)
    target.function=copy.copy(src.function)
    target.logicalmask=copy.copy(src.logicalmask)
    target.transformation=copy.copy(src.transformation)
    target.source=copy.copy(src.source)
    target.dataname=copy.copy(src.dataname)
    target.title=copy.copy(src.title)
    target.units=copy.copy(src.units)
    target.crdate=copy.copy(src.crdate)
    target.crtime=copy.copy(src.crtime)
    target.comment1=copy.copy(src.comment1)
    target.comment2=copy.copy(src.comment2)
    target.comment3=copy.copy(src.comment3)
    target.comment4=copy.copy(src.comment4)
    target.xname=copy.copy(src.xname)
    target.yname=copy.copy(src.yname)
    target.zname=copy.copy(src.zname)
    target.tname=copy.copy(src.tname)
    target.xunits=copy.copy(src.xunits)
    target.yunits=copy.copy(src.yunits)
    target.zunits=copy.copy(src.zunits)
    target.tunits=copy.copy(src.tunits)
    target.xvalue=copy.copy(src.xvalue)
    target.yvalue=copy.copy(src.yvalue)
    target.zvalue=copy.copy(src.zvalue)
    target.tvalue=copy.copy(src.tvalue)
    target.mean=copy.copy(src.mean)
    target.min=copy.copy(src.min)
    target.max=copy.copy(src.max)
    target.xtic1=copy.copy(src.xtic1)
    target.xtic2=copy.copy(src.xtic2)
    target.xmintic1=copy.copy(src.xmintic1)
    target.xmintic2=copy.copy(src.xmintic2)
    target.ytic1=copy.copy(src.ytic1)
    target.ytic2=copy.copy(src.ytic2)
    target.ymintic1=copy.copy(src.ymintic1)
    target.ymintic2=copy.copy(src.ymintic2)
    target.xlabel1=copy.copy(src.xlabel1)
    target.xlabel2=copy.copy(src.xlabel2)
    target.ylabel1=copy.copy(src.ylabel1)
    target.ylabel2=copy.copy(src.ylabel2)
    target.box1=copy.copy(src.box1)
    target.box2=copy.copy(src.box2)
    target.box3=copy.copy(src.box3)
    target.box4=copy.copy(src.box4)
    target.line1=copy.copy(src.line1)
    target.line2=copy.copy(src.line2)
    target.line3=copy.copy(src.line3)
    target.line4=copy.copy(src.line4)
    target.legend=copy.copy(src.legend)
    target.data=copy.copy(src.data)

def array_strings(template, array):
    attrs = [
        "file",
        "function",
        "logicalmask",
        "transformation",
        "source",
        "dataname",
        "title",
        "units",
        "crdate",
        "crtime",
        "comment1",
        "comment2",
        "comment3",
        "comment4",
        "xname",
        "yname",
        "zname",
        "tname",
        "xunits",
        "yunits",
        "zunits",
        "tunits",
        "xvalue",
        "yvalue",
        "zvalue",
        "tvalue",
        "mean",
        "min",
        "max",
        "xtic1",
        "xtic2",
        "xmintic1",
        "xmintic2",
        "ytic1",
        "ytic2",
        "ymintic1",
        "ymintic2",
        "xlabel1",
        "xlabel2",
        "ylabel1",
        "ylabel2",
        "box1",
        "box2",
        "box3",
        "box4",
        "line1",
        "line2",
        "line3",
        "line4",
        "legend",
        "data",
    ]

    template = t(template)

    strings = {}
    for attr in attrs:
        try:
            attribute = getattr(template, attr)

            if is_label(attribute):
                strings[attr] = editors.label.get_label_text(attribute, array)
        except AttributeError:
            pass

    return strings

class Configurator(object):
    def __init__(self, canvas):
        self.canvas = canvas
        self.backend = canvas.backend
        self.interactor = None
        self.display_strings = {}
        self.displays = []
        self.clicking = None
        self.clicked_info = None
        self.target = None
        self.changed = False
        self.toolbar = None
        self.fill_button = None
        self.text_button = None
        self.line_button = None
        self.marker_button = None
        self.initialized = False
        self.animation_speed = 5
        self.animation_timer = None
        self.save_timer = None
        self.save_listener = None
        self.save_anim_button = None
        self.anim_button = None
        self.listeners = []
        self.animation_last_frame_time = datetime.datetime.now()
        # Map custom templates to their source template
        self.templates = {}

        self.creating = False
        self.click_locations = None


    def get_save_path(self, default_name='', dialog_name="Save File"):
        import os.path
        user_home = os.path.expanduser("~")

        output_dir = os.path.join(user_home, ".uvcdat", "animation")
        if os.path.exists(output_dir) == False:
            os.mkdir(output_dir)
        # We'll just use .uvcdat– this is a headless install
        path = os.path.join(output_dir, default_name)

        p_index = 0
        directory, filename = os.path.split(path)
        filename, extension = os.path.splitext(filename)
        while os.path.exists(path):
            path = os.path.join(directory, filename + "_" + str(p_index) + extension)
            p_index += 1
        print "Saving to " + path

        return path


    def shift(self):
        return self.interactor.GetShiftKey() == 1

    def update(self):
        if self.backend.renWin and self.interactor is None:
            self.interactor = self.backend.renWin.GetInteractor()
            if self.interactor is not None:
                self.listeners.append(self.interactor.AddObserver("TimerEvent", self.animate))
                self.listeners.append(self.interactor.AddObserver("LeftButtonPressEvent", self.click))
                self.listeners.append(self.interactor.AddObserver("MouseMoveEvent", self.hover))
                self.listeners.append(self.interactor.AddObserver("LeftButtonReleaseEvent", self.release))
                self.init_buttons()
                self.init_toolbar()

        self.displays = [vcs.elements["display"][display] for display in self.canvas.display_names]
        for display in self.displays:

            if display._template_origin in self.templates:
                continue

            if display.ratio is not None:
                # Ratio'd displays already have a temporary template
                self.templates[display.template] = display._template_origin
            else:
                # Manufacture a placeholder template to use for updates
                new_template = vcs.createtemplate(source=display.template)
                self.templates[new_template.name] = display.template
                display.template = new_template.name
                # This is an attribute used internally; might break
                display._template_origin = new_template.name

    def detach(self):
        if self.interactor is None:
            return

        if self.animation_timer is not None:
            self.stop_animating()

        if self.toolbar is not None:
            self.toolbar.detach()
            self.toolbar = None
        if self.fill_button is not None:
            self.fill_button.detach()
            self.fill_button = None
        if self.text_button is not None:
            self.text_button.detach()
            self.text_button = None
        if self.line_button is not None:
            self.line_button.detach()
            self.line_button = None
        if self.marker_button is not None:
            self.marker_button.detach()
            self.marker_button = None

        if self.target is not None:
            self.target.detach()
            self.target = None

        for listener in self.listeners:
            self.interactor.RemoveObserver(listener)

        # if all of the widgets have been cleaned up correctly, this will delete the manager
        vtk_ui.manager.delete_manager(self.interactor)
        self.interactor.GetRenderWindow().Render()

    def release(self, object, event):
        if self.clicking is None:
            return

        if datetime.datetime.now() - self.clicking[1] < datetime.timedelta(0, .5):

            point = self.clicking[0]
            self.clicking = None
            if self.creating:
                self.click_locations.append(point)
                if len(self.click_locations) == CLICKS_TO_CREATE[self.creating]:
                    self.create()
                return

            if self.target and self.shift() == False and self.target.handle_click(point):
                return

            if self.shift() and type(self.target) != editors.group.GroupEditor:
                self.target = editors.group.GroupEditor(self.interactor, (self.target,))

            clicked = None
            display_clicked = None
            for display in self.displays:
                obj = self.in_display_plot(point, display)
                if obj is not None and (clicked is None or obj.priority >= clicked.priority):
                    clicked = obj
                    display_clicked = display
            if clicked:
                if self.target is None or self.target.is_object(clicked) == False:
                    self.activate(clicked, display_clicked)
                elif self.shift() and self.target.contains_object(clicked):
                    self.target.remove_object(clicked)
                    if len(self.target.targets) == 0:
                        self.target.detach()
                        self.target = None
                        self.save()
                        return
            elif self.target is not None and self.shift() == False:
                self.deactivate(self.target)
                return

        self.clicking = None

    def hover(self, object,event):
        if self.clicking is not None:
            return

        point = self.interactor.GetEventPosition()
        cursor = self.interactor.GetRenderWindow().GetCurrentCursor()
        if self.target:
            if self.target.handle_click(point):
                if self.interactor.GetControlKey() == 1 and type(self.target) in (editors.marker.MarkerEditor, editors.text.TextEditor):
                    if cursor != vtk.VTK_CURSOR_CROSSHAIR:
                        self.interactor.GetRenderWindow().SetCurrentCursor(vtk.VTK_CURSOR_CROSSHAIR)
                else:
                    if cursor != vtk.VTK_CURSOR_HAND:
                        self.interactor.GetRenderWindow().SetCurrentCursor(vtk.VTK_CURSOR_HAND)
                return
        else:
            if self.toolbar.in_toolbar(*point):
                return

        if self.marker_button.in_bounds(*point) or self.text_button.in_bounds(*point):
            return

        for display in self.displays:
            obj = self.in_display_plot(point, display)
            if obj is not None:
                if cursor != vtk.VTK_CURSOR_HAND:
                    self.interactor.GetRenderWindow().SetCurrentCursor(vtk.VTK_CURSOR_HAND)
                return
        if cursor != vtk.VTK_CURSOR_DEFAULT:
            self.interactor.GetRenderWindow().SetCurrentCursor(vtk.VTK_CURSOR_DEFAULT)


    def click(self, object, event):
        self.clicking = (self.interactor.GetEventPosition(), datetime.datetime.now())

    def show(self):
        if self.interactor is None:
            return
        self.place()
        self.toolbar.show()
        self.marker_button.show()
        self.text_button.show()
        man = vtk_ui.manager.get_manager(self.interactor)
        man.elevate()
        self.interactor.Render()
        #self.fill_button.show()
        #self.line_button.show()

    def deactivate(self, obj):
        try:
            if self.target == obj:
                self.target.detach()
                self.target = None
                self.toolbar.show()
            elif obj in self.target.targets:
                self.target.remove_target(obj)
            else:
                # Deactivate the whole group
                self.target.detach()
                self.target = None
                self.toolbar.show()
        except AttributeError:
            pass
        self.save()

    def delete(self, obj, index):
        obj.priority = 0
        self.save()

    def place(self):
        #self.fill_button.place()
        #self.line_button.place()
        self.marker_button.place()
        self.text_button.place()
        self.toolbar.place()
        if self.target:
            self.target.place()

    def activate(self, obj, display):
        if self.target is not None and self.shift() == False:
            self.deactivate(self.target)

        self.toolbar.hide()

        if display.g_type == "fillarea":
            editor = editors.fillarea.FillEditor(self.interactor, obj, self.clicked_info, self)
        elif display.g_type == "line":
            editor = editors.line.LineEditor(self.interactor, obj, self.clicked_info, self)
        elif display.g_type == "marker":
            editor = editors.marker.MarkerEditor(self.interactor, obj, self.clicked_info, display, self)
        elif display.g_type == "text":
            editor = editors.text.TextEditor(self.interactor, obj, self.clicked_info, display, self)
        else:
            if is_box(obj):
                if obj.member == "legend":
                    editor = editors.legend.LegendEditor(self.interactor, t(display.template), self)
                elif obj.member == "data":
                    editor = editors.data.DataEditor(self.interactor, vcs.getgraphicsmethod(display.g_type, display.g_name), t(display.template), self)
                else:
                    editor = editors.box.BoxEditor(self.interactor, obj, self)
            else:
                if is_label(obj):
                    editor = editors.label.LabelEditor(self.interactor, obj, display, self)
                elif is_point(obj):
                    editor = editors.point.PointEditor(self.interactor, obj, self)
                else:
                    editor = None

        if self.target:
            self.target.add_target(editor)
        else:
            self.target = editor



    def in_display_plot(self, point, dp):
        #Normalize the point
        x, y = point
        w, h = self.interactor.GetRenderWindow().GetSize()
        if x > 1 or y > 1:
            point = (x / float(w), y / float(h))
            x, y = point

        if dp.g_type == "fillarea":
            fill = vcs.getfillarea(dp.g_name)
            info = editors.fillarea.inside_fillarea(fill, *point)
            if info is not None:
                self.clicked_info = info
                return fill
        elif dp.g_type == "line":
            l = vcs.getline(dp.g_name)
            # Uses screen_height to determine how much buffer space there is around the line
            info = editors.line.inside_line(l, *point, screen_height=h)
            if info is not None:
                self.clicked_info = info
                return l
        elif dp.g_type == "marker":
            m = vcs.getmarker(dp.g_name)
            info = editors.marker.inside_marker(m, point[0], point[1], w, h)
            if info is not None:
                self.clicked_info = info
                return m
        elif dp.g_type == "text":
            tc = vcs.gettextcombined(dp.g_name)
            info = editors.text.inside_text(tc, point[0], point[1], w, h)
            if info is not None:
                self.clicked_info = info
                return tc
        else:
            fudge = 5 / float(w)
            return in_template(point, t(dp.template), dp, (w, h), fudge=fudge)

    def save(self):
        if self.changed:
            self.canvas.update()
            self.changed = False
        else:
            self.interactor.GetRenderWindow().Render()
        self.canvas.animate.reset()

    def init_toolbar(self):
        self.toolbar = vtk_ui.Toolbar(self.interactor, "Configure", on_open=self.setup_animation)
        # Canvas background color
        color_toolbar = self.toolbar.add_toolbar("Background Color")
        red, green, blue = self.canvas.backgroundcolor

        color_toolbar.add_slider_button(red, 0, 255, "Red", update=self.set_background_red)
        color_toolbar.add_slider_button(green, 0, 255, "Green", update=self.set_background_green)
        color_toolbar.add_slider_button(blue, 0, 255, "Blue", update=self.set_background_blue)

        def logo_on():
          self.canvas.drawlogoon()
          self.canvas.update()
        def logo_off():
          self.canvas.drawlogooff()
          self.canvas.update()

        def save_template_changes(state):
            for new, source in self.templates.iteritems():
                if source != "default":
                    sync_template(vcs.gettemplate(new), vcs.gettemplate(source))

            for display in self.displays:
                if display.g_type not in ("fillarea", "text", "marker", "line"):
                    # Remove the dummy template now that changes are synced
                    new, source = display.template, self.templates[display.template]
                    display.template = source
                    display._template_origin = source

            self.canvas.saveinitialfile()
            self.canvas.update()

        def reset_template_changes(state):
            for new, source in self.templates.iteritems():
                sync_template(vcs.gettemplate(source), vcs.gettemplate(new))
            self.canvas.update()

        # Toggle UV-CDAT logo
        logo_button = self.toolbar.add_toggle_button("Logo", on_prefix="Show", off_prefix="Hide", on=logo_on, off=logo_off)

        self.toolbar.add_button(["Save Templates"], action=save_template_changes)
        self.toolbar.add_button(["Reset Templates"], action=reset_template_changes)

        if self.canvas.getdrawlogo():
          logo_button.set_state(1)

    def setup_animation(self):
        if self.initialized == False:
            self.canvas.animate.create()
            anim_toolbar = self.toolbar.add_toolbar("Animation")
            self.anim_button = anim_toolbar.add_toggle_button("Animation", on=self.start_animating, off=self.stop_animating, on_prefix="Run", off_prefix="Stop")
            anim_toolbar.add_button(["Step Forward"], action=self.step_forward)
            anim_toolbar.add_button(["Step Backward"], action=self.step_back)
            def get_frame():
                return self.canvas.animate.frame_num
            anim_toolbar.add_slider_button(get_frame, 0, self.canvas.animate.number_of_frames(), "Time Slider", update=self.set_animation_frame)
            anim_toolbar.add_slider_button(self.animation_speed, 1, 100, "Speed (Step Delay)", update=self.set_animation_speed)
            self.save_anim_button = anim_toolbar.add_button(["Save Animation", "Cancel Save"], action=self.save_animation_press)
            self.initialized = True

    def step_forward(self, state):
        if self.animation_timer is not None:
            self.stop_animating()
        self.canvas.animate.draw_frame((self.canvas.animate.frame_num + 1) % self.canvas.animate.number_of_frames(), allow_static = False, render_offscreen = False)

    def step_back(self, state):
        if self.animation_timer is not None:
            self.stop_animating()
        self.canvas.animate.draw_frame((self.canvas.animate.frame_num - 1) % self.canvas.animate.number_of_frames(), allow_static = False, render_offscreen = False)

    def save_animation_press(self, state):
        if state == 1:
            if self.animation_timer:
                self.stop_animating()
                self.anim_button.set_state(0)
            self.save_listener = self.interactor.AddObserver("TimerEvent", self.save_tick)
            self.save_timer = self.interactor.CreateRepeatingTimer(10)
        else:
            if self.save_timer:
                self.interactor.DestroyTimer(self.save_timer)
                self.save_timer = None
            if self.save_listener:
                self.interactor.RemoveObserver(self.save_listener)
                self.save_listener = None
            self.canvas.animate.draw_frame(allow_static=False, render_offscreen=False)


    def save_animation(self):
        # Save the animation
        self.canvas.animate.fps(int(1000.0 / self.animation_speed))

        data_titles = {}
        name_to_type = {}
        # Iterate the displays and create a name for the animation
        for display in self.displays:
            name_to_type[display.g_name] = display.g_type

            for array in display.array:
                if array is not None:
                    if display.g_name not in data_titles:
                        data_titles[display.g_name] = []
                    data_titles[display.g_name].append("_".join(array.title.split()))

        plot_names = [name_to_type[d_name]+ "_" + "-".join(data_titles[d_name]) for d_name in data_titles]
        name = "__".join(plot_names) + ".mp4"
        save_path = self.get_save_path(name)
        if save_path == '':
            return

        self.canvas.animate.save(save_path)
        self.canvas.animate.draw_frame(allow_static=False, render_offscreen=False)
        self.save_anim_button.set_state(0)


    def save_tick(self, obj, event):
        if self.save_timer is None or self.save_listener is None:
            return

        if self.canvas.animate.number_of_frames() == len(self.canvas.animate.animation_files):
            self.save_animation()
            if self.save_timer:
                self.interactor.DestroyTimer(self.save_timer)
                self.save_timer = None
            if self.save_listener:
                self.interactor.RemoveObserver(self.save_listener)
                self.save_listener = None
        else:
            self.canvas.animate.draw_frame((self.canvas.animate.frame_num + 1) % self.canvas.animate.number_of_frames())


    def set_animation_speed(self, value):
        v = int(value)
        self.animation_speed = 10 * v
        if self.animation_timer is not None:
            self.interactor.DestroyTimer(self.animation_timer)
            self.animation_timer = self.interactor.CreateRepeatingTimer(self.animation_speed)
        return v

    def animate(self, obj, event):
        if self.animation_timer is not None and datetime.datetime.now() - self.animation_last_frame_time > datetime.timedelta(0, 0, 0, self.animation_speed):
            self.animation_last_frame_time = datetime.datetime.now()
            self.canvas.animate.draw_frame((self.canvas.animate.frame_num + 1) % self.canvas.animate.number_of_frames(), render_offscreen=False, allow_static=False)

    def start_animating(self):
        if self.animation_timer is None:
            self.animation_timer = self.interactor.CreateRepeatingTimer(self.animation_speed)

    def stop_animating(self):
        if self.animation_timer is not None:
            t, self.animation_timer = self.animation_timer, None
            self.interactor.DestroyTimer(t)
            self.anim_button.set_state(0)

    def set_animation_frame(self, value):
        if self.animation_timer is not None:
            self.stop_animating()
        value = int(value)
        self.canvas.animate.draw_frame(value, allow_static=False, render_offscreen=False)
        return value

    def set_background_red(self, value):
        _, g, b = self.canvas.backgroundcolor
        self.canvas.backgroundcolor = int(value), g, b

        self.changed = True
        self.save()
        # Returning a value will update the slider to that value
        return int(value)

    def set_background_green(self, value):
        r, _, b = self.canvas.backgroundcolor
        self.canvas.backgroundcolor = r, int(value), b

        self.changed = True
        self.save()
        # Returning a value will update the slider to that value
        return int(value)

    def set_background_blue(self, value):
        r, g, _ = self.canvas.backgroundcolor
        self.canvas.backgroundcolor = r, g, int(value)

        self.changed = True
        self.save()
        # Returning a value will update the slider to that value
        return int(value)

    def init_buttons(self):
        # An "off" and "on" state
        states = [vtk_ui.button.ButtonState(bgcolor=x) for x in ((.5, .5, .5), (.75, .75, .75))]

        prop = vtk.vtkTextProperty()
        prop.SetBackgroundColor(.87, .79, .55)
        prop.SetBackgroundOpacity(1)
        prop.SetColor(0, 0, 0)

        #self.fill_button = vtk_ui.button.Button(self.interactor, states=states, image=os.path.join(sys.prefix,"share","vcs","fill_icon.png"), top=10, left=10, halign=vtk_ui.button.RIGHT_ALIGN, action=self.fill_click)
        self.text_button = vtk_ui.button.Button(self.interactor, states=states, image=os.path.join(sys.prefix, "share", "vcs", "text_icon.png"), top=10, left=10, halign=vtk_ui.button.RIGHT_ALIGN, action=self.text_click, tooltip="Create Text: click to place.", tooltip_property=prop)

        #self.line_button = vtk_ui.button.Button(self.interactor, states=states, image=os.path.join(sys.prefix, "share", "vcs", "line_icon.png"), top=10, left=116, halign=vtk_ui.button.RIGHT_ALIGN, action=self.line_click)

        self.marker_button = vtk_ui.button.Button(self.interactor, states=states, image=os.path.join(sys.prefix, "share", "vcs", "marker_icon.png"), top=10, left=63, halign=vtk_ui.button.RIGHT_ALIGN, action=self.marker_click, tooltip="Create Marker: click to place.", tooltip_property=prop)


    def creator_enabled(self, button):
        if self.target is not None:
            self.deactivate(self.target)

        buttons = [self.text_button, self.marker_button]
        buttons.remove(button)

        if self.creating:
            for button in buttons:
                button.set_state(0)

        self.click_locations = []

        if button == self.fill_button:
            self.creating = CREATING_FILL
        elif button == self.text_button:
            self.creating = CREATING_TEXT
        elif button == self.line_button:
            self.creating = CREATING_LINE
        elif button == self.marker_button:
            self.creating = CREATING_MARKER

    def creator_disabled(self, button):
        if self.target is not None:
            self.deactivate(self.target)

        self.click_locations = None
        self.creating = None

    def marker_click(self, index):
        if index == 1:
            self.creator_enabled(self.marker_button)
        else:
            self.creator_disabled(self.marker_button)

    def line_click(self, index):
        if index == 1:
            self.creator_enabled(self.line_button)
        else:
            self.creator_disabled(self.line_button)

    def text_click(self, index):
        if index == 1:
            self.creator_enabled(self.text_button)
        else:
            self.creator_disabled(self.text_button)

    def fill_click(self, index):
        if index == 1:
            self.creator_enabled(self.fill_button)
        else:
            self.creator_disabled(self.fill_button)

    def create(self):
        w, h = self.interactor.GetRenderWindow().GetSize()

        x = []
        y = []

        for point in self.click_locations:
            x.append(point[0] / float(w))
            y.append(point[1] / float(h))

        created = None

        if self.creating == CREATING_FILL:
            fill = self.canvas.createfillarea()
            fill.x = x
            fill.y = y
            created = fill
            self.fill_button.set_state(0)
            dp = self.canvas.fillarea(fill)
        elif self.creating == CREATING_TEXT:
            t = self.canvas.createtextcombined()
            t.x = x
            t.y = y
            t.string = ["Click to Edit"]
            created = t
            self.text_button.set_state(0)
            dp = self.canvas.text(t)
        elif self.creating == CREATING_MARKER:
            m = self.canvas.createmarker()
            m.x = x
            m.y = y
            m.size = [10]
            created = m
            self.marker_button.set_state(0)
            dp = self.canvas.marker(m)
        elif self.creating == CREATING_LINE:
            l = self.canvas.createline()
            l.x = x
            l.y = y
            created = l
            self.line_button.set_state(0)
            dp = self.canvas.line(l)

        # Activate an editor for the new object
        self.clicked_info = 0
        self.activate(created, dp)

        self.creating = False
        self.click_locations = None


def t(name):
    return vcs.gettemplate(name)

def is_label(obj):
    return type(obj) in (vcs.Pformat.Pf, vcs.Ptext.Pt)

def in_template(point, template, dp, window_size, fudge=None):
    x, y = point

    attrs = [
        "file",
        "function",
        "logicalmask",
        "transformation",
        "source",
        "dataname",
        "title",
        "units",
        "crdate",
        "crtime",
        "comment1",
        "comment2",
        "comment3",
        "comment4",
        "xname",
        "yname",
        "zname",
        "tname",
        "xunits",
        "yunits",
        "zunits",
        "tunits",
        "xvalue",
        "yvalue",
        "zvalue",
        "tvalue",
        "mean",
        "min",
        "max",
        "xtic1",
        "xtic2",
        "xmintic1",
        "xmintic2",
        "ytic1",
        "ytic2",
        "ymintic1",
        "ymintic2",
        "xlabel1",
        "xlabel2",
        "ylabel1",
        "ylabel2",
        "box1",
        "box2",
        "box3",
        "box4",
        "line1",
        "line2",
        "line3",
        "line4",
        "legend",
        "data",
    ]

    intersecting = None

    for attr in attrs:
        attribute = getattr(template, attr)
        if attribute.priority == 0 or (intersecting is not None and intersecting.priority > attribute.priority):
            # 0 is turned off
            continue
        t_x = safe_get(attribute, "x")
        t_y = safe_get(attribute, "y")

        if t_x is not None or t_y is not None:
            if t_x is not None and t_y is not None:
                # It's probably a text blob
                if is_label(attribute):
                    text = editors.label.get_label_text(attribute, dp)
                    if text == '':
                        continue
                    if editors.label.inside_label(attribute, text, x, y, *window_size):
                        intersecting = attribute
                elif is_point_in_box((x, y), ((t_x - fudge, t_y - fudge), (t_x + fudge, t_y + fudge))):
                    intersecting = attribute
            else:
                pass
                """ Uncomment to enable axis editors
                # It's probably the labels for an axis
                if t_x is not None and t_x < x + fudge and t_x > x - fudge:
                    intersecting = attribute
                elif t_y is not None and t_y < y + fudge and t_y > y - fudge:
                    intersecting = attribute
                """
        else:
            pass
            """ Uncomment to reenable the box editors
            x1 = safe_get(attribute, "x1")
            y1 = safe_get(attribute, "y1")
            x2 = safe_get(attribute, "x2")
            y2 = safe_get(attribute, "y2")

            if None in (x1, y1, x2, y2):
                continue
            else:
                if is_point_in_box((x, y), ((min(x1, x2), min(y1, y2)), (max(x1, x2), max(y1, y2)))):
                    intersecting = attribute
            """
    return intersecting

def is_box(member):
    x1 = safe_get(member, "x1")
    y1 = safe_get(member, "y1")
    x2 = safe_get(member, "x2")
    y2 = safe_get(member, "y2")

    if None in (x1, y1, x2, y2):
        return False
    else:
        return True

def is_point(member):
    x = safe_get(member, "x")
    y = safe_get(member, "y")
    if None in (x, y):
        return False
    else:
        return True

def is_point_in_box(point, box):
    """
    Box should be provided as ((xmin, ymin), (xmax, ymax))
    """
    x, y = point
    (x1, y1), (x2, y2) = box
    return x1 <= x and x2 >= x and y1 <= y and y2 >= y

def safe_get(obj, attr, sentinel=None):
    """
    Returns sentinel value if attr isn't in obj, otherwise attr's value
    """
    try:
        return getattr(obj, attr)
    except AttributeError:
        return sentinel
