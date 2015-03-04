from resize_box import ResizeBox
from handle import Handle

class ConfigTypes(object):
    LIST, BOOL, FLOAT, BOX = range(4)

class ConfigOption(object):
    def __init__(self, name, setter, getter):
        self.setter = setter
        self.getter = getter
        self.name = name

    def __call__(self, *args):
        if len(args) == 0:
            return self.getter()
        else:
            self.setter(*args)

    def add_to_toolbar(self, toolbar):
        toolbar.add_button(self.name, self)

class BoxOption(ConfigOption):
    def __init__(self, name, setter, getter, normalize=False):
        self.resizer = None
        self.normalize = normalize
        super(BoxOption, self).__init__(name, setter, getter)

    def on(self):
        point1, point2 = self()

        self.resizer.x1, self.resizer.y1 = point1
        self.resizer.x2, self.resizer.y2 = point2

        self.resizer.show()

    def off(self):
        point1, point2 = self.resizer.get_points()
        self(point1, point2)
        self.resizer.hide()

    def add_to_toolbar(self, toolbar):
        if self.resizer is not None:
            self.resizer.hide()

        points = self()
        self.resizer = ResizeBox(toolbar.interactor, points[0], points[1], normalize=self.normalize)
        toolbar.add_toggle_button(self.name, on=self.on, off=self.off, on_prefix="Transform", off_prefix="Save")

class PointOption(ConfigOption):
    def __init__(self, name, setter, getter, normalize=False):
        super(PointOption, self).__init__(name, setter, getter)
        self.handle = None
        self.normalize = normalize

    def on(self):
        x, y = self()
        self.handle.x = x
        self.handle.y = y
        self.handle.show()

    def off(self):
        self((self.handle.x, self.handle.y))
        self.handle.hide()

    def add_to_toolbar(self, toolbar):
        if self.handle is not None:
            self.handle.hide()

        point = self()
        self.handle = Handle(toolbar.interactor, point, color=(0,0,0), normalize=self.normalize)
        toolbar.add_toggle_button(self.name, on=self.on, off=self.off, on_prefix="Move", off_prefix="Save")

class BoolOption(ConfigOption):
    def __init__(self, name, setter, getter):
        super(BoolOption, self).__init__(name, setter, getter)

    def on(self):
        self(True)

    def off(self):
        self(False)

    def add_to_toolbar(self, toolbar):
        toolbar.add_toggle_button(self.name, on=self.on, off=self.off)
        if self():
            toolbar.widgets[-1].set_state(1)

class ListOption(ConfigOption):
    def __init__(self, name, setter, getter, values=None):
        self.values = values
        super(ListOption, self).__init__(name, setter, getter)

    def add_to_toolbar(self, toolbar):
        toolbar.add_button(self.values, action=self)
        toolbar.widgets[-1].set_state(self.values.index(self()))

    def __call__(self, *args):
        if len(args):
            self.setter(self.values[args[0]])
        else:
            return self.getter()

class FloatOption(ConfigOption):
    def __init__(self, name, setter, getter, finalize=None, min_val=0, max_val=1, value=0):
        self.finalize = finalize
        self.min = min_val
        self.max = max_val
        super(FloatOption, self).__init__(name, setter, getter)

    def add_to_toolbar(self, toolbar):
        toolbar.add_slider_button(self(), self.min, self.max, self.name, update = self, end = self.finalize)
        toolbar.widgets[-1].set_value(self())

from toolbar import Toolbar

class AutoConfToolbar(Toolbar):
    def __init__(self, interactor, name, *configs, **kwargs):
        super(AutoConfToolbar, self).__init__(interactor, name, **kwargs)
        for config in configs:
            config.add_to_toolbar(self)
        self.config_options = configs