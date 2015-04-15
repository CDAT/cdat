import vtk
# Support multiple render windows
ui_managers = {}

class InterfaceManager(object):
    """
    Provides a full-window renderer for UI widgets

    Keeps UI Widgets in proper positions
    """
    def __init__(self, interactor):
        self.interactor = interactor
        self.window = interactor.GetRenderWindow()
        self.renderer = vtk.vtkRenderer()
        self.renderer.SetViewport(0, 0, 1, 1)
        self.renderer.SetBackground(1, 1, 1)

        # Used to overlay actors above widgets
        self.actor_renderer = vtk.vtkRenderer()
        self.actor_renderer.SetViewport(0, 0, 1, 1)
        self.actor_renderer.SetBackground(1, 1, 1)

        self.window.AddRenderer(self.renderer)
        self.window.AddRenderer(self.actor_renderer)

        self.widgets = []
        self.timer_listener = self.interactor.AddObserver("TimerEvent", self.__render)
        self.window_mod = self.window.AddObserver("ModifiedEvent", self.__place, 30)
        self.render_listener = self.window.AddObserver("RenderEvent", self.__rendered)
        self.last_size = None
        self.timer = None

    def __rendered(self, obj, event):
        if self.timer is not None:
            self.interactor.DestroyTimer(self.timer)
            self.timer = None

    def __place(self, obj, event):
        size = self.window.GetSize()
        if size == self.last_size:
            return
        self.last_size = size
        for widget in self.widgets:
            if widget.showing() == 1:
                widget.place()

    def __render(self, obj, event):
        if self.timer is not None:
            self.timer = None
            self.window.Render()

    def queue_render(self):
        if not self.interactor.GetInitialized():
            self.timer = 1
            self.__render(None, None)
            return

        if self.timer is None:
            # approximately one frame at 60 fps
            self.timer = self.interactor.CreateOneShotTimer(16)

    def elevate(self):
        # Raise to top layer of render window
        layer = self.window.GetNumberOfLayers()

        if layer > 1 and self.window.HasRenderer(self.renderer) and self.renderer.GetLayer() == layer - 1:
            # We don't need to mess with anything and send out ModifiedEvents on the render window if
            # we're already at the top layer.
            return

        self.window.SetNumberOfLayers(layer + 2)

        # To get the layer to change appropriately, have to remove first.
        if self.window.HasRenderer(self.renderer):
            self.window.RemoveRenderer(self.renderer)
        if self.window.HasRenderer(self.actor_renderer):
            self.window.RemoveRenderer(self.actor_renderer)

        self.renderer.SetLayer(layer)
        self.actor_renderer.SetLayer(layer + 1)
        self.window.AddRenderer(self.renderer)
        self.window.AddRenderer(self.actor_renderer)


    def add_widget(self, widget):
        self.widgets.append(widget)
        # This is a weird VTK behavior; if you set renderer on the representation,
        # it gets overriden in widget.SetEnabled(); it uses the coordinate of the
        # last UI event to determine what renderer to use, and then sets CurrentRenderer
        # to that renderer, which it will use in the future to populate the repr.Renderer
        # Setting the CurrentRenderer short circuits that logic, and just assigns the correct
        # renderer to the object.
        widget.widget.SetCurrentRenderer(self.renderer)

    def remove_widget(self, widget):
        if widget in self.widgets:
            self.widgets.remove(widget)
            if len(self.widgets) == 0:
                del ui_managers[self.interactor]
                self.detach()

    def detach(self):
        for w in self.widgets:
            w.detach()
        if self.window.HasRenderer(self.renderer):
            self.window.RemoveRenderer(self.renderer)

        if self.window.HasRenderer(self.actor_renderer):
            self.window.RemoveRenderer(self.actor_renderer)

        self.renderer.RemoveAllViewProps()
        self.actor_renderer.RemoveAllViewProps()

        self.renderer = None
        self.actor_renderer = None

        self.interactor.RemoveObserver(self.timer_listener)
        self.window.RemoveObserver(self.window_mod)
        self.window.RemoveObserver(self.render_listener)

def delete_manager(inter):
    if inter is None:
        return None

    manager = ui_managers.get(inter, None)
    if manager is not None and len(manager.widgets) == 0:
        manager.detach()
        del ui_managers[inter]

def manager_exists(inter):
    if inter is None:
        return False
    return inter in ui_managers

def get_manager(inter):

    if inter is None:
        return None

    if ui_managers.get(inter, None) is None:
        ui_managers[inter] = InterfaceManager(inter)

    return ui_managers[inter]