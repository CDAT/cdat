import vtk

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
        self.window.AddRenderer(self.renderer)
        self.widgets = []

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

        # Remove widget from renderer
        widget.detach()


# Support multiple render windows
ui_managers = {}

def get_manager(inter):

    if inter is None:
        return None

    if ui_managers.get(inter, None) is None:
        ui_managers[inter] = InterfaceManager(inter)

    return ui_managers[inter]