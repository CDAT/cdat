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
        widget.repr.SetRenderer(self.renderer)
        self.widgets.append(widget)

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
        print ""

    return ui_managers[inter]