from box import BoxEditor
from vcs.vtk_ui import Toolbar
import vcs

class DataEditor(BoxEditor):
    """
    Editor for the "data" attribute of a template

    Does everything BoxEditor does, plus a config toolbar for data properties.
    """
    def __init__(self, interactor, gm, template, configurator):
        super(DataEditor, self).__init__(interactor, template.data, configurator)
        self.gm = gm
        # Legend doesn't need anything too exciting, just need to add a toolbar button
        self.toolbar = Toolbar(interactor, "Data Options")
        self.toolbar.show()

        self.projections = vcs.elements["projection"].keys()

        proj_button = self.toolbar.add_button(self.projections, action=self.change_projection)
        proj_button.set_state(self.projections.index(gm.projection))

    def place(self):
        super(DataEditor, self).place()
        self.toolbar.place()

    def change_projection(self, state):
        self.gm.projection = self.projections[state]
        self.save()

    def detach(self):
        self.toolbar.detach()
        super(DataEditor, self).detach()

    def handle_click(self, point):
        handled = super(DataEditor, self).handle_click(point)
        x, y = point
        return handled or self.toolbar.in_toolbar(x, y)