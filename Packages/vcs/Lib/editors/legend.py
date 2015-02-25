from box import BoxEditor
from vcs.vtk_ui import Toolbar
import vcs

class LegendEditor(BoxEditor):
    def __init__(self, interactor, template, configurator):
        super(LegendEditor, self).__init__(interactor, template.legend, configurator)
        # Legend doesn't need anything too exciting, just need to add a toolbar button
        self.toolbar = Toolbar(interactor, "Legend Options")
        self.toolbar.show()

        maps = vcs.elements["colormap"]
        self.maps = maps.keys()
        colormaps = []

        for ind, cm in enumerate(self.maps):
            colormaps.append("Colormap: %s" % cm)

        self.toolbar.add_button(colormaps, action=self.change_map)

    def place(self):
        super(LegendEditor, self).place()
        self.toolbar.place()

    def change_map(self, state):
        self.configurator.canvas.setcolormap(self.maps[state])

    def detach(self):
        self.toolbar.detach()
        super(LegendEditor, self).detach()

    def handle_click(self, point):
        x, y = point
        return self.in_bounds(x, y) or self.toolbar.in_toolbar(x, y)
