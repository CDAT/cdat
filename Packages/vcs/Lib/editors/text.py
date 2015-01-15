from vcs.vtk_ui import Textbox
import vcs.vtk_ui.text
from vtk import vtkTextProperty
from vcs.vtk_ui.behaviors import DraggableMixin, ClickableMixin
import vcs

class TextEditor(ClickableMixin, DraggableMixin):
    def __init__(self, interactor, text, index, configurator):

        self.interactor = interactor
        self.text = text
        self.index = index
        self.configurator = configurator

        # We're going to insert textboxes at each of the locations
        # That way we can edit the text.
        text.priority = 0
        configurator.save()
        self.textboxes = None

        super(TextEditor, self).__init__()
        self.register()
        self.update()

    def update(self):
        if self.textboxes:
            for box in self.textboxes:
                box.detach()
            del self.textboxes
        self.textboxes = []
        w, h = self.interactor.GetRenderWindow().GetSize()
        cmap = vcs.getcolormap()
        for ind, x in enumerate(self.text.x):
            y = self.text.y[ind]
            string = self.text.string[ind]

            prop = vtkTextProperty()
            vcs.vcs2vtk.prepTextProperty(prop, (w, h), self.text, self.text, cmap)

            text_width, text_height = text_dimensions(self.text, ind, (w, h))

            textbox = Textbox(self.interactor, string, left=int(x * w), top=int( h * (1 - y) - text_height / 2.0 ), textproperty=prop)
            textbox.show()
            self.textboxes.append(textbox)

    def in_bounds(self, x, y):
        return inside_text(self.text, x, y, *self.interactor.GetRenderWindow().GetSize(), index=self.index) is not None

    def detach(self):
        self.unregister()

def text_dimensions(text, index, winsize):
    prop = vtkTextProperty()
    vcs.vcs2vtk.prepTextProperty(prop, winsize, text, text, vcs.getcolormap())
    return vcs.vtk_ui.text.text_dimensions(text.string[index], prop)

def inside_text(text, x, y, screen_width, screen_height, index=None):
    winsize = (screen_width, screen_height)

    for ind, xcoord in enumerate(text.x):
        if index is not None:
            if ind != index:
                continue

        # xcoord is the left side
        # ycoord is the middle, vertically
        ycoord = text.y[ind]

        text_width, text_height = text_dimensions(text, ind, winsize)

        if x > xcoord and x < xcoord + text_width and y < ycoord + text_height / 2.0 and y > ycoord - text_height / 2.0:
            return ind

    return None