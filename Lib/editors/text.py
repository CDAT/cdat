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
        self.old_priority = text.priority
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

            textbox = Textbox(self.interactor, string, left=int(x * w), top=int( h * (1 - y) - text_height / 2.0 ), action=self.textbox_clicked, textproperty=prop)
            textbox.show()
            self.textboxes.append(textbox)

    def in_bounds(self, x, y):
        return inside_text(self.text, x, y, *self.interactor.GetRenderWindow().GetSize(), index=self.index) is not None

    def click_release(self):

        x, y = self.event_position()

        text_index = inside_text(self.text, x, y, *self.interactor.GetRenderWindow().GetSize())

        self.handle_click(text_index, x, y)

    def handle_click(self, text_index, x, y):


        if text_index == self.index:
            # Adjust cursor position
            self.textboxes[self.index].start_editing((x, y))
            return
        else:
            self.textboxes[self.index].stop_editing()
            self.text.string[self.index] = self.textboxes[self.index].text
            if text_index is None:
                self.deactivate()
            else:
                # Change which one we're editing
                self.index = text_index
                self.textboxes[self.index].start_editing((x, y))


    def textbox_clicked(self, point):
        x, y = point
        winsize = self.interactor.GetRenderWindow().GetSize()
        clicked_on = inside_text(self.text, x, y, *winsize)
        print clicked_on
        self.handle_click(clicked_on, x, y)

    def save(self):
        self.configurator.save()

    def deactivate(self):
        self.text.priority = self.old_priority
        self.configurator.deactivate(self)

    def detach(self):
        self.unregister()
        for box in self.textboxes:
            box.detach()
        del self.textboxes

def text_dimensions(text, index, winsize):
    prop = vtkTextProperty()
    vcs.vcs2vtk.prepTextProperty(prop, winsize, text, text, vcs.getcolormap())
    return vcs.vtk_ui.text.text_dimensions(text.string[index], prop)

def inside_text(text, x, y, screen_width, screen_height, index=None):
    winsize = (screen_width, screen_height)
    if x > 1:
        x = x / float(screen_width)
    if y > 1:
        y = y / float(screen_height)

    for ind, xcoord in enumerate(text.x):
        if index is not None:
            if ind != index:
                continue

        ycoord = text.y[ind]
        text_width, text_height = text_dimensions(text, ind, winsize)
        text_width = text_width / float(screen_width)
        text_height = text_height / float(screen_height)
        # ycoord is the middle, vertically, so let's adjust for that
        ycoord -= text_height / 2.0
        # xcoord is the left side
        if x > xcoord and x < xcoord + text_width and y < ycoord + text_height and y > ycoord:
            return ind

    return None