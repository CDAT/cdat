from vcs import vtk_ui

class FillEditor(object):
    def __init__(self, interactor, fillarea, index, configurator):
        points = zip(fillarea.x[index], fillarea.y[index])
        self.index = index
        self.fill = fillarea
        self.interactor = interactor
        self.handles = []
        self.configurator = configurator
        for point in points:
            h = vtk_ui.Handle(interactor, point, released=self.adjust, color=(0,0,0), normalize=True)
            h.show()
            self.handles.append(h)

    def adjust(self, handle):
        ind = self.handles.index(handle)
        self.fill.x[self.index][ind], self.fill.y[self.index][ind] = handle.x, handle.y
        self.save()

    def save(self):
        self.configurator.save()
        for h in self.handles:
            h.show()
