import vtk
import vcs
from vcs.vtk_ui import Button, ButtonState, NoInteractionStyle

class ColorPicker(object):
    def __init__(self, width, height, colormap, color, on_save=None, on_cancel=None):
        self.render_window = vtk.vtkRenderWindow()
        self.render_window.SetWindowName("Color Picker")
        self.render_window.SetNumberOfLayers(3)
        self.render_window.SetSize(width, height)
        self.color_renderer = vtk.vtkRenderer()

        if colormap is None:
            colormap = "default"
        if not vcs.iscolormap(colormap):
            colormap = vcs.getcolormap(colormap)

        self.colormap = colormap
        self.colors =  [[int(c / 100.0 * 255.0) for c in colormap.index[i]] for i in range(len(colormap.index))]

        self.actor = make_color_plane(16, 16, self.colors)

        self.color_renderer.SetViewport([.1, .1, .9, .9])
        self.color_renderer.SetBackground((.5, .5, .5))
        self.color_renderer.AddActor(self.actor)
        self.color_renderer.InteractiveOff()
        self.color_renderer.SetLayer(1)

        bg = vtk.vtkRenderer()
        bg.SetBackground(1,1,1)
        bg.SetViewport([0,0,1,1])
        bg.SetLayer(0)
        self.render_window.AddRenderer(bg)
        self.render_window.AddRenderer(self.color_renderer)
        inter = vtk.vtkRenderWindowInteractor()

        self.style = NoInteractionStyle()
        inter.SetInteractorStyle(self.style)
        inter.SetRenderWindow(self.render_window)

        self.render_window.Render()

        self.on_save = on_save
        self.on_cancel = on_cancel

        maps = vcs.elements["colormap"]

        current_state = None
        states = []
        self.colormaps = []
        for ind, mapname in enumerate(maps.keys()):
            states.append(ButtonState(label=mapname))
            if colormap.name == mapname:
                current_state = ind
            self.colormaps.append(maps[mapname])

        self.colormap_button = Button(inter, states=states, action=self.change_map, left = 10, top=10)
        self.colormap_button.set_state(current_state)

        self.save_button = Button(inter, action=self.save, label="Choose Color", left=int(width * .75) - 10, top=int(height * .85))
        self.cancel_button = Button(inter, action=self.cancel, label="Cancel", left=10, top=int(height * .85))

        self.colormap_button.show()
        self.save_button.show()
        self.cancel_button.show()

        self.render_window.MakeCurrent()

        self.selectedMapper = vtk.vtkDataSetMapper()
        self.selectedActor = vtk.vtkActor()
        self.selectedActor.SetMapper(self.selectedMapper);
        self.selectedActor.GetProperty().EdgeVisibilityOn();
        self.selectedActor.GetProperty().SetEdgeColor(0,0,0);
        self.selectedActor.GetProperty().SetLineWidth(3);
        self.color = color
        # Make sure the current color is selected
        self.selectCell(color)
        self.color_renderer.AddActor(self.selectedActor)
        self.click_handler = inter.AddObserver("LeftButtonPressEvent", self.leftButtonPressEvent)

        inter.Start()

    def make_current(self):
        self.render_window.MakeCurrent()

    def topRendererAtPoint(self, x, y):
        inter = self.render_window.GetInteractor()

        top = None

        for renderer in collection(inter.GetRenderWindow().GetRenderers()):
            if renderer.IsInViewport(x, y):
                if top is None or top.GetLayer() <= renderer.GetLayer():
                    top = renderer
        return top

    def change_map(self, state):
        self.colormap = self.colormaps[state]
        self.colors =  [[int(c / 100.0 * 255.0) for c in self.colormap.index[i]] for i in range(len(self.colormap.index))]
        colorData = colors_to_scalars(self.colors)
        self.actor.GetMapper().GetInput().GetCellData().SetScalars(colorData)
        self.actor.GetMapper().Update()
        self.render_window.Render()

    def save(self, state):
        if self.on_save is not None:
            self.on_save(self.colormap, self.color)

        self.close()

    def cancel(self, state):
        if self.on_cancel is not None:
            self.on_cancel()

        self.close()

    def selectCell(self, cellId):
        if cellId == -1:
            return
        ids = vtk.vtkIdTypeArray();
        ids.SetNumberOfComponents(1);
        ids.InsertNextValue(cellId);

        selectionNode = vtk.vtkSelectionNode();
        selectionNode.SetFieldType(vtk.vtkSelectionNode.CELL);
        selectionNode.SetContentType(vtk.vtkSelectionNode.INDICES);
        selectionNode.SetSelectionList(ids);

        selection = vtk.vtkSelection();
        selection.AddNode(selectionNode);

        extractSelection = vtk.vtkExtractSelection();

        extractSelection.SetInputData(0, self.actor.GetMapper().GetInput());
        extractSelection.SetInputData(1, selection);

        extractSelection.Update();

        selected = vtk.vtkUnstructuredGrid();
        selected.ShallowCopy(extractSelection.GetOutput());

        self.selectedMapper.SetInputData(selected);
        self.selectedMapper.Update()

    def leftButtonPressEvent(self, obj, event):
        inter = self.render_window.GetInteractor()

        x, y = inter.GetEventPosition()

        renderer = self.topRendererAtPoint(x, y)

        if renderer:
            picker = vtk.vtkCellPicker()
            picker.SetTolerance(.0005)
            picker.Pick(x, y, 0, renderer)

            cell = picker.GetCellId()
            self.selectCell(cell)
            if cell >= 0:
                self.color = int(cell)

            self.render_window.Render()


    def close(self):
        self.save_button.detach()
        self.cancel_button.detach()
        self.colormap_button.detach()
        self.render_window.RemoveObserver(self.click_handler)
        self.render_window.Finalize()
        inter = self.render_window.GetInteractor()
        inter.SetRenderWindow(None)
        self.render_window.SetInteractor(None)
        del inter
        del self.render_window


def make_color_plane(x, y, colors):
    plane = vtk.vtkPlaneSource()
    plane.SetXResolution(x)
    plane.SetYResolution(y)

    plane.Update()
    colorData = colors_to_scalars(colors)

    plane.GetOutput().GetCellData().SetScalars(colorData)

    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInputConnection(plane.GetOutputPort())
    mapper.SetScalarModeToUseCellData()
    mapper.Update()

    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
    return actor

def colors_to_scalars(colors):
    colorData = vtk.vtkUnsignedCharArray()
    colorData.SetName("colors")
    colorData.SetNumberOfComponents(3)

    for color in colors:
        colorData.InsertNextTuple3(*color)

    return colorData

def collection(collection):
    collection.InitTraversal()
    item = collection.GetNextItemAsObject()
    while item is not None:
        yield item
        item = collection.GetNextItemAsObject()