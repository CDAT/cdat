from vtk import vtkHandleWidget, vtkPointHandleRepresentation2D
from widget import Widget

class Handle(Widget):
    def __init__(self, interactor, point, width=10, height=10, opacity=1, color=(0, 0, 0), clicked=None, dragged=None, released=None, normalize=False):
        
        self.x, self.y = point
        self.color = color
        self.clicked = clicked
        self.dragged = dragged
        self.released = released
        self.normalize = normalize
        widget = vtkHandleWidget()
        
        widget.SetRepresentation(vtkPointHandleRepresentation2D())

        super(Handle, self).__init__(interactor, widget)
        
        self.repr.SetCursorShape(quad_poly_data(width, height))
        
        properties = self.repr.GetProperty()
        properties.SetColor(*color)
        properties.SetOpacity(opacity)

        self.repr.SetHandleSize(10)

        properties = self.repr.GetSelectedProperty()
        properties.SetColor(*color)
        properties.SetOpacity(.5 * opacity)

        self.widget.SetRepresentation(self.repr)
        self.place()

        self.subscribe("StartInteractionEvent", self.click)
        """
        vtkHandleWidget supports these events:
            vtkCommand::StartInteractionEvent (on vtkWidgetEvent::Select)
            vtkCommand::EndInteractionEvent (on vtkWidgetEvent::EndSelect)
            vtkCommand::InteractionEvent (on vtkWidgetEvent::Move)
        """

    def show(self):
        self.place()
        self.widget.On()

    def hide(self):
        self.widget.Off()
    
    def __get_position__(self):
        if self.normalize:
            w, h = self.interactor.GetRenderWindow().GetSize()
        else:
            w, h = 1, 1

        return self.x * float(w), self.y * float(h)


    def place(self):
        x, y = self.__get_position__()
        
        if self.normalize:
            w, h = self.interactor.GetRenderWindow().GetSize()
        else:
            w, h = 1, 1

        self.repr.SetDisplayPosition((int(x), int(y), 0))

    def click(self, object, event):
        
        self.subscribe("EndInteractionEvent", self.release)
        self.subscribe("InteractionEvent", self.drag)

        if self.clicked:
            self.clicked(self)

    def release(self, object, event):
        
        self.unsubscribe("EndInteractionEvent", "InteractionEvent")

        if self.released:
            self.released(self)

    def drag(self, object, event):
        

        if self.normalize:
            w, h = self.interactor.GetRenderWindow().GetSize()
        else:
            w, h = 1, 1
        self.x, self.y, _ = self.repr.GetDisplayPosition()
        
        self.x = self.x / float(w)
        self.y = self.y / float(h)

        if self.dragged:
            # Need to get the point that we're dragged to here
            self.dragged(self, self.x, self.y)

def quad_poly_data(width, height):
    from vtk import vtkPoints, vtkQuad, vtkCellArray, vtkPolyData
    
    points = vtkPoints()
    points.InsertNextPoint((-1 * int(width / 2.0), int(height / 2.0), 0))
    points.InsertNextPoint((int(width / 2.0), int(height / 2.0), 0))
    points.InsertNextPoint((int(width / 2.0), -1 * int(height / 2.0), 0))
    points.InsertNextPoint((-1 * int(width / 2.0), -1 * int(height / 2.0), 0))
    
    quad = vtkQuad()
    quad.GetPointIds().SetId(0, 0)
    quad.GetPointIds().SetId(1, 1)
    quad.GetPointIds().SetId(2, 2)
    quad.GetPointIds().SetId(3, 3)

    arr = vtkCellArray()
    arr.InsertNextCell(quad)
    
    pd = vtkPolyData()
    pd.SetPoints(points)
    pd.SetPolys(arr)

    return pd
