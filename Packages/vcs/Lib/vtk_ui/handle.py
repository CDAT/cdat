from vtk import vtkHandleWidget, vtkPointHandleRepresentation2D
from widget import Widget


__handles_to_render__ = {}

def __schedule_render__(interactor, handle):
    """
    Batch renders handles so they all move in a group together.
    """
    listener = None
    timer = None

    def render(obj, event):
        for handle in __handles_to_render__[interactor]:
            handle.widget.Render()

        interactor.RemoveObserver(listener)
        interactor.DestroyTimer(timer)
        del __handles_to_render__[interactor]

    # Batch render handles
    if __handles_to_render__.get(interactor, None) is None:
        listener = interactor.AddObserver("TimerEvent", render)
        timer = interactor.CreateOneShotTimer(1)
        __handles_to_render__[interactor] = [handle]
    else:
        __handles_to_render__[interactor].append(handle)



class Handle(Widget):
    def __init__(self, interactor, point, width=10, height=10, opacity=1, color=(0, 0, 0), clicked=None, dragged=None, released=None, normalize=False):
        
        self.x, self.y = point
        self.color = color
        self.clicked = clicked
        self.dragged = dragged
        self.released = released
        self.normalize = normalize
        widget = vtkHandleWidget()
        
        widget.AllowHandleResizeOff()

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
        if self.widget.GetEnabled() == False:
            self.widget.On()
            self.place()

    def hide(self):
        if self.widget.GetEnabled():
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
        self.render()

    def click(self, object, event):
        
        self.subscribe("EndInteractionEvent", self.release)
        self.subscribe("InteractionEvent", self.drag)

        if self.clicked:
            self.clicked(self)

    def render(self):
        """
        Doesn't actually immediately render; batches up handle renders so everything shows up in the appropriate place at once
        """
        __schedule_render__(self.interactor, self)

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
