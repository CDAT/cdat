from widget import Widget
import vtk
class Slider(Widget):

    def __init__(self, interactor, value=0, min_val=0, max_val=1, point1=(0,.1), point2=(1,.1), end=None, update=None, title=""):

        sliderWidget = vtk.vtkSliderWidget()
        sliderWidget.SetRepresentation(vtk.vtkSliderRepresentation2D())

        super(Slider, self).__init__(interactor, sliderWidget)

        self.end_callback = end
        self.update_callback = update

        self.x1, self.y1 = point1
        self.x2, self.y2 = point2

        self.repr.GetPoint1Coordinate().SetCoordinateSystemToNormalizedDisplay()
        self.repr.GetPoint2Coordinate().SetCoordinateSystemToNormalizedDisplay()

        prop = self.repr.GetSliderProperty()
        prop.SetColor( 1.0, 0.0, 0.0 )
        prop.SetOpacity( 0.5 )

        sprop = self.repr.GetSelectedProperty()
        sprop.SetOpacity( 0.8 )

        tprop = self.repr.GetTubeProperty()
        tprop.SetColor( 0.5, 0.5, 0.5 )
        tprop.SetOpacity( 0.5 )

        cprop = self.repr.GetCapProperty()
        cprop.SetColor( 0.0, 0.0, 1.0 )
        cprop.SetOpacity( 0.5 )

        self.repr.SetMinimumValue(float(min_val))
        self.repr.SetMaximumValue(float(max_val))
        if callable(value):
            self.repr.SetValue(float(value()))
            self.value_func = value
        else:
            self.repr.SetValue(float(value))
            self.value_func = None

        self.repr.SetSliderLength(0.05)
        self.repr.SetSliderWidth(0.02)
        self.repr.SetTubeWidth(0.01)
        self.repr.SetEndCapLength(0.02)
        self.repr.SetEndCapWidth(0.02)
        self.repr.SetTitleHeight( 0.02 )
        self.repr.SetTitleText(title)

        sliderWidget.SetAnimationModeToJump()

        sliderWidget.AddObserver("EndInteractionEvent", self.end_slide)
        sliderWidget.AddObserver("InteractionEvent", self.slide_value)

        sliderWidget.KeyPressActivationOff()
        self.place()

    def place(self):
        self.repr.GetPoint1Coordinate().SetValue((self.x1, self.y1, 0))
        self.repr.GetPoint2Coordinate().SetValue((self.x2, self.y2, 0))

    def show(self):
        if self.value_func:
            self.repr.SetValue(float(self.value_func()))
        super(Slider, self).show()


    def end_slide(self, obj, event):
        value = self.repr.GetValue()

        if self.end_callback:
            self.end_callback(value)

    def slide_value(self, obj, event):
        value = self.repr.GetValue()

        if self.update_callback:
            new_value = self.update_callback(value)
            if new_value is not None:
                self.repr.SetValue(new_value)