from widget import Widget
import vtk
class Slider(Widget):

    def __init__(self, interactor, value=0, min_val=0, max_val=1, point1=(0,.1), point2=(1,.1), end=None, update=None, title=""):

        sliderWidget = vtk.vtkSliderWidget()
        sliderWidget.SetRepresentation(vtk.vtkSliderRepresentation2D())

        super(Slider, self).__init__(interactor, sliderWidget)

        self.end_callback = end
        self.update_callback = update

        self.repr.GetPoint1Coordinate().SetCoordinateSystemToNormalizedDisplay()
        self.repr.GetPoint2Coordinate().SetCoordinateSystemToNormalizedDisplay()
        self.repr.GetPoint1Coordinate().SetValue((point1[0], point1[1], 0))
        self.repr.GetPoint2Coordinate().SetValue((point2[0], point2[1], 0))
        
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

        self.repr.SetMinimumValue(min_val)
        self.repr.SetMaximumValue(max_val)
        self.repr.SetValue(value)

        self.repr.SetSliderLength(0.05)
        self.repr.SetSliderWidth(0.02)
        self.repr.SetTubeWidth(0.01)
        self.repr.SetEndCapLength(0.02)
        self.repr.SetEndCapWidth(0.02)
        self.repr.SetTitleHeight( 0.02 )
        self.repr.SetTitleText(title)

        sliderWidget.SetAnimationModeToAnimate()
        
        sliderWidget.AddObserver("EndInteractionEvent", self.end_slide)
        sliderWidget.AddObserver("InteractionEvent", self.slide_value)

        sliderWidget.KeyPressActivationOff()

    def show(self):
        self.widget.EnabledOn()
        self.widget.On()
    
    def hide(self):
        self.widget.Off()

    def is_showing(self):
        return self.widget.GetEnabled() == 1

    def end_slide(self, obj, event):
        value = self.repr.GetValue()

        if self.end_callback:
            self.end_callback(value)

    def slide_value(self, obj, event):
        value = self.repr.GetValue()

        if self.update_callback:
            self.update_callback(value)