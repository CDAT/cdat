"""
Test slider value logic
"""
import vcs.vtk_ui
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_slider_values(vtk_ui_test):
    def __init__(self):
        self.failed = False
        self.updated = False
        super(test_vtk_ui_slider_values, self).__init__()
    def do_test(self):
        self.win.SetSize(200, 200)

        slider = vcs.vtk_ui.Slider(self.inter, value=.5, min_val=.3, max_val=.75, point1=(.1, .5), point2=(.9, .5))
        slider.show()

        try:
            slider.set_value(.2)
        except ValueError:
            print "Correctly caught less than minimum error"
        else:
            print "Failed to catch less than minimum error"
            return

        try:
            slider.set_value(1.)
        except ValueError:
            print "Correctly caught greater than maximum error"
        else:
            print "Failed to catch greater than maximum error"
            return

        try:
            slider.set_value(.4)
        except ValueError:
            print "Incorrectly errored on valid input for set_value"
            return

        self.test_file = "test_vtk_ui_slider_values.png"

if __name__ == "__main__":
    test_vtk_ui_slider_values().test()
