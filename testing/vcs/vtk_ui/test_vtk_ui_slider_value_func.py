"""
Test slider callable value argument
"""
import vcs.vtk_ui
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_slider_value_func(vtk_ui_test):
    def __init__(self):
        super(test_vtk_ui_slider_value_func, self).__init__()
        self.got_value = 0

    def do_test(self):
        self.win.SetSize(200, 200)

        slider = vcs.vtk_ui.Slider(self.inter, value=self.get_value, min_val=.3, max_val=.75, point1=(.1, .5), point2=(.9, .5))

        assert self.got_value == 1, "Failed to retrieve value from function on init"

        slider.show()

        assert self.got_value == 2, "Failed to retrieve value from function on show"

        self.test_file = "test_vtk_ui_slider_value_func.png"

    def get_value(self):
        self.got_value += 1
        return .5

if __name__ == "__main__":
    test_vtk_ui_slider_value_func().test()
