"""
Test slider basic appearance
"""
import vcs.vtk_ui
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_slider_appearance(vtk_ui_test):
    def __init__(self):
        self.failed = False
        self.updated = False
        super(test_vtk_ui_slider_appearance, self).__init__()
    def do_test(self):
        self.win.SetSize(100, 500)

        slider = vcs.vtk_ui.Slider(self.inter, point1=(.1, .5), point2=(.9, .5))
        slider.show()

        self.test_file = "test_vtk_ui_slider_appearance.png"

if __name__ == "__main__":
    test_vtk_ui_slider_appearance().test()
