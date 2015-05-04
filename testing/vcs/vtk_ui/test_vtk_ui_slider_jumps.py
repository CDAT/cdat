"""
Test slider jumps to new values instead of animating
"""
import vcs.vtk_ui
from time import sleep
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_slider_jumps(vtk_ui_test):
    def __init__(self):
        self.failed = False
        self.updated = False
        super(test_vtk_ui_slider_jumps, self).__init__()
    def do_test(self):
        self.win.SetSize(100, 100)

        slider = vcs.vtk_ui.Slider(self.inter, value=0, min_val=0, max_val=5, point1=(0, .5), point2=(1, .5), update=self.update_test)
        slider.show()

        self.click_event(80, 50)
        if self.failed:
            self.passed = 1
        else:
            self.passed = 0

    def update_test(self, value):
        if value < 4:
            self.failed = True

if __name__ == "__main__":
    test_vtk_ui_slider_jumps().test()
