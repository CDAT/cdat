"""
Test slider callbacks
"""
import vcs.vtk_ui
from vtk_ui_test import vtk_ui_test
from decimal import Decimal
class test_vtk_ui_slider_callbacks(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(100, 100)

        slider = vcs.vtk_ui.Slider(self.inter, point1=(.1, .5), point2=(.9, .5), end=self.end_callback, update=self.update_callback)
        slider.show()
        self.click_event(75, 50)

    def end_callback(self, value):
        if value == .5 and self.passed == 1:
            self.passed = 0
        else:
            self.passed = 1
            if value != .5:
                print "Did not accept use update_callback value"

    def update_callback(self, value):
        if Decimal(str(value)) != Decimal("0.852112676056"):
            print "Passed wrong value to update_callback", value
            self.passed = 2
        return .5

if __name__ == "__main__":
    test_vtk_ui_slider_callbacks().test()
