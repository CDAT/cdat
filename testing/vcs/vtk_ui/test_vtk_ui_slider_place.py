"""
Test slider placement
"""
import vcs.vtk_ui
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_slider_place(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(500, 500)

        slider = vcs.vtk_ui.Slider(self.inter, point1=(.1, .5), point2=(.9, .5))
        assert slider.x1 == .1, "x1 set incorrectly; expected .1, got %f" % slider.x1
        assert slider.y1 == .5 == slider.y2, "y1 or y2 incorrect; both should be .5, got %f and %f respectively" % (slider.y1, slider.y2)
        assert slider.x2 == .9, "x2 set incorrectly; expected .9, got %f" % slider.x2
        slider.show()

        slider.x1 = .2
        slider.y1 = .3

        slider.place()
        self.win.Render()
        self.test_file = "test_vtk_ui_slider_place.png"

if __name__ == "__main__":
    test_vtk_ui_slider_place().test()
