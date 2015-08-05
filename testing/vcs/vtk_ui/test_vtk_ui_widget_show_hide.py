"""
Test widget show/hide
"""
import vcs.vtk_ui

import vtk
from vtk_ui_test import vtk_ui_test, generate_png

class test_vtk_ui_widget_show_hide(vtk_ui_test):
    def do_test(self):
        self.win.SetSize((100, 100))

        vw = vtk.vtkButtonWidget()
        vr = vtk.vtkTexturedButtonRepresentation2D()

        vr.SetNumberOfStates(1)
        r = vtk.vtkPNGReader()
        r.SetFileName("Pepper.png")
        r.Update()
        image = r.GetOutput()
        vr.SetButtonTexture(0, image)
        vw.SetRepresentation(vr)

        w = vcs.vtk_ui.widget.Widget(self.inter, vw)
        w.show()

        width, height, _ = image.GetDimensions()
        bounds = (0, 0 + width, 0, height, 0, 0)
        vr.SetPlaceFactor(1)
        vr.PlaceWidget(bounds)

        self.test_file = "test_vtk_ui_widget_show.png"
        if self.args:
            self.passed = self.check_image(self.args[0])
            if self.passed == 1:
                print "Failed to show correctly"
                return
        else:
            generate_png(self.win, self.test_file)

        assert w.showing(), "showing() thinks hidden while showing"

        w.hide()

        assert w.showing() == False, "showing() thinks showing while hidden"

        self.test_file = "test_vtk_ui_widget_hide.png"
        if len(self.args) > 1:
            self.passed = self.check_image(self.args[1])
            if self.passed == 1:
                print "Failed to hide correctly"
                return
        else:
            generate_png(self.win, self.test_file)
        self.test_file = None
        self.passed = 0


if __name__ == "__main__":
    test_vtk_ui_widget_show_hide().test()
