"""
Test widget detach
"""
import vcs.vtk_ui

import vtk
from vtk_ui_test import vtk_ui_test, generate_png

class test_vtk_ui_widget_detach(vtk_ui_test):
    def do_test(self):
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

        w.detach()
        assert vr.GetRenderer() is None and vw.GetCurrentRenderer() is None, "Renderer is set after detach"
        assert vw.GetInteractor() is None, "Interactor set after detach"
        assert w.showing() == False, "Widget is flagged as Enabled"
        assert w not in vcs.vtk_ui.manager.get_manager(self.inter).widgets, "Widget still being managed"
        assert w.interactor is None, "Widget has a reference to interactor"
        self.passed = 0

if __name__ == "__main__":
    test_vtk_ui_widget_detach().test()
