"""
Test widget initialization and registration
"""
import vcs.vtk_ui

import vtk
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_widget_init(vtk_ui_test):
    def do_test(self):
        vw = vtk.vtkTextWidget()
        vr = vtk.vtkTextRepresentation()
        vw.SetRepresentation(vr)
        w = vcs.vtk_ui.widget.Widget(self.inter, vw)
        assert w.repr == vr, "Representation improperly set"
        assert w.interactor == self.inter, "Interactor improperly set"
        assert w.manager == vcs.vtk_ui.manager.get_manager(self.inter), "Manager improperly set"
        assert w in w.manager.widgets, "Widget improperly registered"
        self.passed = 0

if __name__ == "__main__":
    test_vtk_ui_widget_init().test()
