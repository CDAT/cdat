"""
Tests handle's basic appearance.
"""

import vcs.vtk_ui
import vtk_ui_test

class test_vtk_ui_handle_appearance(vtk_ui_test.vtk_ui_test):
    def do_test(self):
        self.win.SetSize(100, 100)

        h = vcs.vtk_ui.Handle(self.inter, (50, 50))
        h.show()

        self.test_file = "test_vtk_ui_handle_appearance.png"

if __name__ == "__main__":
    t = test_vtk_ui_handle_appearance()
    t.test()
