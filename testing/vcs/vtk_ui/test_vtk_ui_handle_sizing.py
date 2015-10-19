"""
Tests handle's sizing.
"""

import vcs.vtk_ui
import vtk_ui_test

class test_vtk_ui_handle_sizing(vtk_ui_test.vtk_ui_test):
    def do_test(self):
        self.win.SetSize(100, 100)

        h1 = vcs.vtk_ui.Handle(self.inter, (10, 10), width=20, height=10)
        h1.show()

        h2 = vcs.vtk_ui.Handle(self.inter, (30, 25), width=20, height=20)
        h2.show()

        h3 = vcs.vtk_ui.Handle(self.inter, (45, 50), width=10, height=30)
        h3.show()

        self.test_file = "test_vtk_ui_handle_sizing.png"

if __name__ == "__main__":
    t = test_vtk_ui_handle_sizing()
    t.test()