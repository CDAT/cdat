"""
Tests handle's normalization.
"""

import vcs.vtk_ui
import vtk_ui_test

class test_vtk_ui_handle_normalize(vtk_ui_test.vtk_ui_test):
    def do_test(self):
        self.win.SetSize(100, 100)

        h = vcs.vtk_ui.Handle(self.inter, (.5, .5), normalize=True)
        pos = h.__get_position__()

        assert pos == (50, 50), "Did not denormalize position when normalize set to true"

        h2 = vcs.vtk_ui.Handle(self.inter, (50, 50), normalize=False)
        pos = h2.__get_position__()

        assert pos == (50, 50), "Normalized position when normalize set to false"
        self.passed = 0

if __name__ == "__main__":
    t = test_vtk_ui_handle_normalize()
    t.test()
