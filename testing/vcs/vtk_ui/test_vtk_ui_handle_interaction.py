"""
Tests handle's interactivity.
"""

import vcs.vtk_ui
import vtk_ui_test
import decimal

class test_vtk_ui_handle_interaction(vtk_ui_test.vtk_ui_test):
    def __init__(self):
        super(test_vtk_ui_handle_interaction, self).__init__()
        self.h = None
        self.h2 = None

    def do_test(self):
        self.win.SetSize(100, 100)

        self.h = vcs.vtk_ui.Handle(self.inter, (5, 5), clicked=self.clicked, dragged=self.dragged, released=self.released)
        self.h.show()

        self.mouse_down(5, 5)
        self.mouse_move(10, 10)
        self.mouse_up(10, 10)

        # Test normalized drag provides normalized dx/dy
        self.h2 = vcs.vtk_ui.Handle(self.inter, (.3, .3), dragged=self.norm_drag, normalize=True)
        self.h2.show()
        self.mouse_down(30, 30)
        self.mouse_move(40, 40)
        self.mouse_up(40, 40)

        assert self.passed == 5, "Did not trigger drag on normalized"

        self.passed = 0

    def norm_drag(self, handle, dx, dy):
        assert handle == self.h2, "Normalized passed wrong handle to drag"
        assert decimal.Decimal("%f" % dx) == decimal.Decimal("%f" % .1), "DX normalized incorrectly; %f when expecting %f" % (dx, .1)
        assert decimal.Decimal("%f" % dy) == decimal.Decimal("%f" % .1), "DY normalized incorrectly; %f when expecting %f" % (dy, .1)
        assert self.passed == 4, "Did not trigger released"
        self.passed = 5

    def clicked(self, handle):
        assert handle == self.h, "Clicked received argument that was not the handle"
        self.passed = 2

    def dragged(self, handle, dx, dy):
        assert handle == self.h, "Dragged received argument that was not the handle"
        assert dx == 5, "DX was different from expected value"
        assert dy == 5, "DY was different from expected value"
        assert self.passed == 2, "Did not trigger clicked before dragging"
        self.passed = 3

    def released(self, handle):
        assert handle == self.h, "Released received argument that was not the handle"
        assert self.passed == 3, "Did not trigger dragged before released"
        self.passed = 4

if __name__ == "__main__":
    t = test_vtk_ui_handle_interaction()
    t.test()
