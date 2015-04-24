"""
Test label's set font attribute functions
"""
import vcs.vtk_ui

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_label_font_attrs(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(130, 40)

        l = vcs.vtk_ui.Label(self.inter, "Blue and small")
        l.set_font_size(8)
        l.set_font_color((0, 0, 1))
        l.left = 0
        l.show()

        l2 = vcs.vtk_ui.Label(self.inter, "Red and big")
        l2.set_font_size(18)
        l2.top = 10
        l2.left = 0
        l2.set_font_color((1, 0, 0))
        l2.show()

        self.test_file = "test_vtk_ui_label_font_attrs.png"


if __name__ == "__main__":
    test_vtk_ui_label_font_attrs().test()
