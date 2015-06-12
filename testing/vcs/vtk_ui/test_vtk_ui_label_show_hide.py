"""
Test label show/hide functionality
"""
import vcs.vtk_ui

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_label_show_hide(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(130, 80)

        l_show = vcs.vtk_ui.Label(self.inter, "Shown", top=0, fgcolor=(0, 0, 0))
        l_hide = vcs.vtk_ui.Label(self.inter, "Hidden", top=40, fgcolor=(0, 0, 0))

        l_show.show()
        l_hide.show()
        l_hide.hide()

        self.test_file = "test_vtk_ui_label_show_hide.png"


if __name__ == "__main__":
    test_vtk_ui_label_show_hide().test()
