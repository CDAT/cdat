"""
Test textbox appearance
"""
import vcs.vtk_ui
import vtk
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_textbox_appearance(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(200, 75)

        t = vcs.vtk_ui.Textbox(self.inter, "Sample Text", fgcolor=(0,0,0))
        t.left = 25
        t.top = 25
        t.show()
        self.test_file = "test_vtk_ui_textbox_appearance.png"

if __name__ == "__main__":
    test_vtk_ui_textbox_appearance().test()
