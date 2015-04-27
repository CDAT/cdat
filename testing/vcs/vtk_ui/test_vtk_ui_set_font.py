"""
Test the __set_font function in the text module
"""
from vcs.vtk_ui.text import __set_font as set_font
import vtk
import os

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_set_font(vtk_ui_test):
    def do_test(self):
        prop = vtk.vtkTextProperty()

        set_font("Arial", prop)
        if prop.GetFontFamily() != vtk.VTK_ARIAL:
            print "Font was not set to Arial"
            return

        set_font("Courier", prop)
        if prop.GetFontFamily() != vtk.VTK_COURIER:
            print "Font was not set to Courier"
            return

        set_font("Times", prop)
        if prop.GetFontFamily() != vtk.VTK_TIMES:
            print "Font was not set to Times"
            return

        path = os.path.abspath("blex.ttf")
        set_font(path, prop)
        if prop.GetFontFamily() == vtk.VTK_FONT_FILE:
            if path != prop.GetFontFile():
                print "Set path incorrectly"
                return
        else:
            print "Did not set Font File correctly"
            return

        self.passed = 0


if __name__ == "__main__":
    test_vtk_ui_set_font().test()
