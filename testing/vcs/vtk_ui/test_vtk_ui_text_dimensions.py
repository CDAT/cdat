"""
Test text_dimensions function; uses pre-measured text
"""
import vcs.vtk_ui.text
import vtk

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_text_dimensions(vtk_ui_test):
    def do_test(self):
        text_property = vtk.vtkTextProperty()
        text_property.SetFontFamilyToArial()
        text_property.SetFontSize(24)
        dpi = self.win.GetDPI()

        w, h = vcs.vtk_ui.text.text_dimensions("no descenders", text_property,
                                               dpi)
        if w != 174 or h != 23:
            print "no descenders width/height changed (%d,%d)"%(w,h)
            return

        w, h = vcs.vtk_ui.text.text_dimensions("couple good descenders",
                                               text_property, dpi)
        if w != 298 or h != 23:
            print "couple good descenders width/height changed (%d, %d)"%(w,h)
            return

        w, h = vcs.vtk_ui.text.text_dimensions(
              "This one\nis on\nmultiple lines", text_property, dpi)
        if w != 150 or h != 73:
            print "Multi-line width/height changed (%d,%d)"%(w,h)
            return

        self.passed = 0


if __name__ == "__main__":
    test_vtk_ui_text_dimensions().test()
