"""
Test label placement when using the top property
"""
import vcs.vtk_ui
import vtk

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_label_text(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(200, 40)

        l = vcs.vtk_ui.Label(self.inter, "Test Text", fgcolor=(0, 0, 0))
        l.text = "Other Text"
        if l.get_text() != "Other Text":
            print "Text not set correctly"
            return

        l.set_text("Other other text")
        if l.get_text() != "Other other text" or l.text != "Other other text":
            print "Text not retrieving correctly"
            return
        l.left = 0
        l.top = 0
        l.show()

        self.test_file = "test_vtk_ui_label_text.png"


if __name__ == "__main__":
    test_vtk_ui_label_text().test()
