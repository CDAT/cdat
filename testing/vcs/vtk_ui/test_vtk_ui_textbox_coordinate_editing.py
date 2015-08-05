"""
Test textbox coordinate detection/editing
"""
import vcs.vtk_ui
import vtk
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_textbox_coordinate_editing(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(200, 75)
        t = vcs.vtk_ui.Textbox(self.inter, "Sample\nText", fgcolor=(0,0,0))
        t.left = 25
        t.top = 25
        t.show()
        t.start_editing((65, 35))
        assert t.row == 0 and t.column == 3, "Selected wrong character"

        t.start_editing((.325, .5))
        assert t.row == 0 and t.column == 3, "Selected wrong character"

        t.stop_editing()

        t.text = "\nHi\n"
        t.left = 25
        t.top = 25
        print "Getting dimensions"
        w, _ = t.get_dimensions()
        print "Got dimensions"
        t.start_editing((int(25 + w / 2.), 45))
        assert t.row == 0 and t.column == 0, "Clicked on blank row but got wrong info"

        t.stop_editing()
        t.text = "Sample\nText"
        # Start outside bounds
        t.start_editing((65, 1000))
        assert t.row == 1 and t.column == 4, "Defaults didn't work"

        p = t.actor.GetTextProperty()
        p.SetJustification(0)

        t.left = 25
        t.top = 25

        t.start_editing((0, 0))
        assert t.row == 1 and t.column == 2

        p.SetJustification(2)
        t.left = 25
        t.top = 25
        t.start_editing((1000, 1000))
        assert t.row == 1 and t.column == 4

        self.passed = 0

if __name__ == "__main__":
    test_vtk_ui_textbox_coordinate_editing().test()
