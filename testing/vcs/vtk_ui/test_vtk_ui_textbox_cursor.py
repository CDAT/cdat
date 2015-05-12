"""
Test textbox cursor
"""
import vcs.vtk_ui
import datetime
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_textbox_cursor(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(1050, 55)

        # Cursor showing
        left = 0

        t = vcs.vtk_ui.Textbox(self.inter, "A", fgcolor=(0,0,0))
        t.show()
        t.row = 0
        t.column = 1
        t.show_cursor()

        should_blink_time = datetime.datetime.now() - datetime.timedelta(0, 0, 0, 400)

        w, h = t.get_dimensions()
        left += w

        # Cursor blink on
        t = vcs.vtk_ui.Textbox(self.inter, "C", fgcolor=(0,0,0))
        t.left = left
        t.show()
        t.row = 0
        t.column = 1
        t.editing = True
        t.last_blink = should_blink_time
        t.blink_cursor(None, None)

        w, h = t.get_dimensions()
        left += w

        # Cursor blink off
        t = vcs.vtk_ui.Textbox(self.inter, "D", fgcolor=(0,0,0))
        t.left = left
        t.show()
        t.row = 0
        t.column = 1
        t.editing = True
        t.last_blink = should_blink_time
        t.blink_cursor(None, None)
        t.last_blink = should_blink_time
        t.blink_cursor(None, None)

        w, h = t.get_dimensions()
        left += w

        # Cursor blink fails
        t = vcs.vtk_ui.Textbox(self.inter, "E", fgcolor=(0,0,0))
        t.left = left
        t.show()
        t.row = 0
        t.column = 1
        t.editing = True
        t.last_blink = datetime.datetime.now()
        t.blink_cursor(None, None)

        w, h = t.get_dimensions()
        left += w

        # Rotated cursor
        t = vcs.vtk_ui.Textbox(self.inter, "F", fgcolor=(0,0,0))
        t.left = left
        t.top = 25
        t.show()
        t.row = 0
        t.column = 1
        p = t.actor.GetTextProperty()
        p.SetOrientation(75)
        p.SetJustification(1)
        p.SetVerticalJustification(1)
        t.show_cursor()

        w, h = t.get_dimensions()
        left += w

        # Rotated multi-line cursor
        t = vcs.vtk_ui.Textbox(self.inter, "G\nH", fgcolor=(0,0,0))
        t.left = left
        t.top = 25
        t.row = 1
        t.column = 1
        p = t.actor.GetTextProperty()
        p.SetOrientation(75)
        p.SetJustification(1)
        p.SetVerticalJustification(1)
        t.show()
        t.show_cursor()

        w, h = t.get_dimensions()
        left += w

        # Multi-line cursor
        t = vcs.vtk_ui.Textbox(self.inter, "I\nJ", fgcolor=(0,0,0))
        t.left = left
        t.row = 1
        t.column = 1
        t.show()
        t.show_cursor()

        w, h = t.get_dimensions()
        left += w

        # Multi-column cursor
        t = vcs.vtk_ui.Textbox(self.inter, "KL", fgcolor=(0,0,0))
        t.left = left
        t.row = 0
        t.column = 2
        t.show()
        t.show_cursor()

        w, h = t.get_dimensions()
        left += w

        # Multi-column cursor (Middle of text)
        t = vcs.vtk_ui.Textbox(self.inter, "MN", fgcolor=(0,0,0))
        t.left = left
        t.row = 0
        t.column = 1
        t.show()
        t.show_cursor()

        w, h = t.get_dimensions()
        left += w

        # Multi-column and row cursor
        t = vcs.vtk_ui.Textbox(self.inter, "OP\nQR", fgcolor=(0,0,0))
        t.left = left
        t.row = 1
        t.column = 2
        t.show()
        t.show_cursor()

        w, h = t.get_dimensions()
        left += w

        # Multi-column and row cursor (Middle col)
        t = vcs.vtk_ui.Textbox(self.inter, "ST\nUV", fgcolor=(0,0,0))
        t.left = left
        t.row = 1
        t.column = 1
        t.show()
        t.show_cursor()

        w, h = t.get_dimensions()
        left += w

        # Use all horizontal justifications to test these:
        for h in range(3):
            # Shorter text on second line
            t = vcs.vtk_ui.Textbox(self.inter, "WX\nY", fgcolor=(0,0,0))
            t.row = 1
            t.column = 1
            p = t.actor.GetTextProperty()
            p.SetJustification(h)
            t.left = left
            t.top = 0
            t.show()
            t.show_cursor()

            w, _ = t.get_dimensions()
            left += w + 25

            # Shorter text on first line
            t = vcs.vtk_ui.Textbox(self.inter, "Z\nAB", fgcolor=(0,0,0))
            t.row = 0
            t.column = 1
            p = t.actor.GetTextProperty()
            p.SetJustification(h)
            t.left = left
            t.top = 0
            t.show()
            t.show_cursor()

            w, _ = t.get_dimensions()
            left += w + 25

        # Use all vertical justifications to test these:
        for v in range(3):
            # Shorter text on second line
            t = vcs.vtk_ui.Textbox(self.inter, "CD\nE", fgcolor=(0,0,0))
            t.row = 1
            t.column = 1
            p = t.actor.GetTextProperty()
            p.SetVerticalJustification(v)
            t.left = left
            t.top = 0
            t.show()
            t.show_cursor()

            w, _ = t.get_dimensions()
            left += w + 25

            # Shorter text on first line
            t = vcs.vtk_ui.Textbox(self.inter, "F\nGH", fgcolor=(0,0,0))
            t.row = 0
            t.column = 1
            p = t.actor.GetTextProperty()
            p.SetVerticalJustification(v)
            t.left = left
            t.top = 0
            t.show()
            t.show_cursor()

            w, _ = t.get_dimensions()
            left += w + 25

        self.test_file = "test_vtk_ui_textbox_cursor.png"

if __name__ == "__main__":
    test_vtk_ui_textbox_cursor().test()
