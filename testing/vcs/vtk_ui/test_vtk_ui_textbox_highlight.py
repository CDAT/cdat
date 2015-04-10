"""
Test textbox highlight features
"""
import vcs.vtk_ui

import vtk
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_textbox_highlight(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(170, 85)

        t1 = vcs.vtk_ui.Textbox(self.inter, "Highlight On", fgcolor=(0,0,0), highlight_color=(1, .75, .35), highlight_opacity=(.5), left=10, top=10)
        t1.show()
        t1.show_highlight()

        t2 = vcs.vtk_ui.Textbox(self.inter, "Highlight Off", fgcolor=(0,0,0), highlight_color=(1, .75, .35), highlight_opacity=(.5), left=10, top=50)
        t2.show()
        t2.show_highlight()
        t2.hide_highlight()

        self.test_file = "test_vtk_ui_textbox_highlight.png"

test_vtk_ui_textbox_highlight().test()