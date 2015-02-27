"""
Test button placement
"""
import vcs.vtk_ui


from vtk_ui_test import vtk_ui_test

class test_vtk_ui_button_draw(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(100, 250)

        for i in range(5):
            button = vcs.vtk_ui.Button(self.inter, corner_radius=5, font="Arial", left=10 * i, top=30 * i, label="Test %d" % i, bgcolor=(.1, .1, .1), fgcolor=(1, 1, 1), size=14, halign=vcs.vtk_ui.button.LEFT_ALIGN, valign=vcs.vtk_ui.button.CENTER_ALIGN)
            button.place()
            button.show()
        self.test_file = "test_vtk_ui_button_draw.png"

import sys
sys.exit(test_vtk_ui_button_draw().test())