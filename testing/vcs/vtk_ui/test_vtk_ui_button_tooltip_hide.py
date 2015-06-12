"""
Test button tooltip hides when hovered over
"""
import vcs.vtk_ui

import vtk
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_button_tooltip_hide(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(100, 100)

        prop = vtk.vtkTextProperty()
        prop.SetColor(.5, .5, .5)
        prop.SetBackgroundColor(1, 0, 0)
        prop.SetBackgroundOpacity(1)
        prop.SetVerticalJustificationToCentered()
        b = vcs.vtk_ui.Button(self.inter, label="Hover", tooltip="Hidden", tooltip_property=prop)
        b.show()

        # Timers only work while interacting, have to fake this ID
        b.hover_timer = 1

        self.hover(5, 95, .3)
        self.test_file = "test_vtk_ui_button_tooltip_hide.png"
        self.hover(5, 50, .1)

test_vtk_ui_button_tooltip_hide().test()