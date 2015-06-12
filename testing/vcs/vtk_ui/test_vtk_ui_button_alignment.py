"""
Test button alignment
"""
from vcs.vtk_ui import button

from vtk_ui_test import vtk_ui_test

class test_vtk_ui_button_alignment(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(300, 50)
        left_button = button.Button(self.inter, label="Left", halign=button.LEFT_ALIGN, valign=button.TOP_ALIGN, width=125)
        left_button.show()
        center_button = button.Button(self.inter, label="Center", halign=button.CENTER_ALIGN, valign=button.BOTTOM_ALIGN, top=25, width=125)
        center_button.show()
        right_button = button.Button(self.inter, label="Right", halign=button.RIGHT_ALIGN, valign=button.CENTER_ALIGN, width=125)
        right_button.show()
        self.test_file = "test_vtk_ui_button_alignment.png"

test_vtk_ui_button_alignment().test()