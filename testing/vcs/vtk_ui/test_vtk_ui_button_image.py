"""
Test button image
"""
import vcs.vtk_ui

from vtk_ui_test import vtk_ui_test
import os

class test_vtk_ui_button_image(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(80, 80)
        directory = os.path.dirname(__file__)
        image = os.path.join(directory, "Pepper.png")
        b = vcs.vtk_ui.Button(self.inter, image=image, left=5, top=5)
        b.show()
        self.test_file = "test_vtk_ui_button_image.png"

test_vtk_ui_button_image().test()