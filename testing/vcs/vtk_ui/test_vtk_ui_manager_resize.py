"""
Test window resizing placing widgets correctly
"""
import vcs.vtk_ui

from vtk_ui_test import vtk_ui_test

from time import sleep


class test_vtk_ui_manager_resize(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(250, 100)
        # Due to UV-CDAT/uvcdat#1148, have to render on screen when resizing
        self.win.SetOffScreenRendering(0)

        button = vcs.vtk_ui.Button(self.inter, label="Position me", left=10, top=10)
        button.place()
        button.show()

        self.win.SetSize(200, 50)

        sleep(1) ; # Added delay to prevent race condition - test being triggered before resize is complete

        self.win.Modified()

        self.test_file = "test_vtk_ui_manager_resize.png"

if __name__ == "__main__":
    test_vtk_ui_manager_resize().test()
