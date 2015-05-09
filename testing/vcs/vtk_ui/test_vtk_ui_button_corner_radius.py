"""
Test button placement
"""
import vcs.vtk_ui


from vtk_ui_test import vtk_ui_test

class test_vtk_ui_button_corner_radius(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(100, 300)
        for i in range(10):
            button = vcs.vtk_ui.Button(self.inter, corner_radius=i, font="Arial", left=10, top=30 * i, label="Test", bgcolor=(.1, .1, .1), fgcolor=(1, 1, 1), size=14, halign=vcs.vtk_ui.button.LEFT_ALIGN, valign=vcs.vtk_ui.button.CENTER_ALIGN)
            button.place()
            button.show()
        self.test_file = "test_vtk_ui_button_corner_radius.png"

if __name__ == "__main__":
    test_vtk_ui_button_corner_radius().test()