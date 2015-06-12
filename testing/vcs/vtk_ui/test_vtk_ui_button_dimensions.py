"""
Test button placement
"""
import vcs.vtk_ui


from vtk_ui_test import vtk_ui_test

class test_vtk_ui_button_dimensions(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(290, 300)

        button = vcs.vtk_ui.Button(self.inter, corner_radius=5, font="Arial", height=15, width=100, left=10, top=10, label="100 x 15", bgcolor=(.1, .1, .1), fgcolor=(1, 1, 1), size=14, halign=vcs.vtk_ui.button.LEFT_ALIGN, valign=vcs.vtk_ui.button.CENTER_ALIGN)
        button.place()
        button.show()

        button = vcs.vtk_ui.Button(self.inter, corner_radius=5, font="Arial", height=90, width=90, left=10, top=30, label="90 x 90", bgcolor=(.1, .1, .1), fgcolor=(1, 1, 1), size=14, halign=vcs.vtk_ui.button.LEFT_ALIGN, valign=vcs.vtk_ui.button.CENTER_ALIGN)
        button.place()
        button.show()

        button = vcs.vtk_ui.Button(self.inter, corner_radius=5, font="Arial", height=100, width=30, left=10, top=200, label="30\n x \n100", bgcolor=(.1, .1, .1), fgcolor=(1, 1, 1), size=14, halign=vcs.vtk_ui.button.LEFT_ALIGN, valign=vcs.vtk_ui.button.CENTER_ALIGN)
        button.place()
        button.show()

        self.test_file = "test_vtk_ui_button_dimensions.png"

test_vtk_ui_button_dimensions().test()
