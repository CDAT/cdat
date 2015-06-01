"""
Test button placement
"""
import vcs.vtk_ui


from vtk_ui_test import vtk_ui_test

class test_vtk_ui_button_action(vtk_ui_test):
    def action(self, state):
        self.passed = 0

    def do_test(self):
        self.win.SetSize(100, 100)
        button = vcs.vtk_ui.Button(self.inter, action=self.action, corner_radius=5, font="Arial", left=10, top=10, label="Test", bgcolor=(.1, .1, .1), fgcolor=(1, 1, 1), size=14, halign=vcs.vtk_ui.button.LEFT_ALIGN, valign=vcs.vtk_ui.button.CENTER_ALIGN)
        button.place()
        button.show()
        button.clicked(None, None)

if __name__ == "__main__":
	test_vtk_ui_button_action().test()
