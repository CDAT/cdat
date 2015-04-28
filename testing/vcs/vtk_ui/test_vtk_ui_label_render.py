"""
Test label rendering
"""
import vcs.vtk_ui

from vtk_ui_test import vtk_ui_test, generate_png


class test_vtk_ui_label_render(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(130, 40)

        label = vcs.vtk_ui.Label(self.inter, "Test Rendering", fgcolor=(0, 0, 0))

        label.show()

        self.test_file = "test_vtk_ui_label_render_initial.png"

        # Initialize to passing so we can tell if we failed
        self.passed = 0

        if len(self.args) > 0:
            self.passed = self.check_image(self.args[0])
            if self.passed == 1:
                return
        else:
            generate_png(self.win, self.test_file)

        label.actor.GetTextProperty().SetColor((.25, .5, .75))
        self.test_file = "test_vtk_ui_label_render_color_change.png"
        label.render()

        if len(self.args) > 1:
            self.passed = self.check_image(self.args[1])
        else:
            generate_png(self.win, self.test_file)

        # Skip the auto-image-check
        self.test_file = None
        return

if __name__ == "__main__":
    test_vtk_ui_label_render().test()
