"""
Test label placement when using the y property and alignments
"""
import vcs.vtk_ui
import vtk

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_label_y(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(150, 110)

        font_sizes = [10, 18, 24, 31]

        for ind, y in enumerate((5, 20, 40, 75)):
            # Use as a guide for where the tops should be
            line = vcs.vtk_ui.line.Line((0, y), (150, y))

            line.renderer = self.renderer
            line.show()

            test_string = "Hi"
            black = (0, 0, 0)

            top_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black, size=font_sizes[ind])
            top_label.show()
            top_label.left = 10
            top_label.y = y

            center_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black, size=font_sizes[ind])
            center_label.actor.GetTextProperty().SetVerticalJustificationToCentered()
            center_label.show()
            center_label.left = 50
            center_label.y = y

            bottom_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black, size=font_sizes[ind])
            bottom_label.actor.GetTextProperty().SetVerticalJustificationToBottom()
            bottom_label.show()
            bottom_label.left = 90
            bottom_label.y = y

            if top_label.y != y or center_label.y != y or bottom_label.y != y:
                # All tops should be the same
                if top_label.y != y:
                    print "Top-aligned label's y calculation gives", top_label.y, "should be", y
                if center_label.y != y:
                    print "Center-aligned label's y calculation gives", center_label.y, "should be", y
                if bottom_label.y != y:
                    print "Bottom-aligned label's y calculation gives", bottom_label.y, "should be", y
                return

        self.test_file = "test_vtk_ui_label_y.png"


if __name__ == "__main__":
    test_vtk_ui_label_y().test()
