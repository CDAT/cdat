"""
Test label placement when using the top property
"""
import vcs.vtk_ui
import vtk

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_label_top(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(130, 130)

        font_sizes = [10, 18, 24, 31]

        for ind, y in enumerate((5, 20, 40, 75)):
            # Use as a guide for where the tops should be
            line = vcs.vtk_ui.line.Line((0, 130 - y), (150, 130 - y))

            line.renderer = self.renderer
            line.show()

            test_string = "Hi"
            black = (0, 0, 0)

            top_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black, size=font_sizes[ind])
            top_label.show()
            top_label.left = 10
            top_label.top = y

            center_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black, size=font_sizes[ind])
            center_label.actor.GetTextProperty().SetVerticalJustificationToCentered()
            center_label.show()
            center_label.left = 50
            center_label.top = y

            bottom_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black, size=font_sizes[ind])
            bottom_label.actor.GetTextProperty().SetVerticalJustificationToBottom()
            bottom_label.show()
            bottom_label.left = 90
            bottom_label.top = y

            if top_label.top != y or center_label.top != y or bottom_label.top != y:
                # All tops should be the same
                if top_label.top != y:
                    print "Top-aligned label's top calculation gives", top_label.top, "should be", y
                if center_label.top != y:
                    print "Center-aligned label's top calculation gives", center_label.top, "should be", y
                if bottom_label.top != y:
                    print "Bottom-aligned label's top calculation gives", bottom_label.top, "should be", y
                return

        self.test_file = "test_vtk_ui_label_top.png"


if __name__ == "__main__":
    test_vtk_ui_label_top().test()
