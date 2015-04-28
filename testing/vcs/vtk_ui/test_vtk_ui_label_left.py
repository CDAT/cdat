"""
Test label placement when using the left property
"""
import vcs.vtk_ui
import vtk

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_label_left(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(130, 130)

        font_sizes = [10, 18, 24, 31]

        for ind, x in enumerate((5, 20, 40, 75)):
            # Use as a guide for where the tops should be
            line = vcs.vtk_ui.line.Line((x, 0), (x, 130))

            line.renderer = self.renderer
            line.show()

            test_string = "Hi"
            black = (0, 0, 0)

            left_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black, size=font_sizes[ind])
            left_label.show()
            left_label.actor.GetTextProperty().SetJustificationToLeft()
            left_label.top = 10
            left_label.left = x

            center_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black, size=font_sizes[ind])
            center_label.actor.GetTextProperty().SetJustificationToCentered()
            center_label.show()
            center_label.top = 50
            center_label.left = x

            right_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black, size=font_sizes[ind])
            right_label.actor.GetTextProperty().SetJustificationToRight()
            right_label.show()
            right_label.top = 90
            right_label.left = x

            if left_label.left != x or center_label.left != x or right_label.left != x:
                # All lefts should be the same
                if left_label.left != x:
                    print "Left-aligned label's left calculation gives", left_label.left, "should be", x
                if center_label.left != x:
                    print "Center-aligned label's left calculation gives", center_label.left, "should be", x
                if right_label.left != x:
                    print "Right-aligned label's left calculation gives", right_label.left, "should be", x
                return

        self.test_file = "test_vtk_ui_label_left.png"


if __name__ == "__main__":
    test_vtk_ui_label_left().test()
