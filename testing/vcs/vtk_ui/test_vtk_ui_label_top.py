"""
Test label placement when using the top property
"""
import vcs.vtk_ui
import vtk

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_label_top(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(130, 40)

        # Use as a guide for where the tops should be
        line = vcs.vtk_ui.line.Line((0, 20), (150, 20))

        # Add a renderer for the line
        r = vtk.vtkRenderer()
        self.win.AddRenderer(r)
        self.win.SetNumberOfLayers(2)
        r.SetLayer(1)

        line.renderer = r
        line.show()

        test_string = "Hi"
        black = (0, 0, 0)

        top_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black)
        top_label.show()
        top_label.left = 10
        top_label.top = 20

        center_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black)
        center_label.actor.GetTextProperty().SetVerticalJustificationToCentered()
        center_label.show()
        center_label.left = 50
        center_label.top = 20

        bottom_label = vcs.vtk_ui.Label(self.inter, test_string, fgcolor=black)
        bottom_label.actor.GetTextProperty().SetVerticalJustificationToBottom()
        bottom_label.show()
        bottom_label.left = 90
        bottom_label.top = 20

        if top_label.top != 20 or center_label.top != 20 or bottom_label.top != 20:
            # All tops should be the same
            if top_label.top != 20:
                print "Top-aligned label's top calculation gives", top_label.top, "should be 20"
            if center_label.top != 20:
                print "Center-aligned label's top calculation gives", center_label.top, "should be 20"
            if bottom_label.top != 20:
                print "Bottom-aligned label's top calculation gives", bottom_label.top, "should be 20"
            return

        self.test_file = "test_vtk_ui_label_top.png"


if __name__ == "__main__":
    test_vtk_ui_label_top().test()
