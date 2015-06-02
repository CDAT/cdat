"""
Test label's in_bounds function
"""
import vcs.vtk_ui

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_label_bounds(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(130, 40)

        l = vcs.vtk_ui.Label(self.inter, "Testing", fgcolor=(0, 0, 0), size=24, font="Arial")
        l.show()
        w, h = vcs.vtk_ui.text.text_dimensions("Testing", l.actor.GetTextProperty())

        left = l.left

        # Translate top coordinate from (distance from top) to (distance from bottom)
        top = 40 - l.top

        # Default to passed
        self.passed = 0
        for x in range(130):
            for y in range(40):
                if l.in_bounds(x, y):
                    if x - left < 0:
                        print "Label thinks", x, y, "is inside box, but it's to the left"
                        self.passed = 1
                    if x - left > w + 2:
                        # +2, because there's a little bit of margin
                        print "Label thinks", x, y, "is inside box, but it's to the right"
                        self.passed = 1
                    if y - top > 0:
                        print "Label thinks", x, y, "is inside box, but it's above"
                        self.passed = 1
                    if top - y > h:
                        print "Label thinks", x, y, "is inside box, but it's below"
                        self.passed = 1

        if self.passed == 1:
            print "Found points outside of box that label claimed were in_bounds (x1: %f, y1:%f, x2:%f, y2:%f)" % (left, top, left + w, top - h)

if __name__ == "__main__":
    test_vtk_ui_label_bounds().test()
