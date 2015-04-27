"""
Test white_or_black function from text module
"""
import vcs.vtk_ui

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_white_or_black(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(130, 40)

        white = (1, 1, 1)
        black = (0, 0, 0)

        if vcs.vtk_ui.text.white_or_black(*black) == black:
            print "Gave black for", black, "expected white"
            return

        if vcs.vtk_ui.text.white_or_black(.5, .5, .5) == black:
            print "Gave white for (.5, .5, .5) expected white"
            return

        if vcs.vtk_ui.text.white_or_black(*white) == white:
            print "Gave white for", white, "expected black"
            return

        if vcs.vtk_ui.text.white_or_black(.25, .1, .3) == black:
            print "Gave black for (.25, .1, .3) expected white"
            return

        if vcs.vtk_ui.text.white_or_black(.75, 1, 0) == white:
            print "Gave white for (.75, 1, 0) expected black"
            return

        self.passed = 0


if __name__ == "__main__":
    test_vtk_ui_white_or_black().test()
