"""
Test button font/size
"""

import vcs, os
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_button_font(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(100, 80)
        fonts = ["Arial", "Courier", os.path.join(os.environ["HOME"], vcs.getdotdirectory()[0], "HelvMono.ttf")]
        sizes = [8, 12, 16]
        for ind, font in enumerate(fonts):
            b = vcs.vtk_ui.Button(self.inter, label="Font Test", font=font, top=ind * 25, size=sizes[ind])
            b.show()
        self.test_file = "test_vtk_ui_button_font.png"
test_vtk_ui_button_font().test()