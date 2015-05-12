"""
Test textbox detach
"""
import vcs.vtk_ui
import vtk
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_textbox_detach(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(200, 75)

        t = vcs.vtk_ui.Textbox(self.inter, "Sample Text", fgcolor=(0,0,0))
        t.left = 25
        t.top = 25
        t.show()
        t.start_editing()
        assert t.cursor is not None, "Cursor not initialized"
        t.detach()
        assert t.cursor is None, "Cursor not detached"
        assert self.inter.GetTimerDuration(t.blink_timer) == 0, "Timer not destroyed"
        assert self.inter.GetCommand(t.blink_observer) is None, "Blink observer not destroyed"
        assert self.inter.GetCommand(t.keyboard_observer) is None, "Keyboard observer not destroyed"
        self.passed = 0

if __name__ == "__main__":
    test_vtk_ui_textbox_detach().test()
