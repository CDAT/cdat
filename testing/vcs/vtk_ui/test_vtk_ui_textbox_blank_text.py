"""
Test that text stays the same when you start/stop editing
"""
import vcs.vtk_ui
import vtk

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_textbox_blank_text(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(130, 130)

        textbox = vcs.vtk_ui.Textbox(self.inter, "Test String")
        textbox.show()
        textbox.start_editing()
        textbox.stop_editing()
        assert textbox.text == "Test String", "Start/stop editing altered text"

        textbox.text = ""
        textbox.start_editing()
        textbox.stop_editing()
        assert textbox.text == "", "Start/stop editing altered empty string"

        textbox.text = "Hi"
        textbox.on_editing_end = self.end
        textbox.start_editing()
        textbox.delete_character()
        textbox.delete_character()
        textbox.stop_editing()

        if self.passed != 0:
            print "Did not call editing end callback"
            return

    def end(self, textbox):
        assert textbox.text == "", "Deleting all characters should send blank string to editing end"
        self.passed = 0

if __name__ == "__main__":
    test_vtk_ui_textbox_blank_text().test()
