"""
Test textbox typing
"""
import vcs.vtk_ui

from vtk_ui_test import vtk_ui_test
import difflib

class test_vtk_ui_textbox_typing(vtk_ui_test):
    def do_test(self):
        t = vcs.vtk_ui.Textbox(self.inter, "", fgcolor=(0, 0, 0))
        t.start_editing()
        # Run through whole range of typeable characters
        self.key_event("space")
        for key in range(33, 127):
            self.key_event(chr(key))

        assert t.editing is True, "Something ended editing early"
        self.key_event("Escape")
        assert t.editing is False, "Escape did not end editing"
        t.start_editing()
        # Make sure QQ works (always typed on accident when trying to exit)
        self.key_event("Q")
        self.key_event("Q")
        assert t.editing is False, "QQ did not end editing"
        assert t.text[-2:] != "QQ", "QQ not removed from text"

        ascii = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
        if t.text != ascii:
            print "Text did not match range of ascii characters"
            diff = difflib.SequenceMatcher(a=t.text, b=ascii)
            for op, i1, i2, j1, j2 in diff.get_opcodes():
                if op == "insert":
                    print "Missing '''%s'''" % ascii[j1:j2], "from text"
                elif op != "equal":
                    print "%s '''%s''' via '''%s'''" % (op, t.text[i1:i2], ascii[j1:j2])
            return

        self.passed = 0



if __name__ == "__main__":
    test_vtk_ui_textbox_typing().test()
