"""
Test textbox typing
"""
import vcs.vtk_ui

from vtk_ui_test import vtk_ui_test
import difflib

def strings_are_the_same(a, b):
    diff = difflib.SequenceMatcher(a=a, b=b)
    if diff.ratio() == 1:
        return True

    print "Diff-ing:"
    print a
    print b
    for op, i1, i2, j1, j2 in diff.get_opcodes():
        if op == "insert":
            print "Missing '''%s'''" % b[j1:j2], "from text"
        if op == "delete":
            print "Delete '''%s''' at %d:%d" % (a[i1:i2], i1, i2)
        if op == "replace":
            print "Replace '''%s''' with '''%s'''" % (a[i1:i2], b[j1:j2])
    return False

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
        assert strings_are_the_same(t.text, ascii), "Text did not match range of ascii characters"

        t.text = ""

        self.key_event("space")

        assert t.text != " ", "Accepted key input while not editing"

        t.start_editing()

        for char in "This is a test string":
            if char == " ":
                char = "space"
            self.key_event(char)

        assert strings_are_the_same(t.text, "This is a test string"), "Did not match simple string"

        current_column = t.column
        self.key_event("Backspace")
        assert t.column == current_column - 1, "Backspace did not move the cursor; should be %d, is %d" % (current_column - 1, t.column)
        assert strings_are_the_same(t.text, "This is a test strin"), "Did not delete the last character"

        current_column = t.column

        self.key_event("Left")
        assert t.column == current_column - 1, "Left arrow did not navigate correctly: should be at %s, is at %s" % (current_column - 1, t.column)

        self.key_event("Return")
        assert strings_are_the_same(t.text, "This is a test stri\nn"), "Did not add newline at correct position"

        self.key_event("Shift")
        assert strings_are_the_same(t.text, "This is a test stri\nn"), "Shift was not a noop"

        self.key_event("Up")
        self.key_event("Right")
        self.key_event("Return")
        assert strings_are_the_same(t.text, "T\nhis is a test stri\nn"), "Did not add newline at correct position"

        self.key_event("Down")
        self.key_event("Backspace")
        assert strings_are_the_same(t.text, "T\nhis is a test strin"), "Did not delete newline"

        self.key_event("Tab")
        assert strings_are_the_same(t.text, "T\nhis is a test stri\tn"), "Did not insert tab at correct position"

        self.key_event("Backspace")
        assert strings_are_the_same(t.text, "T\nhis is a test strin"), "Did not delete from middle of string"
        # Advance to the last position
        self.key_event("Right")

        # Clear out the string to empty
        for _ in range(len("T\nhis is a test strin")):
            self.key_event("Backspace")

        assert t.blank is True, "Textbox not blank ('''%s''')" % t.text

        self.key_event("A")
        assert t.blank is False, "Textbox did not reset blank after adding text"
        assert strings_are_the_same(t.text, "A"), "Textbox did not set string correctly after blank"

        self.key_event("Return")
        assert strings_are_the_same(t.text, "A\n")

        self.key_event("Left")
        assert t.row == 0 and t.column == 1, "Did not line wrap on left arrow"

        self.key_event("Right")
        assert t.row == 1 and t.column == 0, "Did not line wrap on right arrow"

        self.key_event("B")

        self.key_event("Down")
        assert t.row == 1 and t.column == 1, "Did not jump to end of line of last row on down arrow"

        self.key_event("C")

        self.key_event("Up")
        assert t.row == 0 and t.column == 2, "Did not keep position on up arrow; column is %d" % t.column

        self.key_event("D")
        assert t.row == 0 and t.column == 2 and strings_are_the_same(t.text, "AD\nBC"), "Did not append character at end of line"

        self.key_event("Up")
        assert t.row == 0 and t.column == 0, "Did not jump to start of line of first row on up arrow"

        self.passed = 0



if __name__ == "__main__":
    test_vtk_ui_textbox_typing().test()
