"""
Test button click
"""
import vcs.vtk_ui


class KeyableSubclass(vcs.vtk_ui.behaviors.KeyableMixin):
    def __init__(self, interactor):
        self.interactor = interactor
        self.expecting = None
        self.matched = False
        super(KeyableSubclass, self).__init__()
        self.register()

    def key_pressed(self, key, shift=False, alt=False, control=False):
        args = (key, shift, alt, control)
        assert args == self.expecting, "%s on keydown does not match %s" % (args, self.expecting)
        self.expecting = None

    def key_released(self, key, shift=False, alt=False, control=False):
        args = (key, shift, alt, control)
        assert args == self.expecting, "%s on keyup does not match %s" % (args, self.expecting)
        self.expecting = None

from vtk_ui_test import vtk_ui_test

class test_vtk_ui_behavior_keyable(vtk_ui_test):
    def do_test(self):

        keyable = KeyableSubclass(self.inter)

        # Simple test; press a key
        simple = ("a", False, False, False)

        self.set_key(*simple)

        keyable.expecting = simple
        self.key_down()
        assert keyable.expecting is None, "KeyPressEvent was not triggered"

        keyable.expecting = simple
        self.key_up()
        assert keyable.expecting is None, "KeyReleaseEvent was not triggered"

        modified = ("A", True, True, True)
        self.set_key(*modified)
        keyable.expecting = modified
        self.key_down()

        keyable.expecting = modified
        self.key_up()

        symbol_char = ("Delete", False, False, False)
        self.set_key(*symbol_char)
        keyable.expecting = symbol_char
        self.key_down()

        self.passed = 0

if __name__ == "__main__":
    t = test_vtk_ui_behavior_keyable()
    t.test()