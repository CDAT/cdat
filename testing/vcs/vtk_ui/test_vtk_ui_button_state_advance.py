"""
Test button state change
"""
import vcs.vtk_ui


from vtk_ui_test import vtk_ui_test

class test_vtk_ui_button_state_advance(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(100, 100)
        states = [vcs.vtk_ui.ButtonState(label="State %d" % i, fgcolor=(.1 * i + .5, .1 * i + .5, .1 * i + .5), bgcolor=(.5 - .1 * i,.5 - .1 * i,.5 - .1 * i)) for i in range(5)]

        b = vcs.vtk_ui.Button(self.inter, states=states, action=self.pass_me, left=0, top=0)
        b.show()

        b.set_state(1)
        b.repr.NextState()
        b.widget.InvokeEvent("StateChangedEvent")


    def pass_me(self, state):
        if state == 2:
            print "Button action executed"
            self.passed = 0
        else:
            print state, "Action passed inaccurate state"
            from sys import exit
            sys.exit(1)

if __name__ == "__main__":
    test_vtk_ui_button_state_advance().test()