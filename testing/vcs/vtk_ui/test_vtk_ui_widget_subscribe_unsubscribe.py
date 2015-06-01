"""
Test widget event subscribe / unsubscribe
"""
import vcs.vtk_ui

import vtk
from vtk_ui_test import vtk_ui_test

class test_vtk_ui_widget_subscribe_unsubscribe(vtk_ui_test):
    def do_test(self):
        self.win.SetSize((100, 100))

        vw = vtk.vtkButtonWidget()
        vr = vtk.vtkTexturedButtonRepresentation2D()

        vr.SetNumberOfStates(1)
        r = vtk.vtkPNGReader()
        r.SetFileName("Pepper.png")
        r.Update()
        image = r.GetOutput()
        vr.SetButtonTexture(0, image)
        vw.SetRepresentation(vr)

        w = vcs.vtk_ui.widget.Widget(self.inter, vw)
        w.show()

        def dummy(*args, **kwargs):
            pass

        w.subscribe("StartInteractionEvent", dummy)

        # Make sure event was properly subscribed to
        assert "StartInteractionEvent" in w.subscriptions, "Event not in subscriptions"

        # Check observers of w for the tag in w.subscriptions
        tag = w.subscriptions["StartInteractionEvent"]
        c = vw.GetCommand(tag)
        assert c is not None, "Listener not attached to widget"

        try:
            w.subscribe("StartInteractionEvent", dummy)
            print "Failed to notice double event subscription on widget"
            return
        except KeyError:
            pass

        w.unsubscribe("StartInteractionEvent")
        assert "StartInteractionEvent" not in w.subscriptions, "Did not remove event from subscriptions on unsubscribe"

        # Test multiple unsubscriptions
        w.subscribe("EndInteractionEvent", dummy)
        w.subscribe("StartInteractionEvent", dummy)
        w.unsubscribe("StartInteractionEvent", "EndInteractionEvent")
        assert "EndInteractionEvent" not in w.subscriptions and "StartInteractionEvent" not in w.subscriptions, "Did not remove both events from widget subscriptions"

        try:
            w.unsubscribe("StartInteractionEvent")
            print "Failed to notice double unsubscribe on widget"
            return
        except KeyError:
            pass

        self.passed = 0


if __name__ == "__main__":
    test_vtk_ui_widget_subscribe_unsubscribe().test()
