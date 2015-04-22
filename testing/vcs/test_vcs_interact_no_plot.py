import vcs
import sys

x = vcs.init()
x.drawlogooff()
x.open()

inter = x.backend.renWin.GetInteractor()


def end_interact(obj, event):
    print "Interaction began"
    inter.TerminateApp()
    sys.exit(0)

inter.AddObserver("StartEvent", end_interact)
x.interact()

# Should not reach this point
print "Did not start interact."
sys.exit(1)
