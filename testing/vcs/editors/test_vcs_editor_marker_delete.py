import vcs
import sys

x = vcs.init(geometry=(800,600))

m = x.createmarker()
m.x = .1,
m.y = .1,

# enable the configurator
x.configure()

# plot
dp = x.plot(m)

# Grab the initialized configurator
c = x.configurator

# Make sure the displays are current
c.update()

w, h = 800, 606

# Retrieve the actor at the specified point
c.interactor.SetEventInformation(int(.1 * w), int(.1 * h))
c.click(None, None)
c.release(None, None)

# Make sure we've got the correct editor
editor = c.target
if editor is None:
    print "Could not find an editable object at", int(.1 * w), ",", int(.1 * h)
    sys.exit(1)
print "Found an editable object"
if type(editor) != vcs.editors.marker.MarkerEditor:
    print "Object found is not a marker"
    sys.exit(1)
print "Found a marker object"
if editor.marker != m:
    print "Did not find the correct marker, expected", m.name, "received", editor.marker.name
    sys.exit(1)
print "Found the correct marker"

# Simulate a right click on the marker
editor.right_release()

# Make sure the editor has been deactivated
if c.target == editor:
    print "Did not end edit of object"
    sys.exit(1)
print "Marker no longer being edited"
# Make sure the marker was deleted
if len(m.x) != len(m.y) != len(m.type) != len(m.color) != 0:
    print "Did not delete all attributes on marker"
    sys.exit(1)
print "Deleted all attributes on marker"
