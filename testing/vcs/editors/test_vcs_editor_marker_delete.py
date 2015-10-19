import vcs
import sys

x = vcs.init()
x.open()
# Needs to set the size of window so it is consistent accross
# test platforms
x.geometry(x.bgX,x.bgY)

m = x.createmarker()
m.x = .1,
m.y = .1,

# enable the configurator
x.configure()

# plot in the background
dp = x.plot(m)

# Grab the initialized configurator
c = x.configurator

# Make sure the displays are current
c.update()

w, h = x.bgX, x.bgY

# Retrieve the actor at the specified point
c.interactor.SetEventInformation(int(.1 * w), int(.1 * h))
c.click(None, None)
c.release(None, None)

# Make sure we've got the correct editor
editor = c.target
if editor is None:
    print "Did not activate an editor"
    sys.exit(1)
print "Editor activated"
if type(editor) != vcs.editors.marker.MarkerEditor:
    print "Did not activate a marker editor"
    sys.exit(1)
print "Editor is a marker editor"
if editor.marker != m:
    print "Did not activate the correct marker editor, expected", m.name, "received", editor.marker.name
    sys.exit(1)
print "Marker editor is editing the correct marker"

# Simulate a right click on the marker
editor.right_release()

# Make sure the editor has been deactivated
if c.target == editor:
    print "Did not deactivate editor"
    sys.exit(1)
print "Marker editor deactivated"
# Make sure the marker was deleted
if len(m.x) != len(m.y) != len(m.type) != len(m.color) != 0:
    print "Did not delete all attributes on marker"
    sys.exit(1)
print "Deleted all attributes on marker"
