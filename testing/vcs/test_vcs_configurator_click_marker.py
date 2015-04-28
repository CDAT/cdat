import vcs
import sys

x = vcs.init()

m = x.createmarker()
m.x = .1,
m.y = .1,

# enable the configurator
x.configure()

# plot in the background
dp = x.plot(m, bg=1)

# Grab the initialized configurator
c = x.configurator

# Make sure the displays are current
c.update()

w, h = x.bgX, x.bgY

# Retrieve the actor at the specified point
actor = c.actor_at_point(.1 * w, .1 * h)

if actor is None:
    print "Couldn't find marker actor"
    sys.exit(1)

display, key = c.display_and_key_for_actor(actor)

if display != dp:
    print "Found wrong display"
    sys.exit(1)

try:
    if actor not in display.backend[key][0]:
        print "Found wrong key for actor"
        sys.exit(1)
except AttributeError:
    print "Found wrong key for actor"
    sys.exit(1)
