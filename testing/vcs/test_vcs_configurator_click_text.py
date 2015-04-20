import vcs
import sys

x = vcs.init()

t = x.createtext()
t.string = "test string"
t.x = .1
t.y = .1

# enable the configurator
x.configure()

# plot in the background
dp = x.plot(t, bg=1)

# Grab the initialized configurator
c = x.configurator

# Make sure the displays are current
c.update()

w, h = x.bgX, x.bgY

# Retrieve the actor at the specified point
actor = c.actor_at_point(.1 * w + 10, .1 * h + 5)

if actor is None:
    print "Couldn't find text actor"
    sys.exit(1)

display, key = c.display_and_key_for_actor(actor)

if display != dp:
    print "Found wrong display"
    sys.exit(1)

if actor != display.backend[key] and actor not in display.backend[key]:
    print "Found wrong key for actor"
    sys.exit(1)
