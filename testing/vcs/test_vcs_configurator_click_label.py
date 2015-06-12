import vcs
import sys
import cdms2
import os

x = vcs.init()

cltfile = cdms2.open(os.path.join(vcs.prefix, "sample_data", "clt.nc"))
clt = cltfile("clt")

# Create a template to use
t = x.createtemplate()

# Hide everything that isn't the dataname
for attr in dir(t):
    try:
        a = getattr(t, attr)
        if "priority" in dir(a):
            a.priority = 0
    except AttributeError, TypeError:
        pass

t.dataname.priority = 1
t.dataname.x = .1
t.dataname.y = .9

orient = x.createtextorientation("level", "default")
orient.angle = 0
orient.halign = 'left'
orient.valign = 'half'

t.dataname.textorientation = orient.name

# enable the configurator
x.configure()

# plot in the background
dp = x.boxfill(clt, t)

# Grab the initialized configurator
c = x.configurator

# Make sure the displays are current
c.update()

w, h = x.bgX, x.bgY

# Retrieve the actor at the specified point
actor = c.actor_at_point(.1 * w + 5, .9 * h + 5)

if actor is None:
    print "Couldn't find text actor at (%f, %f)" % (.1 * w + 5, .9 * h + 5)
    sys.exit(1)

display, key = c.display_and_key_for_actor(actor)

if display != dp:
    print "Found wrong display"
    sys.exit(1)

if actor != display.backend[key] and actor not in display.backend[key]:
    print "Found wrong key for actor"
    sys.exit(1)
