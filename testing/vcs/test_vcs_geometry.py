import sys, vcs

# This will check if we can set the geometry
# at the initialization of canvas
canvas = vcs.init(geometry=(600, 400))
canvas.open()

if dict(width=600, height=400) != canvas.geometry():
    canvas.close()
    sys.exit(1)

canvas.close()

canvas2 = vcs.init()

# This will check if we can safely set the geometry even
# though the canvas window has not been created yet
canvas2.geometry(400, 400)
canvas2.open()
if dict(width=400, height=400) != canvas2.geometry():
    canvas2.close()
    sys.exit(1)

# This will check if we can dynamically change the geometry
canvas2.geometry(500, 400)
canvas2.geometry(500, 500)
if dict(width=500, height=500) != canvas2.geometry():
    canvas2.close()
    sys.exit(1)

canvas2.close()
sys.exit(0)