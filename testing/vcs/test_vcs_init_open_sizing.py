import vcs
import sys


failing = {
    "allowed negative value": (250, -1),
    "allowed 0 value": (0, 125),
    "allowed non-numeric value": "ab",
    "allowed too many values": [1, 2, 3],
    "allowed no values": []
}

for reason in failing:
    # Try as dictionary
    f = failing[reason]
    d = {}
    if len(f) == 2:
        d = {"width": f[0], "height": f[1]}
    elif len(f) > 2:
        d = {"width": f[0], "height": f[1], "other": f[2:]}

    try:
        vcs.init(geometry=d, bg=True)
    except ValueError:
        pass
    else:
        print "Dict", failing
        sys.exit(1)

    try:
        vcs.init(geometry=f, bg=True)
    except ValueError:
        pass
    else:
        print "List/Tuple", failing
        sys.exit(1)

valid = [
    (250, 125),
    (1, 1),
    (800, 1000)
]


def test_canvas_size(c, size, via):
    info = c.canvasinfo()
    w, h = size

    assert info["width"] == w, "Width via %s incorrect; expected %d, got %d" % (via, w, info["width"])
    assert info["height"] == h, "Height via %s incorrect; expected %d, got %d" % (via, h, info["height"])

for size in valid:
    w, h = size
    d = {"width": w, "height": h}

    c = vcs.init(geometry=d, bg=True)
    c.open()
    test_canvas_size(c, size, "dict init")
    c.close()

    c = vcs.init(geometry=size, bg=True)
    c.open()
    test_canvas_size(c, size, "list/tuple init")
    c.close()

    c = vcs.init(bg=True)
    c.open(width=w, height=h)
    test_canvas_size(c, size, "open")
    c.close()
