import vcs

l = vcs.createline("vcs_test_set_line")
l.color=242
l.width = 5.6
l.type = "dash"

v=vcs.createvector()
v.setLineAttributes("vcs_test_set_line")

assert(v.linecolor == 242)
assert(v.linewidth == 5.6)
assert(v.linetype == "dash")

yx = vcs.create1d()
yx.setLineAttributes(l)

assert(yx.linecolor == 242)
assert(yx.linewidth == 5.6)
assert(yx.linetype == "dash")

iso = vcs.createisoline()
# Note "solid" is a line name.
iso.setLineAttributes([l, "solid", l])

assert(iso.linecolors == [242, 1, 242])
assert(iso.linewidths == [5.6, 1, 5.6])
assert(iso.linetypes == ['dash', 'solid', 'dash'])
