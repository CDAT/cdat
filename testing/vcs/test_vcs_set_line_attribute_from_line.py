import vcs

l = vcs.createline("vcs_test_set_line")
l.color=242
l.width = 5.6
l.type = "dash"

v=vcs.createvector()
v.line = "vcs_test_set_line"

assert(v.linecolor == 242)
assert(v.linewidth == 5.6)
assert(v.line == "dash")

yx = vcs.create1d()
yx.line = l


assert(yx.linecolor == 242)
assert(yx.linewidth == 5.6)
assert(yx.line == "dash")
