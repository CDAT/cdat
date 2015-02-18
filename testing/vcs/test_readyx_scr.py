
import vcs,sys
vcs.scriptrun(sys.argv[1])

assert("testyx" in vcs.listelements("yxvsx"))

y = vcs.getyxvsx("testyx")

assert(y.datawc_x1 == -50.)
assert(y.datawc_x2 == 20.)
assert(y.datawc_y1 == 50.)
assert(y.datawc_timeunits == "days since 2100")
assert(y.datawc_calendar == 135441)
assert(y.xaxisconvert == "log10")
assert(y.yaxisconvert == "area_wt")
assert(y.line == "dash")
assert(y.linecolor == 241)
assert(y.linewidth == 1)
assert(y.marker == "circle")
assert(y.markercolor == 241)
assert(y.markersize == 1)
assert(y.flip == False)


