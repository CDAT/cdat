import cdtime

val = 239261039241284857122685270850462023680.0

r = cdtime.reltime(val, "hours since 1")

print "created:",r

print "converted:",r.tocomp()
