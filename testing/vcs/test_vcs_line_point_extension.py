import vcs, os, sys

x = vcs.init()
line = x.createline()
line.x = [[0, .1], [.2, .3], [.4, .5], [.6, .7]]
line.y = [[0, .1], [.2, .3, .4], [.5], [.6, .7]]
x.plot(line)

if line.x[0] != [0, .1]:
  print "line.x[0] should be [0, .1]; is %s" % line.x[0]
  sys.exit(-1)

if line.x[1] != [.2, .3, .3]:
  print 'line.x[1] should be [.2, .3]; is %s' % line.x[1]
  sys.exit(-1)

if line.y[2] != [.5, .5]:
  print "line.y[2] should be [.5, .5]; is %s" % line.y[2]
  sys.exit(-1)

sys.exit(0)
