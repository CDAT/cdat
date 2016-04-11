import vcs

x=vcs.init()

for t in ["boxfill","isofill","meshfill",
    "vector","yxvsx","xyvsy","xvsy","scatter",
    "1d","isoline","line","fillarea","marker",
    "texttable","textorientation","projection",
    "colormap","textcombined"]:
  print "Testing removal of",t,"objects"
  print "\tfrom canvas"
  exec("o = x.create%s()" % t)
  nm = o.name
  x.removeobject(o)
  assert(nm not in x.listelements(t))
  print "\tfrom vcs module"
  exec("o = vcs.create%s()" % t)
  nm = o.name
  vcs.removeobject(o)
  assert(nm not in vcs.listelements(t))

