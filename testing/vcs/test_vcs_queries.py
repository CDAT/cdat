
import vcs
x=vcs.init()
x.drawlogooff()
gms = ["boxfill","isofill","isoline","meshfill","scatter","yxvsx","xvsy","xyvsy","vector"]
for gm in gms:
  print "testing query work for:",gm
  exec("g=vcs.create%s()" % gm)
  exec("res = vcs.is%s(g)" % gm)
  assert(res==1)
  for gm2 in gms:
    if gm2==gm or (gm in ("yxvsx", "xvsy") and gm2 in ("yxvsx", "xvsy")):
      continue
    print "Asserting %s is not %s" % (gm,gm2)
    exec("res = vcs.is%s(g)" % gm2)
    assert(res==0)
