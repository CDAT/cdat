import vcs
x=vcs.init()
gms = ["boxfill","isofill","isoline","meshfill","scatter","yxvsx","xvsy","xyvsy","vector"]
for gm in gms:
  print "testing query work for:",gm
  exec("g=vcs.create%s()" % gm)
  exec("res = vcs.is%s(g)" % gm)
  assert(res==1)
  for gm2 in gms:
    if gm2==gm:
      continue
    print "Asserting %s is not %s" % (gm,gm2)
    exec("res = vcs.is%s(g)" % gm2)
    assert(res==0)
