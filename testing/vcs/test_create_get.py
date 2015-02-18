
import vcs
x=vcs.init()
x.drawlogooff()

for obj in ["boxfill","isofill","isoline","meshfill","taylordiagram","vector",
           "1d","3d_scalar","3d_vector","colormap","fillarea","line","marker",
           "text","projection","template"]:
  print "Testing create/get for %s" % obj
  exec("Ocr = x.create%s()" % obj)
  exec("Ogt = x.get%s(Ocr.name)" %obj)
  assert Ocr.name == Ogt.name
