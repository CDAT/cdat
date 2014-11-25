
import vcs,sys,filecmp,os

good = sys.argv[1]

x=vcs.init()
x.drawlogooff()
tt=x.createtexttable("this_is_my_test_tt")
t=x.createtemplate("this_is_our_test_template")

t.xname.texttable=tt

fnm = "template_test_associated_dump.json"
if os.path.exists(fnm):
  os.remove(fnm)

t.script(fnm)

assert filecmp.cmp(fnm,good)
