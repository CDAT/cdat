
import filecmp
import vcs,numpy,os,sys
src = sys.argv[1]
if os.path.exists("test_vcs_dump_json.json"):
    os.remove("test_vcs_dump_json.json")

b = vcs.createboxfill("vcs_instance")
b.script("test_vcs_dump_json","a")
b = vcs.createisofill("vcs_instance")
b.script("test_vcs_dump_json","a")
b = vcs.createisoline("vcs_instance")
b.script("test_vcs_dump_json","a")
b = vcs.createmeshfill("vcs_instance")
b.script("test_vcs_dump_json","a")
b = vcs.create1d("vcs_instance")
b.script("test_vcs_dump_json","a")
b = vcs.createfillarea("vcs_instance")
b.script("test_vcs_dump_json","a")
b = vcs.createvector("vcs_instance")
b.script("test_vcs_dump_json","a")
b = vcs.createtext("vcs_instance")
b.script("test_vcs_dump_json","a")
b = vcs.createline("vcs_instance")
b.script("test_vcs_dump_json","a")
b = vcs.createmarker("vcs_instance")
b.script("test_vcs_dump_json","a")
b = vcs.createtemplate("vcs_instance")
b.script("test_vcs_dump_json","a")
b = vcs.createprojection("vcs_instance")
b.script("test_vcs_dump_json","a")

print "Comparing:",os.path.realpath("test_vcs_dump_json.json"),src
assert(filecmp.cmp("test_vcs_dump_json.json", src))
os.remove("test_vcs_dump_json.json")


