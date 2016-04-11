
import filecmp
import vcs,numpy,os,sys
src=sys.argv[1]
if os.path.exists("test_vcs_dump_json.json"):
    os.remove("test_vcs_dump_json.json")

b = vcs.createboxfill("Charles.Doutriaux")
b.script("test_vcs_dump_json","a")
b = vcs.createisofill("Charles.Doutriaux")
b.script("test_vcs_dump_json","a")
b = vcs.createisoline("Charles.Doutriaux")
b.script("test_vcs_dump_json","a")
b = vcs.createmeshfill("Charles.Doutriaux")
b.script("test_vcs_dump_json","a")
b = vcs.create1d("Charles.Doutriaux")
b.script("test_vcs_dump_json","a")
b = vcs.createfillarea("Charles.Doutriaux")
b.script("test_vcs_dump_json","a")
b = vcs.createtext("Charles.Doutriaux")
b.script("test_vcs_dump_json","a")
b = vcs.createline("Charles.Doutriaux")
b.script("test_vcs_dump_json","a")
b = vcs.createmarker("Charles.Doutriaux")
b.script("test_vcs_dump_json","a")
b = vcs.createtemplate("Charles.Doutriaux")
b.script("test_vcs_dump_json","a")
b = vcs.createprojection("Charles.Doutriaux")
b.script("test_vcs_dump_json","a")

assert(filecmp.cmp("test_vcs_dump_json.json",src))


