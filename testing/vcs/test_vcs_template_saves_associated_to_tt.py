import vcs
x=vcs.init()
tt=x.createtexttable("this_is_my_test_tt")
t=x.createtemplate("this_is_our_test_template")

t.xname.texttable=tt

t.script("template_test_associated_dump.json")
