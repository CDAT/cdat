import vcs
import sys
src =sys.argv[1]
x=vcs.init()
x.scriptrun(src)

td = x.gettaylordiagram("vcs_test_save_taylor_to_json_and_python")
td.script("vcs_test_save_td_to_json")

import filecmp

assert(filecmp.cmp("vcs_test_save_td_to_json.json",src))
