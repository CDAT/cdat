import os, sys, vcs, vcs.testing.regression as regression
from vcsaddons import EzTemplate
## Initialize VCS
x = vcs.init()
x.drawlogooff()

bg = True
M=EzTemplate.Multi(rows=4,columns=3)
M.spacing.horizontal=.25
M.spacing.vertical=.1

fnm = "test_EzTemplate_12_plots_spacing.png"
M.preview(fnm,bg=bg)
ret = regression.check_result_image(fnm, sys.argv[1])
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
