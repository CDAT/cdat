import os, sys, vcs, vcs.testing.regression as regression
from vcsaddons import EzTemplate

## Initialize VCS
x = vcs.init()
x.drawlogooff()

bg = True
M=EzTemplate.Multi(rows=4,columns=3)
M.margins.top=.25
M.margins.bottom=.25
M.margins.left=.25
M.margins.right=.25

M.legend.direction='vertical'
## The legend uses the right margin for display are
## We need to "shrink it"
M.legend.thickness=.05
for i in range(12):
      t=M.get()

fnm = "test_EzTemplate_12_plots_legd_direction.png"
M.preview(fnm, bg=bg)
ret = regression.check_result_image(fnm, sys.argv[1])
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
