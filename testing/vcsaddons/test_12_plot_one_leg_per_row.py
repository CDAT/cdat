import os, sys, vcs, vcs.testing.regression as regression
from vcsaddons import EzTemplate

## Initialize VCS
x = vcs.init()
x.drawlogooff()

bg = True
M=EzTemplate.Multi(rows=4,columns=3)
M.legend.stretch=2.5 # 250% of width (for middle one)
for i in range(12):
  t=M.get(legend='local')
  if i%3 !=1:
    t.legend.priority=0 # Turn off legend
fnm = "test_12_plot_one_leg_per_row.png"
M.preview(fnm,bg=bg)
ret = regression.check_result_image(fnm, sys.argv[1])
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
