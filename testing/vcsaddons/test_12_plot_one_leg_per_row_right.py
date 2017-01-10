import os, sys, vcs.testing.regression as regression

import vcs
from vcsaddons import EzTemplate
import cdms2,vcs,sys
## 12 plots 1 legend per row on the right
## Initialize VCS
x = vcs.init()
x.drawlogooff()
bg = True
M = EzTemplate.Multi(rows=4,columns=3)
M.legend.direction='vertical'
for i in range(12):
    t=M.get(legend='local')
    if i%3 !=2:
        t.legend.priority=0 # Turn off legend
fnm = "test_12_plot_one_leg_per_row_right.png"
M.preview(fnm,bg=bg)
ret = regression.check_result_image(fnm, sys.argv[1])
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
