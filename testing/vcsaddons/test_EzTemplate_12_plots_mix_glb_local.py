import os, sys, vcs, vcs.testing.regression as regression
from vcsaddons import EzTemplate
## Initialize VCS
x=vcs.init()
x.drawlogooff()

bg = True
M=EzTemplate.Multi(rows=4,columns=3)
for i in range(12):
    if i%2==1:
        if i%4 == 3:
            M.legend.direction='vertical'
        t=M.get(legend='local')
        M.legend.direction='horizontal'
    else:
      t=M.get()

fnm = "test_EzTemplate_12_plots_mix_glb_local.png"
M.preview(fnm,bg=bg)
ret = regression.check_result_image(fnm, sys.argv[1])
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
