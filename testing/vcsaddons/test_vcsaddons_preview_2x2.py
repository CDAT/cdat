import os, sys, vcs.testing.regression as regression, vcsaddons

bg = True
M = vcsaddons.EzTemplate.Multi(rows=2,columns=2)
if bg:
  M.x.setbgoutputdimensions(1200,1091,units="pixels")
fnm = "test_vcsaddons_preview_2x2.png"
M.preview(out=fnm,bg=bg)
ret = regression.check_result_image(fnm, sys.argv[1])
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
