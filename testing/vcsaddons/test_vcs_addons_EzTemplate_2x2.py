import os, sys, cdms2, vcs.testing.regression as regression, vcs, vcsaddons

f = cdms2.open(os.path.join(vcs.sample_data,'clt.nc'))
s = f("clt",time=slice(0,1),squeeze=1)

bg = True
M = vcsaddons.EzTemplate.Multi(rows=2,columns=2)
x = vcs.init()
x.setantialiasing(0)
x.drawlogooff()
if bg:
  x.setbgoutputdimensions(1200,1091,units="pixels")
for i in range(4):
    x.plot(s,M.get(),bg=bg)

fnm = "test_vcs_addons_EzTemplate_2x2.png"
x.png(fnm)
ret = regression.check_result_image(fnm, sys.argv[1])
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
