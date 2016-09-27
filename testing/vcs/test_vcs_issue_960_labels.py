import os, sys, cdms2, vcs, vcs.testing.regression as regression

src1 = sys.argv[1]
src2 = sys.argv[2]
x = regression.init()
f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s=f("clt",time=slice(0,1),latitude=(-7,5),squeeze=1)
x.plot(s,bg=1)
fnm = "test_vcs_issue_960_labels_1.png"
x.png(fnm)
ret = regression.check_result_image(fnm, src1)
b=x.createboxfill()
b.datawc_y1=-7
b.datawc_y2=5
x.plot(s,b,bg=1)
fnm = "test_vcs_issue_960_labels_2.png"
x.png(fnm)
ret += regression.check_result_image(fnm, src2)
sys.exit(ret)