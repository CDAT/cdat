import vcs,cdms2,os,sys,cdtime
import vcs.testing.regression as regression

x = regression.init()

f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s=f("clt",squeeze=1)
x.plot(s,bg=1,time='2015-02-23')
fnm = os.path.split(__file__)[1][:-3]+".png"
x.png(fnm)
regression.run(x, fnm)
