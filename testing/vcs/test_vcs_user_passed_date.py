import vcs,cdms2,os,sys,cdtime
import vcs.testing.regression as regression

f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s=f("clt",squeeze=1)
x=regression.init()
x.plot(s,bg=1,time=cdtime.comptime(2015))
fnm = os.path.split(__file__)[1][:-3]+".png"
regression.run(x, fnm)
