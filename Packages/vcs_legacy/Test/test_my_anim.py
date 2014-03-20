import cdms2
fnm = "/work/cmip5/piControl/atm/mo/clt/cmip5.GISS-E2-R-CC.piControl.r1i1p1.mo.atm.Amon.clt.ver-1.latestX.xml"
var="clt"
f=cdms2.open(fnm)
s=f(var,slice(0,13))
print s.shape
import vcs_legacy
x=vcs_legacy.init()
x.plot(s,"default","isofill","default")
print "Ok plotted the data"
x.animate.create(min=20,max=80)
raw_input("Creating anim")
x.animate.run()

raw_input("Creating anim")
