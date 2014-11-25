import vcs,cdms2
x=vcs.init()
fnmcurv = '/lgm/uvcdat/2014-11-18/sample_data/sampleCurveGrid4.nc'
f=cdms2.open(fnmcurv)

s=f("sample")
m=x.createmeshfill()
m.mesh=True

x.plot(s,m)
