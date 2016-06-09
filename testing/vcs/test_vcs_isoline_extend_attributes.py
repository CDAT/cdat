import cdms2
import vcs
import testing.regression as regression

x = regression.init()
isoline = vcs.createisoline()
f = cdms2.open(vcs.sample_data + '/clt.nc')
s = f("clt")
isoline.line = ["dash-dot"]
isoline.linecolors = [250]
isoline.linewidths = [5]
x.plot(s, isoline)
fnm = "test_vcs_isoline_extend_attributes.png"
regression.run(x, fnm)
