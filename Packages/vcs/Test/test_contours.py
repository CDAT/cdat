import vcs
import cdms2
import os
f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
s = f("clt", time=slice(0, 1), squeeze=1)
print s.shape
x = vcs.init()
cmap = x.getcolormap("rainbow")
x.setcolormap("rainbow")
iso = x.createisofill()

levs = [[10, 40], [60, 65], [65, 80], [90, 100]]
cols = vcs.getcolors(levs)
print cols
iso.levels = levs
iso.fillareacolors = cols
x.plot(s, iso)
x.png("contours")
raw_input("Press Enter")
