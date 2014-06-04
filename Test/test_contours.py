import vcs
import cdms2
import sys
import os
f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt",time=slice(0,1),squeeze=1)
print s.shape
#s[:23,:36] = 0.
#s[:23,36:] = 25.
#s[23:,36:] = 60.
#s[23:,:36] = 100.
x=vcs.init()
cmap = x.getcolormap("rainbow")

iso=x.createisofill()
#iso=x.createboxfill()
#iso.boxfill_type = "custom"
#iso=x.createisoline()

#iso.levels=[0,33,50,66,100]
levs=[-1.e20,10,20,30,40,50,60,70,80,90,1.e20]
cols=vcs.getcolors(levs)
iso.levels=levs
iso.fillareacolors = cols
#iso.linecolors = [242,243,244,245,246]
iso.list()
x.plot(s,iso)

raw_input("Press Enter")


