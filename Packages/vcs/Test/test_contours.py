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
x.setcolormap("rainbow")
iso=x.createisofill()
#iso=x.createboxfill()
#iso.boxfill_type = "custom"
#iso=x.createisoline()

#iso.levels=[0,33,50,66,100]
#levs=[-1.e20,10,50,60,70,80,90,1.e20]
levs = [[10,40],[60,65],[65,80],[90,100]]
cols=vcs.getcolors(levs)
print cols
iso.levels=levs
iso.fillareacolors = cols
#iso.legend = {60:"SIXTY",80:"EIGHTY"}
#iso.linecolors = [242,243,244,245,246]
#iso.list()
x.plot(s,iso)
x.png("contours")
raw_input("Press Enter")


