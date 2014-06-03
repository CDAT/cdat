import vcs
import cdms2
import sys
import os
f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt")

x=vcs.init()
cmap = x.getcolormap("rainbow")

iso=x.createisofill()

iso.levels=[0,33,50,66,100]
iso.fillareacolors = [242,243,244,245]

x.plot(s,iso)

raw_input("Pressenter")


