import vcs
import cdms2
import sys
f=cdms2.open(sys.prefix+"/sample_data/clt.nc")
s=f("clt",time=slice(0,1),squeeze=1)
x=vcs.init()
x.scriptrun("Test/isoleg.scr")
iso = vcs.getisofill("isoleg")
iso.list()
x.plot(s,iso)
raw_input("Ok")
