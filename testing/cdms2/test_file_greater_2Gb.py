import cdms2
import MV2
value = 0
cdms2.setNetcdfShuffleFlag(value) ## where value is either 0 or 1
cdms2.setNetcdfDeflateFlag(value) ## where value is either 0 or 1
cdms2.setNetcdfDeflateLevelFlag(value) ## where value is a integer between 0 and 9 included

f=cdms2.open("crap.nc","w")

b=MV2.ones((100,100,100,100))
N=5
for i in range(N):
  print "I:",i
  f.write(b,id=str(i))
  f.sync()
f.close()
import os
os.remove("crap.nc")
