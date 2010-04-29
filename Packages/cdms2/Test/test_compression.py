import cdms2


a = cdms2.MV2.zeros((1000,2100),'d')

f=cdms2.open("crap_default.nc",'w')
f.write(a)
f.close()


cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

f=cdms2.open("crap_nothing.nc",'w')
f.write(a)
f.close()


cdms2.setNetcdfShuffleFlag(1)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

f=cdms2.open("crap_justshuffle.nc",'w')
f.write(a)
f.close()

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(1)
cdms2.setNetcdfDeflateLevelFlag(6)

f=cdms2.open("crap_justdeflate6.nc",'w')
f.write(a)
f.close()


cdms2.setNetcdfDeflateLevelFlag(9)

f=cdms2.open("crap_justdeflate9.nc",'w')
f.write(a)
f.close()


