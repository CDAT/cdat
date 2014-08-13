import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(src),"..")
sys.path.append(pth)
import checkimage
import numpy

s= numpy.sin(numpy.arange(100))
s=numpy.reshape(s,(10,10))

s[4,6] = numpy.inf
s[7,9] = numpy.NINF
s[9,2] = numpy.nan

x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")
x.plot(s,bg=1)
fnm = "infinity.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,0.05)
sys.exit(ret)

