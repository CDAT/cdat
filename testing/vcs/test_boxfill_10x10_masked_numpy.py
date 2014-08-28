import vcs,numpy,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(src),"..")
sys.path.append(pth)
import checkimage

s= numpy.sin(numpy.arange(100))
s=numpy.reshape(s,(10,10))
s=numpy.ma.masked_greater(s,.5)
x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")
x.plot(s,bg=1)
fnm= "test_vcs_boxfill_10x10_masked_numpy.png"

x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,0.05)
sys.exit(ret)
