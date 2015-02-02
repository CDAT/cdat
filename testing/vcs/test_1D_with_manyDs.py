
import vcs,numpy,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

d = numpy.sin(numpy.arange(100))
d=numpy.reshape(d,(10,10))


one = x.create1d()

x.plot(d,one,bg=1)


fnm = "test_1D_with_manyDs.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)



