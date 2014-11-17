import vcs,numpy,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")

d = numpy.sin(numpy.arange(100))

b = x.createboxfill()

x.plot(d,b,bg=1)


fnm = "test_1d_in_boxfill.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)



