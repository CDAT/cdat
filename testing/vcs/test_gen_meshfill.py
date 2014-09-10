import vcs,numpy,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(src),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")


data = numpy.array([1,2,3,4])

blon = numpy.array([-1,1,1,0,-1])
blat = numpy.array([0,0,1,2,1])

acell=numpy.array([blat,blon])
bcell = numpy.array([blat,blon+2.5])
ccell = numpy.array([blat+2.5,blon+2.5])
dcell = numpy.array([blat+2.5,blon])

mesh = numpy.array([acell,bcell,ccell,dcell])

m=x.createmeshfill()

x.plot(data,mesh,m,bg=1)


fnm = "test_vcs_gen_meshfill.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)


