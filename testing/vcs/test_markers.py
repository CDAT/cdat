import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(src),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")

m = x.createmarker()
m.x=[[0.,],[5,],[10.,],[15.]]
m.y=[[0.,],[5,],[10.,],[15.]]
m.worldcoordinate=[-5,20,-5,20]
#m.worldcoordinate=[-10,10,0,10]
m.type=['plus','diamond','square_fill',"hurricane"]
m.color=[242,243,244,242]
m.size=[5.,]
x.plot(m,bg=1)
fnm= "test_markers.png"

x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
