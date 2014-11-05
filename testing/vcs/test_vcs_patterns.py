import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
f=cdms2.open(vcs.prefix+"/sample_data/clt.nc")
s=f("clt",time=slice(0,1),squeeze=1)
x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")
iso = vcs.createisofill("isoleg")
iso.levels = [0,10,20,30,40,50,60,70,80,90,100]
iso.fillareacolors = vcs.getcolors([0,10,20,30,40,50,60,70,80,90,100])
iso.fillareastyle = "pattern"
iso.fillareindices= [4,5,6,7,8,9,10,11,12,13,14,15]
x.plot(s,iso,bg=1)
fnm = "test_vcs_patterns.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
