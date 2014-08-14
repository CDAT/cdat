import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(src),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")
txt=x.createtext()
txt.x = [.0000005,.00000005,.5,.99999,.999999]
txt.y=[0.05,.9,.5,.9,0.05]
txt.string = ["SAMPLE TEXT A","SAMPLE TEXT B","SAMPLE TEXT C","SAMPLE TEXT D","SAMPLE TEXT E"]
txt.halign = "center"
txt.valign="base"
txt.angle=45
x.plot(txt,bg=1)
fnm = "test_basic_text.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,0.05)
sys.exit(ret)
