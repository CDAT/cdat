import vcs
import cdms2
import os,sys
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt")
x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
iso=x.createisoline()
t=x.createtext()
t.color=243
t.height=25
to=x.createtextorientation()
to.height = 55
tt=x.createtexttable()
tt.color=245
iso.textcolors=[None,None,None,242,244]
iso.text=[t,tt,to]
iso.label="y"
x.plot(s,iso,bg=1)
x.png("test_vcs_isoline_labels_multi_label_input_types.png")

src=sys.argv[1]

ret = checkimage.check_result_image('test_vcs_isoline_labels_multi_label_input_types.png',src,checkimage.defaultThreshold)
sys.exit(ret)
