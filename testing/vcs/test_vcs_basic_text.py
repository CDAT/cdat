
import vcs, numpy, cdms2, MV2, os, sys, vcs.testing.regression as regression
x = regression.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
txt=x.createtext()
txt.x = [.0000005,.00000005,.5,.99999,.999999]
txt.y=[0.05,.9,.5,.9,0.05]
txt.string = ["SAMPLE TEXT A","SAMPLE TEXT B","SAMPLE TEXT C","SAMPLE TEXT D","SAMPLE TEXT E"]
txt.halign = "center"
txt.valign="base"
txt.angle=45
x.plot(txt,bg=1)
regression.run(x, "test_basic_text.png", sys.argv[1])
