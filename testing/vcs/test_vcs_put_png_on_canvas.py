import vcs
import sys
import argparse
import vcs.testing.regression as regression
import os

p =argparse.ArgumentParser()

p.add_argument("-H","--fitToHeight",default=True,action="store_false")
p.add_argument("-u","--units",default="percent")
p.add_argument("-x","--xoffset",default=0,type=float)
p.add_argument("-y","--yoffset",default=0,type=float)
p.add_argument("-z","--zoom",default=1.,type=float)
p.add_argument("-s","--source",default="./somefile.png")

args = p.parse_args(sys.argv[1:])
print args
bg=True
x=vcs.init(bg=bg,geometry=(1200,800))
x.open()
png = os.path.join(sys.prefix,"share","uvcdat","sample_data","BlueMarble.ppm")
x.put_png_on_canvas(png,args.zoom,args.xoffset,args.yoffset,args.units,args.fitToHeight)
fnm = "test_vcs_put_png_on_canvas_%s_%s_%s_%s_%s" % (args.zoom,args.xoffset,args.yoffset,args.units,args.fitToHeight)
x.png(fnm)
ret = regression.check_result_image(fnm+'.png',args.source,20.)
