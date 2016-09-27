import vcs
import sys
import argparse

p =argparse.ArgumentParser()

p.add_argument("-H","--fitToHeight",default=True,type=bool)
p.add_argument("-u","--units",default="percent")
p.add_argument("-x","--xoffset",default=0,type=float)
p.add_argument("-y","--yoffset",default=0,type=float)
p.add_argument("-f","--file")
p.add_argument("-z","--zoom",default=1.)

args = p.parse_args(sys.argv[1:])
bg=False
x=vcs.init(bg=bg,geometry=(814,606))
x.open()
x.put_png_on_canvas(args.file,args.zoom,args.xoffset,args.yoffset,args.units,args.fitToHeight)
fnm = "test_vcs_put_png_on_canvas_%s_%s_%s_%s_%s" % (args.zoom,args.xoffset,args.yoffset,args.units,args.fitToHeight)
x.png(fnm)
