import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
import EzTemplate,vcs
## 12 plot one legend per row

## Initialize VCS
x=vcs.init()

bg = True
M=EzTemplate.Multi(rows=4,columns=3)
M.spacing.horizontal=.25
M.spacing.vertical=.1

fnm = "test_EzTemplate_12_plots_spacing.png"
M.preview(fnm,bg=bg)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
