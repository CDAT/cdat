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
M.margins.top=.25
M.margins.bottom=.25
M.margins.left=.25
M.margins.right=.25

M.legend.direction='vertical'
## The legend uses the right margin for display are
## We need to "shrink it"
M.legend.thickness=.05
for i in range(12):
      t=M.get()

fnm = "test_EzTemplate_12_plots_legd_direction.png"
M.preview(fnm,bg=bg)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
