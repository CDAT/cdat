import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
import EzTemplate,vcs
import cdms,EzTemplate,vcs,sys
## 12 plots 1 legend per row on the right
## Initialize VCS
x=vcs.init()
bg=True
M=EzTemplate.Multi(rows=4,columns=3)
M.legend.direction='vertical'
for i in range(12):
    t=M.get(legend='local')
    if i%3 !=2:
        t.legend.priority=0 # Turn off legend
fnm = "test_12_plot_one_leg_per_row_right.png"
M.preview(fnm,bg=bg)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
