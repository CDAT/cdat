
import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
import EzTemplate,vcs
## 12 plot one legend per row

## Initialize VCS
x=vcs.init()
x.drawlogooff()

bg = True
M=EzTemplate.Multi(rows=4,columns=3)
for i in range(12):
    if i%2==1:
        if i%4 == 3:
            M.legend.direction='vertical'
        t=M.get(legend='local')
        M.legend.direction='horizontal'
    else:
      t=M.get()

fnm = "test_EzTemplate_12_plots_mix_glb_local.png"
M.preview(fnm,bg=bg)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
