import cdms2
import vcs
x=vcs.init()
import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(src),"..")
sys.path.append(pth)
import checkimage

f=cdms2.open(sys.prefix+"/sample_data/clt.nc")
s=f("clt",slice(0,15))
x.plot(s,bg=1)
x.animate.create(thread_it=0)
x.animate.save("test_vcs_anim_basic.mp4")

cmd = "ffmpeg -i test_vcs_anim_basic.mp4 test_anim_basic_out_%d.png"
import subprocess
import shlex
p=subprocess.Popen(shlex.split(cmd))
p.wait()

ret = 0 
## now check the generated images
for i in range(15):
 src_img = src % (i+1)
 ret+=checkimage.check_result_image("test_anim_basic_out_%i.png" % (i+1),src_img,0.05)
sys.exit(ret)
