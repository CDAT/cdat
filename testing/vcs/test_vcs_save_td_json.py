import filecmp
import vcs
import sys
src_json = sys.argv[1]
src_py = sys.argv[2]
x=vcs.init()
td=x.createtaylordiagram("vcs_test_save_taylor_to_json_and_python")
td.detail = 75
td.max = None
td.quadrans=1
td.skillValues = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95]
td.skillColor = 252
td.skillDrawLabels = "y"
td.skillCoefficient = [1.0, 1.0, 1.0]
td.referencevalue = 1.0
td.arrowlength = 0.05
td.arrowangle = 20.0
td.arrowbase = 0.75
ids = ["0","1","2","3","4",""]
idsz = [20,20,20,20,30]
sz = [10,20,10,20,5]
colors = [16,239,16,239,1]
xoff = [2.0380499362945557, 2.1889517307281494, 2.005732774734497, 2.194457530975342, 0.0]
yoff = [1.0833240747451782, 1.5037295818328857, 1.2790908813476562, 1.4414606094360352, 0.0]
for i in range(5):
  td.addMarker(id=ids[i],id_size=idsz[i],color=colors[i],
      xoffset=xoff[i],
      yoffset=yoff[i])

td.script("vcs_test_save_td_to_json","w")
assert(filecmp.cmp("vcs_test_save_td_to_json.json",src_json))
td.script("vcs_test_save_td_to_py.py","w")
assert(filecmp.cmp("vcs_test_save_td_to_py.py",src_py))


